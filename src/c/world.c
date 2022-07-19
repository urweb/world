#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include <curl/curl.h>

#include <openssl/evp.h>
#include <openssl/sha.h>
#include <openssl/rsa.h>
#include <openssl/pem.h>
#include <openssl/hmac.h>

#include <libscrypt.h>

#include <urweb.h>
#include <world.h>

#define BUF_MAX (10 * 1024 * 1024)
#define BUF_INIT 1024

static CURL *curl(uw_context ctx) {
  CURL *r;

  if (!(r = uw_get_global(ctx, "curl"))) {
    r = curl_easy_init();
    if (r)
      uw_set_global(ctx, "curl", r, curl_easy_cleanup);
  }

  return r;
}

typedef struct {
  uw_context ctx;
  uw_buffer *buf;
} ctx_buffer;

static size_t write_buffer_data(void *buffer, size_t size, size_t nmemb, void *userp) {
  ctx_buffer *cb = userp;

  if (uw_buffer_append(cb->buf, buffer, size * nmemb))
    uw_error(cb->ctx, FATAL, "Exceeded maximum size (%d bytes) for payload returned by remote Web server", BUF_MAX);

  return size * nmemb;
}

static const char curl_failure[] = "error=fetch_url&error_description=";
static const char server_failure[] = "error=fetch_url&error_description=";

uw_Basis_int uw_WorldFfi_lastErrorCode(uw_context ctx) {
  void *p = uw_get_global(ctx, "world.lastErrorCode");
  if (p) return (uw_Basis_int)p;
  else return 200;
}

static int allow_http = 0;

uw_Basis_unit uw_WorldFfi_allowHttp(uw_context ctx) {
  allow_http = 1;
  return uw_unit_v;
}

typedef struct uw_WorldFfi_headers {
  uw_Basis_string header, value;
  struct uw_WorldFfi_headers *next;
} *uw_WorldFfiHeaders;

const uw_WorldFfi_headers emptyHeaders = NULL;
uw_WorldFfi_headers uw_WorldFfi_addHeader(uw_context ctx, uw_WorldFfi_headers hs, uw_Basis_string header, uw_Basis_string value) {
  uw_WorldFfiHeaders r = uw_malloc(ctx, sizeof(struct uw_WorldFfi_headers));
  r->header = header;
  r->value = value;
  r->next = hs;
  return r;
}

// Returns 1 on "not found".
static int doweb(uw_context ctx, uw_buffer *buf, CURL *c, uw_Basis_string url, int encode_errors, int special_case_404) {
  if (strncmp(url, "https://", 8) && (!allow_http || strncmp(url, "http://", 7)))
    uw_error(ctx, FATAL, "World: URL is not HTTPS");

  ctx_buffer cb = {ctx, buf};
  char error_buffer[CURL_ERROR_SIZE];
  CURLcode code;

  uw_buffer_init(BUF_MAX, buf, BUF_INIT);
  uw_push_cleanup(ctx, (void (*)(void *))uw_buffer_free, buf);

  curl_easy_setopt(c, CURLOPT_URL, url);
  curl_easy_setopt(c, CURLOPT_WRITEFUNCTION, write_buffer_data);
  curl_easy_setopt(c, CURLOPT_WRITEDATA, &cb);
  curl_easy_setopt(c, CURLOPT_ERRORBUFFER, error_buffer);

  code = curl_easy_perform(c);

  if (code) {
    if (encode_errors) {
      uw_buffer_reset(buf);
      uw_buffer_append(buf, curl_failure, sizeof curl_failure - 1);
      char *message = curl_easy_escape(c, error_buffer, 0);
      uw_buffer_append(buf, message, strlen(message));
      curl_free(message);
    } else
      uw_error(ctx, FATAL, "Error fetching URL: %s", error_buffer);
    return 0;
  } else {
    long http_code;
    curl_easy_getinfo(c, CURLINFO_RESPONSE_CODE, &http_code);
    uw_set_global(ctx, "world.lastErrorCode", (void *)http_code, NULL);

    if (http_code == 200 || http_code == 201 || http_code == 204) {
      uw_buffer_append(buf, "", 1);
      return 0;
    } else if (special_case_404 && http_code == 404) {
      return 1;
    } else if (encode_errors) {
      uw_buffer_reset(buf);
      uw_buffer_append(buf, server_failure, sizeof server_failure - 1);
      char *message = curl_easy_escape(c, error_buffer, 0);
      uw_buffer_append(buf, message, strlen(message));
      curl_free(message);
      return 0;
    } else {
      uw_buffer_append(buf, "", 1);
      uw_error(ctx, FATAL, "Error response #%d from remote server: %s", http_code, buf->start);
      return 0;
    }
  } 
}

static uw_Basis_string nonget(const char *verb, uw_context ctx, uw_Basis_string url, uw_WorldFfi_headers hs, uw_Basis_string bodyContentType, uw_Basis_string body) {
  uw_buffer buf;
  
  uw_Basis_string lastUrl = uw_get_global(ctx, "world.lastUrl");
  if (lastUrl && !strcmp(lastUrl, url)) {
    uw_Basis_string lastVerb = uw_get_global(ctx, "world.lastVerb");
    if (lastVerb && (verb ? !strcmp(lastVerb, verb) : !lastVerb[0])) {
      uw_Basis_string lastBody = uw_get_global(ctx, "world.lastBody");
      if (lastBody && (body ? !strcmp(lastBody, body) : !lastBody[0])) {
        uw_Basis_string lastResponse = uw_get_global(ctx, "world.lastResponse");
        if (!lastResponse)
          uw_error(ctx, FATAL, "Missing response in World cache");
        return lastResponse;
      }
    }
  }

  CURL *c = curl(ctx);

  curl_easy_reset(c);
  if (body)
    curl_easy_setopt(c, CURLOPT_POSTFIELDS, body);
  if (verb)
    curl_easy_setopt(c, CURLOPT_CUSTOMREQUEST, verb);

  struct curl_slist *slist = NULL;
  slist = curl_slist_append(slist, "User-Agent: Ur/Web World library");

  for (; hs; hs = hs->next) {
    uw_Basis_string header = uw_Basis_mstrcat(ctx, hs->header, ": ", hs->value, NULL);
    slist = curl_slist_append(slist, header);
  }

  if (bodyContentType) {
    uw_Basis_string header = uw_Basis_strcat(ctx, "Content-Type: ", bodyContentType);
    slist = curl_slist_append(slist, header);
  }

  if (slist == NULL)
    uw_error(ctx, FATAL, "Can't append to libcurl slist");

  curl_easy_setopt(c, CURLOPT_HTTPHEADER, slist);
  uw_push_cleanup(ctx, (void (*)(void *))curl_slist_free_all, slist);

  (void)doweb(ctx, &buf, c, url, 0, 0);
  uw_set_global(ctx, "world.lastUrl", strdup(url), free);
  uw_set_global(ctx, "world.lastVerb", strdup(verb ? verb : ""), free);
  uw_set_global(ctx, "world.lastBody", strdup(body ? body : ""), free);
  char *ret = strdup(buf.start);
  uw_set_global(ctx, "world.lastResponse", ret, free);
  uw_pop_cleanup(ctx);
  uw_pop_cleanup(ctx);
  return ret;
}

uw_Basis_string uw_WorldFfi_post(uw_context ctx, uw_Basis_string url, uw_WorldFfi_headers hs, uw_Basis_string bodyContentType, uw_Basis_string body) {
  return nonget(NULL, ctx, url, hs, bodyContentType, body);
}

uw_Basis_string uw_WorldFfi_put(uw_context ctx, uw_Basis_string url, uw_WorldFfi_headers hs, uw_Basis_string bodyContentType, uw_Basis_string body) {
  return nonget("PUT", ctx, url, hs, bodyContentType, body);
}

uw_Basis_string uw_WorldFfi_delete(uw_context ctx, uw_Basis_string url, uw_WorldFfi_headers hs) {
  return nonget("DELETE", ctx, url, hs, NULL, NULL);
}

uw_Basis_string uw_WorldFfi_patch(uw_context ctx, uw_Basis_string url, uw_WorldFfi_headers hs, uw_Basis_string bodyContentType, uw_Basis_string body) {
  return nonget("PATCH", ctx, url, hs, bodyContentType, body);
}

uw_Basis_string uw_WorldFfi_get(uw_context ctx, uw_Basis_string url, uw_WorldFfi_headers hs, uw_Basis_bool encode_errors) {
  uw_buffer buf;
  CURL *c = curl(ctx);

  curl_easy_reset(c);

  struct curl_slist *slist = NULL;
  slist = curl_slist_append(slist, "User-Agent: Ur/Web World library");

  for (; hs; hs = hs->next) {
    uw_Basis_string header = uw_Basis_mstrcat(ctx, hs->header, ": ", hs->value, NULL);
    slist = curl_slist_append(slist, header);
  }

  if (slist == NULL)
    uw_error(ctx, FATAL, "Can't append to libcurl slist");

  curl_easy_setopt(c, CURLOPT_HTTPHEADER, slist);
  uw_push_cleanup(ctx, (void (*)(void *))curl_slist_free_all, slist);
 
  (void)doweb(ctx, &buf, c, url, encode_errors, 0);
  uw_Basis_string ret = uw_strdup(ctx, buf.start);
  uw_pop_cleanup(ctx);
  uw_pop_cleanup(ctx);

  return ret;
}

uw_Basis_string uw_WorldFfi_getOpt(uw_context ctx, uw_Basis_string url, uw_WorldFfi_headers hs, uw_Basis_bool encode_errors) {
  uw_buffer buf;
  CURL *c = curl(ctx);
  uw_Basis_string ret;
  
  curl_easy_reset(c);

  struct curl_slist *slist = NULL;
  slist = curl_slist_append(slist, "User-Agent: Ur/Web World library");

  for (; hs; hs = hs->next) {
    uw_Basis_string header = uw_Basis_mstrcat(ctx, hs->header, ": ", hs->value, NULL);
    slist = curl_slist_append(slist, header);
  }

  if (slist == NULL)
    uw_error(ctx, FATAL, "Can't append to libcurl slist");

  curl_easy_setopt(c, CURLOPT_HTTPHEADER, slist);
  uw_push_cleanup(ctx, (void (*)(void *))curl_slist_free_all, slist);
 
  if (doweb(ctx, &buf, c, url, encode_errors, 1))
    ret = NULL;
  else
    ret = uw_strdup(ctx, buf.start);
  uw_pop_cleanup(ctx);
  uw_pop_cleanup(ctx);
  return ret;
}

uw_Basis_int uw_WorldFfi_length(uw_context ctx, uw_WorldFfi_signatur sig) {
  return sig.len;
}
uw_Basis_char uw_WorldFfi_byte(uw_context ctx, uw_WorldFfi_signatur sig, uw_Basis_int i) {
  if (i < 0 || i >= sig.len)
    uw_error(ctx, FATAL, "Referenced out-of-bounds %d in signature.", i);
  return sig.bytes[i];
}

uw_WorldFfi_signatur uw_WorldFfi_sign_rs256(uw_context ctx, uw_Basis_string key, uw_Basis_string message) {
  unsigned char digest[SHA256_DIGEST_LENGTH];
  uw_WorldFfi_signatur sig;

  if (!SHA256((const unsigned char *)message, strlen(message), digest))
    uw_error(ctx, FATAL, "World: SHA256 failed");

  BIO *bo = BIO_new(BIO_s_mem());
  if (BIO_write(bo, key, strlen(key)) <= 0) {
    BIO_free(bo);
    uw_error(ctx, FATAL, "World: BIO_write failed");
  }

  EVP_PKEY *pkey = 0;
  if (!PEM_read_bio_PrivateKey(bo, &pkey, 0, 0)) {
    BIO_free(bo);
    uw_error(ctx, FATAL, "World: PEM_read_bio_PrivateKey failed");
  }

  BIO_free(bo);

  RSA *rsa;
  if (!(rsa = EVP_PKEY_get1_RSA(pkey))) {
    EVP_PKEY_free(pkey);
    uw_error(ctx, FATAL, "World: EVP_PKEY_get1_RSA failed");
  }

  EVP_PKEY_free(pkey);

  sig.bytes = uw_malloc(ctx, RSA_size(rsa));
  if (RSA_sign(NID_sha256, digest, sizeof digest,
               (unsigned char *)sig.bytes, (unsigned int *)&sig.len, rsa) != 1) {
    RSA_free(rsa);
    uw_error(ctx, FATAL, "World: RSA_sign failed");
  }

  RSA_free(rsa);
  return sig;
}

uw_WorldFfi_signatur uw_WorldFfi_sign_hs256(uw_context ctx, uw_Basis_string key, uw_Basis_string message) {
  uw_WorldFfi_signatur sig;

  const EVP_MD *md = EVP_sha256();
  sig.bytes = uw_malloc(ctx, EVP_MD_meth_get_result_size(md));
  if (!HMAC(md, key, strlen(key), (unsigned char *)message, strlen(message),
            (unsigned char *)sig.bytes, (unsigned int *)&sig.len))
    uw_error(ctx, FATAL, "World: HMAC failed");

  return sig;
}

#define SCRYPT_OUTPUT_SIZE 32

uw_WorldFfi_signatur uw_WorldFfi_scrypt(uw_context ctx, uw_Basis_string passwd, uw_Basis_string salt) {
  uw_WorldFfi_signatur sig;
  sig.len = SCRYPT_OUTPUT_SIZE;
  sig.bytes = uw_malloc(ctx, SCRYPT_OUTPUT_SIZE);

  if (libscrypt_scrypt((const unsigned char *)passwd, strlen(passwd),
                       (const unsigned char *)salt, strlen(salt),
                       1024, 1, 1,
                       (unsigned char *)sig.bytes, SCRYPT_OUTPUT_SIZE))
    uw_error(ctx, FATAL, "World: scrypt failed");

  return sig;
}
