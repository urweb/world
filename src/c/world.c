#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include <curl/curl.h>

#include <urweb.h>
#include <world.h>

#define BUF_MAX 10240
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

static size_t write_buffer_data(void *buffer, size_t size, size_t nmemb, void *userp) {
  uw_buffer *buf = userp;

  uw_buffer_append(buf, buffer, size * nmemb);

  return size * nmemb;
}

static const char curl_failure[] = "error=fetch_url&error_description=";
static const char server_failure[] = "error=fetch_url&error_description=";

static uw_Basis_string doweb(uw_context ctx, CURL *c, uw_Basis_string url, int encode_errors) {
  if (strncmp(url, "https://", 8))
    uw_error(ctx, FATAL, "World: URL is not HTTPS");

  uw_buffer buf;
  char error_buffer[CURL_ERROR_SIZE];
  CURLcode code;

  uw_buffer_init(BUF_MAX, &buf, BUF_INIT);
  uw_push_cleanup(ctx, (void (*)(void *))uw_buffer_free, &buf);

  curl_easy_setopt(c, CURLOPT_URL, url);
  curl_easy_setopt(c, CURLOPT_WRITEFUNCTION, write_buffer_data);
  curl_easy_setopt(c, CURLOPT_WRITEDATA, &buf);
  curl_easy_setopt(c, CURLOPT_ERRORBUFFER, error_buffer);

  code = curl_easy_perform(c);

  if (code) {
    if (encode_errors) {
      uw_buffer_reset(&buf);
      uw_buffer_append(&buf, curl_failure, sizeof curl_failure - 1);
      char *message = curl_easy_escape(c, error_buffer, 0);
      uw_buffer_append(&buf, message, strlen(message));
      curl_free(message);
    } else
      uw_error(ctx, FATAL, "Error fetching URL: %s", error_buffer);
  } else {
    long http_code;
    curl_easy_getinfo(c, CURLINFO_RESPONSE_CODE, &http_code);

    if (http_code == 200)
      uw_buffer_append(&buf, "", 1);
    else if (encode_errors) {
      uw_buffer_reset(&buf);
      uw_buffer_append(&buf, server_failure, sizeof server_failure - 1);
      char *message = curl_easy_escape(c, error_buffer, 0);
      uw_buffer_append(&buf, message, strlen(message));
      curl_free(message);
    } else {
      uw_buffer_append(&buf, "", 1);
      uw_error(ctx, FATAL, "Error response from remote server: %s", buf.start);
    }
  } 

  char *ret = uw_strdup(ctx, buf.start);
  uw_pop_cleanup(ctx);
  return ret;
}

uw_Basis_string uw_WorldFfi_post(uw_context ctx, uw_Basis_string url, uw_Basis_string body) {
  uw_Basis_string lastUrl = uw_get_global(ctx, "world.lastUrl");
  if (lastUrl && !strcmp(lastUrl, url)) {
      uw_Basis_string lastBody = uw_get_global(ctx, "world.lastBody");
      if (lastBody && !strcmp(lastBody, body)) {
        uw_Basis_string lastResponse = uw_get_global(ctx, "world.lastResponse");
        if (!lastResponse)
          uw_error(ctx, FATAL, "Missing response in World cache");
        return lastResponse;
      }
  }

  CURL *c = curl(ctx);

  curl_easy_reset(c);
  curl_easy_setopt(c, CURLOPT_POSTFIELDS, body);

  uw_Basis_string ret = doweb(ctx, c, url, 1);
  uw_set_global(ctx, "world.lastUrl", strdup(url), free);
  uw_set_global(ctx, "world.lastBody", strdup(body), free);
  uw_set_global(ctx, "world.lastResponse", strdup(ret), free);
  return ret;
}

uw_Basis_string uw_WorldFfi_get(uw_context ctx, uw_Basis_string url, uw_Basis_string auth) {
  CURL *c = curl(ctx);

  curl_easy_reset(c);

  struct curl_slist *slist = NULL;
  slist = curl_slist_append(slist, "User-Agent: Ur/Web World library");

  if (auth) {
    uw_Basis_string header = uw_Basis_strcat(ctx, "Authorization: ", auth);
    slist = curl_slist_append(slist, header);
  }

  if (slist == NULL)
    uw_error(ctx, FATAL, "Can't append to libcurl slist");

  curl_easy_setopt(c, CURLOPT_HTTPHEADER, slist);
  uw_push_cleanup(ctx, (void (*)(void *))curl_slist_free_all, slist);
 
  uw_Basis_string ret = doweb(ctx, c, url, 0);
  uw_pop_cleanup(ctx);

  return ret;
}
