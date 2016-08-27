#include <ctype.h>
#include <string.h>

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

uw_Basis_string uw_WorldFfi_post(uw_context ctx, uw_Basis_string url, uw_Basis_string body) {
  if (strncmp(url, "https://", 8))
    uw_error(ctx, FATAL, "World: POST URL is not HTTPS");

  uw_buffer *buf = uw_malloc(ctx, sizeof(uw_buffer));
  char error_buffer[CURL_ERROR_SIZE];
  CURL *c = curl(ctx);
  CURLcode code;

  uw_buffer_init(BUF_MAX, buf, BUF_INIT);

  curl_easy_reset(c);
  curl_easy_setopt(c, CURLOPT_URL, url);
  curl_easy_setopt(c, CURLOPT_POSTFIELDS, body);
  curl_easy_setopt(c, CURLOPT_WRITEFUNCTION, write_buffer_data);
  curl_easy_setopt(c, CURLOPT_WRITEDATA, buf);
  curl_easy_setopt(c, CURLOPT_ERRORBUFFER, error_buffer);

  code = curl_easy_perform(c);

  if (code) {
    uw_buffer_reset(buf);
    uw_buffer_append(buf, curl_failure, sizeof curl_failure - 1);
    char *message = curl_easy_escape(c, error_buffer, 0);
    uw_buffer_append(buf, message, strlen(message));
    curl_free(message);
  } else
    uw_buffer_append(buf, "", 1);

  char *ret = uw_strdup(ctx, buf->start);
  uw_buffer_free(buf);
  return ret;
}
