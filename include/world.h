#include <urweb.h>

typedef struct uw_WorldFfi_headers *uw_WorldFfi_headers;

const uw_WorldFfi_headers uw_WorldFfi_emptyHeaders;
uw_WorldFfi_headers uw_WorldFfi_addHeader(uw_context, uw_WorldFfi_headers, uw_Basis_string header, uw_Basis_string value);

typedef struct {
  int len;
  char *bytes;
} uw_WorldFfi_signatur;

uw_Basis_int uw_WorldFfi_length(uw_context, uw_WorldFfi_signatur);
uw_Basis_char uw_WorldFfi_byte(uw_context, uw_WorldFfi_signatur, uw_Basis_int);
uw_WorldFfi_signatur uw_WorldFfi_sign_rs256(uw_context, uw_Basis_string key, uw_Basis_string message);
uw_WorldFfi_signatur uw_WorldFfi_sign_hs256(uw_context, uw_Basis_string key, uw_Basis_string message);
uw_WorldFfi_signatur uw_WorldFfi_scrypt(uw_context, uw_Basis_string passwd, uw_Basis_string salt);

uw_Basis_int uw_WorldFfi_lastErrorCode(uw_context);
uw_Basis_string uw_WorldFfi_get(uw_context, uw_Basis_string url, uw_WorldFfi_headers, uw_Basis_bool encode_errors);
uw_Basis_string uw_WorldFfi_getOpt(uw_context, uw_Basis_string url, uw_WorldFfi_headers, uw_Basis_bool encode_errors);
uw_Basis_string uw_WorldFfi_post(uw_context, uw_Basis_string url, uw_WorldFfi_headers, uw_Basis_string bodyContentType, uw_Basis_string body);
uw_Basis_string uw_WorldFfi_put(uw_context, uw_Basis_string url, uw_WorldFfi_headers, uw_Basis_string bodyContentType, uw_Basis_string body);
uw_Basis_string uw_WorldFfi_delete(uw_context, uw_Basis_string url, uw_WorldFfi_headers);
uw_Basis_string uw_WorldFfi_patch(uw_context, uw_Basis_string url, uw_WorldFfi_headers, uw_Basis_string bodyContentType, uw_Basis_string body);

uw_Basis_unit uw_WorldFfi_allowHttp(uw_context);
