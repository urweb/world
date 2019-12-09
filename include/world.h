#include <urweb.h>

typedef struct {
  int len;
  char *bytes;
} uw_WorldFfi_signatur;

uw_Basis_int uw_WorldFfi_length(uw_context ctx, uw_WorldFfi_signatur);
uw_Basis_char uw_WorldFfi_byte(uw_context ctx, uw_WorldFfi_signatur, uw_Basis_int);
uw_WorldFfi_signatur uw_WorldFfi_sign(uw_context ctx, uw_Basis_string key, uw_Basis_string message);

uw_Basis_string uw_WorldFfi_get(uw_context ctx, uw_Basis_string url, uw_Basis_string auth);
uw_Basis_string uw_WorldFfi_post(uw_context ctx, uw_Basis_string url, uw_Basis_string auth, uw_Basis_string bodyContentType, uw_Basis_string body);
uw_Basis_string uw_WorldFfi_put(uw_context ctx, uw_Basis_string url, uw_Basis_string auth, uw_Basis_string bodyContentType, uw_Basis_string body);
uw_Basis_string uw_WorldFfi_delete(uw_context ctx, uw_Basis_string url, uw_Basis_string auth);
