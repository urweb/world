#include <urweb.h>

uw_Basis_string uw_WorldFfi_get(uw_context ctx, uw_Basis_string url, uw_Basis_string auth);
uw_Basis_string uw_WorldFfi_post(uw_context ctx, uw_Basis_string url, uw_Basis_string auth, uw_Basis_string bodyContentType, uw_Basis_string body);
uw_Basis_string uw_WorldFfi_put(uw_context ctx, uw_Basis_string url, uw_Basis_string auth, uw_Basis_string bodyContentType, uw_Basis_string body);
uw_Basis_string uw_WorldFfi_delete(uw_context ctx, uw_Basis_string url, uw_Basis_string auth);
