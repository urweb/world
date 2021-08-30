fun scrypt pw sa =
    Urls.base64url_encode_signature (WorldFfi.scrypt pw sa)
