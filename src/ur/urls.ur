fun hexchar n =
    if n < 0 then
        error <xml>Urls.hexchar: negative</xml>
    else if n < 10 then
        show n
    else if n < 16 then
        str1 (chr (ord #"A" + n - 10))
    else
        error <xml>Urls.hexchar: too big</xml>

fun unhexchar ch =
    if Char.isDigit ch then
        ord ch - ord #"0"
    else if Char.isXdigit ch then
        ord ch - ord #"A" + 10
    else
        error <xml>Urls.unhexchar: invalid</xml>

fun ord' ch =
    let
        val n = ord ch
    in
        if n < 0 then
            256 + n
        else
            n
    end

fun urlencode s =
    let
        fun loop s acc =
            if strlenUtf8 s = 0 then
                acc
            else
                let
                    val ch = strsubUtf8 s 0

                    val ch' = if Char.isAlnum ch || ch = #"_" || ch = #"-" || ch = #"." then
                                  str1 ch
                              else
                                  "%" ^ hexchar (ord' ch / 16) ^ hexchar (ord' ch % 16)
                in
                    loop (strsuffixUtf8 s 1) (acc ^ ch')
                end
    in
        loop s ""
    end

fun urldecode s =
    let
        fun loop s acc =
            if String.length s = 0 then
                acc
            else if String.sub s 0 = #"+" then
                loop (String.suffix s 1) (acc ^ " ")
            else if String.sub s 0 = #"%" && String.length s >= 3 then
                loop (String.suffix s 3) (acc ^ str1 (chr (16 * unhexchar (String.sub s 1)
                                                           + unhexchar (String.sub s 2))))
            else
                loop (String.suffix s 1) (acc ^ str1 (String.sub s 0))
    in
        loop s ""
    end

fun base64url_encode' (getChar : int -> char) (urlVersion : bool) (len : int) =
    let
        fun char n =
            String.str (Char.fromInt (if n < 0 then
                                          error <xml>Negative character to base64 encode</xml>
                                      else if n < 26 then
                                          Char.toInt #"A" + n
                                      else if n < 52 then
                                          Char.toInt #"a" + (n - 26)
                                      else if n < 62 then
                                          Char.toInt #"0" + (n - 52)
                                      else if n = 62 then
                                          Char.toInt (if urlVersion then #"-" else #"+")
                                      else if n = 63 then
                                          Char.toInt (if urlVersion then #"_" else #"/")
                                      else
                                          error <xml>Invalid base64 digit</xml>))

        fun ch j =
            let
                val n = Char.toInt (getChar j)
            in
                if n < 0 then
                    n + 256
                else
                    n
            end

        fun bytes i acc =
            if i >= len then
                acc
            else if i = len - 1 then
                let
                    val n = ch i * 16
                in
                    acc
                    ^ char (n / 64)
                    ^ char (n % 64)
                    ^ "=="
                end
            else if i = len - 2 then
                let
                    val n1 = ch i
                    val n2 = ch (i + 1)
                    val n = n1 * (256 * 4) + n2 * 4
                in
                    acc
                    ^ char (n / (64 * 64))
                    ^ char (n / 64 % 64)
                    ^ char (n % 64)
                    ^ "="
                end
            else
                let
                    val n1 = ch i
                    val n2 = ch (i + 1)
                    val n3 = ch (i + 2)
                    val n = n1 * (256 * 256) + n2 * 256 + n3
                in
                    bytes (i + 3) (acc
                                   ^ char (n / (64 * 64 * 64))
                                   ^ char (n / (64 * 64) % 64)
                                   ^ char (n / 64 % 64)
                                   ^ char (n % 64))
                end
    in
        bytes 0 ""
    end

fun base64url_encode s = base64url_encode' (String.sub s) True (String.length s)
fun base64url_encode_signature s = base64url_encode' (WorldFfi.byte s) True (WorldFfi.length s)
fun base64_encode_signature s = base64url_encode' (WorldFfi.byte s) False (WorldFfi.length s)
