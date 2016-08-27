fun hexchar n =
    if n < 0 then
        error <xml>Oauth.hexchar: negative</xml>
    else if n < 10 then
        show n
    else if n < 16 then
        str1 (chr (ord #"A" + n - 10))
    else
        error <xml>Oauth.hexchar: too big</xml>

fun unhexchar ch =
    if Char.isDigit ch then
        ord ch - ord #"0"
    else if Char.isXdigit ch then
        ord ch - ord #"A" + 10
    else
        error <xml>Oauth.unhexchar: invalid</xml>

fun urlencode s =
    let
        fun loop s acc =
            if String.length s = 0 then
                acc
            else
                let
                    val ch = String.sub s 0

                    val ch' = if Char.isAlnum ch then
                                  str1 ch
                              else
                                  "%" ^ hexchar (ord ch / 16) ^ hexchar (ord ch % 16)
                in
                    loop (String.suffix s 1) (acc ^ ch')
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
