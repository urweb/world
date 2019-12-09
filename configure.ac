AC_INIT([urweb_world], [1.0])
AC_USE_SYSTEM_EXTENSIONS
AM_INIT_AUTOMAKE([-Wall -Werror foreign no-define])
AC_PROG_CC()
AM_PROG_AR()
AC_PROG_LIBTOOL()
AC_CONFIG_MACRO_DIR([m4])

AX_CHECK_OPENSSL([echo >/dev/null], [echo "You must install OpenSSL development files."; exit 1])

LIBCURL_CHECK_CONFIG(yes, [], [echo >/dev/null], [echo "You must install libcurl development files."; exit 1])

if test [$prefix = "NONE"]; then
    prefix=/usr/local
fi

if test [-z $LIB]; then
    LIB=$prefix/lib/urweb
fi

if test [-z $INCLUDE]; then
    INCLUDE=$prefix/include/urweb
fi

AC_SUBST(LIB)
AC_SUBST(INCLUDE)

AC_CONFIG_FILES([
        Makefile
        src/c/Makefile
        src/ur/config.urp
])

AC_OUTPUT()

cat <<EOF

Ur/Web World library configuration:
  lib directory:       LIB            $LIB
  include directory:   INCLUDE        $INCLUDE
EOF
