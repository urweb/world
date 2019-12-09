lib_LTLIBRARIES = liburweb_world.la

liburweb_world_la_SOURCES = world.c
liburweb_world_la_LDFLAGS = $(OPENSSL_LDFLAGS)

AM_CPPFLAGS = -I../../include -I@INCLUDE@ $(OPENSSL_INCLUDES)
AM_CFLAGS = -Wimplicit -Wall -Werror -Wno-deprecated-declarations
