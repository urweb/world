ACLOCAL_AMFLAGS = -I m4

LIB      = @LIB@
INCLUDE  = @INCLUDE@

LIB_UR   = $(LIB)/ur/world

SUBDIRS  = src/c

install-exec-local:
	mkdir -p $(LIB_UR)
	cp src/ur/*.urs $(LIB_UR)/
	cp src/ur/*.ur $(LIB_UR)/
	cp src/ur/*.urp $(LIB_UR)/
	mkdir -p $(INCLUDE)
	cp include/*.h $(INCLUDE)/
	-ldconfig
