# Makefile for DadaDodo, Copyright (c) 1997 Jamie Zawinski.

SHELL		= /bin/sh
CC		= gcc -Wall -Wstrict-prototypes -Wnested-externs -Wno-format
#CC		= cc -fullwarn -use_readonly_const -rdata_shared -g3
CFLAGS		= -g
LDFLAGS		= 
DEFS		= -DGETTIMEOFDAY_TWO_ARGS -DHAVE_UNISTD_H
LIBS		= 


DEPEND		= makedepend
DEPEND_FLAGS	= 
DEPEND_DEFINES	= 

srcdir		= .
INCLUDES	= -I$(srcdir)

SRCS		= dadadodo.c parse.c generate.c files.c hash.c yarandom.c
OBJS		= dadadodo.o parse.o generate.o files.o hash.o yarandom.o
EXE		= dadadodo

HDRS		= dadadodo.h parse.h parsei.h files.h generate.h hash.h \
		  yarandom.h version.h
MEN		= 
EXTRAS		= Makefile README dodotodo

TARFILES	= $(EXTRAS) $(SRCS) $(HDRS) $(MEN)
TAR		= gtar
COMPRESS	= gzip --verbose --best
COMPRESS_EXT	= gz

all: $(EXE)

clean:
	-rm -f *.o a.out core $(EXE)

distclean: clean
	-rm -f *~ "#"*

depend:
	$(DEPEND) -s '# DO NOT DELETE: updated by make depend'		   \
	$(DEPEND_FLAGS) -- $(INCLUDES) $(DEFS) $(DEPEND_DEFINES) $(CFLAGS) \
	-- $(SRCS)

TAGS: tags
tags:
	find $(srcdir) -name '*.[chly]' -print | xargs etags -a

.c.o:
	$(CC) -c $(INCLUDES) $(DEFS) $(CFLAGS) $<

$(EXE): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

dadadodo.o: dadadodo.h hash.h parse.h generate.h version.h
generate.o: dadadodo.h hash.h parse.h generate.h
parse.o:    dadadodo.h hash.h parse.h
hash.o:     hash.h

# This really makes me sick...
tar:
	@NAME=`sed -n							    \
  's/.* \([0-9]\.[0-9][0-9]*\).*/dadadodo-\1/p' version.h` ;		    \
  rm -f $$NAME ; ln -s . $$NAME ;					    \
  echo creating tar file $${NAME}.tar.$(COMPRESS_EXT)... ;		    \
   $(TAR) -vchf - `echo $(TARFILES)				    	    \
   | sed "s|^|$$NAME/|g; s| | $$NAME/|g" `				    \
   | $(COMPRESS) > $${NAME}.tar.$(COMPRESS_EXT) ;			    \
  rm $$NAME ;								    \
  echo "" ;								    \
  ls -lgF $${NAME}.tar.$(COMPRESS_EXT) ;				    \
  echo "" ;

