ifneq "$(V)" ""
	QUIET_CC	=
else
	QUIET_CC	= @echo '   CC   $@';
endif

CFLAGS	=$(shell sdl-config --cflags) -W
CC	= gcc

all_cbits : cbits/sdl-opengl.o

cbits/sdl-opengl.o : cbits/sdl-opengl.h cbits/sdl-opengl.c
	$(QUIET_CC)$(CC) $(CFLAGS) -c cbits/sdl-opengl.c -o $@

clean_cbits :
	$(RM) cbits/sdl-opengl.o
