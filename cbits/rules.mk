CFLAGS	=$(shell sdl-config --cflags) -W
CC	= gcc

all_cbits : cbits/sdl-opengl.o

cbits/sdl-opengl.o : cbits/sdl-opengl.h cbits/sdl-opengl.c
	$(CC) $(CFLAGS) -c cbits/sdl-opengl.c -o $@

clean_cbits :
	$(RM) cbits/sdl-opengl.o
