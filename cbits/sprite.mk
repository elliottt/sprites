
CFLAGS	= $(shell sdl-config --cflags) -Wall
CC	= gcc $(CFLAGS)

all_cbits : cbits/sdl-opengl.o

cbits/sdl-opengl.o : cbits/sdl-opengl.c cbits/sdl-opengl.h
	$(CC) -c cbits/sdl-opengl.c -o $@

clean_cbits :
	$(RM) cbits/sdl-opengl.o
