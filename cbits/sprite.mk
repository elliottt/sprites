
CFLAGS	= $(shell sdl-config --cflags) -Wall
CC	= gcc $(CFLAGS)

cbits_all : cbits/sdl-opengl.o

cbits/sdl-opengl.o : cbits/sdl-opengl.c cbits/sdl-opengl.h
	$(CC) -c cbits/sdl-opengl.c -o $@

cbits_clean :
	$(RM) cbits/sdl-opengl.o
