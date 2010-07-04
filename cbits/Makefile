CFLAGS	=$(shell sdl-config --cflags) -W
CC	= gcc

all : sdl-opengl.o

sdl-opengl.o : sdl-opengl.h sdl-opengl.c
	$(CC) $(CFLAGS) -c sdl-opengl.c -o $@

clean :
	$(RM) sdl-opengl.o
