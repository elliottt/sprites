
all :
	make -C cbits all
	ghc --make Test cbits/sdl-opengl.o

clean :
	make -C cbits clean
	$(RM) *.hi *.o Test
