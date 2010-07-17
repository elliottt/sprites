GHC		= ghc -odir=ghc -hidir=ghc -ighc
HS_SOURCES	= Animation.hs Graphics.hs Position.hs Render.hs \
		  Sprite.hs Main.hs Time.hs
OBJS		= $(patsubst %.hs,ghc/%.o,$(HS_SOURCES))
PACKAGES	= $(addprefix -package ,OpenGL SDL SDL-image)

.PHONY : all_Test
all_Test : ghc ghc/depend Test

Test : cbits/sdl-opengl.o $(OBJS)
	$(GHC) -o Test $(OBJS) cbits/sdl-opengl.o $(PACKAGES)

ghc :
	mkdir ghc

clean_Test :
	$(RM) -r ghc

ghc/%.o: %.hs
	$(GHC) -c $<

ghc/%.hi: %.hs
	$(GHC) -c $<

ghc/depend : ghc
	$(GHC) -M $(HS_SOURCES) -dep-makefile $@

-include ghc/depend
