GHC		= ghc -odir=ghc -hidir=ghc -ighc
HS_SOURCES	= Animation.hs Graphics.hs Position.hs Render.hs \
		  Sprite.hs Time.hs Event.hs Dungeon.hs
TARGETS		= Test Draw
OBJS		= $(patsubst %.hs,ghc/%.o,$(HS_SOURCES))
PACKAGES	= $(addprefix -package ,OpenGL SDL SDL-image stm containers)

.PHONY : all_Test
all_Test : ghc ghc/depend Test Draw

Test : cbits/sdl-opengl.o $(OBJS) ghc/Test.o
	$(GHC) -o $@ $^ $(PACKAGES)

Draw : cbits/sdl-opengl.o $(OBJS) ghc/Draw.o
	$(GHC) -main-is Draw -o $@  ghc/Draw.o cbits/sdl-opengl.o $(OBJS) $(PACKAGES)

ghc :
	mkdir ghc

clean_Test :
	$(RM) -r ghc

ghc/%.o: %.hs
	$(GHC) -c $<

ghc/%.hi: %.hs
	$(GHC) -c $<

define main-target
ghc/$1.o : $1.hs
	$(GHC) -main-is $1.main -c $1.hs

endef

$(eval $(foreach t,$(TARGETS),$(call main-target,$(t))))

ghc/depend : ghc
	$(GHC) -M $(HS_SOURCES) $(addsuffix .hs,$(TARGETS)) -dep-makefile $@

-include ghc/depend
