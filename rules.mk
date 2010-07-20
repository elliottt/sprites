ifneq "$(V)" ""
	QUIET_GHC	=
	QUIET_LINK	=
	QUIET_DEPEND	=
else
	QUIET_GHC	= @echo '   GHC  $@';
	QUIET_LINK	= @echo '   LINK $@';
	QUIET_DEPEND	= @echo '   DEPEND';
endif

GHC		= ghc -odir=ghc -hidir=ghc -ighc -Wall
HS_SOURCES	= Animation.hs Graphics.hs Position.hs Render.hs \
		  Sprite.hs Time.hs Event.hs Dungeon.hs
TARGETS		= Test Draw
OBJS		= $(patsubst %.hs,ghc/%.o,$(HS_SOURCES))
PACKAGES	= $(addprefix -package ,OpenGL SDL SDL-image stm containers)

.PHONY : all_Test
all_Test : ghc ghc/depend Test Draw

Test : cbits/sdl-opengl.o $(OBJS) ghc/Test.o
	$(QUIET_LINK)$(GHC) -o $@ $^ $(PACKAGES)

Draw : cbits/sdl-opengl.o $(OBJS) ghc/Draw.o
	$(QUIET_LINK)$(GHC) -main-is Draw -o $@  ghc/Draw.o cbits/sdl-opengl.o \
		$(OBJS) $(PACKAGES)

ghc :
	mkdir ghc

clean_Test :
	$(RM) -r ghc

ghc/%.o: %.hs
	$(QUIET_GHC)$(GHC) -c $<

ghc/%.hi: %.hs
	$(QUIET_GHC)$(GHC) -c $<

define main-target
ghc/$1.o : $1.hs
	$$(QUIET_GHC)$(GHC) -main-is $1.main -c $1.hs

endef

$(eval $(foreach t,$(TARGETS),$(call main-target,$(t))))

ghc/depend : ghc
	$(QUIET_DEPEND)$(GHC) -M $(HS_SOURCES) $(addsuffix .hs,$(TARGETS)) \
		-dep-makefile $@

-include ghc/depend
