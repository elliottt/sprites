ifneq "$(V)" ""
	QUIET_GHC	=
	QUIET_LINK	=
	QUIET_DEPEND	=
else
	QUIET_GHC	= @echo '   GHC  $@';
	QUIET_LINK	= @echo '   LINK $@';
	QUIET_DEPEND	= @echo '   DEPEND';
	GHC_VERBOSITY	= -v0
endif

GHC		= ghc -odir ghc -hidir ghc -Wall -ighc $(GHC_VERBOSITY)
HS_SOURCES	= Animation.hs         \
		  Dungeon.hs           \
		  Event.hs             \
		  Gen.hs               \
		  Graphics.hs          \
		  Math/Line.hs         \
		  Math/Matrix.hs       \
		  Math/Normalize.hs    \
		  Math/Point.hs        \
		  Math/Utils.hs        \
		  Physics/AABB.hs      \
		  Physics/Body.hs      \
		  Physics/Collision.hs \
		  Physics/Shape.hs     \
		  Physics/Vector.hs    \
		  Physics/World.hs     \
		  Time.hs
TARGETS		= Test
OBJS		= $(patsubst %.hs,ghc/%.o,$(HS_SOURCES))
PACKAGES	= $(addprefix -package ,OpenGL SDL SDL-image monadLib \
		                        stm containers json)

.PHONY : all_Test
all_Test : ghc ghc/depend $(TARGETS)

Test : cbits/sdl-opengl.o $(OBJS) ghc/Test.o
	$(QUIET_LINK)$(GHC) -o $@ $^ $(PACKAGES)

ghc :
	mkdir ghc

clean_Test :
	$(RM) -r ghc

ghc/depend : ghc $(HS_SOURCES) $(addsuffix .hs,$(TARGETS))
	$(QUIET_DEPEND)$(GHC) -M $(HS_SOURCES) $(addsuffix .hs,$(TARGETS)) \
		-dep-makefile $@

-include ghc/depend
ghc/%.hi : %.hs ghc/%.o
	$(QUIET_GHC)$(GHC) -c $<

ghc/%.o : %.hs
	$(QUIET_GHC)$(GHC) -c $<

define main-target
ghc/$1.o : $1.hs
	$$(QUIET_GHC)$(GHC) -main-is $1.main -c $1.hs

endef

$(eval $(foreach t,$(TARGETS),$(call main-target,$(t))))


