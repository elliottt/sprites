
Test_SLASH_MODS := Math/AffinePlane  \
		   Math/Utils        \
		   Physics/Collision \
		   Physics/AABB      \
		   Physics/Body      \
		   Physics/World     \
		   Physics/Shape     \
		   Time              \
		   Event             \
		   Graphics

Test_HS_SOURCES	:= $(addprefix src/,$(addsuffix .hs,$(Test_SLASH_MODS)))
Test_HS_OBJS	:= $(patsubst src/%.hs,$(GHC_DIR)/%.o,$(Test_HS_SOURCES))

$(GHC_DIR)/%.o : src/%.hs
	$(GHC) -c $<

$(GHC_DIR)/Test-depend :
	$(GHC) -M -dep-makefile $@ $(Test_HS_SOURCES)

Test : cbits/sdl-opengl.o $(GHC_DIR) $(Test_HS_OBJS) Test.hs
	$(GHC) --make Test.hs cbits/sdl-opengl.o

clean_Test :
	$(RM) Test

-include $(GHC_DIR)/Test-depend
