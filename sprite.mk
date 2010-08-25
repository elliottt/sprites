
Test_SLASH_MODS := Event             \
		   Graphics          \
		   Math/AffinePlane  \
		   Math/Utils        \
		   Physics/AABB      \
		   Physics/Body      \
		   Physics/Shape     \
		   Physics/World     \
		   Time              \
		   Utils

Test_HS_SOURCES	:= $(addprefix src/,$(addsuffix .hs,$(Test_SLASH_MODS)))
Test_HS_OBJS	:= $(addprefix $(GHC_DIR)/,$(addsuffix .o,$(Test_SLASH_MODS)))
Test_HS_LIBS	:= -package OpenGL -package containers -package SDL \
		   -package SDL-image -package vector -package stm

-include $(GHC_DIR)/Test-depend

$(GHC_DIR)/%.o : src/%.hs
	echo $@
	$(GHC) $(Test_HS_LIBS) -c $<

$(GHC_DIR)/Test-depend : $(GHC_DIR)
	$(GHC) -M -dep-makefile $@ $(Test_HS_SOURCES)

Test : cbits/sdl-opengl.o $(GHC_DIR)/Test-depend $(Test_HS_OBJS) Test.hs
	$(GHC) -main-is Test.main -c Test.hs
	$(GHC) $(Test_HS_LIBS) -main-is Test:main \
	    -o $@ $(Test_HS_OBJS) ghc/Test.o cbits/sdl-opengl.o

clean_Test :
	$(RM) Test
