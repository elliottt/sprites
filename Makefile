include rules.mk

$(eval $(call hi-rule))

include sprite.mk
include cbits/sprite.mk

.DEFAULT : all
.PHONY : all
all : cbits_all Test
	echo $^

.PHONY : clean
clean : cbits_all clean_$(GHC_DIR)


