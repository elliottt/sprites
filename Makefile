
.PHONY : all
all : all_cbits Test
	echo $^

include rules.mk
include sprite.mk
include cbits/sprite.mk

.PHONY : clean
clean : clean_cbits clean_$(GHC_DIR)
