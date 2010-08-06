include rules.mk
include sprite.mk
include cbits/sprite.mk

$(eval $(call hi-rule))

.PHONY : all
all : all_cbits Test

.PHONY : clean
clean : clean_cbits clean_$(GHC_DIR)
