GHC_DIR	= ghc
GHC	= ghc -odir $(GHC_DIR) -hidir $(GHC_DIR) -i$(GHC_DIR)

$(GHC_DIR) :
	mkdir $@

clean_$(GHC_DIR) :
	$(RM) -r $(GHC_DIR)

define hi-rule
%.hi : %.o ;
endef
