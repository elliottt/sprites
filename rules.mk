GHC_DIR	= ghc
GHC	= ghc -odir $(GHC_DIR) -hidir $(GHC_DIR) -i$(GHC_DIR) -Wall

$(GHC_DIR) :
	mkdir $(GHC_DIR)

clean_$(GHC_DIR) :
	$(RM) -r $(GHC_DIR)

%.hi : %.o ;
