include ../support/include.mk

all: $(EBIN_FILES)

clean:
	rm -rf $(EBIN_FILES) erl_crash.dump

test: $(MODULES)

./$(MODULES):
	@echo "Running tests for $@"
	erl -pa ../ebin -run $@ start -run init stop -noshell

#5: $(EBIN_FILES)
#	erl -pa ../ebin -run etap_t_005 start -run init stop -noshell

_%:
	$(ERLC) $(ERLC_FLAGS) -o . etap_t$@.erl
	erl -pa ../ebin -run etap_t$@ start -run  init stop -noshell

	
