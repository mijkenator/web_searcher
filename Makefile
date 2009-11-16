LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`

all:
	mkdir -p ebin
	(cd src;$(MAKE))

doc:
	(cd src; $(MAKE) doc)

test: all
	(cd t;$(MAKE))
	(cd t;$(MAKE) test)

prove: all
	(cd t;$(MAKE))
	prove t/*.t

clean:
	(cd src;$(MAKE) clean)
	(cd t;$(MAKE) clean)
	rm -rf cover/

