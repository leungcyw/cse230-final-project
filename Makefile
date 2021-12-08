.PHONY: all test clean ghcid distclean

install:
	stack install elsa
clean:
	stack clean --allow-different-user
run:
	stack run
test:
	stack test --test-arguments="--num-threads 1"