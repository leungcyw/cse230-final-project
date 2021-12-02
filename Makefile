install:
	stack install elsa
clean:
	stack clean --allow-different-user
run:
	stack install elsa
	~/.local/bin/elsa
unit:
	stack test --test-arguments="--num-threads 1"