install:
	stack install elsa
clean:
	stack clean --allow-different-user
test:
	stack install elsa
	~/.local/bin/elsa