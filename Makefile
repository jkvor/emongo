all: emake

emake:
	erl -make

test: emake
	prove t/*.t

clean:
	rm -rf $(wildcard ebin/*.beam) erl_crash.dump