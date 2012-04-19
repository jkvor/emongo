all: emake

emake:
	erl -make

test: emake
	prove t/*.t

check: emake
	@./rebar eunit skip_deps=true

clean:
	rm -rf $(wildcard ebin/*.beam) erl_crash.dump .eunit/
