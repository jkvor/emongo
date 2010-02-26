VERSION=0.0.1
PKGNAME=emongo
LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`

all: emake

emake: app
	erl -make

app:
	sh ebin/$(PKGNAME).app.in $(VERSION)
	
release: emake release/$(PKGNAME).tar.gz
	
release/$(PKGNAME).rel release/$(PKGNAME).script release/$(PKGNAME).tar.gz: app
	mkdir -p release
	escript build_rel.escript $(PKGNAME)

test: emake
	prove t/*.t

clean:
	rm -rf $(wildcard ebin/*.beam) erl_crash.dump *.boot *.rel *.script ebin/*.app release
	
install:
	@mkdir -p $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/{ebin,include}
	for i in ebin/*.beam include/*.hrl ebin/*.app; do install $$i $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/$$i ; done