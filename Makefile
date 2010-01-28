PKGNAME=emongo
ROOTDIR=`erl -eval 'io:format("~s~n", [code:root_dir()])' -s init stop -noshell`
LIBDIR=$(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)
# get application vsn from app file
VERSION=$(shell erl -pa ebin/ -eval 'application:load(erldis), {ok, Vsn} = application:get_key(erldis, vsn), io:format("~s~n", [Vsn])' -s init stop -noshell)

all: src

src: FORCE
	@erl -make

test: src
	prove t/*.t

clean:
	rm -rf erl_crash.dump *.boot *.rel *.script ebin/*.beam

package: clean
	@mkdir $(PKGNAME)-$(VERSION)/ && cp -rf ebin include Makefile priv README.markdown src support t $(PKGNAME)-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf $(PKGNAME)-$(VERSION).tgz $(PKGNAME)-$(VERSION)
	@rm -rf $(PKGNAME)-$(VERSION)/
	
install:
	@mkdir -p $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/{ebin,include}
	@mkdir -p $(prefix)/$(ROOTDIR)/bin
	for i in ebin/*.beam include/*.hrl ebin/*.app; do install $$i $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/$$i ; done
	cp *.boot $(prefix)/$(ROOTDIR)/bin/

plt:
	@dialyzer --build_plt --output_plt .plt -q -r . -I include/

check: all
	@dialyzer --check_plt --plt .plt -q -r . -I include/

FORCE: