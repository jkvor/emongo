PKGNAME=emongo
ROOTDIR=`erl -eval 'io:format("~s~n", [code:root_dir()])' -s init stop -noshell`
LIBDIR=$(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)
# get application vsn from app file
VERSION=$(shell erl -pa ebin/ -eval 'application:load(${PKGNAME}), {ok, Vsn} = application:get_key(${PKGNAME}, vsn), io:format("~s~n", [Vsn])' -s init stop -noshell)

all: src

src: FORCE
	@erl -make

test: src
	prove t/*.t

clean:
	rm -rf erl_crash.dump *.boot *.rel *.script ebin/*.beam

package: clean
	@mkdir $(PKGNAME)-$(VERSION)/ && cp -rf ebin include Emakefile Makefile priv README.markdown src t $(PKGNAME)-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf $(PKGNAME)-$(VERSION).tgz $(PKGNAME)-$(VERSION)
	@rm -rf $(PKGNAME)-$(VERSION)/

install: src
	@mkdir -p $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/{ebin,include}
	for i in ebin/*.beam include/*.hrl ebin/*.app; do install $$i $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/$$i ; done


plt: src
	@dialyzer --check_plt -q -r . -I include/

check: src
	@dialyzer --src -r . -I include/

FORCE: