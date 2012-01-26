JSON=jiffy
HTTP=cowboy

.PHONY: all deps test test-prep clean distclean

all: deps deps/$(HTTP)
	make -C deps/$(HTTP)
	./rebar compile

deps:
	./rebar get-deps

test: all
	erl -pa ebin deps/*/ebin \
		-sockjs json_impl $(JSON) \
		-sockjs http_impl $(HTTP) \
		-run sockjs_test

deps/misultin:
	-mkdir -p deps
	cd deps && \
		git clone -b dev https://github.com/ostinelli/misultin.git

deps/cowboy:
	-mkdir -p deps
	cd deps && \
		git clone -b master https://github.com/extend/cowboy.git

clean::
	./rebar clean
	rm -rf priv/www

distclean::
	rm -rf deps priv
