JSON=jiffy
HTTP=cowboy

.PHONY: all deps test test-prep clean distclean

all: deps deps/$(HTTP)
	make -C deps/$(HTTP)
	./rebar compile

deps:
	./rebar get-deps

test: test-prep all
	erl -pa ebin deps/*/ebin \
		-sockjs json_impl $(JSON) \
		-sockjs http_impl $(HTTP) \
		-run sockjs_test

test-prep: deps/sockjs-client priv/www
	cd deps/sockjs-client && npm install
	coffee -v > /dev/null
	make -C deps/sockjs-client tests/html/lib/sockjs.js tests/html/lib/tests.js

priv/www:
	-mkdir -p priv
	ln -s ../deps/sockjs-client/tests/html priv/www

deps/sockjs-client:
	-mkdir -p deps
	cd deps && \
		git clone https://github.com/sockjs/sockjs-client.git

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
