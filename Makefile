JSON=mochijson2
HTTP=cowboy

all: deps
	rebar compile

deps:
	rebar get-deps
	make -C deps/cowboy

test: test-prep all
	erl -pa ebin -pa deps/misultin/ebin -pa deps/mochiweb/ebin \
		-pa deps/cowboy/ebin -pa deps/cowboy/deps/quoted/ebin \
		-pa deps/eep0018/ebin -sockjs json_impl $(JSON) \
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

clean::
	rebar clean
	rm -rf priv/www

distclean:: clean
	rm -rf deps priv
