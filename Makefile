ERLC_OPTS=-W -I include -o ebin
SOURCES=$(wildcard src/*.erl)
BEAM_TARGETS=$(patsubst src/%.erl, ebin/%.beam, $(SOURCES))
JSON=mochijson2
HTTP=cowboy

all: $(BEAM_TARGETS)

test: test-prep all
	erl -pa ebin -pa deps/misultin/ebin -pa deps/mochiweb/ebin \
		-pa deps/cowboy/ebin -pa deps/cowboy/deps/quoted/ebin \
		-pa deps/eep0018/ebin -sockjs json_impl $(JSON) \
		-sockjs http_impl $(HTTP) \
		-run sockjs_test

test-prep: deps/sockjs-client deps/misultin deps/cowboy deps/mochiweb deps/eep0018 priv/www
	cd deps/sockjs-client && npm install
	make -C deps/sockjs-client tests/html/lib/sockjs.js tests/html/lib/tests.js
	make -C deps/misultin
	make -C deps/cowboy
	make -C deps/mochiweb
	make -C deps/eep0018

ebin/%.beam: src/%.erl
	-mkdir -p ebin
	erlc $(ERLC_OPTS) -pa ebin $<

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
		git clone -b dev https://github.com/extend/cowboy.git

deps/mochiweb:
	-mkdir -p deps
	cd deps && \
		git clone https://github.com/mochi/mochiweb.git

deps/eep0018:
	-mkdir -p deps
	cd deps && \
		git clone https://github.com/davisp/eep0018.git

clean::
	rm -rf deps ebin/*.beam priv/www
