ERLC_OPTS=-W -I include -o ebin
SOURCES=$(wildcard src/*.erl)
BEAM_TARGETS=$(patsubst src/%.erl, ebin/%.beam, $(SOURCES))
JSON=mochijson2

all: $(BEAM_TARGETS)

test: test-prep all
	erl -pa ebin -pa deps/misultin/ebin -pa deps/mochiweb/ebin \
		-pa deps/eep0018/ebin -noinput -sockjs json_impl $(JSON) \
		-run sockjs_test

test-prep: deps/sockjs-client deps/misultin deps/mochiweb deps/eep0018 priv/www
	cd deps/sockjs-client && npm install
	make -C deps/sockjs-client tests/html/lib/sockjs.js tests/html/lib/tests.js
	make -C deps/misultin
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
