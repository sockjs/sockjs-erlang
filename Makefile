ERLC_OPTS=-W -I include -o ebin
SOURCES=$(wildcard src/*.erl)
BEAM_TARGETS=$(patsubst src/%.erl, ebin/%.beam, $(SOURCES))

all: $(BEAM_TARGETS)

test: test-prep all
	erl -pa ebin -pa deps/misultin/ebin -noinput -run sockjs_test

test-prep: deps/sockjs-client deps/misultin priv/www
	cd deps/sockjs-client && npm install
	make -C deps/sockjs-client tests/html/lib/sockjs.js tests/html/lib/tests.js
	make -C deps/misultin

ebin/%.beam: src/%.erl
	-mkdir -p ebin
	erlc $(ERLC_OPTS) -pa ebin $<

priv/www:
	-mkdir -p priv
	ln -s ../deps/sockjs-client/tests/html priv/www

deps/sockjs-client:
	-mkdir -p deps
	cd deps && \
		git clone https://github.com/majek/sockjs-client.git

deps/misultin:
	-mkdir -p deps
	cd deps && \
		git clone https://github.com/ostinelli/misultin.git

clean::
	rm -rf deps ebin/*.beam priv/www
