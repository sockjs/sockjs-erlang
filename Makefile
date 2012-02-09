JSON=jiffy
HTTP=cowboy

.PHONY: all deps test test-prep clean distclean

all: deps deps/$(HTTP)
	make -C deps/$(HTTP)
	./rebar compile

deps:
	./rebar get-deps

test:
	erl -pa ebin deps/*/ebin \
		-sockjs json_impl $(JSON) \
		-sockjs http_impl $(HTTP) \
		-run test1

deps/misultin:
	-mkdir -p deps
	cd deps && \
		git clone -b master https://github.com/ostinelli/misultin.git

deps/cowboy:
	-mkdir -p deps
	cd deps && \
		git clone -b master https://github.com/extend/cowboy.git

clean::
	./rebar clean
	rm -rf priv/www

distclean::
	rm -rf deps priv

serve:
	@if [ -e .pidfile.pid ]; then			\
		kill `cat .pidfile.pid`;		\
		rm .pidfile.pid;			\
	fi

	@while [ 1 ]; do				\
		rebar compile && (			\
			echo " [*] Running erlang";	\
			./test_server.erl &			\
			SRVPID=$$!;			\
			echo $$SRVPID > .pidfile.pid;	\
			echo " [*] Pid: $$SRVPID";	\
		); 					\
		inotifywait -r -q -e modify src/*erl *erl src/*hrl; \
		test -e .pidfile.pid && kill `cat .pidfile.pid`; \
		rm -f .pidfile.pid;			\
		sleep 0.1;				\
	done


ERL_TOP=$(HOME)/.erlang-R15B/lib/erlang
.dialyzer_generic.plt:
	dialyzer					\
		--build_plt				\
		--output_plt .dialyzer_generic.plt	\
		--apps erts kernel stdlib compiler sasl os_mon mnesia \
			tools public_key crypto ssl

.dialyzer_sockjs.plt: # deps/*/ebin/*
	dialyzer				\
		--no_native			\
		--add_to_plt			\
		--plt .dialyzer_generic.plt	\
		--output_plt .dialyzer_sockjs.plt -r deps/*/ebin

dialyze: .dialyzer_sockjs.plt
	@dialyzer	 		\
	  --plt .dialyzer_sockjs.plt	\
	  --no_native			\
	  --fullpath			\
		-Wrace_conditions	\
		-Werror_handling	\
		-Wunmatched_returns	\
	  ebin

xref:
	./rebar xref

check: xref dialyze
