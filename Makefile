REBAR=./rebar

.PHONY: all deps clean distclean
all: deps
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean::
	$(REBAR) clean
	rm -rf priv/www

distclean::
	rm -rf deps priv ebin


# **** serve ****

.PHONY: serve
SERVE_SCRIPT=./test_server_misultin.erl
serve:
	@if [ -e .pidfile.pid ]; then			\
		kill `cat .pidfile.pid`;		\
		rm .pidfile.pid;			\
	fi

	@while [ 1 ]; do				\
		$(REBAR) compile && (			\
			echo " [*] Running erlang";	\
			$(SERVE_SCRIPT) &		\
			SRVPID=$$!;			\
			echo $$SRVPID > .pidfile.pid;	\
			echo " [*] Pid: $$SRVPID";	\
		); 					\
		inotifywait -r -q -e modify src/*erl *erl src/*hrl; \
		test -e .pidfile.pid && kill `cat .pidfile.pid`; \
		rm -f .pidfile.pid;			\
		sleep 0.1;				\
	done


# **** dialyzer ****

ERL_TOP=$(HOME)/.erlang-R15B/lib/erlang
.dialyzer_generic.plt:
	dialyzer					\
		--build_plt				\
		--output_plt .dialyzer_generic.plt	\
		--apps erts kernel stdlib compiler sasl os_mon mnesia \
			tools public_key crypto ssl

.dialyzer_sockjs.plt:
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
	$(REBAR) xref | egrep -v unused
