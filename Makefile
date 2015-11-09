.PHONY: all clean compile release run check doc

REBAR=$(shell which rebar || echo ./rebar3)

# eventually this should be just ebin/*.beam, but there are a number
# of warnings in other files. Just check the clean files for now.
CHECK_FILES=\
     _build/default/lib/*/ebin


all: clean compile

clean:
	@$(REBAR) clean

compile:
	@$(REBAR) compile

run:
	erl -pa _build/default/lib/*/ebin


check: compile
	dialyzer --build_plt -r $ERL_TOP/lib/stdlib/ebin\
                                $ERL_TOP/lib/kernel/ebin \
                                $ERL_TOP/lib/mnesia/ebin --verbose --no_check_plt --no_native --fullpath \
		$(CHECK_FILES) \
		-Wunmatched_returns \
		-Werror_handling

doc:
	@$(REBAR) doc skip_deps=true

