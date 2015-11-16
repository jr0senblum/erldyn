.PHONY: all clean compile release run check doc

REBAR=$(shell which rebar || echo ./rebar3)

# eventually this should be just ebin/*.beam, but there are a number
# of warnings in other files. Just check the clean files for now.
CHECK_FILES=\
     _build/default/lib/erldyn/ebin 

CHECK_F=\
     _build/default/lib/jsone/ebin 



all: clean compile

clean:
	@$(REBAR) clean

compile:
	@$(REBAR) compile

run:
	erl -pa _build/default/lib/*/ebin


check: compile
	dialyzer --no_native --fullpath \
		$(CHECK_FILES) $(CHECK_F) \
		-Wunmatched_returns \
		-Werror_handling 

doc:
	@$(REBAR) edoc

