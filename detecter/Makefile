################################################################################
## Project configurations                                                     ##
################################################################################

BIN=ebin
INCLUDE=include
SRC=src
TEST=test
TEST_SRC=$(shell find $(TEST) -name "*.erl" ! -path "*/ebin/*")

define recursive
	$(shell find $(1) -name "*.$(2)")
endef

all: compile

clean:
	rm -f $(BIN)/*.beam $(BIN)/*.E $(BIN)/*.erl $(BIN)/*.tmp $(BIN)/*.app erl_crash.dump

# Behaviour definitions are compiled in their own pass so that
# implementations compiled afterwards resolve -behavior against ebin.
BEHAVIOURS=$(SRC)/behavior/gen_file_poller.erl

compile: clean
	mkdir -p $(BIN)
	erlc -pa $(BIN) +debug_info -I $(INCLUDE) -o $(BIN) $(BEHAVIOURS)
	erlc -pa $(BIN) +debug_info -I $(INCLUDE) -o $(BIN) $(call recursive,$(SRC),erl)

compile-test: clean
	mkdir -p $(BIN)
	erlc -DTEST -pa $(BIN) +debug_info -I $(INCLUDE) -o $(BIN) $(BEHAVIOURS)
	erlc -DTEST -pa $(BIN) +debug_info -I $(INCLUDE) -o $(BIN) $(call recursive,$(SRC),erl)
	erlc -DTEST -pa $(BIN) -I $(INCLUDE) -o $(BIN) $(TEST_SRC)

# tracer_test is intentionally excluded from the default target because
# test/monitoring/tracer_test.erl still relies on timer:sleep-based assertions
# for concurrent trace routing. Run it manually after compile-test when working
# on tracer routing:
#   erl -noshell -pa $(BIN) -eval 'case eunit:test(tracer_test, [verbose]) of error -> init:stop(1); Result -> Result end.' -s init stop
test: compile-test
	erl -noshell -pa $(BIN) -eval 'case eunit:test(log_tracer_test, [verbose]) of error -> init:stop(1); Result -> Result end.' -s init stop
	erl -noshell -pa $(BIN) -eval 'case eunit:test(sys_info_parser_test, [verbose]) of error -> init:stop(1); Result -> Result end.' -s init stop
	erl -noshell -pa $(BIN) -eval 'case eunit:test(generated_monitor_smoke_test, [verbose]) of error -> init:stop(1); Result -> Result end.' -s init stop
	erl -noshell -pa $(BIN) -eval 'case eunit:test(maxhml_synthesis_test, [verbose]) of error -> init:stop(1); Result -> Result end.' -s init stop
	erl -noshell -pa $(BIN) -eval 'case eunit:test(generated_agm_recovery_test, [verbose]) of error -> init:stop(1); Result -> Result end.' -s init stop
	erl -noshell -pa $(BIN) -eval 'case eunit:test(agm_engine_test, [verbose]) of error -> init:stop(1); Result -> Result end.' -s init stop

analyze:
	 dialyzer -pa $(BIN) -I $(INCLUDE) $(call recursive,$(SRC),erl)

test-loop: compile-test
	number=1 ; while [[ $$number -le 100 ]] ; do \
        echo "------------------------------------------------------------------------------------------> Running test #$$number" ; \
        erl -noshell -pa $(BIN) -eval 'case eunit:test(log_tracer_test, [verbose]) of error -> init:stop(1); Result -> Result end.' -s init stop ; \
        ((number = number + 1)) ; \
    done

# dialyzer --build_plt --apps erts kernel stdlib mnesia compiler syntax_tools ssh public_key edoc parsetools
