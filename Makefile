ERL=erl

all:
	$(ERL) -pa /Users/emiller/src/ChicagoBoss/ebin -eval 'boss_load:load_all_modules_and_emit_app_file("ebin", chicagoboss_admin)' -s init stop -noshell

clean:
	rm -fv ebin/*.beam
	rm -fv ebin/chicagoboss_admin.app
	
update_po:
	$(ERL) -pa ebin -pa ../ChicagoBoss/ebin -eval 'boss_load:load_models("ebin")' -eval 'boss_lang:update_po()' -s init stop -noshell

.PHONY: test
test:
	$(ERL) -pa /Users/emiller/src/ChicagoBoss/ebin -run boss_web_test -noshell
