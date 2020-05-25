all:
	rm -rf *~ */*~ */*/*~;
	rm -rf */*/*/*.beam;
	rm -rf */*/*.beam;
	rm -rf erl_crash.dump */erl_crash.dump */*/erl_crash.dump
doc_gen:
	rm -rf doc/*;
	erlc ../doc_gen.erl;
	erl -s doc_gen start -sname doc
test:
	rm -rf *.beam ebin/* test_ebin/* erl_crash.dump;
	cp src/*app ebin;
	erlc -I ../include -o ebin src/*.erl;
	erlc -I ../include -o test_ebin test_src/*.erl;
	erl -pa ebin -pa test_ebin -s dns_service_tests test -sname dns_test
