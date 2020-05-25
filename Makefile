all:
	rm -rf *~ */*~ */*/*~;
	rm -rf */*/*/*.beam;
	rm -rf */*/*.beam;
	rm -rf */erl_crash.dump */*/erl_crash.dump
doc_gen:
	rm -rf doc/*;
	erlc doc_gen.erl;
	erl -s doc_gen start -sname doc 
