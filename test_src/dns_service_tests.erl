%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dns_service_tests). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("common_macros.hrl").
%% --------------------------------------------------------------------

-define(TEST_VECTOR,[{"s1","ip1",1},{"s11","ip1",2},
		     {"s2","ip2",1},{"s21","ip1",1},
		     {"s3","ip1",2},{"s21","ip1",1},
		     {"s1","ip2",1},{"s21","ip2",1}]).

%% External exports
%-export([start/0]).
%-compile(export_all).


%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start_server_test()->
    {spawn,
     {setup,
      fun()->
	      ?assertEqual(ok,application:start(dns_service)),
	      Node=node(),
	      ?assertEqual({glurk,Node,dns_service},dns_service:ping())
      end,
      fun(_)->
	      ?assertEqual(glurk,application:stop(dns_service))
      end
     }
     }.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
add_all_services_test_()->
    {spawn,
     {setup,
      fun()->
	      ?assertEqual(ok,application:start(dns_service)),
	      [dns_service:add(S,I,P)||{S,I,P}<-?TEST_VECTOR],
	      [dns_service:add(S,I,P)||{S,I,P}<-?TEST_VECTOR],
	      ?assertEqual([glurk,{"s21","ip2",1},
			    {"s1","ip2",1},
			    {"s21","ip1",1},
			    {"s3","ip1",2},
			    {"s2","ip2",1},
			    {"s11","ip1",2},
			    {"s1","ip1",1}],dns_service:all())
      end,
      fun(_)->
	      ok
      end
     }
     }.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
get_services_test()->
    {spawn,
     {setup,
      fun()->
	      ?assertEqual(ok,application:start(dns_service)),
	      [dns_service:add(S,I,P)||{S,I,P}<-?TEST_VECTOR],
	      [dns_service:add(S,I,P)||{S,I,P}<-?TEST_VECTOR],
	      ?assertEqual([{"ip2",1},
			    {"ip1",1}],dns_service:get("s21")),
	      ?assertEqual([{"ip1",2}],dns_service:get("s3")),
	      ?assertEqual([],dns_service:get("glurk"))
      end,
      fun(_)->
	      ok
      end
     }
     }.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
delete_services_test()->
    {spawn,
     {setup,
      fun()->
	      ?assertEqual(ok,application:start(dns_service)),
	      [dns_service:add(S,I,P)||{S,I,P}<-?TEST_VECTOR],
	      [dns_service:add(S,I,P)||{S,I,P}<-?TEST_VECTOR],
	      ?assertEqual(glurk,dns_service:delete("s21","ip2",1)),
	      
	      dns_service:delete("s3","ip1",2),
	      dns_service:delete("s1","glurk",1),  

	      ?assertEqual([{"ip2",1},
			    {"ip1",1}],dns_service:get("s21")),
	      ?assertEqual([{"ip1",2}],dns_service:get("s3")),
	      ?assertEqual([],dns_service:get("glurk"))
      end,
      fun(_)->
	      ok
      end
     }
     }.



%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
kill_test_session_test()->
    timer:sleep(1000),
    init:stop().
