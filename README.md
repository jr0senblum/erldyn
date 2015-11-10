erldyn
=====

A Library module for interactin with Amazon's DynamoDB

DynamoDb functions are converted to underscore_case functions of arity 1.
The parameter is JSON as defined by the DynamoDB API and the returns are
map versions of the DnamoDB JSON returns. The batch functions return a
list of maps - one for each return.

Exponentional back-off is uses such that appropriate failures or partial
results are retried according to the back-off algorithm, not to exceed one
minute total for the opperationl

All http opperations are PUTS and Version 4 of the Signature authorizaion
header is used.


Build
-----
erldyn uses rebar3
    $ make compile

Running
-------

    $ export AWS_SECRET_ACCESS_KEY="WIcKN27iNfefefa3499Pd9iqBpm3hEkas0rsDzoSA"
    $ export AWS_ACCESS_KEY_ID="AKJGIJFFIFATLGIJFJLA"
   
    $ make run
    erl -pa _build/default/lib/*/ebin
    Erlang/OTP 18 [erts-7.1] [source-2882b0c] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

    Eshell V7.1  (abort with ^G)

    1> application:ensure_all_started(erldyn)
    ... SNIP ...
    {ok,[sasl,syntax_tools,compiler,goldrush,lager,jsone,inets,
     crypto,asn1,public_key,ssl,erldyn]}
    2> erldyn:config(#{endpoint => "https://dynamodb.us-west-2.amazonaws.com/"}).
    3> erldyn:list_tables("{}").
    {ok,#{<<"TableNames">> => [<<"Forum">>,<<"Reply">>,<<"Thread">>,<<"Wizzards">>]}}ok
    

