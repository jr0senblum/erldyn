Erldyn
=====
DynamoDB functions are converted to underscore_case functions of arity 1.
The parameter is JSON as defined by the DynamoDB API, and the returns are
either {ok, #{...}}, [{ok, #{...}}, ...], or {error, #{...}) <br/> where 
the maps are map versions of DynamoDB, JSON returns. 

The batch functions (batch_get_item/1 and batch_write_item/1) can return
partial results. The unprocessed items will be resubmitted automatically,
consequently, these functions return a list of maps - one for each partial 
result.

Convenience methods (new_table/3, save_table/1, and add_parameter/3) are 
provided for simplifying the process of building the correct strcture
for defining and creating tables.

Exponentional back-off is used such that appropriate failures, or partial
results, are retried according to an exponential,  back-off algorithm, not 
to exceed one minute total for the entire operation.

All http operations are PUTS, and Version 4 of the Signature authorizaion
header is used.

Secret Key and Access Keys can be passed via map and config/1, if not found
the os environment is interrogated for AWS_ACCESS_KEY_ID, and
AWS_SECRET_ACCESS_KEY.

The DynamoDB Endpoint is provided via the same config/1 map parameter, and
is parsed to determine service, streaming service, host and region. 

PROCESS DICTIONARY IS USED,  VALUES ARE CHANGED VIA CONFIG/1 
  put(access_key, ...) <br/>
  put(secret_key, ..) <br/>
  put(stream_endpoint, ...) <br/>
  put(endpoint, ...) <br/>
  put(host, ...) <br/>
  put(service, ..) <br/>
  put(region, ..) <br/>


Dependencies
------------
Lager for logging, jsone for json <--> map translation, and rebar3 for building


Build
-----
erldyn uses rebar3

    $ make compile
    $ make clean
    $ make doc
    $ make check
    $ make run
    

Running
-------
Assuming that $ make has been previously executed:


    $ export AWS_SECRET_ACCESS_KEY="WIcKN27efefff499Pd9iqBpm3hEkas0rsDzoSA"
    $ export AWS_ACCESS_KEY_ID="AKERTJFFIFATLGIJFJLA"
   
    $ make run
    erl -pa _build/default/lib/*/ebin
    Erlang/OTP 18 [erts-7.1] [source-2882b0c] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

    Eshell V7.1  (abort with ^G)

    1> erldyn:config(#{endpoint => "https://dynamodb.us-west-2.amazonaws.com/"}).
    ok
    2> erldyn:list_tables("{}").
    {ok,#{<<"TableNames">> => [<<"Forum">>,<<"Reply">>,<<"Thread">>,<<"Wizzards">>]}}
    
