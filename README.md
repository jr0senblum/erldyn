#Erldyn
======

# 

[![hex.pm version](https://img.shields.io/hexpm/v/erldyn.svg)](https://hex.pm/packages/erldyn)
# 

##DynamoDB API
DynamoDB functions are converted to underscore_case functions of arity 1:<br/>

* batch_get_item/1,
* batch_write_item/1,
* create_table/1,
* delete_item/1,
* delete_table/1,
* describe_table/1,
* get_item/1,
* list_tables/1,
* put_item/1,
* query/1,
* scan/1,
* update_item/1,
* update_table/1]

The single parameter is JSON as defined by the DynamoDB API, and the returns are
either: <br/>

* {ok, #{...}}, 
* [{ok, #{...}}, ...], 
* or {error, #{...}) 

where the maps are map versions of DynamoDB, JSON returns. 

###Batch Functions
The batch functions (batch_get_item/1 and batch_write_item/1) can return
partial results. The unprocessed items will be resubmitted automatically,
consequently, these functions return a list of maps - one for each partial 
result.

##Convenience Methods
Convenience methods (new_table/3, save_table/1, and add_parameter/3) are 
provided for simplifying the process of building the correct strcture
for defining and creating tables.


##Exponential Back-Off
Exponentional back-off is used such that appropriate failures, or partial
results, are retried according to an exponential,  back-off algorithm, not 
to exceed one minute total for the entire operation.

##Authorization
All http operations are PUTS, and Version 4 of the Signature authorizaion
header is used.

Secret Key and Access Keys can be fetched form OS environment variables 
(AWS_ACCESS_KEY_ID, and AWS_SECRET_ACCESS_KEY) or set/changed via config/1.

If AWS Security Token Service is being used then the Token can only 
be supplied via the config map.

The DynamoDB Endpoint is provided via the same config/1 function, and
is parsed to determine service, streaming service, host and region. 

    => ConfigMap = #{endpoint => "https://dynamodb.us-west-2.amazonaws.com/",
                  access_key => "AKIAIX5A77AFFFW7JLA",
                  secret_key => "WIcKN27iNhkvsaEtGgPd9iqBFFFhEkas0rsDzoSA",
                  token => "XXXXXXXXXXXXXXXXXX"}.
    => erldyn:config(ConfigMap).
    => ok

PROCESS DICTIONARY IS USED:  <br/>
*   put(access_key, ...) <br/>
*   put(secret_key, ..) <br/>
*   put(token, ..) <br/>
*   put(stream_endpoint, ...) <br/>
*   put(endpoint, ...) <br/>
*   put(host, ...) <br/>
*   put(service, ..) <br/>
*   put(region, ..) <br/>


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
    
