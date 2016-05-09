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


###Configuration
erldyn:config(#{}) is used to set the dynamoDb endpoint (i.e., "https://dynamodb.us-west-2.amazonaws.com/"),
and to optionally set the Amazon, access-key and secret-key assumin AWS Securtiy Token Service 
is not being used. For example: </br>

    => ConfigMap = #{endpoint => "https://dynamodb.us-west-2.amazonaws.com/",
                  access_key => "XXXXXXXXXXX",
                  secret_key => "YYYYYYYYYY"},
    => erldyn:config(ConfigMap).
    => ok

If, AWS AIM services are being used, config/1 is only used to configure the endpoint, and the AIM
service is used to fetch temporary credentials automatically. The configuration file provides
the AWS metadata endpoint and a flag indicating that AIM should be used: </br>

    [
     {erldyn,
      [
       %% DynamoDb endpoint and metadata uri.
       {endpoint, "https://dynamodb.us-west-2.amazonaws.com/"},
       {metadata, "http://169.254.169.254/latest/meta-data/iam/security-credentials/"},
       {aim, false}
      ]}
     ].

##Authorization
All http operations are PUTS, and Version 4 of the Signature authorizaion
header is used.

Secret Key and Access Keys can be fetched from OS environment variables 
(AWS_ACCESS_KEY_ID, and AWS_SECRET_ACCESS_KEY) or set/changed via config/1.

If AWS Security Token Service is being used then only the endpointthe Token can only 
be supplied via the config map.

The DynamoDB Endpoint is provided via the same config/1 function, and
is parsed to determine service, streaming service, host and region. 

    => ConfigMap = #{endpoint => "https://dynamodb.us-west-2.amazonaws.com/",
                  access_key => "XXXXXXXXXXX",
                  secret_key => "YYYYYYYYYY",
                  token => "ZZZZZZZZZZZZ"}.
    => erldyn:config(ConfigMap).
    => ok

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
    
