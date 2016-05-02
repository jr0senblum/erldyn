%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2015 - 2016, Jim Rosenblum
%%% @doc Libary module for interacting with Amazon's DynamoDB. The entire
%%% 20120810 API is implemented.
%%%
%%% DynamoDB functions are converted to underscore_case functions of arity 1.
%%% The parameter is JSON as defined by the DynamoDB API, and the returns are
%%% either: {ok, #{...}}, [{ok, #{...}}, ...], or {error, #{...}) where the
%%% maps are map versions of DynamoDB, JSON returns. 
%%%
%%% The batch functions (batch_get_item/1 and batch_write_item/1) can return
%%% partial results. The unprocessed items will be resubmitted automatically.
%%% consequently, these functions return a list of maps - one for each partial
%%% result.
%%%
%%% Convenience methods (new_table/3, save_table/1, and add_parameter/3) are 
%%% provided for simplifying the process of building the correct strcture
%%% for defining and creating tables.
%%%
%%% Exponentional back-off is used such that appropriate failures, or partial
%%% results, are retried according to an exponential, back-off algorithm, not 
%%% to exceed one minute total for the entire operation.
%%%
%%% All http operations are PUTS, and Version 4 of the Signature authorizaion
%%% header is used.
%%%
%%% Secret Key, Access Keys and Token can be establishd via config/1. If 
%%% these values are not supplied, the os environment is interrogated for 
%%% AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY.
%%%
%%% If AWS Security Token Service is being used then the Token can only 
%%% be supplied via the config/1 parameter (a Map).
%%%
%%% The DynamoDB Endpoint is provided via the same config/1 map parameter, and
%%% is parsed to determine service, streaming service, host and region. 
%%%
%%% PROCESS DICTIONARY IS USED,  VALUES ARE CHANGED VIA CONFIG/1 <br/>
%%%   put(access_key, ...) <br/>
%%%   put(secret_key, ..) <br/>
%%%   put(token, ..) <br/>
%%%   put(stream_endpoint, ...) <br/>
%%%   put(endpoint, ...) <br/>
%%%   put(host, ...) <br/>
%%%   put(service, ..) <br/>
%%%   put(region, ..) <br/>
%%%
%%% @version {@version}
%%% @end
%%% Created : 16 November 2015 by Jim Rosenblum
%%% ----------------------------------------------------------------------------
-module(erldyn).


% dynamodb sdk
-export([config/0, config/1,
         new_table/3,
         save_table/1,
         add_parameter/3
         ]).


% internal functions called by add_parameter so need to be exported
-export([attribute_definitions/1,
         key_schema/1]).


% dynamodb api
-export([batch_get_item/1,
         batch_write_item/1,
         create_table/1,
         delete_item/1,
         delete_table/1,
         describe_table/1,
         get_item/1,
         list_tables/1,
         put_item/1,
         query/1,
         scan/1,
         update_item/1,
         update_table/1]).


% dynamodb stream api
-export([describe_stream/1,
         get_records/1,
         get_shard_iterator/1,
         list_streams/1]).


-define(ENDPOINT, "https://dynamodb.us-west-2.amazonaws.com/").
-define(API_VERSION, "DynamoDB_20120810.").
-define(API_SVERSION, "DynamoDBStreams_20120810.").


                
%% -----------------------------------------------------------------------------
%% @doc Use values within map for Host, and optionally, Key, Secret and Token.
%% Key and Secret can alternatively be provided via exported envionment variables:
%% "AWS_ACCESS_KEY_ID" and "AWS_SECRET_ACCESS_KEY". 
%%
%% PROCESS DICTIONARY IS USED VALUES ARE CHANGED VIA CONFIG/1 <br/>
%%   put(access_key, ...) <br/>
%%   put(secret_key, ..) <br/>
%%   put(token, ..) <br/>
%%   put(stream_endpoint, ...) <br/>
%%   put(endpoint, ...) <br/>
%%   put(host, ...) <br/>
%%   put(service, ..) <br/>
%%   put(region, ..) <br/>
%%
%% Input map #{access_key, secret_key, token, endpoint}
%%
-spec config(map()) -> ok.

config(Config) ->
    put(access_key, maps:get(access_key, Config, get_access_key())),
    put(secret_key, maps:get(secret_key, Config, get_secret_key())),
    put(token, maps:get(token, Config, undefined)),

    EndPoint = maps:get(endpoint, Config, ?ENDPOINT),
    [Protocol, Host] = string:tokens(EndPoint,"//"),
    TokenizedHost = string:tokens(Host, "."),
    Service = lists:nth(1,TokenizedHost),
    Region = lists:nth(2,TokenizedHost),
    SEndpoint = lists:flatten([Protocol, "//", "streams.", string:join(TokenizedHost, ".")]),

    put(endpoint, EndPoint),
    put(host, Host),
    put(region, Region),
    put(service, Service),
    put(stream_endpoint, SEndpoint),   
    _ = inets:start(),
    _ = ssl:start(),
    ok.


%% -----------------------------------------------------------------------------
%% @doc Convenience function for config(#{}).
%% @see config/1
%%
-spec config() -> ok.
config() ->
    config(#{}).


get_access_key()->
    os:getenv("AWS_ACCESS_KEY_ID").

get_secret_key()->
    os:getenv("AWS_SECRET_ACCESS_KEY").



%% -----------------------------------------------------------------------------
%% DynamoDB API: Dynamo functions are converted to underscore_case of arity/1
%% expecting JSON per DynamoDB API.
%% -----------------------------------------------------------------------------
-type json_in()      :: jsone:json_value() | string().
-type results()      :: {error, #{}} | {ok, #{}}.
-type batch_return() :: [{ok, #{}},...] | {error, #{}}.


-spec batch_get_item(json_in()) -> batch_return().
batch_get_item(JSON) -> until_done("BatchGetItem", JSON, []).


-spec batch_write_item(json_in()) -> batch_return().
batch_write_item(JSON) -> until_done("BatchWriteItem", JSON, []).


-spec create_table(json_in()) -> results().
create_table(JSON) -> execute_command("CreateTable", JSON).


-spec delete_item(json_in()) -> results().
delete_item(JSON) -> execute_command("DeleteItem", JSON).


-spec delete_table(json_in()) -> results().
delete_table(JSON) -> execute_command("DeleteTable", JSON).


-spec describe_table(json_in()) -> results().
describe_table(JSON) -> execute_command("DescribeTable", JSON).


-spec get_item(json_in()) -> results().
get_item(JSON) -> execute_command("GetItem", JSON).


-spec list_tables(json_in()) -> results().
list_tables(JSON) -> execute_command("ListTables", JSON).


-spec put_item(json_in()) -> results().
put_item(JSON) -> execute_command("PutItem", JSON).


-spec query(json_in()) -> results().
query(JSON) -> execute_command("Query", JSON).


-spec scan(json_in()) -> results().
scan(JSON) -> execute_command("Scan", JSON).


-spec update_item(json_in()) -> results().
update_item(JSON) -> execute_command("UpdateItem", JSON).


-spec update_table(json_in()) -> results().
update_table(JSON) -> execute_command("UpdateTable", JSON).


%% -----------------------------------------------------------------------------
%% DynamoDBStream API: Dynamo functions are converted to underscore_case of 
%% arity/1 expecting JSON per DynamoDB API.
%% -----------------------------------------------------------------------------

-spec describe_stream(json_in()) -> results().
describe_stream(JSON) -> execute_command("DescribeStream", JSON, stream).


-spec get_records(json_in()) -> results().
get_records(JSON) -> execute_command("GetRecords", JSON, stream).


-spec get_shard_iterator(json_in()) -> results().
get_shard_iterator(JSON) -> execute_command("GetShardIterator", JSON, stream).


-spec list_streams(json_in()) -> results().
list_streams(JSON) -> execute_command("ListStreams", JSON, stream).



%% -----------------------------------------------------------------------------
%% DynamoDBStream SDK: Convenience functions on top of API.
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% @doc Return a map that contains the appropriate structure for the TableName,
%% and ProvisionedThroughput parameters. When all required/optional parameters
%% added to this map, save_table(Map = #{}) will convert the map to the correct 
%% JSON and utilize create_table/1 to create the DynamoDB table. This is a 
%% convenience function as an alternatvie to constructing the correct JSON and 
%% directly using create_table.
%%
-spec new_table(string(), non_neg_integer(), non_neg_integer()) -> #{}.

new_table(TblNm, RdCpctyUnts, WrtCpctyUnts) ->
    #{<<"TableName">> => list_to_binary(TblNm),
      <<"ProvisionedThroughput">> => 
          #{<<"ReadCapacityUnits">> => RdCpctyUnts,
            <<"WriteCapacityUnits">> => WrtCpctyUnts
           }
     }.


%% -----------------------------------------------------------------------------
%% @doc Convert the Map into JSON and execute create_table/1.
%% @see create_table/1
%%
-spec save_table(Map::#{}) -> {error, #{}} | {ok, binary(), #{}}.

save_table(Table) ->
    case create_table(jsone:encode(Table)) of
        {ok,#{<<"TableDescription">> :=  #{<<"TableStatus">> := Status}} = R} ->
            {ok, Status, R};
        {error, _} = Error ->
            Error
    end.
             

%% -----------------------------------------------------------------------------
%% @doc Add a parameter structure to a Map representing a table specification 
%% under construction. The Map should be derived from calling new_table/1, add
%% the parameter being added would be either an attribute or key_schema. In 
%% either case, the third parameter is expected to be a list of {Name, Type}
%% values appropriate to the parameter. For example <br/>
%% add_parameter(Map, key_schema, [{"SomeAttName","SomeKeyType"}]).
%%
-spec add_parameter(#{}, attribute_definitions | key_schema, list())-> #{}.

add_parameter(Table, Parameter, L) when is_list(L) ->
    Att = erldyn:Parameter(L),
    insert(Table, Parameter, Att).


insert(Table, attribute_definitions, Attrbts) ->
    maps:put(<<"AttributeDefinitions">>, Attrbts, Table);
insert(Table, key_schema, Schma) ->
    maps:put(<<"KeySchema">>, Schma, Table).


%% -----------------------------------------------------------------------------
%% Returns a list of maps representing an attribute_definition. Used by 
%% {@link add_parameter/3. add_parameter}.
%% 
-spec attribute_definitions(list()) -> [#{}].
attribute_definitions(L) ->
    lists:reverse(lists:foldl(fun({N, T}, Acc) -> 
                                      [#{<<"AttributeName">> => list_to_binary(N), 
                                         <<"AttributeType">> => list_to_binary(T)} | Acc] end,
                              [],
                              L)).


%% -----------------------------------------------------------------------------
%% Returns a list of maps representing a key_schema. Used by 
%% {@link add_parameter/3. add_parameter}.
%% 
-spec key_schema(list()) -> [#{}].
key_schema(L) ->
    lists:reverse(lists:foldl(fun({N, T}, Acc) -> 
                                      [#{<<"AttributeName">> => list_to_binary(N), 
                                         <<"KeyType">> => list_to_binary(T)} | Acc] end,
                              [],
                              L)).


%% -----------------------------------------------------------------------------
%% UTILITY FUNCTIONS: execute the command with appropriate back-off and 
%% processing of partial results.
%% -----------------------------------------------------------------------------


%% Batch commands can return an UnprocessedItems component which should be
%% retried. Execute_command uses an exponential back-off per best-practive.
%%
-spec until_done(string(), json_in(), [{ok,#{}},...] | []) -> batch_return().

until_done(Command, JSON, Acc) ->
    case execute_command(Command, JSON) of
        {ok, Map} = R ->
            case maps:get(<<"UnprocessedItems">>, Map, none) of
                none -> 
                    [R | Acc];
                #{} ->
                    [R | Acc];
                Unprocessed -> 
                    until_done(Command, jsone:encode(Unprocessed), [R | Acc])
            end;
        {error, _Map} = RError ->
            RError
    end.



%% -----------------------------------------------------------------------------
%% Executing the command means creating the correct headers, including the 
%% Singature Version 4 Authorization header and then using httpc to perform
%% the PUT.
%% 
execute_command(Command, JSON) ->
    execute_command(Command, JSON, not_stream).

execute_command(Command, JSON, IsStream) ->
    Endpoint = get_endpoint(IsStream),
    Target = get_target(Command, IsStream),
    ReqParam = JSON,
    Uri = "/",
    QS = "",
    case sig_auth:authorization_header(Target, ReqParam, Uri, QS) of
        {error, _} = E ->
            E;
        Headers ->
            back_off_post(1, ReqParam, Headers, Endpoint)
    end.


get_endpoint(stream) ->
    get(stream_endpoint);
get_endpoint(_) ->
    get(endpoint).


get_target(Command, stream) ->
    make_api_stream_target(Command);
get_target(Command, _) ->
    make_api_target(Command).


make_api_target(Command) ->
    ?API_VERSION ++ Command.

make_api_stream_target(Command) ->
    ?API_SVERSION ++ Command.



%% -----------------------------------------------------------------------------
%% Exponential back-off on failure until successful, or 60 seconds.
%% -----------------------------------------------------------------------------

-define (AGAIN(Cd, E), (Cd >= 500 andalso Cd < 600) 
         orelse 
           (E == "ProvisionedThroughputExceededException")).

back_off_post(N, Body, Headers, EndPoint) ->
    case post_msg(Body, Headers, EndPoint) of
        ({ok, {{_, 200, "OK"}, _, JSON}}) ->
                {ok, jsone:decode(JSON)};
        ({ok, {{_, Status, Error}, _, JSON}}) when ?AGAIN(Status, Error) ->
            case back_off(N) of
                stop ->
                    {error, jsone:decode(JSON)};
                NewN ->
                    back_off_post(NewN, Body, Headers, EndPoint)
            end;
        ({ok, {{_, _Status, _}, _, JSON}}) ->
            {error, jsone:decode(JSON)}
    end.


back_off(N) ->
    case (N * N * 50) of
        Delay when Delay < 60000 ->
            timer:sleep(Delay),
            N + 1;
        _ ->
            stop
    end.


post_msg(PostBody, Headers, EndPoint)->
    ContentType = "application/x-amz-json-1.0",
    HTTPOptions = [{timeout, 30000}, {relaxed, true}],
    Options = [{body_format, binary}],
    httpc:request(post, 
                  {EndPoint, Headers, ContentType, PostBody}, 
                  HTTPOptions, Options).
 

