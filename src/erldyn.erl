%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2015 - 2016, Jim Rosenblum
%%% @doc Libary module for interacting with Amazon's DynamoDB. The entire
%%% 20120810 API is implemented.
%%%
%%% DynamoDb functions are converted to underscore_case functions of arity 1.
%%% The parameter is JSON as defined by the DynamoDB API and the returns are
%%% map versions of the DnamoDB JSON returns. The batch functions return a
%%% list of maps - one for each return.
%%% 
%%% Exponentional back-off is uses such that appropriate failures or partial
%%% results are retried according to the back-off algorithm, not to exceed one
%%% minute total for the opperationl
%%%
%%% All http opperations are PUTS and Version 4 of the Signature authorizaion
%%% header is used.
%%%
%%% @version {@version}
%%% @end
%%% Created : 16 November 2015 by Jim Rosenblum
%%% ----------------------------------------------------------------------------



-module(erldyn).

-export([config/1]).

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

-export([list_streams/1]).


-define(METHOD, "POST").
-define(ENDPOINT, "https://dynamodb.us-west-2.amazonaws.com/").

-define(API_VERSION, "DynamoDB_20120810.").
-define(API_SVERSION, "DynamoDBStreams_20120810.").


%% -----------------------------------------------------------------------------
%% @doc Use values within map for Host, Key and Secret can be passed in or
%% provided via exported envionment variables. 
%%
-spec config(map()) -> ok.

config(Config) ->
    put(access_key, maps:get(access_key, Config, get_access_key())),
    put(secret_key, maps:get(secret_key, Config, get_secret_key())),

    EndPoint = maps:get(endpoint, Config, ?ENDPOINT),
    [_, Host] = string:tokens(EndPoint,"//"),
    [Service, Region, _, _] = string:tokens(Host, "."),

    
    put(endpoint, EndPoint),
    put(host, Host),
    put(service, Service),
    put(region, Region),
    ok.


%% -----------------------------------------------------------------------------
%% DynamoDB API: Dynamo functions are converted to underscore_case. If an API
%% can accept 0 paramerts, a 0-arity funciton is provided otherwise there will
%% be an arity-1 function that expects the same JSON that DynamoDB uses.
%% -----------------------------------------------------------------------------


-spec batch_get_item(JSON::string()) -> map() | [map()].
batch_get_item(JSON) -> 
    repeat(make_api_target("BatchGetItem"), JSON).


-spec batch_write_item(JSON::string()) -> map() | [map()].
batch_write_item(JSON) -> 
    repeat(make_api_target("BatchWriteItem"), JSON).


-spec create_table(JSON::string()) -> map().
create_table(JSON) -> execute_command(make_api_target("CreateTable"), JSON).


-spec delete_item(JSON::string()) -> map().
delete_item(JSON) -> execute_command(make_api_target("DeleteItem"), JSON).


-spec delete_table(JSON::string()) -> map().
delete_table(JSON) -> execute_command(make_api_target("DeleteTable"), JSON).


-spec describe_table(JSON::string()) -> map().
describe_table(JSON) -> execute_command(make_api_target("DescribeTable"), JSON).


-spec get_item(JSON::string()) -> map().
get_item(JSON) -> execute_command(make_api_target("GetItem"), JSON).


-spec list_tables(JSON::string()) -> map().
list_tables(JSON) ->  execute_command(make_api_target("ListTables"), JSON).


-spec put_item(JSON::string()) -> map().
put_item(JSON) ->  execute_command(make_api_target("PutItem"), JSON).


-spec query(JSON::string()) -> map().
query(JSON) ->  execute_command(make_api_target("Query"), JSON).


-spec scan(JSON::string()) -> map().
scan(JSON) ->  execute_command(make_api_target("Scan"), JSON).


-spec update_item(JSON::string()) -> map().
update_item(JSON) ->  execute_command(make_api_target("UpdateItem"), JSON).


-spec update_table(JSON::string()) -> map().
update_table(JSON) ->  execute_command(make_api_target("UpdateTable"), JSON).


-spec list_streams(JSON::string()) -> map().
list_streams(JSON) ->  execute_command(make_api_stream_target("ListStreams"), JSON).


%% -----------------------------------------------------------------------------
%% Batch commands can return an UnprocessedItems component which should be
%% retried. Execute_command uses an exponential back-off per best-practive.
%%
repeat(Target, JSON) ->
    case execute_command(Target, JSON) of
        {ok, Map} = R ->
            case maps:get(<<"UnprocessedItems">>, Map, none) of
                none -> 
                    R;
                #{} ->
                    R;
                Unprocessed -> 
                    [R | repeat(Target, jsone:encode(Unprocessed))]
            end;
        {error, _Map} = RError ->
            RError
    end.



%% -----------------------------------------------------------------------------
%% Executing the command means creating the correct headers, including the 
%% Singature Version 4 Authorization header and then using httpc to perform
%% the PUT.
%% 
execute_command(Target, JSON) ->
    ReqParam = JSON,
    Uri = "/",
    QS = "",
    Headers = authorization_header(Target, ReqParam, Uri, QS),
    back_off_post(1, ReqParam, Headers, get(endpoint)).



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
    httpc:request(post, {EndPoint, Headers, ContentType, PostBody},  
                  HTTPOptions, Options).









authorization_header(Amztarget, ReqParam, Uri, Qs) ->
    Alg = "AWS4-HMAC-SHA256",    
    Amzdate = format_iso8601(),
    Datestamp = string:left(Amzdate, 8),

    H = [{"x-amz-date", Amzdate},
         {"host", get(host)},
         {"x-amz-target", Amztarget}],

    % 1. Create the Canonical Request
    SigHeaders = signed_headers(H),
    CanRequest = canonical_request(?METHOD, H, ReqParam, Uri, Qs, SigHeaders),
                                                                            
    % 2. Create the String to Sign
    CredentialScope = credential_scope(Datestamp, get(region), get(service)),
    StringToSign = string_to_sign(Alg, Amzdate, CredentialScope, CanRequest),

    % 3. Calculate the AWS Signature Version 4
    SecretKey = get_secret_key(),
    SigningKey = get_signature_key(SecretKey, Datestamp, get(region), get(service)),
    Signature = base16(crypto:hmac(sha256, SigningKey, StringToSign)),
    
    % 4. Create the Authorization header
    Auth = auth_header(Alg, CredentialScope, SigHeaders, Signature),

    [{"Authorization", Auth} | H].



%% To create the canonical headers list, convert all header names to lowercase 
%% and trim excess white space characters out of the header values. When you 
%% trim, remove leading spaces and trailing spaces, and convert sequential 
%% spaces in the value to a single space. However, do not remove extra spaces
%% from any values that are inside quotation marks. Sort.
canonical_headers(Headers) ->
    [[normalize(H,V), "\n"] ||
        {H,V} <- lists:keysort(1, Headers)].


normalize(H, V) ->
    [string:strip(string:to_lower(H)),
     ":",
     string:strip(V)].



%% Signed Headers are a list of the names of the headers included in the 
%% canonical headers.
signed_headers(Headers) ->
    F = fun({H, _}, []) -> [H];
           ({H, _}, Acc) -> [H,";"|Acc] 
        end,
    lists:reverse(lists:foldl(F, [], lists:keysort(1, Headers))).


%% CanonicalRequest =
%%   HTTPRequestMethod + '\n' +
%%   CanonicalURI + '\n' +
%%   CanonicalQueryString + '\n' +
%%   CanonicalHeaders + '\n' +
%%   SignedHeaders + '\n' +
%%   HexEncode(Hash(RequestPayload))

canonical_request(Method, H, ReqParam, Uri, Qs, SignedHeaders) ->
    Headers = canonical_headers(H),
    PayloadHash = payload_hash(ReqParam),

      [Method, "\n"
      , Uri, "\n"
      , Qs, "\n" 
      , Headers, "\n"
      , SignedHeaders, "\n" 
      , PayloadHash].


%% Date (just the date, not the date and time), the region you are targeting, 
%% the service you are requesting, and a termination string ("aws4_request") 
%% in lowercase characters.
credential_scope(Datestamp, Region, Service) ->
    [Datestamp, "/"
    , Region, "/"
    , Service, "/"
    , "aws4_request"].


%% StringToSign  =
%%  Algorithm + '\n' +
%%  RequestDate + '\n' +
%%  CredentialScope + '\n' +
%%  HashedCanonicalRequest
string_to_sign(Alg, Date, Scope, Request) ->
    [Alg, "\n"
    , Date, "\n"
    , Scope, "\n"
    , payload_hash(Request)].


%% Calculate the signature key.
get_signature_key(Key, Date, Region, Service) ->
    KDate = crypto:hmac(sha256,"AWS4" ++ Key, Date),
    KRegion = crypto:hmac(sha256, KDate, Region),
    KService = crypto:hmac(sha256, KRegion, Service),
    crypto:hmac(sha256, KService, "aws4_request").


%% Create the Authorization Header
 auth_header(Alg, Scope, SignedHeaders, Sig) ->
    Access = get_access_key(),
    [Alg, " ", "Credential=", Access, "/", Scope, ", "
    , "SignedHeaders=", SignedHeaders, ", ", "Signature=", Sig].

payload_hash(Payload) ->
    base16(crypto:hash(sha256, Payload)).
    

base16(Data) ->
    io_lib:format("~64.16.0b", [binary:decode_unsigned(Data)]).



format_iso8601() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} =
        calendar:universal_time(),

    lists:flatten(io_lib:format(
                    "~.4.0w~.2.0w~.2.0wT~.2.0w~.2.0w~.2.0wZ",
                    [Year, Month, Day, Hour, Min, Sec] )).


make_api_target(Command) ->
    ?API_VERSION ++ Command.

make_api_stream_target(Command) ->
    ?API_SVERSION ++ Command.


get_access_key()->
    os:getenv("AWS_ACCESS_KEY_ID").

get_secret_key()->
    os:getenv("AWS_SECRET_ACCESS_KEY").
