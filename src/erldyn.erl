-module(erldyn).

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

-export([describe_streams/1]).


-define(METHOD, "POST").
-define(ENDPOINT, "https://dynamodb.us-west-2.amazonaws.com/").
-define(HOST, "dynamodb.us-west-2.amazonaws.com").
-define(REGION, "us-west-2").
-define(SERVICE, "dynamodb").                    
-define(API_VERSION, "DynamoDB_20120810.").
-define(MAXRETRY, 5).



%% -----------------------------------------------------------------------------
%% DynamoDB API: Dynamo functions are converted to underscore_case. If an API
%% can accept 0 paramerts, a 0-arity funciton is provided otherwise there will
%% be an arity-1 function that expects the same JSON that DynamoDB uses.
%% -----------------------------------------------------------------------------


-spec batch_get_item(JSON::string()) -> map() | [map()].
batch_get_item(JSON) -> 
    repeat("BatchGetItem", JSON).


-spec batch_write_item(JSON::string()) -> map() | [map()].
batch_write_item(JSON) -> 
    repeat("BatchWriteItem", JSON).


-spec create_table(JSON::string()) -> map().
create_table(JSON) -> execute_command("CreateTable", JSON).


-spec delete_item(JSON::string()) -> map().
delete_item(JSON) -> execute_command("DeleteItem", JSON).


-spec delete_table(JSON::string()) -> map().
delete_table(JSON) -> execute_command("DeleteTable", JSON).


-spec describe_table(JSON::string()) -> map().
describe_table(JSON) -> execute_command("DescribeTable", JSON).


-spec get_item(JSON::string()) -> map().
get_item(JSON) -> execute_command("GetItem", JSON).


-spec list_tables(JSON::string()) -> map().
list_tables(JSON) ->  execute_command("ListTables", JSON).


-spec put_item(JSON::string()) -> map().
put_item(JSON) ->  execute_command("PutItem", JSON).


-spec query(JSON::string()) -> map().
query(JSON) ->  execute_command("Query", JSON).


-spec scan(JSON::string()) -> map().
scan(JSON) ->  execute_command("Scan", JSON).


-spec update_item(JSON::string()) -> map().
update_item(JSON) ->  execute_command("UpdateItem", JSON).


-spec update_table(JSON::string()) -> map().
update_table(JSON) ->  execute_command("UpdateTable", JSON).



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
execute_command(Command, JSON) ->
    Target = make_target(Command),
    ReqParam = JSON,
    Uri = "/",
    QS = "",
    Headers = authorization_header(Target, ReqParam, Uri, QS),
    back_off_post(1, ReqParam, Headers, ?ENDPOINT).



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
         {"host", ?HOST},
         {"x-amz-target", Amztarget}],

    % 1. Create the Canonical Request
    SigHeaders = signed_headers(H),
    CanRequest = canonical_request(?METHOD, H, ReqParam, Uri, Qs, SigHeaders),
                                                                            
    % 2. Create the String to Sign
    CredentialScope = credential_scope(Datestamp, ?REGION, ?SERVICE),
    StringToSign = string_to_sign(Alg, Amzdate, CredentialScope, CanRequest),

    % 3. Calculate the AWS Signature Version 4
    SecretKey = get_secret_key(),
    SigningKey = get_signature_key(SecretKey, Datestamp, ?REGION, ?SERVICE),
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



make_target(Command) ->
    ?API_VERSION ++ Command.

get_access_key()->
    os:getenv("AWS_ACCESS_KEY_ID").

get_secret_key()->
    os:getenv("AWS_SECRET_ACCESS_KEY").
