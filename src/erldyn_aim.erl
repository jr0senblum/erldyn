%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@jims-mbp.jhs.local>
%%% @copyright (C) 2016, Jim Rosenblum
%%% @doc Library module that manages the configuration aspects of erldyn: 
%%% specifically: stores, retrieves and refreshes IAM credentials; stores and
%%% retrieves conventional credentials if not using IAM; stores and retrieves 
%%% the DynamoDB Endpoint; parses, stores and retrieves the Host, Region, 
%%% Stream, etc. from the supplied Endpoint.
%%% @end
%%% Created :  20 Apr 2016 by Jim Rosenblum <jrosenblum@jims-mbp.jhs.local>
%%% ----------------------------------------------------------------------------
-module(erldyn_aim).


-export([configure/1, 
         get/1, 
         get_credential/1,
         put/2]).


-define(ENDPOINT, "https://dynamodb.us-west-2.amazonaws.com/").


% definition of the credential record.
-record (credentials, 
         {expiration, 
          access_key,
          secret_key,
          token}).



%%% ============================================================================
%%%                         API
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% Fetch and persist IAM temporary credentials or persist supplied credentials;
%% parse and persist DynamoDB access parameters (host, region, etc.) from 
%% the supplied endpoint.
%% 
-spec configure(map()) -> ok | error.

configure(ConfigMap) ->
    persist_credentials(ConfigMap),
    config_dynamo_access(ConfigMap),
    ok.


persist_credentials(ConfigMap) ->
    case using_aim() of
        true ->
            refresh_iam_credentials();
        false ->
            Creds = #credentials{access_key = encrypt(maps:get(access_key, ConfigMap, get_access_key())),
                                 secret_key = encrypt(maps:get(secret_key, ConfigMap, get_secret_key())),
                                 token = undefined},
            store_credentials(Creds)
    end.


config_dynamo_access(ConfigMap)->
    EndPoint = maps:get(endpoint, ConfigMap, ?ENDPOINT),
    [Protocol, Host] = string:tokens(EndPoint,"//"),
    TokenizedHost = string:tokens(Host, "."),
    Service = lists:nth(1,TokenizedHost),
    Region = lists:nth(2,TokenizedHost),
    SEndpoint = lists:flatten([Protocol, "//", "streams.", string:join(TokenizedHost, ".")]),

    erldyn_aim:put(endpoint, EndPoint),
    erldyn_aim:put(host, Host),
    erldyn_aim:put(region, Region),
    erldyn_aim:put(service, Service),
    erldyn_aim:put(stream_endpoint, SEndpoint).


get_access_key()->
    os:getenv("AWS_ACCESS_KEY_ID").

get_secret_key()->
    os:getenv("AWS_SECRET_ACCESS_KEY").



%% -----------------------------------------------------------------------------
%% Get a DynamoDb access, configuration element.
%% 
-spec get(Key) -> string() when
      Key :: endpoint | host | region | service | steam_endpoint.

get(Key) ->
    case ets:lookup(aim_cred, Key) of
        [{Key, Value}] ->
            Value;
        _ ->
            undefined
    end.


%% -----------------------------------------------------------------------------
%% Return one of the authorizaion components: secret key, access key or 
%% token
%%
-spec get_credential(secret_key | access_key | token) -> string().

get_credential(Item) ->
    case using_aim() of
        true ->
            refresh_iam_credentials();
        false ->
            ok
    end,
    case ets:lookup(aim_cred, credentials) of
        [#credentials{} = Creds] ->
            get_item(Item, Creds);
        _ ->
            errror
    end.


get_item(secret_key, #credentials{secret_key = Secret}) ->
    binary_to_list(decrypt(Secret));

get_item(access_key, #credentials{access_key = Access}) ->
    binary_to_list(decrypt(Access));

get_item(token, #credentials{token = undefined}) ->
            undefined;

get_item(token, #credentials{token = Token}) ->
    binary_to_list(decrypt(Token)).



%% -----------------------------------------------------------------------------
%% Set one of the authorizaion components: secret key, access key or 
%% token
%%
-spec put(Key, Value) -> true when
      Key :: endpoint | host | region | service | steam_endpoint,
      Value :: string().

put(Key, Value) ->
    ets:insert(aim_cred, {Key, Value}).



%%% ============================================================================
%%%                        Helper Functions
%%% ============================================================================



%% -----------------------------------------------------------------------------
%% Return the encrypted, temporary AWS credentials. If expired, get new ones.
%%
refresh_iam_credentials() ->
    case retrieve_credentials() of
        {ok, Credentials} ->
            case is_current(Credentials) of
                true -> 
                    ok;
                false -> 
                    fetch_and_store_credentials()
            end;
        _ ->
            % missing or ets access error of some sort
            fetch_and_store_credentials()
    end.


retrieve_credentials() ->
    try ets:lookup(aim_cred, creds) of
        [#credentials{} = Credentials] ->
            {ok, Credentials};
        [] ->
            missing
    catch
        _:_  ->
            error
    end.


is_current(#credentials{expiration = Exp}) ->
    Now = calendar:universal_time(),
    Expiration = remove_milliseconds(ec_date:parse(Exp)),
    Expiration > Now.


remove_milliseconds({_YMD, {_, _, _}} = DateTime) ->
    DateTime;
remove_milliseconds({YMD, {H, M, S, _MS}}) ->
    {YMD, {H, M, S}}.


using_aim() ->
    application:get_env(erldyn, aim, false).



% ------------------------------------------------------------------------------
% Hit the AIM URL, convert result into structure, persist into ets.
%
fetch_and_store_credentials() ->
    case pull_from_aws() of
        error ->
            error;
        Map ->
            Credentials = structure_from_map(Map),
            lager:notice("~p: fetched new credentials that expire ~p.",
                         [?MODULE, Credentials#credentials.expiration]),
            store_credentials(Credentials)
    end.


pull_from_aws() ->
    EndPoint = get_endpoint(),
    case httpc:request(EndPoint) of
        {ok, {{_, 200,"OK"}, _, Result}} ->
            jsone:decode(list_to_binary(Result));
        _ ->
            error
end.


store_credentials(Credentials) ->
    try 
        true = ets:insert(aim_cred, Credentials),
        ok
    catch
        T:E ->
            {error, {T,E}}
    end.



% ------------------------------------------------------------------------------
% Instantiate a #credentials{} with everything but the expiration encrypted.
%
structure_from_map(Map) ->
    #credentials{expiration = binary_to_list(maps:get(<<"Expiration">>, Map)),
                 access_key = encrypt(binary_to_list(maps:get(<<"AccessKeyId">>, Map))),
                 secret_key = encrypt(binary_to_list(maps:get(<<"SecretAccessKey">>, Map))),
                 token = encrypt(binary_to_list(maps:get(<<"Token">>, Map)))}.


encrypt(PlainText) ->
    X = get_key(),
    State = crypto:stream_init(aes_ctr, X, X),
    {_, CipherText} = crypto:stream_encrypt(State, PlainText),
    CipherText.


% ------------------------------------------------------------------------------
% AIM endpoint from environment + associated role.
%
get_endpoint() -> 
    {ok, AIMEp} = application:get_env(erldyn, metadata),
    AIMEp ++ "NookRole".


% ------------------------------------------------------------------------------
% get / set the encryption key.
%
get_key() ->
    case application:get_env(erldyn, left) of
        undefined ->
            Key = crypto:rand_bytes(16),
            application:set_env(erldyn, left, Key),
            Key;
        {ok, Value} ->
            Value
end.


decrypt(CipherText) ->
    {ok, X} = application:get_env(erldyn, left),
    State = crypto:stream_init(aes_ctr, X, X),
    {_, PlainText} = crypto:stream_decrypt(State, CipherText),
    PlainText.                                       
