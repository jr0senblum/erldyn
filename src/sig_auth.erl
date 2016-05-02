%%%-----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2015, Jim Rosenblum
%%% @doc Signature Version 4 Signing Process per Amazon 
%%% [http://docs.aws.amazon.com/general/latest/gr/sigv4_signing.html] <br/>
%%% Task 1: Create A Canonical Request <br/>
%%% Arrange the contents of request (endpoint, action, headers, etc.) into a 
%%% canonical, format. Then create a hash (digest) of the canonical request, 
%%% add it to the canonical request, and then create a digest of the updated 
%%% canonical request. <br/>
%%% Task 2: Create a String to Sign <br/>
%%% Using content from the request (the algorithm, request, date, credential 
%%% scope, and the digest of the canonical request), create a string to 
%%% sign. <br/>
%%% Task 3:Create a Signature <br/>
%%% Derive a signing key by performing a succession of recursive keyed hash 
%%% operations (HMAC operations) on the request date, region, service, and 
%%% signing value, using an AWS secret access key as the key for the hashing 
%%% operation. After you have derived the signing key, you then calculate the 
%%% signature by performing a keyed hash operation on the string to sign, using
%%% the derived key as the hash key. Finally, add the signature to the header or
%%% to the query string of the request.
%%% @end
%%% Created : 13 Nov 2015 by Jim Rosenblum <jrosenblum@Jims-MBP.attlocal.net>
%%%-------------------------------------------------------------------
-module(sig_auth).


%% API
-export([authorization_header/4]).



-define(METHOD, "POST").



%%% ============================================================================
%%% API
%%% ============================================================================


%% -----------------------------------------------------------------------------
%% Calculate the authorization header per Amazon's V4 Signature instructions.
%%
-spec authorization_header(string(), JSON::string(), string(), string()) -> 
                                  [{string(), string()| non_neg_integer()}] | 
                                  {error, #{}}.


authorization_header(Amztarget, ReqParam, Uri, Qs) ->
    try 
        authorization_header_(Amztarget, ReqParam, Uri, Qs)
    catch
        _:E ->
            {error, #{authorization_error => E}}
    end.
            
            
authorization_header_(Amztarget, ReqParam, Uri, Qs) ->
    Alg = "AWS4-HMAC-SHA256",    
    Amzdate = format_iso8601(),
    Datestamp = string:left(Amzdate, 8),

    % 0. Start with the headers
    H = headers(Amzdate, Amztarget),
    
    % 1. Create the Canonical Request
    SigHeaders = signed_headers(H),
    CanRequest = canonical_request(?METHOD, H, ReqParam, Uri, Qs, SigHeaders),
                                                                            
    % 2. Create the String to Sign
    CredentialScope = credential_scope(Datestamp, get(region), get(service)),
    StringToSign = string_to_sign(Alg, Amzdate, CredentialScope, CanRequest),

    % 3. Calculate the AWS Signature Version 4
    SecretKey = get(secret_key),
    SigningKey = get_signature_key(SecretKey, 
                                   Datestamp, 
                                   get(region), 
                                   get(service)),
    Signature = base16(crypto:hmac(sha256, SigningKey, StringToSign)),
    
    % 4. Create the Authorization header
    Auth = auth_header(Alg, CredentialScope, SigHeaders, Signature),

    [{"Authorization", Auth} | H].



%%% ============================================================================
%%% Internal functions
%%% ============================================================================


%% List of relevant headers.
headers(Amzdate, Amztarget) ->
    H = [{"x-amz-date", Amzdate},
         {"host", get(host)},
         {"x-amz-target", Amztarget}],
    
    case get(token) of
        undefined ->
            H;
        Token ->
            [{"x-amz-security-token", Token} | H]
    end.


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
    Access = get(access_key),
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


