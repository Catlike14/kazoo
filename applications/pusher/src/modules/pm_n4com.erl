%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(pm_n4com).
-behaviour(gen_server).

-include("pusher.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ,test/1
        ]).

-record(state, {tab :: ets:tid() | atom()}).
-type state() :: #state{}.

-define(N4COM_MAP, [{<<"Call-ID">>, [<<"call_id">>]}
                      ,{<<"caller_number">>, [<<"">>]}
                      ,{<<"caller_name">>, [<<"">>]]}
                      ,{<<"Alert-Params">>, [<<"alert">>, <<"loc-args">>]}
                      ,{<<"Sound">>, [<<"sound">>]}
                      ,{<<"Call-ID">>, [<<"Call-ID">>]}
                      ,{<<"Payload">>, fun kz_json:merge/2}
                      ]).

-define(CONNECT_TIMEOUT_MS
       ,kapps_config:get_integer(?CONFIG_CAT, [<<"pusher">>, <<"connect_timeout_ms">>], 10 * ?MILLISECONDS_IN_SECOND)
       ).
-define(HTTP_OPTS, [{'connect_timeout', ?CONNECT_TIMEOUT_MS}]).

-spec test(any()) -> any().
test(JString) ->
    JObj = kz_json:decode(JString),
    pusher_listener:handle_push(JObj, []).

-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [],[]).

-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_util:put_callid(?MODULE),
    lager:debug("starting server"),
    {'ok', #state{tab='undefined'}}.

-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'push', JObj}, State) ->
    lager:debug("process a push"),
    lager:debug("~p", [JObj]),
    TokenApp = kz_json:get_value(<<"Token-App">>, JObj),
    maybe_send_push_notification(get_mw(TokenApp), JObj),
    {'noreply', State};
handle_cast('stop', State) ->
    {'stop', 'normal', State}.

-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Request, State) ->
    {'noreply', State}.

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{}) ->
    'ok'.

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec maybe_send_push_notification(kz_term:api_binary(), kz_json:object()) -> any().
maybe_send_push_notification('undefined', _JObj) -> lager:debug("no server to send push");
maybe_send_push_notification(Endpoint, JObj) ->
    TokenID = kz_json:get_value(<<"Token-ID">>, JObj),
    Message = kz_json:from_list([{<<"data">>, build_payload(JObj)}]),
    send_push_notification(Endpoint, Message, TokenID).

-spec send_push_notification(kz_term:api_binary(), kz_json:object(), kz_term:api_binary()) -> any().
send_push_notification(Endpoint, Message, TokenID) ->
    Body = kz_json:encode(Message),
    URL = binary:replace(Endpoint, <<"{token}">>, TokenID),
    Response = kz_http:post(URL, [{"Content-Type", "application/json"}], Body, ?HTTP_OPTS),
    lager:debug("pushing to ~s: ~p", [URL, Message]),
    handle_resp(Response).

-spec handle_resp(kz_http:ret()) -> any().
handle_resp({'ok', 200, _Headers, _Body}) ->
    lager:debug("push request sent successfully");
handle_resp({'ok', RetCode, _Headers, Body}) ->
    lager:warning("unexpected response on push request: got HTTP ~p with body: ~p", [RetCode, Body]);
handle_resp({'error', Error}) ->
    lager:error("push request failed with error: ~p", [Error]).

-spec build_payload(kz_json:object()) -> kz_json:object().
build_payload(JObj) ->
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    CallerNumber = kz_json:get_value([<<"Payload">>,<<"caller-id-number">>], JObj),
    CallerName = kz_json:get_value([<<"Payload">>,<<"caller-id-name">>], JObj),
    Msg = [{<<"call_id">>, CallId}
          ,{<<"caller_number">>, CallerNumber}
          ,{<<"caller_name">>, CallerName}
          ],
    kz_json:from_list(Msg).

-spec get_mw(kz_term:api_binary()) -> kz_types:api_binary().
get_mw('undefined') -> 'undefined';
get_mw(App) -> kapps_config:get_binary(?CONFIG_CAT, [<<"pusher">>, <<"endpoint">>], 'undefined', App).
