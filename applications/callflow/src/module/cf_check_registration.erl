%%%-------------------------------------------------------------------
%%% @doc
%%% Check if at least one device is registered
%%% @end
%%% @contributors
%%%   Mattia Alfonso
%%%-------------------------------------------------------------------
-module(cf_check_registration).

-behaviour(gen_cf_action).

-export([handle/2]).

-include("callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    DevicesId = kz_json:get_list_value(<<"devices">>, Data, []),
    Registered = check_registrations(DevicesId, Call),
    lager:info("At least one device is registered? ~s", [Registered]),
    cf_exe:continue(Registered, Call).


-spec check_registrations(list(), kapps_call:call()) -> boolean().
check_registrations([], _) -> <<"unregistered">>;
check_registrations(Devices, Call) ->
    AccountDb = kapps_call:account_db(Call),
    Realm = kapps_call:account_realm(Call),
    lager:debug("Iterating on ~s: ~p", [AccountDb, Devices]),
    iterate(Devices, AccountDb, Realm).

-spec iterate(list(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
iterate([], _, _) -> <<"unregistered">>;
iterate([Device | Devices], AccountDb, Realm) ->
    DeviceId = kz_json:get_value(<<"id">>, Device),
    case is_registered(DeviceId, AccountDb, Realm) of
        true ->
            lager:debug("Device ~s is registered", [DeviceId]),
            <<"registered">>;
        false ->
            lager:info("Device ~s is not registered", [DeviceId]),
            iterate(Devices, AccountDb, Realm)
    end.

-spec is_registered(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_registered(DeviceId, AccountDb, Realm) ->
    case kz_endpoint:get(DeviceId, AccountDb) of
        {'ok', Endpoint} ->
            User = kzd_devices:sip_username(Endpoint),
            case lookup_registration(Realm, User) of
                {'ok', _} -> true;
                {'error', Error} ->
                    lager:debug("Error on lookup_registration ~s@~s: ~p", [User, Realm, Error]),
                    false
            end;
        {'error', Error} ->
            lager:info("Error getting device ~s on account ~s: ~p", [DeviceId, AccountDb, Error]),
            false
    end.

%Wrapper of ecallmgr_registrar:lookup_registration
-spec lookup_registration(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                 {'ok', kz_json:object()} |
                                 {'error', 'not_found'}.
lookup_registration(Realm, Username) ->
    Req = [{<<"Realm">>, Realm}
          ,{<<"Username">>, Username}
          ,{<<"Fields">>, [<<"Authorizing-ID">>]}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case kz_amqp_worker:call_collect(Req
                                    ,fun kapi_registration:publish_query_req/1
                                    ,{'ecallmgr', 'true'}
                                    )
    of
        {'error', _E} -> {'error', 'not_found'};
        {_, JObjs} ->
            case parse_query_response(JObjs) of
                true -> {'ok', JObjs};
                false -> {'error', 'not_fond'}
            end
    end.

-spec parse_query_response(kz_json:objects()) -> true|false.
parse_query_response([]) -> false;
parse_query_response([JObj|JObjs]) ->
    case kz_json:get_value(<<"Event-Name">>, JObj) of
        <<"reg_query_resp">> -> true;
        <<"reg_query_error">> -> parse_query_response(JObjs)
    end.
