%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019, N4COM
%%% @doc Enable/disable CFB, CFDA.
%%%     Data:
%%%     -branch (USER_BUSY, NO_ANSWER, _, ...)
%%%     -action (activate/deactivate)
%%% @author Alfonso Mattia
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_forwards_toggle).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".call_forwards_toggle">>).
-define(MIN_CALLFWD_NUMBER_LENGTH, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"min_callfwd_number_length">>, 3)).
-define(MAX_CALLFWD_NUMBER_LENGTH, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"max_callfwd_number_length">>, 20)).
-define(CALLFWD_NUMBER_TIMEOUT, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"callfwd_number_timeout">>, 8000)).
-define(MAX_SAVE_RETRIES, 3).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successful.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case get_endpoint_number(Data, Call) of
        {'error', _} ->
            catch({'ok', _} = kapps_call_command:b_prompt(<<"cf-not_available">>, Call)),
            cf_exe:stop(Call);
        {'ok', Number} ->
            {'ok', _} = kapps_call_command:b_answer(Call),
            AccountDb = kapps_call:account_db(Call),
            {'ok', Callflow} = kz_datamgr:get_single_result(AccountDb, <<"callflows/listing_by_number">>, [{'key', Number}]),
            CallflowId = kz_json:get_value(<<"id">>, Callflow),
            {'ok', Callflow1} = kz_datamgr:open_cache_doc(AccountDb, CallflowId),
            lager:info("Callflow to update is ~s", [CallflowId]),
            Branch = kz_json:get_ne_binary_value(<<"branch">>, Data),
            CaptureGroup = kapps_call:kvs_fetch('cf_capture_group', Call),
            DataAction = kz_json:get_ne_binary_value(<<"action">>, Data),
            case DataAction of
                <<"activate">> ->
                    {UpdatedFlow, NewNumber} = cf_activate(Callflow1, Branch, CaptureGroup, Call),
                    save_callflow(AccountDb, kzd_callflow:set_flow(Callflow1, UpdatedFlow)),
                    _ = try
                        {'ok', _} = kapps_call_command:b_prompt(<<"vm-saved">>, Call),
                        {'ok', _} = kapps_call_command:b_prompt(<<"cf-now_forwarded_to">>, Call),
                        {'ok', _} = kapps_call_command:b_say(NewNumber, Call)
                    catch
                        _:_ -> 'ok'
                    end;
                <<"deactivate">> ->
                    UpdatedFlow = cf_deactivate(Callflow1, Branch, Call),
                    save_callflow(AccountDb, kzd_callflow:set_flow(Callflow1, UpdatedFlow)),
                    catch({'ok', _} = kapps_call_command:b_prompt(<<"cf-disabled">>, Call));
                Action ->
                    lager:info("improper action in Data: ~p", [Action]),
                    'undefined'
            end
    end,
    cf_exe:continue(Call).

-spec get_endpoint_number(kz_json:object(), kapps_call:call()) -> {'ok', any()} | {'error', any()}.
get_endpoint_number(_Data, Call) ->
    AuthorizingId = kapps_call:authorizing_id(Call),
    OwnerId =
        case kz_attributes:owner_id(AuthorizingId, Call) of
            'undefined' -> AuthorizingId;
            UserId -> UserId
        end,
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), OwnerId) of
        {'ok', EndpointJObj} ->
            InternalNumber = kz_json:get_ne_value([<<"caller_id">>, <<"internal">>, <<"number">>], EndpointJObj),
            lager:info("loaded internal number from ~s: ~s", [OwnerId, InternalNumber]),
            {'ok', InternalNumber};
        {'error', R} ->
            lager:info("failed to load internal number from ~s, ~w", [OwnerId, R]),
            {'error', undefined}
    end.

-spec cf_activate(kz_json:object(), kz_term:ne_binary(), kz_term:api_binary(), kapps_call:call()) -> any().
cf_activate(Callflow, Branch, CaptureGroup, Call) ->
    lager:info("activating call forwarding on ~s to '~s'", [Branch, CaptureGroup]),
    Number = cf_update_number(Callflow, CaptureGroup, Call),
    lager:info("number is ~s", [Number]),
    Flow = find_and_change_transfer(kzd_callflow:flow(Callflow), Number, Branch, Call),
    {Flow, Number}.

-spec cf_deactivate(kz_json:object(), kz_term:ne_binary(), kapps_call:call()) -> any().
cf_deactivate(Callflow, Branch, Call) ->
    lager:info("deactivating call forwarding on ~s", [Branch]),
    find_and_remove_transfer(kzd_callflow:flow(Callflow), Branch, Call).

-spec cf_update_number(kz_json:object(), kz_term:api_binary(), kapps_call:call()) -> any().
cf_update_number(Callflow, CaptureGroup, Call)
    when is_atom(CaptureGroup); CaptureGroup =:= <<>> ->
    EnterNumber = kapps_call:get_prompt(Call, <<"cf-enter_number">>),

    NoopId = kapps_call_command:play(EnterNumber, Call),
    Min = ?MIN_CALLFWD_NUMBER_LENGTH,

    lager:info("collecting digits from user"),

    case kapps_call_command:collect_digits(?MAX_CALLFWD_NUMBER_LENGTH
                                          ,?CALLFWD_NUMBER_TIMEOUT
                                          ,2000
                                          ,NoopId
                                          ,Call
                                          )
    of
        {'ok', Short} when byte_size(Short) < Min ->
            lager:debug("too short of input(~p): '~s'", [Min, Short]),
            cf_update_number(Callflow, CaptureGroup, Call);
        {'ok', Number} ->
            lager:info("update call forwarding number with ~s", [Number]),
            Number;
        {'error', _E} ->
            lager:debug("error collecting digits: ~p", [_E]),
            exit('normal')
    end;
cf_update_number(_Callflow, CaptureGroup, _) ->
    lager:info("update call forwarding number with '~s'", [CaptureGroup]),
    CaptureGroup.

-spec find_and_change_transfer(kz_json:object(), any(), any(), kapps_call:call()) -> kz_json:object().
find_and_change_transfer(Action, Number, Branch, Call) ->
    Module = kz_json:get_binary_value(<<"module">>, Action),
    lager:info("module is ~p", [Module]),
    NewAction = case Module of
                    <<"user">> -> alter_endpoint(Action, Number, Branch, Call);
                    <<"device">> -> alter_endpoint(Action, Number, Branch, Call);
                    _ -> Action
                end,
    NewAction.

-spec alter_endpoint(kz_json:object(), any(), kz_term:ne_binary(), kapps_call:call()) -> kz_json:object().
alter_endpoint(Module, Number, Branch, Call) ->
    %AuthorizingId = kapps_call:authorizing_id(Call),
    InternalCallerIdNumber = kapps_call:caller_id_number(Call),
%        case kz_attributes:owner_id(AuthorizingId, Call) of
%            'undefined' -> <<"">>;
%            UserId -> i
%        end,
    AccountDb = kapps_call:account_db(Call),
    AccountId = kapps_call:account_id(Call),
    Device = create_device(Number, InternalCallerIdNumber, Branch, AccountDb, AccountId),
    OldChildren = kz_json:get_value(<<"children">>, Module),
    Timeout = case Branch of
        <<"NO_ANSWER">> ->
            case kz_json:get_value(Branch, OldChildren, undefined) of
                undefined -> kz_json:get_value([<<"data">>, <<"timeout">>], Module, 60);
                OldBranch -> kz_json:get_value([<<"data">>, <<"timeout">>], OldBranch, 60)
            end;
        _ -> 60
    end,

    Data = kz_json:set_value(<<"id">>, kz_doc:id(Device), kz_json:new()),
    Data1 = kz_json:set_value(<<"timeout">>, Timeout, Data),
    Data2 = kz_json:set_value(<<"dial_strategy">>, <<"simultaneous">>, Data1),
    Data3 = kz_json:set_value(<<"delay">>, 0, Data2),
    Data4 = kz_json:set_value(<<"can_call_self">>, true, Data3),
    Flow = kz_json:set_value(<<"data">>, Data4, kz_json:new()),
    Flow1 = kz_json:set_value(<<"module">>, <<"device">>, Flow),
    Flow2 = kz_json:set_value(<<"children">>, [], Flow1),
    NewChildren = kz_json:set_value(Branch, Flow2, OldChildren),
    NewModule = case Branch of
        <<"NO_ANSWER">> -> 
            case kz_json:get_value(Branch, OldChildren, undefined) of
                undefined -> kz_json:set_value([<<"data">>, <<"timeout">>], 30, Module);
                _ -> Module
            end;
        _ -> Module
    end,

    case kz_json:get_value(Branch, OldChildren, undefined) of
        undefined -> 'ok';
        OldBranch1 ->
            EndpointId = kz_json:get_value([<<"data">>, <<"id">>], OldBranch1),
            kz_datamgr:del_doc(AccountDb, EndpointId)
    end,

    lager:info("New children in ~p", [NewChildren]),
    kz_json:set_value(<<"children">>, NewChildren, NewModule).

save_callflow(AccountDb, Callflow) ->
    save_callflow(AccountDb, Callflow, ?MAX_SAVE_RETRIES).
save_callflow(_AccountDb, Callflow, -1) ->
    lager:error("failed to update callflow to ~p", [kz_json:encode(Callflow)]);
save_callflow(AccountDb, Callflow, Retries) ->
    case kz_datamgr:save_doc(AccountDb, Callflow) of
        {'error', 'conflict'} ->
            save_callflow(AccountDb, Callflow, Retries);
        {'error', _} ->
            save_callflow(AccountDb, Callflow, Retries - 1);
        {'ok', _} ->
            lager:info("updated flow to ~p", [kz_json:encode(Callflow)])
    end.

-spec find_and_remove_transfer(kz_json:object(), any(), kapps_call:call()) -> kz_json:object().
find_and_remove_transfer(Action, Branch, Call) ->
    NewAction = case kz_json:get_binary_value(<<"module">>, Action) of
                    <<"user">> -> remove_endpoint(Action, Branch, Call);
                    <<"device">> -> remove_endpoint(Action, Branch, Call);
                    _ -> Action
                end,
    NewAction.

-spec remove_endpoint(kz_json:object(), any(), kapps_call:call()) -> kz_json:object().
remove_endpoint(Module, Branch, Call) ->
lager:debug("Removing endpoint"),
    OldChildren = kz_json:get_value(<<"children">>, Module),
    case kz_json:get_value([Branch, <<"data">>, <<"id">>], OldChildren, undefined) of
        undefined -> 'ok';
        EndpointId ->
lager:debug("Deleting endpoing ~p", [EndpointId]),
            DbName = kapps_call:account_db(Call),
            kz_datamgr:del_doc(DbName, EndpointId)
    end,
    NewChildren = kz_json:delete_key(Branch, OldChildren),
    NewModule = kz_json:set_value(<<"children">>, NewChildren, Module),
lager:debug("Branch is ~p", [Branch]),
    
    case Branch of
        <<"NO_ANSWER">> ->
            Timeout = kz_json:get_value([Branch, <<"data">>, <<"timeout">>], OldChildren, 60),
lager:debug("Timeout is ~p", [Timeout]),
            kz_json:set_value([<<"data">>, <<"timeout">>], Timeout, NewModule);
        _ -> NewModule
    end.

-spec create_device(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> any().
create_device(Number, InternalCallerIdNumber, Branch, AccountDb, AccountId) ->
    Name = <<Branch/binary, "_", Number/binary>>,
    lager:info("creating new device ~s", [Name]),
    CallerId = kz_json:set_value([<<"internal">>, <<"number">>], InternalCallerIdNumber, kz_json:new()),
    CallerId1 = kz_json:set_value([<<"external">>, <<"number">>], InternalCallerIdNumber, CallerId),
    CallerId2 = kz_json:set_value([<<"emergency">>], kz_json:new(), CallerId1),
    Setters = [{fun kzd_devices:set_call_forward_enabled/2, true}
              ,{fun kzd_devices:set_call_forward_number/2, Number}
              ,{fun kzd_devices:set_call_forward_require_keypress/2, true}
              ,{fun kzd_devices:set_call_forward_failover/2, false}
              ,{fun kzd_devices:set_call_forward_direct_calls_only/2, false}
              ,{fun kzd_devices:set_call_forward_ignore_early_media/2, false}
              ,{fun kzd_devices:set_call_forward_keep_caller_id/2, false}
              ,{fun kzd_devices:set_call_forward_substitute/2, true}
              ,{fun kzd_devices:set_name/2, Name}
              %,{fun kzd_devices:set_owner_id/2, OwnerId}
              ,{fun kzd_devices:set_device_type/2, <<"sip_device">>}
              %,{fun kzd_devices:set_caller_id/2, kz_json:set_value([<<"internal">>,<<"number">>], InternalCallerIdNumber, kz_json:new())}
              ,{fun kzd_devices:set_caller_id/2, CallerId2}
              ,{fun kzd_devices:set_sip_invite_format/2, <<"contact">>}
              ,{fun kzd_devices:set_sip_username/2, Name}
              ,{fun kz_doc:set_account_db/2, AccountDb}
              ,{fun kz_doc:set_account_id/2, AccountId}
              ],
    Device = kz_doc:setters(kzd_devices:new(), Setters),
    save_device(AccountDb, Device).


save_device(AccountDb, Device) ->
    save_device(AccountDb, Device, ?MAX_SAVE_RETRIES).
save_device(_AccountDb, Device, -1) ->
    lager:error("failed to update device to ~p", [kz_json:encode(Device)]);
save_device(AccountDb, Device, Retries) ->
    case kz_datamgr:save_doc(AccountDb, Device) of
        {'error', 'conflict'} ->
            save_callflow(AccountDb, Device, Retries);
        {'error', _} ->
            save_callflow(AccountDb, Device, Retries - 1);
        {'ok', Device1} ->
            lager:info("saved device ~p", [kz_json:encode(Device)]),
            Device1
    end.

%    Device = kzd_device:new(),
%    Device1 = kzd_device:set_call_forward_enabled(Device, true),
%    Device2 = kzd_device:set_call_forward_number(Device1, Number),
%    Device3 = kzd_device:set_call_forward_require_keypress(Device2, true),
%    Device4 = kzd_device:set_call_forward_failover(Device3, false),
%    Device5 = kzd_device:set_call_forward_direct_calls_only(Device4, false),
%    Device6 = kzd_device:set_call_forward_ignore_early_media(Device5, false),
%    Device7 = kzd_device:set_call_forward_keep_caller_id(Device6, false),
%    Device8 = kzd_device:set_call_forward_substite(Device7, true),
%
%    Device9 = kzd_device:set_device_type(Device8, <<"cellphone">>),
%    Device10 = kzd_device:set_enabled(Device9, true),
%    Device11 = kzd_device:set_name(Device10, Number),
%    Device12 = kzd_device:set_owner_id(Device11, OwnerId),
%
