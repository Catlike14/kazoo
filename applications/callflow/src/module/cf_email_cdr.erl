%%%-------------------------------------------------------------------
%%% @copyright (C) 2019 N4COM Srl
%%% @contributors
%%%     Mattia Alfonso
%%%-------------------------------------------------------------------

-module(cf_email_cdr).

-behaviour(gen_cf_action).

-export([handle/2]).

-include("callflow.hrl").

-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Email = kz_json:get_value(<<"email">>, Data),
    Note = kz_json:get_value(<<"note">>, Data),
    CallerID = knm_util:pretty_print(kapps_call:from_user(Call)),
    CalledID = knm_util:pretty_print(kapps_call:to_user(Call)),
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),

    BodyHTML = list_to_binary(io_lib:format("Salve:<br><br>la informiamo che ha ricevuto sul numero ~s una nuova chiamata da ~s il ~2..0B-~2..0B-~4..0B alle ore ~2..0B:~2..0B:~2..0B.<br>Grazie.<br><br>~s<br><br><div align=center>--N4COM--</div>", [CalledID, CallerID, Day, Month, Year, Hour, Minute, Second, Note])),
    BodyPlain = list_to_binary(io_lib:format("Salve:\n\nla informiamo che ha ricevuto sul numero ~s una nuova chiamata da ~s il ~2..0B-~2..0B-~4..0B alle ore ~2..0B:~2..0B:~2..0B.\nGrazie.\n\n~s\n\t\t\t--N4COM--", [CalledID, CallerID, Day, Month, Year, Hour, Minute, Second, Note])),

    lager:info("Sending call notification to ~p", [Email]),
    From = <<"no_reply@pbx.n4com.com">>,
    Emails = [{<<"reply_to">>,undefined},{<<"from">>,[From]},{<<"bcc">>,undefined},{<<"cc">>,undefined},{<<"to">>,[Email]}],
    RenderedTemplates = props:filter_undefined([{<<"text/html">>, BodyHTML}, {<<"text/plain">>, BodyPlain}]),
    teletype_util:send_email(Emails, <<"Notifica nuova chiamata">>, RenderedTemplates),
    cf_exe:continue(Call).
