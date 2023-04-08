-module(async).

-export([new/2, wait/1, poll/1, loop/1]).
new(Fun, Arg) ->spawn(fun () ->
    Me = self(),
    spawn (fun () ->
        try
            Res = Fun(Arg),
            Me !{ok, Res}
        catch
            _:Ex -> Me! {exception, Ex}
        end
    end),
    loop({init})
end).

wait(Aid) ->
    case poll(Aid) of
        nothing -> wait(Aid);
        {ok, Res} -> Res;
        {exception, Ex} -> throw(Ex)
    end.

poll(Aid) ->
    Aid !{self(), poll_request},
    receive
        {ok, Res} -> {ok, Res};
        {exception, Ex} -> {exception,Ex};
        {nothing} -> nothing
    end.

loop (State)->
    receive
        {ok, Res} -> loop({ok, Res});
        {exception, Ex} -> loop({exception, Ex});
        {From, poll_request} ->
            case State of
                {init} ->
                    From !{nothing}, 
                    loop(State);
                 _  -> From !State, loop(State)
            end
    end.
