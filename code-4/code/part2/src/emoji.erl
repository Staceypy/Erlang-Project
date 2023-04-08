%%%-------------------------------------------------------------------
%%% @author dell
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 10月 2022 10:26
%%%-------------------------------------------------------------------

-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
  analytics/5, get_analytics/2, remove_analytics/3, loop/1,stop/1]).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).

check_duplicate(Initial) ->
  Cur_List = lists:ukeysort(1,Initial),
  Cur_Length = length(Cur_List),
  Cur_Length.


start(Initial) ->
  Length = length(Initial),
  Cur_Length = check_duplicate(Initial),
  if
    Length /= Cur_Length ->
      {error, duplicate_shortcode_in_Initial};
    true ->
      process_flag(trap_exit, true),
      E = spawn(fun() -> loop({Initial,[],[]}) end),
      {ok,E}
  end.

loop ({Emo_List,Alias,Fun_List}=State) ->
  receive

    {From,Ref,{analytics,Short,Fun,Label,Init}} ->
      {Reply,New_State} = handle_analytics({analytics,Short,Fun,Label,Init},State),
      From ! {Ref,Reply},
      loop (New_State);
    {From,Ref,{get_analytics,Short}} ->
      {Reply,New_State} = handle_analytics({get_analytics,Short},State),
      From ! {Ref,Reply},
      loop (New_State);
    {From,Ref,{remove_analytics, Short, Label}} ->
      {Reply,New_State} = handle_analytics({remove_analytics, Short, Label},State),
      From ! {Ref,Reply},
      loop (New_State);
    {From,Ref,Request} ->
      {Reply,New_State} = handle_req(Request,State),
      From ! {Ref,Reply},
      loop (New_State)
  end.

request_reply(E,Request) ->
  Ref = make_ref(),
  E ! {self(),Ref,Request},
  receive
    {Ref,Reply} -> Reply
  end.

new_shortcode(E, Short, Emo) ->
  request_reply(E,{new_shortcode,Short,Emo}).

alias(E, Short1, Short2) ->
  request_reply(E,{alias,Short1, Short2}).

delete(E, Short) ->
  request_reply(E,{delete,Short}).
  %E ! {self(), {delete,Short}}.
  % Non-blocking

lookup(E, Short) ->
  request_reply(E,{lookup,Short}).

analytics(E, Short, Fun, Label, Init) ->
  request_reply(E,{analytics,Short,Fun,Label,Init}).

get_analytics(E, Short) ->
  request_reply(E,{get_analytics,Short}).

remove_analytics(E, Short, Label) ->
  E ! {self(), {remove_analytics, Short,Label}}.

stop(E) ->
  exit (E,ok),
  ok.

handle_analytics(Request,{Emo_List,Alias,Fun_List}=State) ->
  case Request of
    {analytics,Short,Fun,Label,Init} ->
      case lists:keyfind(Short,1,Emo_List) of
        {Short,_} ->
          Names = get_all_name(Short,Alias),
          Fun_List_copy  = Fun_List,
          case lists:search(fun({S,{L,_,_}}) -> {S, L} =:= {Short, Label} end, Fun_List_copy) of
            false ->
              New_Fun_List = [{ShortA, {Label, Fun, Init}} || ShortA <-Names] ++ Fun_List,
              NewState = {Emo_List, Alias, New_Fun_List},
              {ok,NewState};
            _ ->
              {{error, label_already_exists},State}
          end;
        false -> {{error, shortcode_not_exists},State}
      end;

    {get_analytics,Short} ->
      case lists:keyfind(Short,1,Emo_List) of
       _ ->
          Stats = [{L, Value}|| {S, {L,Fun,Value}} <- Fun_List, S =:= Short],
          {{ok, Stats},State};
        false -> {{error, shortcode_not_exists},State}
      end;

    {remove_analytics, Short, Label} ->
      case lists:search(fun({S,{L,_,_}}) -> {S, L} =:= {Short, Label} end, Fun_List) of
        false ->
          {{ok,no_such_analytics_function},State};
        _ ->
          Names = get_all_name(Short, Alias),
          New_Fun_List = [{S,{L,F,V}} || {S,{L,F,V}} <- Fun_List,
            case lists:member(S, Names) of
              false -> true;
              _ -> L =/= Label end],
          NewState = {Emo_List, Alias, New_Fun_List},
          {ok,NewState}
      end

  end.

handle_req(Request, {Emo_List,Alias,Fun_List}=State) ->
  case Request of
    {new_shortcode,Short,Emo} ->
      case lists:keyfind(Short,1,Emo_List) of
        false ->
          New_Emo_List = [{Short, Emo} | Emo_List],
          New_State = {New_Emo_List,Alias,Fun_List},
          {ok, New_State};
        {Short,_} ->
          {{error, shortcode_exists},State}
      end;

    {lookup,Short} ->
      case lists:keyfind(Short,1,Emo_List) of
        false -> {no_emoji,State};
        {_,Value} ->
          New_State = run_Analytic(Short,State),
          {{ok, Value},New_State}
      end;

    {delete,Short} ->
      case lists:keyfind(Short,1,Emo_List) of
        false -> {{ok, shortcode_not_exists},State};
        {_,Value} ->
          New_State = helper_delete(Short,State),
          {ok, New_State}
      end;

    {alias,Short1,Short2} ->
      case lists:keyfind(Short1,1,Emo_List) of
        false -> {{error, shortcode_not_exists},State};
        {Short1,Emo} ->
          case lists:keyfind(Short2,1,Emo_List) of
            {_,_} -> {{error, alias_already_exists},State};
            false ->
              New_State = helper_alias(Short1,Short2,State),
              {ok, New_State}
          end
      end

end.

%% Helper Functions
helper_alias(Short1,Short2,{Emo_List,Alias,Fun_List}=State) ->
  case lists:keyfind(Short1,1,Alias) of
    false ->
      NewAlias = [{Short2,Short1}|Alias];
    {Short1,TrueName} ->
      NewAlias = [{Short2,TrueName}|Alias]
  end,
  {_,Emo} = lists:keyfind(Short1,1,Emo_List),
  New_Emo_List = [{Short2,Emo}|Emo_List],

  New_State = {New_Emo_List, NewAlias,Fun_List},
  New_State.

helper_delete(Short,{Emo_List,Alias,Fun_List}=State) ->

  Names = get_all_name(Short,Alias),
  NamesCopy = Names, NamesCopyAna = Names,
  New_Emo_List = deleteAlias(Names,Emo_List),
  NewAlias = deleteAlias(NamesCopy,Alias),
  New_Fun_List = deleteAliasFromAna(Short, NamesCopyAna,Fun_List),
  New_State = {New_Emo_List, NewAlias,New_Fun_List},
  New_State.

get_all_name(Short,Alias)->
  case lists:keyfind(Short,1,Alias) of
    false ->
      Names = find_all_name(Short,Alias);
    {Short,TrueName} ->
      Names = find_all_name(TrueName,Alias)
  end,
  Names.

find_all_name(Short,Alias) ->
  AllName = [Fst|| {Fst, Snd} <- Alias, Snd =:= Short],
  [Short|AllName].

deleteAlias ([],List) -> List;
deleteAlias ([Head|Rest],List) ->
  NewList = lists:keydelete(Head,1,List),
  NewL = lists:keydelete(Head,2,NewList),
  deleteAlias(Rest,NewL).

deleteAliasFromAna(Short,Names,Fun_List) ->
  case lists:keyfind(Short,1,Fun_List) of
    false -> New_Fun_List = Fun_List;
    _ ->
      New_Fun_List = [{ShortA, Res} || {ShortA, Res} <- Fun_List ,
        case lists:search(fun(X) -> X =:= ShortA end, Names) of
          false -> true;
          _ ->false end]
  end,
  New_Fun_List.


run_Analytic(Short,{Emo_List,Alias,Fun_List}=State) ->
  Names = get_all_name(Short,Alias),

  NewAnalytics = [case lists:member(ShortA, Names) of
                    true -> {ShortA, {Label, Fun, run_Fun(Short, Fun, Init)}};
                    false-> {ShortA, {Label, Fun, Init}} end
    || {ShortA, {Label, Fun, Init}} <- Fun_List],
  NewState = {Emo_List, Alias, NewAnalytics},
  NewState.

run_Fun(Short, Fun, Value) ->
  try
    Fun(Short, Value)
  catch
    _ -> Value
  end.

