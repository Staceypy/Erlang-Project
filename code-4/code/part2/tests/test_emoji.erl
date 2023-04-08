-module(test_emoji).

-export([test_all/0]).

% We'll use EUnit
-include_lib("eunit/include/eunit.hrl").
hit(_, N) -> N+1.

test_all() -> eunit:test(testsuite(), [verbose]).

testsuite() ->
  [ {"Basic behaviour", spawn,
    [ test_start_server(),
      test_stop(),
      test_shortcode(),
      test_alias(),
      test_lookup(),
      test_delete(),
      test_analytics(),
      test_get_analytics(),
      test_remove_analytics()
    ]
  }
  ].
test_start_server() ->
  {"We can call start/1 and it does not crash",
    fun () ->
      ?assertMatch({ok, _}, emoji:start([]))
    end }.

test_stop() ->
  {"Start the server and stop it",
    fun () ->
      Initial = [],
      {ok, E} = emoji:start(Initial),
      emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
      ?assertEqual(ok, emoji:stop(E))
    end }.

test_shortcode() ->
  [{"Register new shortcode",
    fun () ->
      {ok, E} = emoji:start([{"thumbsup", <<240,159,145,141>>}]),
      ?assertEqual(ok, emoji:new_shortcode(E, "smiley", <<240,159,152,131>>))
    end },
    {"Register new shortcode2",
      fun () ->
        {ok, E} = emoji:start([{"thumbsup", <<240,159,145,141>>}]),
        ?assertEqual(ok, emoji:new_shortcode(E, "poop", <<"\xF0\x9F\x92\xA9">>))
      end }
  ].
test_alias() ->
  [{"Add an alias hankey for poop",
    fun () ->
      {ok, E} = emoji:start([{"poop", <<"\xF0\x9F\x92\xA9">>}]),
      ?assertEqual(ok, emoji:alias(E, "poop", "hankey"))
    end },
    {"Add an alias haha for smiley",
      fun () ->
        {ok, E} = emoji:start([{"smiley", <<240,159,152,131>>}]),
        ?assertEqual(ok, emoji:alias(E, "smiley", "haha"))
      end },
    {"Add another alias for alias",
      fun () ->
        {ok, E} = emoji:start([{"smiley", <<240,159,152,131>>}]),
        emoji:alias(E, "smiley", "haha"),
        ?assertEqual(ok, emoji:alias(E, "haha", "laugh"))
      end },
    {"add alias to self",
      fun () ->
        {ok, E} = emoji:start([{"smiley", <<240,159,152,131>>}]),
        ?assertMatch({error,_}, emoji:alias(E, "smiley", "smiley"))
      end }
  ].



test_lookup() ->
  [{"lookup short",
    fun () ->
      {ok, E} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ?assertEqual({ok, <<240,159,152,131>>}, emoji:lookup(E, "smiley"))
    end },

    {"lookup alias",
      fun () ->
        {ok, E} = emoji:start([{"smiley", <<240,159,152,131>>}]),
        emoji:alias(E, "smiley", "haha"),
        ?assertEqual({ok, <<240,159,152,131>>}, emoji:lookup(E, "haha"))
      end },
    {"lookup alias for alias",
      fun () ->
        {ok, E} = emoji:start([{"smiley", <<240,159,152,131>>}]),
        emoji:alias(E, "smiley", "haha"),
        emoji:alias(E, "haha", "laugh"),
        ?assertEqual({ok, <<240,159,152,131>>}, emoji:lookup(E, "laugh"))
      end }
  ].

test_delete() ->
  [{"Delete from empty",
    fun () ->
      Initial = [],
      {ok, E} = emoji:start(Initial),
      emoji:delete(E, "smiley"),
      ?assertEqual(no_emoji, emoji:lookup(E, "smiley"))
    end },
    {"Delete shortcode",
      fun () ->
        {ok, E} = emoji:start([]),
        emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
        emoji:delete(E, "smiley"),
        ?assertEqual(no_emoji, emoji:lookup(E, "smiley"))
      end },
    {"Delete one alias",
      fun () ->
        {ok, E} = emoji:start([]),
        emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
        emoji:alias(E, "smiley", "haha"),
        emoji:delete(E, "smiley"),
        ?assertEqual(no_emoji, emoji:lookup(E, "smiley")),
        ?assertEqual(no_emoji, emoji:lookup(E, "haha"))
      end },
    {"Delete alias of alias",
      fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"thumbsup", <<240,159,145,141>>}],
        {ok, E} = emoji:start(Initial),
        emoji:alias(E, "smiley", "haha"),
        emoji:alias(E, "haha", "laugh"),
        emoji:delete(E, "laugh"),
        ?assertEqual(no_emoji, emoji:lookup(E, "smiley")),
        ?assertEqual(no_emoji, emoji:lookup(E, "haha")),
        ?assertEqual(no_emoji, emoji:lookup(E, "laugh")),
        ?assertEqual({ok, <<240,159,145,141>>}, emoji:lookup(E, "thumbsup"))
      end }].

test_analytics() ->
  [{"analytics1",
    fun () ->
      {ok, E} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ?assertEqual(ok, emoji:analytics(E, "smiley", fun(_, N) -> N+1 end, "Counter", 0))
    end },

    {"Register analytics1 two times",
      fun () ->
        {ok, E} = emoji:start([{"smiley", <<240,159,152,131>>}]),
        emoji:analytics(E, "smiley", fun hit/2, "Counter", 0),
        ?assertEqual({error, label_already_exists}, emoji:analytics(E, "smiley", fun hit/2, "Counter", 0))
      end },

    {"analytics2",
      fun () ->
        {ok, E} = emoji:start([{"smiley", <<240,159,152,131>>}]),
        emoji:analytics(E, "smiley", fun(_, N) -> N+1 end, "Counter", 0),
        ?assertEqual(ok, emoji:analytics(E, "smiley", fun(_, N) -> N+2 end, "Counter2", 0))
      end }
  ].

test_get_analytics() ->
  [{"get_analytics",
    fun () ->
      {ok, E} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      emoji:analytics(E, "smiley", fun(_, N) -> N+1 end, "Counter", 0),
      emoji:analytics(E, "smiley", fun(_, N) -> N+2 end, "Counter2", 0),
      ?assertMatch({ok,[{"Counter2",0},{"Counter",0}]}, emoji:get_analytics(E, "smiley"))
    end },
    {"get_analytics for Last function",
      fun () ->
        {ok, E} = emoji:start([{"smiley", <<240,159,152,131>>}]),
        emoji:analytics(E, "smiley", fun (S, _) -> S end, "Last", none),
        ?assertEqual({ok,[{"Last",none}]}, emoji:get_analytics(E, "smiley"))
      end },
    {"getanalytics_after_lookup",
      fun () ->
        {ok, E} = emoji:start([{"smiley", <<240,159,152,131>>}]),
        emoji:analytics(E, "smiley", fun(_, N) -> N+1 end, "Counter", 0),
        emoji:analytics(E, "smiley", fun(_, N) -> N+2 end, "Counter2", 0),
        emoji:lookup(E, "smiley"),
        ?assertMatch({ok,[{"Counter2",2},{"Counter",1}]}, emoji:get_analytics(E, "smiley"))
      end },
    {"get_analytics for alias",
      fun () ->
        {ok, E} = emoji:start([{"smiley", <<240,159,152,131>>}]),
        emoji:alias(E, "smiley", "haha"),
        emoji:analytics(E, "smiley", fun(_, N) -> N+1 end, "Counter", 0),
        emoji:analytics(E, "haha", fun(_, N) -> N+2 end, "Counter2", 0),
        ?assertMatch({ok,[{"Counter2",0},{"Counter",0}]}, emoji:get_analytics(E, "haha"))
      end }
  ].

test_remove_analytics() ->
  [{"Remove analytics shortcode not exist",
    fun () ->
      {ok, E} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      emoji:analytics(E, "smiley", fun(_, N) -> N+1 end, "Counter", 0),
      emoji:remove_analytics(E, "smiley", "Counter"),
      ?assertEqual({ok, []}, emoji:get_analytics(E, "smiley"))
    end },

    {"Remove analytics fine",
      fun () ->
        Initial = [],
        {ok, E} = emoji:start(Initial),
        emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
        emoji:alias(E, "smiley", "haha"),
        emoji:analytics(E, "smiley", fun(_, N) -> N+1 end, "Counter", 0),
        emoji:remove_analytics(E, "haha", "Counter"),
        ?assertEqual({ok,[]}, emoji:get_analytics(E, "smiley"))
      end }].

