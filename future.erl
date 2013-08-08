-module(future).

-export([%% Future initialisation
         init/0,
         run/1,
         %% Callbacks
         onComplete/2,
         onSuccess/2,
         onFailure/2,
         %% Combinators
         map/2,
         flatMap/2,
         filter/2,
         foreach/2,
         %% Blocking operations
         await_result/1,
         await_ready/1,
         %% Promises
         success/2,
         failure/2
        ]).

%% Types and records  ----------------------------------------------------------
-type future_id() :: pid().
-type future_state() :: running | success | failure.

-record(future, {
          id                 :: future_id(),
          state  = running   :: future_state(),
          result = undefined :: any(),
          success_callbacks = [] :: list(fun()),
          failure_callbacks = [] :: list(fun())
         }).
-type future() :: #future().

%% Future initialisation -------------------------------------------------------

-spec init() -> true.
init() ->
    ets:new(future, [named_table,
                     public,
                     {keypos, #future.id},
                     {read_concurrency, true}]).

-spec run(fun()) -> future_id().
run(Fun) ->
    Future = fun() ->
                     ets:insert(future, #future{id = self()}),
                     Result = Fun(),
                     ets:insert(future, #future{
                                           id = self(),
                                           state = success,
                                           result = Result
                                          }),
                     SuccessCallbacks = ets:lookup_element(
                                          future,
                                          self(),
                                          #future.success_callbacks),
                     lists:map(fun(C) ->
                                       C(Result)
                               end, SuccessCallbacks)
             end,
    spawn(Future).

%% Callbacks -------------------------------------------------------------------

onComplete(Future, Callback) ->
    case ets:lookup(future, Future) of
        [#future{state = success,
                 result = Result}] ->
            Do = fun() ->
                         Callback(Result)
                 end,
            spawn(Do);
        [#future{state = running,
                 success_callbacks = Cs} = F] ->
            ets:insert(future, F#future{success_callbacks = [Callback | Cs]})
    end.

onSuccess(Future, Fun) ->
    ok.

onFailure(Future, Fun) ->
    ok.

%% Combinators -----------------------------------------------------------------

map(Fun, List) ->
    ok.

flatMap(Fun, List) ->
    ok.

filter(Fun, List) ->
    ok.

foreach(Fun, List) ->
    ok.

%% Blocking operations ---------------------------------------------------------

await_result(Future) ->
    ok.

await_ready(Future) ->
    ok.

%% Promises --------------------------------------------------------------------

success(Future, Result) ->
    ok.

failure(Future, Result) ->
    ok.
