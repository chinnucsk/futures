-module(future_proc).

-behaviour(gen_server).

%% API functions
-export([start_link/1,
         onSuccess/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type future_status() :: running | success | failure.

-record(state, {
          status  = running   :: future_status(),
          result = undefined :: any(),
          complete_callbacks = [] :: list(fun()),
          success_callbacks  = [] :: list(fun()),
          failure_callbacks  = [] :: list(fun())
         }).

%% API functions ---------------------------------------------------------------
start_link(Future) ->
    gen_server:start_link(?MODULE, [Future], []).

onSuccess(Future, Callback) ->
    gen_server:cast(Future, {onSuccess, Callback}).

%% gen_server callbacks --------------------------------------------------------

init([Future]) ->
    gen_server:cast(self(), {run, Future}),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({run, Future}, State) ->
    ReturnPid = self(),
    Do = fun() ->
                 Result = Future(),
                 gen_server:cast(ReturnPid, {result, success, Result})
         end,
    spawn(Do),
    {noreply, State};
handle_cast({result, Status, Result}, State) ->
    {noreply, State#state{status = Status,
                          result = Result
                         }};
handle_cast({onSuccess, Callback}, #state{status = running,
                                          success_callbacks = Cs
                                         } = State) ->
    {noreply, State#state{success_callbacks = [Callback | Cs]}};
handle_cast({onSuccess, Callback}, #state{status = success,
                                          result = Result} = State) ->
    Do = fun() ->
                 Callback(Result)
         end,
    spawn(Do),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
