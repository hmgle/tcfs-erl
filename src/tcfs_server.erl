%% Using the gen_listener_tcp behaviour.

-module(tcfs_server).
-behaviour(gen_listener_tcp).

-define(TCP_PORT, 9876).
-define(TCP_OPTS, [binary, inet,
                   {active,    false},
                   {backlog,   10},
                   {nodelay,   true},
                   {packet,    2},
                   {reuseaddr, true}]).

%% External API
-export([start/1]).

%% gen_listener_tcp callbacks
-export([init/1,
         handle_accept/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% @doc Start the server.
start(RootPath) ->
    gen_listener_tcp:start({local, ?MODULE}, ?MODULE, RootPath, []).

%% @doc The tcfs client process.
tcfs_client(Socket, RootPath) ->
    error_logger:info_msg("client()~n"),
    ok = inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            error_logger:info_msg("Got Data: ~p", [Data]),
            gen_tcp:send(Socket, "TODO"),
            tcfs_client(Socket, RootPath);
        {tcp_closed, Socket} ->
            error_logger:info_msg("Client Disconnected.")
    end.

init(RootPath) ->
    {ok, {?TCP_PORT, ?TCP_OPTS}, RootPath}.

handle_accept(Sock, State) ->
    Pid = spawn(fun() -> tcfs_client(Sock, State) end),
    gen_tcp:controlling_process(Sock, Pid),
    {noreply, State}.

handle_call(Request, _From, State) ->
    {reply, {illegal_request, Request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
