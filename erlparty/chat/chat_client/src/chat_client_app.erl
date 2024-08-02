%%%-------------------------------------------------------------------
%% @doc chat_client public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_client_app).

-behaviour(gen_server).

-behaviour(application).

-export([start/2, stop/1]).

-export([start_link/1, connect_to_channel/2, send_message/2, get_messages/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {server_node, channel, last_read = 0}).

start(_StartType, _StartArgs) ->
    chat_client_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

start_link(ServerNode) ->
    gen_server:start_link(?MODULE, [ServerNode], []).

connect_to_channel(Pid, ChannelName) ->
    gen_server:call(Pid, {connect_to_channel, ChannelName}).

send_message(Pid, Message) ->
    gen_server:cast(Pid, {send_message, Message}).

get_messages(Pid) ->
    gen_server:call(Pid, get_messages).

init([ServerNode]) ->
    {ok, #state{server_node = ServerNode}}.

handle_call({connect_to_channel, ChannelName}, _From, State) ->
    case rpc:call(State#state.server_node, chat_server, create_channel, [ChannelName]) of
        ok ->
            {reply, ok, State#state{channel = ChannelName}};
        {error, channel_exists} ->
            {reply, ok, State#state{channel = ChannelName}};
        Error ->
            {reply, Error, State}
    end;

handle_call(get_messages, _From, State) ->
    case rpc:call(State#state.server_node, chat_server, get_messages, [State#state.channel, State#state.last_read]) of
        {ok, Messages} ->
            LastRead = case Messages of
                [] -> State#state.last_read;
                [{Timestamp, _, _} | _] -> Timestamp
            end,
            {reply, {ok, Messages}, State#state{last_read = LastRead}};
        Error ->
            {reply, Error, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send_message, Message}, State) ->
    rpc:cast(State#state.server_node, chat_server, send_message, [State#state.channel, self(), Message]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
