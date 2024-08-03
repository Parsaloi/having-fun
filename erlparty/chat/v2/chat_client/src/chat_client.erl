-module(chat_client).
-behaviour(gen_server).

-export([start_link/1, connect_to_channel/2, disconnect_from_channel/2, send_message/3, get_messages/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {server_node, channels = #{}, last_read = #{}}).

start_link(ServerNode) ->
    gen_server:start_link(?MODULE, [ServerNode], []).

connect_to_channel(Pid, ChannelName) ->
    gen_server:call(Pid, {connect_to_channel, ChannelName}).

disconnect_from_channel(Pid, ChannelName) ->
    gen_server:call(Pid, {disconnect_from_channel, ChannelName}).

send_message(Pid, ChannelName, Message) ->
    gen_server:cast(Pid, {send_message, ChannelName, Message}).

get_messages(Pid, ChannelName) ->
    gen_server:call(Pid, {get_messages, ChannelName}).

init([ServerNode]) ->
    {ok, #state{server_node = ServerNode}}.

handle_call({connect_to_channel, ChannelName}, _From, State) ->
    case rpc:call(State#state.server_node, chat_server, join_channel, [self(), ChannelName]) of
        ok ->
            NewChannels = maps:put(ChannelName, [], State#state.channels),
            NewLastRead = maps:put(ChannelName, 0, State#state.last_read),
            {reply, ok, State#state{channels = NewChannels, last_read = NewLastRead}};
        Error ->
            {reply, Error, State}
    end;

handle_call({disconnect_from_channel, ChannelName}, _From, State) ->
    case rpc:call(State#state.server_node, chat_server, leave_channel, [self(), ChannelName]) of
        ok ->
            NewChannels = maps:remove(ChannelName, State#state.channels),
            NewLastRead = maps:remove(ChannelName, State#state.last_read),
            {reply, ok, State#state{channels = NewChannels, last_read = NewLastRead}};
        Error ->
            {reply, Error, State}
    end;

handle_call({get_messages, ChannelName}, _From, State) ->
    case maps:find(ChannelName, State#state.channels) of
        {ok, _} ->
            LastRead = maps:get(ChannelName, State#state.last_read, 0),
            case rpc:call(State#state.server_node, chat_server, get_messages, [ChannelName, LastRead]) of
                {ok, Messages} ->
                    NewLastRead = case Messages of
                        [] -> LastRead;
                        [{Timestamp, _, _} | _] -> Timestamp
                    end,
                    NewLastReadMap = maps:put(ChannelName, NewLastRead, State#state.last_read),
                    {reply, {ok, Messages}, State#state{last_read = NewLastReadMap}};
                Error ->
                    {reply, Error, State}
            end;
        error ->
            {reply, {error, not_subscribed}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({send_message, ChannelName, Message}, State) ->
    case maps:is_key(ChannelName, State#state.channels) of
        true ->
            rpc:cast(State#state.server_node, chat_server, send_message, [ChannelName, self(), Message]),
            {noreply, State};
        false ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({new_message, ChannelName, {Timestamp, From, Message}}, State) ->
    case maps:find(ChannelName, State#state.channels) of
        {ok, Messages} ->
            NewMessages = [{Timestamp, From, Message} | Messages],
            NewChannels = maps:update(ChannelName, NewMessages, State#state.channels),
            io:format("New message in ~p from ~p: ~p~n", [ChannelName, From, Message]),
            {noreply, State#state{channels = NewChannels}};
        error ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
