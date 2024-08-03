-module(chat_server).
-behaviour(gen_server).

-export([start_link/0, create_channel/1, join_channel/2, leave_channel/2, send_message/3, get_messages/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {channels = #{}, subscriptions = #{}}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_channel(ChannelName) ->
    gen_server:call(?MODULE, {create_channel, ChannelName}).

join_channel(ClientPid, ChannelName) ->
    gen_server:call(?MODULE, {join_channel, ClientPid, ChannelName}).

leave_channel(ClientPid, ChannelName) ->
    gen_server:call(?MODULE, {leave_channel, ClientPid, ChannelName}).

send_message(ChannelName, From, Message) ->
    gen_server:cast(?MODULE, {send_message, ChannelName, From, Message}).

get_messages(ChannelName, Since) ->
    gen_server:call(?MODULE, {get_messages, ChannelName, Since}).

init([]) ->
    {ok, #state{}}.

handle_call({create_channel, ChannelName}, _From, State) ->
    case maps:is_key(ChannelName, State#state.channels) of
        true ->
            {reply, {error, channel_exists}, State};
        false ->
            NewChannels = maps:put(ChannelName, [], State#state.channels),
            {reply, ok, State#state{channels = NewChannels}}
    end;

handle_call({join_channel, ClientPid, ChannelName}, _From, State) ->
    case maps:is_key(ChannelName, State#state.channels) of
        true ->
            NewSubscriptions = maps:update_with(ClientPid, 
                fun(Channels) -> lists:usort([ChannelName | Channels]) end, 
                [ChannelName], 
                State#state.subscriptions),
            {reply, ok, State#state{subscriptions = NewSubscriptions}};
        false ->
            {reply, {error, channel_not_found}, State}
    end;

handle_call({leave_channel, ClientPid, ChannelName}, _From, State) ->
    case maps:find(ClientPid, State#state.subscriptions) of
        {ok, Channels} ->
            NewChannels = lists:delete(ChannelName, Channels),
            NewSubscriptions = maps:update(ClientPid, NewChannels, State#state.subscriptions),
            {reply, ok, State#state{subscriptions = NewSubscriptions}};
        error ->
            {reply, {error, not_subscribed}, State}
    end;

handle_call({get_messages, ChannelName, Since}, _From, State) ->
    case maps:find(ChannelName, State#state.channels) of
        {ok, Messages} ->
            FilteredMessages = lists:takewhile(fun({Timestamp, _, _}) -> Timestamp > Since end, Messages),
            {reply, {ok, FilteredMessages}, State};
        error ->
            {reply, {error, channel_not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({send_message, ChannelName, From, Message}, State) ->
    case maps:find(ChannelName, State#state.channels) of
        {ok, Messages} ->
            NewMessage = {erlang:system_time(millisecond), From, Message},
            UpdatedMessages = [NewMessage | Messages],
            NewChannels = maps:update(ChannelName, UpdatedMessages, State#state.channels),
            % Notify all subscribers
            maps:fold(
                fun(ClientPid, Channels, _) ->
                    case lists:member(ChannelName, Channels) of
                        true -> 
                            ClientPid ! {new_message, ChannelName, NewMessage};
                        false -> 
                            ok
                    end
                end, 
                ok, 
                State#state.subscriptions),
            {noreply, State#state{channels = NewChannels}};
        error ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
