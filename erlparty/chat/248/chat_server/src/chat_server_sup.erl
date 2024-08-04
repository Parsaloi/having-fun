%%%-------------------------------------------------------------------
%% @doc chat_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
%init([]) ->
%    SupFlags = #{strategy => one_for_all,
%                 intensity => 0,
%                 period => 1},
%    ChildSpecs = [],
%    {ok, {SupFlags, ChildSpecs}}.

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    ChildSpecs = [#{id => chat_server,
                    start => {chat_server, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [chat_server]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
