%%-------------------------------------------------------------------
%%% gnosus database utilities
%%%-------------------------------------------------------------------
-module(gnosus_dbi).
 
%% API
-export([
    create_table/2,
    delete_table/1,
    clear_table/1,
    write_row/1,
    delete_row/1,
    read_row/1,
    read_row/3,
    q/1,
    limit/2,
    fold/3,
    foreach/2,
    transaction/1,
    count/1,
    dirty_select/2
]).
 
%% include
-include_lib("stdlib/include/qlc.hrl").
 
%%====================================================================
create_table(Model, Options) ->
	mnesia:create_table(Model, Options).
 
%%--------------------------------------------------------------------
delete_table(Model) ->
	mnesia:delete_table(Model).
 
%%--------------------------------------------------------------------
clear_table(Model) ->
	mnesia:clear_table(Model).
 
%%--------------------------------------------------------------------
transaction(F) ->
	case mnesia:transaction(F) of
		{atomic, Val} ->
			Val;
		 _ ->
			aborted
     end.
                
%%--------------------------------------------------------------------
write_row(Row) ->
	transaction(
		fun() ->
			mnesia:write(Row)
		end).
 
%%--------------------------------------------------------------------
read_row(Row) ->
    transaction(
        fun() ->
            mnesia:read(Row)
        end).
 
%%--------------------------------------------------------------------
read_row(Table, Value, Index) ->
    transaction(
        fun() ->
            mnesia:index_read(Table, Value, Index)
        end).
 
%%--------------------------------------------------------------------
delete_row(Oid) ->
    transaction(
        fun() ->
            mnesia:delete(Oid)
        end).
 
%%--------------------------------------------------------------------
q(Q) ->
    transaction(
        fun() ->
             qlc:e(Q)
        end).
 
%%--------------------------------------------------------------------
limit(Q, C) ->
   	transaction(
        fun() ->
           Cursor = qlc:cursor(Q),
           Result = qlc:next_answers(Cursor, C),
           qlc:delete_cursor(Cursor),
           Result
        end).
 
%%--------------------------------------------------------------------
fold(F, I, Q) ->
    transaction(
        fun() ->
           qlc:fold(F, I, Q)
        end).
 
%%--------------------------------------------------------------------
foreach(F, Table) ->
    Result = transaction(
        fun() ->
            mnesia:first(Table)
        end),
    case Result of
        aborted ->
            aborted;
        FirstKey ->
            foreach(F, Table, FirstKey)
    end.
     
foreach(_, _, '$end_of_table') ->
    ok;
 
foreach(F, Table, ThisKey) ->
    case read_row({Table, ThisKey}) of
        aborted ->
            aborted;
        Row ->
            F(Row)
    end,
    Result = transaction(
        fun() ->
            mnesia:next(Table, ThisKey)
        end) ,
    case Result of
        aborted ->
            aborted;
        NextKey ->
            foreach(F, Table, NextKey)
    end.
 
%%--------------------------------------------------------------------
count(Table) ->
    webgnosus_dbi:fold(
        fun(_S, Sum) ->
            Sum + 1
        end,
        0,
        qlc:q([S || S <- mnesia:table(Table)])).
 

%%--------------------------------------------------------------------
dirty_select(Table, Select) -> 
    mnesia:dirty_select(Table, Select).
    
