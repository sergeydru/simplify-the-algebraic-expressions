% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution


-module(solution).
-export([main/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main() ->
    Data = read_data(),
    Res = calculate(Data),
    output_data(Res),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -record(node, 
%     {
%         left = none, 
%         op = , 
%         right = noe
%     }).

calculate(Ts) ->
    %%io:format("insert ~p\n",[Ts]),
    Parsed = lists:map(fun(T) -> parse_string(prepare_parsing(T, [])) end, Ts),
    %%io:format("Parsed------ ~p\n",[Parsed]),
    lists:map(fun simplify/1, Parsed).


prepare_parsing([N,D|T], Acc) when (( ((D >= $a) and (D =< $z)) or (D == $( )) and ((N =< $9) and (N >= $0))) ->
    prepare_parsing([D| T], [$*,N | Acc]);
prepare_parsing([D,$^,N|T], Acc) when D >= $a, D =< $z  ->
    prepare_parsing(T, [$},N,$,,D,${ | Acc]);
prepare_parsing([D, $(|T], Acc) when D >= $a, D =< $z  ->
    prepare_parsing(T, [$(,$*,$},$1,$,,D,${ | Acc]);
prepare_parsing([D|T], Acc) when D >= $a, D =< $z ->
    prepare_parsing(T, [$},$1,$,,D,${ | Acc]);
prepare_parsing([N|T], Acc) ->
    prepare_parsing(T, [N | Acc]);
prepare_parsing([], Acc) ->
    lists:reverse([$.|Acc]).
parse_string(String) ->
    {ok, Ts, _} = erl_scan:string(String),
    {ok, [ListAST]} = erl_parse:parse_exprs(Ts),
    ListAST.


simplify({integer,_,A}) ->
    {var, A, 0, "num"};
simplify({op, _,'-',{integer,_,A}}) ->
    {var,-1*A, 0, "num"};
simplify({op,_,'*', {integer,_,A}, {tuple,_,[{_, _, Z},{integer,_,N}]}}) ->
    % A*X^N
    {var, A, N, Z};
simplify({tuple,_,[{_, _, Z},{integer,_,N}]}) ->
    % A*X^N
    {var, 1, N, Z};
simplify({op,_,'+', T1, T2}) ->
    ST1 = simplify(T1),
    ST2 = simplify(T2),
    add(ST1, ST2);
simplify({op,_,'-', T1, T2}) ->
    ST1 = simplify(T1),
    ST2 = simplify(T2),
    sub(ST1, ST2);
simplify({op,_,'*', T1, T2}) ->
    ST1 = simplify(T1),
    ST2 = simplify(T2),
    mul(ST1, ST2);
simplify({op,_,'/', T1, T2}) ->
    ST1 = simplify(T1),
    ST2 = simplify(T2),
    coc(ST1, ST2);
simplify({op,_,'-', T1}) ->
    ST1 = simplify(T1),
    {_, _, _, Z} = ST1,
    mul({var, -1, 0, Z}, ST1).

add(V1 = {var, A, N, Z}, V2 = {var, B, M, Z}) ->    
     if                
         N == M -> 
             {var, A + B, N, Z};
         N < M ->
             {op, 1, '+', V2, V1};
         N > M ->
             {op, 1, '+', V1, V2}
     end;    
add(V1 = {var, _, N, _}, V2 = {var, _, M, _}) ->    
     if                
         N < M ->
             {op, 1, '+', V2, V1};
         true->
             {op, 1, '+', V1, V2}
                   
     end;
add(V = {var, _, N,Z}, Op = {op, _ , '+', V1 = {var, _, M1, Z}, V2}) -> 
    if
        N > M1 ->
            {op, 1, '+', V, Op};
        N == M1->
            {op, 1, '+', add(V, V1), V2};
        N < M1 ->
            {op, 1, '+', V1, add(V, V2)}
    end;
add(V = {var, _, N, _}, Op = {op, _ , '+', V1 = {var, _, M1, _}, V2}) -> 
    if
        N > M1 ->
            {op, 1, '+', V, Op};
        true ->
            {op, 1, '+', V1, add(V, V2)}
    end;
add({op, _ , '+', V1, V2}, Op = {op, _ , '+', _, _}) -> 
    add(V2, add(V1, Op));
add(O1, O2) ->
    add(O2, O1).

sub(V1, V2) ->
    {_, _, _, Z} = V2,
    add(V1, mul({var, -1, 0, Z}, V2)).

mul({var, A, N, Z}, {var, B, M, Z}) ->
    {var, A * B, N + M, Z};
mul({var, A, N, "num"}, {var, B, M, Z}) ->
    {var, A * B, N + M, Z};
mul({var, A, N, Z}, {var, B, M, "num"}) ->
    {var, A * B, N + M, Z};

mul(V = {var, _, _, _}, {op, _, '+', V1, V2}) -> 
    add(mul(V, V1), mul(V, V2));
mul({op, _, '+', V1, V2}, Op = {op, _ , '+', _, _}) -> 
    % io:format("MUL2: ~p\n", [{{op, 1, '+', V1, V2}, Op}]),
    % io:format("MUL2: ~p\n", [{mul(V1, Op), mul(V2, Op)}]),
    add(mul(V1, Op), mul(V2, Op));
mul(O1, O2) ->
    % io:format("MUL: ~p\n", [{O1, O2}]),
    mul(O2, O1).

coc({var, A, N, Z}, {var, B, M, Z}) ->
    {var, A div B, N - M, Z};
coc({op, _ , '+', V1, V2}, V = {var, _, _, _}) -> 
    add(coc(V1, V), coc(V2, V)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_data(Ds) -> 
    lists:map(
        % fun(D) -> io:format("~s\n", [to_str(D)]) end, 
        fun(D) -> io:format("~s\n", [to_str(D)]) end, 
        Ds).

to_str(E) -> 
   case to_str_aux(E) of 
        [$ , $+, $  | T] ->
            T;
        [$ , $-, $  | T] ->
            [$- | T]
    end.

to_str_aux({op, _, '+', Op1, Op2}) ->
    to_str_aux(Op1) ++ to_str_aux(Op2);
to_str_aux({var, A, N, Z}) ->
    AStr = 
        if 
            ((A == 1) and (N /= 0) )->
                [$ ,$+, $ ];
            ((A == -1) and (N /= 0) )->
                [$ ,$-, $ ];
            ((A == 0) and (N /= 0) ) ->
                [];
            A < 0 ->
                [$ ,$-, $  | integer_to_list(abs(A))];
            A >= 0 ->
                [$ ,$+, $  | integer_to_list(A)]
        end,
    NStr = 
        if 
            ((N == 0) or (A == 0)) ->
                "";
            N == 1 ->
		io_lib:format( "~s" , [Z] );
            N > 1 ->
                [$x, $^ | integer_to_list(N)]
        end,
    AStr ++ NStr.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_data() ->
    Binary = read(),
    Res = binary:split(Binary, [<<"\n">>], [global]),
    [_| Tests] = [binary_to_list(R) || R <- Res, R =/= <<>>],
    Tests.

-define(BLK_SIZE, 16384).

read() ->
    ok = io:setopts(standard_io, [binary]),
    read(<<>>).

read(Acc) ->
    case file:read(standard_io, ?BLK_SIZE) of
        {ok, Data} ->
            read(<<Acc/bytes, Data/bytes>>);
        eof ->
            Acc
    end.

ed(T) ->    
    erlang:display(T).
