% time_helper
:- consult('cosmwasm:okp4-objectarium:okp41gwprhwmm0ktpn8z04ge60gc4nzjxnzgnjflesuyyjrsfmxtusqvqnnjg36?query=%7B%22object_data%22%3A%7B%22id%22%3A%22c1d1d7cc1c7371723f3a0192367bc4fd0af4de6cef9cce23f7b4aa0e48d58aa1%22%7D%7D').
% string_helper
:- consult('cosmwasm:okp4-objectarium:okp41gwprhwmm0ktpn8z04ge60gc4nzjxnzgnjflesuyyjrsfmxtusqvqnnjg36?query=%7B%22object_data%22%3A%7B%22id%22%3A%22565d0d7720599b16096b5f2795f187dd197ab9afa76e1f0e9f86707ab9e88f07%22%7D%7D').
% data_helper
:- consult('cosmwasm:okp4-objectarium:okp41gwprhwmm0ktpn8z04ge60gc4nzjxnzgnjflesuyyjrsfmxtusqvqnnjg36?query=%7B%22object_data%22%3A%7B%22id%22%3A%22518b001e4301b69981dffea242f50135bcc9f8ee280f18b590720f5a88e7c2b9%22%7D%7D').

% floor is supported, but not ceil or round, so we have to implement them
ceil(X, Ceil) :-
    Floor is floor(X),
    ( X =:= Floor  % if X is an integer
    -> Ceil is X
    ;  Ceil is Floor + 1
    ).

round(X, Rounded) :-
    Floor is floor(X),
    ceil(X, Ceil),
    (   X - Floor < Ceil - X
    ->  Rounded = Floor
    ;   Rounded = Ceil
    ).

% the random library isn't available, not even random_float. Using block_time,
% this is the worst deterministic random number generator ever.
random_between(Low, High, Result) :-
    block_time(Timestamp),
    Range is High - Low + 1,
    Result is Low + (Timestamp mod Range).

random_request_count(Role, RequestCount, Seconds) :-
    request_limits(Role, Limit, TimeUnit),
    timeframe_to_seconds(TimeUnit, LimitSeconds),
    NormalizedLimit is Limit * Seconds / LimitSeconds,
    % try to get about 2/3 of results within rate limits
    ResultMax is round(NormalizedLimit * 1.5),
    random_between(0, ResultMax, RequestCount).

% returns strings like Data = '100Mb'
random_data_count(Role, Data, Seconds) :-
    data_limits(Role, LimitRecord, TimeUnit),
    data_record_to_bytes(LimitRecord, DataLimitBytes),
    convert_bytes(DataLimitBytes, DataUnit, DataLimit),
    timeframe_to_seconds(TimeUnit, LimitSeconds),
    NormalizedLimit is DataLimit * Seconds / LimitSeconds,
    % try to get about 2/3 of results within rate limits
    ResultMax is round(NormalizedLimit * 1.5),
    random_between(0, ResultMax, Value),
    atom_number(ValueString, Value),
    % Return a string with the data amount and unit
    atom_concat(ValueString, DataUnit, Data).
