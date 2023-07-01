% time_helper
:- consult('cosmwasm:okp4-objectarium:okp41gwprhwmm0ktpn8z04ge60gc4nzjxnzgnjflesuyyjrsfmxtusqvqnnjg36?query=%7B%22object_data%22%3A%7B%22id%22%3A%22c1d1d7cc1c7371723f3a0192367bc4fd0af4de6cef9cce23f7b4aa0e48d58aa1%22%7D%7D').
% string_helper
:- consult('cosmwasm:okp4-objectarium:okp41gwprhwmm0ktpn8z04ge60gc4nzjxnzgnjflesuyyjrsfmxtusqvqnnjg36?query=%7B%22object_data%22%3A%7B%22id%22%3A%22565d0d7720599b16096b5f2795f187dd197ab9afa76e1f0e9f86707ab9e88f07%22%7D%7D').
% data_helper
:- consult('cosmwasm:okp4-objectarium:okp41gwprhwmm0ktpn8z04ge60gc4nzjxnzgnjflesuyyjrsfmxtusqvqnnjg36?query=%7B%22object_data%22%3A%7B%22id%22%3A%22518b001e4301b69981dffea242f50135bcc9f8ee280f18b590720f5a88e7c2b9%22%7D%7D').
% random_helper
:- consult('cosmwasm:okp4-objectarium:okp41gwprhwmm0ktpn8z04ge60gc4nzjxnzgnjflesuyyjrsfmxtusqvqnnjg36?query=%7B%22object_data%22%3A%7B%22id%22%3A%225cc1a4e8fa875b930f11b5947d1d89210c1d17a72e3d65e9fda7a6ad9a2ec551%22%7D%7D').

% Helper predicate to get the user usage logs a given timeframe in seconds
user_consommed(User, Timeframe, RequestCount, Data) :-
    user_role(User, Role), % user exists
    timeframe_to_seconds('year', OneYear),
    Timeframe =< OneYear, % only accept timeframes of one year or less

    % TODO: Get the user usage logs from the cognitarium with SPARQL
    random_request_count(Role, RequestCount, Timeframe),
    random_data_count(Role, Data, Timeframe).


% Template for rate_limit predicate
rate_limit(User, requests, Timeframe, Limit) :-
    user_role(User, Role),
    request_limits(Role, Requests, ReqTimeframe),
    (   number(Timeframe)
    ->  Seconds = Timeframe
    ;   timeframe_to_seconds(Timeframe, Seconds)
    ),
    timeframe_to_seconds(ReqTimeframe, ReqSeconds),
    Limit is (Requests * Seconds) / ReqSeconds.

rate_limit(User, data, Timeframe, Limit) :-
     user_role(User, Role),
     data_limits(Role, DataRecord, DataTimeframe),
     data_record_to_bytes(DataRecord, DataLimit),
     timeframe_to_seconds(DataTimeframe, DataSeconds),
     timeframe_to_seconds(Timeframe, Seconds),
     Limit is (DataLimit * Seconds) / DataSeconds.

result_string(RequestCount, RequestLimit, DataCount, DataLimit, String) :-
    atom_number(RequestCountStr, RequestCount),
    atom_number(RequestLimitStr, RequestLimit),
    atom_number(DataCountStr, DataCount),
    atom_number(DataLimitStr, DataLimit),
    atom_concat(RequestCountStr, ' requests / ', Temp1),
    atom_concat(Temp1, RequestLimitStr, Temp2),
    atom_concat(Temp2, '; ', Temp3),
    atom_concat(Temp3, DataCountStr, Temp4),
    atom_concat(Temp4, ' bytes / ', Temp5),
    atom_concat(Temp5, DataLimitStr, String).

 % Template for can_query predicate
check_rate_limits(User, RequestCount, RequestLimit, DataCount, DataLimit) :-
    % get the user role
    user_role(User, Role),

    % what are the request limits for this role?
    request_limits(Role, RequestLimit, RequestTimeframe),
    timeframe_to_seconds(RequestTimeframe, T1),

    % what are the data limits for this role?
    data_limits(Role, LimitRecord, DataTimeframe),
    data_record_to_bytes(LimitRecord, DataLimit),
    timeframe_to_seconds(DataTimeframe, T2),

    % Get the current usage for this user for each timeframe
    user_consommed(User, T1, RequestCount, _),
    user_consommed(User, T2, _, LogRecord),
    data_record_to_bytes(LogRecord, DataCount).

get_usage(User, Log) :-
    check_rate_limits(User, RequestCount, RequestLimit, DataCount, DataLimit),
    % Return a string with the details
    result_string(RequestCount, RequestLimit, DataCount, DataLimit, Log).

can_query(User) :-
    check_rate_limits(User, RequestCount, RequestLimit, DataCount, DataLimit),
    % Verify limits
    RequestCount =< RequestLimit,
    DataCount =< DataLimit.
