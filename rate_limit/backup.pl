% User role and limits (requests per timeframe, data per timeframe)
user_role('User1', 'Role1').
user_role('User2', 'Role2').
user_role('User3', 'Role1').
user_role('User4', 'Role3').
user_role('User5', 'Role2').
user_role('User6', 'Role3').
user_role('User7', 'Role1').
user_role('User8', 'Role2').
user_role('User9', 'Role3').
user_role('User10', 'Role1').
user_role('User11', 'Role2').
user_role('User12', 'Role3').
user_role('User13', 'Role1').
user_role('User14', 'Role2').

% Request limits per role (role, requests, timeframe)
request_limits('Role1', 1000, 'minute').
request_limits('Role2', 20000, 'hour').
request_limits('Role3', 500000, 'day').

% Data limits per role (role, amount, unit, timeframe)
data_limits('Role1', '500Mb', 'day').
data_limits('Role2', '5Gb', 'week').
data_limits('Role3', '10Gb', 'month').

% Helper predicate to convert timeframe to seconds
timeframes(['second', 'minute', 'hour', 'day', 'week', 'month', 'year']).
timeframe_to_seconds('minute', 60).
timeframe_to_seconds('hour', 3600).
timeframe_to_seconds('day', 86400). timeframe_to_seconds('week', 604800).
timeframe_to_seconds('month', 2592000). % This assumes a 30-day month
timeframe_to_seconds('year', 31536000). % This assumes a 365-day year

% Helper predicate to convert digital information units to bytes
data_units(['TB', 'GB', 'MB', 'KB', 'B']). % Order matters
data_unit_to_bytes('B', 1).
data_unit_to_bytes('KB', 1024).
data_unit_to_bytes('MB', 1048576).
data_unit_to_bytes('GB', 1073741824).
data_unit_to_bytes('TB', 1099511627776).

% helper to find the timeframe unit above given seconds
timeframe_unit_above(Seconds, TimeUnit) :-
    timeframe_to_seconds(TimeUnit, TimeframeSeconds),
    Seconds =< TimeframeSeconds.

% helper to find the data unit above given bytes
data_unit_above(Bytes, DataUnit) :-
    data_unit_to_bytes(DataUnit, DataBytes),
    Bytes =< DataBytes.

data_record_to_bytes(DataRecord, DataBytes) :-
    split_data_unit(DataRecord, DataCount, DataUnit),
    upcase_atom(DataUnit, DataUnitUpper),
    data_unit_to_bytes(DataUnitUpper, UnitBytes),
    DataBytes is DataCount * UnitBytes.

% Convert bytes to an appropriate unit
convert_bytes(Bytes, Unit, Result) :-
    data_unit_to_bytes(Unit, UnitSize),
    NextUnitSize is UnitSize * 1024,
    (Bytes < NextUnitSize -> 
        Result is Bytes / UnitSize ; 
        next_unit(Unit, NextUnit),
        convert_bytes(Bytes, NextUnit, Result)
    ).

% Find the next unit in the list of units
next_unit(Unit, NextUnit) :-
    data_units(Units),
    append(_, [Unit, NextUnit|_], Units).

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

% atom_number isn't supported by ichiban/prolog, so we have to implement it
atom_number(Atom, Number) :- % string to number
    atom(Atom),
    atom_chars(Atom, Chars),
    number_chars(Number, Chars).
atom_number(Atom, Number) :- % number to string
    number(Number),
    number_chars(Number, Chars),
    atom_chars(Atom, Chars).

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

upcase_char(Char, UpperChar) :-
    char_code(Char, Code),
    Code >= 97, Code =< 122, !,  % ASCII codes for 'a' to 'z'
    UpperCode is Code - 32,     % ASCII code for corresponding uppercase letter
    char_code(UpperChar, UpperCode).
upcase_char(Char, Char).  % If not a lowercase letter, return the character as is

% Base case: An empty list of characters is an empty list of characters.
upcase_chars([], []).
% Recursive case: Convert the first character to uppercase and recurse on the rest of the list.
upcase_chars([Char | Chars], [UpperChar | UpperChars]) :-
    upcase_char(Char, UpperChar),
    upcase_chars(Chars, UpperChars).

% upcase_atom isn't supported by ichiban/prolog, so we have to implement it ourselves
% Now use upcase_chars in upcase_atom:
upcase_atom(Atom, UpperAtom) :-
    atom_chars(Atom, Chars),
    upcase_chars(Chars, UpperChars),
    atom_chars(UpperAtom, UpperChars).

% split data record into count and unit
split_data_unit(String, Count, Unit) :-
    data_units(Units),
    upcase_atom(String, StringUpper), % Convert to uppercase to ensure match
    member(Unit, Units),
    atom_length(Unit, UnitLength),
    atom_length(StringUpper, StringLength),
    CountLength is StringLength - UnitLength,
    sub_atom(StringUpper, 0, CountLength, _, CountStr),
    atom_codes(CountStr, CountCodes),
    number_codes(Count, CountCodes),
    sub_atom(StringUpper, _, UnitLength, 0, UnitStr),
    UnitStr = Unit.

% time_difference(Time1, Time2, Difference)
% Difference is the number of seconds between Time1 and Time2
time_difference(time(Y1, M1, D1, H1, Mi1, S1), time(Y2, M2, D2, H2, Mi2, S2), Difference) :-
    convert_to_seconds(Y1, M1, D1, H1, Mi1, S1, Time1Seconds),
    convert_to_seconds(Y2, M2, D2, H2, Mi2, S2, Time2Seconds),
    Difference is abs(Time1Seconds - Time2Seconds).

% convert_to_seconds(Year, Month, Day, Hour, Minute, Second, TotalSeconds)
% TotalSeconds is the total number of seconds in the given date and time
convert_to_seconds(Y, M, D, H, Mi, S, TotalSeconds) :-
    TotalSeconds is ((((Y * 365 + M * 30 + D) * 24 + H) * 60 + Mi) * 60) + S.


result_string(RequestCount, RequestLimit, DataCount, DataLimit, String) :-
    atom_number(RequestCountStr, RequestCount),
    atom_number(RequestLimitStr, RequestLimit),
    atom_number(DataCountStr, DataCount),
    atom_number(DataLimitStr, DataLimit),
    atom_concat(RequestCountStr, ' / ', Temp1),
    atom_concat(Temp1, RequestLimitStr, Temp2),
    atom_concat(Temp2, '; ', Temp3),
    atom_concat(Temp3, DataCountStr, Temp4),
    atom_concat(Temp4, ' / ', Temp5),
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


