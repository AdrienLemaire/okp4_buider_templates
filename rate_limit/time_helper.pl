% Helper predicate to convert timeframe to seconds
timeframes(['second', 'minute', 'hour', 'day', 'week', 'month', 'year']).
timeframe_to_seconds('minute', 60).
timeframe_to_seconds('hour', 3600).
timeframe_to_seconds('day', 86400).
timeframe_to_seconds('week', 604800).
timeframe_to_seconds('month', 2592000). % This assumes a 30-day month
timeframe_to_seconds('year', 31536000). % This assumes a 365-day year

% helper to find the timeframe unit above given seconds
timeframe_unit_above(Seconds, TimeUnit) :-
    timeframe_to_seconds(TimeUnit, TimeframeSeconds),
    Seconds =< TimeframeSeconds.

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



