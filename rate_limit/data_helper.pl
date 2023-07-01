% string_helper
:- consult('cosmwasm:okp4-objectarium:okp41gwprhwmm0ktpn8z04ge60gc4nzjxnzgnjflesuyyjrsfmxtusqvqnnjg36?query=%7B%22object_data%22%3A%7B%22id%22%3A%22565d0d7720599b16096b5f2795f187dd197ab9afa76e1f0e9f86707ab9e88f07%22%7D%7D').

% Helper predicate to convert digital information units to bytes
data_units(['TB', 'GB', 'MB', 'KB', 'B']). % Order matters
data_unit_to_bytes('B', 1).
data_unit_to_bytes('KB', 1024).
data_unit_to_bytes('MB', 1048576).
data_unit_to_bytes('GB', 1073741824).
data_unit_to_bytes('TB', 1099511627776).

% helper to find the data unit above given bytes
data_unit_above(Bytes, DataUnit) :-
    data_unit_to_bytes(DataUnit, DataBytes),
    Bytes =< DataBytes.

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

data_record_to_bytes(DataRecord, DataBytes) :-
    split_data_unit(DataRecord, DataCount, DataUnit),
    upcase_atom(DataUnit, DataUnitUpper),
    data_unit_to_bytes(DataUnitUpper, UnitBytes),
    DataBytes is DataCount * UnitBytes.

% Find the next unit in the list of units
next_unit(Unit, NextUnit) :-
    data_units(Units),
    append(_, [Unit, NextUnit|_], Units).

% Convert bytes to an appropriate unit
convert_bytes(Bytes, Unit, Result) :-
    data_unit_to_bytes(Unit, UnitSize),
    NextUnitSize is UnitSize * 1024,
    (Bytes < NextUnitSize -> 
        Result is Bytes / UnitSize ; 
        next_unit(Unit, NextUnit),
        convert_bytes(Bytes, NextUnit, Result)
    ).
