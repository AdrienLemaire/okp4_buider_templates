% atom_number isn't supported by ichiban/prolog, so we have to implement it
atom_number(Atom, Number) :- % string to number
    atom(Atom),
    atom_chars(Atom, Chars),
    number_chars(Number, Chars).
atom_number(Atom, Number) :- % number to string
    number(Number),
    number_chars(Number, Chars),
    atom_chars(Atom, Chars).

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
