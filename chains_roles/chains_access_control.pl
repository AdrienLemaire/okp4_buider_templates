% This template provides a basic access control system based on Cosmos chains.


% get_list/2 retrieves the list of allowed roles or chains
get_list(roles, List) :-
    findall(Role, allow_role(_, Role), List).
get_list(chains, List) :-
    allow_chains(List).

% help/2 is a helper predicate to retrieve the list of allowed roles or chains.
help(Key, List) :-
    get_list(Key, List).

% authorized_did/3 checks if a given DID is part of the authorized chains.
authorized_did(DID, Addr, Chain) :-
    did_components(DID, did(Method, Addr, _, _, _)), 
    allow_did_method(Method), 
    bech32_address(-(Chain, _), Addr),
    get_list(chains, Chains),
    member(Chain, Chains).

% hour_range/2 checks if the current UTC hour is within a given range.
hour_range(From, To) :-
    block_time(Now),
    Hour is Now mod 86400 // 3600,
    Hour >= From,
    Hour < To.

% can/3 checks if a given DID is authorized to use a role or find the role.
can(use_role, DID, Role) :-
    authorized_did(DID, Address, Chain),
    allow_role(Chain, Role).
