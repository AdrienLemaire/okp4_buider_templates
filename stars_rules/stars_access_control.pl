% This template provides access to any cosmos DID with an authorized stargaze address.

% get_address/2 strips the prefixes from a DID and returns the address.
get_address(DID, Addr) :-
    did_components(DID, did(Method, Addr, _, _, _)),
    allow_did_method(Method).

% convert_address_to/3 converts a DID to another equivalent address in a different chain.
convert_address_to(FromAddr, Prefix, ToAddr) :-
    bech32_address(-(_, Words), FromAddr),
    bech32_address(-(Prefix, Words), ToAddr).

% authorized_star/2 checks if a DID is associated to an authorized stargaze address.
authorized_star(DID, Client) :-
    get_address(DID, Addr),
    convert_address_to(Addr, stars, StargazeAddr),
    allow_star(StargazeAddr, Client).

% has_funds/3 checks if a DID has enough funds to pay for the access.
has_funds(DID, MinAmount) :-
    get_address(DID, Addr),
    convert_address_to(Addr, okp4, Okp4Addr),
    allow_denom(Denom),
    bank_spendable_balances(Okp4Addr, [Denom-Amount]),
    Amount @>= MinAmount.

% can/3 checks if a DID from any cosmos chain can access a resource.
can(access_resource, DID, Resource) :-
    % Is DID authorized to access the resource?
    authorized_star(DID, Client),
    % Is the resource available, at what price?
    (allow_resource(Resource, MinAmount, Client);
        allow_resource(Resource, MinAmount)),
    % Does DID have enough funds to pay for the access?
    has_funds(DID, MinAmount).
