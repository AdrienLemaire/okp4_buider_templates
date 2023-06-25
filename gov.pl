:- consult('cosmwasm:okp4-objectarium:okp41gwprhwmm0ktpn8z04ge60gc4nzjxnzgnjflesuyyjrsfmxtusqvqnnjg36?query=%7B%22object_data%22%3A%7B%22id%22%3A%22d635757b8a0631a4230f5c2ef2053789a67121f20d7330d94bb2080d5d665e61%22%7D%7D').

allow_did_method('key').

% Authorize a number of cosmos chains
allow_chains([okp4, cosmos, osmo]).

% Assign roles to DIDs from the above chains (default: guest)
% note: order matters
allow_role(osmo, member).
allow_role(okp4, admin) :- hour_range(7, 12), !.
allow_role(okp4, member).
allow_role(_, guest).
