:- consult('cosmwasm:okp4-objectarium:okp41gwprhwmm0ktpn8z04ge60gc4nzjxnzgnjflesuyyjrsfmxtusqvqnnjg36?query=%7B%22object_data%22%3A%7B%22id%22%3A%227056660a0b1980d6b77d2501e3107992532c5c27a1a1e20b52ccb5b9679969e3%22%7D%7D').

allow_did_method('key').
allow_denom('uknow').

% List of authorized stargaze accounts with client label
allow_star(stars1wy8ywt98sv5pmsg873rct4pxtw0ntxuwldehwe, corp_a).
allow_star(stars1g5jd3s3nhmxewu5kgqmauuc9en7v8hfnnrfst4, corp_b).

% List of authorized resources
allow_resource(database1, 1000000).
% Resources can have different access cost per client
allow_resource(database2, 500000, corp_a).
allow_resource(database2, 800000, corp_b).
