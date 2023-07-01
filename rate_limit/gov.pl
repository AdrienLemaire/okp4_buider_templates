% rate_limit
:- consult('cosmwasm:okp4-objectarium:okp41gwprhwmm0ktpn8z04ge60gc4nzjxnzgnjflesuyyjrsfmxtusqvqnnjg36?query=%7B%22object_data%22%3A%7B%22id%22%3A%2215479e8e8829ead870eb75f0747d8fd389860c6903fc2430fdb4fe23b57d6e9c%22%7D%7D').

% List of allowed users
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
data_limits('Role2', '50Gb', 'week').
data_limits('Role3', '250Gb', 'month').
