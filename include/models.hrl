%% models
%%--------------------------------------------------------------------------------

%% users
-record(users, {
	email,
  	uid,
  	password,
  	status,
  	registration_code,
  	role,
  	created_at,
  	updated_at,
  	last_login,
  	login_count,
  	failed_login_count,
  	demerits,
  	product             = free,
  	services            = []
}).

%% client_users
-record(client_users, {
    jid,
	email,
  	password,
  	status,
  	registration_code,
  	created_at,
  	updated_at,
  	last_login,
  	login_count,
  	failed_login_count
}).

%% domains
-record(hosts, {
    host,
    uid
}).

%% ejabberd password database
-record(passwd,  {
    us, 
	password
}).

%% gnosus configuration
-record(gnosus,  {
    key, 
    value
}).
