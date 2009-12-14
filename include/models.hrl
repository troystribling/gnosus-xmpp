%% models
%%--------------------------------------------------------------------------------

%% users
-record(users, {
	email,
  	uid,
  	password,
  	registration_status,
  	registration_code,
  	role,
  	created_at,
  	updated_at,
  	last_login,
  	login_count,
  	failed_login_count,
  	product             = free,
  	services            = []
}).

%% domains
-record(domains, {
	  domain,
	  uid
}).

%% ejabberd password database
-record(passwd,  {
	  us, 
	  password
}).
