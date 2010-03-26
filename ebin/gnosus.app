{application, gnosus, [
	{description,  "gnos.us"},
	{mod, {gnosus_app, []}},
	{env, [
		{platform, mochiweb}, %% {inets|yaws|mochiweb}
		{port, 8000},
		{session_timeout, 20},
		{sign_key, "gf1d0963tbae"},
		{www_root, "./wwwroot"}
	]}
]}.