{application, emongo, [
	{description, "Erlang MongoDB Driver"},
	{vsn, "0.0.2"},
	{modules, [
		emongo, emongo_app, emongo_sup, emongo_bson, emongo_packet, emongo_server
	]},
	{registered, [emongo_sup, emongo]},
	{mod, {emongo_app, []}},
	{applications, [kernel, stdlib]},
	{env, [
		{pools, [
			{emongo, [
				{size, 1},
				{host, "localhost"},
				{port, 27017},
				{database, "test"}
			]}
		]}
	]}
]}.