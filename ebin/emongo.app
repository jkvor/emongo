{application, emongo, [
	{description, "Erlang MongoDB Driver"},
	{vsn, "0.0.5"},
	{modules, [
		emongo, emongo_app, emongo_sup, emongo_bson, emongo_packet,
		emongo_server, emongo_server_sup, emongo_collection
	]},
	{registered, [emongo_sup, emongo]},
	{mod, {emongo_app, []}},
	{applications, [kernel, stdlib]},
	{env, [
		{pools, [
			% NOTE: PoolId will be a locally registered name & therefore
			% cannot conflict with other registered names
			{emongo_pool, [
				{size, 1},
				{host, "localhost"},
				{port, 27017},
				{database, "test"}
			]}
		]}
	]}
]}.