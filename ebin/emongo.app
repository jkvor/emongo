{application, emongo, [
	{description, "Erlang MongoDB Driver"},
	{vsn, "0.0.4"},
	{modules, [
		emongo, emongo_app, emongo_sup, emongo_bson, emongo_packet,
		emongo_server, emongo_pool, emongo_collection, emongo_balancer
	]},
	{registered, [emongo_sup, emongo]},
	{mod, {emongo_app, []}},
	{applications, [kernel, stdlib]}
]}.
