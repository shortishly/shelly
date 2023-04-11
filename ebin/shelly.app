{application, 'shelly', [
	{description, "secure shell"},
	{vsn, "rolling"},
	{modules, ['shelly','shelly_app','shelly_config','shelly_key_store','shelly_ssh_daemon','shelly_statem','shelly_sup']},
	{registered, [shelly_sup]},
	{applications, [kernel,stdlib,crypto,sasl,ssh,envy]},
	{mod, {shelly_app, []}},
	{env, []}
]}.