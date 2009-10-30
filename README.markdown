#### The goal of emongo is to be stable, fast and easy to use.

## Compile and install

	make
	sudo make install
	
## Start emongo

	application:start(emongo).
	
## Connecting to mongodb

#### Option 1 - Config file

example.config:
	
	[{emongo, [
		{pools, [
			{pool1, [
				{size, 1},
				{host, "localhost"},
				{port, 27017},
				{database, "testdatabase"}
			]}
		]}
	]}].
	
specify the config file path when starting Erlang

	erl -config priv/example

start the application

	application:start(emongo).
	
#### Option 2 - Add pool

start the app and then add as many pools as you like

	application:start(emongo).
	emongo:add_pool(pool1, "localhost", 27017, "testdatabase", 1).
	
## API

#### Types

PoolName = atom()

Host = string()

Port = integer()

Database = string()

PoolSize = integer()

CollectionName = string()

Selector = Document

Document = [{Key, Val}]

Key = string() | atom() | binary() | integer()

Val = float() | string() | binary() | Document | {array, [term()]} | {binary, BinSubType, binary()} | {oid, binary()} | {oid, string()} | bool() | now() | datetime() | undefined | {regexp, string(), string()} | integer()

BinSubType = integer() <http://www.mongodb.org/display/DOCS/BSON#BSON-noteondatabinary>

#### Add Pool

	emongo:add_pool(PoolName, Host, Port, Database, PoolSize) -> ok

#### Find
	
Options = {timeout, Timeout} | {limit, Limit} | {offset, Offset} | 
		  {orderby, Orderby} | {fields, Fields} | response_options
Timeout = integer (timeout in milliseconds)
Limit = integer
Offset = integer
Orderby = [{Key, Direction}]
Direction = 1 (Asc) | -1 (Desc)
Fields = [Key] = specifies a list of fields to return in the result set
response_options = return #response{header, response_flag, cursor_id, offset, limit, documents}
Result = [Document] | response()
	
	emongo:find(PoolName, CollectionName) -> Result
	emongo:find(PoolName, CollectionName, Selector) -> Result
	emongo:find(PoolName, CollectionName, Selector, Options) -> Result