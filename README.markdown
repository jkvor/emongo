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
	
## API Type Reference

__PoolName__ = atom()  
__Host__ = string()  
__Port__ = integer()  
__Database__ = string()  
__PoolSize__ = integer()  
__CollectionName__ = string()  
__Selector__ = Document  
__Document__ = [{Key, Val}]  
__Key__ = string() | atom() | binary() | integer()  
__Val__ = float() | string() | binary() | Document | {array, [term()]} | {binary, BinSubType, binary()} | {oid, binary()} | {oid, string()} | bool() | now() | datetime() | undefined | {regexp, string(), string()} | integer()  
__BinSubType__ = integer() <http://www.mongodb.org/display/DOCS/BSON#BSON-noteondatabinary>  

## Add Pool

	emongo:add_pool(PoolName, Host, Port, Database, PoolSize) -> ok

## Insert

__PoolName__ = atom()  
__CollectionName__ = string()  
__Document__ = [{Key, Val}]  
__Documents__ = [Document]  

	emongo:insert(PoolName, CollectionName, Document) -> ok
	emongo:insert(PoolName, CollectionName, Documents) -> ok
	
### Examples

	%% insert a single document with two fields into the "collection" collection
	emongo:insert(test, "collection", [{"field1", "value1"}, {"field2", "value2"}]).
	
	%% insert two documents, each with a single field into the "collection" collection
	emongo:insert(test, "collection", [[{"document1_field1", "value1"}], [{"document2_field1", "value1"}]]).	

## Update

__PoolName__ = atom()  
__CollectionName__ = string()  
__Selector__ = Document  
__Document__ = [{Key, Val}]  
__Upsert__ = true | false (insert a new document if the selector does not match an existing document)

	%% by default upsert == false
	emongo:update(PoolName, CollectionName, Selector, Document) -> ok
	emongo:update(PoolName, CollectionName, Selector, Document, Upsert) -> ok
	
### Examples

	%% update the document that matches "field1" == "value1"
	emongo:update(test, "collection", [{"field1", "value1"}], [{"field1", "value1"}, {"field2", "value2"}]).

## Delete

__PoolName__ = atom()  
__CollectionName__ = string()  
__Selector__ = Document  

	%% delete all documents in a collection
	emongo:delete(PoolName, CollectionName) -> ok
	
	%% delete all documents in a collection that match a selector
	emongo:delete(PoolName, CollectionName, Selector) -> ok
	
## Find
	
__Options__ = {timeout, Timeout} | {limit, Limit} | {offset, Offset} | {orderby, Orderby} | {fields, Fields} | response_options  
__Timeout__ = integer (timeout in milliseconds)  
__Limit__ = integer  
__Offset__ = integer  
__Orderby__ = [{Key, Direction}]  
__Direction__ = 1 (Asc) | -1 (Desc)  
__Fields__ = [Key] = specifies a list of fields to return in the result set  
__response_options__ = return #response{header, response_flag, cursor_id, offset, limit, documents}  
__Result__ = [Document] | response()  
	
	emongo:find(PoolName, CollectionName) -> Result
	emongo:find(PoolName, CollectionName, Selector) -> Result
	emongo:find(PoolName, CollectionName, Selector, Options) -> Result
	
### Examples

__limit, offset, timeout, orderby, fields__

	%% find documents from 'collection' where field1 equals 1 and abort the query if it takes more than 5 seconds
	%% limit the number of results to 100 and offset the first document 10 documents from the beginning
	%% return documents in ascending order, sorted by the value of field1
	%% limit the fields in the return documents to field1 (the _id field is always included in the results)
	emongo:find(test, "collection", [{"field1", 1}], [{limit, 100}, {offset, 10}, {timeout, 5000}, {orderby, [{"field1", asc}]}, {fields, ["field1"]}]).
	
__great than, less than, great than or equal, less than or equal__

	%% find documents where field1 is greater than 5 and less than 10
	emongo:find(test, "collection", [{"field1", [{gt, 5}, {lt, 10}]}]).
	  
	%% find documents where field1 is greater than or equal to 5 and less than or equal to 10
	emongo:find(test, "collection", [{"field1", [{gte, 5}, {lte, 10}]}]).
	  
	%% find documents where field1 is greater than 5 and less than 10
	emongo:find(test, "collection", [{"field1", [{'>', 5}, {'<', 10}]}]).
	  
	%% find documents where field1 is greater than or equal to 5 and less than or equal to 10
	emongo:find(test, "collection", [{"field1", [{'>=', 5}, {'=<', 10}]}]).
	
__not equal__

	%% find documents where field1 is not equal to 5 or 10
	emongo:find(test, "collection", [{"field1", [{ne, 5}, {ne, 10}]}]).
	  
	%% find documents where field1 is not equal to 5
	emongo:find(test, "collection", [{"field1", [{'=/=', 5}]}]).
	  
	%% find documents where field1 is not equal to 5
	emongo:find(test, "collection", [{"field1", [{'/=', 5}]}]).
	
__in__

	%% find documents where the value of field1 is one of the values in the list [1,2,3,4,5]
	emongo:find(test, "collection", [{"field1", [{in, [1,2,3,4,5]}]}]).

__not in__
	
	%% find documents where the value of field1 is NOT one of the values in the list [1,2,3,4,5]
	emongo:find(test, "collection", [{"field1", [{nin, [1,2,3,4,5]}]}]).
	
__all__

	%% find documents where the value of field1 is an array and contains all of the values in the list [1,2,3,4,5]
	emongo:find(test, "collection", [{"field1", [{all, [1,2,3,4,5]}]}]).
	
__size__

	%% find documents where the value of field1 is an array of size 10
	emongo:find(test, "collection", [{"field1", [{size, 10}]}]).
	
__exists__

	%% find documents where field1 exists
	emongo:find(test, "collection", [{"field1", [{exists, true}]}]).
	
__where__

	%% find documents where the value of field1 is greater than 10
	emongo:find(test, "collection", [{where, "this.field1 > 10"}]).
	
__nested queries__

	%% find documents with an address field containing a sub-document 
	%% with street equal to "Maple Drive".
	%% ie: [{"address", [{"street", "Maple Drive"}, {"zip", 94114}]
	emongo:find(test, "people", [{"address.street", "Maple Drive"}]).