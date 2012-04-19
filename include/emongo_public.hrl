-ifndef(EMONGO_PUBLIC).

-record(response, {header, response_flag, cursor_id, offset, limit, documents}).

% Additional options that can be passed to emongo:find()
-define(TAILABLE_CURSOR, 2).
-define(SLAVE_OK, 4).
-define(OPLOG, 8).
-define(NO_CURSOR_TIMEOUT, 16).

-define(EMONGO_PUBLIC, true).
-endif.
