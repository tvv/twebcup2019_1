
-define(DBPOOL, pgdb).
-define(DBPOOLR, pgdbro).

-define(PG_WAIT_TIMEOUT, {300, ms}).
-define(PG_TIMEOUT, infinity).
-define(PG_RECONNECT_TIMEOUT, ?S2MS(5)).

-define(MAX_PG_INT, 2147483647).
-define(EPOCH, 62167219200).

-define(REDIS_WAIT_TIMEOUT, {300, ms}).
-define(REDIS_MAX_TRIES, 2).
-define(REDIS_TRY_SLEEP, 100).

-define(PG_SQ(Q), epgsql:squery(C, Q)).
