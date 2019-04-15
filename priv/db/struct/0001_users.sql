-- 
-- User general information
-- 
CREATE SEQUENCE users_id_seq MAXVALUE 2147483647;

CREATE TABLE users (
  id integer NOT NULL DEFAULT nextval('users_id_seq'),
  name varchar(250),
  email varchar(250),
  password varchar(32),
  token varchar(250) NOT NULL,
  meta JSONB,
  enabled boolean NOT NULL DEFAULT TRUE,
  removed boolean NOT NULL DEFAULT FALSE,
  ctime timestamp without time zone DEFAULT (now() at time zone 'utc'),
  atime timestamp without time zone DEFAULT NULL,
  dtime timestamp without time zone DEFAULT NULL,
  PRIMARY KEY (id)
) WITH (fillfactor=70);


CREATE INDEX users_cridetials_idx 
  ON users (email, password) WHERE enabled = TRUE AND removed = FALSE;

CREATE UNIQUE INDEX users_token_idx 
  ON users (token) WHERE removed = FALSE;

CREATE UNIQUE INDEX users_email_idx 
  ON users (email) WHERE removed = FALSE;

CREATE INDEX users_enabled_idx 
  ON users (enabled);

CREATE INDEX users_removed_idx 
  ON users (removed);
