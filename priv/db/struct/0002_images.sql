-- 
-- Images 
-- 
CREATE SEQUENCE images_id_seq MAXVALUE 2147483647;

CREATE TABLE images (
  id integer NOT NULL DEFAULT nextval('images_id_seq'),
  user_id integer NOT NULL REFERENCES users (id),
  resource_id varchar(200) DEFAULT NULL,
  rating integer NOT NULL DEFAULT 0,
  removed boolean NOT NULL DEFAULT FALSE,
  ctime timestamp without time zone DEFAULT (now() at time zone 'utc'),
  atime timestamp without time zone DEFAULT NULL,
  dtime timestamp without time zone DEFAULT NULL,
  PRIMARY KEY (id)
) WITH (fillfactor=70);

CREATE INDEX images_user_idx
  ON images (user_id);

CREATE UNIQUE INDEX images_related_id_idx
  ON images (resource_id) WHERE removed = FALSE;

CREATE INDEX images_rating_idx
  ON images (rating) WHERE removed = FALSE;

CREATE INDEX images_removed_idx 
  ON images (removed);


--
-- Tags
--
CREATE SEQUENCE tags_id_seq MAXVALUE 2147483647;

CREATE TABLE tags (
  id integer NOT NULL DEFAULT nextval('tags_id_seq'),
  name varchar(250) NOT NULL,
  ctime timestamp without time zone DEFAULT (now() at time zone 'utc'),
  PRIMARY KEY (id)
) WITH (fillfactor=70);

CREATE UNIQUE INDEX tags_name_idx
  ON tags (name);


--
-- Image tags
--
CREATE SEQUENCE image_tags_id_seq MAXVALUE 2147483647;

CREATE TABLE image_tags (
  id integer NOT NULL DEFAULT nextval('image_tags_id_seq'),
  image_id integer NOT NULL REFERENCES images (id),
  tag_id integer NOT NULL REFERENCES tags (id),
  removed boolean NOT NULL DEFAULT TRUE,
  ctime timestamp without time zone DEFAULT (now() at time zone 'utc'),
  dtime timestamp without time zone DEFAULT NULL,
  PRIMARY KEY (id)
) WITH (fillfactor=70);

CREATE INDEX image_tags_image_idx
  ON image_tags (image_id);

CREATE INDEX image_tags_tag_idx
  ON image_tags (tag_id);

CREATE INDEX image_tags_enabled_idx 
  ON image_tags (removed);