DROP DATABASE IF EXISTS twebcup2019_1;
CREATE USER twebcup2019_1 WITH PASSWORD 'twebcup2019_1';
CREATE DATABASE twebcup2019_1 WITH ENCODING 'utf8' OWNER twebcup2019_1;
GRANT ALL PRIVILEGES ON DATABASE twebcup2019_1 TO twebcup2019_1;
\c twebcup2019_1
CREATE EXTENSION pg_trgm;
CREATE EXTENSION intarray;