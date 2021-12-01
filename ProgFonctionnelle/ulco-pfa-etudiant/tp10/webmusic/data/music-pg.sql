-- create database:
-- psql -U postgres -f music-pg.sql

-- test database:
-- psql "host='localhost' port=5432 dbname=music_db user=music_user"
-- SELECT * FROM artist;

-- delete database:
-- sudo -u postgres dropdb music_db

CREATE DATABASE music_db;
\connect music_db

SELECT current_database();
SELECT current_user;

-- \ir music.sql

CREATE TABLE artist (
  artist_id SERIAL PRIMARY KEY,
  artist_name TEXT
);

CREATE TABLE title (
  title_id SERIAL PRIMARY KEY, 
  title_artist INTEGER, 
  title_name TEXT, 
  FOREIGN KEY(title_artist) REFERENCES artist(artist_id)
);

INSERT INTO artist (artist_name) VALUES('Radiohead');
INSERT INTO artist (artist_name) VALUES('Rage against the machine');
INSERT INTO artist (artist_name) VALUES('Ibrahim Maalouf');

INSERT INTO title (title_artist, title_name) VALUES(1, 'Paranoid android');
INSERT INTO title (title_artist, title_name) VALUES(1, 'Just');
INSERT INTO title (title_artist, title_name) VALUES(2, 'Take the power back');
INSERT INTO title (title_artist, title_name) VALUES(2, 'How I could just kill a man');
INSERT INTO title (title_artist, title_name) VALUES(3, 'La porte bonheur');


DROP ROLE IF EXISTS music_user;
CREATE ROLE music_user WITH LOGIN PASSWORD 'toto';

-- GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO music_user;
-- GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO music_user;

GRANT SELECT ON TABLE title TO music_user;
GRANT SELECT ON TABLE artist TO music_user;

