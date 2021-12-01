-- create database:
-- sqlite3 music.db < music.sql

CREATE TABLE artist (
  artist_id INTEGER PRIMARY KEY AUTOINCREMENT,
  artist_name TEXT
);

CREATE TABLE title (
  title_id INTEGER PRIMARY KEY AUTOINCREMENT, 
  title_artist INTEGER, 
  title_name TEXT, 
  FOREIGN KEY(title_artist) REFERENCES artist(artist_id)
);

INSERT INTO artist VALUES(1, 'Radiohead');
INSERT INTO artist VALUES(2, 'Rage against the machine');
INSERT INTO artist VALUES(3, 'Ibrahim Maalouf');

INSERT INTO title VALUES(1, 1, 'Paranoid android');
INSERT INTO title VALUES(2, 1, 'Just');
INSERT INTO title VALUES(3, 2, 'Take the power back');
INSERT INTO title VALUES(4, 2, 'How I could just kill a man');
INSERT INTO title VALUES(5, 3, 'La porte bonheur');

