-- create database:
-- sqlite3 movie.db < movie.sql

CREATE TABLE movie (
  movie_id INTEGER PRIMARY KEY AUTOINCREMENT,
  movie_title TEXT,
  movie_year INTEGER
);

CREATE TABLE person (
  person_id INTEGER PRIMARY KEY AUTOINCREMENT,
  person_name TEXT
);

CREATE TABLE role (
  role_id INTEGER PRIMARY KEY AUTOINCREMENT,
  role_name TEXT
);

CREATE TABLE prod (
  prod_movie INTEGER, 
  prod_person INTEGER, 
  prod_role INTEGER, 
  PRIMARY KEY(prod_movie, prod_person, prod_role),
  FOREIGN KEY(prod_movie) REFERENCES movie(movie_id),
  FOREIGN KEY(prod_person) REFERENCES person(person_id),
  FOREIGN KEY(prod_role) REFERENCES role(role_id)
);

INSERT INTO movie VALUES(1, 'Bernie', 1996);
INSERT INTO movie VALUES(2, 'Le Kid', 1921);
INSERT INTO movie VALUES(3, 'Metropolis', 1927);
INSERT INTO movie VALUES(4, 'Citizen Kane', 1941);

INSERT INTO person VALUES(1, 'Orson Welles');
INSERT INTO person VALUES(2, 'Charlie Chaplin');
INSERT INTO person VALUES(3, 'Albert Dupontel');
INSERT INTO person VALUES(4, 'Claude Perron');
INSERT INTO person VALUES(5, 'Alfred Abel');
INSERT INTO person VALUES(6, 'Fritz Lang');

INSERT INTO role VALUES(1, 'Réalisateur');
INSERT INTO role VALUES(2, 'Acteur');
INSERT INTO role VALUES(3, 'Producteur');

INSERT INTO prod VALUES(1, 3, 1);
INSERT INTO prod VALUES(1, 3, 2);
INSERT INTO prod VALUES(1, 4, 2);
INSERT INTO prod VALUES(2, 2, 1);
INSERT INTO prod VALUES(2, 2, 2);
INSERT INTO prod VALUES(2, 2, 3);
INSERT INTO prod VALUES(3, 5, 2);
INSERT INTO prod VALUES(3, 6, 1);
INSERT INTO prod VALUES(4, 1, 1);
INSERT INTO prod VALUES(4, 1, 2);
INSERT INTO prod VALUES(4, 1, 3);

