-- create database:
-- sqlite3 thread.db < thread.sql

CREATE TABLE thread (
  thread_id INTEGER PRIMARY KEY AUTOINCREMENT,
  thread_name TEXT
);

CREATE TABLE t_message (
  message_id INTEGER PRIMARY KEY AUTOINCREMENT,
  message_user TEXT,
  message_text TEXT,
  message_thread INTEGER,
  FOREIGN KEY(message_thread) REFERENCES thread(thread_id)
);

INSERT INTO thread VALUES(1, "Vacances 2020-2021");
INSERT INTO thread VALUES(2, "Master Info nouveau programme");

INSERT INTO t_message VALUES (1, "John Doe", "Pas de vacances cette année.",1);
INSERT INTO t_message VALUES (2, "Toto", "Youpi!",1);
INSERT INTO t_message VALUES (3, "Toto", "Tous les cours passent en Haskell",2);