DROP TABLE IF EXISTS reservations, performances, users, movies, theaters;

CREATE TABLE users (
  username VARCHAR(25) NOT NULL,
  name VARCHAR(25) NOT NULL,
  address VARCHAR(25),
  phonenbr TINYINT(10),
  PRIMARY KEY(username)
);

CREATE TABLE movies (
  name VARCHAR(25) NOT NULL,
  PRIMARY KEY(name)
);

CREATE TABLE theaters (
  name VARCHAR(25) NOT NULL,
  seats INTEGER,
  PRIMARY KEY(name)
);

CREATE TABLE performances (
  moviename VARCHAR(25),
  date DATE,
  theatername VARCHAR(25),
  seatsleft INTEGER,
  PRIMARY KEY(moviename, date),
  FOREIGN KEY(moviename) REFERENCES movies(name) ON DELETE CASCADE,
  FOREIGN KEY(theatername) REFERENCES theaters(name) ON DELETE CASCADE
);

CREATE TABLE reservations (
  resnbr INTEGER AUTO_INCREMENT,
  date DATE,
  moviename VARCHAR(25),
  username VARCHAR(25),
  PRIMARY KEY(resnbr),
  FOREIGN KEY(moviename, date) REFERENCES performances(moviename, date) ON DELETE CASCADE,
  FOREIGN KEY(username) REFERENCES users(username) ON DELETE CASCADE
);

INSERT INTO users VALUES("per123", "Per Andersson", NULL, NULL);
# INSERT INTO users VALUES("per123", "Per Svensson", NULL, NULL);
INSERT INTO theaters VALUES("SF 1", 72);
# INSERT INTO theaters VALUES("SF 1", 32);
INSERT INTO theaters VALUES("SF 2", 201);
INSERT INTO movies VALUES("The Revenant");
INSERT INTO movies VALUES("Star Wars");
INSERT INTO performances VALUES("The Revenant", "1984-01-01", "SF 2", NULL);
INSERT INTO performances VALUES("Star Wars", "1942-02-28", "SF 1", NULL);
# INSERT INTO performances VALUES("The Revenant", "1984-01-01", "SF 2", NULL);
INSERT INTO reservations VALUES(NULL, "1984-01-01", "The Revenant", "per123");
# INSERT INTO reservations VALUES(NULL, "1984-01-01", "The Revenant", "per12");

UPDATE performances, theaters SET seatsleft=seats WHERE theatername=name;

SELECT * FROM reservations;
SELECT * FROM movies;
SELECT * FROM theaters;
SELECT * FROM performances;
