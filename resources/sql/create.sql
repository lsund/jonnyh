CREATE TABLE Game
(
    id          SERIAL PRIMARY KEY,
    event       varchar(64) NOT NULL,
    site        varchar(64) NOT NULL,
    date        varchar(64) NOT NULL,
    round       varchar(64) NOT NULL,
    white       varchar(64) NOT NULL,
    black       varchar(64) NOT NULL,
    result      varchar(64) NOT NULL,
    whiteelo    varchar(64) NOT NULL,
    blackelo    varchar(64) NOT NULL,
    eco         varchar(64) NOT NULL
);

CREATE TABLE Move
(
    id          SERIAL PRIMARY KEY,
    gameid      INT NOT NULL,
    movenumber  INT NOT NULL,
    white       varchar(64) NOT NULL,
    black       varchar(64) NOT NULL,


    FOREIGN KEY (GameID) REFERENCES Game (ID)

);

