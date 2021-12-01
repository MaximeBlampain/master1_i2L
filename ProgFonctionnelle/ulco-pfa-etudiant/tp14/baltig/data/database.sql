
CREATE TABLE news (
    news_id     INTEGER PRIMARY KEY AUTOINCREMENT,
    news_title  TEXT,
    news_date   DATE,
    news_body   TEXT
);
 
INSERT INTO news VALUES(1, "Ouverture de baltig", "1970-01-01",
    "Ca y est baltig est ouvert. Youpie, youpie, trop d'la balle !");
INSERT INTO news VALUES(2, "Perturbation de service", "1970-01-02",
    "Devant la faramineuse affluence de 3 requêtes en 24h, baltig subit actuellement quelques ralentissements. Ce problème sera résolu prochainement par la fermeture de baltig.");

