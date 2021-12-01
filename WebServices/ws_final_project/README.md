# Web Service - M1 I2L


## Présentation

Projet final de Web Service, réalisé par [Maxime Blampain](https://www.gitlab.com/mblampain) et [Nicolas Fourny](https://www.gitlab.com/nicolas.fourny5962).

La partie front-end représente une interface d'administration pour gérer les utilisateurs et les recettes enregistrées dans les bases de données.

La partie back-end représente une API REST qui gère les utilisateurs et les recettes enregistrées.

Une connexion à une API externe est aussi utilisée : [TheMealDB](https://www.themealdb.com/api.php)

⚠️ **INFO** : Pour se connecter à la plateforme, merci d'utiliser le login suivant :

`Login : test@test.fr`
`Password : test`

⚠️ **INFO** : Voici les ports utilisés pour le projet :
`Localhost:3001 => Front-end
Localhost:3000 => Back-end / API
Localhost:8080 => Phpmyadmin
Localhost:3306 => MySQL
Localhost:27017 => MongoDB`
## Prérequis

- [Docker](https://www.docker.com/)
- [Docker-Compose](https://docs.docker.com/compose/install/)
- [Insomnia](https://insomnia.rest/) ou [Postman](https://www.postman.com/) ou un autre software pour tester les requêtes

## Requêtes disponibles

⚠️ **INFO** : Un exemple est donné pour chaque recette dans le fichier `EXAMPLE.md`

#### Users :

|   Type   |        URL         |Paramètres|Token ?|                Body                |                Return                 |
|:--------:|--------------------|:--------:|:-----:|------------------------------------|---------------------------------------|
|**GET**   |/users/all          |   Aucun  |  Oui  |Aucun                               |Liste de User si valide, erreur sinon  |
|**POST**  |/users/auth         |   Aucun  |  Non  |email, password                     |User & Token si valide, erreur sinon   |
|**POST**  |/users/auth_google  |   Aucun  |  Non  |providerID                          |User & Token si valide, erreur sinon   |
|**GET**   |/users/:idUser      |   idUser |  Oui  |Aucun                               |User si valide, erreur sinon           |
|**POST**  |/users/create       |   Aucun  |  Non  |firstname, lastname, email, password|Success Message si valide, erreur sinon|
|**POST**  |/users/create_google|   Aucun  |  Non  |googleUserToCreate                  |Success Message si valide, erreur sinon|
|**PATCH** |/users/edit         |   Aucun  |  Oui  |idUser, fieldsToEdit                |UserEdited si valide, erreur sinon     |
|**DELETE**|/users/delete       |   Aucun  |  Oui  |idUser                              |Success Message si valide, erreur sinon|

#### Recettes :
|   Type   |         URL        |Paramètres|Token ?|           Body          |                 Return                 |
|:--------:|--------------------|:--------:|:-----:|-------------------------|----------------------------------------|
|**GET**   |/recettes/          |Aucun     |Oui    |Aucun                    |Liste de Recette si valide, erreur sinon|
|**POST**  |/recettes/new       |Aucun     |Oui    |title, description, steps|Recette si valide, erreur sinon         |
|**GET**   |/recettes/:idRecette|idRecette |Oui    |Aucun                    |Recette si valide, erreur sinon         |
|**PATCH** |/recettes/:idRecette|idRecette |Oui    |title, description, steps|Recette si valide, erreur sinon         |
|**DELETE**|/recettes/idRecette |idRecette |Oui    |Aucun                    |Success Message si valide, erreur sinon |


## Commandes utiles

Avant d'exécuter chaque commande, veillez à bien être dans le dossier correspondant.

#### Deploiement : dossier `Docker`

Créer le réseau pour le déploiement :
```docker network create reverse-proxy```

Lancer le déploiement dans le terminal :
```docker compose up ```

OU

Lancer le déploiement en tâche de fond :
```docker compose up -d```

## Technologies utilisées

#### Bases de données:
- [MongoDB](https://www.mongodb.com/fr-fr)
- [MySQL](https://www.mysql.com/fr/)

#### Front-end:
- [ReactJS](https://fr.reactjs.org/)
- [Redux](https://redux.js.org/)
- [Redux Saga](https://redux-saga.js.org/)
- [Styled Components](https://styled-components.com/)

#### Back-end: 
- [ExpressJS](http://expressjs.com/)
- [Joi](https://softchris.github.io/pages/joi.html)
- [JsonWebToken (JWT)](https://jwt.io/)
- [Mongoose](https://mongoosejs.com/)
- [Sequelize](https://sequelize.org/)