## Liste des exemples de requête

### Users

#### GET `/users/all`
- Le Token est placé dans le Header de la requête.
- Return :
  ```json
  {
    "type": "string",
    "message": "string",
    "usersGetted": "array or null",
  }
  ```

#### POST `/users/auth`
- Body :
  ```json
  {
    "email": "string",
    "password": "string",
  }
  ```
- Return :
  ```json
  {
    "type": "string",
    "message": "string",
    "userGetted": "object or null",
  }
  ```

#### POST `/users/auth_google`
- Body :
  ```json
  {
    "providerID": "string",
  }
  ```
- Return :
  ```json
  {
    "type": "string",
    "message": "string",
    "userGetted": "object or null",
  }
  ```

#### GET `/users/:idUser`
- idUser : `integer`
- Return :
  ```json
  {
    "type": "string",
    "message": "string",
    "userGetted": "object or null",
  }
  ```

#### POST `/users/create`
- Body :
  ```json
  {
    "firstName": "string",
    "lastName": "string",
    "email": "string",
    "password": "string",
  }
  ```
- Return :
  ```json
  {
    "type": "string",
    "message": "string",
    "userCreated": "object or null",
  }
  ```

#### POST `/users/create_google`
- Body :
  ```json
  {
    "firstName": "string",
    "lastName": "string",
    "email": "string",
    "provider": "string",
    "providerID": "string",
  }
  ```
- Return :
  ```json
  {
    "type": "string",
    "message": "string",
    "userCreated": "object or null",
  }
  ```

#### PATCH `/users/edit`
- Body :
  ```json
  {
    "idUser": "integer",
    "fieldsToEdit": {
      "firstName": "string, NOT REQUIRED",
      "lastName": "string, NOT REQUIRED",
      "email": "string, NOT REQUIRED",
      "password": "string, NOT REQUIRED",
    },
  }
  ```
- Return :
  ```json
  {
    "type": "string",
    "message": "string",
    "userEdited": "object or null",
  }
  ```

#### DELETE `/users/delete`
- Body :
  ```json
  {
    "idUser": "integer",
  }
  ```
- Return :
  ```json
  {
    "type": "string",
    "message": "string",
  }
  ```

### Recettes

#### GET `/recettes/`
- Le Token est placé dans le Header de la requête.
- Return :
  ```json
  {
    "type": "string",
    "message": "string",
    "recettes": "array or null",
  }
  ```

#### POST `/recettes/new`
- Le Token est placé dans le Header de la requête.
- Body : 
  ```json
  {
    "title": "string",
    "description": "string",
    "steps": [
      {
        "idStep" : "string",
        "step": "string",
      }
    ],
  }
  ```
- Return :
  ```json
  {
    "type": "string",
    "message": "string",
    "recette": "object or null",
  }
  ```

#### GET `/recettes/:idRecette`
- Le Token est placé dans le Header de la requête.
- idRecette : `integer`
- Return :
  ```json
  {
    "type": "string",
    "message": "string",
    "recette": "object or null",
  }
  ```

#### PATCH `/recettes/:idRecette`
- Le Token est placé dans le Header de la requête.
- idRecette : `integer`
- Body : 
  ```json
  {
    "title": "string NOT REQUIRED",
    "description": "string NOT REQUIRED",
    "steps": "array NOT REQUIRED",
  }
  ```
- Return :
  ```json
  {
    "type": "string",
    "message": "string",
    "recette": "object or null",
  }
  ```

#### DELETE `/recettes/:idRecette`
- Le Token est placé dans le Header de la requête.
- idRecette : `integer`
- Return :
  ```json
  {
    "type": "string",
    "message": "string",
  }
  ```