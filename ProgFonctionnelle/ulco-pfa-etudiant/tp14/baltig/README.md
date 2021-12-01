
# baltig

- client web utilisant l'[API gitlab](https://docs.gitlab.com/ee/api/api_resources.html#project-resources)

- compiler et lancer l'application :

```text
make database
make
```

- puis aller à `localhost:3000` dans un navigateur

- pour tester l'API gitlab :

```text
curl "https://gitlab.com/api/v4/users?username=juliendehos"
curl "https://gitlab.com/api/v4/users/juliendehos/projects"
```

