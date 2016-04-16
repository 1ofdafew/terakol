# Terakol

This is a REST API on top of Solr Engine library

The API

| Endpoint        | Method | Params | Description       |
|-----------------|--------|--------|-------------------|
| /api/auth       | POST   | json   | Authenticate for Token |
| /api/users      | GET    | _none_ | List all users |
| /api/users      | POST   | json   | Create a new user |
| /api/users/:id  | GET    | email  | List user by email |
| /api/users/:id  | PUT    | json   | Update user by email |
| /api/users/:id  | PATCH  | json   | Update user by email |
| /api/users/:id  | DELETE | json   | Delete user by email |
| /api/media/     | GET    | _none_ | List all media |
| /api/upload/    | POST   | _none_ | Upload media |
