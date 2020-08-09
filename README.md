# Banking-api

## Prerequisite
Install [Java 8](https://sdkman.io/)

Install [SBT](https://www.scala-sbt.org/)

Install [Docker](https://docs.docker.com/docker-for-mac/install/)

## Build docker image from the project
Run `sbt` in the project directory (root)

Run `docker:publishLocal` in the SBT prompt

## Run the server inside the docker container
Run `docker run -p 8080:8080 -v $(pwd)/banking-db.mv.db:/home/demiourgos728/banking-db.mv.db banking-api:1.0-SNAPSHOT`

The server is listening at `localhost:8080`

You can use the openapi documentation `swagger.yaml` to use it inside Postman, for example

## Examples
Some users, companies, wallets and cards are already created, from the flighway SQL scripts
Companies : `6ca1e2b7-f11c-4f95-945f-78f75a09382d` (Spendesk for the master wallets), `9d6493b3-b550-49a5-9a55-a5b0568225fc`

Users : `f0ef3449-f5f8-4e07-82ee-683247e11dc3`, `76c4785e-da22-4cfe-8495-27ae4d6f9c15`

Wallets : `c7f3c868-ea95-4072-a507-1543799d26fe`, `10f6d391-11cd-463b-8a51-c95bc4580d2e`, `130081e3-3677-46b9-b7eb-518a3e1f8f19`

Cards : `48a9cf36-32b2-4b79-8964-020ce6f234f9`, `5244839a-0272-4786-bb29-6423623578b5`
