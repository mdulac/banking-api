# Banking-api

## What is it ?

Banking-api is a small Scala API to manage Wallets, Cards and Tranfers between them.
It has been created only using pure functional programming (no side effect, except for the logger for the unit tests).
It uses a lot of libraries : http4s, doobie, cats / cats-effects, circe, refined, scalacheck

## Prerequisite
Install [Java 8](https://sdkman.io/)

Install [SBT](https://www.scala-sbt.org/)

Install [Docker](https://docs.docker.com/docker-for-mac/install/)

## Build docker image from the project
Run `sbt` in the project directory (root)

Run `docker:publishLocal` in the SBT prompt

## Run the server and the H2 database inside docker containers
Run `docker-compose up` in the project root directory

The server is listening at `localhost:8080` and the database at `jdbc:h2:tcp://localhost:9082/banking-api`

You can use the openapi documentation `swagger.yaml` to use it inside Postman, for example

## Examples
Some users, companies, wallets and cards are already created, from the flighway SQL scripts
Companies : `6ca1e2b7-f11c-4f95-945f-78f75a09382d` (Holding for the master wallets), `9d6493b3-b550-49a5-9a55-a5b0568225fc`

Users : `f0ef3449-f5f8-4e07-82ee-683247e11dc3`, `76c4785e-da22-4cfe-8495-27ae4d6f9c15`

Wallets : `c7f3c868-ea95-4072-a507-1543799d26fe`, `10f6d391-11cd-463b-8a51-c95bc4580d2e`, `130081e3-3677-46b9-b7eb-518a3e1f8f19`

Cards : `48a9cf36-32b2-4b79-8964-020ce6f234f9`, `5244839a-0272-4786-bb29-6423623578b5`

__Do not forget to add headers `User-Id` and `Company-Id` in your http requests__

You can use the file `banking-api.postman_collection.json` as an example