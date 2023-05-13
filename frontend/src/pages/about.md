# Motivation

This project was initiated as [university](https://www.fri.uniza.sk/en/) assignment. Goal of the assignment is to create a fullstack application using microservice architecture.

# Architecture

The project is fully implemented in rust. Frontend is powered by [dioxus](https://dioxuslabs.com/), backend is using [poem](https://github.com/poem-web/poem), database used here is [mongodb](https://www.mongodb.com/) and finally second microservice is a search engine [meilisearch](https://www.meilisearch.com/).

## Service Diagram

```
    +----------+               +-------------+
    | frontend |               | meilisearch |
    +----------+               +-------------+
        |                             |
        |                  +--------------------+               +---------+
        +------------------| backend controller |---------------| mongodb |
                           +--------------------+               +---------+
```
