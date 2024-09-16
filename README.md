
The package serves as an interface to work with relational Database, by do the following actions :

-   Visualize your data
-   Add tables
-   Add variables
-   Manage Access

## Motivation

We were inspired by the article of [Yu Day](https://shanghai.hosting.nyu.edu/data/r/case-4-database-management-shiny.html) and thought maybe this can used internally. However the script was not available, and also important features were missing. Thus, we have build our own Database manager GUI using [shiny](https://shiny.posit.co/) framework. Recently we have decided to make the GUI as an open source package

# Main features

# Demo 

![](./inst/video/demo_DBM.mp4)

## Scope

## Data encryption as a must

# Interactive Database manager : how to

Before starting the GUI , first of all you need to set some configs, as follows:

- Database credentials as ENV Variables:
    -   target_db
    -   host/server
    -   database name
    -   username
    -   password
    -   port
    -   additional params

- Encryption passphrase

Supported Databases
  - Postgresql
  - Oracle
  - SQLite
  - Snowflake
  - Databricks
  - Amazon Redshift
  - Google BigQuery
  
In order to establish connection to Databases, we rely on a very powerful R package [DatabaseConnector](https://github.com/OHDSI/DatabaseConnector)


## On Local: on your browser using webR

We recommend that you use [shinylive](https://github.com/posit-dev/r-shinylive) package

## On Server using Docker

A detailed Dockerfiler is put at your disposal in order to generate your own docker image. We have a public one available on our [DockerHub]()

# Further Dev

## Licence 

## Contribution 

## Acknowledgements
