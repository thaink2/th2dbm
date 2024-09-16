
The package serves as a user interface to work with relational Database, the GUI nables users to interact with the database and can turn into a small ERP by doing the following actions:

-   Create, Edit & Delete tables 
-   Visualize your data content
-   Add new entries using different types:
    * numerical, categorical, logical
    * time, dates
    * List of user defined choices
    * Choices from different tables (dynamically changing)
    * UUIDs

## Motivation

We were inspired by the article of [Yu Day](https://shanghai.hosting.nyu.edu/data/r/case-4-database-management-shiny.html) and thought maybe this can used internally. However the script was not available, and some important features were missing. Thus, we have built our own Database manager GUI using [shiny](https://shiny.posit.co/) framework. Recently we have decided to make the GUI as an open source package

# Main features

# Walk through demo

![database viewer/manager](./media/videos/demo_DBM.gif)

## Scope


## Data encryption as a must


# Interactive Database manager : how to

Before starting the GUI, first of all you need to set some configs, as follows:

- Database credentials as ENV Variables:
    -   target database
    -   host/server
    -   database name
    -   username
    -   password
    -   port
    -   additional params

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

* Next steps are under development

## On Server using Docker

A detailed Dockerfiler is put at your disposal in order to generate your own docker image. 
You can simply use the public docker image available on our [th2dbm docker image](https://hub.docker.com/repository/docker/th2farid/th2dbm/general)


# Further Dev

## Licence 

## Contribution 

## Acknowledgements
