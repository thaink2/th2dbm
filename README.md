# Database Manager GUI

This package serves as an interface to work with relational databases, allowing you to:
-   Visualize your data
-   Add tables
-   Add variables
-   Manage Access

## Motivation

We were inspired by the article of [Yu Day](https://shanghai.hosting.nyu.edu/data/r/case-4-database-management-shiny.html) and thought maybe this could be used internally. However, the script was not available, and important features were missing. Thus, we have built our own Database manager GUI using the [shiny](https://shiny.posit.co/) framework. Recently, we have decided to make the GUI an open-source package.

## Main features

(You can add main features here)

## Demo 

[![Demo of Database Manager](./inst/video/demo_DBM_thumbnail.jpg)](./inst/video/demo_DBM.mp4)

Click on the image above to view the demo video.

## Scope

(Add scope information here)

## Data encryption as a must

(Add information about data encryption here)

## Interactive Database manager: how to

Before starting the GUI, first of all you need to set some configs, as follows:
- Database credentials as ENV Variables:
    -   target_db
    -   host/server
    -   database name
    -   username
    -   password
    -   port
    -   additional params
- Encryption passphrase

### Supported Databases
  - Postgresql
  - Oracle
  - SQLite
  - Snowflake
  - Databricks
  - Amazon Redshift
  - Google BigQuery
  
In order to establish connections to Databases, we rely on a very powerful R package [DatabaseConnector](https://github.com/OHDSI/DatabaseConnector)

### On Local: on your browser using webR
We recommend that you use the [shinylive](https://github.com/posit-dev/r-shinylive) package

### On Server using Docker
A detailed Dockerfile is put at your disposal in order to generate your own docker image. We have a public one available on our [DockerHub]()

## Further Development

(Add information about future development plans)

## License 

(Add license information)

## Contribution 

(Add contribution guidelines)

## Acknowledgements

(Add acknowledgements)
