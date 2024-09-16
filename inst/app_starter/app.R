initialize_db_confgs()
shinyApp(ui = th2dbm::app_ui,
                server = th2dbm::app_server)
