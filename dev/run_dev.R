
# pkgload::load_all()

# Select trust to run app for
Sys.setenv("R_CONFIG_ACTIVE" = "trusts_abc")

# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
run_app(where_am_i = "dev") # Change to "production" to run on server.
