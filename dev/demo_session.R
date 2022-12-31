
# Clean environment and load package
golem::detach_all_attached()
rm(list = ls(all.names = TRUE))
golem::document_and_reload()

# Initialize working session
session_init()
