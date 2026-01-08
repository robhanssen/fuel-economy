source("defaults.r")
source("import.r")

safe_source <- safely(source, quiet = FALSE)

scripts <-
    list.files(path = "./scripts/", pattern = "\\.r$", full.names = TRUE)

walk(scripts, safe_source)