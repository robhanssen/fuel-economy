source("defaults.r")
source("import.r")

scripts <-
    list.files(path = "./scripts/", pattern = "\\.r$", full.names = TRUE)

walk(scripts, source)