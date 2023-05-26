# This scripts looks for dependent packages to be included on the software page

packages <- renv::dependencies()
packages <- sort(setdiff(unique(packages$Package), c("tools", "stats", "rsconnect")))
saveRDS(packages, "static/dependent-packages.rds", version = 2)
