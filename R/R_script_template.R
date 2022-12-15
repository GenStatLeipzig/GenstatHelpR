#############################
########## HEADING ##########
#############################


# rm(list = ls())


# Start Time Tracking
start.time <- Sys.time()


# Load R Package
# .libPaths("/net/ifs1/san_projekte/projekte/genstat/07_programme/rpackages/[...]")
# .libPaths("C:/Program Files/R/R-[...]/library/")


# Load Data
# dir.load <- "/net/ifs1/san_projekte/projekte/genstat/02_projekte/[...]/data/"
# dir.load <- "J:/genstat/02_projekte/[...]/data/"
# data.0   <- read.table(file = paste0(dir.load, ""))


# Analysis
# ...


# Plot Results
# ...


# Save Data
# dir.save = "/net/ifs1/san_projekte/projekte/genstat/02_projekte/[...]/results/"
# dir.save = "J:/genstat/02_projekte/[...]/results/"
# save.image(file = paste0(dir.save, "_", "", ".RData"))
# save(...,
#      file = paste0(dir.save, "_", "", ".RData"))
# write.table(...,
#             file = paste0(dir_save, "_", "", ".txt"),
#             quote = TRUE,
#             sep = "\t",
#             row.names = FALSE)


# Stop Time Tracking
stop.time      <- Sys.time()
diff.time      <- difftime(stop.time, start.time,
                           units = "secs")
diff.time.unit <- " Seconds"
if (as.numeric(diff.time) > 60) {

  diff.time      <- difftime(stop.time, start.time,
                             units = "mins")
  diff.time.unit <- " Minutes"
}
if (as.numeric(diff.time) > 60) {

  diff.time      <- difftime(stop.time, start.time,
                             units = "hours")
  diff.time.unit <- " Hours"
}
message("Execution Time: ",
        formatC(as.numeric(diff.time),
                digits = 1,
                format = "f"),
        diff.time.unit)


################################################################################

