#This isn't part of the build process.  They should be executed infrequently, not for every build.
rm(list=ls(all=TRUE))
if( any(search()=="package:sAUC") ) detach("package:sAUC") #So the lazy-loaded datasets aren't available
# if( any(.packages(all.available=TRUE) == "sAUC") ) remove.packages("sAUC") #system("R CMD REMOVE sAUC") #This shouldn't be necesary.
# require(sAUC) #Don't load' the lazy-loaded datasets shouldn't be accessible

###############################################################
###  Declare Paths
###############################################################
directory_dataset_csv <- "./inst/extdata" #These CSVs are in the repository, but not in the build.
directory_datasets_rda <- "./data" #These RDAs are derived from the CSV, and included in the build as compressed binaries.

fasd <- read.csv(file.path(directory_dataset_csv, "fasd.csv"))
path_output_data <- file.path(directory_datasets_rda, "fasd.rda")

save(fasd, file=path_output_data, compress="xz")



