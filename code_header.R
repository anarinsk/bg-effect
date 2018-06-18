### Load packages and prepare tdf ----

install_load_package <- function(packages_v){
  #
  new.packages <- packages_v[!(packages_v %in% installed.packages()[,"Package"])]
  if(length(new.packages) > 0) { install.packages(new.packages) } 
  lapply(packages_v, require, character.only = TRUE)
  invisible(capture.output())
  cat("--------------- \n")
  lapply(packages_v, function(x) {cat(x, "are loaded! \n")})
  cat("--------------- \n")
  #
}
#
c("conflicted", "tidyverse", "wrapr", "here") -> .; install_load_package(.)
c("readxl") -> .; install_load_package(.); 
{} -> .
