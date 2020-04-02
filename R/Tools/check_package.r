# Check if package exist, if not then install package
#
check_package <- function(vector_package){
  
  # DESC:  Installs packages if such package is not installed
  # INPUT: Vector , one or more package i.e, c("package_1", "package_2")
  
  list.of.packages <- vector_package
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
}


