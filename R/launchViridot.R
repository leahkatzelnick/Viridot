launchViridot <- function(overridevol = NA) {
 
   list.of.packages <-
    c("shinyFiles",
      "shiny",
      "gtools",
      "tools",
      "drc",
      "Hmisc",
      "shinythemes")
  new.packages <-
    list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
  if (length(new.packages))
    install.packages(new.packages)
   
  msgebi <- try(require("EBImage"))
  
  if (msgebi == FALSE)  {
    
    source("https://bioconductor.org/biocLite.R")
    biocLite("BiocInstaller")
    library("BiocInstaller")
    
    biocLite("EBImage")
  }

    #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
  #  Run these two functions
  #~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
  library(shinyFiles)
  library(shiny)
  library(shinythemes)
  
  appDir <-
    system.file("shiny-Viridot", "theapp", package = "Viridot")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `Viridot`.",
         call. = FALSE)
  }
  
  
  shiny::runApp(appDir, display.mode = "normal")
}
