setVolumes <- function(location,name.volumes="my-data") {
  
  if (location=="homeDirectoryForMac") {
    
    volumes =  "~/"
    names(volumes) <- "home"
    
  }
  
  
  if (location=="examples") {
    
    volumes=paste(find.package("Viridot"),"/shiny-Viridot/Viridot-data/",sep = "")
    names(volumes) <- "Viridot-data"
    
  }
  
  
  if (location=="homeDirectoryForWindows") {
    
    volumes = "C:/"
    names(volumes) <- "C"
    
  }
  
  
  if (location!="homeDirectoryForMac"&location!="examples"&location!="homeDirectoryForWindows") {

    print("You have tried to specify the following as an exact path to where your data are located:")
    print(location)
    print("These are the directories and files listed in your specified path:")
    print(list.files(location))
    print("If this is not correct, correct the path specified in the setVolumes() function until you get successfully navigate the directory you want.")
    volumes = location
   names(volumes) <- name.volumes

  }  
  
return(volumes)
  
}