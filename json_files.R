# Loading libraries
libraries = c('data.table', 'jsonlite')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}


# Clean R
rm(list=ls()) 

path_data = file.path('C:/Users/au784040/Documents_C/birch_mice_noVC/training/sorted')
folder = "annotations_other"
output_folder = "annotations_other_update"

files = list.files(file.path(path_data, folder), full.names = TRUE)
species= fread(file = file.path(path_data,"species_helper.csv"), header = TRUE)
categories = species[Category != "", unique(Category)]

#bird = "Rhinolophus sp."
#small_animal = "Pipistrellus sp."
#large_animal = "Nyctalus noctula"

for (file in files) {
  name = basename(file)
  print(name)
  
  if (length(grep("bird", name, ignore.case = TRUE)) > 0) {
    category = "Rhinolophus sp."
  } else if (length(grep(paste(species[Category == "small_animal", Abbreviation], collapse = "|"), 
                         name, ignore.case = TRUE)) > 0) {
    category = "Pipistrellus sp."
  } else if (length(grep(paste(species[Category == "large_animal", Abbreviation], collapse = "|"), 
                         name, ignore.case = TRUE)) > 0) {
    category = "Nyctalus noctula"
  } else {
    print("ERROR: Could not find category for")
    print(name)
  }
  print(category)
  
  json = fromJSON(file)
  json$shapes$label = category
  json$predictions = category
  json$labels = category
  output = toJSON(json, pretty = TRUE, auto_unbox = TRUE)
  
  write(output, file= file.path(path_data, output_folder, name))
  
}

 

