# Script to prepare tarining data from birch mouse project 
library(data.table) # data.frames better

#clear workspace
rm(list = ls())

set.seed(42)

#set path
data_path = file.path("C:/Users/au784040/Documents_C/birch_mice_noVC/training")

training_data = data.table(image = character(860), annot = character(860))

#read in all training files
mouse_files = list.files(path = file.path(data_path, "sorted/mouse"), full.names = TRUE)
mouse_annot = list.files(path = file.path(data_path, "sorted/annotations_mouse"), full.names = TRUE)

rand_index = sample(1:length(mouse_files), 200)
training_data[1:200, image := mouse_files[rand_index]]
training_data[1:200, annot := mouse_annot[rand_index]]

shrew_files = list.files(path = file.path(data_path, "sorted/shrew"), full.names = TRUE)
shrew_annot = list.files(path = file.path(data_path, "sorted/annotations_shrew"), full.names = TRUE)

rand_index = sample(1:length(shrew_files), 200)
training_data[201:400, image := shrew_files[rand_index]]
training_data[201:400, annot := shrew_annot[rand_index]]

vole_files = list.files(path = file.path(data_path, "sorted/vole"), full.names = TRUE)
vole_annot = list.files(path = file.path(data_path, "sorted/annotations_vole"), full.names = TRUE)

rand_index = sample(1:length(vole_files), 200)
training_data[401:600, image := vole_files[rand_index]]
training_data[401:600, annot := vole_annot[rand_index]]

other_files = list.files(path = file.path(data_path, "sorted/other"), full.names = TRUE)
other_annot = list.files(path = file.path(data_path, "sorted/annotations_other_update"), full.names = TRUE)

rand_index = sample(1:length(other_files), 200)
training_data[601:800, image := other_files[rand_index]]
training_data[601:800, annot := other_annot[rand_index]]

empty_files = list.files(path = file.path(data_path, "sorted/empty"), full.names = TRUE)
empty_annot = list.files(path = file.path(data_path, "sorted/annotations_empty"), full.names = TRUE)

rand_index = sample(1:length(empty_files), 60)
training_data[801:860, image := empty_files[rand_index]]
training_data[801:860, annot := empty_annot[rand_index]]


all_data = data.table(image = c(mouse_files, shrew_files, vole_files, other_files, empty_files),
                      annot = c(mouse_annot, shrew_annot, vole_annot, other_annot, empty_annot))

validation_data = all_data[!(image %in% training_data$image),]


#save table of training data
fwrite(
  training_data,
  file = file.path(data_path, "training_set3/training_data.csv"),
  sep = ",",
  quote = TRUE,      # ensures special characters are preserved safely
  bom = TRUE         # adds UTF-8 BOM for better compatibility (especially with Excel)
)

#save table of validation data
fwrite(
  validation_data,
  file = file.path(data_path, "training_set3/validation_data.csv"),
  sep = ",",
  quote = TRUE,      # ensures special characters are preserved safely
  bom = TRUE         # adds UTF-8 BOM for better compatibility (especially with Excel)
)
