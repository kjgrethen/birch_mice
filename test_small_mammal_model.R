#test Böhner model

set.seed(42)
## load libraries
library("keras")
library("tensorflow")

## set up keras and tensorflow
keras::use_condaenv("keras_r") ## tell R which anaconda environment it should use
reticulate::py_discover_config()  # check that R is using the correct anaconda environment (the python version R is using should be inside the environment)


## set directories
model.dir = "C:/Users/au784040/Documents_C/Image_Classification/camera_trap_workflow-main/model"
model.name <- "small_mammal_classification_model_2022.h5"  # name of the classification model
image.dir <- "Z:/training" # folder with images

## load the model
model <- load_model_hdf5(file.path(model.dir, model.name))

## get some images
images_names <- sample(dir(image.dir), 30)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## AUTOMATIC CLASSIFICATION ---
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## load and prepare data (some error and warning messages can be ignored)
test_data <- data.frame(filename = images_names, class = "0")

testgen <- image_data_generator(rescale = 1/255) # the rescaling trasnlates the rgb channels to ratios between 0 and 1

test_generator <- flow_images_from_dataframe(test_data, 
                                             directory = image.dir, 
                                             x_col = "filename", 
                                             y_col = "class", 
                                             generator = testgen, 
                                             target_size = c(224, 224), # PROBLEM: downsizes resolution a lot and loses aspect ratio
                                             batch_size = 128, 
                                             class_mode = "categorical", 
                                             shuffle = FALSE)
# more current way to do it:
# testgen = image_dataset_from_directory(
#   directory,
#   labels = "inferred",
#   label_mode = "int",
#   class_names = NULL,
#   color_mode = "rgb",
#   batch_size = 32,
#   image_size = c(256, 256),
#   shuffle = TRUE,
#   seed = NULL,
#   validation_split = NULL,
#   subset = NULL,
#   interpolation = "bilinear",
#   follow_links = FALSE,
#   crop_to_aspect_ratio = FALSE,
#   ...
# )


## predict classes
pred <- model %>% predict(test_generator, steps = ceiling(nrow(test_data)/128))

## make output nice
results_test <- data.frame(site = NA, year = NA, filename = test_data$filename, 
                           guess1 = (apply(pred, 1, which.max))-1, confidence1 = apply(pred, 1, max))




## check the output
results_test
## this should show the first rows of the classification file that contains guess1 (the class), 
## which will be 1 in most cases (empty image) and the confidence

## we use the following coding for the classes
## 0  bad quality
## 1  empty
## 2  bird
## 3  vole
## 4  least weasel
## 5  lemming
## 6  shrew
## 7  stoat
