# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: mønsted article
# Author: Simeon Q. Smeele
# Description: Creates confusion matrix from deepfaune predictions and manual 
# annotation in BatNet format. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('data.table', 'stringr', 'caret', 'dplyr', 'tidyverse')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}


# Clean R
rm(list=ls()) 

source('calc.iou.R')
source('plot.boxes.R')

# Paths 
#way = 'ind'
#model = '2025_10_30_model8_birch_mice.pt'

folder= "deepfaune"
graph = "conf_matrix.pdf"


path_data = file.path('C:/Users/au784040/Documents_C/birch_mice_project', folder)
#path_signe = 'analysis/results/annotations_signe'
#path_test_set = 'analysis/results/annotations_signe_for_batnet/testing'
#path_batnet = sprintf(
#  'analysis/results/batnet/2024_03_31_%s_%s/detected_bats.csv', way, model)
path_pdf = file.path('C:/Users/au784040/Documents_C/birch_mice_project', folder, graph) 

# Load data and function
deepfaune = fread(file.path(path_data, "deepfaune.csv"))
manual = fread(file.path(path_data, "ground_truth.csv"))

setnames(deepfaune, c("filename"), c("filepath"))
setnames(manual, c("Filename"), c("filename"))
split_paths = deepfaune[, tstrsplit(filepath, "\\" , fixed = TRUE)]
deepfaune[, filename := split_paths[, V10]]

manual <- manual %>%
  group_by(filename) %>%
  mutate(
    instance_count = n(),
    all_species = paste(unique(Species), collapse = "; ")
  ) %>%
  # keep only first row when multiple, otherwise keep all rows
  filter(Multiple != "multiple" | row_number() == 1) %>%
  # for non-multiple rows, instance_count should be 1 and all_species = Species
  mutate(
    instance_count = if_else(Multiple == "multiple", instance_count, 1L),
    all_species = if_else(Multiple == "multiple", all_species, Species)
  ) %>%
  ungroup()

manual = as.data.table(manual)

# Report issues
issues = manual$filename[which(is.na(manual$Species != 'Not-A-Bat'))]
if(length(issues) > 0) warning('Issues with pictures: ', 
                               paste(issues, collapse = ', '), '.')
manual = manual[!is.na(manual$Species != 'Not-A-Bat'),]

# List unique images
images = unique(manual$filename)

# Rename Species to actual names
manual <- manual %>%
  mutate(
    orig_species = coalesce(
      str_extract(filename, "(?<=_)[^_\\d]+(?=\\.[Jj][Pp][Gg]$)"),
      "empty"
    )
  )

unique(manual$orig_species)
unique(deepfaune$prediction)

deepfaune[grepl("bird", prediction), prediction := "bird"]
deepfaune[prediction == "cat", prediction := "feline"]
deepfaune[grepl("deer", prediction), prediction := "ungulate"]
deepfaune[prediction == "dog" | prediction == "fox" , prediction := "canine"]

manual[orig_species == "vole" | orig_species == "mouse" |
         orig_species == "shrew" | orig_species == "rat" | orig_species == "mole", 
       orig_species := "micromammal"]

manual[orig_species == "small mustelid" | orig_species == "large mustelid" , orig_species := "mustelid"]
manual[orig_species == "large mammal", orig_species := "undefined"]
manual[grepl("Mmel", filename), orig_species := "badger"]
manual[grepl("Llut", filename), orig_species := "otter"]
manual[grepl("Npro", filename), orig_species := "raccoon dog"]
manual[grepl("Leur", filename), orig_species := "lagomorph",]


# manual[Species == "Barbastella barbastellus", Code := "mouse"]
# manual[Species == "Myotis bechsteinii", Code := "vole"]
# manual[Species == "Eptesicus serotinus", Code := "shrew"]
# manual[Species == "Rhinolophus sp.", Code := "bird"]
# manual[Species == "Pipistrellus sp.", Code := "small_animal"]
# manual[Species == "Nyctalus noctula", Code := "large_animal"]
# 
# # Remove Not-A-Bat and empty images from both
# # (FP and FN will be added later in loop again)
# manual = manual[manual$Species != 'Not-A-Bat',]
# manual = manual[manual$Species != '',]
# batnet = batnet[batnet$Species != '',]
# batnet = batnet[batnet$Species !='Not-A-Bat',]


compar = merge(deepfaune[, .(filename, prediction, score, top1, count)], 
               manual[, .(filename,orig_species, instance_count)], by = c("filename"))

# Create confusion matrix
levels = sort(unique(c(compar$orig_species, compar$prediction)))
#levels = c("empty", "small_animal", "large_animal", "bird", "mouse", "shrew", "vole")
compar$orig_species = factor(compar$orig_species, levels = levels)
compar$prediction = factor(compar$prediction, levels = levels)


#CAREFUL: The matrix will not include empty = empty 
# something to think about for the future
conf_matrix = table(compar$prediction, compar$orig_species)
conf_matrix = conf_matrix[rowSums(conf_matrix) > 0,
                          colSums(conf_matrix) > 0]
#conf_matrix = conf_matrix[order(rownames(conf_matrix)),
#                          order(colnames(conf_matrix))]
percentages = conf_matrix
for(i in seq_len(ncol(percentages))) 
  percentages[,i] = percentages[,i]/sum(percentages[,i]) * 100

# Compute stats
## tp_detection is when both g and d have a bat
## fp_detection is when d has a bat, but g has none
## fn_detection is when g has a bat, but d has none
## (in)correct_classification excludes g == '-None-'
## tp_classification is when bat is correct
## fp_classification is for bat_a, when bat_not_a -> bat_a
## fn_classification is for bat_a, when bat_a -> bat_not_a


accuracy_overall = length(which(compar$prediction == compar$orig_species))/
  nrow(compar)
tp_detection = length(which(compar$orig_species != 'empty' &
                              compar$prediction != 'empty'))
fp_detection = length(which(compar$orig_species == 'empty' &
                              compar$prediction != 'empty'))
fn_detection = length(which(compar$orig_species != 'empty' &
                              compar$prediction == 'empty'))

if(sum(tp_detection, fp_detection, fn_detection) != nrow(compar))
  stop('This does not add up.') # because this time we have TN as well
accuracy_detection = tp_detection/nrow(compar)
precision_detection = tp_detection/sum(tp_detection, fp_detection)
recall_detection = tp_detection/sum(tp_detection, fn_detection)
f1_detection = 2*precision_detection*recall_detection/
  (precision_detection+recall_detection)
correct_classification = length(which(compar$orig_species == compar$prediction &
                                        compar$orig_species != 'empty'))
incorrect_classification = length(which(compar$orig_species != compar$prediction &
                                          compar$orig_species != 'empty'))
accuracy_classification = correct_classification/
  sum(correct_classification, incorrect_classification)

# Plot confusion matrix
pdf(path_pdf, 
    width = length(unique(compar$prediction))+1.5,
    height = length(unique(compar$orig_species)))
par(mar = c(4, 5, 1, 7.5))
color_gradient = colorRampPalette(c('lightblue', 'darkblue'))
colors <- color_gradient(101)
plot(seq_along(levels), type = 'n', xlab = '', ylab = '',
     xlim = c(0.5, length(unique(compar$prediction))+0.5), 
     ylim = c(0.5, length(unique(compar$orig_species))+0.5),
     xaxt = 'n', yaxt = 'n')
for(lev_i in unique(compar$prediction)){
  for(lev_j in unique(compar$orig_species)){
    i = which(rownames(conf_matrix) == lev_i)
    j = which(colnames(conf_matrix) == lev_j)
    rect(i - 0.5, j - 0.5, i + 0.5, j + 0.5,
         col = colors[as.numeric(percentages[i, j]+1)])
    text(i, j, labels = conf_matrix[i, j], col = 'white', cex = 1.5)
  }
}
label_offset <- length(unique(compar$orig_species))
mtext('Ground truth', 2, 3.5)
mtext('Deepfaune', 1, 2.5)
mtext(sort(unique(compar$prediction)), side = 1, 
      at = seq_along(unique(compar$prediction)), line = 0.75)
mtext(sort(unique(compar$orig_species)), side = 2, 
      at = seq_along(unique(compar$orig_species)), las = 2, line = 0.5)
mtext('Overall:', side = 4, line = 1, 
      at = label_offset+0.5, font = 2, las = 1, adj = 0)
mtext(sprintf('accuracy = %.2f', round(accuracy_overall, 2)), side = 4, line = 1, 
      at = label_offset, font = 1, las = 1, adj = 0)
mtext('Detection:', side = 4, line = 1, 
      at = label_offset-0.5, font = 2, las = 1, adj = 0)
mtext(sprintf('accuracy = %.2f', round(accuracy_detection, 2)), side = 4, line = 1, 
      at = label_offset-1, font = 1, las = 1, adj = 0)
mtext(sprintf('precision = %.2f', round(precision_detection, 2)), side = 4, line = 1, 
      at = label_offset-1.5, font = 1, las = 1, adj = 0)
mtext(sprintf('recall = %.2f', round(recall_detection, 2)), side = 4, line = 1, 
      at = label_offset-2, font = 1, las = 1, adj = 0)
mtext(sprintf('F1 = %.2f', round(f1_detection, 2)), side = 4, line = 1, 
      at = label_offset-2.5, font = 1, las = 1, adj = 0)
mtext('Classification:', side = 4, line = 1, 
      at = label_offset-3, font = 2, las = 1, adj = 0)
mtext(sprintf('accuracy = %.2f', round(accuracy_classification, 2)), side = 4, line = 1, 
      at = label_offset-3.5, font = 1, las = 1, adj = 0)
dev.off()

# Message
message('Done.')

mistakes = compar[orig_species != prediction,]

file.copy(
  from = file.path("C:/Users/au784040/Documents_C/birch_mice_project/training/training_set4", "validation_images", mistakes$filename),
  to   = file.path("C:/Users/au784040/Documents_C/birch_mice_project/deepfaune","mistakes", mistakes$filename),
  overwrite = FALSE
)
