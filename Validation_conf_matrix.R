# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: mønsted article
# Author: Simeon Q. Smeele
# Description: Creates confusion matrix from BatNet predictions and manual 
# annotation in BatNet format. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('data.table', 'stringr', 'caret', 'dplyr')
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

folder= "model10"
graph = "conf_matrix10.pdf"


path_data = file.path('C:/Users/au784040/Documents_C/birch_mice_project', folder)
#path_signe = 'analysis/results/annotations_signe'
#path_test_set = 'analysis/results/annotations_signe_for_batnet/testing'
#path_batnet = sprintf(
#  'analysis/results/batnet/2024_03_31_%s_%s/detected_bats.csv', way, model)
path_pdf = file.path('C:/Users/au784040/Documents_C/birch_mice_project', folder, graph) 

# Load data and function
batnet = fread(file.path(path_data, "model_outcome.csv"))

manual = fread(file.path(path_data, "ground_truth.csv"))

# Report issues
issues = manual$Filename[which(is.na(manual$Species != 'Not-A-Bat'))]
if(length(issues) > 0) warning('Issues with pictures: ', 
                               paste(issues, collapse = ', '), '.')
manual = manual[!is.na(manual$Species != 'Not-A-Bat'),]

# List unique images
images = unique(manual$Filename)

# Rename Species to actual names
manual[Species == "Barbastella barbastellus", Code := "mouse"]
batnet[Species == "Barbastella barbastellus", Code := "mouse"]

manual[Species == "Myotis bechsteinii", Code := "vole"]
batnet[Species == "Myotis bechsteinii", Code := "vole"]

manual[Species == "Eptesicus serotinus", Code := "shrew"]
batnet[Species == "Eptesicus serotinus", Code := "shrew"]

#manual[Species == "Rhinolophus sp.", Code := "bird"]
#batnet[Species == "Rhinolophus sp.", Code := "bird"]

#manual[Species == "Pipistrellus sp.", Code := "small_animal"]
#batnet[Species == "Pipistrellus sp.", Code := "small_animal"]

#manual[Species == "Nyctalus noctula", Code := "large_animal"]
#batnet[Species == "Nyctalus noctula", Code := "large_animal"]

#manual[Species == "Not-A-Bat", Code := "other"]
#batnet[Species == "Not-A-Bat", Code := "other"]

manual[Species =='', Code := "empty"]
batnet[Species == '', Code := "empty"]

# Remove Not-A-Bat and empty images from both
# (FP and FN will be added later in loop again)
manual = manual[manual$Species != 'Not-A-Bat',]
manual = manual[manual$Species != '',]
batnet = batnet[batnet$Species != '',]
batnet = batnet[batnet$Species !='Not-A-Bat',]

#batnet = batnet[Flag != "unsure", ]

batnet = as.data.frame(batnet)
manual = as.data.frame(manual)

# Create place holders for output
fps = fns = tps = c()
class_results = data.frame()



# Run through images
for(image in images){
  
  print(image)
  
  ## subset
  d = batnet[batnet$Filename == image,]
  g = manual[manual$Filename == image,]
  links = data.frame()
  
  #if both are empty nothing to be detected on image
  if(nrow(g) == 0 & nrow(d) == 0){
    next
  }
  
  #if ground truth is empty than we have a FP
  if(nrow(g) == 0){
  #if(any(g$Code == "empty")){
    new_fps = rownames(d)
    fps = c(fps, new_fps)
    next
  }
  
  ## plot for debugging
  if(FALSE) plot.boxes(d$Box, g$Box)
  
  ## create links and compute IoU
  
  for(di in seq_len(nrow(d))){
    for(gi in seq_len(nrow(g))){
      links = rbind(links, 
                    data.frame(
                      file = d$Filename[di],
                      # cl = d$Confidence.level[di],
                      row_d = rownames(d)[di],
                      row_g = rownames(g)[gi],
                      d = d$Code[di],
                      g = g$Code[gi],
                      iou = calc.iou(d$Box[di], g$Box[gi]),
                      confid = d$`Confidence level`[di]
                    ))
    }
  }
  
  ## remove links with less than 20% overlap
  links = links[links$iou > 0.2,]
  
  ## run through ground truths and remove all but one link
  ## first order so that the ones with highest overlap are at the top
  ## then remove duplications (which will not be the top entry)
  if(nrow(links) > 1){
    links = links[order(links$iou, decreasing = TRUE),]
    links = links[!duplicated(links$row_g),]
    links = links[!duplicated(links$row_d),]
  }
  
  ## check if there are any duplications left
  if(any(duplicated(links$row_d)) | any(duplicated(links$row_g)))
    stop('Found duplications in file ', image, '.')
  
  ## store remaining
  class_results = rbind(class_results,
                        links)
  
  ## get fps
  new_fps = rownames(d)[!rownames(d) %in% links$row_d]
  fps = c(fps, new_fps)
  
  ## get fns
  new_fns = rownames(g)[!rownames(g) %in% links$row_g]
  fns = c(fns, new_fns)
  
} # end image loop

# Add false positives as such
class_results = rbind(class_results,
                      data.table(file = batnet[fps,]$Filename,
                                 # cl = batnet[fps,]$Confidence.level,
                                 row_d = fps,
                                 row_g = 'NA',
                                 d = batnet[fps,]$Code,
                                 g = 'empty',
                                 iou = NA,
                                 confid = batnet[fps,]$`Confidence level`))

# Add false negatives as such
class_results = rbind(class_results,
                      data.frame(file = manual[fns,]$Filename,
                                 # cl = NA,
                                 row_d = 'NA',
                                 row_g = fns,
                                 d = 'empty',
                                 g = manual[fns,]$Code,
                                 iou = NA,
                                 confid = NA))

# Run checks
if(any(duplicated(class_results$row_d[class_results$row_d != 'NA']))) 
  stop('Duplications in row d.')
if(any(duplicated(class_results$row_g[class_results$row_g != 'NA']))) 
  stop('Duplications in row g.')
if(any(!rownames(batnet) %in% class_results$row_d)) 
  stop('Missing rows from BatNet.')
if(any(!rownames(manual[manual$Multiple != 'empty',]) %in% 
       class_results$row_g)) 
  stop('Missing rows from ground truth.')
if(any(!rownames(manual[manual$Multiple != 'empty',]) %in% 
       class_results$row_g)) 
  stop('Missing rows from ground truth.')

# Create confusion matrix
#levels = sort(unique(c(class_results$d, class_results$g)))
levels = c("empty", "mouse", "shrew", "vole")
class_results$g = factor(class_results$g, levels = levels)
class_results$d = factor(class_results$d, levels = levels)


#CAREFUL: The matrix will not include empty = empty 
# something to think about for the future
conf_matrix = table(class_results$d, class_results$g)
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


accuracy_overall = length(which(class_results$d == class_results$g))/
  nrow(class_results)
tp_detection = length(which(class_results$g != 'empty' &
                              class_results$d != 'empty'))
fp_detection = length(which(class_results$g == 'empty' &
                              class_results$d != 'empty'))
fn_detection = length(which(class_results$g != 'empty' &
                              class_results$d == 'empty'))

if(sum(tp_detection, fp_detection, fn_detection) != nrow(class_results))
  stop('This does not add up.')
accuracy_detection = tp_detection/nrow(class_results)
precision_detection = tp_detection/sum(tp_detection, fp_detection)
recall_detection = tp_detection/sum(tp_detection, fn_detection)
f1_detection = 2*precision_detection*recall_detection/
  (precision_detection+recall_detection)
correct_classification = length(which(class_results$g == class_results$d &
                                        class_results$g != 'empty'))
incorrect_classification = length(which(class_results$g != class_results$d &
                                          class_results$g != 'empty'))
accuracy_classification = correct_classification/
  sum(correct_classification, incorrect_classification)

# Plot confusion matrix
pdf(path_pdf, 
    width = length(unique(class_results$d))+1.5,
    height = length(unique(class_results$g)))
par(mar = c(4, 5, 1, 7.5))
color_gradient = colorRampPalette(c('lightblue', 'darkblue'))
colors <- color_gradient(101)
plot(seq_along(levels), type = 'n', xlab = '', ylab = '',
     xlim = c(0.5, length(unique(class_results$d))+0.5), 
     ylim = c(0.5, length(unique(class_results$g))+0.5),
     xaxt = 'n', yaxt = 'n')
for(lev_i in unique(class_results$d)){
  for(lev_j in unique(class_results$g)){
    i = which(rownames(conf_matrix) == lev_i)
    j = which(colnames(conf_matrix) == lev_j)
    rect(i - 0.5, j - 0.5, i + 0.5, j + 0.5,
         col = colors[as.numeric(percentages[i, j]+1)])
    text(i, j, labels = conf_matrix[i, j], col = 'white', cex = 1.5)
  }
}
label_offset <- length(unique(class_results$g))
mtext('Ground truth', 2, 3.5)
mtext('BatNet', 1, 2.5)
mtext(sort(unique(class_results$d)), side = 1, 
      at = seq_along(unique(class_results$d)), line = 0.75)
mtext(sort(unique(class_results$g)), side = 2, 
      at = seq_along(unique(class_results$g)), las = 2, line = 0.5)
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


# # Adjust width to make room for table
# pdf(path_pdf, 
#     width = length(unique(class_results$d)) + 4,
#     height = length(unique(class_results$g)))
# par(mar = c(4, 5, 1, 1))  # Smaller right margin
# 
# # Start plot
# plot(seq_along(levels), type = 'n', xlab = '', ylab = '',
#      xlim = c(0.5, length(unique(class_results$d)) + 3.5),  # widen x axis for table
#      ylim = c(0.5, length(unique(class_results$g)) + 0.5),
#      xaxt = 'n', yaxt = 'n')
# 
# # Draw confusion matrix
# for (lev_i in unique(class_results$d)) {
#   for (lev_j in unique(class_results$g)) {
#     i <- which(rownames(conf_matrix) == lev_i)
#     j <- which(colnames(conf_matrix) == lev_j)
#     rect(i - 0.5, j - 0.5, i + 0.5, j + 0.5,
#          col = colors[as.numeric(percentages[i, j] + 1)])
#     text(i, j, labels = conf_matrix[i, j], col = 'white', cex = 1.5)
#   }
# }
# 
# # Axis labels
# mtext('Ground truth', 2, 3.5)
# mtext('BatNet', 1, 2.5)
# mtext(sort(unique(class_results$d)), side = 1, 
#       at = seq_along(unique(class_results$d)), line = 0.75)
# mtext(sort(unique(class_results$g)), side = 2, 
#       at = seq_along(unique(class_results$g)), las = 2, line = 0.5)
# 
# # Add a table of metrics
# metrics_x <- length(unique(class_results$d)) + 1.5  # Start of metrics table
# metrics_y_start <- length(unique(class_results$g)) + 0.5
# line_spacing <- 0.6
# 
# metrics <- c("Overall",
#              sprintf("Accuracy = %.2f", round(accuracy_overall, 2)),
#              "Detection",
#              sprintf("Accuracy = %.2f", round(accuracy_detection, 2)),
#              sprintf("Precision = %.2f", round(precision_detection, 2)),
#              sprintf("Recall = %.2f", round(recall_detection, 2)),
#              sprintf("F1 Score = %.2f", round(f1_detection, 2)),
#              "Classification",
#              sprintf("Accuracy = %.2f", round(accuracy_classification, 2)))
# 
# # Draw table background and text
# for (i in seq_along(metrics)) {
#   rect(metrics_x - 0.5, metrics_y_start - i + 0.2,
#        metrics_x + 2.5, metrics_y_start - i - 0.2, col = "white", border = NA)
#   text(metrics_x, metrics_y_start - i, labels = metrics[i], adj = 0, cex = 0.85)
# }

