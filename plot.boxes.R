plot.boxes = function(boxes_d, boxes_g) {
  plot(1, type = 'n', xlab = 'X', ylab = 'Y', 
       xlim = c(0, 5000), ylim = c(4000, 0))
  
  for(box in boxes_g){
    box = as.numeric(strsplit(box, ' ')[[1]])
    rect(box[1], box[2], box[3], box[4], border = 'black')
  }
  
  for(box in boxes_d){
    box = as.numeric(strsplit(box, ' ')[[1]])
    rect(box[1], box[2], box[3], box[4], border = 'red')
  }
  
}