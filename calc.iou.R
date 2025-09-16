calc.iou = function(box1, box2){
  
  # Extract coordinates for each box
  box1_coords = as.numeric(strsplit(box1, ' ')[[1]])
  box2_coords = as.numeric(strsplit(box2, ' ')[[1]])
  
  # Calculate the intersection coordinates
  intersection = c(max(box1_coords[1], box2_coords[1]),
                    max(box1_coords[2], box2_coords[2]),
                    min(box1_coords[3], box2_coords[3]),
                    min(box1_coords[4], box2_coords[4]))
  
  # Calculate the area of intersection
  intersection_area = max(0, intersection[3] - intersection[1] + 1) * 
    max(0, intersection[4] - intersection[2] + 1)
  
  # Calculate the area of the individual boxes
  box1_area = (box1_coords[3] - box1_coords[1] + 1) * 
    (box1_coords[4] - box1_coords[2] + 1)
  box2_area = (box2_coords[3] - box2_coords[1] + 1) * 
    (box2_coords[4] - box2_coords[2] + 1)
  
  # Calculate the union area
  union_area = box1_area + box2_area - intersection_area
  
  # Calculate IoU
  iou = intersection_area / union_area
  
  # Return
  return(iou)
  
}




