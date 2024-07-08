# SHINY -----
get_arrowhead = function(from_lng, from_lat, to_lng, to_lat, length_percentage = 5, angle = 45) {
  angle_rad <- pi * angle / 180
  
  # Calculate distance between from and to points
  distance <- sqrt((to_lng - from_lng)^2 + (to_lat - from_lat)^2)
  
  # Convert length percentage to actual length based on the distance
  length <- distance * length_percentage
  
  # Calculate angle of the line
  line_angle <- atan2(to_lat - from_lat, to_lng - from_lng)
  
  # Calculate offset angles for the two sides of the arrow
  arrow_angle1 <- line_angle + angle_rad
  arrow_angle2 <- line_angle - angle_rad
  
  # Calculate the points for the arrow sides
  arrowPoint1_lng <- to_lng - length * cos(arrow_angle1)
  arrowPoint1_lat <- to_lat - length * sin(arrow_angle1)
  
  arrowPoint2_lng <- to_lng - length * cos(arrow_angle2)
  arrowPoint2_lat <- to_lat - length * sin(arrow_angle2)
  
  return(list(
    arrowPoint1 = list(lng = arrowPoint1_lng, lat = arrowPoint1_lat),
    arrowPoint2 = list(lng = arrowPoint2_lng, lat = arrowPoint2_lat)
  ))
}

