aggtrgate_cars_data <- function(){
 dt <- data.table(name = c("Ford Mst", "Toyota Clr", "Honda CVC"))
 # Regex Lookahead
 dt <- dt[, manufacturer := str_extract(name, '.+(?=\\s)')]
 # Regex Lookbehind
 dt <- dt[, model := str_extract(name, '(?<=\\s).+')]
 
 dt2 <- data.table(name = c("Ford Mst 1994", "Toyota Clr 2018", "Honda CVC 2020"))
 dt2 <- dt2[]
# Regex
 sat_img <- "lat_38.43122-lng_102.32445.jpg"
 sat_lat <- stringr::str_extract(sat_img, "(?<=lat\\_).+(?=\\-lng)")
 
 }
