# Function to convert squares to lat/lon coordinates
ices.rect <- function(rectangle){
  if(is.factor(rectangle)) rectangle <- as.character(rectangle)
  
  lat <- function(r){
    split <- unlist(strsplit(as.character(r),''))
    if(split[1] %in% 0:9 & split[2] %in% 0:9 & split[3] %in% LETTERS & split[4] %in% 0:9)
      ry <- as.numeric(substr(r, 1, 2)) else
        ry <- as.numeric(r) / 10^floor(log10(as.numeric(r)) - 1)
      return((ry + 71.5) / 2)
  }
  
  lon <- function(r){
    split <- unlist(strsplit(as.character(r),''))
    if(split[1] %in% 0:9 & split[2] %in% 0:9 & split[3] %in% LETTERS & split[4] %in% 0:9)
      rx <- as.numeric(paste(match(split[3], LETTERS), split[4], sep = '')) else
        rx <- floor(log10(as.numeric(r)) - 1) + 50
      return(rx - 59.5)
  }
  
  data.frame(lon = unlist(lapply(rectangle, lon)), lat = unlist(lapply(rectangle, lat)))
}