# 台灣地理中心經緯度  c(120.97967, 23.97379)
# example
station_location <- c(121.88569, 24.92528)

mid_station_angle <- function(station_location){
  mid_location <- c(120.97967, 23.97379)
  x <- station_location[1] - mid_location[1]
  y <- station_location[2] - mid_location[2]
  z = sqrt(x* x + y * y)
  angle <- ifelse(x >= 0 , 
                  ifelse(y >= 0, asin(y/z)/pi*180, 360 + asin(y/z)/pi*180), 
                  ifelse(y >= 0, 180 - asin(y/z)/pi*180, 180 - asin(y/z)/pi*180))
  return(angle)
}

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(700)

#### featuring ####
train = read.csv('./train.csv')
train %>% group_by(Station) %>% count() %>% filter( n == 3)
# M17 is not full season
which(train$Station == "M17")
train <- train[-c(101:103),]
train_num <- sort(sample(1:(nrow(train)/4), (nrow(train)/4)*0.75))
train_num <- c(train_num*4-3, train_num*4-2, train_num*4 -1, train_num*4)%>%sort()
train_num
nrow(train)/4
length(train_num)/4
test <-  train[-train_num,]
train <- train[train_num,]
angle_level <-train %>% rowwise() %>%
  mutate(angle = mid_station_angle(c(Lon,Lat))) %>%
  select(Season, angle, LEVEL)
angle_level_s1 <- angle_level %>% filter(Season == 1)
angle_level_s2 <- angle_level %>% filter(Season == 2)
angle_level_s3 <- angle_level %>% filter(Season == 3)
angle_level_s4 <- angle_level %>% filter(Season == 4)
library(ggplot2)
qplot(angle_level$angle, angle_level$LEVEL,
      colour = factor(angle_level$Season), geom = c("line"))


preprocessing <- function(origin_data){
  mid_location <- c(120.97967, 23.97379)
  x <- station_location[1] - mid_location[1]
  y <- station_location[2] - mid_location[2]
  z = sqrt(x* x + y * y)
  angle <- ifelse(x >= 0 , 
                  ifelse(y >= 0, asin(y/z)/pi*180, 360 + asin(y/z)/pi*180), 
                  ifelse(y >= 0, 180 - asin(y/z)/pi*180, 180 - asin(y/z)/pi*180))
  return(angle)
}


