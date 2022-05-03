# Specificing selecting parameters used in test set

# Libraries
library(caret)
library(glmnet)
library(pracma)
library(dplyr)
library(COUNT)


rm(list=ls())

setwd('C:/Users/conno/Documents/School work/STA 5900/Project 2')

df <- read.csv('taipei_train.csv')

df.test <- read.csv('taipei_test.csv')



# selecting data that should help with answering our client question

df.new <- df[which(df$building_area<=200 &
                     df$building_age<=1974 &
                     df$building_age>=1924 &
                     df$num_toilet<=2 &
                     df$num_hall>=0 &
                     df$num_hall<=2 &
                     df$num_room<=4 &
                     df$num_room>=2 &
                     df$land_area<=80 &
                     df$total_ntd<=3e07
                   ),
             ]

df.new <- subset(df.new,df.new$main_building_material=='Reinforced concrete construction' |
                       df.new$main_building_material=='Strengthen brickwork')

df.new <- subset(df.new,df.new$district!='Zhongzheng District' &
                   df.new$district!='Nangang District' &
                   df.new$district!='Datong District')



# Making certain variables categorical
df.new$district <- as.factor(df.new$district)
district.name <- unique(df.new$district)
levels(df.new$district) <- district.name

df.new$main_building_material <- as.factor(df.new$main_building_material)
material.name <- unique(df.new$main_building_material)
levels(df.new$main_building_material) <- material.name


# Creating prediction function
prediction_plot <- function(data, name_y, color_by = FALSE, m = 2, n=2){
  palette_old <- palette()
  palette(c("blue2","chocolate2","violetred4","aquamarine4","darkgoldenrod4","mediumorchid3","seagreen4","tan4","yellowgreen"))
  omfrow <- par()$mfrow
  par(mfrow = c(m,n))
  if(color_by != FALSE){
    col_ind <- which(names(data) == color_by)
    cols <- as.numeric(data[,col_ind])
  } else{
    cols <- 1
  }
  y_ind <- which(names(data) == name_y)
  y <- data[,y_ind]
  data <- data[which(is.na(y) == FALSE),]; y <- data[,y_ind]
  I <- (1:ncol(data))[-y_ind]
  for(i in I){
    name_x = names(data)[i]
    x <- data[,i]
    plot(x, y,pch=16,cex=0.75,
         xlab = name_x,
         ylab = name_y,
         col = cols)
    if(is.numeric(x)){
      
      df <- data.frame(x,y)
      df$loess <- predict(loess(y~x,df))
      df <- df[order(x),]
      lines(df$x, df$loess, col="dodgerblue",lwd=2)
    }
  }
  par(mfrow=omfrow)
  palette(palette_old)
}

prediction_plot(df.new,'total_ntd',
                m=2,
                n=2,
                color_by='district'
                )


# Making plots
nam <- levels(df.new$district)
clip <- c(1:length(nam))
joke <- 1


for (i in nam) {
  bluh <- median(df.new$total_ntd[df.new$district==i])
  clip[joke] <- bluh
  joke <- joke+1
}

high_dist <- nam[c(2,5)]
low_dist <- nam[6]

# Prepare a vector of colors with specific color for Nairobi and Eskimo
myColors <- ifelse(levels(df.new$district)==high_dist[1] , rgb(0.1,0.1,0.7,0.5) , 
                   ifelse(levels(df.new$district)==high_dist[2] , rgb(0.1,0.1,0.7,0.5) , 
                   ifelse(levels(df.new$district)==low_dist, rgb(0.8,0.1,0.3,0.6),
                          "grey90" ) ) )


plot(df.new$district, df.new$total_ntd,pch=16,cex=0.75,
     xlab = 'District',
     ylab = 'Price (NTD)',
     names=c('Beitou','Wenshan','Daan','Neihu','Shihlin',
             'Wanhua','Songshan','Xinyi','Zongshan'),
     main='Average Prices of Properties in Each District',
     col=myColors)


# Materials

nam2 <- levels(df.new$main_building_material)
clip <- c(1:length(nam2))
joke <- 1

for (i in nam2) {
  bluh <- median(df.new$total_ntd[df.new$main_building_material==i])
  clip[joke] <- bluh
  joke <- joke+1
}

myColors <- ifelse(levels(df.new$main_building_material)==nam2[1] , rgb(0.1,0.1,0.7,0.5) , 
                   ifelse(levels(df.new$main_building_material)==nam2[2], rgb(0.8,0.1,0.3,0.6),
                          "grey90" ) )

plot(df.new$main_building_material, df.new$total_ntd,pch=16,cex=0.75,
     xlab = 'Building Material',
     ylab = 'Price (NTD)',
     main='Average Prices of Properties With Each Material',
     col=myColors)

# Number of Rooms

nam3 <- unique(df.new$num_room)
clip <- c(1:length(nam3))
joke <- 1

for (i in nam3) {
  bluh <- mean(df.new$total_ntd[df.new$num_room==i])
  clip[joke] <- bluh
  joke <- joke+1
}

myColors <- ifelse(df.new$num_room==nam3[1] , rgb(0.1,0.1,0.7,0.5) , 
                   ifelse(df.new$num_room==nam3[3], rgb(0.8,0.1,0.3,0.6),
                          "green" ) )

plot(df.new$num_room, df.new$total_ntd,pch=16,cex=0.75,
     xlab = 'Number of Rooms',
     ylab = 'Price (NTD)',
     main='Average Prices of Properties With Rooms',
     col=myColors)
points(clip,cex=1.25,lwd=2,col='black',pch=19)



mlr <- lm(nthroot(total_ntd^3,5)~district+
            land_area+
            main_building_material+
            district*main_building_material+
            main_building_material*building_area+
            building_area+
            num_room+
            num_hall+
            num_toilet+
            transaction_year+
            transaction_month+
            building_age+
            num_room*building_age+
            num_hall*num_room,
          data=df.new)

mlr_resid <- resid(mlr)

df.new$mlr_resid <- mlr_resid

prediction_plot(df.new,'mlr_resid',
                m=2,
                n=2
                )

# Utilize Cook's distance to remove outliers
cooksD <- cooks.distance(mlr)
influential <- cooksD[(cooksD > (3*mean(cooksD, na.rm=TRUE)))]

names.influential <- names(influential)
outliers <- df.new[names.influential,]
df.new_out <- df.new %>% anti_join(outliers)

# Step 2 plug into Cross Validation
  # LOOCV
train_control <- trainControl(method='LOOCV')
model <- train(nthroot(total_ntd^3,5)~district+
                 land_area+
                 main_building_material+
                 district*main_building_material+
                 main_building_material*building_area+
                 building_area+
                 num_room+
                 num_hall+
                 num_toilet+
                 transaction_month+
                 building_age+
                 num_room*building_age+
                 num_hall*num_room,
               data=df.new_out,
               trControl=train_control,
               method='lm'
                )
  # K-Fold
num <- c(1:10)
for (i in num) {
  
train_control2 <- trainControl(method='repeatedcv', 
                               number=5,
                               repeats=5,
                               savePredictions='all',
                               classProbs =TRUE)

model2 <- train(nthroot(total_ntd^3,5)~district+
                 land_area+
                 main_building_material+
                 district*main_building_material+
                 main_building_material*building_area+
                 building_area+
                 num_room+
                 num_hall+
                 num_toilet+
                 transaction_month+
                 building_age+
                 num_room*building_age+
                 num_hall*num_room,
               data=df.new_out,
               trControl=train_control2,
               method='lm'
                )



# RMSE of k-fold
z <- model2$pred$obs
zhat <- model2$pred$pred
y <- nthroot(z^5,3)
yhat <- nthroot(zhat^5,3)
resid_sqrt <- y - yhat
MSE <- mean(resid_sqrt^2)
RMSE <- sqrt(MSE);RMSE
num[i] <- RMSE

}
min(num)
print(num)

# nthroot RMSE Equation for LOOCV
z <- model$pred$obs
zhat <- model$pred$pred
y <- nthroot(z^5,3)
yhat <- nthroot(zhat^5,3)
resid_sqrt <- y - yhat
MSE <- mean(resid_sqrt^2)
RMSE <- sqrt(MSE);RMSE

RMSE_Lowest <- 3079797

model_resid <-  resid(model)
df.new_out$model_resid <- model_resid

prediction_plot(df.new_out,'model_resid',
                m=2,
                n=2,
                color_by='main_building_material'
                )

# these are out predicted values
predictions <- nthroot(predict(model2,df.test)^5,3)
print(predictions)

# Selecting data
datata <- subset(df,df$district=='Zhongshan Area' & 
                   df$building_area >=145 & 
                   df$building_area <=157 &
                   df$land_area >=28 &
                   df$land_area <= 36
                 )
write.csv(datata,file='datata10.csv',row.names=FALSE)  
predictions[10]
