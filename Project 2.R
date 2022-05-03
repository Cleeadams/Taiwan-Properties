# STA 5900 Project 2


rm(list=ls())

setwd('C:/Users/conno/Documents/School work/STA 5900/Project 2')

tab <- read.csv('taipei_train.csv')

tab2 <- tab[which(tab$land_area<=400),]

library(caret)
library(glmnet)
library(pracma)
library(dplyr)

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

# Step 1 design model
tab$district <- as.factor(tab$district)
district.name <- unique(tab$district)
levels(tab$district) <- district.name

tab$main_building_material <- as.factor(tab$main_building_material)
material.name <- unique(tab$main_building_material)
levels(tab$main_building_material) <- material.name

tab$transaction_month <- as.factor(tab$transaction_month)
levels(tab$transaction_month) <- c(1:12)



# Prediction plot
prediction_plot(tab_no_ntd_outliers,'total_ntd',m=2,n=2,
                color_by='district')

# Simple MLR of entire data set
model_preview <- lm(total_ntd~.,
                    data=tab)

# Plot model
plot(model_preview)

  # Notice a lot of outliers in residual vs. leverage

# Utilize Cook's distance to remove outliers
cooksD <- cooks.distance(model_preview)
influential <- cooksD[(cooksD > (3*mean(cooksD, na.rm=TRUE)))]
influential

names.influential <- names(influential)
outliers <- tab[names.influential,]
tab_no_outliers <- tab %>% anti_join(outliers)

# Preview new model
model_preview2 <- lm(sqrt(total_ntd)~district+
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
                    data=tab_no_outliers)

# Plot model
plot(model_preview2)


# Step 2 plug into k-fold

train_control <- trainControl(method='LOOCV')
model <- train(sqrt(total_ntd)~district+
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
               data=tab_no_ntd_outliers,
               trControl=train_control,
               method='lm'
               )

# SQRT RMSE Equation
z <- model$pred$obs
zhat <- model$pred$pred
y <- z^2
yhat <- zhat^2
resid_sqrt <- y - yhat
MSE <- mean(resid_sqrt^2)
RMSE <- sqrt(MSE);RMSE

# LN RMSE Equation
z <- model$pred$obs
zhat <- model$pred$pred
y <- exp(z)
yhat <- exp(zhat)
resid_sqrt <- y - yhat
MSE <- mean(resid_sqrt^2)
RMSE <- sqrt(MSE);RMSE

# RMSE sqrt
RMSE_sqrt <- 4787981; RMSE_sqrt

# RMSE sqrt, month is categorical, 
# interaction: room*age and district*material 
RMSE_sqrt_month_cat <- 4731475

# RMSE sqrt, month is categorical, 
# interaction: room*age, room*hall and district*material 
RMSE_sqrt_roomhall <- 4724086

# RMSE sqrt, month is categorical, 
# interaction: room*age, room*hall and district*material
# material*buildingarea
RMSE_Lowest <- 4682459

# RMSE of normal lm but using cooks distance of tab 1*mean
RMSE_cook1 <- 4318300

# RMSE of lowest lm but using cooks distance of tab 1*mean
RMSE_cook2 <- 3382364

# RMSE of lowest lm but using cooks distance of tab 3*mean
RMSE_cook3 <- 3936077

RMSE_sqrt-RMSE
RMSE_Lowest - RMSE



print(model)
summary(model)

# New stuff
resid.model <- resid(model)
tab$resid <- resid.model

prediction_plot(tab, 'resid',
                m=1,
                n=1
)






