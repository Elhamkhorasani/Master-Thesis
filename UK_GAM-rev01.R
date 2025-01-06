library(mgcv)
library(ggplot2)
library(grid)
library(animation)
theme_set(theme_bw())
library(dplyr)
library(tidymv)
library(ggformula)
#-------------------load the dataset-----------------------
load("C:\\Users\\Elhami\\Desktop\\UKload.rdata")
#--------------------EDA------------------------------------
library(ggplot2)
ggplot(UKload, aes(x = NetDemand)) +
  geom_histogram(fill = "darkred", color = "white", bins = 30) +
  labs(title = "Histogram of NetDemand",
       x = "NetDemand",
       y = "Frequency")

#------------------view class------------------------------
sapply(UKload, class) 

# ----------------Plotting --------------------------------
ggplot(UKload, aes(Date ,NetDemand)) +
  geom_line() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "NetDemand (kW)")

ggsave("NetDemand-Date.png")


#-------------hEATMAP--------------
# install.packages("ggplot2")
library(ggplot2)
library(reshape2)
library(tidyverse)

ggplot(UKload, aes(Posan ,NetDemand)) +
  geom_line() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Posan", y = "NetDemand (kW)")

ggplot(UKload, aes(wM ,NetDemand)) +
  geom_line() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "wM", y = "NetDemand (kW)")

ggplot(UKload, aes(Trend ,NetDemand)) +
  geom_line() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Trend", y = "NetDemand (kW)")

ggplot(UKload, aes(Holy ,NetDemand)) +
  geom_line() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Holy", y = "NetDemand (kW)")

ggplot(UKload, aes(Year ,NetDemand)) +
  geom_line() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Year", y = "NetDemand (kW)")

ggplot(UKload, aes(Dow ,NetDemand)) +
  geom_line() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Dow", y = "NetDemand (kW)")



gf_point(NetDemand ~ wM, data=UKload, alpha=0.4, color = "DARKred")
gf_point(NetDemand ~ Dow, data=UKload, alpha=0.4, color = "dARKred")
gf_point(NetDemand ~ Date, data=UKload, alpha=0.4, color = "DARKred")
gf_point(NetDemand ~ Year, data=UKload, alpha=0.4, color = "DARKred")
gf_point(NetDemand ~ Posan, data=UKload, alpha=0.4, color = "DARKred")
gf_point(NetDemand ~ Trend, data=UKload, alpha=0.4, color ="DARKred")
gf_point(NetDemand ~ Holy, data=UKload, alpha=0.4, color = "DARKred")
gf_point(NetDemand ~ NetDemand.48, data=UKload, alpha=0.4, color = "DARKred")





#------------------Data Preproccesing-------------------------------------------
UK<- tidyr::separate(UKload, Date, c("Date", "Time"), sep = " ")

library(lubridate)
library(forcats)

# Create a vector of French weekday names
french_weekdays <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")
# Convert the Dow column to lowercase for consistency
UK$Dow <- tolower(UKload$Dow)

# Convert the Dow column to numeric values from 1 to 7
UK$Dow <- match(UKload$Dow, french_weekdays)

# View the first few rows of the updated dataset
head(UK)

# Print the resulting data frame
print(UK)


UK $Date <- as.Date(as.factor(UKload $Date))
UK $Holy <- as.numeric(as.factor(UKload $Holy))
UK$Trend <- (UK$Trend - min(UK$Trend)) / (max(UK$Trend) - min(UK$Trend))
UK$NetDemand.48 <- (UK$NetDemand.48 - min(UK$NetDemand.48)) / (max(UK$NetDemand.48) - min(UK$NetDemand.48))
gf_point(NetDemand ~ Trend, data=UK, alpha=0.4, color ="DARKred")
gf_point(NetDemand ~ Dow, data=UK, alpha=0.4, color ="DARKred")
sapply(UK, class) 



install.packages("ggcorrplot")
# Calculate the correlation matrix
cor_matrix <- cor(select(UK, -c(Date,Time)))

# Print the correlation matrix
print(cor_matrix)

# Plot the correlation matrix using ggcorrplot
library(ggcorrplot)
ggcorrplot(cor_matrix, type = "lower", hc.order = TRUE, 
           colors = c("#6D9EC1", "white", "#E46726"))



#######################################TEST#####################################
"TEST" #Let’s look at some data chunk of electricity consumption .
test<- UK[c(1:200),c(1:10)]
dim(test)

ggplot(test, aes(Date, NetDemand)) +
  geom_line() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "NetDemand (kW)")

sapply(UKload, class) 
"GAM TEST"
gam_test <- gam(NetDemand ~ 
                  s(wM , k=5) 
                + s(Posan, k=5) 
                + s(Dow,k=7) 
                + s(Trend) 
                + s(NetDemand.48)  
                + Holy
                + Year
                ,
             data = test,
             family = gaussian)
summary (gam_test)   #provides information on the linear effects
summary(gam_test)$r.sq
k.check(gam_test)


# create the prediction
pred_test=predict.gam(gam_test,test,type='response')
# Create a new data table that includes the Data and the prediction
df0=data.frame(test,pred_test)
dev.off()

ggplot(df0, aes(Date, pred_test)) +
  geom_line() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "newNetDemand (kW)")

#-----------------------Plot of Predicted vs. Observed Values in ggplot2--------
library(tidyr)
library(dplyr)

D<- df0 %>%
  select(Date, NetDemand, pred_test) %>%
  gather(key = "variable", value = "value", -Date)
head(D, 3)

ggplot(D, aes(x = Date, y = value)) + 
  geom_line(aes(color = variable), size = 0.5) +
  scale_color_manual(values = c("#FC4E07", "#00AFBB")) +
  theme_minimal()

dim(UK)


################################################################################

#-----------------------Splitting data ----------------------------------------
#use 70% of dataset as training set and 30% as test set
train_uk<- UK[c(1:1400),c(1:10)]
dim(train_uk)
test_uk<- UK[c(1401:2008),c(1:10) ]
dim(test_uk)


#sample <- sample(c(TRUE, FALSE), nrow(UK), replace=TRUE, prob=c(0.7,0.3))
#train  <- UK[sample, ]
#test   <- UK[!sample, ]
#dim(train)
#dim(test)

sapply(train_uk, class) 
ggplot(train_uk, aes(Date, NetDemand)) +
  geom_line() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "NetDemand (kW)")

# -----------------Normalizing the NetDemand column-----------------------------
# UK$nNetDemand= scale(UKload$NetDemand)
# UK

#-------------------------GAM---------------------------------------------------
# wM, Posan, Dow, Trend, Date, Year, Holy, NetDemand.48 

gam_uk<- gam(NetDemand ~ 
               s(wM,bs='cs', k= 10 ) 
             + s(Posan,bs='cs', k=20 )
             + s(Dow,  bs='cs',k=7)
             + s(NetDemand.48, bs='cs', k=10)
             + Holy
             + Year,
             full = TRUE
             ,
             data = train_uk,
             method="REML",
             family = gaussian)
summary (gam_uk )   #provides information on the linear effects
summary(gam_uk )$r.sq
gam.check(gam_uk, rep=500)
qq.gam(gam_uk)
par(mfrow = c(2, 2))
plot(gam_uk , pages=1, scale=F, shade=T)

summary(gam_uk, full = TRUE)
k.check(gam_uk)
summary(gam_uk)$sp.criterion


library(gratia)
draw(b, scales="True")
# calculate the MSE and MAE of the full model
library(caret)
#MSE
mse_full <- mean(resid(gam_uk)^2)
print(mse_full)

#MAE
# Make predictions on the test set
predictions <- predict(gam_uk, newdata = test_uk)

# Calculate the MAE
mae <- MAE(predictions, test_uk$NetDemand)
mae

# lambda
GAM_UK$sp
GAM_UK$sp[which(names(GAM_UK$sp) == "s(wM)")]

#--------------------Predict using the GAM model--------------------------------
test_uk$pred <- predict(gam_opt, newdata = test_uk)

#-------------------------extract most of the data used to build the plot-------
library('gratia')
evaluate_smooth(gam_uk, "Dow")
#--------------------------------2D smoothers----------------------------------- 

# plot the smooth terms
plot.gam(gam_uk, pages = 1, all.terms = TRUE)

ggplot(train_uk, aes(x = Trend, fill = Trend)) +
  geom_density(alpha = 0.3)
#-------------------------Feature Importance------------------------------------
install.packages("mgcv")
library(mgcv)

# Fit the GAM model
gam_uk <- gam(NetDemand ~ 
                s(wM, bs='cs', k=10) +
                s(Posan, bs='cs', k=20) +
                s(Dow, bs='cs', k=7) +
                s(NetDemand.48, bs='cs', k=10) +
                Holy +
                Year,
              data = train_uk,
              method="REML",
              family = gaussian)

# Calculate the importance of each term
drop_results <- drop.term(gam_uk)

# Plot the results
plot(drop_results)


# Create a data frame of variable names and their corresponding importance
importance <- data.frame(var = names(gam_uk$coefficients), 
                         imp = abs(coef(gam_uk)))

# Sort the variables by their importance
importance <- importance[order(importance$imp, decreasing = TRUE), ]

# Create a bar plot of variable importance
ggplot(importance, aes(x = reorder(var, imp), y = imp)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance in GAM Model",
       x = "Variable",
       y = "Importance")


--------------------------------------------------------------------------------

# use gam.check to choose the optimal value of smoothing parameter
GAM_UK <- gam.check(GAM_UK, method = "REML", scale = 0, criterion = "AIC")


# extract the optimal value of smoothing parameter
opt_lambda <- gam_check$gcv.amin

# refit the model with the optimal value of smoothing parameter
gam_opt <- gam(NetDemand ~ s(wM, k= 10) 
               + s(Posan, k=20)
               + s(Dow, k=7)
               + s(NetDemand.48, k=20)
               + Holy
               + Year,
               data = train_uk,
               method="REML",
               family = gaussian,
               sp = opt_lambda)

summary (gam_opt)
summary(gam_opt )$r.sq
gam.check(gam_opt)
qq.gam(gam_opt)
par(mfrow = c(2, 2))
plot(gam_opt)
plot(gam_opt , pages=1, scale=F, shade=T)

summary(gam_opt, full = TRUE)
k.check(gam_opt )
summary(gam_opt )$sp.criterion

# lambda
GAM_UK$sp
gam_opt$sp

#----------------------------MSE------------------------------------------------
# Get lambda from gam models
opt_lambda <- GAM_UK$sp
gam_opt$sp
# Make predictions on validation set
predictions <- predict(GAM_UK, newdata = test_uk)
predictions1 <- predict(gam_opt, newdata = test_uk)
# Calculate mean squared error
mse <- mean((predictions - test_uk$NetDemand) ^ 2)
mse1 <- mean((predictions1 - test_uk$NetDemand) ^ 2)
mse
mse1


#-----------------------------------AIC----------------------------------------
AIC(GAM_UK)
AIC(gam_opt)


#-----------------------------------MAE-----------------------------------------
# predict on test data
test_pred <- predict(gam_opt, newdata = test_uk)

# calculate MAE
MAE_opt <- mean(abs(test_pred - test_uk$NetDemand))
print(MAE_opt)

# Assuming test_uk contains the test data
y_pred <- predict(GAM_UK, newdata = test_uk)
mae_uk <- mean(abs(y_pred - test_uk$NetDemand))
print(mae_uk)



#--------------------------Compare predicted vs actual values-------------------
# Predict using the GAM model
test_uk$pred <- predict(gam_opt, newdata = test_uk)

# Compare predicted vs actual values

ggplot(test_uk, aes(x = NetDemand, y = pred)) + 
  geom_point(color = "dARKred") + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1) + 
  labs(x = "Actual Values", y = "Predicted Values", 
       title = "Comparison of Predicted vs Actual Values")


# create the prediction
NetDemand=predict.gam(gam_opt,train_uk,type='response')
# Create a new data table that includes the Data and the prediction
big_data=data.frame(train_uk,NetDemand)
dev.off()
library(tidyr)
library(dplyr)

datas<- big_data %>%
  select(Date, NetDemand, NetDemand.1) %>%
  gather(key = "variable", value = "Value", -Date)

ggplot(datas, aes(x = Date, y = Value)) + 
  geom_line(aes(color = variable), size = 0.5) +
  scale_color_manual(values = c("#0d88e6", "darkred")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "gray90", color = NA),
        panel.grid.major = element_line(color = "white"))


# Save the prediction to a new file
write.csv(big_data, "C:/Users/Elhami/GAM1Output.csv")



#------------------------GAM model with GCV-------------------------------------

gam_gcv<- gam(NetDemand ~ 
                s(wM, k= 10) 
              + s(Posan, k=20)
              + s(Dow, k=7)
              + s(NetDemand.48, k=10)
              + Holy
              + Year,
              full = TRUE
              ,
              data = train_uk,
              method="GCV.Cp",
              family = gaussian)

summary (gam_gcv)   #provides information on the linear effects
summary(gam_gcv)$r.sq
gam.check(gam_gcv)
qq.gam(gam_gcv)
par(mfrow = c(2, 2))
plot(gam_gcv , pages=1, scale=F, shade=T)

# use gam.check to obtain the GCV plot
gcv_plot <- gam.check(gam_gcv, method = "GCV")


# use gam.check to choose the optimal value of smoothing parameter
gam_check <- gam.check(gam_gcv, method = "GCV.Cp", scale = 0, criterion = "AIC")

# extract the optimal value of smoothing parameter
opt_lambda <- gam_check$gcv.amin

# refit the model with the optimal value of smoothing parameter
gam_opt <- gam(NetDemand ~ s(wM, k= 10) 
               + s(Posan, k=20)
               + s(Dow, k=7)
               + s(NetDemand.48, k=10)
               + Holy
               + Year,
               data = train_uk,
               method="REML",
               family = gaussian,
               sp = opt_lambda)

summary (gam_opt)   #provides information on the linear effects
summary(gam_opt)$r.sq
gam.check(gam_opt)
qq.gam(gam_opt)
par(mfrow = c(2, 2))
plot(gam_opt , pages=1, scale=F, shade=T)

#---------------------# perform 10-fold cross-validation on both models------------------------------------
library(mgcv)

# create a list of models to compare
models <- list(gam_opt, GAM_UK)
model_names <- c("gam_opt", "GAM_UK")

# set up cross-validation
cv <- cv.gam(train_uk, models, K = 10, method = "GCV.Cp", type = "response")

# plot the cross-validation results
plot(cv)

# extract the root mean squared errors (RMSEs) for each model
rmse <- sapply(cv$cv.error, function(x) sqrt(mean(x^2)))

# print the RMSEs for each model
print(rmse)

# find the best model
best_model <- model_names[which.min(rmse)]

# print the name of the best model
print(best_model)



















set.seed(123)
cv_opt <- cv.gam(gam_opt, K = 10, method = "GCV.Cp", type = "response")
cv_uk <- cv.gam(GAM_UK, K = 10, method = "REML", type = "response")

# calculate the root mean squared error (RMSE) for both models
rmse_opt <- sqrt(mean(cv_opt$cv.errors^2))
rmse_uk <- sqrt(mean(cv_uk$cv.errors^2))

# compare the RMSE of the two models
if (rmse_opt < rmse_uk) {
  # the first model with optimal lambda is better
  best_model <- gam_opt
} else {
  # the second model with GCV is better
  best_model <- GAM_UK
}


#-------------the model I used first in my thesis--------------
gam_3 <- gam(NetDemand ~ 
               s(wM )
             + s(Posan ,k=20) 
             + s(Dow ,k=7)
             + s(Trend , k=10)
            + Holy + Year,
             full = TRUE,
             data = train_uk,
             method="REML",family = gaussian)

summary (gam_3)   #provides information on the linear effects
summary(gam_3)$r.sq
gam.check(gam_3 )
qq.gam(gam_3 )
par(mfrow = c(2, 2))
plot(gam_3  , pages=1, scale=F, shade=T)
k.check(gam_3  )


#-----------------------------------MSE-----------------------------------------
# generate predicted values
y_pred <- predict(GAM_UK, newdata = test_uk)
y_pred1 <- predict(gam_gcv, newdata = test_uk)

# calculate MSE
mse <- mean((test_uk$NetDemand - y_pred)^2)
mse1 <- mean((test_uk$NetDemand - y_pred1)^2)
# print MSE
cat("MSE:", mse, "\n")
cat("MSE:", mse1, "\n")

#-----------------------------------AIC----------------------------------------
AIC(GAM_UK)
AIC(gam_gcv)


AIC(GAM_UK, gam_gcv)
anova(GAM_UK, gam_gcv, test = "Chisq")
#-----------------------------------RMSE----------------------------------------
# assume y_pred is the predicted response and y_obs is the observed response
y_pred <- predict(GAM_UK, newdata = test_uk)
y_obs <- test_uk$NetDemand

# calculate RMSE
RMSE <- sqrt(mean((y_pred - y_obs)^2))
RMSE1 <- sqrt(mean((y_pred1 - y_obs)^2))

# Print the RMSE value
print(paste0("RMSE: ", RMSE))
print(paste0("RMSE: ", RMSE1))


# generate predicted values using the trained GAM model
pred_uk <- predict(GAM_UK, newdata = test_uk)

# plot predicted values against actual values
plot(test_uk$NetDemand, pred_uk, main = "Predicted vs Actual NetDemand",
     xlab = "Actual NetDemand", ylab = "Predicted NetDemand")
abline(a = 0, b = 1, col = "red")




#--------------------------Plotting gam_1------------------------
require(rgl)
logLik.gam(GAM_UK )
plot(GAM_UK , pages=1, scale=F, shade=T)
# plot3d(gam_1)
plot(train_uk$NetDemand,fitted(GAM_UK))
plot(train_uk$NetDemand,fitted(gam_opt))
#--------------------------Is our model actually Normal?------------------------
par(mfrow = c(2, 2))
plot(GAM_UK , pages=1, scale=F, shade=T)
#In practice, the exact choice of k is arbitrary, but it should be large enough 
#to accommodate a sufficiently complex smooth function.
#k: basis dimension - Determines the maximum number of base functions used to build
#the curve. - Sets the wiggliness of a smooth, in a trade-off with the smoothing
#parameter.
"k"
# - The  k should always be less than the number of unique data points. 
#- The complexity (i.e.non-linearity) of a smooth function in a fitted model is
#reflected by its effective degrees of freedom (EDF).

"EDF"
# #When a term has an EDF value that is close to 1, it is close to being a linear
# term. Higher values indicate that the term’s spline is more wiggly, or in 
# otherwords, highly non-linear.

gam1 <- predict(GAM_UK )
gam1


# create the prediction
NetDemand=predict.gam(GAM_UK,train_uk,type='response')
# Create a new data table that includes the Data and the prediction
big_data=data.frame(train_uk,NetDemand)
dev.off()

ggplot(big_data, aes(Date, NetDemand.1)) +
  geom_line() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "newNetDemand (kW)")


rsd <- residuals(GAM_UK )
qq.gam(GAM_UK ,rep=100); plot(fitted(GAM_UK ),rsd)
plot(big_data$Date,rsd); plot(big_data$Holy,rsd)
#-----------------------Plot of Predicted vs. Observed Values in ggplot2--------
library(tidyr)
library(dplyr)

datas<- big_data %>%
  select(Date, NetDemand, NetDemand.1) %>%
  gather(key = "variable", value = "value", -Date)
head(datas, 3)

ggplot(datas, aes(x = Date, y = value)) + 
  geom_line(aes(color = variable), size = 0.5) +
  scale_color_manual(values = c("#FC4E07", "#00AFBB")) +
  theme_minimal()


# Save the prediction to a new file
write.csv(big_data, "C:/Users/Elhami/GAM1Output.csv")

gam.check(GAM_UK)



vis.gam(gam_1 ,type = 'response', plot.type = 'contour', color="terrain")
vis.gam(
  GAM_UK,
  type      = 'response',
  plot.type = 'persp',
  color="terrain",
  phi       = 30,
  theta     = 30,
  n.grid    = 500,
  border    = NA
)

sapply(UK, class) 





#-------------------------model_2----------------------------------------------
#-------------------------GAM------------------------------
# wM, Posan, Dow, Trend, Date, Year, Holy, NetDemand.48 

gam <- gam(NetDemand ~ Dow + s(wM) +  s(Posan ) + Holy,
              full = TRUE
              ,
              data = train_uk,
              method="REML",
              family = gaussian)
summary (gam  )   #provides information on the linear effects
summary(gam  )$r.sq
gam.check(gam )
qq.gam(gam )
par(mfrow = c(2, 2))
plot(gam  , pages=1, scale=F, shade=T)
k.check(gam  )
summary(gam  )$sp.criterion
qq(gam , rep = 100, level = .9, CI = "quantile")

#------------------------- model 2 with interaction-----------------------------

gam_2 <- gam(NetDemand ~ 
               s(wM) 
             + s(Posan) 
             + s(Dow, K=7)
             + s(Trend) 
             + s(NetDemand.48)  
             + Holy
             + Year
          
           ,
          full = TRUE,
            data = train_uk,
            family = gaussian)

summary (gam_2)
gam.check(gam_2)
gam_summary$p.table
summary(gam_2)$r.sq
summary(gam_2)$sp.criterion

par(mfrow = c(2, 2))
plot(gam_2, pages=1, scale=F, shade=T)
gam.check(gam_2)

layout(matrix(1:2, nrow = 1))
plot(gam_2, rug = FALSE, se = FALSE, n2 = 80, main = "gam n.4 with te()")

"contour lines and corresponding numbers on them represents effects of ind.
variables to the response variable"

gam2 <- predict(gam_2)
gam2

# create the prediction
NetDemand.2=predict.gam(gam_2,train,type='response')
# Create a new data table that includes the Data and the prediction
big_data_2=data.frame(train,NetDemand.2)
dev.off()

ggplot(big_data_2, aes(Date, NetDemand.2)) +
  geom_line() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "newNetDemand (kW)")

#-----------------------Plot of Predicted vs. Observed Values in ggplot2--------
library(tidyr)
library(dplyr)

datas_2<- big_data_2 %>%
  select(Date, NetDemand, NetDemand.2) %>%
  gather(key = "variable", value = "value", -Date)
head(datas_2, 3)

ggplot(datas_2, aes(x = Date, y = value)) + 
  geom_line(aes(color = variable), size = 0.5) +
  scale_color_manual(values = c("#FC4E07", "#00AFBB")) +
  theme_minimal()


# Save the prediction to a new file
write.csv(big_data, "C:/Users/Elhami/GAM2Output.csv")

gam.check(gam_1)


vis.gam(gam_2, main = "t2(D, W)", plot.type = "contour",
        color = "terrain", contour.col = "black", lwd = 2)
vis.gam(gam_2 ,type = 'response', plot.type = 'contour', color="terrain")
vis.gam(
  gam_2,
  type      = 'response',
  plot.type = 'persp',
  color="terrain",
  phi       = 30,
  theta     = 30,
  n.grid    = 500,
  border    = NA
)





AIC(gam_1, gam_2)
anova(gam_1, gam_2, test = "Chisq")
gam.check(gam_1)
gam.check(gam_2)
























#--------------------gamm, which stands for GAM mixture models.-----------------

# gam_1_ar0 <- gamm(NetDemand ~                
#                     s(wM , k=20) 
#                   + s(Posan , bs= "cc" ,k=100)
#                   + s(Dow  ,k=7) 
#                   + s(Trend , k=5) 
#                   + s(NetDemand.48 , k=5)
#                   + te (Trend, NetDemand.48)
#                   + Holy
#                   + Year
#                   + Date,
#                   full = TRUE
#                   ,
#                   data = train,
#                   family = gaussian,
#                   method = "REML")
# 
# 
# gam_1_ar1 <- gamm(NetDemand ~                
#                     s(wM , k=20) 
#                   + s(Posan , bs= "cc" ,k=100)
#                   + s(Dow  ,k=7) 
#                   + s(Trend , k=5) 
#                   + s(NetDemand.48 , k=5)
#                   + te (Trend, NetDemand.48)
#                   + Holy
#                   + Year
#                   + Date,
#                   full = TRUE
#                   ,
#                   data = train,
#                   family = gaussian,
#                   correlation = corARMA(form = ~ 1|wM , P=1),
#                   method = "REML")
# 
# anova(gam_1_ar0$lme, gam_1_ar1$lme)
# 





#----------------------model 3  with interaction--------------------------------

gam_3 <- gam(NetDemand ~ 
              s(wM)
             + s(Posan , k=20)
             + s(Dow , k=7)
             + s(Trend)
             + Holy
             ,
             data = train,
             family = gaussian)
summary (gam_3)
gam_3_summary <- summary(gam_2)
gam_3_summary$p.table

par(mfrow = c(1, 6))
plot(gam_3, all.terms = TRUE)

summary(gam_3)$r.sq
par(mfrow = c(2, 2))
gam.check(gam_3)

plot(gam_3, page = 1, scheme = 2)
vis.gam(gam_3, view = c("wM", "Holy"),
        theta = 150, n.grid = 50, lwd = 0.4 , color = "cm")
#"topo", "heat", "cm", "terrain", "gray" or "bw". 



layout(matrix(1:4, nrow = 1))
plot(gam_1, rug = FALSE, se = FALSE, n2 = 80, main = "gam n.1")
plot(gam_2, rug = FALSE, se = FALSE, n2 = 80, main = "gam n.2 ")

#---------------------Model 4 with numeric Date---------------------------
# 
# train1 <- train
# test1 <- test
# 
# sapply(train1, class) 
# train1 $Date <- as.numeric(as.factor(train1 $Date))
# sapply(train1, class) 
# 
# sapply(train1, class) 
# plot(train1$NetDemand, type = 'l')
# 
# gam_4 <- gam(NetDemand ~ 
#                s(wM)
#              + s(Posan, k=25)
#              + t2(Trend, Dow )
#              + s(Dow , k=7)
#              + s(Trend)
#              + s (NetDemand.48)
#              + Holy
#              + s(Year, bs= "cr" , k=5)
#              + Date,
#             
#              full = TRUE
#              ,
#              data = train1,
#              family = gaussian)
# summary (gam_4)   #provides information on the linear effects
# summary(gam_4)$r.sq
# k.check(gam_4)
# summary(gam_4)$sp.criterion
# par(mfrow = c(1, 2))
# plot(gam_4, all.terms = TRUE)
# 
# 
# gam_5 <- gam(NetDemand ~ s(wM)
#              + s(Dow,  k = 7) 
#              + s(Posan, k = 5) 
#              + te(Dow, Posan)
#              + s(Year , k=5)
#              + s(Trend)
#              + te(Trend , Year),
#              data = train,
#              family = gaussian)
# 
# summary (gam_5)   #provides information on the linear effects
# summary(gam_5)$r.sq
# k.check(gam_5)
# summary(gam_5)$sp.criterion
# par(mfrow = c(1, 2))
# plot(gam_5, all.terms = TRUE)





# We can see that gam_1 has the lowest AIC value.
 # summary(gam_1)
 # layout(matrix(1:2, nrow = 1))
 # plot(gam_1, shade = TRUE)
 # dev.off()
# head(predict(gam1))
# predict<-predict(gam1, type = "terms")
# predict
# # what to look at: summary: EDF, p-values, R^2, GCV; AIC, magic
# summary(gam1)
# # Parametric coefficients:
# summary(gam1)$p.table
# # smooth terms:
# summary(gam1)$s.table
# # R^2:
# summary(gam1)$r.sq
# # GCV:
# summary(gam1)$sp.criterion
# # AIC:
# gam1$aic
# # BIC:
# BIC(gam1)r
























