#=============================================================================================
# Studying the effect of sample size on the test error
#=============================================================================================

#DATA:
library(ggplot2)
data("diamonds")
set.seed(0)
rand = sample(1:nrow(diamonds),38000)
rand_test = sample(1:nrow(diamonds),50)
train = diamonds[rand, c("carat","price") ]
test = diamonds[rand_test, c("carat","price") ]

# FITTING A POLYNOMIAL REGRESSION OF ORDER 7

m = price ~  carat + I(carat^2) + I(carat^3) + I(carat^4) + I(carat^5) + I(carat^6) + I(carat^7)
nte <- data.frame("SampleSize" = character(0), "TestError" = character(0),"TrainError" = character(0), stringsAsFactors = FALSE)


for ( i in c(10,20,30,50,70,100, 200, 350)){
  # Creatingsample
  set.seed(0)
  s = diamonds[sample(1:nrow(train), i), c("carat","price") ] 
  
  #Creating model
  mod = lm(m, s)
  
  #PLOTTING THE MODEL OVER THE DATA
  plot(s$carat,s$price, pch=19, cex=0.5, xlab = "Carat", ylab = "Price", main =paste("  Price vs Carat (degree 7) 
    sample size = ", i, sep = " "))
  lines(sort(s$carat), fitted(mod)[order(s$carat)], col='brown', type='l',pch=20) 
  
  
  #TRAIN AND TEST ACCURACY
  train_error <- sum(mod$residuals^2)
  pred = predict(mod, newdata=test)
  test_error <- sum((pred-test$price)^2)
  
  
  # Creating a data frame for SampleSize and TestError
  nte1  <- data.frame("SampleSize" = i, "TestError" = test_error, "TrainError" = train_error)
  nte<- rbind(nte, nte1)
  
}


plot(nte$SampleSize, nte$TestError, pch = 19, cex = 0.5,xlab= "Sample Size", ylab= "Test Error", main = "Sample Size vs Test Error")
lines(nte$SampleSize, nte$TestError, type = "l")

#========================================================================================================
# Studying the effect of model complexity on Test and TrainError
#========================================================================================================

# Divide the cars into equal parts
mysample<-split(sample(nrow(train)), 1:4)
s1 <- train[sample(mysample[[1]], 50), ]
s2<- train[sample(mysample[[2]],50), ]
s3 <- train[sample(mysample[[3]],50), ]
s4 <- train[sample(mysample[[4]], 50), ]

# For sample size = 100
s5 <- train[sample(mysample[[1]], 100), ]
s6<- train[sample(mysample[[2]],100), ]
s7 <- train[sample(mysample[[3]],100), ]
s8 <- train[sample(mysample[[4]], 100), ]

samples = list(s1,s2,s3,s4,s5,s6,s7,s8)
mctesam = list()

for (d in 1:length(samples)){
  mc_te <- data.frame("Model_Complexity" = character(0), "Test_RSS" = character(0), "Train_RSS" = character(0))
  
for (i in c(1,2,7,8,9,10)) {
  predictors =  I(0)
  for (j in 1:i){
    predictors = cbind(predictors, I((samples[d]$carat)^j))
    
  }
  m = lm(samples[d]$price ~  predictors)
  #setwd("C:\\Users\\Mirra\\Desktop\\Python_GN\\ML2_AssignmentPlots")
  #png(filename = paste("Sample",samples[d], "Order", i,".png", sep=""))
  
  plot(samples[d]$carat,samples[d]$price, pch=19, cex=0.5, xlab = "Carat", ylab = "Price", main = paste("Price vs Carat for Order",i))
  lines(sort(samples[d]$carat), fitted(m)[order(samples[d]$carat)], col='red', type='l') 
  
  train_err = sum(m$residuals^2)
  pred = predict(m, newdata=test)
  test_error = sum((pred-test$price)^2)
  mc_te_1 <- data.frame("Model_Complexity" = i, "Test_RSS" = test_error, "Train_RSS" = train_err)
  mc_te <- rbind(mc_te,mc_te_1)
  
}
  mctesam[[d]]<-mcte
}  


for  (z in (1:length(mctesam))){
for (i in mctesam[[z]]$Test_RSS){
  mctesam[[z]]$Test_RMSE[mctesam[[z]]$Test_RSS == i] <- sqrt(i/100)
  mctesam[[z]]$Train_RMSE[mctesam[[z]]$Test_RSS == i] <- sqrt((mctesam[[z]]$Train_RSS[mctesam[[z]]$Test_RSS == i])/100)
  
}
}

# Plotting test RMSE
#plot(z$Model_Complexity, z$Test_RMSE, xlab= "Model Complexity", ylab="Test RMSE", main = "Test RMSE vs Model Complexity")
#lines(sort(z$Model_Complexity), z$Test_RMSE, col= "blue", type=  "l", pch= 20)

# Plotting train RMSE
#plot(z$Model_Complexity, z$Train_RMSE, xlab= "Model Complexity", ylab="Train RMSE", main = "Train RMSE vs Model Complexity")
#lines(sort(z$Model_Complexity), z$Train_RMSE, col= "red", type=  "l", pch= 20)


# Plot Model Complexity Vs Test_RSS for samples
#setwd("C:\\Users\\Mirra\\Desktop\\Python_GN\\ML2_AssignmentPlots")
#png(filename = paste("Sample",z, "Order", i,".png", sep=""))


# Plotting Test RMSE vs Complexity for S1-S4
plot(range(1,10), range(4800,5500), xlab= "Model Complexity", ylab="Test RMSE", main = "Test RMSE vs Model Complexity")
lines(sort(mc_te_s1$Model_Complexity), mc_te_s1$Test_RMSE, col= "blue", type=  "l", pch= 20)
lines(sort(mc_te_s2$Model_Complexity), mc_te_s2$Test_RMSE, col= "pink", type = "l", pch = 20)
lines(sort(mc_te_s3$Model_Complexity), mc_te_s3$Test_RMSE, col= "brown", type = "l", pch = 20)
lines(sort(mc_te_s4$Model_Complexity), mc_te_s4$Test_RMSE, col= "orange", type = "l", pch = 20)
legend("topright",legend = c("Sample 1","Sample 2", "Sample 3", "Sample 4"), lwd=c(5,2,5,2), col=c("blue","pink","brown","orange"), pch=c(15,19,15,19),cex = 0.5, y.intersp=1.5)

# Plotting Train RMSE vs Complexity for S1-S4
plot(range(1,10), range(590,1570), xlab= "Model Complexity", ylab="Train RMSE", main = "Train RMSE vs Model Complexity")
lines(sort(mc_te_s1$Model_Complexity), mc_te_s1$Train_RMSE, col= "blue", type=  "l", pch= 20)
lines(sort(mc_te_s2$Model_Complexity), mc_te_s2$Train_RMSE, col= "pink", type = "l", pch = 20)
lines(sort(mc_te_s3$Model_Complexity), mc_te_s3$Train_RMSE, col= "brown", type = "l", pch = 20)
lines(sort(mc_te_s4$Model_Complexity), mc_te_s4$Train_RMSE, col= "orange", type = "l", pch = 20)
legend("topright",legend = c("Sample 1","Sample 2", "Sample 3", "Sample 4"), lwd=c(5,2,5,2), col=c("blue","pink","brown","orange"), pch=c(15,19,15,19),cex = 0.5, y.intersp=1.5)

# Plotting Test RMSE vs Complexity for S5-S8
plot(range(1,10), range(4900,5860), xlab= "Model Complexity", ylab="Test RMSE", main = "Test RMSE vs Model Complexity")
lines(sort(mc_te_s5$Model_Complexity), mc_te_s5$Test_RMSE, col= "blue", type=  "l", pch= 20)
lines(sort(mc_te_s6$Model_Complexity), mc_te_s6$Test_RMSE, col= "pink", type = "l", pch = 20)
lines(sort(mc_te_s7$Model_Complexity), mc_te_s7$Test_RMSE, col= "brown", type = "l", pch = 20)
lines(sort(mc_te_s8$Model_Complexity), mc_te_s8$Test_RMSE, col= "orange", type = "l", pch = 20)
legend("topright",legend = c("Sample 5","Sample 6", "Sample 7", "Sample 8"), lwd=c(5,2,5,2), col=c("blue","pink","brown","orange"), pch=c(15,19,15,19),cex = 0.5, y.intersp=1.5)

# Plotting Train RMSE vs Complexity for S5-S8
plot(range(1,10), range(1300, 1930), xlab= "Model Complexity", ylab="Train RMSE", main = "Train RMSE vs Model Complexity")
lines(sort(mc_te_s5$Model_Complexity), mc_te_s5$Train_RMSE, col= "blue", type=  "l", pch= 20)
lines(sort(mc_te_s6$Model_Complexity), mc_te_s6$Train_RMSE, col= "pink", type = "l", pch = 20)
lines(sort(mc_te_s7$Model_Complexity), mc_te_s7$Train_RMSE, col= "brown", type = "l", pch = 20)
lines(sort(mc_te_s8$Model_Complexity), mc_te_s8$Train_RMSE, col= "orange", type = "l", pch = 20)
legend("topright",legend = c("Sample 5","Sample 6", "Sample 7", "Sample 8"), lwd=c(5,2,5,2), col=c("blue","pink","brown","orange"), pch=c(15,19,15,19),cex = 0.5, y.intersp=1.5)

#dev.off()

