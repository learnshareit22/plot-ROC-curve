## Install packages if you don't have
#install("pROC")
#install("randomForest")

## Load packages
library(pROC)
library(randomForest)

## Make my results match yours
set.seed(600)

nSamples <- 300

## Generate 300 values from a normal distribution with
## mean 190, sd 32
wt <- sort(rnorm(n = nSamples, mean = 190, sd = 32))

ob <- ifelse(test = (runif(n = nSamples) < (rank(wt) / nSamples)),
             no = 0, yes = 1)

## Change the color of the ROC line
roc(ob,
    plot = TRUE, glmFit$fitted.values,
    ylab = "True Percentage", xlab = "False Percentage", 
    col = "#377eb8", lwd = 5,
    percent = TRUE, legacy.axes = TRUE)

## Determine the area.
roc(ob,
    plot = TRUE, glmFit$fitted.values,
    ylab = "True Postive Percentage", xlab = "False Positive Percentage",
    col = "#377eb8", print.auc = TRUE, lwd = 5, 
    percent = TRUE, legacy.axes = TRUE)

## The partial region.
roc(ob,
    plot = TRUE, glmFit$fitted.values,
    ylab = "True Postive Percentage", xlab = "False Positive Percentage", 
    col = "#377eb8", print.auc.x = 45, lwd = 5, print.auc = TRUE, 
    legacy.axes = TRUE, percent = TRUE,
    auc.polygon.col = "#377eb822",
    auc.polygon = TRUE, partial.auc = c(100, 90))

#######################################
##
## Fit the data
##
#######################################

rocModel <- randomForest(factor(ob) ~ wt)

roc(ob, plot = TRUE, 
    rocModel$votes[, 1],
    percent = TRUE,, legacy.axes = TRUE, 
    ylab = "True Percentage", xlab = "False Percentage", 
    col = "#4daf4a", print.auc = TRUE, lwd = 5)


#######################################
##
## Plot random forest ROC graphs and layer logistic regression graphs.
##
#######################################
roc(ob, percent = TRUE, glmFit$fitted.values,
    xlab = "False Percentage", col = "#377eb8",
    plot = TRUE, legacy.axes = TRUE,
    lwd = 4, print.auc = TRUE,
    ylab = "True Percentage")

plot.roc(ob, rocModel$votes[, 1],
         col = "#4daf4a", lwd = 4,  add = TRUE, print.auc = TRUE,
         percent = TRUE, print.auc.y = 40)

legend(legend = c("The Logisitic Regression", "The Random Forest"), "bottomright",
       col = c("#377eb8", "#4daf4a"), lwd = 5)

par(pty = "m")
