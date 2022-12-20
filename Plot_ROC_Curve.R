## Load packages
# install.packages("pROC") if don't install
library(pROC)
# install.packages("randomForest") if don't install
library(randomForest)

## Make my results match yours
set.seed(420)

num.samples <- 100

## Generate 100 values from a normal distribution with
## mean 190 and standard deviation 32, then sort them
weight <- sort(rnorm(n = num.samples, mean = 190, sd = 32))

obese <- ifelse(test = (runif(n = num.samples) < (rank(weight) / num.samples)),
                yes = 1, no = 0
)

## Plot the data
plot(x = weight, y = obese)

## Fit a logistic regression to the data...
glm.fit <- glm(obese ~ weight, family = binomial)
lines(weight, glm.fit$fitted.values)

roc(obese, glm.fit$fitted.values, plot = TRUE)

## Prints the graph as a square.
par(pty = "s")

## We can also change the color of the ROC line, and make it wider...
roc(obese, glm.fit$fitted.values,
    plot = TRUE,
    legacy.axes = TRUE, percent = TRUE,
    xlab = "False Positive Percentage", ylab = "True Postive Percentage",
    col = "#377eb8", lwd = 4
)

## If we want to find out the optimal threshold we can store the
## data used to make the ROC graph in a variable...
roc.info <- roc(obese, glm.fit$fitted.values, legacy.axes = TRUE)
str(roc.info)

## and then extract just the information that we want from that variable.
roc.df <- data.frame(
  tpp = roc.info$sensitivities * 100, ## tpp = true positive percentage
  fpp = (1 - roc.info$specificities) * 100, ## fpp = false positive precentage
  thresholds = roc.info$thresholds
)

## now let's look at the thresholds between TPP 60% and 80%...
roc.df[roc.df$tpp > 60 & roc.df$tpp < 80, ]

## We can calculate the area under the curve...
roc(obese, glm.fit$fitted.values,
    plot = TRUE,
    legacy.axes = TRUE, percent = TRUE,
    xlab = "False Positive Percentage", ylab = "True Postive Percentage",
    col = "#377eb8", lwd = 4, print.auc = TRUE
)

## ...and the partial area under the curve.
roc(obese, glm.fit$fitted.values,
    plot = TRUE,
    legacy.axes = TRUE, percent = TRUE,
    xlab = "False Positive Percentage", ylab = "True Postive Percentage",
    col = "#377eb8", lwd = 4, print.auc = TRUE, print.auc.x = 45,
    partial.auc = c(100, 90), auc.polygon = TRUE,
    auc.polygon.col = "#377eb822"
)


#######################################
##
## Now let's fit the data with a random forest...
##
#######################################

rf.model <- randomForest(factor(obese) ~ weight)

## ROC for random forest
roc(obese, rf.model$votes[, 1],
    plot = TRUE,
    legacy.axes = TRUE, percent = TRUE,
    xlab = "False Positive Percentage", ylab = "True Postive Percentage",
    col = "#4daf4a", lwd = 4, print.auc = TRUE
)


#######################################
##
## Now layer logistic regression and random forest ROC graphs..
##
#######################################
roc(obese, glm.fit$fitted.values,
    plot = TRUE, legacy.axes = TRUE,
    percent = TRUE, xlab = "False Positive Percentage",
    ylab = "True Postive Percentage", col = "#377eb8",
    lwd = 4, print.auc = TRUE
)

plot.roc(obese, rf.model$votes[, 1],
         percent = TRUE,
         col = "#4daf4a", lwd = 4, print.auc = TRUE, add = TRUE,
         print.auc.y = 40
)
legend("bottomright",
       legend = c("Logisitic Regression", "Random Forest"),
       col = c("#377eb8", "#4daf4a"), lwd = 4
)


#######################################
##
## Now that we're done with our ROC fun, let's reset the par() variables.
## There are two ways to do it...
##
#######################################
par(pty = "m")
