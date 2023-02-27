# Install new R packages
install.packages("foreign") # for read.dbf()
install.packages("corrplot") # for correlation plot corplot()
install.packages("pdp") # for partial dependency plots
install.packages("ggplot2") # for plotting above
install.packages("gplots") # for plotMeans()

# Previously used R packages-- install if not already installed
install.packages("dplyr")
install.packages("car")
install.packages("forcats")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("nnet")
install.packages("randomForest")

# Load required R packages
library("dplyr")
library("car")
library("forcats")
library("rpart")
library("rpart.plot")
library("nnet")
library("foreign")
library("corrplot")
library("randomForest")
library("pdp")
library("ggplot2")
library("gplots")

source("BCA_functions_source_file.R")
RSP <-read.csv("vcRSP2017.csv", stringsAsFactors = TRUE)
View(RSP)

# Create estimation and validation samples
RSP$Sample <- create.samples(RSP, est = 0.6, val = 0.4, rand.seed = 1)
View(RSP)

# Summarise data
variable.summary(RSP) # Note %.NA and Min.Level.Size

# Missing Values:"BALCHQ", "BALSAV", "BALLOC", "BALMRGG", "BALLOAN"
# Recode missing values (NA) to 0 using is.na() and if_else()
RSP$BALCHQ <- if_else(condition = is.na(RSP$BALCHQ),
                      true = 0,
                      false = RSP$BALCHQ)
RSP$BALSAV <- if_else(condition = is.na(RSP$BALSAV),
                      true = 0,
                      false = RSP$BALSAV)
RSP$BALLOC <- if_else(condition = is.na(RSP$BALLOC),
                      true = 0,
                      false = RSP$BALLOC)
RSP$BALMRGG <- if_else(condition = is.na(RSP$BALMRGG),
                       true = 0,
                       false = RSP$BALMRGG)
RSP$BALLOAN <- if_else(condition = is.na(RSP$BALLOAN),
                       true = 0,
                       false = RSP$BALLOAN)

# Remove some geodemographic info about RRSPs
RSP$N_IND_INC_ <- NULL

#Removing variables with thin factor levels
RSP$pcode <- NULL

#Removing geodemographic variables that don't affect individual's likelihood to buy RRSP
RSP$numrr_1 <- NULL
RSP$numcon_1 <- NULL

# Not useful statistically since they are arbitrary numbers
row.names(RSP) <- RSP$unique # Set "ID" as record name
RSP$unique <- NULL
View(RSP)

# Summarise data
variable.summary(RSP) # Note %.NA and Min.Level.Size

#Remove changes in services/products over 12 months
RSP$CH_NM_SERV <- NULL
RSP$CH_NM_PRD <- NULL

#Remove ad hoc human-based judgment variable
RSP$valsegm <- NULL

# Remove all missing values from dataset
RSP2 <- na.omit(RSP) #83 rows deleted

# Check all changes, including no missing values.
variable.summary(RSP2)
glimpse(RSP2)

RSPForestAllv <- randomForest(formula = APURCH ~ age + gendf +
                                gendm + atmcrd + paydep + BALCHQ + BALSAV +
                                TOTDEP + BALLOAN + BALLOC + BALMRGG +
                                NEWLOC + NEWMRGG + TXBRAN + TXATM +
                                TXPOS + TXCHQ + TXWEB + TXTEL +
                                TOTSERV + avginc_1 + avginv_1,
                              data = filter(RSP2, Sample =="Estimation"),
                              importance = TRUE,
                              ntree = 500, mtry = 4)

#LOGISTIC REGRESSION
# Correlation Matrix
# Select numeric columns only, then calculate and print correlation coefficients
corrMatrix <- cor(select_if(RSP2, is.numeric)) # see ?dplyr::select_if
# temporarily reduce the number of output digits for easier inspection
options(digits = 2)
corrMatrix
options(digits = 7) # then reset output digits

# Visualize correlation
corrplot(corrMatrix,method="number",type="lower",
         diag = FALSE,number.cex = 0.7)

# Recode "APURCH" factor variable to be numerical for easier visualizations
RSP2$APURCH.num <- if_else(RSP2$APURCH == "Y",1,0)

View(RSP2)

# Create a logistic regression model
RSPLogis <- glm(formula = APURCH ~ age + gendf +
                  gendm + atmcrd + paydep + BALCHQ + BALSAV +
                  TOTDEP + BALLOAN + BALLOC + BALMRGG +
                  NEWLOC + NEWMRGG + TXBRAN + TXATM +
                  TXPOS + TXCHQ + TXWEB + TXTEL +
                  TOTSERV + avginc_1 + avginv_1,
                data = filter(RSP2, Sample =="Estimation"),
                family = binomial(logit))

# Print
summary(RSPLogis)

# Calculate and print McFadden R square
MR2 <- 1 - (RSPLogis$deviance / RSPLogis$null.deviance)
MR2.3 <- round(MR2,digits = 3)
print(paste("McFadden Rsquared: ",MR2.3))

Anova(RSPLogis)

View(RSP2)

# Bin variable using equal proportions (equal counts in each bin)
RSP2$age.Cat <- binVariable(RSP2$age, bins = 4,
                                      method = "proportions",
                                      labels = NULL) # bin borders as x-labels

# Create plot of means - Default and Income
plotmeans(APURCH.num ~ age.Cat, data = RSP2)

# Bin variable using equal proportions (equal counts in each bin)
RSP2$BALCHQ.Cat <- binVariable(RSP2$BALCHQ, bins = 4,
                            method = "proportions",
                            labels = NULL) # bin borders as x-labels

# Create plot of means - Default and Income
plotmeans(APURCH.num ~ BALCHQ.Cat, data = RSP2)

# Bin variable using equal proportions (equal counts in each bin)
RSP2$TOTDEP.Cat <- binVariable(RSP2$TOTDEP, bins = 4,
                               method = "proportions",
                               labels = NULL) # bin borders as x-labels

# Create plot of means - Default and Income
plotmeans(APURCH.num ~ TOTDEP.Cat, data = RSP2)

# Bin variable using equal proportions (equal counts in each bin)
RSP2$TXBRAN.Cat <- binVariable(RSP2$TXBRAN, bins = 4,
                               method = "proportions",
                               labels = NULL) # bin borders as x-labels

# Create plot of means - Default and Income
plotmeans(APURCH.num ~ TXBRAN.Cat, data = RSP2)

# Bin variable using equal proportions (equal counts in each bin)
RSP2$TXWEB.Cat <- binVariable(RSP2$TXWEB, bins = 4,
                               method = "proportions",
                               labels = NULL) # bin borders as x-labels

# Create plot of means - Default and Income
plotmeans(APURCH.num ~ TXPOS.Cat, data = RSP2)

# Create summary statistics for select variables
summary(select(RSP2, age, BALCHQ, BALSAV, BALLOAN, BALMRGG, BALLOC, TOTDEP, TXBRAN, TXATM, TXPOS, TXCHQ,TXWEB, TXTEL))

RSP2$Log.age <- log(RSP2$age + 1)
RSP2$Log.BALCHQ <- log(RSP2$BALCHQ + 1)
RSP2$Log.BALSAV <- log(RSP2$BALSAV + 1)
RSP2$Log.BALLOAN <- log(RSP2$BALLOAN + 1)
RSP2$Log.BALMRGG <- log(RSP2$BALMRGG + 1)
RSP2$Log.BALLOC <- log(RSP2$BALLOC + 1)
RSP2$Log.TOTDEP <- log(RSP2$TOTDEP + 1)
RSP2$Log.TXBRAN <- log(RSP2$TXBRAN + 1)
RSP2$Log.TXATM <- log(RSP2$TXATM + 1)
RSP2$Log.TXPOS <- log(RSP2$TXPOS + 1)
RSP2$Log.TXCHQ <- log(RSP2$TXCHQ + 1)
RSP2$Log.TXWEB <- log(RSP2$TXWEB + 1)
RSP2$Log.TXTEL <- log(RSP2$TXTEL + 1)

RSPLogis2 <- glm(formula = APURCH ~ Log.age + gendf +
                  gendm + atmcrd + paydep + Log.BALCHQ + Log.BALSAV +
                  Log.TOTDEP + Log.BALLOAN + Log.BALLOC + Log.BALMRGG +
                  NEWLOC + NEWMRGG + Log.TXBRAN + Log.TXATM +
                  Log.TXPOS + Log.TXCHQ + Log.TXWEB + Log.TXTEL +
                  TOTSERV + avginc_1 + avginv_1,
                data = filter(RSP2, Sample =="Estimation"),
                family = binomial(logit))

summary(RSPLogis2)

Anova(RSPLogis2)

RSPLogis3 <- glm(formula = APURCH ~ Log.age + gendf +
                   gendm + atmcrd + paydep + Log.BALCHQ + Log.BALSAV +
                   Log.TOTDEP + Log.BALLOAN + Log.BALLOC + Log.BALMRGG +
                   NEWLOC + NEWMRGG + Log.TXBRAN + Log.TXATM +
                   Log.TXPOS + Log.TXCHQ + Log.TXWEB + Log.TXTEL +
                   TOTSERV + avginc_1 + avginv_1,
                 data = filter(RSP2, Sample =="Estimation"),
                 family = binomial(logit))

summary(RSPLogis3)


#Run a stepwise regression using the "RSPlogis" model
RSPStep3 <- step(RSPLogis3, direction = "both")

#
summary(RSPStep3)

# McFadden R2
MR2.step <- 1 - (RSPStep3$deviance / RSPStep3$null.deviance)
MR2.step.3 <- round(MR2.step,digits = 3)
print(paste("McFadden Rsquared: ",MR2.step.3))

# Compare both models using cumulative Lift Chart
lift.chart(modelList = c("RSPLogis","RSPStep3"),
           data = filter(RSP2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")

# Compare forests and regression models
lift.chart(modelList = c("RSPLogis","RSPStep3", "RSPForestAllv"),
           data = filter(RSP2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022, type = "cumulative",
           sub = "Validation")

# 4 node nnet with variables from stepwise logistic
RSPNnet4 <- Nnet(formula = APURCH ~ Log.age + atmcrd + paydep +
                   Log.BALSAV + Log.TOTDEP + Log.BALLOAN + Log.BALLOC +
                   Log.BALMRGG + NEWLOC + NEWMRGG +
                   Log.TXBRAN + Log.TXTEL,
                 data = filter(RSP2, Sample =="Estimation"),
                 decay = 0.15, size = 4)

# Compare stepwise, tree, and neural network model
lift.chart(modelList = c("RSPStep3", "RSPNnet4","RSPForestAllv"),
           data = filter(RSP2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022,
           type = "cumulative", sub = "Validation")

## All variables in nnet
RSPNetAllv <- Nnet(formula = APURCH ~ Log.age + gendf +
                     gendm + atmcrd + paydep + Log.BALCHQ + Log.BALSAV +
                     Log.TOTDEP + Log.BALLOAN + Log.BALLOC + Log.BALMRGG +
                     NEWLOC + NEWMRGG + Log.TXBRAN + Log.TXATM +
                     Log.TXPOS + Log.TXCHQ + Log.TXWEB + Log.TXTEL +
                     TOTSERV + avginc_1 + avginv_1,
                   data = filter(RSP2, Sample =="Estimation"),
                   decay = 0.15, size = 4)


# Compare on Validation Sample
lift.chart(modelList = c("RSPStep3", "RSPNnet4",
                         "RSPNetAllv", "RSPForestAllv"),
           data = filter(RSP2, Sample == "Validation"),
           targLevel = "Y", trueResp = 0.022,
           type = "cumulative", sub = "Validation")

# Raw Estimated Probabilities added to data in ScoreRaw
RSP2$ScoreRaw <- rawProbScore(model = "RSPForestAllv",
                              data = RSP2,
                              targLevel = "Y")

# Adjusted Estimated Probabilities, corrected for oversampling, in ScoreAdj
RSP2$ScoreAdj <- adjProbScore(model = "RSPForestAllv",
                              data= RSP2,
                              targLevel = "Y",
                              trueResp = 0.022)

# Rank Order - rank individuals in dataframe from best to worst, in ScoreRank
RSP2$ScoreRAnk <- rankScore(model = "RSPForestAllv",
                            data = RSP2,
                            targLevel = "Y")

View(RSP2)

# Put customer ID's back as a variable
RSP2$unique <- rownames(RSP2)

View(RSP2)
