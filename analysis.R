# Libraries for basic data wrangling and exploration
library(xda) # https://github.com/ujjwalkarn/xda
library(tidyverse) # https://github.com/tidyverse/tidyverse

# Libraries for basic ML
library(ROCR)
library(caret) # https://github.com/topepo/caret
library(rpart) # https://github.com/cran/rpart
library(rpart.plot) # https://github.com/cran/rpart.plot

# Custom support functions
source("./support.R")

set.seed(415) # for reproducibility

df <- readr::read_csv("./data.csv")

# code = identifier, location = location, class = class, 
# phase = phase, pr = fraud probability, y = fraud or no fraud
colnames(df) <- c("code", "location", "class", "phase", "pr", "y")

# as factors
df<- df %>%
        mutate_if(is.character, as.factor)



# Understanding the data
head(df)
summary(df)

# Removing the unique identifier - we don't need it
df <- df %>% select(-code)

# Plot 1 - Histogram of model output + inspection result
ggplot(df, aes(x = pr, fill = y)) +
        geom_histogram(binwidth = 0.1) +
        theme_bw() +
        labs(x = "Fraud Probability",
             y = "Total number",
             title = "Histogram of model output") +
        scale_fill_discrete(name="Inspection\nResult")

# Table of inspection results
table(df$y)


# Question 1: 
# Assessment of the overall result of the model

# Make a table of y (the actual result) vs model output

table(df %>% select(y, pr))
# Result:
#           pr
#y          0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9
#Fraud      8   3   1   2   3   0   2   1   4
#No Fraud  33  13   4   5   1   2   1   5   3

# Construct a confusion matrix

df %>% filter (pr > 0.5) %>% group_by(y) %>% tally()
df %>% filter (pr <= 0.5) %>% group_by(y) %>% tally()

#                               FRAUD        NO FRAUD
# Predicted FRAUD (pr > 0.5)      7             11
# Predict NO FRAUD (pr <= 0.5)    17            56

# Overall accuracy = (true positive + true negative)/ total population
# Overall accuracy = (7+56)/91 = 69.2%

# Run a bivariate analysis to discover fraud probability thresholds
# Independent variable = model output, dependent variable = actual result
results<- xda::bivariate(df, dep.var  = "y", indep.var = "pr")

#        bin_pr No Fraud Fraud
# (0.0992,0.3]       50    12
#    (0.5,0.7]        3     2
#    (0.3,0.5]        6     5
#  (0.7,0.901]        8     5


# Discover the percentage of fraud/no fraud for each probability bin
results <- results %>% 
        mutate(
                pct_fraud = as.integer(as.character(Fraud))/(as.integer(as.character(`No Fraud`))+as.integer(as.character(Fraud))),
                pct_nfraud = as.integer(as.character(`No Fraud`))/(as.integer(as.character(Fraud))+as.integer(as.character(`No Fraud`)))
        )

#bin_pr          No Fraud Fraud pct_fraud pct_nfraud
# (0.0992,0.3]       50    12     0.194      0.806
#    (0.5,0.7]        3     2     0.400      0.600
#    (0.3,0.5]        6     5     0.455      0.545
#  (0.7,0.901]        8     5     0.385      0.615

# Build ROC curve
target_pred <- as.matrix(df$pr)
target_class <- as.matrix(df$y)

pred <- prediction(target_pred, target_class)
perf <- performance(pred,"err")
# changing params for the ROC plot - width, etc
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.0,cex.lab=1.0)
plot(perf,col="black",lty=3, lwd=3)
# calculating AUC
auc <- performance(pred,"auc")
# now converting S4 class to vector
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc, 2)
# adding min and max ROC AUC to the center of the plot
auct <- paste(c("AUC  = "),auc,sep="")
legend(0.1,0.9,c(auct,"\n"),border="white",cex=0.7,box.col = "white")

#Finding best threshold

threshold <- 0
# New confusion matrix
df %>% filter (pr > threshold) %>% group_by(y) %>% tally()
df %>% filter (pr <= threshold) %>% group_by(y) %>% tally()



# Question 2: 
# The relationship of the variables in the model: 
# When the model works better or worse? (use the available datafields)

# All data
plot1 <- plot_all_predictors(df, "All data")

# Accurate model
accurate <- df %>%
        filter(
                (pr >= 0.75 & y == "Fraud") | (pr <= 0.25 & y == "No Fraud")
        )
plot2 <- plot_all_predictors(accurate, "Accurate")

# Inaccurate model
inaccurate <- df %>%
        filter(
                (pr >= 0.75 & y == "No Fraud") | (pr <= 0.25 & y == "Fraud")
        )
plot3 <- plot_all_predictors(inaccurate, "Inaccurate")

# With question 1 in mind, we will explore the model when its output is 0.1 <= pr <= 0.3
# given the fact that the model accuracy in this interval is higher than any other 

df2 <- df %>% filter(pr <= 0.3)

hist(df2$pr) # Most are below 0.2

# Graphical analysis of all variables (phase, location, class, fraud vs no fraud)


# The graphic hints that the model seems to be more accurate
# (considering accuracy > 80%, output probability <= 0.3, results = No Fraud)
# when phase = 1 and class = commercial. 
# We can compare the model to a simple decision tree and see if that is true.

# Creating the dataset
df.tree <- df2 %>% select(-pr)

# Creating a rpart model
frmla <- y ~ .
tree <- rpart::rpart(frmla,
                    data = df.tree
                    )

# Plotting the tree
rpart.plot::prp(tree)

# Getting variable importance
caret::varImp(tree)
# reduction in the loss function (e.g. mean squared error)
# attributed to each variable
#class       4.65
#location    8.49
#phase       7.54

# Question 2 short answer:

# The model works best with residential, 1-phase locations.
# In all other scenarios, the model does not show an acceptable accuracy.
