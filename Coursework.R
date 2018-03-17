

## Data load in and preparation 
creditcard <- read.csv("CreditCard.csv", header = TRUE)
str(creditcard)

#Encoding variables as factors since they were recognized as integers

#Accoung balance
sum(is.na(creditcard$account_balance))


temp <- factor(creditcard$account_balance, levels = c("1", "2", "3", "4"))
sum(is.na(temp))
levels(temp)

creditcard$account_balance <- temp
sum(is.na(creditcard$account_balance))
levels(creditcard$account_balance)

#Duration
sum(is.na(creditcard$duration))

#Credit History
sum(is.na(creditcard$credit_history))



temp <- factor(creditcard$credit_history, levels = c("1", "2", "3", "4"))
sum(is.na(temp))
levels(temp)

creditcard$credit_history <- temp
sum(is.na(creditcard$credit_history))
levels(creditcard$credit_history)

#Purpose

table(creditcard$purpose)
sum(is.na(creditcard$purpose))
#One interesting thing to note is that Purpose is supposed to be a nominal variable with 0, 1, 2, 3, 4
#as levels, however, there are NA's in purpose and there are no mentions of variable 0,
#So all NA's are encoded as 0's
temp <- creditcard$purpose
sum(is.na(temp))
temp[is.na(temp) == TRUE] = 0
sum(is.na(temp))

creditcard$purpose <- temp
sum(is.na(creditcard$purpose))


temp <- factor(creditcard$purpose, levels = c("0","1", "2", "3", "4"))
sum(is.na(temp))
levels(temp)

creditcard$purpose <- temp
sum(is.na(creditcard$purpose))
levels(creditcard$purpose)
table(creditcard$purpose)

str(creditcard)

#Amount

sum(is.na(creditcard$amount))



#Savings

sum(is.na(creditcard$savings))
#Savings has 62 entries that are NA's, this may be because people don't have savings accounts

temp <- factor(creditcard$savings, levels = c("1", "2", "3"))
sum(is.na(temp))
levels(temp)

creditcard$savings <- temp
sum(is.na(creditcard$savings))
levels(creditcard$savings)

#Installment Percentage
sum(is.na(creditcard$install_percent))

#Time at Address

sum(is.na(creditcard$time_at_address))

#144 NA entries for time at address
table(creditcard$time_at_address)

temp <- factor(creditcard$time_at_address, levels = c("1", "2", "3"))
sum(is.na(temp))
levels(temp)

creditcard$time_at_address <- temp
sum(is.na(creditcard$time_at_address))
levels(creditcard$time_at_address)

#Property

sum(is.na(creditcard$property))
table(creditcard$property)

temp <- factor(creditcard$property, levels = c("1", "2", "3", "4"))
sum(is.na(temp))
levels(temp)

creditcard$property <- temp
sum(is.na(creditcard$property))
levels(creditcard$property)

#Age 

sum(is.na(creditcard$age))

#Residential Status

sum(is.na(creditcard$res_status))
table(creditcard$res_status)

temp <- factor(creditcard$res_status, levels = c("1", "2", "3"))
sum(is.na(temp))
levels(temp)

creditcard$res_status <- temp
sum(is.na(creditcard$res_status))
levels(creditcard$res_status)

#Occupation

sum(is.na(creditcard$occupation))
table(creditcard$occupation)
#59 NA values, since there is no 0 levels, the NA values are encoded as level 0

temp <- creditcard$occupation
sum(is.na(temp))
temp[is.na(temp) == TRUE] = 0
sum(is.na(temp))

creditcard$occupation <- temp
sum(is.na(creditcard$occupation))


temp <- factor(creditcard$occupation, levels = c("0","1", "2", "3", "4"))
sum(is.na(temp))
levels(temp)

creditcard$occupation <- temp
sum(is.na(creditcard$occupation))
levels(creditcard$occupation)
table(creditcard$occupation)

#Type

#One important thing to note is that 1 is a good customer and 0 is a bad customer, unlike the workshop

sum(is.na(creditcard$Type))
table(creditcard$Type)

#temp <- factor(creditcard$Type, levels = c("0", "1"))
#sum(is.na(temp))
#levels(temp)

#creditcard$Type <- temp
#sum(is.na(creditcard$Type))
#levels(creditcard$Type)

str(creditcard)

###Data Exploration


hist(creditcard$duration)
hist(creditcard$amount)
hist(creditcard$age)
sum(is.na(creditcard$install_percent))

summary(creditcard)

cor(creditcard, use = "complete.obs", method = "pearson")


plot(creditcard)

attach(creditcard)

#Account Balance
T <- table(creditcard$account_balance, creditcard$Type)
T

#Calculates proportion of BAD customers varied across account balance
T/rowSums(T)

chisq.test(T)

#Takes first column in T, which is number of 0 customers across jobs divides it by the total and 
#repeats for number of columns
#So basically this gives the overall probability of being bad regardless of jobs

my_p <- rep( sum(T[,1])/sum(T), ncol(T))
my_p

chisq.test(T, p = my_p)


#Credit History
T <- table(creditcard$credit_history, creditcard$Type)
T

#Calculates proportion of BAD customers varied across credit history
T/rowSums(T)

chisq.test(T)

sum(is.na(creditcard$savings))




##Visualization

hist(creditcard$duration, freq = FALSE)
lines(density(creditcard$duration), t="l", col = "blue")
#Fitting a normal distribution on top of it
curve(dnorm(x, mean = mean(creditcard$duration), sd = sd(creditcard$duration)), from = min(creditcard$duration), 
      to = max(creditcard$duration), col = "red", add = TRUE)
library(ggplot2)
ggplot(creditcard) + geom_bar(aes(duration, color = Type))
ggplot(creditcard) + geom_bar(aes(duration, color = Type))



histogram(~account_balance | Type, data = creditcard, type = "density")
histogram(~duration | Type, data = creditcard, type = "density")
histogram(~credit_history | Type, data = creditcard, type = "density")
histogram(~purpose| Type, data = creditcard, type = "density")
histogram(~amount | Type, data = creditcard, type = "density")
histogram(~savings | Type, data = creditcard, type = "density")
histogram(~install_percent | Type, data = creditcard, type = "density")
histogram(~time_at_address | Type, data = creditcard, type = "density")
histogram(~property | Type, data = creditcard, type = "density")
histogram(~age | Type, data = creditcard, type = "density")
histogram(~res_status | Type, data = creditcard, type = "density")
histogram(~occupation | Type, data = creditcard, type = "density")


densityplot(~account_balance, data = creditcard, groups = Type, plot.points = FALSE, 
            auto.key = TRUE, main = "Density plots for account balance")
densityplot(~duration, data = creditcard, groups = Type, plot.points = FALSE, 
            auto.key = TRUE, main = "Density plots for account balance")
densityplot(~credit_history, data = creditcard, groups = Type, plot.points = FALSE, 
            auto.key = TRUE, main = "Density plots for account balance")
densityplot(~purpose, data = creditcard, groups = Type, plot.points = FALSE, 
            auto.key = TRUE, main = "Density plots for account balance")
densityplot(~amount, data = creditcard, groups = Type, plot.points = FALSE, 
            auto.key = TRUE, main = "Density plots for account balance")
densityplot(~savings, data = creditcard, groups = Type, plot.points = FALSE, 
            auto.key = TRUE, main = "Density plots for account balance")
densityplot(~install_percent, data = creditcard, groups = Type, plot.points = FALSE, 
            auto.key = TRUE, main = "Density plots for account balance")
densityplot(~time_at_address, data = creditcard, groups = Type, plot.points = FALSE, 
            auto.key = TRUE, main = "Density plots for account balance")
densityplot(~age, data = creditcard, groups = Type, plot.points = FALSE, 
            auto.key = TRUE, main = "Density plots for account balance")
densityplot(~res_status, data = creditcard, groups = Type, plot.points = FALSE, 
            auto.key = TRUE, main = "Density plots for account balance")
densityplot(~occupation, data = creditcard, groups = Type, plot.points = FALSE, 
            auto.key = TRUE, main = "Density plots for account balance")



counts  <- table(account_balance)
counts
#Barchart of JOB variable
barplot(counts)

#Barchart of proportions of JOB
barplot(counts/sum(counts), ylab = "Proportion", xlab = "JOB")

counts <- table(creditcard$Type, creditcard$account_balance)
#Using Grouped Bar Plot instead
barplot(counts, main = "JOB frequencies by class", xlab = "JOB", col = c("Darkblue", "red"), 
        legend = rownames(counts), beside = TRUE)

T <- prop.table(counts, 2)
T

#plotting proportions
barplot(prop.table(counts,2), beside = TRUE, main = "Distribution of JOB by Class",
        xlab = "JOB", col = c("darkblue","red"), 
        legend = c("good", "bad"), ylim = c(0,1))



counts <- table(creditcard$Type, creditcard$duration)
#Using Grouped Bar Plot instead
barplot(counts, main = "JOB frequencies by class", xlab = "JOB", col = c("Darkblue", "red"), 
        legend = rownames(counts), beside = TRUE)

T <- prop.table(counts, 2)
T

#plotting proportions
barplot(prop.table(counts,2), beside = TRUE, main = "Distribution of JOB by Class",
        xlab = "JOB", col = c("darkblue","red"), 
        legend = c("good", "bad"), ylim = c(0,1))


counts <- table(creditcard$Type, creditcard$credit_history)
#Using Grouped Bar Plot instead
barplot(counts, main = "JOB frequencies by class", xlab = "JOB", col = c("Darkblue", "red"), 
        legend = rownames(counts), beside = TRUE)

T <- prop.table(counts, 2)
T

#plotting proportions
barplot(prop.table(counts,2), beside = TRUE, main = "Distribution of JOB by Class",
        xlab = "JOB", col = c("darkblue","red"), 
        legend = c("good", "bad"), ylim = c(0,1))


counts <- table(creditcard$Type, creditcard$purpose)
#Using Grouped Bar Plot instead
barplot(counts, main = "JOB frequencies by class", xlab = "JOB", col = c("Darkblue", "red"), 
        legend = rownames(counts), beside = TRUE)

T <- prop.table(counts, 2)
T

#plotting proportions
barplot(prop.table(counts,2), beside = TRUE, main = "Distribution of JOB by Class",
        xlab = "JOB", col = c("darkblue","red"), 
        legend = c("good", "bad"), ylim = c(0,1))


counts <- table(creditcard$Type, creditcard$savings)
#Using Grouped Bar Plot instead
barplot(counts, main = "JOB frequencies by class", xlab = "JOB", col = c("Darkblue", "red"), 
        legend = rownames(counts), beside = TRUE)

T <- prop.table(counts, 2)
T

#plotting proportions
barplot(prop.table(counts,2), beside = TRUE, main = "Distribution of JOB by Class",
        xlab = "JOB", col = c("darkblue","red"), 
        legend = c("good", "bad"), ylim = c(0,1))


counts <- table(creditcard$Type, creditcard$install_percent)
#Using Grouped Bar Plot instead
barplot(counts, main = "JOB frequencies by class", xlab = "JOB", col = c("Darkblue", "red"), 
        legend = rownames(counts), beside = TRUE)

T <- prop.table(counts, 2)
T

#plotting proportions
barplot(prop.table(counts,2), beside = TRUE, main = "Distribution of JOB by Class",
        xlab = "JOB", col = c("darkblue","red"), 
        legend = c("good", "bad"), ylim = c(0,1))

counts <- table(creditcard$Type, creditcard$time_at_address)
#Using Grouped Bar Plot instead
barplot(counts, main = "JOB frequencies by class", xlab = "JOB", col = c("Darkblue", "red"), 
        legend = rownames(counts), beside = TRUE)

T <- prop.table(counts, 2)
T

#plotting proportions
barplot(prop.table(counts,2), beside = TRUE, main = "Distribution of JOB by Class",
        xlab = "JOB", col = c("darkblue","red"), 
        legend = c("good", "bad"), ylim = c(0,1))



counts <- table(creditcard$Type, creditcard$property)
#Using Grouped Bar Plot instead
barplot(counts, main = "JOB frequencies by class", xlab = "JOB", col = c("Darkblue", "red"), 
        legend = rownames(counts), beside = TRUE)

T <- prop.table(counts, 2)
T

#plotting proportions
barplot(prop.table(counts,2), beside = TRUE, main = "Distribution of JOB by Class",
        xlab = "JOB", col = c("darkblue","red"), 
        legend = c("good", "bad"), ylim = c(0,1))


counts <- table(creditcard$Type, creditcard$age)
#Using Grouped Bar Plot instead
barplot(counts, main = "JOB frequencies by class", xlab = "JOB", col = c("Darkblue", "red"), 
        legend = rownames(counts), beside = TRUE)

T <- prop.table(counts, 2)
T

#plotting proportions
barplot(prop.table(counts,2), beside = TRUE, main = "Distribution of JOB by Class",
        xlab = "JOB", col = c("darkblue","red"), 
        legend = c("good", "bad"), ylim = c(0,1))


counts <- table(creditcard$Type, creditcard$res_status)
#Using Grouped Bar Plot instead
barplot(counts, main = "JOB frequencies by class", xlab = "JOB", col = c("Darkblue", "red"), 
        legend = rownames(counts), beside = TRUE)

T <- prop.table(counts, 2)
T

#plotting proportions
barplot(prop.table(counts,2), beside = TRUE, main = "Distribution of JOB by Class",
        xlab = "JOB", col = c("darkblue","red"), 
        legend = c("good", "bad"), ylim = c(0,1))



counts <- table(creditcard$Type, creditcard$occupation)
#Using Grouped Bar Plot instead
barplot(counts, main = "JOB frequencies by class", xlab = "JOB", col = c("Darkblue", "red"), 
        legend = rownames(counts), beside = TRUE)

T <- prop.table(counts, 2)
T

#plotting proportions
barplot(prop.table(counts,2), beside = TRUE, main = "Distribution of JOB by Class",
        xlab = "JOB", col = c("darkblue","red"), 
        legend = c("good", "bad"), ylim = c(0,1))


pairs(~ creditcard$account_balance + creditcard$duration + creditcard$credit_history, data = creditcard, col = ifelse(creditcard$Type == 1, "blue", "red"))
pairs(~ creditcard$purpose + creditcard$amount + creditcard$savings, data = creditcard, col = ifelse(creditcard$Type == 1, "blue", "red"))
pairs(~ creditcard$install_percent + creditcard$time_at_address + creditcard$property, data = creditcard, col = ifelse(creditcard$Type == 1, "blue", "red"))
pairs(~ creditcard$age + creditcard$res_status + creditcard$occupation, data = creditcard, col = ifelse(creditcard$Type == 1, "blue", "red"))



xyplot(creditcard$account_balance ~ creditcard$duration, creditcard, groups = creditcard$Type, auto.key = TRUE, 
       type = c("p", "smooth"), col.line = c("darkblue","red"), lwd = 3)


xyplot(creditcard$account_balance ~ creditcard$duration | creditcard$Type, creditcard, groups = creditcard$Type, auto.key = TRUE, 
       type = c("p", "smooth"), col.line = c("darkblue","red"), lwd = 3)


xyplot(creditcard$account_balance ~ creditcard$credit_history, creditcard, groups = creditcard$Type, auto.key = TRUE, 
       type = c("p", "smooth"), col.line = c("darkblue","red"), lwd = 3)



xyplot(creditcard$occupation ~ creditcard$savings, creditcard, groups = creditcard$Type, auto.key = TRUE, 
       type = c("p", "smooth"), col.line = c("darkblue","red"), lwd = 3)


xyplot(creditcard$account_balance ~ creditcard$ | creditcard$Type, creditcard, groups = creditcard$Type, auto.key = TRUE, 
       type = c("p", "smooth"), col.line = c("darkblue","red"), lwd = 3)





##

B <- read.csv("Creditcard.csv", na.strings = c("", " ", "NA"))

IV <- create_infotables(data = B, y = "Type", bins = 10)
IV$Summary

str(B)



#Credit History
sum(is.na(B$credit_history))



temp <- factor(B$credit_history, levels = c("1", "2", "3", "4"))
sum(is.na(temp))
levels(temp)

B$credit_history <- temp
sum(is.na(B$credit_history))
levels(B$credit_history)

#Purpose

table(B$purpose)
sum(is.na(B$purpose))
#One interesting thing to note is that Purpose is supposed to be a nominal variable with 0, 1, 2, 3, 4
#as levels, however, there are NA's in purpose and there are no mentions of variable 0,
#So all NA's are encoded as 0's
temp <- B$purpose
sum(is.na(temp))
temp[is.na(temp) == TRUE] = 0
sum(is.na(temp))

B$purpose <- temp
sum(is.na(B$purpose))


temp <- factor(B$purpose, levels = c("0","1", "2", "3", "4"))
sum(is.na(temp))
levels(temp)

B$purpose <- temp
sum(is.na(B$purpose))
levels(B$purpose)
table(B$purpose)

#Residential Status

sum(is.na(B$res_status))
table(B$res_status)

temp <- factor(B$res_status, levels = c("1", "2", "3"))
sum(is.na(temp))
levels(temp)

B$res_status <- temp
sum(is.na(B$res_status))
levels(B$res_status)

#Occupation

sum(is.na(B$occupation))
table(B$occupation)
#59 NA values, since there is no 0 levels, the NA values are encoded as level 0

temp <- B$occupation
sum(is.na(temp))
temp[is.na(temp) == TRUE] = 0
sum(is.na(temp))

B$occupation <- temp
sum(is.na(B$occupation))


temp <- factor(B$occupation, levels = c("0","1", "2", "3", "4"))
sum(is.na(temp))
levels(temp)

B$occupation <- temp
sum(is.na(B$occupation))
levels(B$occupation)
table(B$occupation)



IV <- create_infotables(data = B, y = "Type", bins = 10)
IV$Summary

str(B)

IV$Tables$duration


plot_infotables(IV, "account_balance")
plot_infotables(IV, "duration")
plot_infotables(IV, "credit_history")
plot_infotables(IV, "purpose")
plot_infotables(IV, "amount")
plot_infotables(IV, "savings")
plot_infotables(IV, "install_percent")
plot_infotables(IV, "time_at_address")
plot_infotables(IV, "property")
plot_infotables(IV, "age")
plot_infotables(IV, "res_status")
plot_infotables(IV, "occupation")


#Plot WoE for 3 variables with highest IV 
plot_infotables(IV, IV$Summary$Variable[1:3])



#Wrong implementation
A <- read.csv("Creditcard.csv", na.strings = c("", " ", "NA"))
IV <- create_infotables(data = A, y = "Type", bins = 10)
Z <- WOE_replace_df(A, IV)
head(A, 10)
head(Z, 10)

#
Y <- WOE_replace_df(B, IV)
head(B, 10)
head(Y, 10)




###########################

IV <- create_infotables(data = creditcard, y = "Type", bins = 10)
IV$Summary



str(creditcard)

IV$Tables$duration
IV$Tables$account_balance

plot_infotables(IV, "account_balance")
plot_infotables(IV, "duration")
plot_infotables(IV, "credit_history")
plot_infotables(IV, "purpose")
plot_infotables(IV, "amount")
plot_infotables(IV, "savings")
plot_infotables(IV, "install_percent")
plot_infotables(IV, "time_at_address")
plot_infotables(IV, "property")
plot_infotables(IV, "age")
plot_infotables(IV, "res_status")
plot_infotables(IV, "occupation")

#Plot WoE for 3 variables with highest IV 
plot_infotables(IV, IV$Summary$Variable[1:3])


#Created a credit with weights of evidence for all interval variables: duration, amount, and age
creditwoe <- creditcard
str(creditwoe)
creditwoe$duration <- WOE_replace(creditcard$duration, IV$Tables$duration)
creditwoe$amount <- WOE_replace(creditcard$amount, IV$Tables$amount)
creditwoe$age <- WOE_replace(creditcard$age, IV$Tables$age)
creditwoe$install_percent <- WOE_replace(creditcard$install_percent, IV$Tables$install_percent)
summary(creditwoe)


##Created a credit with weights of evidence, same as last one, but this time with savings and 
#time at address to deal with the NA situation

creditwoenona <- creditwoe

creditwoenona$savings <- WOE_replace(creditcard$savings, IV$Tables$savings)
creditwoenona$time_at_address <- WOE_replace(creditcard$time_at_address, IV$Tables$time_at_address)


####


T <- xtabs(~ creditwoe$duration + creditwoe$Type)
T

#Logistic Regression 

set.seed(1)
#Train 70% of data
train <- sample(1:nrow(creditcard), floor(0.7*nrow(creditcard)))

creditcard.train <- creditcard[train,]
creditcard.test <- creditcard[-train,]


set.seed(1)
#Train 70% of data
train <- sample(1:nrow(B), floor(0.7*nrow(B)))

creditcard.train <- creditcard[train,]
creditcard.test <- creditcard[-train,]

creditwoe.train <- creditwoe[train,]
creditwoe.test <- creditwoe[-train,]

creditwoenona.train <- creditwoenona[train,]
creditwoenona.test <- creditwoenona[-train,]

#Only intercept
reg0 <- glm(Type ~ 1, data = creditcard.train, family = binomial)
summary(reg0)


#Regression with duration as it is the most important
reg1 <- glm(Type ~ duration, data = creditcard.train, family = binomial)
summary(reg1)

beta <- coef(reg1)

#Scatterplot
plot(creditcard$Type~ creditcard$duration)
#x is a vector with a range with that of DEBTINC 
x <- c(0:80)
#Recall that our logistic regression : P(BAD = 1 | DEBTINC) = 1/(1 + e^ (-beta0 - beta1DEBTINC))
#the following line therefore plots the estimated probability of default

lines(x, 1/(1+exp(-beta[1] - beta[2]*x)), col = "red", type = "l")



## Testing reg1

probs <- predict(reg1, newdata = creditcard.test, type = "response")

#cl is TRUE when the probability of default 0.5 otherwise its false
cl <- (probs>0.5)


#Construct confusiong matrix 
table(creditcard.test$Type, cl)

89/nrow(creditcard.test)
#9.888% error rate 


####testing on train data

#Setting the type to "response" option tells R to output probabilities of the form P(Y=1|X)
#as opposed to other information such as the logit
probs <- predict(reg1, newdata = creditcard.train, type = "response")

#cl is TRUE when the probability of default 0.5 otherwise its false
cl <- (probs>0.5)


#Construct confusiong matrix 
table(creditcard.train$Type, cl)

211/nrow(creditcard.train)

#10.04% error rate on train data


#Using duration but with weights of evidence this time

reg2 <- glm(Type ~ duration, data = creditwoe.train, family = binomial)
summary(reg2)

beta <- coef(reg2)

#Scatterplot
plot(creditwoe$Type~ creditwoe$duration)
#x is a vector with a range with that of DEBTINC 
x <- c(-2:2)
#Recall that our logistic regression : P(BAD = 1 | DEBTINC) = 1/(1 + e^ (-beta0 - beta1DEBTINC))
#the following line therefore plots the estimated probability of default

lines(x, 1/(1+exp(-beta[1] - beta[2]*x)), col = "red", type = "l")

## Testing reg2

probs <- predict(reg2, newdata = creditwoe.test, type = "response")

#cl is TRUE when the probability of default 0.5 otherwise its false
cl <- (probs>0.5)


#Construct confusiong matrix 
table(creditwoe.test$Type, cl)

89/nrow(creditwoe.test)
#9.888% error rate 


####testing on train data

#Setting the type to "response" option tells R to output probabilities of the form P(Y=1|X)
#as opposed to other information such as the logit
probs <- predict(reg2, newdata = creditwoe.train, type = "response")

#cl is TRUE when the probability of default 0.5 otherwise its false
cl <- (probs>0.5)


#Construct confusiong matrix 
table(creditwoe.train$Type, cl)

213/nrow(creditwoe.train)

#10.14% error rate on train data




#Contingency table with rows corresponding to WOE for DEBTINC and columns to # of BAD
T <- xtabs(~ creditwoe$duration + creditwoe$Type)
T
#Print proportion of good/bad customers for each weights of evidence value
P <- T/rowSums(T)
P
#Predict proportion of bad customers in each of the twenty bins and the dedicated proportion
#from our logistic regression

#The first way is to realize that these are the names of the rows of the table P
woe.vals <- rownames(P)
#The second way is to obtain these as the unique values of the Y$DEBTINC variable
#In this case we also need to sort them in ascending order
woe.vals <- sort(unique(creditwoe$duration))
woe.vals
#Plot the proportion of missign values against the values of the WoE for DEBTINC
plot(P[,2] ~ woe.vals, xlab = "WoE for Duration", ylab = "Proportion of GOOD customers")

#Next plot the predicted probabilities from our logistic regression model 

#Create vector x by sequential values between -2 to 2 with a stepsize of 0.01
x <- seq(-2,2,0.01)
#Obtain estimated coefficients
beta <- coef(reg2)
lines(x, 1/(1 + exp(-beta[1] - beta[2] * x)), col = "red", type = "l")


#Deviance and Simple model comparision
#Can do Chi-squared because we binned the values instead of using contiuous values

#You can obtain appropriate values from summary(logRegWOE)
summary(reg2)
pchisq(1244.5,2098, lower.tail = FALSE)
#A better way is to use functions to extract that info from the logistic regression object
pchisq(deviance(reg2), df.residual(reg2), lower.tail = FALSE)
#Can't reject null because very big so null is true

#Comparing model with only the intercept

#Chi-squared test with null model being model containing intercept only 
pchisq(reg2$null.deviance - reg2$deviance, 1, lower.tail = FALSE)
#Can reject null because very small


##Performance assessment 
#Sensitivity and Specificity

#Notice that since we use WoE encoding there are no more missing values so there is no need to 
#subset the dataset
probsWOE <- predict(reg2, newdata = creditwoe.test, type = "response")
#Create confusion matrix using 0.5 for threshold for classification
table(creditwoe.test$Type, probsWOE>0.5)

#Compute sensitivity = Proportion of Bad customers correctly indentified 
Sensitivity <- T[2,2]/sum(T[2,])
Sensitivity



#library used to display ROC curves
library(pROC)
#Compute and plot the ROC curve
roc1 <- roc(creditwoenona.test$Type, probsWOE, plot = TRUE, auc = TRUE, grid = TRUE, col = "red", main = "ROC Curve for Duration", legend)

#adding ROC from previous logistic regression
probs <- predict(object = reg1, newdata = creditcard.test, type = "response")
roc2 <- roc(creditcard.test$Type, probs, plot = TRUE, add = TRUE, auc = TRUE, grid = TRUE, col = "blue")


roc1$auc
roc2$auc
#Compute Area Under Curve 
AUC_WOE <- auc(creditwoe.test$Type, probsWOE)
AUC_WOE

#Asking for specific values from ROC curve
coords(roc1, x= 0.8, input = "sensitivity", 
       ret = c("threshold", "specificity", "sensitivity", "accuracy", "tp", "tn", "fp", "fn"))

roc1$sensitivities

rstring <- c("threshold", "specificity", "sensitivity", "accuracy", "tp","tn","fp","fn")
coords(roc1, x=0.5, input = "threshold", ret = rstring)



####This shows that the WOE is better for use with duration 


#Doing the same thing for amount

reg3 <- glm(Type ~ amount, data = creditcard.train, family = binomial)
summary(reg3)

beta <- coef(reg3)

#Scatterplot
plot(creditcard$Type~ creditcard$amount)
#x is a vector with a range with that of DEBTINC 
x <- c(0:20000)
#Recall that our logistic regression : P(BAD = 1 | DEBTINC) = 1/(1 + e^ (-beta0 - beta1DEBTINC))
#the following line therefore plots the estimated probability of default

lines(x, 1/(1+exp(-beta[1] - beta[2]*x)), col = "red", type = "l")



## Testing reg3

probs <- predict(reg3, newdata = creditcard.test, type = "response")

#cl is TRUE when the probability of default 0.5 otherwise its false
cl <- (probs>0.5)


#Construct confusiong matrix 
table(creditcard.test$Type, cl)

89/nrow(creditcard.test)
#9.888% error rate 


####testing on train data

#Setting the type to "response" option tells R to output probabilities of the form P(Y=1|X)
#as opposed to other information such as the logit
probs <- predict(reg3, newdata = creditcard.train, type = "response")

#cl is TRUE when the probability of default 0.5 otherwise its false
cl <- (probs>0.5)


#Construct confusiong matrix 
table(creditcard.train$Type, cl)

213/nrow(creditcard.train)

#10.04% error rate on train data


#Using amount but with weights of evidence this time

reg4 <- glm(Type ~ amount, data = creditwoe.train, family = binomial)
summary(reg4)

beta <- coef(reg4)

#Scatterplot
plot(creditwoe$Type~ creditwoe$amount)
#x is a vector with a range with that of DEBTINC 
x <- c(-2:2)
#Recall that our logistic regression : P(BAD = 1 | DEBTINC) = 1/(1 + e^ (-beta0 - beta1DEBTINC))
#the following line therefore plots the estimated probability of default

lines(x, 1/(1+exp(-beta[1] - beta[2]*x)), col = "red", type = "l")

## Testing reg2

probs <- predict(reg4, newdata = creditwoe.test, type = "response")

#cl is TRUE when the probability of default 0.5 otherwise its false
cl <- (probs>0.5)


#Construct confusiong matrix 
table(creditwoe.test$Type, cl)

89/nrow(creditwoe.test)
#9.888% error rate 


####testing on train data

#Setting the type to "response" option tells R to output probabilities of the form P(Y=1|X)
#as opposed to other information such as the logit
probs <- predict(reg4, newdata = creditwoe.train, type = "response")

#cl is TRUE when the probability of default 0.5 otherwise its false
cl <- (probs>0.5)


#Construct confusiong matrix 
table(creditwoe.train$Type, cl)

213/nrow(creditwoe.train)

#10.14% error rate on train data




#Contingency table with rows corresponding to WOE for DEBTINC and columns to # of BAD
T <- xtabs(~ creditwoe$amount + creditwoe$Type)
T
#Print proportion of good/bad customers for each weights of evidence value
P <- T/rowSums(T)
P
#Predict proportion of bad customers in each of the twenty bins and the dedicated proportion
#from our logistic regression

#The first way is to realize that these are the names of the rows of the table P
woe.vals <- rownames(P)
#The second way is to obtain these as the unique values of the Y$DEBTINC variable
#In this case we also need to sort them in ascending order
woe.vals <- sort(unique(creditwoe$amount))
woe.vals
#Plot the proportion of missign values against the values of the WoE for DEBTINC
plot(P[,2] ~ woe.vals, xlab = "WoE for Amount", ylab = "Proportion of GOOD customers")

#Next plot the predicted probabilities from our logistic regression model 

#Create vector x by sequential values between -2 to 2 with a stepsize of 0.01
x <- seq(-2,2,0.01)
#Obtain estimated coefficients
beta <- coef(reg4)
lines(x, 1/(1 + exp(-beta[1] - beta[2] * x)), col = "red", type = "l")


#Deviance and Simple model comparision
#Can do Chi-squared because we binned the values instead of using contiuous values

#You can obtain appropriate values from summary(logRegWOE)
summary(reg4)
pchisq(1347.2,2098, lower.tail = FALSE)
#A better way is to use functions to extract that info from the logistic regression object
pchisq(deviance(reg4), df.residual(reg4), lower.tail = FALSE)
#Can't reject null because very big so null is true

#Comparing model with only the intercept

#Chi-squared test with null model being model containing intercept only 
pchisq(reg4$null.deviance - reg4$deviance, 1, lower.tail = FALSE)
#Can reject null because very small


##Performance assessment 
#Sensitivity and Specificity

probsWOE <- predict(reg4, newdata = creditwoe.test, type = "response")
#Create confusion matrix using 0.5 for threshold for classification
table(creditwoe.test$Type, probsWOE>0.5)

#Compute sensitivity = Proportion of Bad customers correctly indentified 
Sensitivity <- T[2,2]/sum(T[2,])
Sensitivity



#Compute and plot the ROC curve
roc3 <- roc(creditwoe.test$Type, probsWOE, plot = TRUE, auc = TRUE, grid = TRUE, col = "red", main = "ROC Curve for Amount")

#adding ROC from previous logistic regression
probs <- predict(object = reg3, newdata = creditcard.test, type = "response")
roc4 <- roc(creditcard.test$Type, probs, plot = TRUE, add = TRUE, auc = TRUE, grid = TRUE, col = "blue")


roc3$auc
roc4$auc
#Compute Area Under Curve 
AUC_WOE <- auc(creditwoe.test$Type, probsWOE)
AUC_WOE

#Asking for specific values from ROC curve
coords(roc3, x= 0.8, input = "sensitivity", 
       ret = c("threshold", "specificity", "sensitivity", "accuracy", "tp", "tn", "fp", "fn"))

roc3$sensitivities

rstring <- c("threshold", "specificity", "sensitivity", "accuracy", "tp","tn","fp","fn")
coords(roc3, x=0.5, input = "threshold", ret = rstring)


#This shows that WoE is better for Amount

#Now lets do the same thing for installment_percentage




reg5 <- glm(Type ~ install_percent, data = creditcard.train, family = binomial)
summary(reg5)

beta <- coef(reg5)

#Scatterplot
plot(creditcard$Type~ creditcard$install_percent)
#x is a vector with a range with that of DEBTINC 
x <- c(0:6)
#Recall that our logistic regression : P(BAD = 1 | DEBTINC) = 1/(1 + e^ (-beta0 - beta1DEBTINC))
#the following line therefore plots the estimated probability of default

lines(x, 1/(1+exp(-beta[1] - beta[2]*x)), col = "red", type = "l")



## Testing reg5

probs <- predict(reg5, newdata = creditcard.test, type = "response")

#cl is TRUE when the probability of default 0.5 otherwise its false
cl <- (probs>0.5)


#Construct confusiong matrix 
table(creditcard.test$Type, cl)

89/nrow(creditcard.test)
#9.888% error rate 


####testing on train data

#Setting the type to "response" option tells R to output probabilities of the form P(Y=1|X)
#as opposed to other information such as the logit
probs <- predict(reg5, newdata = creditcard.train, type = "response")

#cl is TRUE when the probability of default 0.5 otherwise its false
cl <- (probs>0.5)


#Construct confusiong matrix 
table(creditcard.train$Type, cl)

213/nrow(creditcard.train)

#10.14% error rate on train data


#Using amount but with weights of evidence this time

reg6 <- glm(Type ~ install_percent, data = creditwoe.train, family = binomial)
summary(reg6)

beta <- coef(reg6)

#Scatterplot
plot(creditwoe$Type~ creditwoe$install_percent)
#x is a vector with a range with that of DEBTINC 
x <- c(0:6)
#Recall that our logistic regression : P(BAD = 1 | DEBTINC) = 1/(1 + e^ (-beta0 - beta1DEBTINC))
#the following line therefore plots the estimated probability of default

lines(x, 1/(1+exp(-beta[1] - beta[2]*x)), col = "red", type = "l")

## Testing reg6

probs <- predict(reg6, newdata = creditwoe.test, type = "response")

#cl is TRUE when the probability of default 0.5 otherwise its false
cl <- (probs>0.5)


#Construct confusiong matrix 
table(creditwoe.test$Type, cl)

89/nrow(creditwoe.test)
#9.888% error rate 


####testing on train data

#Setting the type to "response" option tells R to output probabilities of the form P(Y=1|X)
#as opposed to other information such as the logit
probs <- predict(reg6, newdata = creditwoe.train, type = "response")

#cl is TRUE when the probability of default 0.5 otherwise its false
cl <- (probs>0.5)


#Construct confusiong matrix 
table(creditwoe.train$Type, cl)

213/nrow(creditwoe.train)

#10.14% error rate on train data




#Contingency table with rows corresponding to WOE for DEBTINC and columns to # of BAD
T <- xtabs(~ creditwoe$install_percent + creditwoe$Type)
T
#Print proportion of good/bad customers for each weights of evidence value
P <- T/rowSums(T)
P
#Predict proportion of bad customers in each of the twenty bins and the dedicated proportion
#from our logistic regression

#The first way is to realize that these are the names of the rows of the table P
woe.vals <- rownames(P)
#The second way is to obtain these as the unique values of the Y$DEBTINC variable
#In this case we also need to sort them in ascending order
woe.vals <- sort(unique(creditwoe$install_percent))
woe.vals
#Plot the proportion of missign values against the values of the WoE for DEBTINC
plot(P[,2] ~ woe.vals, xlab = "WoE for Installment_Percentage", ylab = "Proportion of GOOD customers")

#Next plot the predicted probabilities from our logistic regression model 

#Create vector x by sequential values between -2 to 2 with a stepsize of 0.01
x <- seq(0,6,0.01)
#Obtain estimated coefficients
beta <- coef(reg6)
lines(x, 1/(1 + exp(-beta[1] - beta[2] * x)), col = "red", type = "l")


#Deviance and Simple model comparision
#Can do Chi-squared because we binned the values instead of using contiuous values

#A better way is to use functions to extract that info from the logistic regression object
pchisq(deviance(reg6), df.residual(reg6), lower.tail = FALSE)
#Can't reject null because very big so null is true

#Comparing model with only the intercept

#Chi-squared test with null model being model containing intercept only 
pchisq(reg6$null.deviance - reg6$deviance, 1, lower.tail = FALSE)
#Can reject null because very small


##Performance assessment 
#Sensitivity and Specificity

probsWOE <- predict(reg6, newdata = creditwoe.test, type = "response")
#Create confusion matrix using 0.5 for threshold for classification
table(creditwoe.test$Type, probsWOE>0.5)

#Compute sensitivity = Proportion of Bad customers correctly indentified 
Sensitivity <- T[2,2]/sum(T[2,])
Sensitivity



#Compute and plot the ROC curve
roc5 <- roc(creditwoe.test$Type, probsWOE, plot = TRUE, auc = TRUE, grid = TRUE, col = "red", main = "ROC Curve for Installment_Percentage")

#adding ROC from previous logistic regression
probs <- predict(object = reg5, newdata = creditcard.test, type = "response")
roc6 <- roc(creditcard.test$Type, probs, plot = TRUE, add = TRUE, auc = TRUE, grid = TRUE, col = "blue")


roc5$auc
roc6$auc
#Compute Area Under Curve 
AUC_WOE <- auc(creditwoe.test$Type, probsWOE)
AUC_WOE

#Asking for specific values from ROC curve
coords(roc5, x= 0.8, input = "sensitivity", 
       ret = c("threshold", "specificity", "sensitivity", "accuracy", "tp", "tn", "fp", "fn"))

roc5$sensitivities

rstring <- c("threshold", "specificity", "sensitivity", "accuracy", "tp","tn","fp","fn")
coords(roc5, x=0.5, input = "threshold", ret = rstring)


#This shows that WoE is the same for installment_percentage so it doesn't make a difference 
#to use them or not


#Now checking the same thing for age





reg7 <- glm(Type ~ age, data = creditcard.train, family = binomial)
summary(reg7)

beta <- coef(reg7)

#Scatterplot
plot(creditcard$Type~ creditcard$age)
#x is a vector with a range with that of DEBTINC 
x <- c(0:75)
#Recall that our logistic regression : P(BAD = 1 | DEBTINC) = 1/(1 + e^ (-beta0 - beta1DEBTINC))
#the following line therefore plots the estimated probability of default

lines(x, 1/(1+exp(-beta[1] - beta[2]*x)), col = "red", type = "l")



## Testing reg7

probs <- predict(reg7, newdata = creditcard.test, type = "response")

#cl is TRUE when the probability of default 0.5 otherwise its false
cl <- (probs>0.5)


#Construct confusiong matrix 
table(creditcard.test$Type, cl)

89/nrow(creditcard.test)
#9.888% error rate 


####testing on train data

#Setting the type to "response" option tells R to output probabilities of the form P(Y=1|X)
#as opposed to other information such as the logit
probs <- predict(reg7, newdata = creditcard.train, type = "response")

#cl is TRUE when the probability of default 0.5 otherwise its false
cl <- (probs>0.5)


#Construct confusiong matrix 
table(creditcard.train$Type, cl)

213/nrow(creditcard.train)

#10.14% error rate on train data


#Using amount but with weights of evidence this time

reg8 <- glm(Type ~ age, data = creditwoe.train, family = binomial)
summary(reg8)

beta <- coef(reg8)

#Scatterplot
plot(creditwoe$Type~ creditwoe$age)
#x is a vector with a range with that of DEBTINC 
x <- c(-1:1)
#Recall that our logistic regression : P(BAD = 1 | DEBTINC) = 1/(1 + e^ (-beta0 - beta1DEBTINC))
#the following line therefore plots the estimated probability of default

lines(x, 1/(1+exp(-beta[1] - beta[2]*x)), col = "red", type = "l")

## Testing reg8

probs <- predict(reg8, newdata = creditwoe.test, type = "response")

#cl is TRUE when the probability of default 0.5 otherwise its false
cl <- (probs>0.5)


#Construct confusiong matrix 
table(creditwoe.test$Type, cl)

89/nrow(creditwoe.test)
#9.888% error rate 


####testing on train data

#Setting the type to "response" option tells R to output probabilities of the form P(Y=1|X)
#as opposed to other information such as the logit
probs <- predict(reg8, newdata = creditwoe.train, type = "response")

#cl is TRUE when the probability of default 0.5 otherwise its false
cl <- (probs>0.5)


#Construct confusiong matrix 
table(creditwoe.train$Type, cl)

213/nrow(creditwoe.train)

#10.14% error rate on train data




#Contingency table with rows corresponding to WOE for DEBTINC and columns to # of BAD
T <- xtabs(~ creditwoe$age + creditwoe$Type)
T
#Print proportion of good/bad customers for each weights of evidence value
P <- T/rowSums(T)
P
#Predict proportion of bad customers in each of the twenty bins and the dedicated proportion
#from our logistic regression

#The first way is to realize that these are the names of the rows of the table P
woe.vals <- rownames(P)
#The second way is to obtain these as the unique values of the Y$DEBTINC variable
#In this case we also need to sort them in ascending order
woe.vals <- sort(unique(creditwoe$age))
woe.vals
#Plot the proportion of missign values against the values of the WoE for DEBTINC
plot(P[,2] ~ woe.vals, xlab = "WoE for Installment_Percentage", ylab = "Proportion of GOOD customers")

#Next plot the predicted probabilities from our logistic regression model 

#Create vector x by sequential values between -2 to 2 with a stepsize of 0.01
x <- seq(-1,1,0.01)
#Obtain estimated coefficients
beta <- coef(reg8)
lines(x, 1/(1 + exp(-beta[1] - beta[2] * x)), col = "red", type = "l")


#Deviance and Simple model comparision
#Can do Chi-squared because we binned the values instead of using contiuous values

#A better way is to use functions to extract that info from the logistic regression object
pchisq(deviance(reg8), df.residual(reg8), lower.tail = FALSE)
#Can't reject null because very big so null is true

#Comparing model with only the intercept

#Chi-squared test with null model being model containing intercept only 
pchisq(reg8$null.deviance - reg8$deviance, 1, lower.tail = FALSE)
#Can reject null because very small


##Performance assessment 
#Sensitivity and Specificity

probsWOE <- predict(reg8, newdata = creditwoe.test, type = "response")
#Create confusion matrix using 0.5 for threshold for classification
table(creditwoe.test$Type, probsWOE>0.5)

#Compute sensitivity = Proportion of Bad customers correctly indentified 
Sensitivity <- T[2,2]/sum(T[2,])
Sensitivity



#Compute and plot the ROC curve
roc7 <- roc(creditwoe.test$Type, probsWOE, plot = TRUE, auc = TRUE, grid = TRUE, col = "red", main = "ROC Curve for Age")

#adding ROC from previous logistic regression
probs <- predict(object = reg7, newdata = creditcard.test, type = "response")
roc8 <- roc(creditcard.test$Type, probs, plot = TRUE, add = TRUE, auc = TRUE, grid = TRUE, col = "blue")


roc7$auc
roc8$auc
#Compute Area Under Curve 
AUC_WOE <- auc(creditwoe.test$Type, probsWOE)
AUC_WOE

#Asking for specific values from ROC curve
coords(roc7, x= 0.8, input = "sensitivity", 
       ret = c("threshold", "specificity", "sensitivity", "accuracy", "tp", "tn", "fp", "fn"))

roc7$sensitivities

rstring <- c("threshold", "specificity", "sensitivity", "accuracy", "tp","tn","fp","fn")
coords(roc7, x=0.5, input = "threshold", ret = rstring)


#This shows that WoE is the same for age so it doesn't make a difference 
#to use them or not


##Tackling Missing values 

v = apply(creditcard, 2, anyNA)
v
#Savings and Time at Address have missing values

pMiss(creditcard$savings)
pMiss(creditcard$time_at_address)


#vector with numeric element of length ncol(Data)
v = vector(length = ncol(creditcard), mode = "numeric")
v

for (i in 1:ncol(Data)) { v[i] <- pMiss(creditcard[,i])}
v

#Adds two columns which have a 1 when the value is missing for savings and time at address
creditcard.ind <- na_indicator(creditcard)
creditcard.ind

ind0 <- glm(M_savings ~ 1, data = creditall.train, family = binomial)
ind1 <- glm(M_savings ~., data = creditall.train, family = binomial)
ind2 <- glm(M_time_at_address ~. , data = creditall.train, family = binomial)
ind3 <- glm(M_time_at_address ~ 1, data = creditall.train, family = binomial)
summary(ind2)

perf <- predict(ind2, newdata = creditall.test, type = "response")
rocperf1 <- roc(creditall.test$M_savings, perf, plot = TRUE, auc = TRUE, grid = TRUE, col = "red")

stepsavings <- step(ind0, scope = formula(ind1), direction = "both")
formula(stepsavings)

perfs <- predict(stepsavings, newdata = creditall.test, type = "response")
rocperf1 <- roc(creditall.test$M_savings, perfs, plot = TRUE, auc = TRUE, grid = TRUE, col = "red", main = "ROC for M_Savings")

steptime <- step(ind3, scope = formula(ind2), direction = "both")
formula(steptime)

perft <- predict(stepsavings, newdata = creditall.test, type = "response")
rocperf2 <- roc(creditall.test$M_time_at_address, perft, plot = TRUE, auc = TRUE, grid = TRUE, col = "red", main = "ROC for M_Time_at_Address")




#Multiple imputation of missing values
md.pattern(creditcard)
#shows that there are 2798 with complete fields after the classification
#58 people that have only savings missing
#140 people with time at address missing
#4 people with both missing


mice_plot <- aggr(creditcard, numbers = TRUE, sortVars = TRUE)

#The parameters specified are the default values (except for m)
creditcard.imp <- mice(creditcard, m=1, method = "pmm", seed = 10)


#Scatter plot of imputed data
xyplot(creditcard.imp, age  ~ savings + time_at_address, pch = 18, alpha = 0.4, auto.key = TRUE)

densityplot(creditcard.imp)


creditcard.imp2 <- mice(creditcard, m = 1, method = "mean", seed = 10)
xyplot(creditcard.imp2, age ~ savings + time_at_address, pch = 18, alpha = 0.4, auto.key = TRUE)

densityplot(creditcard.imp2)



#Missing values have been replaced with the imputed values in the first of the five datasets
creditcard.comp <- complete(creditcard.imp,1)
#Verify that there are no missing values in the dataset
apply(creditcard.comp,2,anyNA)


str(creditcard.ind)
names(creditcard.ind)
names(creditcard.ind)[14:15]

#Created dataset with imputed values to remove NA's and a NA indicator since it is signficant
creditcard.all <- cbind(creditcard.comp, creditcard.ind[,14:15])
str(creditcard.all)
apply(creditcard.all,2,anyNA)


creditall.train <- creditcard.all[train,]
creditall.test <- creditcard.all[-train,]

regall0 <- glm(Type ~ 1, data = creditall.train, family = binomial)
summary(regall0)

regall1 <- glm(Type ~., data = creditall.train, family = binomial)
summary(regall1)


stepall <- step(regall0, scope = formula(regall1), direction = "both")
formula(stepall)
summary(stepall)
stepall$anova


stepall2 <- step(regall0, scope = formula(regall1), direction = "both", k = log(nrow(creditall.train)))
formula(stepall2)
summary(stepall2)


probs1 <- predict(stepall, newdata = creditall.test, type = "response")
rocfin1 <- roc(creditall.test$Type, probs1, plot = TRUE, auc = TRUE, grid = TRUE, col = "red")

#adding ROC from previous logistic regression
probs2 <- predict(object = stepall2, newdata = creditall.test, type = "response")
rocfin2 <- roc(creditall.test$Type, probs2, plot = TRUE, add = TRUE, auc = TRUE, grid = TRUE, col = "blue")




#Cross Validation

set.seed(2)
#10-fold
my_folds <- sample(1:10, nrow(creditall.train), replace = TRUE)

out <- crossValid(creditall.train, y= "Type", folds = my_folds)
summary(out$BestModel)
formula(out$BestModel)
coef(out$BestModel)


probs3 <- predict(object = out$BestModel, newdata = creditall.test, type = "response")

plotCVauc(out)




#Using creditwoenona
str(creditwoenona)
apply(creditwoenona.train, 2, anyNA)

regwoe0 <- glm(Type ~ 1, data = creditwoenona.train, family = binomial)
summary(regwoe0)

regwoe1 <- glm(Type ~., data = creditwoenona.train, family = binomial)
summary(regwoe1)


stepwoe <- step(regwoe0, scope = formula(regwoe1), direction = "both")
formula(stepwoe)
summary(stepwoe)
stepwoe$anova


stepwoe2 <- step(regwoe0, scope = formula(regwoe1), direction = "both", k = log(nrow(creditwoenona.train)))
formula(stepwoe2)
summary(stepwoe2)

#Cross Validation


outwoe <- crossValid(creditwoenona.train, y= "Type", folds = my_folds)
summary(outwoe$BestModel)
formula(outwoe$BestModel)
coef(outwoe$BestModel)


plotCVauc(outwoe)





WOE1 <- predict(stepwoe, newdata = creditwoenona.test, type = "response")

rocfin3 <- roc(creditwoenona.test$Type, WOE1, plot = TRUE, auc = TRUE, grid = TRUE, col = "red", main = "ROC curve with AIC", sub = "Red is with WoE, Blue is with imputed")
rocfin1 <- roc(creditall.test$Type, probs1, plot = TRUE, auc = TRUE, grid = TRUE, col = "blue", add = TRUE)

#adding ROC from previous logistic regression
WOE2 <- predict(object = stepwoe2, newdata = creditwoenona.test, type = "response")
rocfin4 <- roc(creditwoenona.test$Type, WOE2, plot = TRUE, add = TRUE, auc = TRUE, grid = TRUE, col = "purple")

rocfin2 <- roc(creditall.test$Type, probs2, plot = TRUE, add = TRUE, auc = TRUE, grid = TRUE, col = "green")


WOE3 <- predict(object = outwoe$BestModel, newdata= creditwoenona.test, type = "response")

rocfin6 <- roc(creditwoenona.test$Type, WOE3, plot = TRUE, add = TRUE, auc = TRUE, grid = TRUE, col = "purple")

rocfin5 <- roc(creditall.test$Type, probs3, plot = TRUE, add = TRUE, auc = TRUE, grid = TRUE, col = "green")

sum(creditcard.test$Type == "1")




rocfin3$specificities

rstring <- c("threshold", "specificity", "sensitivity", "accuracy", "tp","tn","fp","fn")
coords(rocfin3,x = 0.90, input = "specificity", ret = rstring)


coords(rocfin3,x = 0.75, input = "sensitivity", ret = rstring)
coords(rocfin1,x = 0.75, input = "sensitivity", ret = rstring)






###Decision Trees



tr1 <- rpart( Type ~ . , data = creditcard.train, method = "class")
plot(tr1)
text(tr1)
printcp(tr1)
print(tr1)
tr2 <- rpart( Type ~ . , data = creditcard.train, method = "class", cp = 0.040)
plot(tr2)
text(tr2)


tr3 <- rpart(Type ~ ., data = creditcard.train, method = "class", control = rpart.control(xval = 10, cp = 0))
plot(tr3)
printcp(tr3)
plotcp(tr3)
bestTree <- prune(tr3, cp = 0.005)
plot(bestTree)
text(bestTree, cex = 0.6)
printcp(bestTree)


trE <- rpart(Type ~ . , data = creditcard.train, method = "class", parms = list(split = "information"))
plot(trE)
text(trE)

#Generate probability predictions for the test using fully grown decision tree
prob1 <- predict(tr3, newdata = creditcard.test, type = "prob")
head(prob1)

roc9 <- roc(creditcard.test$Type, prob1[,2], plot = TRUE, col = "red", grid = TRUE, main = "ROC Curve comparing Decision Trees")

#Probability predictions for pruned decision tree
prob2 <- predict(bestTree, newdata= creditcard.test, type = "prob")
roc10 <- roc(creditcard.test$Type, prob2[,2], plot = TRUE, add = TRUE, col = "blue")

tr4 <- rpart( Type ~ . , data = creditwoe.train, method = "class")


#Probability predictions for default tree that uses cp = 0.01
prob3 <- predict(tr4, newdata = creditwoe.test, type = "prob")
roc11 <- roc(creditcard.test$Type, prob3[,2], plot = TRUE, add = TRUE, col = "green")

trE <- rpart(Type ~ . , data = creditcard.train, method = "class", parms = list(split = "information"))

prob4 <- predict(trE, newdata = creditall.test, type = "prob")
roc12 <- roc(creditcard.test$Type, prob4[,2], plot = TRUE, add = TRUE, col = "black")


roc13 <- roc(creditwoenona.test$Type, WOE1, plot = TRUE, add = TRUE, auc = TRUE, grid = TRUE, col = "purple")


printcp(tr3)
tr3 <- rpart(Type ~ ., data = creditcard.train, method = "class", control = rpart.control(xval = 10, cp = 0))
prob1 <- predict(tr3, newdata = creditcard.test, type = "prob")
roc9 <- roc(creditcard.test$Type, prob1[,2], plot = TRUE, col = "red", grid = TRUE, main = "ROC Curve comparing Decision Trees")

tr4 <- rpart( Type ~ . , data = creditwoe.train, method = "class")


#Probability predictions for default tree that uses cp = 0.01
prob3 <- predict(tr4, newdata = creditwoe.test, type = "prob")
roc11 <- roc(creditcard.test$Type, prob3[,2], plot = TRUE, add = TRUE, col = "green")


tr5 <- rpart( Type ~ . , data = creditall.train, method = "class")

prob4 <- predict(tr5, newdata = creditall.test, type = "prob")
roc14 <- roc(creditcard.test$Type, prob4[,2], plot = TRUE, add = TRUE, col = "blue")


tr6 <- rpart(Type ~ ., data = creditall.train, method = "class", control = rpart.control(xval = 10, cp = 0))
prob5 <- predict(tr6, newdata = creditall.test, type = "prob")
roc15 <- roc(creditcard.test$Type, prob5[,2], plot = TRUE, add = TRUE, col = "black")
printcp(tr6)

tr7 <- rpart( Type ~ . , data = creditcard.ind[train,], method = "class")

prob6 <- predict(tr7, newdata = creditcard.ind[-train,], type = "prob")
roc16 <- roc(creditcard.ind[-train,]$Type, prob6[,2], plot = TRUE, add = TRUE, col = "purple")


tr8<- rpart(Type ~ ., data = creditcard.ind[train,], method = "class", control = rpart.control(xval = 10, cp = 0))
prob7 <- predict(tr8, newdata = creditcard.ind[-train,], type = "prob")
roc17 <- roc(creditcard.ind[-train,]$Type, prob7[,2], plot = TRUE, add = TRUE, col = "yellow")

roc13 <- roc(creditwoenona.test$Type, WOE1, plot = TRUE, add = TRUE, auc = TRUE, grid = TRUE, col = "magenta", main = "Comparing Decision Tree to Logistic Regression")
