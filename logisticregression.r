#uncomment to download this package that I used if you don't have it already
#install.packages("caTools")
library(caTools)

#use this to upload dataset file.choose() will bring up a pop-up window to choose file. choose the german credit data file.
germancredit <- read.csv(file.choose())

#changing categorical variables to factors for use in the model 

factors <- c(1, 2, 4, 5, 7, 8, 10, 11, 12, 13, 15, 16, 18, 20, 21)

creditfactors <- germancredit

for (i in factors) {
  creditfactors[, i] = factor(creditfactors[, i])
}

#full model with full data - baseline
fullmod <- glm(Creditability ~., family='binomial'(link='logit'), data=creditfactors)

#null model
nullmodel <- glm(Creditability ~ 1, family='binomial' (link = 'logit'), data = creditfactors)

#step on full model with full data going backward
stepattrib <- step(fullmod, Creditability~., family='binomial', data=creditfactors)

#stepwise regression with null model going both ways forward and backward
stepattrib <- step(nullmodel, scope=list(upper=fullmod), data=creditfactors, direction = 'both')


##starting to split data into training/and test##


#splitting the data using sample.split()
#using a ration of 80/20
split <- sample.split(creditfactors$Creditability, SplitRatio = 0.8)


#dividing train and test set using subset()
train = subset(creditfactors, split==T)
test  = subset(creditfactors, split==F)


#fitting model to Creditability vs. all
mod <- glm(Creditability ~., family='binomial'(link='logit'), data=train)


#using stepwise function to evaluate attributes for feature selection
stepattrib <- step(mod, Creditability~., family='binomial', data=creditfactors)


#took the attributes from  the stepwise regression

stepmodel <- glm(Creditability ~ Account.Balance + Duration.of.Credit..month. + 
                   Payment.Status.of.Previous.Credit + Purpose + Value.Savings.Stocks + 
                   Guarantors + Instalment.per.cent + Sex...Marital.Status + 
                   Credit.Amount + Foreign.Worker + Duration.in.Current.address + 
                   Length.of.current.employment + Age..years. + No.of.Credits.at.this.Bank, family='binomial'(link='logit'), data=train)


#testing model's accuracy
#type = response will output probabilities in the form 
#of P(y=1|X).
steptestmod <- predict(stepmodel, newdata = test, type ='response')
#an ifelse statement to have all prob classed into 1 or 0 for dep. var.
steptestmod <- ifelse(steptestmod > 0.5,1,0)


#contstructing confusion matrix
table(ActualValue = test$Creditability, PredictedValue = steptestmod > 0.5)


#finding accuracy of new model
stepclassiferror <- mean(steptestmod != test$Creditability)
accustepmod <- paste('Accuracy',1-stepclassiferror)
accustepmod
#Accuracy is 0.75"
