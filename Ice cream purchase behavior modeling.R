library(rsq)
library(corrplot)


#### Purchases of Ice Cream
ice = read.csv("ice_cream.csv")

## explore
names(ice)

## create a new variable for price per unit
priceper1 = (ice$price_paid_deal + ice$price_paid_non_deal) / ice$quantity
y <- log(1 + priceper1)
hist(y)
hist(priceper1)
## collect some variables of interest
## create a data.frame
x <- ice[ ,c("flavor_descr", "size1_descr", "household_income", "household_size")]
colnames(x)

## relevel 'flavor' to have baseline of vanilla
x$flavor_descr <- relevel(x$flavor_descr, "VAN")
## coupon usage
x$usecoup = factor(ice$coupon_value > 0)
x$couponper1 <- ice$coupon_value / ice$quantity
#x$usecoup
## organize some demographics
x$region <- factor(ice$region, levels=1:4, labels=c("East","Central","South","West"))
x$married <- factor(ice$marital_status==1)
x$race <- factor(ice$race, levels=1:4, labels=c("white", "black", "asian", "other"))
x$hispanic_origin <- ice$hispanic_origin==1
x$microwave <- ice$kitchen_appliances %in% c(1,4,5,7)
x$dishwasher <- ice$kitchen_appliances %in% c(2,4,6,7)
x$sfh <- ice$type_of_residence == 1
x$internet <- ice$household_internet_connection == 1
x$tvcable <- ice$tv_items > 1

## combine x and y
## cbind is "column bind".  It takes two dataframes and makes one.
xy <- cbind(x,y)

## fit the regression
fit <- glm(y~., data=xy) 
## Summary and R-squared for glm
summary(fit)
rsq(fit)
rsq(fit,adj=TRUE)

#doing the same using lm()
#Adding a column for total spent
xy_1 <- xy
xy_1 <- xy_1[,-c(16)]
xy_1$TOTAL <- ice$total_spent

model_sol_ex_1a <- lm(y ~ .,xy_1)
summary(model_sol_ex_1a)
colnames(xy_1)

#Checking relationship with independant variables
cor(xy_1$household_size,xy_1$household_income)
plot(xy_1$usecoup,xy_1$couponper1)
plot(priceper1,xy_1$couponper1)
plot(xy_1$household_income,priceper1)
plot(xy_1$household_size,priceper1)

#Categorical variables
boxplot(y~xy_1$household_income)
summary(aov(y~xy_1$household_income))

boxplot(y~xy_1$race)
summary(aov(y~xy_1$race))

boxplot(y~xy_1$region)
summary(aov(y~xy_1$region))
##
colnames(xy_1)

model_sol_ex_1a <- lm(priceper1 ~ .,xy_1)
summary(model_sol_ex_1a)


#Model trial 1 - removed microwave and dishwasher
colnames(xy_1)
x_1 <-  xy_1[,-c(11,12)]
model_sol_2 <- lm(y ~.,x_1)
summary(model_sol_2)

#Model trial 2 - removed microwave,dishwasher, hispanic origin
colnames(x_1)
x_2 <-  x_1[,-c(10)]
model_sol_3 <- lm(y ~.,x_2)
summary(model_sol_3)


#Model trial 3 - removed microwave,dishwasher, hispanic origin, internet, cable
colnames(x_2)
x_3 <-  x_2[,-c(11,12)]
model_sol_4 <- lm(y ~.,x_3)
summary(model_sol_4)

#Model trial 4 - removed microwave,dishwasher, hispanic origin, internet, cable, changed race to numeric
# create a dummy and see if shows results different from factors
colnames(x_3)
x_4 <- x_3
x_4$race <-  as.numeric(ifelse(x_3$race=='black' | x_3$race=='other',1,0))
class(x_4$race)
model_sol_5 <- lm(y ~.,x_4)
summary(model_sol_5)

#Model trial 5 - removed microwave,dishwasher, hispanic origin, internet, cable,added flavor type
colnames(x_4)
x_4$flavr_type <- ice$formula_descr
model_sol_6 <- lm(y ~.,x_4)
summary(model_sol_6)
#not significant -doesn't bring any value to the model
#Removing it again
colnames(x_4)
x_4 <- x_4[,c(-12)]
model_sol_6 <- lm(y ~.,x_4)
summary(model_sol_6) #the most sorted regression
#Low correlation
cor(y,x_4$TOTAL)

#Without log transform -better but not much improvement
#and we don't know why there is a transform so this is not an option
model_sol_trial <- lm(priceper1 ~.,x_4)
summary(model_sol_trial)

hist(y)
## grab the non-intercept p-values from a glm
## -1 to drop the intercept, 4 is 4th column
pvals <- summary(model_sol_6)$coef[-1,4] 

## source the fdr_cut function
source("fdr.R")
fdr(pvals,0.05) 
