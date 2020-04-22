library(ggplot2)
library(forcats)
library(ggcorrplot)
library(stringr)

ice <- read.csv("ice_cream.csv")

str(ice)

table(ice$price_paid_deal >0 , ice$price_paid_non_deal > 0)

addmargins(table(ice$promotion_type, exclude = NULL))
addmargins(table(ice$promotion_type))

addmargins(table(ice$size1_descr))

length(unique(ice$flavor_descr))

addmargins(table(ice$flavor_descr, ice$quantity))
addmargins(table(ice$flavor_descr))

length(unique(ice$household_id))

addmargins(table(ice$household_id, ice$quantity))

## create a new variable for price per unit
priceper1 = (ice$price_paid_deal + ice$price_paid_non_deal) / ice$quantity
y <- log(1 + priceper1)

## collect some variables of interest
## create a data.frame
x <- ice[ ,c("flavor_descr", "size1_descr", "household_income", "household_size")]

## relevel 'flavor' to have baseline of vanilla
x$flavor_descr <- relevel(x$flavor_descr, "VAN")
## coupon usage
x$usecoup = factor(ice$coupon_value > 0)
x$couponper1 <- ice$coupon_value / ice$quantity
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

xy <- cbind(x,y)

xy$child <- factor(ice$age_and_presence_of_children)

xy$total_spend = ice$total_spent

xy <- cbind(xy, ice$census_tract_county_code)

plot(xy$household_size, xy$y)

plot(xy$flavor_descr, xy$y)

#Bar plot for flavors and the sales quantity
flavor_count <- as.data.frame(table("Flavors" = ice$flavor_descr))
flavor_count1 <- flavor_count[order(flavor_count$Freq, decreasing = TRUE),][1:10,]

flavor_count1$Flavors1 = str_wrap(flavor_count1$Flavors, width = 10)
flavor_count1

g <- ggplot(flavor_count1, aes(x = reorder(Flavors1,-Freq), weight = Freq))
g + geom_bar(width = 0.5, fill = "deepskyblue3") + 
  theme(axis.text.x = element_text(vjust=0.6)) + 
  labs(title="Popularity of Flavors", 
       x="Flavor",
       y="Quantity Sold")

data = cbind("y" = xy$y, "income" = xy$household_income, "size" = xy$household_size, "couponper1" = xy$couponper1)
corr <- round(cor(data), 2)
ggcorrplot(corr,  
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           colors = c("tomato2", "ghostwhite", "deepskyblue3"), 
           title="Correlogram of variables used in the given model", 
           ggtheme=theme_bw)

ice$PricePaid <- ice$price_paid_deal + ice$price_paid_non_deal
data1 = cbind("quantity" = ice$quantity, "price_paid" = ice$PricePaid, "coupon_value" = ice$coupon_value, "total_spent" = ice$total_spent, "household_size" = ice$household_size, "household_income" = ice$household_income)
ggcorrplot(cor(data1),  
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           colors = c("tomato2", "ghostwhite", "deepskyblue3"), 
           title="Correlogram of variables in the dataset", 
           ggtheme=theme_bw)

ice$age_and_presence_of_children = factor(ice$age_and_presence_of_children)
ggplot(ice, aes(x=household_size, y=quantity)) + 
  geom_point(aes(col = age_and_presence_of_children)) + 
  labs(x="Household Size",  
       y="Quantity", 
       title="Scatterplot") +
  scale_x_continuous(limits = c(1,9), breaks = c(1,2,3,4,5,6,7,8,9))


ggplot(ice, aes(x=household_size, y=PricePaid)) + 
  geom_point(aes(col = age_and_presence_of_children)) + 
  labs(x="Household Size",  
       y="PricePaid", 
       title="Scatterplot") +
  scale_x_continuous(limits = c(1,9), breaks = c(1,2,3,4,5,6,7,8,9))


ice$region <- factor(ice$region, levels = c(1,2,3,4), labels=c("East","Central","South","West"))

ggplot(ice, aes(region, PricePaid)) + 
  geom_boxplot(fill="plum") + 
  labs(x="Region",  
       y="PricePaid", 
       title="Boxplot") +
  scale_y_continuous(limits = c(0, 40), breaks = c(5, 10, 15, 20, 25, 30, 35, 40))


length(xy$y == 0)

plot(xy$tvcable, xy$y)

plot(xy$race, xy$y)

xy$promotion_type = factor(ice$promotion_type >= 1)
xy$promotion_type <- is.na(xy$promotion_type) == 0

ggplot(xy, aes(promotion_type, y)) + 
  geom_boxplot(fill="deepskyblue2") + 
  labs(x="Promotion",  
       y="PricePerUnit", 
       title="Promotion vs Price Per Unit")