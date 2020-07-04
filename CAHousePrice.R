library(tidyverse)
#reading data
caH = read_csv("http://www.utdallas.edu/~vds190000/housing.csv")
summary(caH)

#boxplot for total_bedrooms
ggplot(caH,aes("var",total_bedrooms))+
  geom_boxplot()+
  xlab(" ")+
  ylab("Total Bedrooms")+
  scale_x_discrete(breaks=NULL)+
  labs(title = "Boxplot of Total Bedrooms")+
  coord_flip()

#calculating mean and median of total_bedrooms after removing NA values
totalBedroomMean = mean(caH$total_bedrooms, na.rm = TRUE)
totalBedroomMedian = median(caH$total_bedrooms, na.rm = TRUE)

#histogram of total bedrooms
ggplot(data=caH, aes(x=caH$total_bedrooms)) + 
  geom_histogram(bins = 40, col="black", fill="blue",alpha = .2) + 
  labs(title="Histogram for Total Bedrooms", x="Total Bedrooms", y="Count")

#imputing missing values with median
caH$total_bedrooms[is.na(caH$total_bedrooms)] = totalBedroomMedian

#converting categorical variable into a factor
caH$ocean_proximity = as.factor(caH$ocean_proximity)
levels(caH$ocean_proximity)

#barplot for categorial variable
ggplot(data = caH,mapping = aes(x=factor(ocean_proximity)))+
  geom_bar(color="black", fill="light blue")+
  labs(title="Ocean Proximity of CA houses", x="ocean proximity")

#histograms for quantitative variables
data1 = subset(caH , select=-c(ocean_proximity))
data1 %>% gather() %>% head()
ggplot(gather(data1), aes(value)) + 
  geom_histogram(bins = 20, color="black", fill="purple", alpha=0.3) + 
  facet_wrap(~key, scales = 'free_x')+
  labs(y="Frequency")

#correlation plots
require(corrplot)
require(RColorBrewer)
corrplot(cor(caH[,1:9]),tl.cex = 0.7, tl.col = "black",cl.cex = 0.7,cl.ratio = 0.2,cl.align.text = "l",order = "hclust",col = brewer.pal(n=8,name="PuOr"))
corrplot(cor(caH[,1:9]),method="number",number.cex=0.7,tl.cex = 0.7, tl.col = "black",cl.cex = 0.7,cl.ratio = 0.2,cl.align.text = "l",order = "hclust",col = brewer.pal(n=8,name="PuOr"))

colnames(caH)
nrow(caH)
ncol(caH)

#map with location and median income
plot_map2 = ggplot(caH, 
                   aes(x = longitude, y = latitude, color=median_income))+
  geom_point()+
  scale_color_gradient(low="light blue", high="red")+
  labs(title="Location and Median Income of Households",color="Median income (in 1000's of USD)",x="Longitude", y="Latitude")
plot_map2

#boxplots of price and ocean proximity
ggplot(caH, aes(x=ocean_proximity, y=median_house_value))+
  geom_boxplot(color="black", fill="yellow", alpha=0.2)+
  labs(title="Boxplots of median house value by ocean proximity", x="Ocean Proximity", y="Median house value ($)")

#correlation of median_house_value and median_income
ggplot(data = caH)+
  geom_point(mapping=aes(x=median_house_value, y=median_income),color="light blue")+
  labs(title = "Correlation of Median house value and median income", x="Median house value", y="Median income")

ggplot(data = caH)+
  geom_point(mapping=aes(x=log2(median_house_value), y=median_income),color="light blue")+
  labs(title = "Correlation of log(Median house value) and median income", x="Median house value", y="Median income")

#map with location, population and price
require(scales)
#options(scipen = 999)
plot_map1 = ggplot(caH, 
                   aes(x = longitude, y = latitude))+
  geom_point(aes(color=median_house_value, size=population))+
  scale_color_distiller(palette = "RdYlBu")+
  theme_pander()
labs(title="Location and Population vs Price",color="Median house value ($)", size="Population", x="Longitude", y="Latitude")
plot_map1

#linear regression modelling

fit1 = lm(caH$median_house_value~caH$median_income +caH$ocean_proximity, data=caH)
summary(fit1)

#log transformation of response variable improves fit
fit1.1 = lm(log(caH$median_house_value)~caH$median_income +caH$ocean_proximity, data=caH)
summary(fit1.1)

fit2 = lm(log(caH$median_house_value)~., data=caH)
summary(fit2)

anova(fit1.1,fit2)


#residual plot and QQ plot

plot(fitted(fit2),resid(fit2), xlab ="Fitted values", ylab="Residuals", main="Fitted vs Residuals", col="orange")
abline(h=0)
qqnorm(resid(fit2), col="orange", main = "Normal QQ Plot")
qqline(resid(fit2))


#plots to show relationship between model and variables
library(visreg)
lm.fit <- lm(log(median_house_value)~median_income+ocean_proximity,data=caH)
visreg(lm.fit,"ocean_proximity",gg=TRUE)
visreg(lm.fit,"median_income",gg=TRUE)


  





