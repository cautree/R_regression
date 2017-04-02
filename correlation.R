#Setting the use argument to "pairwise.complete.obs" allows cor() 
#to compute the correlation coefficient for those observations where the values of x and y 
#are both not missing.

load(url("http://s3.amazonaws.com/assets.datacamp.com/course/dasi/nc.Rdata"))
# list names in dataset
names(nc)
dim(nc)
library(ggplot2)
library(dplyr)


nc %>%
  summarize(N = n(), r = cor(weight, mage))

# Compute correlation for all non-missing pairs
nc %>%
  summarize(N = n(), r = cor(weight, mage, use = "pairwise.complete.obs"))


anscombe = read.csv("anscombe.csv")
anscombe

names(diamonds)
data(diamonds)
diamonds%>%
  ggplot(aes(carat,price))+
  geom_point()+
  geom_smooth(method="lm", se=TRUE)



#While the geom_smooth(method = "lm") function is useful for drawing linear models 
#on a scatterplot, it doesn't actually return the characteristics of the model.
#As suggested by that syntax, however, the function that creates linear models is lm(). 
#This function generally takes two arguments:

#A formula that specifies the model
#A data argument for the data frame that contains the data you want to use to fit the model
#The lm() function return a model object having class "lm". 
#This object contains lots of information about your regression model, 
#including the data used to fit the model, the specification of the model, 
#the fitted values and residuals, etc

install.packages("broom")
library(broom)


#An "lm" object contains a host of information about the regression model that you fit. 
#There are various ways of extracting different pieces of information.

#The coef() function displays only the values of the coefficients. 
#Conversely, the summary() function displays not only that information, 
#but a bunch of other information, including the associated standard error and p-value 
#for each coefficient, the R2R2, adjusted R2R2, and the residual standard error. 
#The summary of an "lm" object in R is very similar to the output you would see 
#in other statistical computing environments (e.g. Stata, SPSS, etc.)

data("diamonds")

mod =lm(price~carat, data=diamonds)
coef(mod)
summary(mod)

#he mean of the fitted values must equal the mean of the response variable, very close
mean(diamonds$price)
mean(fitted.values(mod))

# Mean of the residuals
#The least squares fitting procedure guarantees that the mean of the residuals is zero 
mean(residuals(mod))



#As you fit a regression model, there are some quantities (e.g. R2R2) 
#that apply to the model as a whole, while others apply to each observation (e.g. y^iy^i). 
#If there are several of these per-observation quantities, 
#it is sometimes convenient to attach them to the original data as new variables.

#The augment() function from the broom package does exactly this. 
#It takes a model object as an argument and returns a data frame 
#that contains the data on which the model was fit, 
#along with several quantities specific to the regression model, 
#including the fitted values, residuals, leverage scores, and standardized residuals.

# Create bdims_tidy
bdims_tidy = augment(mod)
bdims_tidy

# Glimpse the resulting data frame
library(dplyr)
glimpse(bdims_tidy)
head(bdims_tidy)


#We can use the predict() function to generate expected values 
#for the weight of new individuals. 
#We must pass the data frame of new observations through the newdata argument.



#The geom_smooth() function makes it easy to add a simple linear 
#regression line to a scatterplot of the corresponding variables. 
#And in fact, there are more complicated regression models that can 
#be visualized in the data space with geom_smooth(). 
#However, there may still be times when we will want to add regression 
#lines to our scatterplot manually. To do this, we will use the geom_abline() function, 
#which takes slope and intercept arguments. Naturally, 
#we have to compute those values ahead of time, 
#but we already saw how to do this (e.g. using coef()).

#The coefs data frame contains the model estimates retrieved from coef(). 
#Passing this to geom_abline() as the data argument will enable you to 
#draw a straight line on your scatterplot.

coefs=as.data.frame(t(as.data.frame(coef(mod))))

ggplot(data = diamonds, aes(x = carat, y = price)) + 
  geom_point() + 
  geom_abline(data =coefs, 
              aes(intercept = `(Intercept)`, slope = carat),  
              color = "dodgerblue")
     

mod_diamonds = lm(price ~carat, data=diamonds)  

mod_diamonds %>%
  augment() %>%
  summarise (SSE= sum(.resid^2),
             SSE_aslo = (n()-1)*var(.resid))

mod_mod_diamonds = augment(mod_diamonds)
summary(mod_diamonds)

#One way to assess strength of fit is to consider 
#how far off the model is for a typical case. 
#That is, for some observations, 
#the fitted value will be very close to the actual value, 
#while for others it will not. 
#The magnitude of a typical residual can give us a sense of generally how close our estimates are.

#However, recall that some of the residuals are positive, 
#while others are negative. In fact, 
#it is guaranteed by the least squares fitting procedure that 
#the mean of the residuals is zero. 
#Thus, it makes more sense to compute the square root of the mean squared residual, 
#or root mean squared error (RMSE).
#R calls this quantity the residual standard error.


# Compute the mean of the residuals
mean(residuals(mod))

# Compute RMSE
sqrt(sum(residuals(mod)^2) / df.residual(mod))


#null model
model_null = lm(price~1, data=diamonds)
model_null %>%
  augment(diamonds) %>%
  summarise(SST=sum(.resid^2))

mod_null_diamonds = augment(model_null)
mod_null_diamonds 

# R squared
# coefficient of determinant

#Recall that the coefficient of determination (R2), can be computed as

#R2=1−SSE/SST=1−Var(e)/Var(y),
#where e is the vector of residuals and 
#y is the response variable. 
#This gives us the interpretation of R2R2 as the percentage 
#of the variability in the response that is explained by the model, 
#since the residuals are the part of that variability that remains unexplained by the model.


# View model summary
summary(mod)


# Compute R-squared
bdims_tidy %>%
  summarize(var_y = var(price), var_e = var(.resid)) %>%
  mutate(R_squared = 1-var_e/var_y)


# Compute SSE for null model

mod_null_diamonds %>%
  summarize(SSE = var(.resid))

# Compute SSE for regression model
mod_mod_diamonds %>%
  summarize(SSE =var(.resid))


#outliers can affect the regression line, look for outliers
#.hat represent the leverage in the augment function, large leverage value, the close to the line, the smaller the leverage value
mod_diamonds%>%
  augment()%>%
  arrange(desc(.hat)) %>%
  select(price, carat, .fitted, .resid, .hat, .cooksd) %>%
  head()


# high leverage might not be infulential

# high leverage plus high residual determins influence, which can be refleced by .cooksd value
#cooksd  cooks' distance value

mod_diamonds%>%
  augment()%>%
  arrange(desc(.cooksd)) %>%
  select(price, carat, .fitted, .resid, .hat, .cooksd) %>%
  head()

#get rid of outliers
diamonds_clean=diamonds%>%
  filter(price<18531)

new_mod = lm(price~carat, data=diamonds_clean)

summary(new_mod)

diamonds_clean%>%
  ggplot(aes(carat, price))+
  geom_point()+
  geom_smooth(method="lm")