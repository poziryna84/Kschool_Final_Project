##### Eda #####
library('ggplot2')      # library to create plots 
# As our response variable is SalePrice it´s a good idea to take a look at its distribution. 
summary(data_full$SalePrice)
ggplot(data_full, aes(SalePrice, fill = MS.SubClass)) +
  geom_histogram(bins = 100) +  scale_x_continuous("Sales Price in thousands",labels=function(x)x/1000)
# The distribution is right skewed I am going to log transorm it:

ggplot(data_full, aes(log(SalePrice), fill = MS.SubClass)) +
  geom_histogram(bins = 100) +  scale_x_continuous("Sales Price in thousands",labels=function(x)x/1000)

# Gr.Liv.Area
ggplot(data_full, aes(x=Gr.Liv.Area, y=log(SalePrice)))+ geom_point(size=2)+stat_smooth(method = "lm", se = TRUE)
#If the relationship looks linear, we can quantify the strength of the relationship with the correlation coefficient.
data_full %>%
  summarise(cor(Gr.Liv.Area, SalePrice))# 0.7283779 - shows strong uphill (positive) linear relationship
# Linear model:
m1 <- lm(log(SalePrice) ~ Gr.Liv.Area, data = data_full)
m1
summary(m1)
data_full$sa
ggplot(data = data_full, aes(x = Gr.Liv.Area, y = log(SalePrice))) +
  geom_point() +
  stat_smooth(method = "lm", se = TRUE)

# Model diagnostics.
# 1. Linearity ; scatterplot (see above) and a plot of the residuals vs. fitted (predicted) values.

ggplot(data = m1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals") #  we look for a random scatter around zero. 

# 2. Nearly normal residuals:
# To check this condition, we can look at a histogram
ggplot(data = m1, aes(x = .resid)) +
  geom_histogram(binwidth = 0.05) +
  xlab("Residuals") # The histogram shows a somewhat symmetric distribution. It is indeed centered at zero
# Or a normal probability plot of the residuals:
ggplot(data = m1, aes(sample = .resid)) +
  stat_qq() #  the normal probability plot shows that there are some values on the lower end of the tail, that 
            # actually steer away from normality. But that's only just a few observations. 

# 3. The last condition is constant variability, which says that variability of points around the least 
# squares line should be roughly constant. This implies that the variability of residuals around the zero line should be roughly constant as well.
# This condition is also called homoscedasticity. And we can check this using a residuals plot.

ggplot(data = m1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals") #  the variability of the residuals, that is how far they are from zero, do not vary by the value of the explanatory variable. 



#Lot.Area
ggplot(data_full, aes(Lot.Area, )) +
  geom_histogram(bins = 100) 
ggplot(data_full, aes(x=Lot.Area, y=SalePrice, colour=MS.Zoning))+ geom_point(size=2)+stat_smooth(method = "lm", se = TRUE)
data_full$MS.Zoning
#If the relationship looks linear, we can quantify the strength of the relationship with the correlation coefficient.
data_full %>%
  summarise(cor(Lot.Area, SalePrice))# 0.274267 - shows very weak uphill (positive) linear relationship
# Linear model:
m2 <- lm(log(SalePrice) ~ Lot.Area, data = data_full)
m2
summary(m2)

ggplot(data = data_full, aes(x = Lot.Area, y = SalePrice)) +
  geom_point() +
  stat_smooth(method = "lm", se = TRUE) + scale_y_continuous("Sales Price in thousands",labels=function(x)x/1000)

# Model diagnostics.
# 1. Linearity ; scatterplot (see above) and a plot of the residuals vs. fitted (predicted) values.

ggplot(data = m2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals") #  we look for a random scatter around zero. 

# 2. Nearly normal residuals:
# To check this condition, we can look at a histogram
ggplot(data = m2, aes(x = .resid)) +
  geom_histogram(binwidth = 0.05) +
  xlab("Residuals") # The histogram shows a somewhat symmetric distribution. It is indeed centered at zero
# Or a normal probability plot of the residuals:
ggplot(data = m2, aes(sample = .resid)) +
  stat_qq() #  the normal probability plot shows that there are some values on the lower end of the tail, that 
# actually steer away from normality. But that's only just a few observations. 

# 3. The last condition is constant variability, which says that variability of points around the least 
# squares line should be roughly constant. This implies that the variability of residuals around the zero line should be roughly constant as well.
# This condition is also called homoscedasticity. And we can check this using a residuals plot.

ggplot(data = m2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals") #  the variability of the residuals, that is how far they are from zero, do not vary by the value of the explanatory variable. 

# Total.Bsmt.SF
ggplot(data_full, aes(Total.Bsmt.SF, )) +
  geom_histogram(bins = 100) 
ggplot(data_full, aes(x=Total.Bsmt.SF, y=SalePrice))+ geom_point(size=2)+stat_smooth(method = "lm", se = TRUE)
data_full$MS.Zoning
#If the relationship looks linear, we can quantify the strength of the relationship with the correlation coefficient.
data_full %>%
  summarise(cor(Total.Bsmt.SF, SalePrice))# 0.6410957 - shows very weak uphill (positive) linear relationship
# Linear model:
m3 <- lm(log(SalePrice) ~ Total.Bsmt.SF, data = data_full)
m3
summary(m2)

ggplot(data = data_full, aes(x = Total.Bsmt.SF, y = SalePrice)) +
  geom_point() +
  stat_smooth(method = "lm", se = TRUE) + scale_y_continuous("Sales Price in thousands",labels=function(x)x/1000)

# Model diagnostics.
# 1. Linearity ; scatterplot (see above) and a plot of the residuals vs. fitted (predicted) values.

ggplot(data = m3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals") #  we look for a random scatter around zero. 

# 2. Nearly normal residuals:
# To check this condition, we can look at a histogram
ggplot(data = m3, aes(x = .resid)) +
  geom_histogram(binwidth = 0.05) +
  xlab("Residuals") # The histogram shows a somewhat symmetric distribution. It is indeed centered at zero
# Or a normal probability plot of the residuals:
ggplot(data = m3, aes(sample = .resid)) +
  stat_qq() #  the normal probability plot shows that there are some values on the lower end of the tail, that 
# actually steer away from normality. But that's only just a few observations. 

# 3. The last condition is constant variability, which says that variability of points around the least 
# squares line should be roughly constant. This implies that the variability of residuals around the zero line should be roughly constant as well.
# This condition is also called homoscedasticity. And we can check this using a residuals plot.

ggplot(data = m3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals") #  the variability of the residuals, that is how far they are from zero, do not vary by the value of the explanatory variable.

# "Bsmt.Unf.SF"
ggplot(data_full, aes(Bsmt.Unf.SF, )) +
  geom_histogram(bins = 100) 
ggplot(data_full, aes(x=Bsmt.Unf.SF, y=SalePrice))+ geom_point(size=2)+stat_smooth(method = "lm", se = TRUE)
data_full$MS.Zoning
#If the relationship looks linear, we can quantify the strength of the relationship with the correlation coefficient.
data_full %>%
  summarise(cor(Bsmt.Unf.SF, SalePrice))# 0.1582301- shows very weak uphill (positive) linear relationship
# Linear model:
m4 <- lm(log(SalePrice) ~ Bsmt.Unf.SF, data = data_full)
m4
summary(m2)

ggplot(data = data_full, aes(x = Bsmt.Unf.SF, y = SalePrice)) +
  geom_point() +
  stat_smooth(method = "lm", se = TRUE) + scale_y_continuous("Sales Price in thousands",labels=function(x)x/1000)

# Model diagnostics.
# 1. Linearity ; scatterplot (see above) and a plot of the residuals vs. fitted (predicted) values.

ggplot(data = m4, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals") #  we look for a random scatter around zero. 

# 2. Nearly normal residuals:
# To check this condition, we can look at a histogram
ggplot(data = m4, aes(x = .resid)) +
  geom_histogram(binwidth = 0.05) +
  xlab("Residuals") # The histogram shows a somewhat symmetric distribution. It is indeed centered at zero
# Or a normal probability plot of the residuals:
ggplot(data = m4, aes(sample = .resid)) +
  stat_qq() #  the normal probability plot shows that there are some values on the lower end of the tail, that 
# actually steer away from normality. But that's only just a few observations. 

# 3. The last condition is constant variability, which says that variability of points around the least 
# squares line should be roughly constant. This implies that the variability of residuals around the zero line should be roughly constant as well.
# This condition is also called homoscedasticity. And we can check this using a residuals plot.

ggplot(data = m4, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals") 

#Year.Built 
#Year.Remod.Add
# Mas.Vnr.Area
# BsmtFin.SF.1
# BsmtFin.SF.2 
# Bsmt.Unf.SF
# Total.Bsmt.SF

nums <- sapply(data_full, is.numeric)
names(data_full[ , nums])
