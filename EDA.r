##### Eda #####
library('ggplot2') # library to create plots 


library(GGally) # for ggpairs
library("SignifReg")
install.packages("SignifReg")
(l <- sapply(data_full, function(x) is.factor(x)))
m <- data_full[, l]
ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")
n
names(data_full)
x<-na.omit(data_full)
SignifReg(SalePrice~.,data=data_full[ , !(names(data_full) %in% drops)],  alpha = 0.05, direction = "forward",
          criterion = "p-value", correction = "FDR")
drops <- c("Sale.Condition", 'Order', 'PID', "MS.SubClass")
data_full<-data_full[ , !(names(data_full) %in% drops)]
data(mtcars)
cg<-mtcars
names(cg) <- c("M.mpg", "C.cyl", "D.disp", "H.hp", "D.drat", "W.wt", "Q.qsec", "V.vs", "A.am", "G.gear", "C.carb" )
scope1 <- M.mpg~.
model11 <- SignifReg(scope=scope1,data=cg,alpha=0.05,
                    direction="forward", criterion="p-value",
                    correction="FDR")
summary(model11)
summary(data_full$Sale.Condition)
str(data_full$MS.SubClass)
data_full$Sale.Condition
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
m1 <- lm(log(SalePrice) ~ Gr.Liv.Area+Lot.Area, data = data_full)
m1
# paraweiss scatter plot:
ggpairs(evals, columns = 13:19)
grep("^SalePrice$", colnames(data_full))

col<-c(grep("SalePrice", colnames(data_full)),grep("Gr.Liv.Area", colnames(data_full)), grep("Lot.Area", colnames(data_full)) )
ggpairs(data_full, columns = col)

summary(m1)

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
ggplot(data_full, aes(Lot.Area )) +
  geom_histogram(bins = 1000) 
ggplot(data_full, aes(x=Lot.Area, y=log(SalePrice)))+ geom_point(size=2)+stat_smooth(method = "lm", se = TRUE)
data_full <- data_full %>% filter(Sale.Condition == "Normal" & Lot.Area < 100000 )
#If the relationship looks linear, we can quantify the strength of the relationship with the correlation coefficient.
data_full %>%
  summarise(cor(Lot.Area, log(SalePrice)))# 0.274267 - shows very weak uphill (positive) linear relationship
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
#  p-value approach if what we're interested in is finding out which predictors are statistically 
#  significant.
#  To do forward selection using p-values, we start with single predictor regressions of response 
# versus each explanatory variable. We then pick the variable with the lowest significant p-value.
# And we add the remaining variables one at a time to the existing model, and pick the variable with
# the lowest significant p-value again. We repeat until any of the remaining variables do not have a significant p-value. 
# 
# 1. Single predictor regressions of response versus each explanatory variable.
# Gr.Liv.Area
r1 <- lm(SalePrice ~ Gr.Liv.Area, data = data_full)
summary(r1) # <2e-16
names(data_full)
# How to get p-val from the summary
lmp(r1)
summary(r1)$coefficients[2,4]==summary(r2)$coefficients[2,4]
(summary(r1)$coefficients[c(2:(length(summary(r1)$coefficients[, 4]))), 4])
summary(r2)$coefficients[2,4]
summary(r8)$coefficients[2,4]
summary(r39)$coefficients[4,4]
summary(r8) # 0.0631 - bad predictor
summary(r39) # 2.02e-13 
summary(fit)$coefficients[,4] 


# Lot.Frontage
r2<- lm(SalePrice ~ Lot.Frontage, data = data_full)
summary(r2) # <2e-16
summary(r2)$coefficients[2,4]
# Lot.Area
r3 <- lm(SalePrice ~ Lot.Area, data = data_full)
summary(r3) # <2e-16
summary(r3)$coefficients[2,4]
# Year.Built
r4 <- lm(log(SalePrice) ~ Year.Built, data = data_full)
summary(r4) # <2e-16
summary(r4)$coefficients[2,4]
# Year.Remod.Add
r5 <- lm(log(SalePrice) ~ Year.Remod.Add, data = data_full)
summary(r5) # <2e-16
summary(r5)$coefficients[2,4]
#Mas.Vnr.Area
r6 <- lm(log(SalePrice) ~ Mas.Vnr.Area, data = data_full)
summary(r6) # <2e-16
summary(r6)$coefficients[2,4]
#BsmtFin.SF.1 
r7 <- lm(log(SalePrice) ~ BsmtFin.SF.1, data = data_full)
summary(r7) # <2e-16
summary(r7)$coefficients[2,4]#8.366476e-113
#BsmtFin.SF.2
r8 <- lm(log(SalePrice) ~ BsmtFin.SF.2, data = data_full)
summary(r8) # 0.0631 - bad predictor
summary(r8)$coefficients[2,4]
# Bsmt.Unf.SF
r9 <- lm(log(SalePrice) ~Bsmt.Unf.SF, data = data_full)
summary(r9) # <2e-16
summary(r9)$coefficients[2,4]
# Bsmt.Half.Bath
r10 <- lm(log(SalePrice) ~Bsmt.Half.Bath, data = data_full)
summary(r10) #  0.181 - bad predictor
summary(r10)$coefficients[2,4]
# Full.Bath
r11 <- lm(log(SalePrice) ~Full.Bath, data = data_full)
summary(r11) #  <2e-16
summary(r11)$coefficients[2,4]
# Bsmt.Full.Bath
r12 <- lm(log(SalePrice) ~Bsmt.Full.Bath, data = data_full)
summary(r12) #  <2e-16
summary(r12)$coefficients[2,4]
# X1st.Flr.SF
r13 <- lm(log(SalePrice) ~X1st.Flr.SF, data = data_full)
summary(r13) #  <2e-16
summary(r13)$coefficients[2,4]
# X2nd.Flr.SF
r14 <- lm(SalePrice ~ X2nd.Flr.SF, data = data_full)
summary(r14) #  <2e-16
summary(r14)$coefficients[2,4]
# Low.Qual.Fin.SF
r15 <- lm(log(SalePrice) ~ Low.Qual.Fin.SF, data = data_full)
summary(r15) #  0.107
summary(r15)$coefficients[2,4]
# Half.Bath
r16 <- lm(log(SalePrice) ~ Half.Bath, data = data_full)
summary(r16) #  <2e-16 ***
summary(r16)$coefficients[2,4]# 1.97821e-56
# Bedroom.AbvGr
r17 <- lm(SalePrice ~ Bedroom.AbvGr, data = data_full)
summary(r17) #  <2e-16 ***
summary(r17)$coefficients[2,4]
# Kitchen.AbvGr
r18 <- lm(log(SalePrice) ~ Kitchen.AbvGr, data = data_full)
summary(r18) #  7.55e-10 >2e-16
summary(r18)$coefficients[2,4]# 6.78654e-10

# TotRms.AbvGrd
r19 <- lm(log(SalePrice) ~ TotRms.AbvGrd, data = data_full)
summary(r19) #  <2e-16
summary(r19)$coefficients[2,4]
# Fireplaces
r20 <- lm(log(SalePrice) ~ Fireplaces, data = data_full)
summary(r20) #  <2e-16
summary(r20)$coefficients[2,4]
# Garage.Cars
r21 <- lm(log(SalePrice) ~ Garage.Cars, data = data_full)
summary(r21) #  <2e-16
summary(r21)$coefficients[2,4]
# Garage.Area
r22 <- lm(log(SalePrice) ~ Garage.Area, data = data_full)

summary(r22) #  <2e-16
summary(r22)$coefficients[2,4]
# Wood.Deck.SF
r23 <- lm(log(SalePrice) ~ Wood.Deck.SF, data = data_full)
summary(r23) #  <2e-16
summary(r23)$coefficients[2,4]
# Open.Porch.SF
r24 <- lm(log(SalePrice) ~ Open.Porch.SF, data = data_full)
summary(r24) #  <2e-16
summary(r24)$coefficients[2,4]# 1.752705e-64
# Enclosed.Porch
r25 <- lm(log(SalePrice) ~ Enclosed.Porch, data = data_full)
summary(r25) #  3.68e-11<2e-16
summary(r25)$coefficients[2,4]# 2.865001e-11
# X3Ssn.Porch
r26 <- lm(log(SalePrice) ~ X3Ssn.Porch, data = data_full)
summary(r26) #  0.181
summary(r26)$coefficients[2,4]
# Screen.Porch
r27 <- lm(log(SalePrice) ~ Screen.Porch, data = data_full)
summary(r27) #  2.75e-09 >2e-16
summary(r27)$coefficients[2,4]#  3.621894e-09
# Pool.Area
r28 <- lm(log(SalePrice) ~ Pool.Area, data = data_full)
summary(r28) #   0.072 - NOT A GOOD PREDICTOR
summary(r28)$coefficients[2,4]
# Misc.Val
r29 <- lm(log(SalePrice) ~ Misc.Val, data = data_full)
summary(r29) #   0.522 
summary(r29)$coefficients[2,4]
# Mo.Sold
r31 <- lm(log(SalePrice) ~ Mo.Sold, data = data_full)
summary(r31) #   0.43 
summary(r31)$coefficients[2,4]
# Yr.Sold
r32 <- lm(log(SalePrice) ~ Yr.Sold, data = data_full)
summary(r32) # 0.239
summary(r32)$coefficients[2,4]

#Categorical var
# Utilities
r33 <- lm(log(SalePrice) ~ Utilities, data = data_full)
summary(r33) # 0.239
summary(r33)$coefficients[2,4]
names(data_full)
# MS.SubClass

r34 <- lm(SalePrice ~ Gr.Liv.Area+MS.SubClass, data = data_full)
summary(r34) # 0.239
summary(r34)$coefficients[c(3:(length(summary(r34)$coefficients[, 4]))), 4]

# Loop to get p-val
p <- c()
match(c(min(p)),p)
p[75]
# The variable with the lowest pval
#Drop Sale.Condition column


p_values = data.frame(Variable=col_name, Pvalue=p_val)
p_values_ordered <- p_values[order(p_values$Pvalue),]# Gr.Liv.Area
p_values_ordered$Pvalue[2]==p_values_ordered$Pvalue[1]
col_name<-c()
p_val <- c()


for (i in names(data_full[ , !(names(data_full) %in% c("PID","Order", "SalePrice"))])) {
    ind_var <- data_full[[i]]
    response_var<-log(data_full$SalePrice)
    lm_fun<-lm(response_var ~ ind_var, data = data_full)
    pval<-min(summary(lm_fun)$coefficients[c(2:(length(summary(lm_fun)$coefficients[, 4]))), 4])
    p_val<-append(p_val,pval,after = length(p_val))
    col_name<-append(col_name, i, after=length(col_name))
    
}
p<-lm(log(SalePrice)~ Gr.Liv.Area, data= data_full)
summary(p)


#Second highest variable:

p_values_1 = data.frame(Variable=col_name_1, Pvalue=p_val_1)

p_values_ordered_1 <- p_values_1[order(p_values_1$Pvalue),] # Year.Built
p_val_1<-c()
col_name_1<-c()


for (i in names(data_full[ , !(names(data_full) %in% c("PID","Order", "Gr.Liv.Area", "SalePrice"))])) {
  ind_var_1 <- data_full[[i]]
  response_var_1<-log(data_full$SalePrice)
  first_predictor<-data_full$Gr.Liv.Area
    lm_fun_1<-lm(response_var_1 ~ first_predictor +ind_var_1, data = data_full)
    pval_1<-min(summary(lm_fun_1)$coefficients[c(3:(length(summary(lm_fun_1)$coefficients[, 4]))), 4])
    p_val_1<-append(p_val_1,pval_1,after = length(p_val_1))
    col_name_1<-append(col_name_1, i, after=length(col_name_1))  
  
  
  
}
s<-lm(log(SalePrice)~ Gr.Liv.Area+ Year.Built, data= data_full)
summary(s)
summary(s)$coefficients[c(3), 4]
# P-val - 4.544732e-286
# Second highest var is Year.Built.


# Choosing third variable

p_values_2 = data.frame(Variable=col_name_2, Pvalue=p_val_2)
p_values_ordered_2 <- p_values_2[order(p_values_2$Pvalue),]# Total.Bsmt.SF
col_name_2<-c()
p_val_2 <- c()
for (i in names(data_full[ , !(names(data_full) %in% c("PID","Order", "Gr.Liv.Area", "SalePrice", "Year.Built"))])) {
  ind_var_2 <- data_full[[i]]
  response_var_2<-log(data_full$SalePrice)
  first_predictor_2<-data_full$Gr.Liv.Area
  second_predictor_2<-data_full$Year.Built
  lm_fun_2<-lm(response_var_2~ first_predictor_2 +second_predictor_2+ind_var_2, data = data_full)
  pval_2<-min(summary(lm_fun_2)$coefficients[c(4:(length(summary(lm_fun_2)$coefficients[, 4]))), 4])
  p_val_2<-append(p_val_2,pval_2,after = length(p_val_2))
  col_name_2<-append(col_name_2, i, after=length(col_name_2))  
  
  
  
} 
sh<-lm(log(SalePrice)~ Gr.Liv.Area+ Year.Built+ Total.Bsmt.SF, data= data_full)
summary(sh)
summary(sh)$coefficients[c(4), 4]
summary(lm(SalePrice~Gr.Liv.Area+Bsmt.Full.Bath+Bsmt.Qual, data=data_full))# R adj -  0.7504

# Choosing fourth variable

p_values_3 = data.frame(Variable=col_name_3, Pvalue=p_val_3)
p_values_ordered_3 <- p_values_3[order(p_values_3$Pvalue),]# Year.Remod.Add

col_name_3<-c()
p_val_3 <- c()
for (i in names(data_full[ , !(names(data_full) %in% c("PID","Order", "Gr.Liv.Area", "SalePrice", "Total.Bsmt.SF", "Year.Built"))])) {
  ind_var_3 <- data_full[[i]]
  response_var_3<-log(data_full$SalePrice)
  first_predictor_3<-data_full$Gr.Liv.Area
  second_predictor_3<-data_full$Year.Built
  third_predictor_3<-data_full$Total.Bsmt.SF
  lm_fun_3<-lm(response_var_3~ first_predictor_3 +second_predictor_3+third_predictor_3+ind_var_3, data = data_full)
  pval_3<-min(summary(lm_fun_3)$coefficients[c(5:(length(summary(lm_fun_3)$coefficients[, 4]))), 4])
  p_val_3<-append(p_val_3,pval_3,after = length(p_val_3))
  col_name_3<-append(col_name_3, i, after=length(col_name_3))  
  
  
  
} 
sha<-lm(log(SalePrice)~ Gr.Liv.Area+ Year.Built+Total.Bsmt.SF+Year.Remod.Add, data= data_full)
summary(sha)
summary(sha)$coefficients[c(5), 4]


# Choosing fifth variable

p_values_4 = data.frame(Variable=col_name_4, Pvalue=p_val_4)
p_values_ordered_4 <- p_values_4[order(p_values_4$Pvalue),]#Fireplaces
col_name_4<-c()
p_val_4 <- c()
min(p_val_4)
for (i in names(data_full[ , !(names(data_full) %in% c("PID","Order", "Gr.Liv.Area", "SalePrice", "Year.Built", "Total.Bsmt.SF", "Year.Remod.Add"))])) {
  ind_var_4 <- data_full[[i]]
  response_var_4<-log(data_full$SalePrice)
  first_predictor_4<-data_full$Gr.Liv.Area
  second_predictor_4<-data_full$Year.Built
  third_predictor_4<-data_full$Total.Bsmt.SF
  fourth_predictor_4<-data_full$Year.Remod.Add
  lm_fun_4<-lm(response_var_4~ first_predictor_4 +second_predictor_4+third_predictor_4+ fourth_predictor_4+ind_var_4, data = data_full)
  pval_4<-min(summary(lm_fun_4)$coefficients[c(6:(length(summary(lm_fun_4)$coefficients[, 4]))), 4])
  p_val_4<-append(p_val_4,pval_4,after = length(p_val_4))
  col_name_4<-append(col_name_4, i, after=length(col_name_4))  
  
  
  
} 
sharg<-lm(log(SalePrice)~ Gr.Liv.Area+ Year.Built+Total.Bsmt.SF+Year.Remod.Add+Fireplaces, data= data_full)
summary(sharg)
summary(sharg)$coefficients[c(6), 4]


# Choosing sixth variable

p_values_5 = data.frame(Variable=col_name_5, Pvalue=p_val_5)
p_values_ordered_5 <- p_values_5[order(p_values_5$Pvalue),] # Garage.Area
col_name_5<-c()
p_val_5 <- c()
for (i in names(data_full[ , !(names(data_full) %in% c("PID","Order", "SalePrice", "Gr.Liv.Area", "Year.Built", "Total.Bsmt.SF", "Year.Remod.Add", "Fireplaces"))])) {
  ind_var_5 <- data_full[[i]]
  response_var_5<-log(data_full$SalePrice)
  first_predictor_5<-data_full$Gr.Liv.Area
  second_predictor_5<-data_full$Year.Built
  third_predictor_5<-data_full$Total.Bsmt.SF
  fourth_predictor_5<-data_full$Year.Remod.Add
  fifth_predictor_5<-data_full$Fireplaces
  lm_fun_5<-lm(response_var_5~ first_predictor_5 +second_predictor_5+third_predictor_5+ fourth_predictor_5+fifth_predictor_5+ind_var_5, data = data_full)
  pval_5<-min(summary(lm_fun_5)$coefficients[c(7:(length(summary(lm_fun_5)$coefficients[, 4]))), 4])
  p_val_5<-append(p_val_5,pval_5,after = length(p_val_5))
  col_name_5<-append(col_name_5, i, after=length(col_name_5))  
  
  
  
} 
shargo<-lm(log(SalePrice)~ Gr.Liv.Area+ Year.Built+Total.Bsmt.SF+Year.Remod.Add+Fireplaces+ Garage.Area, data= data_full)
summary(shargo)$coefficients[c(7), 4]
# Choosing seventh variable

p_values_6 = data.frame(Variable=col_name_6, Pvalue=p_val_6)
p_values_ordered_6 <- p_values_6[order(p_values_6$Pvalue),]# Garage.Area
col_name_6<-c()
p_val_6 <- c()
for (i in names(data_full[ , !(names(data_full) %in% c("PID","Order", "Gr.Liv.Area", "SalePrice", "Bsmt.Full.Bath", "Bsmt.Qual", "Total.Bsmt.SF", "Kitchen.Qual", "Kitchen.AbvGr"))])) {
  ind_var_6 <- data_full[[i]]
  response_var_6 <-data_full$SalePrice
  first_predictor_6 <-data_full$Gr.Liv.Area
  second_predictor_6 <-data_full$Bsmt.Full.Bath
  third_predictor_6<-data_full$Bsmt.Qual
  fourth_predictor_6<-data_full$Total.Bsmt.SF
  fifth_predictor_6<-data_full$Kitchen.Qual
  sixth_predictor_6<-data_full$Kitchen.AbvGr
  lm_fun_6<-lm(response_var_6~ first_predictor_6 +second_predictor_6+third_predictor_6+ fourth_predictor_6+fifth_predictor_6+sixth_predictor_6+ind_var_6, data = data_full)
  pval_6<-min(summary(lm_fun_6)$coefficients[c(15:(length(summary(lm_fun_6)$coefficients[, 4]))), 4])
  p_val_6<-append(p_val_6,pval_6,after = length(p_val_6))
  col_name_6<-append(col_name_6, i, after=length(col_name_6))  
  
  
  
} 
summary(lm(SalePrice~Gr.Liv.Area+Bsmt.Full.Bath+Bsmt.Qual+Total.Bsmt.SF+Kitchen.Qual+Kitchen.AbvGr+Garage.Area, data=data_full)) #  0.8549


# Choosing eighth variable

p_values_7 = data.frame(Variable=col_name_7, Pvalue=p_val_7)
p_values_ordered_7 <- p_values_7[order(p_values_7$Pvalue),]# Overall.Qual
2.015408e-30<0.05
col_name_7<-c()
p_val_7 <- c()
for (i in names(data_full[ , !(names(data_full) %in% c("PID","Order", "Gr.Liv.Area", "SalePrice", "Bsmt.Full.Bath", "Bsmt.Qual", "Total.Bsmt.SF", "Kitchen.Qual", "Kitchen.AbvGr", "Garage.Area"))])) {
  ind_var_7 <- data_full[[i]]
  response_var_7 <-data_full$SalePrice
  first_predictor_7 <-data_full$Gr.Liv.Area
  second_predictor_7 <-data_full$Bsmt.Full.Bath
  third_predictor_7<-data_full$Bsmt.Qual
  fourth_predictor_7<-data_full$Total.Bsmt.SF
  fifth_predictor_7<-data_full$Kitchen.Qual
  sixth_predictor_7<-data_full$Kitchen.AbvGr
  seventh_predictor_7<-data_full$Garage.Area
  lm_fun_7<-lm(response_var_7~ first_predictor_7 +second_predictor_7+third_predictor_7+ fourth_predictor_7+fifth_predictor_7+sixth_predictor_7+seventh_predictor_7+ind_var_7, data = data_full)
  pval_7<-min(summary(lm_fun_7)$coefficients[c(16:(length(summary(lm_fun_7)$coefficients[, 4]))), 4])
  p_val_7<-append(p_val_7,pval_7,after = length(p_val_7))
  col_name_7<-append(col_name_7, i, after=length(col_name_7))  
  
  
  
} 
summary(lm(SalePrice~Gr.Liv.Area+Bsmt.Full.Bath+Bsmt.Qual+Total.Bsmt.SF+Kitchen.Qual+Kitchen.AbvGr+Garage.Area+Overall.Qual, data=data_full)) #  0.8866 


# Choosing ninth variable

p_values_8 = data.frame(Variable=col_name_8, Pvalue=p_val_8)
p_values_ordered_8 <- p_values_8[order(p_values_8$Pvalue),]# Bsmt.Unf.SF
7.621845e-47<0.05

col_name_8<-c()
p_val_8 <- c()
for (i in names(data_full[ , !(names(data_full) %in% c("PID","Order", "Gr.Liv.Area", "SalePrice", "Bsmt.Full.Bath", "Bsmt.Qual", "Total.Bsmt.SF", "Kitchen.Qual", "Kitchen.AbvGr", "Garage.Area", "Overall.Qual"))])) {
  ind_var_8 <- data_full[[i]]
  response_var_8 <-data_full$SalePrice
  first_predictor_8 <-data_full$Gr.Liv.Area
  second_predictor_8 <-data_full$Bsmt.Full.Bath
  third_predictor_8<-data_full$Bsmt.Qual
  fourth_predictor_8<-data_full$Total.Bsmt.SF
  fifth_predictor_8<-data_full$Kitchen.Qual
  sixth_predictor_8<-data_full$Kitchen.AbvGr
  seventh_predictor_8<-data_full$Garage.Area
  eighth_predictor_8<-data_full$Overall.Qual
  lm_fun_8<-lm(response_var_8~ first_predictor_8 +second_predictor_8+third_predictor_8+ fourth_predictor_8+fifth_predictor_8+sixth_predictor_8+seventh_predictor_8+eighth_predictor_8+ind_var_8, data = data_full)
  pval_8<-min(summary(lm_fun_8)$coefficients[c(24:(length(summary(lm_fun_8)$coefficients[, 4]))), 4])
  p_val_8<-append(p_val_8,pval_8,after = length(p_val_8))
  col_name_8<-append(col_name_8, i, after=length(col_name_8))  
  
  
  
} 
summary(lm(SalePrice~Gr.Liv.Area+Bsmt.Full.Bath+Bsmt.Qual+Total.Bsmt.SF+Kitchen.Qual+Kitchen.AbvGr+Garage.Area+Overall.Qual+Bsmt.Unf.SF, data=data_full)) #   0.896 


cor(data_full$Bsmt.Unf.SF,data_full$Total.Bsmt.SF)

# Choosing tenth variable

p_values_9 = data.frame(Variable=col_name_9, Pvalue=p_val_9)
p_values_ordered_9 <- p_values_9[order(p_values_9$Pvalue),]#   Heating.QC
7.170407e-51<0.05
min(p_values_9$Pvalue)

col_name_9<-c()
p_val_9 <- c()
for (i in names(data_full[ , !(names(data_full) %in% c("PID","Order", "Gr.Liv.Area", "SalePrice", "Bsmt.Full.Bath", "Bsmt.Qual", "Total.Bsmt.SF", "Kitchen.Qual", "Kitchen.AbvGr", "Garage.Area", "Overall.Qual", "Bsmt.Unf.SF"))])) {
  ind_var_9 <- data_full[[i]]
  response_var_9 <-data_full$SalePrice
  first_predictor_9 <-data_full$Gr.Liv.Area
  second_predictor_9 <-data_full$Bsmt.Full.Bath
  third_predictor_9<-data_full$Bsmt.Qual
  fourth_predictor_9<-data_full$Total.Bsmt.SF
  fifth_predictor_9<-data_full$Kitchen.Qual
  sixth_predictor_9<-data_full$Kitchen.AbvGr
  seventh_predictor_9<-data_full$Garage.Area
  eighth_predictor_9<-data_full$Overall.Qual
  nineth_predictor_9<-data_full$Bsmt.Unf.SF
  lm_fun_9<-lm(response_var_9~ first_predictor_9 +second_predictor_9+third_predictor_9+ fourth_predictor_9+fifth_predictor_9+sixth_predictor_9+seventh_predictor_9+eighth_predictor_9+nineth_predictor_9+ind_var_9, data = data_full)
  pval_9<-min(summary(lm_fun_9)$coefficients[c(25:(length(summary(lm_fun_9)$coefficients[, 4]))), 4])
  p_val_9<-append(p_val_9,pval_9,after = length(p_val_9))
  col_name_9<-append(col_name_9, i, after=length(col_name_9))  
  
  
  
} 
summary(lm(SalePrice~Gr.Liv.Area+Bsmt.Full.Bath+Bsmt.Qual+Total.Bsmt.SF+Kitchen.Qual+Kitchen.AbvGr+Garage.Area+Overall.Qual+Bsmt.Unf.SF+ Heating.QC, data=data_full)) #    0.8979 

# Choosing eleventh variable

p_values_10 = data.frame(Variable=col_name_10, Pvalue=p_val_10)
p_values_ordered_10 <- p_values_10[order(p_values_10$Pvalue),]#   Lot.Area
2.499886e-31<0.05
min(p_values_10$Pvalue)

col_name_10<-c()
p_val_10 <- c()
for (i in names(data_full[ , !(names(data_full) %in% c("PID","Order", "Gr.Liv.Area", "SalePrice", "Bsmt.Full.Bath", "Bsmt.Qual", "Total.Bsmt.SF", "Kitchen.Qual", "Kitchen.AbvGr", "Garage.Area", "Overall.Qual", "Bsmt.Unf.SF", "Heating.QC"))])) {
  ind_var_10 <- data_full[[i]]
  response_var_10 <-data_full$SalePrice
  first_predictor_10 <-data_full$Gr.Liv.Area
  second_predictor_10 <-data_full$Bsmt.Full.Bath
  third_predictor_10<-data_full$Bsmt.Qual
  fourth_predictor_10<-data_full$Total.Bsmt.SF
  fifth_predictor_10<-data_full$Kitchen.Qual
  sixth_predictor_10<-data_full$Kitchen.AbvGr
  seventh_predictor_10<-data_full$Garage.Area
  eighth_predictor_10<-data_full$Overall.Qual
  nineth_predictor_10<-data_full$Bsmt.Unf.SF
  tenth_predictor_10<-data_full$Heating.QC
  lm_fun_10<-lm(response_var_10~ first_predictor_10 +second_predictor_10+third_predictor_10+ fourth_predictor_10+fifth_predictor_10+sixth_predictor_10+seventh_predictor_10+eighth_predictor_10+nineth_predictor_10+tenth_predictor_10+ind_var_10, data = data_full)
  pval_10<-min(summary(lm_fun_10)$coefficients[c(29:(length(summary(lm_fun_10)$coefficients[, 4]))), 4])
  p_val_10<-append(p_val_10,pval_10,after = length(p_val_10))
  col_name_10<-append(col_name_10, i, after=length(col_name_10))  
  
  
  
} 
summary(lm(SalePrice~Gr.Liv.Area+Bsmt.Full.Bath+Bsmt.Qual+Total.Bsmt.SF+Kitchen.Qual+Kitchen.AbvGr+Garage.Area+Overall.Qual+Bsmt.Unf.SF+ Heating.QC+Lot.Area, data=data_full)) # R-adj -  0.9035 
cor(data_full$Bsmt.Unf.SF,data_full$Total.Bsmt.SF)


# Choosing 12th variable

p_values_11= data.frame(Variable=col_name_11, Pvalue=p_val_11)
p_values_ordered_11 <- p_values_11[order(p_values_11$Pvalue),]#   MS.SubClass
4.101173e-31<0.05
min(p_values_11$Pvalue)

col_name_11<-c()
p_val_11 <- c()
for (i in names(data_full[ , !(names(data_full) %in% c("PID","Order", "Gr.Liv.Area", "SalePrice", "Bsmt.Full.Bath", "Bsmt.Qual", "Total.Bsmt.SF", "Kitchen.Qual", "Kitchen.AbvGr", "Garage.Area", "Overall.Qual", "Bsmt.Unf.SF", "Heating.QC", "Lot.Area"))])) {
  ind_var_11 <- data_full[[i]]
  response_var_11 <-data_full$SalePrice
  first_predictor_11 <-data_full$Gr.Liv.Area
  second_predictor_11 <-data_full$Bsmt.Full.Bath
  third_predictor_11<-data_full$Bsmt.Qual
  fourth_predictor_11<-data_full$Total.Bsmt.SF
  fifth_predictor_11<-data_full$Kitchen.Qual
  sixth_predictor_11<-data_full$Kitchen.AbvGr
  seventh_predictor_11<-data_full$Garage.Area
  eighth_predictor_11<-data_full$Overall.Qual
  nineth_predictor_11<-data_full$Bsmt.Unf.SF
  tenth_predictor_11<-data_full$Heating.QC
  eleventhth_predictor_11<-data_full$Lot.Area
  lm_fun_11<-lm(response_var_11~ first_predictor_11 +second_predictor_11+third_predictor_11+ fourth_predictor_11+
                  fifth_predictor_11+sixth_predictor_11+seventh_predictor_11+eighth_predictor_11+nineth_predictor_11+tenth_predictor_11+ eleventhth_predictor_11+ind_var_10, data = data_full)
  pval_11<-min(summary(lm_fun_11)$coefficients[c(30:(length(summary(lm_fun_11)$coefficients[, 4]))), 4])
  p_val_11<-append(p_val_11,pval_11,after = length(p_val_11))
  col_name_11<-append(col_name_11, i, after=length(col_name_11))  
  
  
  
} 
summary(lm(SalePrice~Gr.Liv.Area+Bsmt.Full.Bath+Bsmt.Qual+Total.Bsmt.SF+Kitchen.Qual+Kitchen.AbvGr+Garage.Area+Overall.Qual+Bsmt.Unf.SF+ Heating.QC+Lot.Area+ MS.SubClass, data=data_full)) # R-adj -  0.9085 



# Choosing 12th variable

p_values_12= data.frame(Variable=col_name_12, Pvalue=p_val_12)
p_values_ordered_12 <- p_values_12[order(p_values_12$Pvalue),]#    MS.Zoning
1.278594e-07-31<0.05
min(p_values_12$Pvalue)

col_name_12<-c()
p_val_12 <- c()
for (i in names(data_full[ , !(names(data_full) %in% c("PID","Order", "Gr.Liv.Area", "SalePrice", "Bsmt.Full.Bath", "Bsmt.Qual", "Total.Bsmt.SF", "Kitchen.Qual", "Kitchen.AbvGr", "Garage.Area", "Overall.Qual", "Bsmt.Unf.SF", "Heating.QC", "Lot.Area", "MS.SubClass"))])) {
  ind_var_12 <- data_full[[i]]
  response_var_12 <-data_full$SalePrice
  first_predictor_12 <-data_full$Gr.Liv.Area
  second_predictor_12 <-data_full$Bsmt.Full.Bath
  third_predictor_12<-data_full$Bsmt.Qual
  fourth_predictor_12<-data_full$Total.Bsmt.SF
  fifth_predictor_12<-data_full$Kitchen.Qual
  sixth_predictor_12<-data_full$Kitchen.AbvGr
  seventh_predictor_12<-data_full$Garage.Area
  eighth_predictor_12<-data_full$Overall.Qual
  nineth_predictor_12<-data_full$Bsmt.Unf.SF
  tenth_predictor_12<-data_full$Heating.QC
  eleventhth_predictor_12<-data_full$Lot.Area
  twelvth_predictor_12<-data_full$MS.SubClass
  lm_fun_12<-lm(response_var_12~ first_predictor_12 +second_predictor_12+third_predictor_12+ fourth_predictor_12+
                  fifth_predictor_12+sixth_predictor_12+seventh_predictor_12+eighth_predictor_12+nineth_predictor_12+tenth_predictor_12+ eleventhth_predictor_12+twelvth_predictor_12+ind_var_10, data = data_full)
  pval_12<-min(summary(lm_fun_12)$coefficients[c(45:(length(summary(lm_fun_12)$coefficients[, 4]))), 4])
  p_val_12<-append(p_val_12,pval_12,after = length(p_val_12))
  col_name_12<-append(col_name_12, i, after=length(col_name_12))  
  
  
  
} 
summary(lm(SalePrice~Gr.Liv.Area+Bsmt.Full.Bath+Bsmt.Qual+Total.Bsmt.SF+Kitchen.Qual+Kitchen.AbvGr+Garage.Area+Overall.Qual+Bsmt.Unf.SF+ Heating.QC+Lot.Area+ MS.SubClass+MS.Zoning, data=data_full)) # R-adj - 0.9104 

# Check for quality.
ggplot(data = data_full, aes(x = Overall.Qual, y =SalePrice, colour=Kitchen.Qual))+ geom_jitter(size=1)+
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))

str(data_full$Bsmt.Qual)
str(data_full$Exter.Qual)
grep("Qual", names(data_full), value=TRUE)
str(data_full[,grep("Qual", names(data_full), value=TRUE)] )
data_full$Garage.Qual <- recode.features(data_full$Garage.Qual )

#Exterior qual
data_full$Exter.Qual<-recode(data_full$Exter.Qual, Ex = "9",Gd="7", Fa="3", TA="5", Po="2")
data_full$Exter.Qual<-as.numeric(levels(data_full$Exter.Qual))[data_full$Exter.Qual]
#Kitchen
data_full$Kitchen.Qual<-recode(data_full$Kitchen.Qual, Ex = "9",Gd="7", Fa="3", TA="5", Po="2")
data_full$Kitchen.Qual<-as.numeric(levels(data_full$Kitchen.Qual))[data_full$Kitchen.Qual]
ggpairs(data_full, columns = c("SalePrice",   "Overall.Qual", "Exter.Qual", "Kitchen.Qual", "Garage.Qual"))
?ggpairs
  
#Garage cual:
data_full$Garage.Qual<-as.numeric(levels(data_full$Garage.Qual))[data_full$Garage.Qual]
#Basement qual:
data_full$Bsmt.Qual<-as.numeric(levels(data_full$Bsmt.Qual))[data_full$Bsmt.Qual]
# 10	Very Excellent
# 9	Excellent
# 8	Very Good
# 7	Good
# 6	Above Average
# 5	Average
# 4	Below Average
# 3	Fair
# 2	Poor
# 1	Very Poor
#Kitchen qual
# Ex	Excellent
# Gd	Good
# TA	Typical/Average
# Fa	Fair
# Po	Poor


lo<-lm(SalePrice~ Gr.Liv.Area +Bsmt.Full.Bath+Bsmt.Qual+Total.Bsmt.SF +Kitchen.Qual+ Kitchen.AbvGr+ Lot.Shape, data = data_full)
summary(lo)$coefficients[c(10:(length(summary(lu)$coefficients[, 4]))), 4]
summary(lu)
g<-c()
data("evals")
str(evals)
h<-c(1,2,3,4,5,6)
for (i in h){
  g<-append(g, min(h), after=length(g))
}
h<-list(1, "ww")
data_full$Sale.Type
names(data_full)
ex_col<-c("SalePrice", "Lot.Area", "Lot.Frontage", "Bedroom.AbvGr", "MS.SubClass")
data_y<-data_full[ex_col] 
head(data_y)
head(data_y)

summary(r34) #0.000908>2e-16

length(summary(r34)$coefficients[, 4])
summary(r34)$coefficients[16, 4]
d<-(nlevels(data_full$MS.SubClass)+1)
data_full[data_full$MS.SubClass=="020"]
str(data_full$MS.SubClass)
(nlevels(data_full$MS.SubClass)+1)
nlevels(data_full$MS.SubClass)
c(2:(nlevels(data_full$MS.SubClass)+1))
cv<-c()

# MS.Zoning
r35 <- lm(log(SalePrice) ~ MS.Zoning, data = data_full)
summary(r35) # 0.00541
# Street
r36 <- lm(log(SalePrice) ~ Street, data = data_full)
summary(r36) # 1.28e-05>>2e-16
# Alley
r37 <- lm(log(SalePrice) ~ Alley, data = data_full)
summary(r37) # 3.43e-16>2e-16

# Lot.Shape
r38 <- lm(log(SalePrice) ~ Lot.Shape, data = data_full)
summary(r38) # <2e-16

# Land.Contour
r39 <- lm(log(SalePrice) ~ Land.Contour, data = data_full)
summary(r39) # 2.02e-13 

head(num_data)
ncol(num_data)
#Numerical variables selection: 
num_var <- sapply(data_full, is.numeric)

num_data<-data_full[num_var]
ggpairs(data=num_data, # data.frame with variables
        columns=3:35, # columns to plot, default to all.
        title="Density Data"
) 
# Year built. describe the distribution.
ggplot(data=data_full, aes(data_full$Year.Built)) + geom_histogram()
ggplot(data=chol, aes(chol$AGE)) + geom_histogram()
data_full$Neighborhood
#Neigbourhoods
ggplot(data = data_full, aes(x =reorder(Neighborhood, SalePrice, median) , y =SalePrice,color=Neighborhood)) +geom_boxplot(outlier.size=0.2)+ geom_jitter(shape=16, position=position_jitter(0.1))+ scale_x_discrete(name = "Neighborhood") +
  scale_y_continuous(name = "Price",labels = function(y) paste0(y / 1000, "k"))+ ggtitle("The Neighborhood vs Price")
ggplot(data = movies, aes(x =reorder(genre, imdb_rating, median) , y =imdb_rating,color=genre)) +geom_boxplot(outlier.size=0.2)+ geom_jitter(shape=16, position=position_jitter(0.1))+ scale_x_discrete(name = "Genre") +
  scale_y_continuous(name = "IMBD rating")+ ggtitle("The genre vs rating")+ scale_x_discrete(labels=c("Com","Hor","SciF","Act","Anim","Art","Myst","Dram","Oth","Mus","Doc"))