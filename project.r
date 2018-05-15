##### 1. Loading Dataset. #####

data = read.csv("AmesHousing.csv", header=TRUE, sep = ";")


table(data$Sale.Condition)
##### 2. Loading packages #####

library(dplyr)# data manipulation #

library('ggplot2')      # library to create plots #   
require("stringi")      # string/text processing
library('statsr')       # staistics functions
library(forcats)        # reordering and modifying factor levels

library("naniar")       # replace specified values with an NA
library(GGally)
library(caret)          #near z-predictors
library(gdata)          # to use drop levels

library(SignifReg)
install.packages("regclass")
library("regclass")             # In regclass: Tools for an Introductory Class in Regression and Modeling
library("Boruta")
install.packages("Boruta")
install.packages("rFerns")
library("rFerns")
library("e1071") # outliers
library("outliers") # outliers
install.packages("outliers") # outliers
library("ggpubr") # visualization

# Deviding data set into train and test.

set.seed(123)

split <- sample(nrow(data), floor(0.7*nrow(data)))

train<- data[split,]
test <- data[-split,]
dim(train)
head(train)


##### 4. Response variable analysis. #####

# As our response variable is SalePrice it?s a good idea to take a look at its distribution and shape to decide
# on the most appropriate measures of center and spread; whether or not any variable transformation is needed; and
# outliers strategy. 

summary(train$SalePrice)

# With the minimum price of $ 13100 and the maximum of $ 745000 the mean of $ 181118 is higher than the median $ 161500,
# which indicates the skewness of the distribution:

mode <- function(x) {
  un <- unique(x)
  un[which.max(tabulate(match(x, un)))]
}
ggplot(train, aes(x=SalePrice,y = ..density.., colour="red" )) +geom_density(size = 1, colour = 'brown')+
  geom_histogram(bins = 30, fill = 'pink', colour = 'brown') +  scale_x_continuous("Sales Price in thousands",labels=function(x) x/1000)+
  labs(title = "Histogram - Prices of houses", x = 'Age', y = "Frequency")+
  geom_vline(data = train, mapping = aes( xintercept = mean(train$SalePrice), colour = 'mean'), size = 1.5)+
  geom_vline(data = train,mapping = aes( xintercept = median(train$SalePrice), colour = 'median'), size = 1.5)+
  geom_vline(data = train,mapping = aes( xintercept = mode(train$SalePrice), colour = 'mode'), size = 1.5)

# It is a unimodal right-skewed distribution. To make it easier to model I will log transform it and use its median and 
# and interquartile range as measures of center and variability for the further EDA  as they are robust to the outliers.

# Shapiro-Wilk normality test

shapiro.test(train$SalePrice) 

# With the p-val < 2.2e-16 which is less than the significance level of 0.05 we reject the null hypothesis that 
# the data is normally distributed. 
# W = 0.87827. The closer w to 1 the more likely it is that this distribution is normal.

# Testing for skewness:

skewness(train$SalePrice) 

# Positive skewness with the coefficient of 1.70039 > 1 indicates that the mean of the data values is larger than the median, and the
# data is extremely right-skewed.

# Identifying potential outliers.

# Test for outliers:

grubbs.test(train$SalePrice, type=10)

# Type = 10 is a test for one outlier (side is detected automatically and can be reversed by opposite parameter)
# With p-value = 9.058e-10 we reject the null hypothesis that this is not an outlier in favor to the alternative 
# hypothesis that the highest value 745000 is indeed an outlier.

# Testing for outliers on the opposite side:

grubbs.test(train$SalePrice, type=10, opposite = TRUE)

# With p-value = 1 and greater than 0.01 we won?t reject the null hypothesis that this is not an outlier.

# After several iterations I identified the cut-off for the outliers with the p-val set to 0.01:

outliers<-train[(train$SalePrice<534000),]

grubbs.test(outliers$SalePrice, type=10)
nrow(train)-nrow(outliers)

# The 11 properties sold for higher than $ 550000 won?t be included in my future modeling. 
shapiro.test(train$SalePrice) # W = 0.87827
shapiro.test(log(train$SalePrice)) # W = 0.98782
shapiro.test(outliers$SalePrice) # Getting rid of those 11 properties - W = 0.91136
# Going to have to fix this later! We want our data to be as "normal" as possible. Probably log transformation.

par(mfrow=c(2, 3))

##### 5.EDA #####

# Sale price vs Gr.Liv.Area vs Sale.Condition:

ggplot(data = train, aes(x = Gr.Liv.Area, y =SalePrice, colour=Sale.Condition))+ geom_jitter(size=1)+
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))

# Two of them with the living area bigger than 4500 square feet  sold under $200 000.
# They were also were sold partially which might not represent true market values.

train[( train$Gr.Liv.Area>4000 & train$SalePrice<200000), c("Sale.Condition")]

# The other one is just an unusual sale and outliers was sold under Abnorml condition.

train[(train$Gr.Liv.Area>4000 & train$SalePrice>650000), c("Sale.Condition")]

# A house sold under abnormal conditions often sells for much less than expected given its square footage. 
# Similarly, a partial sale of a house results in a higher price on average holding constant square footage.  
# Because houses with non-normal selling conditions exhibit atypical behavior and can disproportionately influence the model,
# I will only consider the properties sold unde Normal Sale condition.

# For my further analysis I will only leave those properties that were sold under "Normal" sale condition.

outliers <- outliers %>% filter(Sale.Condition == "Normal")

par(mfrow=c(2, 2))


# Area of the 1st and 2nd floors ("X1st.Flr.SF", "X2nd.Flr.SF") and above ground living area (Gr.Liv.Area):

nrow(train[train$X2nd.Flr.SF+train$X1st.Flr.SF==train$Gr.Liv.Area ,])
nrow(train)
table(train$Sale.Condition)
# We can see that 2023 out of 2051 properties have the sum of the first and the second floor eaqual to the above ground living area.
# In other words in 98.63% of the times the sum of X1st.Flr.SF and X2nd.Flr.SF is simply the Gr.Liv.Area therefore
# I am going to exlude 1st and 2nd floors are variables from my future modeling as redundant.

# Price vs. Neighborhood:

number <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
}
ggplot(data = train, aes(x =reorder(Neighborhood, SalePrice, median) , y =SalePrice,color=Neighborhood)) +geom_boxplot(outlier.size=0.2)+stat_summary(fun.data = number, geom = "text", fun.y = median)+ 
  scale_y_continuous(name = "Price", labels = function(y) paste0(y / 1000, "k"))+ ggtitle("The Neighborhood vs Price")+
  
  scale_x_discrete(name="Neighborhood", labels=c("Mdw","BrDl","IDTR","OldT","Edwr","BrkSd","Blst","Swyr","SWSU","Lndmk","NAms", "NPKvl",
                                                 "Mtchl", "SwyrW", "NWAms","Glbrt", "Blmgt", "ClgCr", 
                                                 "Grns","ClgCr", "Crwf", "Smrst","Tmbr", "Veen", "GrnHll", "NoRd" ,  "NrdgHt", "StnBr"))





table(train$Neighborhood)

g<-train %>%
  group_by(Neighborhood)%>%
  summarise(median.price=median(SalePrice), Iqr.price=IQR(SalePrice), counts=n())%>%
  arrange(desc(median.price, counts))
print(g)

# The plot shows that there is definitely strong relationship between the neighborhood and the price of the property.
# There are also some obvious outliers especially the cases of Old Town, NridgHt, NoRidge and StoneBr.
# The plot and the summary statistics show that the least expensive neighborhood is Meadow with the median price of  88250 USD
# and the most expensive as well as the most heterogeneous are StoneBr, NridgHt with the median price of 319000, 317750 
# and  interquartile range of  168216 and  125280 USD respectively. We can also see that there are 
# some levels with the cases as low as 1 which will cause problems in future modeling.

# Number of properties in each neighbourhood:

par(mfrow=c(1, 1))
neighb<-ggplot(train, aes(Neighborhood, fill=Neighborhood))+geom_histogram(stat="count")+ggtitle("Neighborhoods")+xlab("Neighborhood counts")+theme_bw()
print(neighb)
# Reordering levels in feature engeeniring part.


# Overall Quality vs Price.

table(data_full$Overall.Qual)
ggplot(data =train, aes(x =reorder(Overall.Qual, SalePrice, median) , y =SalePrice, color=Overall.Qual)) +geom_boxplot(outlier.size=0.2)+ geom_jitter(shape=16, position=position_jitter(0.1))+
  scale_y_continuous(name = "Price", labels = function(y) paste0(y / 1000, "k"))+ ggtitle("Quality vs Price")+
  scale_x_discrete(name="Quality")

# The boxplot shows very strong relationship between overall quality and price. Not a big surprise since
# the quality of the property is one of the most important factors when buying. There are
# some outliers. Three properties with overall quality were sold relatively low price (<200000),
# but they are all located in Edwards neighborhood which is one of the least expensive in the city.

data_full[(train$Overall.Qual==10 & train$SalePrice<200000),c("Neighborhood")]



# Overall Condition vs Price.

table(train$Overall.Cond)
ggplot(data = train, aes(x =reorder(Overall.Cond, SalePrice, median) , y =SalePrice, color=Overall.Cond)) +geom_boxplot(outlier.size=0.2)+ geom_jitter(shape=16, position=position_jitter(0.1))+
  scale_y_continuous(name = "Price", labels = function(y) paste0(y / 1000, "k"))+ ggtitle("Condition vs Price")+
  scale_x_discrete(name="Condition")


# On the other hand there are some levels with very low number of cases:

qual <- ggplot(train, aes(Overall.Qual, fill=Overall.Qual))+geom_histogram(stat="count")+ggtitle("Overall quality")+xlab("Rates of overall quality")+theme_bw()
table(train$Overall.Qual)

cond <- ggplot(train, aes(Overall.Cond, fill=Overall.Cond))+geom_histogram(stat="count")+ggtitle("Overall condition")+xlab("Rates of overall condition")+theme_bw()
table(train$Overall.Cond)

fig <- ggarrange(qual, cond,
                    ncol = 2, nrow = 1,
                    common.legend = TRUE, legend = "bottom")
print(fig)
# Further level transformation is needed.

# Year built and its distribution.

ggplot(data=train, aes(Year.Built, y = ..density.., colour="red")) +geom_density(size = 1, colour = 'brown')+ geom_histogram(bins = 30, fill = 'pink', colour = 'brown')


# The distribution shows multimodality and left-skewness of the data. We can see that there were a construction boom 
# from 1950 up to 1975, then after a drop of construction in the 80's building houses continued in  the early 90's again.
# Most properties have been built recently. 


# The median and the mean are arond 40 years, the minimum and the maximum are 1 and 139. The majority of the
# properties are relatively new (the mode is 7 years) while there are also some which are over 100 years old.

# The relationship between the age and the price:

ggplot(data = data_full, aes(x = Age, y =SalePrice, colour="blue"))+ geom_jitter(size=1)+
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))


# The scatter plot shows somewhat negative moderate to strong linear relationship between age and price. There are some outliers
# but they are mostly due to the overall condition of the properties. We can conclude that even really
# old houses can be sold at high prices if the condition is above 7.5 and vice versa.
# For my further modeling I am going to use "Age" variable instead of Year.Built:


# Remode Year Remod/Add (Discrete): Remodel date (same as construction date if no remodeling or additions)

ggplot(data = train, aes(x = Year.Remod.Add, y =SalePrice))+ geom_jitter(size=1)+
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))



##### 6. Missing values and variable type modifications. #####
# First I am going to drop all the cases which were not sold under normal conditions.
train <- train %>% filter(Sale.Condition == "Normal")
test <-  test %>% filter(Sale.Condition == "Normal")
data_full<-rbind(train, test)

##### 3. Understanding the structure of the data. Variables type modifications. #####

# The summary of its internal structue (for more details: https://github.com/poziryna84/Kschool_Final_Project/blob/master/DataSet_Description.RMD):

dim(data_full)

glimpse(data_full)

# MS.SubClass identifies the type of dwelling involved in the sale, e.g. "020" stands for	"1-STORY 1946 & NEWER ALL STYLES",
# and therefore should be converted into factor as well as Overall.Qual and Overall.Cond:

data_full$MS.SubClass<-as.factor(data_full$MS.SubClass)
data_full$Overall.Qual<-as.factor(data_full$Overall.Qual)
data_full$Overall.Cond<-as.factor(data_full$Overall.Cond)

# A few variables in the dataframe have NAs. 
summary(data_full)
sum((is.na(data_full)))
# To get a better picture I am going to visualize  them.
data_full[data_full=="" | data_full==" "] <- NA


NA_data_full <- data_full %>% 
  select_if(function(x) any(is.na(x))) %>% 
  summarise_all(funs(sum(is.na(.))))

NA_data_full<-sort(NA_data_full, decreasing = TRUE)

ggplot(,aes(x=as.integer(NA_data_full[1,]), y=variable.names(NA_data_full))) + 
  geom_point(size=3, show.legend = TRUE, colour="orange") + 
  ggtitle(label="MISSING VALUES")+ labs(x="the number of NAs", y="variables") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="brown", linetype="dashed"))

# For many predictors the number of NA values is too significant to ignore. The variable with the biggest number of missing values is "Pool.QC"(pool quality) - 2917 NAs:

sum(is.na(data_full$Pool.QC))


table(data_full$Pool.QC)

# As per data description document "NA"	stands for "No Pool". Iowa has a humid continental climate throughout the state with
# average low and high of -11.3	and -1.3	in the month of January; 17.6	and 29.1	in July accordingly particularly in Ames.
# Therefore it is only natural that pool is not the priority because its maintenance is rather costly keeping in mind that there are 
# only 200 sunny days a year on average.


# Appart from pool quality there are 14 other categorical variables  and 1 numeric -Garage.Yr.Blt where NA stands for no variable.
# I am going to set NA to "No_ftr" (no feature) in all of them and 0 in the case of Garage.Yr.Blt:

NA_cols<-c("Pool.QC", "Misc.Feature", "Alley", "Fence", "Fireplace.Qu","Garage.Type", "Garage.Qual", 
           "Garage.Finish","Garage.Cond", "BsmtFin.Type.1", "BsmtFin.Type.2","Bsmt.Qual",
           "Bsmt.Cond","Bsmt.Exposure")

length(NA_cols)

data_full[NA_cols]<-lapply(data_full[,NA_cols], function(x) fct_explicit_na(x, na_level = "No_ftr"))

data_full$Garage.Yr.Blt[is.na(data_full$Garage.Yr.Blt)] <- 0
# Age of garage variable:
data_full$Garage.Age<- sapply(data_full$Garage.Yr.Blt, function(x) 2011 - x)
data_full$Garage.Yr.Blt <- NULL

# The rest of the NAs:
NA_data_full <- data_full %>% 
  select_if(function(x) any(is.na(x))) %>% 
  summarise_all(funs(sum(is.na(.))))

NA_data_full<-sort(NA_data_full, decreasing = TRUE)

ggplot(,aes(x=as.integer(NA_data_full[1,]), y=variable.names(NA_data_full))) + 
  geom_point(size=3, show.legend = TRUE, colour="orange") + 
  ggtitle(label="MISSING VALUES")+ labs(x="the number of NAs", y="variables") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="brown", linetype="dashed"))


# LotFrontage Lot frontage means the portion of a lot that abuts a public or private street.

# There are 477 NAs.



# As it doesn´t say that NAs stand for "no frontage" I am going to replace it with its median value:

data_full$Lot.Frontage[is.na(data_full$Lot.Frontage)] <- median(data_full$Lot.Frontage, na.rm = TRUE)

# Regarding the rest of the variables the number of NA is negligible (less than 0.5%) so I'll 
# assume that NA are "None" for MasVnrType and 0 for MasVnrArea.

table(data_full$Mas.Vnr.Type)
data_full$Mas.Vnr.Area[is.na(data_full$Mas.Vnr.Area)] <- 0
data_full<-transform(data_full, Mas.Vnr.Type=fct_explicit_na(data_full$Mas.Vnr.Type, "None"))

#Basement variables:
# There is clearly no basement in the property 1301 therefore I am going to replace them with 0:
data_full[is.na(data_full$Bsmt.Half.Bath)   & is.na(data_full$Bsmt.Full.Bath),]
data_full[is.na(data_full$Bsmt.Half.Bath & is.na(data_full$Bsmt.Full.Bath)),c( "Bsmt.Full.Bath", "Bsmt.Half.Bath",  "Total.Bsmt.SF", "Bsmt.Qual", "Bsmt.Cond",  "BsmtFin.SF.1","BsmtFin.SF.2", "Bsmt.Unf.SF")] 

data_full$Total.Bsmt.SF[is.na(data_full$Total.Bsmt.SF)] <- 0
data_full$Bsmt.Unf.SF[is.na(data_full$Bsmt.Unf.SF)] <- 0
data_full$BsmtFin.SF.2[is.na(data_full$BsmtFin.SF.2)] <- 0
data_full$BsmtFin.SF.1[is.na(data_full$BsmtFin.SF.1)] <- 0
data_full$Bsmt.Half.Bath[is.na(data_full$Bsmt.Half.Bath)] <- 0
data_full$Bsmt.Full.Bath[is.na(data_full$Bsmt.Full.Bath)] <- 0



# There is only 1 NA in "Electrical, which I am going to replace with the most commun level "SBrkr"
# and also drop the "Mix" level since there are no properties with such electrical system:
sum(is.na(data_full$Electrical))
table(data_full$Electrical)

data_full<-transform(data_full, Electrical=fct_explicit_na(data_full$Electrical, "SBrkr"))

sum(is.na(data_full))

##### Feature engeneering part. #####

# Combining levels in Neghbouhood:

# This function determines levels that are similar to each other in 
# terms of their average value of some quantitative variable (SalePrice in this case).

f<-suggest_levels(SalePrice~Neighborhood,data=data_full,target=5,recode=TRUE) # the target number of 5 neighborhood

# allows us to have decent number of cases whithin each level.

length(unique(f$newlevels))

data_full$New_Neighb<-f$newlevels

table(data_full$New_Neighb)

# The new levels are A, B, C , E as follows:
f$Conversion2

new_neighb<-ggplot(data_full, aes(New_Neighb, fill=New_Neighb))+geom_histogram(stat="count")+ggtitle("New Neighborhoods")+xlab("New Neighborhood counts")+theme_bw()

new_price_neigh<-ggplot(data = data_full, aes(x =reorder(New_Neighb, SalePrice, median) , y =SalePrice,color=New_Neighb)) +geom_boxplot(outlier.size=0.2)+stat_summary(fun.data = number, geom = "text", fun.y = median)+ 
  scale_y_continuous(name = "Price", labels = function(y) paste0(y / 1000, "k"))+ ggtitle("The Neighborhood vs Price")


fig_neighb <- ggarrange(neighb, new_neighb, new_price_neigh,
                        ncol = 2, nrow = 2)
print(fig_neighb)

# Overall condition and Overall quality modifications:

# Qaulity and Cuantity variable levels will be transformed as follows:
# 10 Very Excellent  as exc 
# 9	Excellent        as exc 
# 8	Very Good        as exc
# 7	Good             as good
# 6	Above Average    as good
# 5	Average          as avg
# 4	Below Average    as blw_avg
# 3	Fair             as blw_avg
# 2	Poor             as blw_avg
# 1	Very Poor        as blw_avg

lev_reset<-function(vector){
  levels(vector)<-list(blw_avg=c("1","2","3", "4"),avg="5",good=c("6", "7"), exc=c("8", "9", "10"))
  vector
}
variables<-c("Overall.Qual", "Overall.Cond")

data_full[variables]<-lapply(data_full[,variables], function(x) lev_reset(x))

new_qual <- ggplot(data_full, aes(Overall.Qual, fill=Overall.Qual))+geom_histogram(stat="count")+ggtitle("Overall quality")+xlab("Rates of overall quality")+theme_bw()
table(data_full$Overall.Qual)

new_cond <- ggplot(data_full, aes(Overall.Cond, fill=Overall.Cond))+geom_histogram(stat="count")+ggtitle("Overall condition")+xlab("Rates of overall condition")+theme_bw()

new_fig <- ggarrange(qual, cond, new_qual,new_cond, 
                     ncol = 2, nrow = 2)

print(new_fig)


# Since all the quality and condition variables are ordinal, have a clear ordering 
# and share the same legend it makes sense to analyse and/or modify them separately.


g_c<-ggplot(data_full, aes(Garage.Cond, fill=Garage.Cond))+geom_histogram(stat="count")
g_q<-ggplot(data_full, aes(Garage.Qual, fill=Garage.Qual))+geom_histogram(stat="count")
heat<-ggplot(data_full, aes(Heating.QC, fill=Heating.QC))+geom_histogram(stat="count")
bsmt_cond<-ggplot(data_full, aes(Bsmt.Cond, fill=Bsmt.Cond))+geom_histogram(stat="count")
kitch<-ggplot(data_full, aes(Kitchen.Qual, fill=Kitchen.Qual))+geom_histogram(stat="count")
pool<-ggplot(data_full, aes(Pool.QC, fill=Pool.QC))+geom_histogram(stat="count")
fpl<-ggplot(data_full, aes(Fireplace.Qu, fill=Fireplace.Qu))+geom_histogram(stat="count")
extq<-ggplot(data_full, aes(Exter.Qual, fill=Exter.Qual))+geom_histogram(stat="count")

extc<-ggplot(data_full, aes(Exter.Cond, fill=Exter.Cond))+geom_histogram(stat="count")
bsmt_q<-ggplot(data_full, aes(Bsmt.Qual, fill=Bsmt.Qual))+geom_histogram(stat="count")



figure <- ggarrange(g_c, g_q, heat, bsmt_cond, bsmt_q, kitch, pool, fpl, extq, extc,
                    ncol = 3, nrow = 4,
                    common.legend = TRUE, legend = "bottom")


print(figure)

# Reseting the levels to blw_avg, avg, abv_avg and No_ftr:

nu<-function(vector){
  levels(vector)<-list(blw_avg=c("Fa","Po"),avg="TA",abv_avg=c("Ex", "Gd"), No_ftr=("No_ftr"))
  vector
}
s<-c("Heating.QC", "Garage.Qual", "Garage.Cond", "Bsmt.Cond", "Kitchen.Qual", "Pool.QC", "Fireplace.Qu", "Exter.Qual", "Exter.Cond", "Bsmt.Qual")

# Applying:

data_full[,s]<-lapply(data_full[,s], function(x) nu(x))



# Identifying unused factor levels

zero_levels<- function(dataset, number){
  factor_var <- sapply(dataset, is.factor)
  factor_df <-dataset[, factor_var]
  factors<-c()
  for (column in names(factor_df)){
    
    levels_ = table(factor_df[[column]])
    levels_df = as.data.frame(levels_)
    
    if (min(levels_df$Freq)<=number){
      factors<-append(factors,column,after = length(factors)) 
      
    }
    
    
    
    
  }
  return(factors)
}


zero_levels(data_full, number=0)


# drops the levels that do not occur:

data_full[zero_levels(data_full, number = 0)]<-lapply(data_full[,zero_levels(data_full, number = 0)], function(x) droplevels(x))    # drops the levels that do not occur

# Combining levels

# To combine levels using their frequency, we first look at the frequency distribution of each level 
# and combine levels that have frequency less than 5% of total observation into "Other" level.

# Function to spot variables with levels that have frequency less than 5%:

lowvar_levels<- function(dataset, prct){
  factor_var <- sapply(dataset, is.factor)
  factor_df <-dataset[, factor_var]
  factors<-c()
  for (column in names(factor_df)){
    
    levels_ = table(factor_df[[column]])
    levels_df = as.data.frame(levels_)
    levels_df$perc<-100/nrow(factor_df)*levels_df$Freq
    
    if (min(levels_df$perc)<prct){
      factors<-append(factors,column,after = length(factors)) 
      
    }
    
    
    
    
  }
  return(factors)
}


l<-lowvar_levels(data_full, 5)
print(l)

# Function condenseMe(vector, name, limit) sets low frequency counts into an 'Other' category:

condenseMe <- function(vector, name, limit) {
  
  toCondense <- names(which(prop.table(table(vector)) < limit))
  levels(vector)[levels(vector) %in% toCondense] <- name
  
  vector
}

# Apllying the function:

# Important: since all the quality and condition variables are ordinal and therefore have a clear ordering I won?t apply
# it to them in order not to mix the order.

u<-lowvar_levels(data_full, 5)

# Filtering the quality and condition variables:

q<-u[ !grepl( "Q" , u, fixed = TRUE ) & !grepl( ".Cond" , u, fixed = FALSE )]

#Applying the function:

data_full[q]<-lapply(data_full[,q], function(x) condenseMe(x, limit = 0.05, name = "Other"))

df<-data_full
df1<-data_full

# Creating new variables:
# To see distribution of the age of the properties and how it is related to the price I am going to create a new variable "Age".

data_full$Age <- sapply(data_full$Year.Built, function(x) 2011 - x) #2011 as it is 2006 to 2010 data.

mode <- function(x) {
  un <- unique(x)
  un[which.max(tabulate(match(x, un)))]
}
max(data_full$Age)
mode(data_full$Age)
ggplot(data = data_full, aes(x = Age, y = ..density..)) +
  geom_histogram(bins = 30, fill = 'yellow', colour = 'black')+geom_density(size = 1, colour = 'brown')+
  labs(title = "Histogram - ages of houses in years", x = 'Age', y = "Frequency")+  
  geom_vline(data = data_full, mapping = aes( xintercept = mean(data_full$Age), colour = 'mean'), size = 1.5)+
  geom_vline(data = data_full,mapping = aes( xintercept = median(data_full$Age), colour = 'median'), size = 1.5)+
  geom_vline(data = data_full,mapping = aes( xintercept = mode(data_full$Age), colour = 'mode'), size = 1.5)

# The histogram similarly to the above one shows a multimodal right-skewed distribution. Most of the builings are new 
# Summary of the age:

data_full %>% summarise(Age_mean = mean(Age),
                        Age_median = median(Age),
                        Age_stdv = sd(Age),
                        Age_min = min(Age),
                        Age_max = max(Age),
                        Age_iqr = IQR(Age))

# Creating a new variable "Remode" that will indicate how many years ago the building was remodeled.

data_full$Remode <- sapply(data_full$Year.Remod.Add, function(x) 2011 - x)

ggplot(data = data_full, aes(x =Remode , y =SalePrice))+ geom_jitter(size=1)+
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))

# For my further modeling I am going to use "Remode" variable instead of Year.Remod.Add:

# Filtering for outliers and drops.


data_full <- data_full %>% filter(Sale.Condition == "Normal")
dim(data_full)
drops<-c("Year.Built", "Year.Remod.Add", "Sale.Condition", "X1st.Flr.SF", "X2nd.Flr.SF", "Neighborhood", "PID","Mo.Sold", "Yr.Sold" )

data_full<-data_full[ , !(names(data_full) %in% drops)] 

##### 8. Identify zero and near zero variance predictors. Feature Selection in R with the Boruta R Package. #####

# One interesting aspect of this dataset is that it contains many variables and many of these variables have extemely low variances. 
# This means that there is very little information in these variables because they mostly consist of a single level or value (e.g. zero).

# I am going to use nearZeroVar() for removing such variables to save time during modeling.
# By default, caret uses freqCut = 19 and uniqueCut = 10, which is fairly conservative.


remove_cols <- nearZeroVar(data_full, names = TRUE, 
                           freqCut = 19, uniqueCut = 10)

agr_remove_cols <- nearZeroVar(data_full, names = TRUE, 
                           freqCut = 2, uniqueCut = 12)
all_cols<-names(data_full)

data_full <- data_full[ , setdiff(all_cols, remove_cols)]
agr_data_full<-data_full[ , setdiff(all_cols, agr_remove_cols)]
str(agr_data_full)


##### 10. Feature selection. Wrapper method. Boruta. #####

df <- data_full
str(data_full)
dim(data_full)
# Splitting it back

train <- subset(data_full, Order %in% train$Order)
test <- subset(data_full, Order %in% test$Order)
train$Order<-NULL
test$Order<-NULL
dim(train)
dim(test)

#Linear models do not require variables to have a Gaussian distribution 
# (only the errors / residuals must be normally distributed); they do require, 
# however, a linear relation between the dependent and independent variables. 
# The distribution of the amount of rain is right-skewed, 
# and the relation with some other variables is highly non-linear. 
# For this reason of linearity, and also to help fixing the problem with 
# residuals having non-constant variance across the range of predictions (called heteroscedasticity),
# we will do the usual log transformation to the dependent variable.
train$SalePrice<-log(train$SalePrice)

set.seed(123)

boruta.train <- Boruta(SalePrice~., data = train, doTrace = 2)
par(mfrow=c(1,1)) # reset the screen 
print(boruta.train)

# Plot the result:
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

# Tentative attributes
final.boruta <- TentativeRoughFix(boruta.train)

print(final.boruta)

# Final plot

lz<-lapply(1:ncol(final.boruta$ImpHistory),function(i)
  final.boruta$ImpHistory[is.finite(final.boruta$ImpHistory[,i]),i])
names(lz) <- colnames(final.boruta$ImpHistory)
Labels <- sort(sapply(lz,median))
plot(final.boruta, xlab = "", xaxt = "n")
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(final.boruta$ImpHistory), cex.axis = 0.7)

# It's time for results now. Let's obtain the list of confirmed attributes

length(getSelectedAttributes(final.boruta, withTentative = F))
# [1] "MS.SubClass"    "MS.Zoning"      "Lot.Frontage"   "Lot.Area"       "Lot.Shape"     
# [6] "Condition.1"    "Bldg.Type"      "House.Style"    "Overall.Qual"   "Overall.Cond"  
# [11] "Roof.Style"     "Exterior.1st"   "Exterior.2nd"   "Mas.Vnr.Type"   "Mas.Vnr.Area"  
# [16] "Exter.Qual"     "Foundation"     "Bsmt.Qual"      "Bsmt.Exposure"  "BsmtFin.Type.1"
# [21] "BsmtFin.SF.1"   "BsmtFin.SF.2"   "Bsmt.Unf.SF"    "Total.Bsmt.SF"  "Heating.QC"    
# [26] "Central.Air"    "Electrical"     "Gr.Liv.Area"    "Bsmt.Full.Bath" "Full.Bath"     
# [31] "Half.Bath"      "Bedroom.AbvGr"  "Kitchen.Qual"   "TotRms.AbvGrd"  "Fireplaces"    
# [36] "Fireplace.Qu"   "Garage.Type"    "Garage.Finish"  "Garage.Cars"    "Garage.Area"   
# [41] "Garage.Qual"    "Garage.Cond"    "Paved.Drive"    "Wood.Deck.SF"   "Fence"         
# [46] "New_Neighb"     "Age"            "Remode"         "Garage.Age" 

# We'll create a data frame of the final result derived from Boruta.

boruta.df <- attStats(final.boruta)


class(boruta.df)

print(boruta.df)

# 20 most important var:
# https://www.kaggle.com/erick5/predicting-house-prices-with-machine-learning/code

f<- names(final.boruta$finalDecision[final.boruta$finalDecision %in% c("Confirmed")])
f<-names(Labels)
f<-rev(f)
most_imp<-f[(length(f)-56):length(f)]
most_imp<-append(f, "SalePrice", after = length(f))
length(f)
length(most_imp)
# Filtering the dataset:
dim(test)
test21 = test[, most_imp]
names(test21)

train21 <- train[, most_imp]
dim(test21)
dim(train21)
str(train21)



#create a dummy train and test:
#train 21:
f<-dummyVars("~ .", data=train21, fullRank = FALSE)
train21<- data.frame(predict(f, newdata=train21))
#test21
foo<-dummyVars("~ .", data=test21, fullRank = FALSE)
test21<- data.frame(predict(foo, newdata=test21))

predictors_train <- subset(train21, select = -c(SalePrice))

predictors_test  <-  subset(test21, select = -c(SalePrice))
dim(train21)

# Using k-fod validation:
set.seed(345)
myControl <- trainControl(
  method = "cv", number = 10,
  verboseIter = TRUE
)

# 1. lm model using PCA as preprocessing method:
m2<-train(predictors_train, train21$SalePrice, method = "lm", trControl = myControl,preProcess=c("zv", "center",
                                                                            "scale", "pca",  "spatialSign"))

print(m2) # RMSE =0.1049536 Rsquared  = 0.9183646
min(m2$results$RMSE)


# 2. glm linear model:

mg<-train(predictors_train, exp(train21$SalePrice), method = "glm", trControl = myControl,preProcess=c("zv", "center",
                                                                                                 "scale", "pca",  "spatialSign"))
print(mg) # RMSE = 0.104495 


# 3. glmnet model:

model_glm <- train(x =predictors_train, y =train21$SalePrice,
               metric = "RMSE",
               method = "glmnet",
               trControl = myControl,
               preProcess=c("center","scale")
              
)
print(model_glm)

print(min(model_glm$results$RMSE)) # RMSE = 0.103496
plot(model_glm)
summary(model_glm)

# 4. Train glmnet with custom trainControl and tuning: model
set.seed(42)
model_glm_tune <- train(x =predictors_train, y =train21$SalePrice,
                        metric = "RMSE",
                   tuneGrid = expand.grid(alpha = 0:1,
                                          lambda =0:10/10),
                   
                   method = "glmnet",
                   trControl = myControl,
                   preProcess=c("center","scale")
                   
)
# for lambda: seq(0.0001, 100, length = 150))
print(model_glm_tune)
print(min(model_glm_tune$results$RMSE)) #  0.1044852
plot(model_glm_tune)
plot(model_glm_tune$finalModel) # suggests ridge regression


# 5. Simple random forest:

# No need to pre process
# Rather than using the classic randomForest package, you'll be 
# using the ranger package, which is a re-implementation of randomForest

model_rf = train(x =predictors_train, y =train21$SalePrice,
                 metric="RMSE",
                 method = "ranger",
                 trControl = myControl)
#Here carot automatically choses the best number of mtry (number of randomly selected variables here It suggests Fitting mtry = 73, )
# But it´s always a good idea to visualize the results
plot(model_rf)
print(model_rf)
# RMSE was used to select the optimal model using the smallest value which in this case is:
print(min(model_rf$results$RMSE))#  0.1113404 with Rsquared = 0.9120005

# Tuning a random forest model specifying tune length equal to 10.
# It will take longer than the default tune length which is equal to three
# and might potential find a more accurate model at the expense of longer waiting for it to run:

# 6. Tuned forest:
model_rf2 = train(x =predictors_train, y =train21$SalePrice,
                  tuneLength = 10,
                  method = "ranger",
                  metric="RMSE",
                  
                  trControl = myControl)

# the best number of mtry = 144

plot(model_rf2)
print(model_rf)
print(min(model_rf2$results$RMSE))#  0.1093362 with Rsquared = 0.9120005

# Comparing the models:
# compare their out-of-sample predictions and choose which one is the 
# best model for my dataset.
# In general, you want the model with the lower median RMSE, as well as a 
# smaller range between min and max RMSE.

model_list <- list( m2 = m2,mg=mg, model_glm = model_glm, model_glm_tune = model_glm_tune, 
                   model_rf= model_rf, model_rf2=model_rf2)
# Pass model_list to resamples(): resamples
resamples <- resamples(model_list)
# Summarize the results
summary(resamples)

# Visualize the results:

bwplot(resamples, metric = "Rsquared")# check both Rsquared and Rmse
bwplot(resamples, metric = "RMSE")
dotplot(resamples, metric = "RMSE")
densityplot(resamples, metric = "RMSE")










# Model 6 - stochastic gradient boosting machine
model_gbm = train(x =predictors_train, y =train21$SalePrice,
                  tuneLength = 2,
                  method = "gbm",
                  trControl = myControl,
                  preProcess=c( "center",
                                "scale"))
model_gbm # Rsquared - 0.9104542,  RMSE  - 0.1097472

# Model 7 - stochastic gradient boosting machine with custom tuning grid:
gbmTuningGrid = expand.grid(interaction.depth = 4, 
                            n.trees = c(50, 100, 150, 200), 
                            shrinkage = 0.3,
                            n.minobsinnode = 20)
model_gbm2 = train(x =predictors_train, y =train21$SalePrice,
                   tuneLength = 3,
                   method = "gbm",
                   trControl = myControl,
                   tuneGrid = gbmTuningGrid,
                   preProcess=c( "center",
                                 "scale"))
model_gbm2 # Rsquared - 0.9069945, RMSE - 0.1117653

# Model 8 - extreme gradient boosting:
xgbTuningGrid = expand.grid(nrounds = c(50, 100), 
                            lambda = seq(0.1, 0.5, 0.1), 
                            alpha = seq(0.1, 0.5, 0.1),
                            eta = c(0.3, 0.4))
model_xgb4 = train(x =predictors_train, y =train21$SalePrice,
                   tuneLength = 3,
                   method = "xgbLinear",
                   trControl = myControl,
                   tuneGrid = xgbTuningGrid)
model_xgb4

# Predicting using the  models:

# 1. lm model using PCA as preprocessing method:

pm2<- predict(m2, predictors_test)

errorm2<-pm2-log(test21$SalePrice)
print(sqrt(mean(errorm2^2))) # 0.1031118
print(m2$results$RMSE) # 0.1049536

# Exp MRSE

pred1 <-exp(pm2)
error1<-pred1-(test21$SalePrice)
print(sqrt(mean(error1^2))) # 19444.39


# 2. glm linear model:

pmg<- predict(mg, predictors_test)

errormg<-pmg-log(test21$SalePrice)
print(sqrt(mean(errormg^2))) # 0.1031118
print(mg$results$RMSE) # 0.104495

# Exp MRSE
pred2 <-exp(pmg)
error2<-pred2-(test21$SalePrice)
print(sqrt(mean(error2^2))) # 19444.39 

# 3. glmnet model:

pmglm<- predict(model_glm, predictors_test)

errorglm<-pmglm-log(test21$SalePrice)
print(sqrt(mean(errorglm^2))) # 0.1047751
print(median(model_glm$results$RMSE)) # 0.103496

# Exp MRSE:

pred3 <-exp(pmglm)
error3<-pred3-(test21$SalePrice)
print(sqrt(mean(error3^2))) # 19616.14 

# 4. Train glmnet with custom trainControl and tuning: model

pmglmt<- predict(model_glm_tune, predictors_test)

errorglmt<-pmglmt-log(test21$SalePrice)
print(sqrt(mean(errorglmt^2))) # 0.1033023
print(median(model_glm_tune$results$RMSE)) #  0.1365016
exp(model_glm_tune$results$RMSE)

# Exp MRSE

pred4 <-exp(pmglmt)
error4<-pred4-(test21$SalePrice)
print(sqrt(mean(error4^2))) # 19071.96

# 5. Simple random forest:

pmodel_rf<-predict( model_rf, predictors_test)

errorrf<-pmodel_rf-log(test21$SalePrice)
print(sqrt(mean(errorrf^2))) # 0.1191724
print(median(model_rf$results$RMSE)) #   0.1183233
print(head(exp(pmodel_rf)))
print(head((test21$SalePrice)))

# Exp MRSE

pred5 <-exp(pmodel_rf)
error5<-pred5-(test21$SalePrice)
print(sqrt(mean(error5^2))) # 26707.64

# 6. Tuned forest:

pmodel_rf_2<-predict(model_rf2, predictors_test)

errorrf2<-pmodel_rf_2-log(test21$SalePrice)
print(sqrt(mean(errorrf2^2))) # 0.1189721
print(median(model_rf2$results$RMSE)) # 0.1176338
model_rf2$finalModel

# Exp MRSE

pred6 <-exp(pmodel_rf_2)
pred6<-pred6-(test21$SalePrice)
print(sqrt(mean(pred6^2)))  # 26950.26


