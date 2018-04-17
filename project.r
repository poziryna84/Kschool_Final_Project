##### 1. Loading Dataset. #####

data_full = read.csv("AmesHousing.csv", header=TRUE, sep = ";")



##### 2. Loading packages #####

library(dplyr)# data manipulation #
library('plyr')         # data manipulation
library('ggplot2')      # library to create plots #   
require("stringi")      # string/text processing
library('statsr')       # staistics functions
library(forcats)        # reordering and modifying factor levels

library("naniar")       # replace specified values with an NA
library(GGally)
library(caret)          #near z-predictors
library(gdata)          # to use drop levels
##### 3. Understanding the structure of the data. Variable type modifications. #####


# The summary of its internal structue (for more details: https://github.com/poziryna84/Kschool_Final_Project/blob/master/DataSet_Description.RMD):



# As our response variable is SalePrice it´s a good idea to take a look at its distribution and shape to decide
# on the most appropriate measures of center and spread; whether or not any varibly transformation is needed; and
# outliers strategy. 

summary(data_full$SalePrice)

# With the minimum price of $ 12789 and the maximum of $ 755 000 the mean of $ 180 796 is higher than the median $ 160 000,
# which indicates the skewness of the distribution:

ggplot(data_full, aes(x=SalePrice,y = ..density.., colour="red" )) +geom_density(size = 1, colour = 'brown')+
  geom_histogram(bins = 30, fill = 'pink', colour = 'brown') +  scale_x_continuous("Sales Price in thousands",labels=function(x)x/1000)+
  labs(title = "Histogram - Prices of houses", x = 'Age', y = "Frequency")+
  geom_vline(data = data_full, mapping = aes( xintercept = mean(data_full$SalePrice), colour = 'mean'), size = 1.5)+
  geom_vline(data = data_full,mapping = aes( xintercept = median(data_full$SalePrice), colour = 'median'), size = 1.5)+
  geom_vline(data = data_full,mapping = aes( xintercept = mode(data_full$SalePrice), colour = 'mode'), size = 1.5)

# It is a unimodal right-skewed distribution. To make it easier to model I will log transform it and use its median and 
# and interquartile range as measures of center and variability for the further EDA  as they are robust to the outliers.


ggplot(data_full, aes(x=log(SalePrice), y = ..density.., colour="blue" )) +geom_density(size = 1, colour = 'brown')+
  geom_histogram(bins = 30, fill = 'pink', colour = 'brown')

# Price vs. Neighborhood:

ggplot(data = data_full, aes(x =reorder(Neighborhood, SalePrice, median) , y =SalePrice,color=Neighborhood)) +geom_boxplot(outlier.size=0.2)+ geom_jitter(shape=16, position=position_jitter(0.1))+
  scale_y_continuous(name = "Price", labels = function(y) paste0(y / 1000, "k"))+ ggtitle("The Neighborhood vs Price")+
  
  scale_x_discrete(name="Neighborhood", labels=c("Mdw","BrDl","IDTR","OldT","Edwr","BrkSd","Blst","Swyr","SWSU","Lndmk","NAms", "NPKvl",
                                                 "Mtchl", "SwyrW", "NWAms","Glbrt", "Blmgt", "ClgCr", 
                                                 "Grns","ClgCr", "Crwf", "Smrst","Tmbr", "Veen", "GrnHll", "NoRd" ,  "NrdgHt", "StnBr"))

data_full %>%
  group_by(Neighborhood)%>%
  summarise(median.price=median(SalePrice), Iqr.price=IQR(SalePrice))%>%
  arrange(desc(median.price), Iqr.price)

# The plot shows that there is definitely strong relationship between the neighborhood and the price of the property.
# There are also some obvious outliers especially the cases of Old Town, NridgHt, NoRidge and StoneBr.
# The plot and the summary statistics show that the least expensive neighborhood is Meadow with the median price of  88250 USD
# and the most expensive as well as the most heterogeneous are StoneBr, NridgHt with the median price of 319000, 317750 
# and  interquartile range of  168216 and  125280 USD respectively.

# Condition.1 (proximity to various conditions) and condition.2 (proximity to various conditions if more than one is present).
# For convenience I am going to create a new column that´ll combine both conditions and modify its levels to avoid repetitions.

data_full$Conditions<-as.factor(paste(data_full$Condition.1, data_full$Condition.2, sep = "/"))

levels(data_full$Conditions) <- gsub("Norm/Norm", "Norm", levels(data_full$Conditions))
levels(data_full$Conditions) <- gsub("PosA/PosA", "PosA", levels(data_full$Conditions))
levels(data_full$Conditions) <- gsub("Artery/Artery", "Artery", levels(data_full$Conditions))
levels(data_full$Conditions) <- gsub("Feedr/Feedr", "Feedr", levels(data_full$Conditions))
levels(data_full$Conditions) <- gsub("PosN/PosN", "PosN", levels(data_full$Conditions))
levels(data_full$Conditions) <- gsub("RRNn/Feedr", "Feedr/RRNn", levels(data_full$Conditions))
levels(data_full$Conditions) <- gsub("RRAn/Feedr", "Feedr/RRAn", levels(data_full$Conditions))
drops_c <- c("Condition.1", "Condition.2")
data_full<-data_full[ , !(names(data_full) %in% drops_c)]

# Visualizing the relationship between proximity to various conditions and price:
# cHECK WHAT NUMBER IS! 
number <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
}

ggplot(data_full, aes(x=reorder(Conditions, SalePrice, median), y= SalePrice, colour = Conditions)) + 
  geom_boxplot() +
  stat_summary(fun.data = number, geom = "text", fun.y = median)+
  scale_y_continuous(name = "Price", labels = function(y) paste0(y / 1000, "k"))+
  scale_x_discrete(name="Proximity to Conditions", labels=c("Artery","RRAan/Art","Feedr","Artery/Nrm","Feed/RRNn","RRNn/Art","Feed/RRAn","Feed/Nrm","RRAe/Nrm","Feed/Art","RRNe/Nrm", "Norm","RRAn/Nrm", "Fdr/RRAe", "PosA/Nrm", "PosN/Nrm", "RRNn/Nrm","Art/PosA", "PosN", "PosA"))+
  ggtitle("Proximity to conditions vs Price")

data_full%>%
  group_by(Conditions)%>%
  summarise(median=median(SalePrice),  Iqr.price_c=IQR(SalePrice))%>%
  arrange(desc(median))


# The boxplot shows that the majority(2522) of the properties have normal proximity to various conditions
# Even though there are just a couple of houses with PosA	(adjacent to postive off-site feature) or 
# PosN (near positive off-site feature--park, greenbelt, etc.) conditions, their median prices are considerably higher.
# On the other hand those with  Artery (adjacent to arterial street) and Feedr (adjacent to feeder street) show
# a lot more lower results.

# Neighborhood vs. MS.Zoning: 

data_full %>%
  group_by(Neighborhood, MS.Zoning) %>%
  summarise(counts = n())%>%
  arrange(desc(counts))

ggplot(df, aes(x = reorder(Neighborhood, counts), y = counts, fill=MS.Zoning)) +
  geom_bar( stat = "identity") +
  geom_text(aes(label = counts), hjust = -0.3) + coord_flip()+
  ggtitle("Neighborhood Zoning")

# The plot shows that the neighborhoods with the biggest number of properties are
# NAmes, CollgCr, OldTown and Edwards while Landmark and Green Hill are the ones with the fewest properties.
# Considering Zonings the vast majority of the buildings belongs to RL and RM which stand for
# Residential Low Density and Residential Medium Density Zones; 139 are located in Floating Village Residential (FV)
# 22 - Commercial (C) and very few are in Residential High Density (RH). If we look closely
# at the contigency table and histogram we can see that Zonings such as A (Agriculture)  and I (Industrial)
# have only two properties each: 

table(data_full$MS.Zoning)
ggplot(data_full, aes(MS.Zoning, fill=MS.Zoning))+geom_histogram(stat="count")+ggtitle("Zonings count")+xlab("Zonings")+theme_bw()


# MS SubClass identifies the type of dwelling involved in the sale. 

table(data_full$MS.SubClass)
ggplot(data_full, aes(as.factor(MS.SubClass), fill=as.factor(MS.SubClass)))+geom_histogram(stat="count")+ggtitle("Dwelling type")+xlab("Zonings")+theme_bw()

# The histogram and the table show that the majority of the houses (1077) falls into class "020"	(1-STORY 1946 & NEWER ALL STYLES) 
# and "060"	2-STORY 1946 & NEWER while there is only 1 property
# that falls into class 150	(1-1/2 STORY PUD - ALL AGES), therefore I am going to drop it as well:


# Overall Quality vs Price.

table(data_full$Overall.Qual)
ggplot(data = data_full, aes(x =reorder(Overall.Qual, SalePrice, median) , y =SalePrice, color=Overall.Qual)) +geom_boxplot(outlier.size=0.2)+ geom_jitter(shape=16, position=position_jitter(0.1))+
  scale_y_continuous(name = "Price", labels = function(y) paste0(y / 1000, "k"))+ ggtitle("Quality vs Price")+
  scale_x_discrete(name="Quality")

# The boxplot shows very strong relationship between overall quality and price. Not a big surprise since
# the quality of the property is one of the most important factors when buying. There are
# some outliers. Three properties with overall quality were sold relatively low price (<200000),
# but they are all located in Edwards neighborhood which is one of the least expensive in the city.

data_full[(data_full$Overall.Qual==10 & data_full$SalePrice<200000),c("Neighborhood")]

table(as.factor(data_full$Overall.Qual))
ggplot(data_full, aes(x = Overall.Qual, fill=as.factor(Overall.Qual))) + 
  geom_bar() +
  facet_wrap(~ Neighborhood)

# Overall Condition:

ggplot(data_full, aes(as.factor(Overall.Cond), fill=as.factor(Overall.Cond)))+geom_histogram(stat="count")+ggtitle("Overall condition")+xlab("Rates of overall condition")+theme_bw()

table(as.factor(data_full$Overall.Cond))
ggplot(data_full, aes(x = Overall.Cond, fill=as.factor(Overall.Cond))) + 
  geom_bar() +
  facet_wrap(~ Neighborhood)

# The majority of the properties have around average (4-7) overall condition with 1652 of them
# having average overall condition 5. NAmes, Edwards and Old Town neighborhoods are the most varied Condition wise, which
# can be explained by the biggest number of the properties there.

# Fireplaces:

table(as.factor(data_full$Fireplaces))
ggplot(data = data_full, aes(x =reorder(as.factor(Fireplaces), SalePrice, median) , y =SalePrice,color=as.factor(Fireplaces))) +geom_boxplot(outlier.size=0.2)+ 
    scale_y_continuous(name = "Price", labels = function(y) paste0(y / 1000, "k"))+ ggtitle("Number of fireplaces vs Price")+
    scale_x_discrete(name="Number of fireplaces")

# The number of fireplaces seems to be related to the price too. 
ggplot(data_full, aes(House.Style, fill=House.Style))+geom_histogram(stat="count")+theme_bw()
table(data_full$Electrical)

levels(data_full$Electrical)
ggplot(data = data_full, aes(x =reorder(Electrical, SalePrice, median) , y =SalePrice,color=Electrical)) +geom_boxplot(outlier.size=0.2)+ 
  scale_y_continuous(name = "Price", labels = function(y) paste0(y / 1000, "k"))+ ggtitle("Number of fireplaces vs Price")+
  scale_x_discrete(name="Building Type")




# Outliers analysis and filtering the data set.

# Creating a plot of SALE PRICE vs GR LIV AREA to identify the outliers:

ggplot(data = data_full, aes(x = Gr.Liv.Area, y =SalePrice, colour=Sale.Condition))+ geom_jitter(size=1)+
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))


# Three of them with the living area bigger than 4500 square feet (418.0636 square meters) sold at low prices of less than $200 000.
# They were also were sold partially and might not represent true market values.

data_full[(data_full$Gr.Liv.Area>4000 & data_full$SalePrice<200000), c("Sale.Condition")]



# The other two (order numbers 1761 1768) are just unusual sales. Very large houses and quite appropriate prices.Though one of them was sold under abnormal sale 
# conditions.

data_full[(data_full$Gr.Liv.Area>4000 & data_full$SalePrice>650000), c("Sale.Condition")]

# For my further analysis I will remove any houses with above grade (ground) living area  larger 
# than 4000 square feet from the data set and only leave those that were sold under "Normal" sale condition.

# Lot Area vs. Sale Price shows some significant outliers and non-linear relationship between the two variables:
# Check its distribution first:
ggplot(data=data_full, aes(Lot.Area, y = ..density.., colour="red")) +geom_density(size = 1, colour = 'brown')+ geom_histogram(bins = 30, fill = 'pink', colour = 'brown')

ggplot(data = data_full, aes(x = Lot.Area, y =SalePrice, colour=Land.Slope))+ geom_jitter(size=1)+
scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))

# There are four properties with unusually big lot areas that were sold just above the average prices.

data_full[(data_full$Lot.Area>100000 & data_full$SalePrice<400000),c("SalePrice",  "Land.Slope","Lot.Config", "Lot.Shape","Year.Built")]

# If we take a closer look at them we can see that they were all built before 1971, but most importantly all four of them
# are located on a sever slope and have slightly irregular to irregular shape of property. One of them is located on the corener,
# and two have CulDSac lot configuration which which stands for "cul-de-sac" or round end of the street (lollipop shape) meaning the houses are at the end 
# of very steep streets and might be difficult to reach.

# These are good reasons for the prices being a bit lower than expected. To reduce skew and to straighten a nonlinear relationship in a scatterplot,
# I am going to log transform the Lot.Area too.


ggplot(data = data_full, aes(x = log(Lot.Area), y =log(SalePrice), colour=Land.Slope))+ geom_jitter(size=1)+
  scale_y_continuous(name = "Sale Price")


# Year built and its distribution.

ggplot(data=data_full, aes(Year.Built, y = ..density.., colour="red")) +geom_density(size = 1, colour = 'brown')+ geom_histogram(bins = 30, fill = 'pink', colour = 'brown')


# The distribution shows multimodality and left-skewness of the data. We can see that there were a construction boom 
# from 1950 up to 1975, then after a drop of construction in the 80's building houses continued in  the early 90's again.
# Most properties have been built recently. 

# To see distribution of the age of the properties and how it is related to the price I am going to create a new variable "Age".

data_full$Age <- sapply(data_full$Year.Built, function(x) 2011 - x) #2011 as it is 2006 to 2010 data.

mode <- function(x) {
  un <- unique(x)
  un[which.max(tabulate(match(x, un)))]
}
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

# The median and the mean are arond 40 years, the minimum and the maximum are 1 and 139. The majority of the
# properties are relatively new (the mode is 7 years) while there are also some which are over 100 years old.

# The relationship between the age and the price:
ggplot(data = data_full, aes(x = Age, y =SalePrice, colour=Overall.Cond))+ geom_jitter(size=1)+
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))


# The scatter plot shows somewhat negative moderate to strong linear relationship between age and price. There are some outliers
# but they are mostly due to the overall condition of the properties. We can conclude that even really
# old houses can be sold at high prices if the condition is above 7.5 and vice versa.
# For my further modeling I am going to use "Age" variable:
data_full<-data_full[ , !(names(data_full) %in% "Year.Built")] 

#,Remode Year Remod/Add (Discrete): Remodel date (same as construction date if no remodeling or additions)

ggplot(data = data_full, aes(x = Year.Remod.Add, y =SalePrice, colour=Overall.Cond))+ geom_jitter(size=1)+
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))

# Creating a new variable "Remode" that will indicate how many years ago the building was remodeled.

data_full$Remode <- sapply(data_full$Year.Remod.Add, function(x) 2011 - x)

ggplot(data = data_full, aes(x =Remode , y =SalePrice, colour="red"))+ geom_jitter(size=1)+
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))

# For my further modeling I am going to use "Remode" variable:
data_full<-data_full[ , !(names(data_full) %in% "Year.Remod.Add")] 

# Facet wrap !!! for future reference
ggplot(data_full, aes(x = MS.Zoning, fill=MS.Zoning)) + 
  geom_bar() +
  facet_wrap(~ Neighborhood)

# Total Bsmnt. square feet:

ggplot(data = data_full, aes(x = Total.Bsmt.SF, y =SalePrice, colour=Sale.Condition))+ geom_jitter(size=1)+
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))

# The plot shows strong positive linear relationship between  price the total area of the basement.
# There are a couple of outliers that indicate very big basements, but those will be removed, as they 
# were sold under partial conditions.

#Garage area:

sum(is.na(data_full$Garage.Cars))
ggplot(data = data_full, aes(x =Garage.Area, y =SalePrice,  colour="red", na.rm = TRUE))+ geom_jitter(size=1)+
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))

ggplot(data = data_full, aes(x =reorder(as.factor(Garage.Cars), SalePrice, median) , y =SalePrice,color=as.factor(Garage.Cars), na.rm = TRUE)) +geom_boxplot(outlier.size=0.2)+ 
  scale_y_continuous(name = "Price", labels = function(y) paste0(y / 1000, "k"))+ ggtitle("Number of cars vs Price")+
  scale_x_discrete(name="Number of cars")
cor(data_full$Garage.Area, data_full$SalePrice)

# The plot shows strong positive linear relationship between  price the total area of the garage.


data_full <- data_full %>% filter(Sale.Condition == "Normal" & Gr.Liv.Area < 4001 )
# Drop Sale.Condition column

drops <- c("Sale.Condition")
data_full<-data_full[ , !(names(data_full) %in% drops)] 

##### 5. Missing values. #####

summary(data_full)
sum((is.na(data_full)))

# A few variables in the dataframe have NAs. 

# To get a better picture I am going to visualize  them.


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

# For many predictors the number of NA values is too significant to ignore. The variable with the biggest number of missing values is "Pool.QC"(pool quality) - 2403 NAs:

sum(is.na(data_full$Pool.QC))
table(data_full$Pool.QC)

# As per data description document "NA"	stands for "No Pool". Iowa has a humid continental climate throughout the state with
# average low and high of -11.3	and -1.3	in the month of January; 17.6	and 29.1	in July accordingly particularly in Ames.
# Therefore it is only natural that pool is not the priority because its maintenance is rather costly keeping in mind that there are 
# only 200 sunny days a year on average.


# Appart from pool quality there are 14 other variables where NA stands for no variable.
# I am going to set NA to a level in all of them:

NA_cols<-c("Pool.QC", "Misc.Feature", "Alley", "Fence", "Fireplace.Qu",  "Garage.Yr.Blt","Garage.Type", "Garage.Qual", 
           "Garage.Finish","Garage.Cond", "BsmtFin.Type.1", "BsmtFin.Type.2","Bsmt.Qual",
           "Bsmt.Cond","Bsmt.Exposure")
length(NA_cols)
data_full[NA_cols]<-lapply(data_full[,NA_cols], function(x) addNA(x))

# Before dealing with NAs I am going to replace all blanks with NAs

data_full[data_full==""] <- NA


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

# There are 451 NAs.

sum(is.na(data_full$Lot.Frontage))


# As it doesn´t say that NAs stand for "no frontage" I am going to replace it with its median value:

data_full$Lot.Frontage[is.na(data_full$Lot.Frontage)] <- median(data_full$Lot.Frontage, na.rm = TRUE)

#Basement bathrooms:

base_data[is.na(base_data$Bsmt.Half.Bath & is.na(base_data$Bsmt.Full.Bath)),] 

# There is clearly no basement in the property 1301 therefore I am going to replace them with 0:

data_full$Bsmt.Half.Bath[is.na(data_full$Bsmt.Half.Bath)] <- 0
data_full$Bsmt.Full.Bath[is.na(data_full$Bsmt.Full.Bath)] <- 0

# Regarding the two Masonry veneer related variables the number of NA is negligible (less than 0.5%) so I'll 
# assume that NA are "None" for MasVnrType and 0 for MasVnrArea.

table(data_full$Mas.Vnr.Type)
data_full$Mas.Vnr.Area[is.na(data_full$Mas.Vnr.Area)] <- 0
data_full<-transform(data_full, Mas.Vnr.Type=fct_explicit_na(data_full$Mas.Vnr.Type, "None"))

# There is only 1 NA in "Electrical, which I am going to replace with the most commun level "SBrkr"
# and also drop the "Mix" level since there are no properties with such electrical system:
table(data_full$Electrical)
data_full<-transform(data_full, Electrical=fct_explicit_na(data_full$Electrical, "SBrkr"))

# Identify near zero variance predictors:
# One interesting aspect of this dataset is that it contains many variables and many of these variables have extemely low variances. 
# This means that there is very little information in these variables because they mostly consist of a single value (e.g. zero).

# I am going to use nearZeroVar() for removing such variables to save time during modeling.
# By default, caret uses freqCut = 19 and uniqueCut = 10, which is fairly conservative.

remove_cols <- nearZeroVar(data_full, names = TRUE, 
                           freqCut = 19, uniqueCut = 10)

all_cols<-names(data_full)

data_full <- data_full[ , setdiff(all_cols, remove_cols)]


# Correlation matrix.
# Quality variables:
# Since the overall quality has the widest range of levels I am going to recode the rest of the quality variables accordingly:
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


data_full$Overall.Qual
q<-data_full[c("SalePrice",   "Overall.Qual", "Exter.Qual", "Bsmt.Qual", "Kitchen.Qual", "Fireplace.Qu", "Pool.QC", "Heating.QC")]
q[,3:8] <- ifelse(q[,3:8] == "Ex", 9, ifelse(q[,3:8] == "Gd", 7, ifelse(q[,3:8] == "Fa", 3, ifelse(q[,3:8] == "TA", 5,ifelse(q[,3:8]=="Po", 2,0)))))
ggpairs(q, columns = colnames(q))

# Since exterior quality, basement quality and kitchen quality are very highly correlated (>0.5) with
# with each other and with overall quality, having more than 1 one of them might lead
# to multicollinearity. Therefore I am going to drop exterior quality, basement quality and kitchen quality 
# and use overall quality to represent them all.

# Area variables.
# Lot area
# BsmtFin SF 1 

#The area of the house and the rooms:
ggpairs(data_full, columns = c("SalePrice",  "Gr.Liv.Area", "TotRms.AbvGrd", "Bedroom.AbvGr", 
                               "X1st.Flr.SF", "X2nd.Flr.SF", "Full.Bath", "Half.Bath", "Fireplaces"))
str(data_full)
"Fireplaces" %in% names(data_full)
# TotRms.AbvGrd, Bedroom.AbvGr, X1st.Flr.SF, X2nd.Flr.SF, Full.Bath are highly correlated with Gr.Liv.Area
# and some of them with each other. Therefore I will drop these variables:

# Garage and garage cars
ggpairs(data_full, columns = c( "SalePrice", "Garage.Cars", "Garage.Area")) # 0.887 - Garage.Cars will be dropped


#Basement and its rooms and square feet and bathrooms:
ggpairs(data_full, columns = c("SalePrice",  "Total.Bsmt.SF", "BsmtFin.SF.1", "BsmtFin.SF.2" , "Bsmt.Unf.SF", 
                               
                               "Bsmt.Full.Bath", "Bsmt.Half.Bath"))

# Total Basement square feet is very correlated with BsmtFin.Sf.1
# Therefore I am going to drop it.


# Final Modifications & drops:


drops_f <- c("Exter.Qual", "Bsmt.Qual", "Kitchen.Qual", "TotRms.AbvGrd", "Bedroom.AbvGr", "X1st.Flr.SF", "X2nd.Flr.SF", "Full.Bath",
             "Garage.Cars","BsmtFin.SF.1", "Sale.Condition" )
data_full<-data_full[ , !(names(data_full) %in% drops_f)]
str(data_full)
# to factor:
glimpse(data_full)
data_full$Overall.Qual=as.factor(data_full$Overall.Qual)
data_full$Overall.Cond=as.factor(data_full$Overall.Cond)

# MS.SubClass identifies the type of dwelling involved in the sale, e.g. "020" stands for	"1-STORY 1946 & NEWER ALL STYLES",
# and therefore should be converted into factor
data_full$MS.SubClass=as.factor(data_full$MS.SubClass)
# Log transformation:

data_full$SalePrice<-log(data_full$SalePrice)
data_full$Lot.Area<-log(data_full$Lot.Area) 
data_full$Garage.Yr.Blt<-as.numeric(data_full$Garage.Yr.Blt)

# Identifying unused factor levels

unsd_levels<- function(dataset){
  factor_var <- sapply(dataset, is.factor)
  factor_df <-dataset[, factor_var]
  factors<-c()
  for (column in names(factor_df)){
    
    levels_ = table(factor_df[[column]])
    levels_df = as.data.frame(levels_)
    
    if (min(levels_df$Freq)==0){
      factors<-append(factors,column,after = length(factors)) 
      
    }
    
    
    
    
  }
  return(factors)
}

unsd_levels(data_full)


table(data_full$Exterior.1st)
table(data_full$Exterior.2nd)
table(data_full$Mas.Vnr.Type)
table(data_full$Bsmt.Exposure)
table(data_full$BsmtFin.Type.1)
table(data_full$Electrical)
table(data_full$Garage.Finish)
table(data_full$Misc.Feature)
table(data_full$Misc.Feature)

# drops the levels that do not occur:
data_full[unsd_levels(data_full)]<-lapply(data_full[,unsd_levels(data_full)], function(x) droplevels(x))    # drops the levels that do not occur

# Modeling 


# Forward selection:
b<-lm(log(SalePrice) ~., data = data_full[,!(names(data_full) %in% c("Order", "PID", "Yr.Sold", "Mo.Sold"))])
formula(b)
frst= lm(SalePrice~1, data=data_full[,!(names(data_full) %in% c("Order", "PID", "Yr.Sold", "Mo.Sold"))])
step(frst, direction="forward", scope=formula(b))
frwd<-lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + Neighborhood + MS.SubClass + 
           BsmtFin.Type.1 + Overall.Cond + Age + Lot.Area + Total.Bsmt.SF + 
           Bsmt.Unf.SF + Garage.Area + Conditions + Fireplace.Qu + Exterior.1st + 
           MS.Zoning + Heating.QC + Bsmt.Exposure + Remode + Central.Air + 
           Foundation + Lot.Config + Half.Bath + Bsmt.Full.Bath + Mas.Vnr.Area + 
           House.Style + Paved.Drive + Misc.Feature + BsmtFin.SF.2 + 
           Lot.Frontage + Fireplaces + Garage.Type + Garage.Yr.Blt, data = data_full)

AIC(frwd)
# Bacward selection:
FITALL=lm(SalePrice~.,data = data_full[,!(names(data_full) %in% c("Order", "PID", "Yr.Sold", "Mo.Sold"))])
summary(FITALL)
step(FITALL, direction= "backward")
bkwrd<-lm(SalePrice~MS.SubClass + MS.Zoning + Lot.Frontage + Lot.Area + 
            Lot.Config + Neighborhood + House.Style + Overall.Qual + 
            Overall.Cond + Exterior.1st + Mas.Vnr.Area + Foundation + 
            Bsmt.Exposure + BsmtFin.Type.1 + BsmtFin.SF.2 + Bsmt.Unf.SF + 
            Total.Bsmt.SF + Heating.QC + Central.Air + Gr.Liv.Area + 
            Bsmt.Full.Bath + Half.Bath + Fireplaces + Fireplace.Qu + 
            Garage.Type + Garage.Yr.Blt + Garage.Area + Paved.Drive + 
            Misc.Feature + Conditions + Age + Remode, data = data_full)

AIC(bkwrd)

# Combining both:
FitStart<-lm(SalePrice~1, data=data_full[,!(names(data_full) %in% c("Order", "PID", "Yr.Sold", "Mo.Sold"))])
step(FitStart, direction = "both", scope=formula(b))
both<-lm(formula = SalePrice ~ Overall.Qual + Gr.Liv.Area + Neighborhood + 
           MS.SubClass + BsmtFin.Type.1 + Overall.Cond + Age + Lot.Area + 
           Total.Bsmt.SF + Bsmt.Unf.SF + Garage.Area + Conditions + 
           Fireplace.Qu + Exterior.1st + MS.Zoning + Heating.QC + Bsmt.Exposure + 
           Remode + Central.Air + Foundation + Lot.Config + Half.Bath + 
           Bsmt.Full.Bath + Mas.Vnr.Area + House.Style + Paved.Drive + 
           Misc.Feature + BsmtFin.SF.2 + Lot.Frontage + Fireplaces + 
           Garage.Type + Garage.Yr.Blt, data = data_full[, !(names(data_full) %in% 
                                                               c("Order", "PID", "Yr.Sold", "Mo.Sold"))])


AIC(both)
table(data_full$BsmtFin.Type.1)
fin_var<-c("SalePrice", "Overall.Qual","Gr.Liv.Area", "Neighborhood", "MS.SubClass", "BsmtFin.Type.1", "Overall.Cond", "Age", "Lot.Area",
           "Total.Bsmt.SF", "Garage.Area", "Conditions")
new_data<-data_full[ , (names(data_full) %in% fin_var)]
d<-lm(log(SalePrice) ~., data =new_data)
formula(d)
h= lm(SalePrice~1, data=new_data)
step(h, direction="forward", scope=formula(d))
n_frwd<-lm(formula = SalePrice ~ Overall.Qual + Gr.Liv.Area + Neighborhood + 
           MS.SubClass + BsmtFin.Type.1 + Overall.Cond + Age + Lot.Area + 
           Total.Bsmt.SF + Garage.Area + Conditions, data = new_data)

AIC(n_frwd)
