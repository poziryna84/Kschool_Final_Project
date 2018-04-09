##### 1. Loading Dataset. #####

data_full = read.csv("AmesHousing.csv", header=TRUE, sep = ";")

##### 2. Loading packages #####

library(dplyr)          # data manipulation #
library('plyr')         # data manipulation
library('ggplot2')      # library to create plots #   
require("stringi")      # string/text processing
library('statsr')       # staistics functions
library(forcats)        # reordering and modifying factor levels
install.packages("naniar")
library("naniar")       # replace specified values with an NA
library(GGally)


##### 3. Understanding the structure of the data. Variable type modifications. #####

dim(data_full)

# The data frame has 2930 rows and 82 columns.

# Look at the column names:

names(data_full)

#The summary of its internal structue (for more details: https://github.com/poziryna84/Kschool_Final_Project/blob/master/DataSet_Description.RMD):

glimpse(data_full)

# MS.SubClass identifies the type of dwelling involved in the sale, e.g. "020" stands for	"1-STORY 1946 & NEWER ALL STYLES",
# and therefore should be converted into factor

data_full$MS.SubClass=as.factor(data_full$MS.SubClass)

# As our response variable is SalePrice it´s a good idea to take a look at its distribution and shape to decide
# on the most appropriate measures of center and spread; whether or not any varibly transformation is needed; and
# outliers strategy. 

summary(data_full$SalePrice)

# With the minimum price of $ 12789 and the maximum of $ 755 000 the mean of $ 180 796 is slightly higher than the median $ 160 000,
# which indicates the skewness of the distribution:

ggplot(data_full, aes(x=SalePrice,y = ..density.., colour="red" )) +geom_density(size = 1, colour = 'brown')+
  geom_histogram(bins = 30, fill = 'pink', colour = 'brown') +  scale_x_continuous("Sales Price in thousands",labels=function(x)x/1000)+
  labs(title = "Histogram - Prices of houses", x = 'Age', y = "Frequency")+
  geom_vline(data = data_full, mapping = aes( xintercept = mean(data_full$SalePrice), colour = 'mean'), size = 1.5)+
  geom_vline(data = data_full,mapping = aes( xintercept = median(data_full$SalePrice), colour = 'median'), size = 1.5)+
  geom_vline(data = data_full,mapping = aes( xintercept = mode(data_full$SalePrice), colour = 'mode'), size = 1.5)

# It is a unimodal right-skewed distribution. To make it easier to model I will log transform it and use its median and 
# and interquartile range as measures of center and variability for the further EDA  as they are robust to the outliers.

data_full$SalePrice<-log(data_full$SalePrice) # !!!!! Later
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

# Condition.1 (proximity to various conditions) and 2 (proximity to various conditions if more than one is present).
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

# Visualizing the relationship between proximity to various conditions with price:

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
# To simplify the analysis, these levels will be dropped:
data_full <- data_full %>%
  filter(MS.Zoning != "A (agr)" & MS.Zoning != "I (all)") %>%
  droplevels()

# MS SubClass identifies the type of dwelling involved in the sale. 

table(data_full$MS.SubClass)
ggplot(data_full, aes(MS.SubClass, fill=MS.SubClass))+geom_histogram(stat="count")+ggtitle("Zonings count")+xlab("Zonings")+theme_bw()

# The histogram and the table show that the majority of the houses (1077) falls into class "020"	(1-STORY 1946 & NEWER ALL STYLES) 
# and "060"	2-STORY 1946 & NEWER while there is only 1 property
# that falls into class 150	(1-1/2 STORY PUD - ALL AGES), therefore I am going to drop it as well:

data_full <- data_full %>%
  filter(MS.SubClass != "150") %>%
  droplevels()

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

ggplot(data_full, aes(as.factor(Overall.Cond), fill=as.factor(Overall.Cond)))+geom_histogram(stat="count")+ggtitle("Zonings count")+xlab("Zonings")+theme_bw()

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

data_full <- data_full %>% filter(Sale.Condition == "Normal" & Gr.Liv.Area < 4001 ) # !!! Later

# Drop Sale.Condition column

drops <- c("Sale.Condition")
data_full<-data_full[ , !(names(data_full) %in% drops)] # Later!!!

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

data_full$Lot.Area<-log(data_full$Lot.Area)

ggplot(data = data_full, aes(x = Lot.Area, y =log(SalePrice), colour=Land.Slope))+ geom_jitter(size=1)+
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


ggplot(data = data_full, aes(x = Age, y =SalePrice, colour=Overall.Cond))+ geom_jitter(size=1)+
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))


# The scatter plot shows somewhat negative moderate to strong linear relationship between age and price. There are some outliers
# but they are mostly due to the overall condition of the properties. We can conclude that even really
# old houses can be sold at high prices if the condition is above 7.5 and vice versa.

#Remode:

ggplot(data = data_full, aes(x = Year.Remod.Add, y =SalePrice, colour=Overall.Cond))+ geom_jitter(size=1)+
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))

sum(is.na(data_full$Total.Bsmt.SF))
cor(data_full$Year.Remod.Add, data_full$SalePrice)

# Facet wrap !!! for future reference
ggplot(data_full, aes(x = MS.Zoning, fill=MS.Zoning)) + 
  geom_bar() +
  facet_wrap(~ Neighborhood)

# Total Bsmnt. square feet:

ggplot(data = data_full, aes(x = Total.Bsmt.SF, y =SalePrice, colour="red"))+ geom_jitter(size=1)+
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))
cor(data_full$Total.Bsmt.SF, data_full$SalePrice)
# The plot shows strong positive linear relationship between  price the total area of the basement.

#Garage area:

sum(is.na(data_full$Garage.Area))
ggplot(data = data_full, aes(x = Garage.Area, y =SalePrice, colour=Overall.Cond))+ geom_jitter(size=1)+
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))
cor(data_full$Garage.Area, data_full$SalePrice)

# The plot shows strong positive linear relationship between  price the total area of the garage.


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

# For many predictors the number of NA values is too significant to ignore and we should keep in mind that in some cases NAs 
# stand for no variable. More detailed analysis of missing data is needed.

# Pool.QC.

# The variable with the highest number of missing values is "Pool.QC"(pool quality) - 2403 NAs:

sum(is.na(data_full$Pool.QC))
table(data_full$Pool.QC)

# As per data description document "NA"	stands for "No Pool". Iowa has a humid continental climate throughout the state with
# average low and high of -11.3	and -1.3	in the month of January; 17.6	and 29.1	in July accordingly particularly in Ames.
# Therefore it is only natural that pool is not the priority because its maintenance is rather costly keeping in mind that there are 
# only 200 sunny days a year on average.


# As NA stands for no pool, I am going to change them for "None" level:

data_full<-transform(data_full, Pool.QC=fct_explicit_na(Pool.QC, "None"))
str(data_full$Pool.QC)
data_full$Pool.QC<-recode(data_full$Pool.QC, Ex = "9",Gd="7", Fa="3", TA="5", Po="2", None="0")
data_full$Pool.QC<-as.numeric(levels(data_full$Pool.QC))[data_full$Pool.QC]

# "Miscellaneous feature".

# With 2316 NAs out of 2412. These are the propreties with some features not covered in other 
# categories such as, elevator, 2nd garage, tennins court, shed (a simple roofed structure used for garden storage, to 
# shelter animals, or as a workshop), other.

sum(is.na(data_full$Misc.Feature))

# As was mentioned above  NA stands for no "Miscellaneous feature". That´s why I am going to change NAs for "None" and set to a level:

data_full<-transform(data_full, Misc.Feature=fct_explicit_na( Misc.Feature, "None"))


# "Alley" - type of alley access to property(Grvl./Pave).

table(data_full$Alley)
sum(is.na(data_full$Alley))

# With 2258 out of  2412 being NAs. NA stands for "No alley access",therefore I am going to turn it into "None" and set it to a level.

data_full<-transform(data_full, Alley=fct_explicit_na( Alley, "None"))

# "Fense"(fence quality). 

sum(is.na(data_full$Fence))


# With  1910 being NAs. NAs stand for "no_fence", therefore I am going to turn it into "None" and set it to a level.


data_full<-transform(data_full, Fence=fct_explicit_na( Fence, "None"))

# The next variable with high number of NAs is "Fireplace.Qu"(fireplace quality).

sum(is.na(data_full$Fireplace.Qu))

# With 1164 NAs that stand for "no_fireplace". 
# Before doing anything let´s see if the number of properties
# with 0 or no fireplace coinsides with the number of NAs in Fireplace.Qu, using Fireplaces which stands for number of fireplaces.

sum(is.na(data_full$Fireplace.Qu)) == length(which(data_full$Fireplaces== 0)) # they are the same;
table(data_full$Fireplaces)
str(data_full$Fireplace.Qu)

# I am going to turn it into "None" and set it to a level.

data_full<-transform(data_full, Fireplace.Qu=fct_explicit_na( Fireplace.Qu, "0"))
data_full$Fireplace.Qu<-recode(data_full$Fireplace.Qu, Ex = "9",Gd="7", Fa="3", TA="5", Po="2")
data_full$Fireplace.Qu<-as.numeric(levels(data_full$Fireplace.Qu))[data_full$Fireplace.Qu]
table(data_full$Fireplace.Qu)

# LotFrontage Lot frontage means the portion of a lot that abuts a public or private street.

# There are 451 NAs.

sum(is.na(data_full$Lot.Frontage))


# As it doesn´t say that NAs stand for "no frontage" I am going to replace it with its median value:

data_full$Lot.Frontage[is.na(data_full$Lot.Frontage)] <- median(data_full$Lot.Frontage, na.rm = TRUE)

# Let´s take a look at the 5 "garage" group variables: Garage.Yr.Blt, Garage.Type, Garage.Qual,
# Garage.Finish, Garage.Cond, Garage.Area, Garage.Cars. Almost all of them have different number of NAs.

sum(is.na(data_full$Garage.Yr.Blt)) # with 117 NAs
sum(is.na(data_full$Garage.Type))   # with 116 NAs  NA	stands for "No Garage"
sum(is.na(data_full$Garage.Qual))   # with 117 NAs  NA	stands for "No Garage"
sum(is.na(data_full$Garage.Finish)) # with 116 NAs  NA	stands for "No Garage"
sum(is.na(data_full$Garage.Cond))   # with 117 NAs  NA	stands for "No Garage"

# To see how they all might relate I am going to create a subset of the dataset with the 7 "garage"
# variables and the PID and order:

gar_col<-c("Order", "PID", "Garage.Yr.Blt", "Garage.Type", "Garage.Qual", "Garage.Finish", "Garage.Cond",
                      "Garage.Area", "Garage.Cars")
gar_data<-data_full[gar_col]

gar_data[!is.na(gar_data$Garage.Type) & is.na(gar_data$Garage.Qual),]
glimpse(data_full)
# Property 1357 has a detached garage with area of 360  sq. feet  for 1 car. Even though NAs in Garage.Cond and 

# Garage.Qual stand for "no garage" I doubt this is the case. Therefore I am going replace 116 NAs with "None" for Garage.Type,
# Garage.Qual, Garage.Finish, Garage.Cond and Garage.Yr.Blt only if all five of them are NAs (which stands for "no ragage").


data_full<-transform(data_full, Garage.Qual=fct_explicit_na(Garage.Qual, "0"))
data_full<-transform(data_full, Garage.Cond=fct_explicit_na(Garage.Cond, "0"))
data_full$Garage.Yr.Blt[is.na(data_full$Garage.Yr.Blt)] <- 0
data_full<-transform(data_full, Garage.Finish =fct_explicit_na(Garage.Finish , "None"))
data_full<-transform(data_full, Garage.Type =fct_explicit_na(Garage.Type , "None"))

# The last group of variables of interest with NAs are the 11 basement varibals:

sum(is.na(data_full$BsmtFin.Type.1))    # with 67 NAs - Rating of basement finished area; with NA for "No Basement"

sum(is.na(data_full$BsmtFin.Type.2))    # with 67 NAs - Rating of basement finished area (if multiple types); with NA for "No Basement"

sum(is.na(data_full$Bsmt.Qual))         # with 67 NAs - evaluates the height of the basement (excellent: 100+ inches, etc.); with NA for "No Basement"
sum(is.na(data_full$Bsmt.Cond))         # with 67 NAs - evaluates the general condition of the basement; with NA for "No Basement"
sum(is.na(data_full$Bsmt.Exposure))     # with 67 NAs - Refers to walkout or garden level walls; with NA for "No Basement"
sum(is.na(data_full$Bsmt.Half.Bath))    # with 1  NAs - Basement half bathrooms
sum(is.na(data_full$Bsmt.Full.Bath))    # with 1  NAs - Basement full bathrooms

# For convenience I am going to create a subset of 11  "basement variables" only.

basement_col<-c("Order", "PID", "BsmtFin.Type.1", "BsmtFin.SF.1", "BsmtFin.Type.2", "BsmtFin.SF.2", "Total.Bsmt.SF",
                               "Bsmt.Unf.SF", "Bsmt.Qual", "Bsmt.Cond", "Bsmt.Exposure", "Bsmt.Full.Bath", "Bsmt.Half.Bath" )
base_data<-data_full[basement_col] 


# Since all the variables where NAs stand for "no basement" have the same number of NAs we can check if they all coincide:

#Bathrooms check:
base_data[is.na(base_data$Bsmt.Half.Bath),] # clearly no basement in the property 1302:
nrow(base_data[is.na(base_data$BsmtFin.Type.1) & is.na(base_data$BsmtFin.Type.2) & is.na(base_data$Bsmt.Qual)
               & is.na(base_data$Bsmt.Cond) & is.na(base_data$Bsmt.Exposure),]) # 67 cases
data_full$Bsmt.Half.Bath[is.na(data_full$Bsmt.Half.Bath)] <- 0
data_full$Bsmt.Full.Bath[is.na(data_full$Bsmt.Full.Bath)] <- 0
# And convert these NAs to "None"

data_full<-transform(data_full, BsmtFin.Type.1=fct_explicit_na(BsmtFin.Type.1, "None"))
data_full<-transform(data_full, BsmtFin.Type.2=fct_explicit_na(BsmtFin.Type.2, "None"))
data_full<-transform(data_full, Bsmt.Qual=fct_explicit_na(Bsmt.Qual, "0"))
data_full<-transform(data_full, Bsmt.Cond=fct_explicit_na(Bsmt.Cond, "0"))
data_full<-transform(data_full, Bsmt.Exposure=fct_explicit_na(Bsmt.Exposure, "None"))

# The last variable with 11 NAs is Mas.Vnr.Area - the  masonry veneer area. It doesn´t say that NAs stand for no veneer area, so we leave it as
# it is for now.

sum(is.na(data_full$Mas.Vnr.Area)) 

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

# Regarding the two Masonry veneer related variables the number of NA is negligible (less than 0.5%) so I'll 
# assume that NA are "None" for MasVnrType and 0 for MasVnrArea.

data_full$Mas.Vnr.Area[is.na(data_full$Mas.Vnr.Area)] <- 0
data_full<-transform(data_full, Mas.Vnr.Type=fct_explicit_na(data_full$Mas.Vnr.Type, "None"))

# Correlation matrix.
# Quality variables:
str(data_full[,grep("Q", names(data_full), value=TRUE)] )

# Exterior quality has five levels:  Ex = "excellent",Gd="good", Fa="fair", TA="average/typical", Po = "poor"
str(data_full$Exter.Qual)
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
data_full$Exter.Qual<-recode(data_full$Exter.Qual, Ex = "9",Gd="7", Fa="3", TA="5", Po="2")
data_full$Exter.Qual<-as.numeric(levels(data_full$Exter.Qual))[data_full$Exter.Qual]
table(data_full$Exter.Qual)
#Basement quality has six levels Ex ="excellent", Gd="good", TA= "typical", Fa = "fair", Po = "poor", 0 ="no basement" :
str(data_full$Bsmt.Qual)
data_full$Bsmt.Qual<-recode(data_full$Bsmt.Qual, Ex = "9",Gd="7", Fa="3", TA="5", Po="2")
data_full$Bsmt.Qual<-as.numeric(levels(data_full$Bsmt.Qual))[data_full$Bsmt.Qual]
table(data_full$Bsmt.Qual)
# Kitchen Quality:
str(data_full$Kitchen.Qual)
data_full$Kitchen.Qual<-recode(data_full$Kitchen.Qual, Ex = "9",Gd="7", Fa="3", TA="5", Po="2")
data_full$Kitchen.Qual<-as.numeric(levels(data_full$Kitchen.Qual))[data_full$Kitchen.Qual]
table(data_full$Kitchen.Qual)
# Garage quality:
str(data_full$Garage.Qual)
data_full$Garage.Qual<-recode(data_full$Garage.Qual,   Ex = "9",Gd="7", Fa="3", TA="5", Po="2")
data_full$Garage.Qual<-as.numeric(levels(data_full$Garage.Qual))[data_full$Garage.Qual]
str(data_full$Overall.Cond)
table(data_full$Garage.Qual)
#Heating
str(data_full$Heating.QC)
data_full$Heating.QC<-recode(data_full$Heating.QC,   Ex = "9",Gd="7", Fa="3", TA="5", Po="2")
data_full$Heating.QC<-as.numeric(levels(data_full$Heating.QC))[data_full$Heating.QC]
table(data_full$Heating.QC)
# Correlation plot:
ggpairs(data_full, columns = c("SalePrice",   "Overall.Qual", "Exter.Qual", "Bsmt.Qual", "Kitchen.Qual","Garage.Qual", "Fireplace.Qu", "Pool.QC", "Heating.QC"))


# Since exterior quality, basement quality and kitchen quality are very highly correlated (>0.5) with
# with each other and with overall quality, having more than 1 one of them might lead us
# to multicollinearity. Therefore I am going to drop exterior quality, basement quality and kitchen quality 
# and use overall quality to represent them all.
drops_q <- c("Exter.Qual", "Bsmt.Qual", "Kitchen.Qual")
data_full<-data_full[ , !(names(data_full) %in% drops_q)]
data_full$Overall.Qual=as.factor(data_full$Overall.Qual)
data_full$Overall.Qual=as.factor(data_full$Heating.QC)
data_full$Overall.Qual=as.factor(data_full$Garage.Qual)
data_full$Overall.Qual=as.factor(data_full$Fireplace.Qu)
data_full$Overall.Qual=as.factor(data_full$Pool.QC)
# Condition variables:
str(data_full[,grep("Cond", names(data_full), value=TRUE)] )
#Overall condition:  rates the overall condition of the house
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
glimpse(data_full)
str(data_full$Exter.Cond)
str(data_full$Overall.Cond)
#Kitchen condition:

data_full$Overall.Cond<-recode(data_full$Overall.Cond, Ex = "5",Gd="4", Fa="3", TA="2", Po="1")
data_full$Exter.Cond<-recode(data_full$Exter.Cond, Ex = "5",Gd="4", Fa="3", TA="2", Po="1")

data_full$Exter.Cond<-as.numeric(levels(data_full$Exter.Cond))[data_full$Exter.Cond]
data_full$Overall.Cond<-as.numeric(levels(data_full$Overall.Cond))[data_full$Overall.Cond]
# Basement condition:
data_full$Bsmt.Cond<-recode(data_full$Bsmt.Cond, Ex = "5",Gd="4", Fa="3", TA="2", Po="1")
data_full$Bsmt.Cond<-as.numeric(levels(data_full$Bsmt.Cond))[data_full$Bsmt.Cond]

# Garage condition:
data_full$Garage.Cond<-recode(data_full$Garage.Cond, Ex = "5",Gd="4", Fa="3", TA="2", Po="1")
data_full$Garage.Cond<-as.numeric(levels(data_full$Garage.Cond))[data_full$Garage.Cond]
ggpairs(data_full, columns = c("SalePrice",   "Overall.Cond", "Exter.Cond", "Bsmt.Cond", "Garage.Cond"))

# Condition variables are not correlated(<0.5) therefore I am leaving them as the are.
data_full$Overall.Cond=as.factor(data_full$Overall.Cond)
# Area variables.
# Lot area
# BsmtFin SF 1 
data_full$Total.Bsmt.SF
#The area of the house and the rooms:
ggpairs(data_full, columns = c("SalePrice","Gr.Liv.Area", "TotRms.AbvGrd", "Bedroom.AbvGr", "Kitchen.AbvGr", 
                               "X1st.Flr.SF", "X2nd.Flr.SF", "Low.Qual.Fin.SF", "Full.Bath", "Half.Bath", "Fireplaces"))

# TotRms.AbvGrd, Bedroom.AbvGr, X1st.Flr.SF, X2nd.Flr.SF, Full.Bath are highly correlated with Gr.Liv.Area
# and some of them with each other. Therefore I am going to drop these variables:
drops_2 <- c("TotRms.AbvGrd", "Bedroom.AbvGr", "X1st.Flr.SF", "X2nd.Flr.SF", "Full.Bath")
data_full<-data_full[ , !(names(data_full) %in% drops_2)]

# Lot Area:
ggpairs(data_full, columns = c("SalePrice", "Lot.Area", "Lot.Frontage", "Open.Porch.SF", "Enclosed.Porch",
                               "X3Ssn.Porch", "Screen.Porch", "Pool.Area", "Garage.Cars", "Gr.Liv.Area")) # no correlations

#Basement and its rooms and square feet:
ggpairs(data_full, columns = c("SalePrice","Total.Bsmt.SF", "BsmtFin.SF.1", "BsmtFin.SF.2" , "Bsmt.Unf.SF", 
                               
                               "Bsmt.Full.Bath", "Bsmt.Half.Bath"))
# Total Basement square feet is very correlated with BsmtFin.Sf.1
# Therefore I am going to drop it.
drops_1 <- c("BsmtFin.SF.1")
data_full<-data_full[ , !(names(data_full) %in% drops_1)]

data_full$Garage.Area
str(data_full$X1st.Flr.SF)
sum(is.na(data_full$Full.Bath))
# TotRmsAbvGrd	(Discrete) -TotRms.AbvGrd
# , "X1st.Flr.SF", "X2nd.Flr.SF", "Low.Qual.Fin.SF", "Bsmt.Full.Bath", "Bsmt.Half.Bath",
# "Full.Bath", "Half.Bath", "Bedroom.AbvGr", "Kitchen.AbvGr"
#Bsmt Full Bath (Discrete): Basement full bathrooms

#Bsmt Half Bath (Discrete): Basement half bathrooms

#Full Bath (Discrete): Full bathrooms above grade
str(data_full)
#Half Bath (Discrete): Half baths above grade

#Bedroom (Discrete): Bedrooms above grade (does NOT include basement bedrooms)

warnings()

#Kitchen (Discrete): Kitchens above grade
