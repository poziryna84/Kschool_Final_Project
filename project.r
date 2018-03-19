##### 1. Loading Dataset #####
               
data_full = read.csv("AmesHousing.csv", header=TRUE, sep = ";")

##### 2. Loading packages #####


library(dplyr)          # data manipulation
library('plyr')         # data manipulation
library('ggplot2')      # library to create plots       
require("stringi")      # string/text processing
library('statsr')       # staistics functions
library(forcats)        # reordering and modifying factor levels
install.packages("naniar")
library("naniar")       # replace specified values with an NA


##### 3. Understanding the structure of the data. Variable type modifications. #####


# To verify we have got a dataframe.

class(data_full)

# View its dimentions:

dim(data_full)

# The data frame has 2930 rows and 82 columns.

# Look at the column names:

names(data_full)

#The summary of its internal structue (for more details: https://github.com/poziryna84/Kschool_Final_Project/blob/master/DataSet_Description.RMD):

glimpse(data_full)

# Overall.Qual and Overall. Cond are ratings and can only take on a finite number of values 
# (i.e. the magnitude of the number does not matter) Therefore I am going to change them to factors.

data_full$Overall.Qual=as.factor(data_full$Overall.Qual)
data_full$Overall.Cond=as.factor(data_full$Overall.Cond)


##### 4. Identifying outliers and filtering the data set. #####

# Creating a plot of SALE PRICE vs GR LIV AREA to identify the outliers:

ggplot(data = data_full, aes(x = Gr.Liv.Area, y =SalePrice, colour=Sale.Condition))+ geom_jitter(size=1)+
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))

# Three of them with the living area bigger than 4500 square feet (418.0636 square meters) sold at low prices of less than $200 000.
# They were also were sold partially that might not represent true market values.

which(data_full$Gr.Liv.Area>4000 & data_full$SalePrice<200000) # 1499 2181 2182

# The other two are just unusual sales with very large houses and quite appropriate prices. One of them was sold under abnormal sale 
# conditions.

which(data_full$Gr.Liv.Area>4000 & data_full$SalePrice>650000) # 1761 1768


# For my further analysis I will remove any houses larger than 4000 square feet from the 
# data set and leave those that were sold under "Normal" sale condition only.

data_full <- data_full %>% filter(Sale.Condition == "Normal" & Gr.Liv.Area < 4001 )

##### 5. Treating NAs. #####

summary(data_full)
sum((is.na(data_full)))

# Quite a few variables in the dataframe have NAs. 

# To get a better picture I am going to visualize  them.

data_full$SalePrice
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

# I am going to turn it into "None" and set it to a level.

data_full<-transform(data_full, Fireplace.Qu=fct_explicit_na( Fireplace.Qu, "None"))

# LotFrontage Lot frontage means the portion of a lot that abuts a public or private street.

# There are 451 NAs.

sum(is.na(data_full$Lot.Frontage))
min(data_full$Lot.Frontage, na.rm = TRUE)

# As the minimum is 21, I presume that the NA means 0 (e.g. no street is connected)
# can be replaced by 0:

data_full$Lot.Frontage[is.na(data_full$Lot.Frontage)] <- 0

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

# Property 1357 has a detached garage with area of 360  sq. feet  for 1 car. Even though NAs in Garage.Cond and 
# Garage.Qual stand for "no garage" I doubt this is the case. Therefore I am going replace 116 NAs with "None" for Garage.Type,
# Garage.Qual, Garage.Finish, Garage.Cond and Garage.Yr.Blt only if all five of them are NAs (which stands for "no ragage").

data_full$Garage.Qual <- ifelse(is.na(data_full$Garage.Type) & is.na(data_full$Garage.Finish), "None", data_full$Garage.Qual)
data_full$Garage.Cond <- ifelse(is.na(data_full$Garage.Type) & is.na(data_full$Garage.Finish), "None", data_full$Garage.Cond)
data_full$Garage.Yr.Blt <- ifelse(is.na(data_full$Garage.Type) & is.na(data_full$Garage.Finish), "None", data_full$Garage.Yr.Blt)
data_full<-transform(data_full, Garage.Finish=fct_explicit_na(Garage.Finish, "None"))
data_full<-transform(data_full, Garage.Type=fct_explicit_na(Garage.Finish, "None"))



# The last group of variables of interest with NAs are the 11 basement varibals:

sum(is.na(data_full$BsmtFin.Type.1))    # with 67 NAs - Rating of basement finished area; with NA for "No Basement"

sum(is.na(data_full$BsmtFin.Type.2))    # with 67 NAs - Rating of basement finished area (if multiple types); with NA for "No Basement"

sum(is.na(data_full$Bsmt.Qual))         # with 67 NAs - evaluates the height of the basement (excellent: 100+ inches, etc.); with NA for "No Basement"
sum(is.na(data_full$Bsmt.Cond))         # with 67 NAs - evaluates the general condition of the basement; with NA for "No Basement"
sum(is.na(data_full$Bsmt.Exposure))     # with 67 NAs - Refers to walkout or garden level walls; with NA for "No Basement"
sum(is.na(data_full$Bsmt.Half.Bath))    # with 1  NAs - Basement half bathrooms
sum(is.na(data_full$Bsmt.Full.Bath))    # with 1  NAs - Basement full bathrooms


#For convenience I am going to create a subset of 11  "basement variables" only.

basement_col<-c("Order", "PID", "BsmtFin.Type.1", "BsmtFin.SF.1", "BsmtFin.Type.2", "BsmtFin.SF.2", "Total.Bsmt.SF",
                "Bsmt.Unf.SF", "Bsmt.Qual", "Bsmt.Cond", "Bsmt.Exposure", "Bsmt.Full.Bath", "Bsmt.Half.Bath" )
base_data<-data_full[basement_col] 

# Since all the variables where NAs stand for "no basement" have the same number of NAs we can check if they all coincide:

nrow(base_data[is.na(base_data$BsmtFin.Type.1) & is.na(base_data$BsmtFin.Type.2) & is.na(base_data$Bsmt.Qual)
               & is.na(base_data$Bsmt.Cond) & is.na(base_data$Bsmt.Exposure),]) # 67 cases

# And convert these NAs to "None"

data_full<-transform(data_full, BsmtFin.Type.1=fct_explicit_na(BsmtFin.Type.1, "None"))
data_full<-transform(data_full, BsmtFin.Type.2=fct_explicit_na(BsmtFin.Type.2, "None"))
data_full<-transform(data_full, Bsmt.Qual=fct_explicit_na(Bsmt.Qual, "None"))
data_full<-transform(data_full, Bsmt.Cond=fct_explicit_na(Bsmt.Cond, "None"))
data_full<-transform(data_full, Bsmt.Exposure=fct_explicit_na(Bsmt.Exposure, "None"))


# The last variable with 11 NAs is Mas.Vnr.Area - the  masonry veneer area. It doesn´t say that NAs stand for no veneer area, so we leave it as
# it is for now

sum(is.na(data_full$Mas.Vnr.Area)) 


# Before dealing with NAs I am going to replace all blanks with NAs

data_full[data_full==""] <- NA


data_full$SalePrice
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

