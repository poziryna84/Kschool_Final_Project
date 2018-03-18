# Download data                

data_full = read.csv("AmesHousing.csv", header=TRUE, sep = ";")

require("stringi")
library(dplyr)
library('plyr')         # data manipulation
library('ggplot2')      # library to create plots        # data manipulation
library('knitr')        # required to apply knitr options 
library('statsr') 
library("weights")
library(forcats)
library("naniar")# replace specified values with an NA
install.packages("weights")
install.packages("naniar")

#Understanding the structure of the data:
#To verify we have got a dataframe.
class(data_full)
#View its dimentions:
dim(data_full)

#The data frame has 2930 rows and 82 columns.

#Look at the column names:
names(data_full)

#The summary of its internal structue:
glimpse(data_full)
# or
sapply(data_full, class)

# Overall.Qual and Overall. Cond are ratings and can only take on a finite number of values (i.e. the magnitude of the number does not matter) Therefore I am going to change them to factors.
data_full$Overall.Qual=as.factor(data_full$Overall.Qual)
data_full$Overall.Cond=as.factor(data_full$Overall.Qual)
# Treating year as a categorical variable will calculate effect of each indivudal year -
# i.e. what impact on the target variable was in average in a given year. Therefore I am going to change all the "year variables" to fatcors.  
data_full$Garage.Yr.Blt <- as.factor(data_full$Garage.Yr.Blt)
data_full$Year.Built <- as.factor(data_full$Year.Built)
data_full$Year.Remod.Add <- as.factor(data_full$Year.Remod.Add)
data_full$Yr.Sold <- as.factor(data_full$Yr.Sold)

#Removing any variables that required special knowledge or previous calculations for their 
#use most of which are  related to weighting and adjustment factors used in the city's 
#current modeling system.  
#Only 80 variables remained and they are directly related to property sales. 
#Although too vast to describe here individually (see the documentation file http://www.amstat.org/publications/jse/v19n3/decock/DataDocumentation.txt),
#The 80 variables focus on the quality and quantity of many physical attributes of the property. 
#Most of the variables are exactly the type of information that a typical home buyer would want to know about
#a potential property (e.g. When was it built? How big is the lot? How many squarefeet of 
#living space is in the dwelling? Is the basement finished? How many bathrooms are there?). 

#The variables selected:

# 1. "PID"-(Nominal): Parcel identification number  - can be used with city web site for parcel review.

# 2. "MS.Zoning"- (Nominal): Identifies the general zoning classification of the sale.
#       A	Agriculture
#       C	Commercial
#       FV	Floating Village Residential
#       I	Industrial
#       RH	Residential High Density
#       RL	Residential Low Density
#       RP	Residential Low Density Park 
#       RM	Residential Medium Density

# 3. "Gr.Liv.Area" - (Continuous): Above grade (ground) living area square feet

# 4. "MS.SubClass" (Nominal): Identifies the type of dwelling involved in the sale.
# 020	1-STORY 1946 & NEWER ALL STYLES
# 030	1-STORY 1945 & OLDER
# 040	1-STORY W/FINISHED ATTIC ALL AGES
# 045	1-1/2 STORY - UNFINISHED ALL AGES
# 050	1-1/2 STORY FINISHED ALL AGES
# 060	2-STORY 1946 & NEWER
# 070	2-STORY 1945 & OLDER
# 075	2-1/2 STORY ALL AGES
# 080	SPLIT OR MULTI-LEVEL
# 085	SPLIT FOYER
# 090	DUPLEX - ALL STYLES AND AGES
# 120	1-STORY PUD (Planned Unit Development) - 1946 & NEWER
# 150	1-1/2 STORY PUD - ALL AGES
# 160	2-STORY PUD - 1946 & NEWER
# 180	PUD - MULTILEVEL - INCL SPLIT LEV/FOYER
# 190	2 FAMILY CONVERSION - ALL STYLES AND AGES

#  5. "SalePrice" - (Continuous): Sale price $$

#  6. "Lot.Frontage" (Continuous): Linear feet of street connected to property
#  7. "Lot.Area" - (Continuous): Lot size in square feet  
#  8. "Street" - (Nominal): Type of road access to property
#        Grvl	Gravel	
#        Pave	Paved
#  9. "Alley" - (Nominal): Type of alley access to property
#  Grvl	Gravel
# Pave	Paved
# NA 	No alley access

# 10. "Lot.Shape" - (Ordinal): General shape of property

# Reg	Regular	
# IR1	Slightly irregular
# IR2	Moderately Irregular
# IR3	Irregular

# 11."Land.Contour" -(Nominal): Flatness of the property

# Lvl	Near Flat/Level	
# Bnk	Banked - Quick and significant rise from street grade to building
# HLS	Hillside - Significant slope from side to side
# Low	Depression

# 12. "Utilities" - (Ordinal): Type of utilities available

# AllPub	All public Utilities (E,G,W,& S)	
# NoSewr	Electricity, Gas, and Water (Septic Tank)
# NoSeWa	Electricity and Gas Only
# ELO	Electricity only

# 13. "Lot.Config" - (Nominal): Lot configuration

# Inside	Inside lot
# Corner	Corner lot
# CulDSac	Cul-de-sac
# FR2	Frontage on 2 sides of property
# FR3	Frontage on 3 sides of property

# 14. "Land.Slope" (Ordinal): Slope of property

# Gtl	Gentle slope
# Mod	Moderate Slope	
# Sev	Severe Slope

# 15. "Neighborhood" -(Nominal): Physical locations within Ames city limits (map available)

# Blmngtn	Bloomington Heights
# Blueste	Bluestem
# BrDale	Briardale
# BrkSide	Brookside
# ClearCr	Clear Creek
# CollgCr	College Creek
# Crawfor	Crawford
# Edwards	Edwards
# Gilbert	Gilbert
# Greens	Greens
# GrnHill	Green Hills
# IDOTRR	Iowa DOT and Rail Road
# Landmrk	Landmark
# MeadowV	Meadow Village
# Mitchel	Mitchell
# Names	North Ames
# NoRidge	Northridge
# NPkVill	Northpark Villa
# NridgHt	Northridge Heights
# NWAmes	Northwest Ames
# OldTown	Old Town
# SWISU	South & West of Iowa State University
# Sawyer	Sawyer
# SawyerW	Sawyer West
# Somerst	Somerset
# StoneBr	Stone Brook
# Timber	Timberland
# Veenker	Veenker

# 16. "Condition.1" - (Nominal): Proximity to various conditions

# Artery	Adjacent to arterial street
# Feedr	Adjacent to feeder street	
# Norm	Normal	
# RRNn	Within 200' of North-South Railroad
# RRAn	Adjacent to North-South Railroad
# PosN	Near positive off-site feature--park, greenbelt, etc.
# PosA	Adjacent to postive off-site feature
# RRNe	Within 200' of East-West Railroad
# RRAe	Adjacent to East-West Railroad

# 17. "Condition.2" - (Nominal): Proximity to various conditions (if more than one is present)

# Artery	Adjacent to arterial street
# Feedr	Adjacent to feeder street	
# Norm	Normal	
# RRNn	Within 200' of North-South Railroad
# RRAn	Adjacent to North-South Railroad
# PosN	Near positive off-site feature--park, greenbelt, etc.
# PosA	Adjacent to postive off-site feature
# RRNe	Within 200' of East-West Railroad
# RRAe	Adjacent to East-West Railroad

# 18. "Bldg.Type" - (Nominal): Type of dwelling

# 1Fam	Single-family Detached	
# 2FmCon	Two-family Conversion; originally built as one-family dwelling
# Duplx	Duplex
# TwnhsE	Townhouse End Unit
# TwnhsI	Townhouse Inside Unit

# 19. "House.Style" - (Nominal): Style of dwelling

# 1Story	One story
# 1.5Fin	One and one-half story: 2nd level finished
# 1.5Unf	One and one-half story: 2nd level unfinished
# 2Story	Two story
# 2.5Fin	Two and one-half story: 2nd level finished
# 2.5Unf	Two and one-half story: 2nd level unfinished
# SFoyer	Split Foyer
# SLvl	Split Level

# 20. "Overall.Qual" - (Ordinal): Rates the overall material and finish of the house

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

# 21. "Overall.Cond" - (Ordinal): Rates the overall condition of the house

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

# 22. "Year.Built" - (Discrete): Original construction date

# 23. "Year.Remod.Add" - (Discrete): Remodel date (same as construction date if no remodeling or additions)

# 24. "Roof.Style"-(Nominal): Type of roof

# Flat	Flat
# Gable	Gable
# Gambrel	Gabrel (Barn)
# Hip	Hip
# Mansard	Mansard
# Shed	Shed

# 25. "Roof.Matl" - ClyTile	Clay or Tile

# CompShg	Standard (Composite) Shingle
# Membran	Membrane
# Metal	Metal
# Roll	Roll
# Tar&Grv	Gravel & Tar
# WdShake	Wood Shakes
# WdShngl	Wood Shingles
# data_select<-data_full["PID"]

# 26. "Exterior.1st" - (Nominal): Exterior covering on house

# AsbShng	Asbestos Shingles
# AsphShn	Asphalt Shingles
# BrkComm	Brick Common
# BrkFace	Brick Face
# CBlock	Cinder Block
# CemntBd	Cement Board
# HdBoard	Hard Board
# ImStucc	Imitation Stucco
# MetalSd	Metal Siding
# Other	Other
# Plywood	Plywood
# PreCast	PreCast	
# Stone	Stone
# Stucco	Stucco
# VinylSd	Vinyl Siding
# Wd Sdng	Wood Siding
# WdShing	Wood Shingles

# 27. "Exterior.2nd" - (Nominal): Exterior covering on house (if more than one material)

# AsbShng	Asbestos Shingles
# AsphShn	Asphalt Shingles
# BrkComm	Brick Common
# BrkFace	Brick Face
# CBlock	Cinder Block
# CemntBd	Cement Board
# HdBoard	Hard Board
# ImStucc	Imitation Stucco
# MetalSd	Metal Siding
# Other	Other
# Plywood	Plywood
# PreCast	PreCast
# Stone	Stone
# Stucco	Stucco
# VinylSd	Vinyl Siding
# Wd Sdng	Wood Siding
# WdShing	Wood Shingles

# 28. "Mas.Vnr.Type" -  (Nominal): Masonry veneer type

# BrkCmn	Brick Common
# BrkFace	Brick Face
# CBlock	Cinder Block
# None	None
# Stone	Stone

# 29. "Mas.Vnr.Area" - (Continuous): Masonry veneer area in square feet

# 30. "Exter.Qual" - (Ordinal): Evaluates the quality of the material on the exterior 

# Ex	Excellent
# Gd	Good
# TA	Average/Typical
# Fa	Fair
# Po	Poor

# 31. "Exter.Cond" - (Ordinal): Evaluates the present condition of the material on the exterior

# Ex	Excellent
# Gd	Good
# TA	Average/Typical
# Fa	Fair
# Po	Poor

# 32. "Foundation" - (Nominal): Type of foundation

# BrkTil	Brick & Tile
# CBlock	Cinder Block
# PConc	Poured Contrete	
# Slab	Slab
# Stone	Stone
# Wood	Wood

# 33. "Bsmt.Qual" -(Ordinal): Evaluates the height of the basement

# Ex	Excellent (100+ inches)	
# Gd	Good (90-99 inches)
# TA	Typical (80-89 inches)
# Fa	Fair (70-79 inches)
# Po	Poor (<70 inches
# NA	No Basement

# 34. "Bsmt.Cond" - (Ordinal): Evaluates the general condition of the basement

# Ex	Excellent
# Gd	Good
# TA	Typical - slight dampness allowed
# Fa	Fair - dampness or some cracking or settling
# Po	Poor - Severe cracking, settling, or wetness
# NA	No Basement

# 35. "Bsmt.Exposure" - (Ordinal): Refers to walkout or garden level walls

# Gd	Good Exposure
# Av	Average Exposure (split levels or foyers typically score average or above)	
# Mn	Mimimum Exposure
# No	No Exposure
# NA	No Basement

# 36. "BsmtFin.Type.1" - (Ordinal): Rating of basement finished area

# GLQ	Good Living Quarters
# ALQ	Average Living Quarters
# BLQ	Below Average Living Quarters	
# Rec	Average Rec Room
# LwQ	Low Quality
# Unf	Unfinshed
# NA	No Basement

# 37. "BsmtFin.SF.1" - (Continuous): Type 1 finished square feet

# 38. "BsmtFin.Type.2" - (Ordinal): Rating of basement finished area (if multiple types)

# GLQ	Good Living Quarters
# ALQ	Average Living Quarters
# BLQ	Below Average Living Quarters	
# Rec	Average Rec Room
# LwQ	Low Quality
# Unf	Unfinshed
# NA	No Basement

# 39. "BsmtFin.SF.2" - (Continuous): Type 2 finished square feet

# 40. "Bsmt.Unf.SF" - (Continuous): Unfinished square feet of basement area

# 41. "Total.Bsmt.SF" - (Continuous): Total square feet of basement area

# 42. "Heating" - (Nominal): Type of heating

# Floor	Floor Furnace
# GasA	Gas forced warm air furnace
# GasW	Gas hot water or steam heat
# Grav	Gravity furnace	
# OthW	Hot water or steam heat other than gas
# Wall	Wall furnace

# 43. "Heating.QC" - (Ordinal): Heating quality and condition

# Ex	Excellent
# Gd	Good
# TA	Average/Typical
# Fa	Fair
# Po	Poor

# 44. "Central.Air" - (Nominal): Central air conditioning

# N	No
# Y	Yes

# 45. "Electrical" - (Ordinal): Electrical system

# SBrkr	Standard Circuit Breakers & Romex
# FuseA	Fuse Box over 60 AMP and all Romex wiring (Average)	
# FuseF	60 AMP Fuse Box and mostly Romex wiring (Fair)
# FuseP	60 AMP Fuse Box and mostly knob & tube wiring (poor)
# Mix	Mixed

# 46. "X1st.Flr.SF" - (Continuous): First Floor square feet

# 47. "X2nd.Flr.SF" - (Continuous)	: Second floor square feet

# 48. "Low.Qual.Fin.SF" - (Continuous): Low quality finished square feet (all floors)

# 49. "Bsmt.Full.Bath" - (Discrete): Basement full bathrooms

# 50. "Bsmt.Half.Bath" - (Discrete): Basement half bathrooms

# 51. "Full.Bath" - (Discrete): Full bathrooms above grade

# 52. "Half.Bath" - (Discrete): Half baths above grade

# 53. "Bedroom.AbvGr" - (Discrete): Bedrooms above grade (does NOT include basement bedrooms)

# 54. "Kitchen.AbvGr" - (Discrete): Kitchens above grade

# 55. "Kitchen.Qual" - (Ordinal): Kitchen quality

# Ex	Excellent
# Gd	Good
# TA	Typical/Average
# Fa	Fair
# Po	Poor

# 56. "TotRms.AbvGrd" - (Discrete): Total rooms above grade (does not include bathrooms)

# 57. "Functional" - (Ordinal): Home functionality (Assume typical unless deductions are warranted)

# Typ	Typical Functionality
# Min1	Minor Deductions 1
# Min2	Minor Deductions 2
# Mod	Moderate Deductions
# Maj1	Major Deductions 1
# Maj2	Major Deductions 2
# Sev	Severely Damaged
# Sal	Salvage only

# 58. "Fireplaces" - (Discrete): Number of fireplaces

# 59. "Fireplace.Qu" -  (Ordinal): Fireplace quality

# Ex	Excellent - Exceptional Masonry Fireplace
# Gd	Good - Masonry Fireplace in main level
# TA	Average - Prefabricated Fireplace in main living area or Masonry Fireplace in basement
# Fa	Fair - Prefabricated Fireplace in basement
# Po	Poor - Ben Franklin Stove
# NA	No Fireplace 

# 60. "Garage.Type" - (Nominal): Garage location

# 2Types	More than one type of garage
# Attchd	Attached to home
# Basment	Basement Garage
# BuiltIn	Built-In (Garage part of house - typically has room above garage)
# CarPort	Car Port
# Detchd	Detached from home
# NA	No Garage

# 61. "Garage.Yr.Blt" - (Discrete): Year garage was built

# 62. "Garage.Finish" - (Ordinal)	: Interior finish of the garage

# Fin	Finished
# RFn	Rough Finished	
# Unf	Unfinished
# NA	No Garage

# 63. "Garage.Cars" - (Discrete): Size of garage in car capacity

# 64. "Garage.Area" - (Continuous): Size of garage in square feet

# 65. "Garage.Qual" - (Ordinal): Garage quality

# Ex	Excellent
# Gd	Good
# TA	Typical/Average
# Fa	Fair
# Po	Poor
# NA	No Garage

# 66. "Garage.Cond" - (Ordinal): Garage condition

# Ex	Excellent
# Gd	Good
# TA	Typical/Average
# Fa	Fair
# Po	Poor
# NA	No Garage

# 67. "Paved.Drive" - (Ordinal): Paved driveway

# Y	Paved 
# P	Partial Pavement
# N	Dirt/Gravel

# 68. "Wood.Deck.SF" -  (Continuous): Wood deck area in square feet

# 69. "Open.Porch.SF" - (Continuous): Open porch area in square feet

# 70. "Enclosed.Porch" - (Continuous): Enclosed porch area in square feet

# 71. "X3Ssn.Porch" - (Continuous): Three season porch area in square feet

# 72. "Screen.Porch" - (Continuous): Screen porch area in square feet

# 73. "Pool.Area" - (Continuous): Pool area in square feet

# 74. "Pool.QC" - (Ordinal): Pool quality

# Ex	Excellent
# Gd	Good
# TA	Average/Typical
# Fa	Fair
# NA	No Pool

# 75. "Fence" - (Ordinal): Fence quality

# GdPrv	Good Privacy
# MnPrv	Minimum Privacy
# GdWo	Good Wood
# MnWw	Minimum Wood/Wire
# NA	No Fence

# 76. "Misc.Feature" -(Nominal): Miscellaneous feature not covered in other categories

# Elev	Elevator
# Gar2	2nd Garage (if not described in garage section)
# Othr	Other
# Shed	Shed (over 100 SF)
# TenC	Tennis Court
# NA	None

# 77. "Misc.Val" - (Continuous): $Value of miscellaneous feature

# 78. "Mo.Sold" - (Discrete): Month Sold (MM)

# 79. "Yr.Sold" - (Discrete): Year Sold (YYYY)

# 80. "Sale.Type" - (Nominal): Type of sale

# WD 	Warranty Deed - Conventional
# CWD	Warranty Deed - Cash
# VWD	Warranty Deed - VA Loan
# New	Home just constructed and sold
# COD	Court Officer Deed/Estate
# Con	Contract 15% Down payment regular terms
# ConLw	Contract Low Down payment and low interest
# ConLI	Contract Low Interest
# ConLD	Contract Low Down
# Oth	Other

# 81. "Sale.Condition" - (Nominal): Condition of sale

# Normal	Normal Sale
# Abnorml	Abnormal Sale -  trade, foreclosure, short sale
# AdjLand	Adjoining Land Purchase
# Alloca	Allocation - two linked properties with separate deeds, typically condo with a garage unit	
# Family	Sale between family members
# Partial	Home was not completed when last assessed (associated with New Homes)

# 82. "Order" - (Discrete): Observation number

variables<-c("PID", "MS.Zoning", "area", "MS.SubClass", "price", "Lot.Frontage", "Lot.Area",
             "Street", "Alley", "Lot.Shape", "Land.Contour", "Utilities",  "Lot.Config","Land.Slope", 
             "Neighborhood","Condition.1", "Condition.2", "Bldg.Type",
             "House.Style", "Overall.Qual", "Overall.Cond", "Year.Built", "Year.Remod.Add",
             "Roof.Style", "Roof.Matl", "Exterior.1st", "Exterior.2nd", "Mas.Vnr.Type", "Mas.Vnr.Area",
             "Exter.Qual","Exter.Cond", "Foundation","Bsmt.Qual", "Bsmt.Cond", "Bsmt.Exposure",
             "BsmtFin.Type.1", "BsmtFin.SF.1", "BsmtFin.Type.2", "BsmtFin.SF.2", "Bsmt.Unf.SF", "Total.Bsmt.SF",
             "Heating", "Heating.QC", "Central.Air", "Electrical", "X1st.Flr.SF","X2nd.Flr.SF", "Bsmt.Full.Bath",
             "Bedroom.AbvGr", "Low.Qual.Fin.SF", "Bsmt.Half.Bath", "Half.Bath", "Kitchen.AbvGr","Kitchen.Qual", "TotRms.AbvGrd",
             "Functional", "Fireplaces", "Fireplace.Qu", "Garage.Type", "Garage.Yr.Blt", "Garage.Finish", "Garage.Cars",
             "Garage.Area",  "Garage.Qual", "Garage.Cond", "Paved.Drive", "Wood.Deck.SF", "Open.Porch.SF", "Enclosed.Porch",
             "X3Ssn.Porch", "Screen.Porch", "Pool.Area", "Pool.QC", "Fence", "Misc.Feature","Misc.Val", "Mo.Sold", "Yr.Sold",
             "Sale.Type", "Sale.Condition")

# https://ww2.amstat.org/publications/jse/v19n3/decock/datadocumentation.txt details
data_select<-subset(data_full, select=variables)
data_select<- subset(data_full, select=c("Order","PID", "MS.Zoning", "area", "MS.SubClass", "price", "Lot.Frontage", "Lot.Area",
                                         "Street", "Alley", "Lot.Shape", "Land.Contour", "Utilities",  "Lot.Config","Land.Slope", 
                                         "Neighborhood","Condition.1", "Condition.2", "Bldg.Type",
                                         "House.Style", "Overall.Qual", "Overall.Cond", "Year.Built", "Year.Remod.Add",
                                         "Roof.Style", "Roof.Matl", "Exterior.1st", "Exterior.2nd", "Mas.Vnr.Type", "Mas.Vnr.Area",
                                         "Exter.Qual","Exter.Cond", "Foundation","Bsmt.Qual", "Bsmt.Cond", "Bsmt.Exposure",
                                         "BsmtFin.Type.1", "BsmtFin.SF.1", "BsmtFin.Type.2", "BsmtFin.SF.2", "Bsmt.Unf.SF", "Total.Bsmt.SF",
                                         "Heating", "Heating.QC", "Central.Air", "Electrical", "X1st.Flr.SF","X2nd.Flr.SF", "Bsmt.Full.Bath",
                                         "Bedroom.AbvGr", "Low.Qual.Fin.SF", "Bsmt.Half.Bath", "Half.Bath", "Kitchen.AbvGr","Kitchen.Qual", "TotRms.AbvGrd",
                                         "Functional", "Fireplaces", "Fireplace.Qu", "Garage.Type", "Garage.Yr.Blt", "Garage.Finish", "Garage.Cars",
                                         "Garage.Area",  "Garage.Qual", "Garage.Cond", "Paved.Drive", "Wood.Deck.SF", "Open.Porch.SF", "Enclosed.Porch",
                                         "X3Ssn.Porch", "Screen.Porch", "Pool.Area", "Pool.QC", "Fence", "Misc.Feature","Misc.Val", "Mo.Sold", "Yr.Sold",
                                         "Sale.Type", "Sale.Condition"))



#To see the summary of each column:

summary(data_full)

# Quite a few variables in the dataframe have NAs. 

#Cecking for NA

sum((is.na(data_full)))# 13997 NAs

#Identifying outliers:
#Creating a a plot of SALE PRICE vs GR LIV AREA to identify the outliers:

ggplot(data = data_full, aes(x = Gr.Liv.Area, y =SalePrice, colour=Sale.Condition))+ geom_jitter(size=1)+
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))

#Three of them with the living area bigger than 450 0000 square feet and with very low price of of less than 200 000.
#Those were also were sold partially that might not represent true market values.

which(data_full$Gr.Liv.Area>4000 & data_full$SalePrice<200000)# 1499 2181 2182

# The other two are just unusual sales with very large houses and quite appropriate prices. One of them was sold under abnormal sale conditions.

which(data_full$Gr.Liv.Area>4000 & data_full$SalePrice>650000)# 1761 1768


# For my further analysis I will remove any houses with more than 4000 square feet from the 
#data set with "Normal" Sale.Condition only.
data_full <- data_full %>% filter(Sale.Condition == "Normal" & Gr.Liv.Area < 4001 )

# Home prices lesser than $500k.
# Gr Liv Area(area) limiting to above less than 4000 sqrFt. 

data_full <- data_full %>% filter(Sale.Condition == "Normal" & Gr.Liv.Area < 4001 )

data_full <- data_full %>% filter(Sale.Condition == "Normal" & Gr.Liv.Area < 4001 )
max(data_full$Sale.Condition)
max(data_full$SalePrice)

#To get a better picture I am going to visualize  them.
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


# The variable with highest number of missing variable is "Pool.QC"(pool quality):

sum(is.na(data_full$Pool.QC))
nrow(data_full)
table(data_full$Pool.QC)

# There are 4 swimming pools with excellent conditions, 2 with fair, 4 - good and 3 - avg. The number of NAs is 2403 out of 2412. As per data description document "NA"	stands for "No Pool". Iowa has a humid continental climate throughout the state with
# average low and high of -11.3	and -1.3	in the month of January; 17.6	and 29.1	in July accordingly particularly in Ames.
# Therefore it is only natural that pool is not the priority and having a pool and its maintenance is rather costly keeping in mind that there are only 200 sunny days ayear on average.


#As was mentioned above  NA stands for no pool, therefore for convenience I am going to change NAs for "no_pool" level:

data_full<-transform(data_full, Pool.QC=fct_explicit_na(Pool.QC, "None"))
any(stri_isempty(data_full$Pool.QC)) # checking for empty strings
# I am going to create a new variable which I am going to use for my model called "Pool" that will indicate wheather or not there is a pool in a property:

data_full<-transform(data_full, Pool=ifelse(Pool.QC=="no_pool", "no", "yes"))



# The second value with the highest variable is "Miscellaneous feature" with 2316 NAs out of 2412. These are 
# the propreties with some features not covered in other categories such as, elevator, 2nd garage,
# tennins court, shed (a simple roofed structure used for garden storage, to shelter animals, or as a workshop), other.

sum(is.na(data_full$Misc.Feature))

# As was mentioned above  NA stands for no "Miscellaneous feature". That´s why I am going to change NAs for "no_feat" and set it as a level:
data_full<-transform(data_full, Misc.Feature=fct_explicit_na( Misc.Feature, "None"))
any(stri_isempty(data_full$Misc.Feature)) #checking for empty strings

# I am going to create a new variable which I am going to use for my model called "M_feat" that will indicate wheather or not there is any miscellaneous features in a property:

data_full<-transform(data_full, M_feat=ifelse(Misc.Feature=="no_feat", "no", "yes"))
table(data_full$M_feat)

# The third variable with the highest number of missing values is "Alley" - type of alley access to property(Grvl./Pave).
table(data_full$Alley)
sum(is.na(data_full$Alley))

# With 2258 out of  2412 being NAs. NA stands for "No alley access",
# therefore I am going to turn it into "None" and set it to a level.

data_full<-transform(data_full, Alley=fct_explicit_na( Alley, "None"))
any(stri_isempty(data_full$Alley)) #checking for empty strings
# I am going to create a new variable which I am going to use for my model called "alley" that will indicate wheather or not there is an alley near a property:

data_full<-transform(data_full, alley=ifelse(Alley=="no_alley", "no", "yes"))
table(data_full$alley)

# The fourth variable with the highest number of missing values is "Fense" (fence quality). 

sum(is.na(data_full$Fence))

# With  1910 being NAs. NAs stand for "no_fence",
# therefore I am going to turn it into "None" and set it to a level.

data_full<-transform(data_full, Fence=fct_explicit_na( Fence, "None"))
any(stri_isempty(data_full$Fence)) #checking for empty strings

# I am going to create a new variable which I am going to use for my model called "fence" that will indicate wheather or not there is a fence in a property:

data_full<-transform(data_full, fence=ifelse( Fence=="no_fence", "no", "yes"))

#The next variable with high number of NAs is "Fireplace.Qu"(fireplace quality).

sum(is.na(data_full$Fireplace.Qu))

# With 1164 NAs that stand for "no_fireplace". 
#Before doing anything let´s see if the number of properties
#with 0 or no fireplace coinsides with the number of NAs in Fireplace.Qu, using Fireplaces which stands for number of fireplaces.

sum(is.na(data_full$Fireplace.Qu)) == length(which(data_full$Fireplaces== 0)) # they are the same;
table(data_full$Fireplaces)

# I am going to turn it into "None" and set it to a level.

data_full<-transform(data_full, Fireplace.Qu=fct_explicit_na( Fireplace.Qu, "None"))
any(stri_isempty(data_full$Fireplace.Qu)) #checking for empty strings

#LotFrontage Lot frontage means the portion of a lot that abuts a public or private street.

#There are 451 NAs.

sum(is.na(data_full$Lot.Frontage))
min(data_full$Lot.Frontage, na.rm = TRUE)

# As the minimum is 21, I PRESUME that the NAs do actually stand for no 
# "Lot Frontage" and can be replaced by 0, since in this cases the areas of the Lot Frontages are equal to zero.
# Converting NAs into 0:

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
gar_data[is.na(gar_data$Garage.Finish) & is.na(gar_data$Garage.Cond),]
table(gar_data$Garage.Finish)
# Property 1357 has a detached garage with area of 360  sq. feet  for 1 car. Even though NAs in Garage.Cond and 
# Garage.Qual stand for "no garage" I doubt this is the case. So I am going to replce its categorical variable
# with the most commun level: Garage.Finish with "Unf", Garage.Cond and Garage.Qual with "TA" (avergae)
#I am going to change the Bsmt.Exposure of these three properties to "No"
data_full[1357, grep("Garage.Finish", colnames(data_full))] = "Unf"
data_full[1357, grep("Garage.Cond", colnames(data_full))] = "TA"
data_full[1357, grep("Garage.Qual", colnames(data_full))] = "No"
data_full<-transform(data_full, BsmtFin.Type.2=fct_explicit_na(BsmtFin.Type.2, "no_bsmnt"))
table(data_full$Garage.Cond)
#Therefore I am going replace 116 NAs for Garage.Type,
# Garage.Qual, Garage.Finish, Garage.Cond and Garage.Yr.Blt only if all five of them are NAs(which stands for "no ragage") and create a new variable 
#called "garage" with "yes/no" levels based on the above modifications.


data_full$Garage.Qual <- ifelse(is.na(data_full$Garage.Type) & is.na(data_full$Garage.Finish), "None", data_full$Garage.Qual)
data_full$Garage.Cond <- ifelse(is.na(data_full$Garage.Type) & is.na(data_full$Garage.Finish), "None", data_full$Garage.Cond)
data_full$Garage.Yr.Blt <- ifelse(is.na(data_full$Garage.Type) & is.na(data_full$Garage.Finish), "None", data_full$Garage.Yr.Blt)
data_full<-transform(data_full, Garage.Finish=fct_explicit_na(Garage.Finish, "None"))
data_full<-transform(data_full, Garage.Type=fct_explicit_na(Garage.Finish, "None"))


data_full$garage<- with(data_full, ifelse(Garage.Type=="no_gar", "no",ifelse(Garage.Finish=="no_gar", 
                                      "no", ifelse(Garage.Qual=="no_gar", "no",ifelse(Garage.Cond=="no_gar", "no", "yes")))))
# To check if there is 157 properties with no garage:
table(data_full$garage)
# We can conclude that there are 157 properties with no garage.

# The last group of variables of interest with NAs are THE 11 basement varibals:

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
(base_data[is.na(base_data$BsmtFin.Type.1) & is.na(base_data$BsmtFin.Type.2),])
# And convert these NAs to "None"
data_full<-transform(data_full, BsmtFin.Type.1=fct_explicit_na(BsmtFin.Type.1, "None"))
data_full<-transform(data_full, BsmtFin.Type.2=fct_explicit_na(BsmtFin.Type.2, "None"))
data_full<-transform(data_full, Bsmt.Qual=fct_explicit_na(Bsmt.Qual, "None"))
data_full<-transform(data_full, Bsmt.Cond=fct_explicit_na(Bsmt.Cond, "None"))
data_full<-transform(data_full, Bsmt.Exposure=fct_explicit_na(Bsmt.Exposure, "None"))


# Creating a new variable called "basement" indicating wheather or not there is a basement in a property
data_full$basement<- with(data_full, ifelse(BsmtFin.Type.1=="no_bsmnt", "no",ifelse(BsmtFin.Type.2=="no_bsmnt", 
                    "no", ifelse(Bsmt.Qual=="no_bsmnt", "no",ifelse(Bsmt.Cond=="no_bsmnt", "no",ifelse(Bsmt.Exposure=="no_bsmnt", "no", "yes"))))))
# See if there are 79 properties with no basement:
table(data_full$basement)


# The last variable with 11 NAs is Mas.Vnr.Area - the  masonry veneer area. It doesn´t say that NAs stand for no veneer area, so we leave it as
# it is for now

sum(is.na(data_full$Mas.Vnr.Area)) 
min(data_full$Mas.Vnr.Area, na.rm=TRUE)

# Before dealing with NAs I am going to replace all blanks with NAs
data_full[data_full==""] <- NA

#Mas.Vnr.Type has None as the most commun level, so I will change all NAs for "None"
table(data_full$Mas.Vnr.Type)
data_full<-transform(data_full, Mas.Vnr.Type=fct_explicit_na(Mas.Vnr.Type, "None"))
data_full$Mas.Vnr.Area[is.na(data_full$Mas.Vnr.Area)] <- 0

sapply(data_full, function(x) sum(is.na(x)))

library(dplyr) 
data_full <- data_full %>%
  mutate(
    Garage.Qual = as.factor(Garage.Qual),
    Garage.Cond = as.factor(Garage.Cond),
    Garage.Cars= as.numeric(Garage.Cars)
  )
str(data_full)




data_full[is.na(data_full$Garage.Yr.Blt),]
sum(is.na(data_full$BsmtFin.SF.1))
sum(is.na(data_full$Garage.Yr.Blt))
na_col<-c("Order", "PID", "BsmtFin.Type.1", "BsmtFin.SF.1", "BsmtFin.Type.2", "BsmtFin.SF.2", "Total.Bsmt.SF",
          "Bsmt.Unf.SF", "Bsmt.Qual", "Bsmt.Cond", "Bsmt.Exposure", "Bsmt.Full.Bath", "Bsmt.Half.Bath",
          "Garage.Yr.Blt", "Garage.Type", "Garage.Qual", "Garage.Finish", "Garage.Cond",
          "Garage.Area", "Garage.Cars", "Electrical","Mas.Vnr.Type", "Mas.Vnr.Area", "Lot.Frontage" )
na_data<-data_full[na_col]
sum(is.na(na_data$Bsmt.Exposure))
na_data[is.na(na_data$Bsmt.Exposure),]
# The minimum masonry veneer area is 0, meaning there is no masonry veneer.
min(data_full$Mas.Vnr.Area, na.rm=TRUE)
nrow(data_full[data_full$Mas.Vnr.Type=="None",])
nrow(data_full[data_full$Mas.Vnr.Area==0,])
sum(is.na(data_full$Mas.Vnr.Area))

# Let´s take a look at "Mas.Vnr.Area" vs "Mas.Vnr.Type" (the type of the material the masonry veneer is made of):

mas_vnr_col<-c("Order", "PID", "Mas.Vnr.Area", "Mas.Vnr.Type" )
mas_vnr_data<-data_full[mas_vnr_col] 
mas_vnr_data[is.na(mas_vnr_data$Mas.Vnr.Area)] 

# As one can see all of them have an empty string as its type. The number of empty strings in "Mas.Vnr.Type" is also 23:

sum(stri_isempty(data_full$Mas.Vnr.Type)) 

# Since the majority of the properties have no masonry veneer 

table(data_full$Bsmt.Exposure)

# I am going to convert the empty strings into "None" type as well and NAs in Mas.Vnr.Area into 0 since presumably there are no masonry veneer.
data_full$Mas.Vnr.Type<- sub("^$", "None", data_full$Mas.Vnr.Type)
mas_vnr_data[is.na(data_full$Mas.Vnr.Area),]
data_full$Mas.Vnr.Area[is.na(data_full$Mas.Vnr.Area)] <- 0


data_full[is.na(data_full$Mas.Vnr.Area),] 
sum(stri_isempty(data_full$Mas.Vnr.Type)) 

employee <- c('John Doe','Peter Gynn','Jolie Hope',"" )
salary <- c(21000, 23400, 26800, 45633)
startdate <- as.Date(c('2010-11-1','2008-3-25','2007-3-14', ""))
employ.data <- data.frame(employee, salary)
employ.data[c("employee", "b")][is.na(x[c("a", "b")])] <- 0
sum(stri_isempty(employ.data$employee)) >0
empty(employ.data)

employ.data[employ.data==""] <- NA


foo[foo==""] <- NA
empty <- function(data_set){
  columns<-c()
  for (column in names(data_set)){
    if(sum(stri_isempty(data_set$column)) >0){
      columns<-c(columns, column)
      
    }
      
  }
 
  return(columns)
}

for(i in names(df)){
  df[[paste(i, 'length', sep="_")]] <- str_length(df[[i]])
}
# one way
for (i in 1:length(values))
  vector[i] <- values[i]
# another way
for (i in 1:length(values))
  vector <- c(vector, values[i])
# yet another way?!?
for (v in values)
  vector <- c(vector, v)
# ... more ways
empty(employ.data)
#There is an observed difference, but is this difference statistically 
#significant? In order to answer this question we will conduct a hypothesis test:


inference(y = SalePrice, x = m_feature, data = data_full, statistic = "mean",sig_level = 0.05, type = "ht", null = 0, 
          alternative = "less", method = "theoretical")
sum(is.na(ames_train$Garage.Type))
data_full<-transform(data_full, m_feature=ifelse(Misc.Feature=="NA", 0, 1))
data_full$m_feature[is.na(data_full$m_feature)]<-0

summary(data_full$SalePrice)
xtabs(SalePrice ~ m_feature, data=data_full)
ggplot(data_full, mapping = aes(x=m_feature, y=SalePrice))+geom_boxplot()
nrow(data_full)

ggplot(data_full, mapping = aes(x=m_feature, y=SalePrice))+geom_boxplot()
# First let´s see if the min value of Misc.Val which stands for $Value of miscellaneous feature is 0 (indication of no miscellaneous feature).

min(data_full$Misc.Val, na.rm=TRUE)

# Let´s see if the number of NAs(which stands for "None") of Misc.Feature is the same s the number of 0 in Misc.Val:
sum(is.na(data_full$Misc.Feature))     # 2824 NAs
length(which(data_full$Misc.Val == 0)) # 2827 NAs

# There are three properties with  miscellaneous features (two sheds and one "other") with no price for them. Let´s see what they are:
###Don´t do this one yet
misc_feat_vec<-c("Order", "PID", "Misc.Feature", "Misc.Val")
misc_data<-data_full[misc_feat_vec]
misc_data[!is.na(misc_data$Misc.Feature) & misc_data$Misc.Val == 0,]

# Obviously these 0 are missing prices, which I am going to change for their median values.
# The median price of the features:

aggregate(data_full$Misc.Val, by=list(data_full$Misc.Feature), FUN=median)

#The median price of all sheds and features called "other" are 500 and 3250 respectively. 

data_full[676, grep("Misc.Val", colnames(data_full))] = 3250
data_full[1964, grep("Misc.Val", colnames(data_full))] = 500
data_full[2361, grep("Misc.Val", colnames(data_full))] = 500

sum(is.na(data_full$Misc.Feature)) == length(which(data_full$Misc.Val == 0)) # Now the number of NAs in Misc.Feature and 0 in Misc.Val is equal

# Pool Area is highly related to Pool.QC as it simply represents pool area in square feet and 0
# means there is no swimming pool:
min(data_full$Pool.Area, na.rm=TRUE)

# Let´s see if the number of 0 in pool Area corresponds to the number of NAs in Pool.QC.

sum(is.na(data_full$Pool.QC)) == length(which(data_full$Pool.Area == 0))
#Before doing anything let´s see if the number of properties
#with 0 or no fireplace coinsides with the number of NAs in Fireplace.Qu, using Fireplaces which stands for number of fireplaces.

sum(is.na(data_full$Fireplace.Qu)) == length(which(data_full$Fireplaces== 0)) # they are the same;
# Even though in the description it doesn´t explicitly say that NA stands for no lot frontage
# I suspect that might be the case.
# The absence of Lot Frontage can wheather be represented by 0 or by NA.
# First I would like to check if there are any zeros that could be attributed to no "Lot Frontage" 


min(data_full$Lot.Frontage, na.rm=TRUE)

# As the minimum is 21, I strongly believe that the NAs do actually stand for no 
# "Lot Frontage" and can be replaced by 0, since in this cases the areas of the Lot Frontages are equal to zero.
# Converting NAs into 0:

data_full$Lot.Frontage[is.na(data_full$Lot.Frontage)] <- 0
data_full[is.na(data_full$Garage.Type) & is.na(data_full$Garage.Qual) & is.na(data_full$Garage.Finish) & is.na(data_full$Garage.Cond) ,]
data_full<-transform(data_full, garage=ifelse( Fence=="no_fence", "no", "yes"))
base_data[is.na(data_full$Bsmt.Exposure) & !is.na(data_full$BsmtFin.Type.1),]
base_data[is.na(data_full$Bsmt.Exposure) & !is.na(data_full$BsmtFin.Type.2),] # o
base_data[!is.na(data_full$Bsmt.Exposure) & is.na(data_full$BsmtFin.Type.2),]
#stop here
data_full$Bsmt.Full.Bath[is.na(data_full$Bsmt.Full.Bath)] <- 0
data_full$Bsmt.Half.Bath[is.na(data_full$Bsmt.Half.Bath)] <- 0
data_full$BsmtFin.SF.1[is.na(data_full$BsmtFin.SF.1)] <- 0
data_full$BsmtFin.SF.2[is.na(data_full$BsmtFin.SF.2)] <- 0
data_full$Bsmt.Unf.SF[is.na(data_full$Bsmt.Unf.SF)] <- 0
data_full$Total.Bsmt.SF[is.na(data_full$Total.Bsmt.SF)] <- 0



#For convenience I am  also going to replace the empty strings of the variables BsmtFin.Type.1, BsmtFin.Type.2, Bsmt.Qual, 
#Bsmt.Cond, Bsmt.Exposure with NAs for the property with PID 903230120:

data_full[1342, grep("BsmtFin.Type.1", colnames(data_full))] = NA
data_full[1342, grep("BsmtFin.Type.2", colnames(data_full))] = NA
data_full[1342, grep("Bsmt.Qual", colnames(data_full))] = NA
data_full[1342, grep("Bsmt.Cond", colnames(data_full))] = NA
data_full[1342, grep("Bsmt.Exposure", colnames(data_full))] = NA

data_full<-transform(data_full, BsmtFin.Type.1=fct_explicit_na(BsmtFin.Type.1, "no_bas"))
data_full<-transform(data_full, BsmtFin.Type.2=fct_explicit_na(BsmtFin.Type.2, "no_bas"))
data_full<-transform(data_full, Bsmt.Qual=fct_explicit_na(Bsmt.Qual, "no_bas"))
data_full<-transform(data_full, Bsmt.Cond=fct_explicit_na(Bsmt.Cond, "no_bas"))
data_full<-transform(data_full, Bsmt.Exposure=fct_explicit_na(Bsmt.Exposure, "no_bas"))


# Check for any empty strings

any(stri_isempty(data_full$BsmtFin.Type.1))
any(stri_isempty(data_full$BsmtFin.Type.2)) 
any(stri_isempty(data_full$Bsmt.Qual)) 
any(stri_isempty(data_full$Bsmt.Cond)) 
any(stri_isempty(data_full$Bsmt.Exposure)) 

# There are two variables with empty strings: BsmtFin.Type.2 and Bsmt.Exposure. Let´s take a closer look at them:
base_data[(data_full$BsmtFin.Type.2)=="",]
base_data[(data_full$Bsmt.Exposure)=="",]

# Since the majority of the properties have unfinished status 
table(data_full$BsmtFin.Type.2)
# I am going to change status of the property with PID 528142130 and order number 445 to "Unf" too:
data_full[445, grep("BsmtFin.Type.2", colnames(data_full))] = "Unf"

# Since the majority of the properties have No exposure 

table(data_full$Bsmt.Exposure)

#I am going to change the Bsmt.Exposure of these three properties to "No"
data_full[67, grep("Bsmt.Exposure", colnames(data_full))] = "No"
data_full[1797, grep("Bsmt.Exposure", colnames(data_full))] = "No"
data_full[2780, grep("Bsmt.Exposure", colnames(data_full))] = "No"

