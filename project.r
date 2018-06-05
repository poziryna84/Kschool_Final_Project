##### 1. Loading data set and packages #####

data = read.csv("AmesHousing.csv", header=TRUE, sep = ";")

if(!require("ggplot2")){
  install.packages("ggplot2")
  library("ggplot2")
}

if (!require("e1071")){
  install.packages("e1071")
  library("e1071")
}

if (!require("ggpubr")){
  install.packages("ggpubr")
  library("ggpubr")
}
if (!require("outliers")){
  install.packages("outliers")
  library("outliers")
}
if (!require("dplyr")){
  install.packages("dplyr")
  library("dplyr")
}
if (!require("corrplot")){
  install.packages("corrplot")
  library("corrplot")
}
if (!require("regclass")){
  install.packages("regclass")
  library("regclass")
}
if (!require("forcats")){
  install.packages("forcats")
  library("forcats")
}
if (!require("caret")){
  install.packages("caret")
  library("caret")
}
if (!require("Boruta")){
  install.packages("Boruta")
  library("Boruta")
}

##### 2. EDA #####

set.seed(123)

split <- sample(nrow(data), floor(0.7*nrow(data)))

train<- data[split,]
test <- data[-split,]

##### 2.1 Response variable analysis. #####


summary(train$SalePrice)

#Visualizing the distribution the response variable.

mode <- function(x) {
  un <- unique(x)
  un[which.max(tabulate(match(x, un)))]
}
S_P<-ggplot(train, aes(x=SalePrice,y = ..density.., colour="red" )) +geom_density(size = 1, colour = 'brown')+
  geom_histogram(bins = 30, fill = 'pink', colour = 'brown') +  scale_x_continuous("Sale Price in thousands",labels=function(x) x/1000)+
  labs(title = "Sale Price distribution")+
  geom_vline(data = train, mapping = aes( xintercept = mean(train$SalePrice), colour = 'mean'), size = 1.5)+
  geom_vline(data = train,mapping = aes( xintercept = median(train$SalePrice), colour = 'median'), size = 1.5)+
  geom_vline(data = train,mapping = aes( xintercept = mode(train$SalePrice), colour = 'mode'), size = 1.5)

print(S_P)

# Shapiro-Wilk normality test

shapiro.test(train$SalePrice)

# Testing for skewness:

skewness(train$SalePrice) 

# For my future modeling I want my data to be as "normal" as possible. To decide on the
# outliers strategy I am going to conduct several shapiro tests and make some visualizations.

S_P_log<-ggplot(train, aes(x=log(SalePrice),y = ..density.., colour="red" )) +geom_density(size = 1, colour = 'brown')+
  geom_histogram(bins = 30, fill = 'pink', colour = 'brown')+scale_x_continuous("The log of Sale Price",labels=function(x) x/1000)+
  labs(title = "Sale Price distribution (log)")

S_p_no_outliers<-ggplot(outliers, aes(x=SalePrice,y = ..density.., colour="red" )) +geom_density(size = 1, colour = 'brown')+
  geom_histogram(bins = 30, fill = 'pink', colour = 'brown')+scale_x_continuous("Sale Price in thousands",labels=function(x) x/1000)+
  labs(title = "Sale Price distribution (without outliers)")

qq_lsp <- qplot(sample = log(SalePrice), data = train)
qq_sp<-qplot(sample = SalePrice, data = train)
fig_s_p <- ggarrange(S_P,S_P_log,qq_sp,qq_lsp, 
                 ncol = 2, nrow = 2)

print(fig_s_p)

# Shapiro-Wilk normality test.

shapiro.test(train$SalePrice) # W = 0.87827
shapiro.test(log(train$SalePrice)) # W = 0.98782

# Grubbs test to identify the outliers:

grubbs.test(log(train$SalePrice), type=10, opposite = FALSE)

# It looks like log transformation results in the most "normal" distribution therefore for my future modeling
# I will be using the log of the SalePrice and filter out the lowest value =  9.48036750918924.

##### 2.2 Multivariate analysis and understanding the structure of the data. #####

# The summary of its internal structue: 

dim(train)

glimpse(train)

# Converting MS.SubClass, Overall.Qual and Overall.Cond to factor.

train$MS.SubClass<-as.factor(train$MS.SubClass)
test$MS.SubClass<-as.factor(test$MS.SubClass)

train$Overall.Qual<-as.factor(train$Overall.Qual)
test$Overall.Qual<-as.factor(test$Overall.Qual)

train$Overall.Cond<-as.factor(train$Overall.Cond)
test$Overall.Cond<-as.factor(test$Overall.Cond)

##### 2.2.1 Numeric variables. #####

# Identifying and visualizing numerical variables that are stromgly correlated (-0.5 >cor.coef>0.5) with SalePrice.
# excluding "Order", "PID", "Yr.Sold" and "Mo.Sold" variables. 

nums <- unlist(lapply(train, is.numeric)) 
colnames <- dimnames(train[ , nums])[[2]]
colnames <- colnames[!colnames %in% c("Order", "PID", "Yr.Sold", "Mo.Sold" )]

num_data <- train[,colnames]
crls <- cor(num_data, method = "spearman")

price_cor <- as.matrix(sort(crls [,'SalePrice'], decreasing = TRUE))
idx_cor = names(which(apply(price_cor, 1, function(x) (x > 0.5 | x < -0.5))))

corrplot(as.matrix(crls[idx_cor,idx_cor]), type = 'upper', method='color', 
         addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7)



# Sale price vs Gr.Liv.Area (above grade (ground) living area square feet) vs Sale.Condition:

fig1 <- ggplot(train, aes(x=Gr.Liv.Area, y=SalePrice)) +
  geom_point(shape=1) + 
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))+
  labs(title = "Sale price vs Gr.Liv.Area")+
   geom_smooth(method="lm") 

fig2 <- ggplot(data = train, aes(x = Gr.Liv.Area, y =SalePrice, color=Sale.Condition))+ geom_jitter(size=1)+
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))+
  labs(title = "Sale price vs Gr.Liv.Area vs Sale.Condition")

fig3 <- ggarrange( fig1, fig2,
                     ncol = 1, nrow = 2)
print(fig3)

# Garage.cars vs Garage.Area vs SalePrice:

g.a <- ggplot(train, aes(x= Garage.Area, y=SalePrice)) +
  geom_point(shape=1) + 
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))+
  labs(title = "Sale price vs Garage.Area")+
  geom_smooth(method="lm")

g.a.c <- ggplot(data = train, aes(x = Garage.Area, y =SalePrice, color=as.factor(Garage.Cars)))+ geom_jitter(size=1)+
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))+
  labs(title = "SalePrice vs Garage.cars vs Garage.Area")

fig4 <- ggarrange( g.a, g.a.c,
                   ncol = 1, nrow = 2)
print(fig4)

# Year.Built and Year.Remod.Add vs Sale Price:

ggplot(data=train, aes(Year.Built, y = ..density.., colour="red")) +
  geom_density(size = 1, colour = 'brown')+ 
  geom_histogram(bins = 30, fill = 'pink', colour = 'brown')+
  labs(title = "Year.Built distribution")

y.b<- ggplot(train, aes(x= Year.Built, y=SalePrice)) +
  geom_point(shape=1) + 
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))+
  labs(title = "Year.Built vs SalePrice")+
  geom_smooth(method="lm")
 
y.r<- ggplot(train, aes(x= Year.Remod.Add, y=SalePrice)) +
   geom_point(shape=1) + 
   scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))+
   labs(title = "Year.Remod.Add vs SalePrice")+
   geom_smooth(method="lm")

fig5 <- ggarrange( y.b, y.r,
                   ncol = 1, nrow = 2)
print(fig5)

# Full bath and TotRms.AbvGrd vs SalePrice:

f.b <- ggplot(train, aes(x= Full.Bath, y=SalePrice)) +
  geom_point(shape=1) + 
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))+
  labs(title = "Full.Bath vs SalePrice")+
  geom_smooth(method="lm")

t.r <- ggplot(train, aes(x= TotRms.AbvGrd, y=SalePrice)) +
  geom_point(shape=1) + 
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))+
  labs(title = "Total rooms vs SalePrice")+
  geom_smooth(method="lm")

b.r.p <- ggplot(train, aes(x= TotRms.AbvGrd, y=SalePrice, color=as.factor(Full.Bath))) +
  geom_point(shape=15) + 
  scale_y_continuous(name = "Sale Price",labels = function(y) paste0(y / 1000, "k"))+
  labs(title = "Total rooms vs Full.Bath vs SalePrice")

fig6 <- ggarrange( f.b, t.r, b.r.p, 
                   ncol = 2, nrow = 2)
print(fig6)


# Above ground living area (Gr.Liv.Area) and area of the 1st and 2nd floors ("X1st.Flr.SF", "X2nd.Flr.SF"):

nrow(train[train$X2nd.Flr.SF+train$X1st.Flr.SF==train$Gr.Liv.Area ,])
nrow(train)

##### 2.2.2 Categorical variables analysis. #####

# Price vs. Neighborhood:

number <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
}
ggplot(data = train, aes(x =reorder(Neighborhood, SalePrice, median) , y =SalePrice,color=Neighborhood)) +geom_boxplot(outlier.size=0.2)+stat_summary(fun.data = number, geom = "text", fun.y = median)+ 
  scale_y_continuous(name = "Price", labels = function(y) paste0(y / 1000, "k"))+ ggtitle("The Neighborhood vs Price")+
  
  scale_x_discrete(name="Neighborhood", labels=c("Mdw","BrDl","IDTR","OldT","BrkSd", "Edwr","SWSU", "Swyr", "Blst","Lndmk","NAms", "NPKvl",
                                                 "Mtchl", "SwyrW", "NWAms", "Blmgt", "Glbrt", "ClrCr", "ClgCr", "Crwf",
                                                 "Grns", "Tmbr","Smrst", "Veen", "GrnHll", "NoRd" ,  "NrdgHt", "StnBr"))


neig_stats<-train %>%
  group_by(Neighborhood)%>%
  summarise(median.price=median(SalePrice), Iqr.price=IQR(SalePrice), counts=n())%>%
  arrange(desc(median.price, counts))
print(as.data.frame(neig_stats))



# Overall quality and Overall condition.

o.q<-ggplot(data =train, aes(x =reorder(Overall.Qual, SalePrice, median) , y =SalePrice, color=Overall.Qual)) +geom_boxplot(outlier.size=0.2)+
  stat_summary(fun.data = number, geom = "text", fun.y = median)+
  scale_y_continuous(name = "Price", labels = function(y) paste0(y / 1000, "k"))+ ggtitle("Overall Quality vs Price")+
  scale_x_discrete(name="Quality")

o.c<-ggplot(data =train, aes(x =reorder(Overall.Cond, SalePrice, median) , y =SalePrice, color=Overall.Cond)) +geom_boxplot(outlier.size=0.2)+
  stat_summary(fun.data = number, geom = "text", fun.y = median)+
  scale_y_continuous(name = "Price", labels = function(y) paste0(y / 1000, "k"))+ ggtitle("Overall Condition vs Price")+
  scale_x_discrete(name="Condition")
fig7 <- ggarrange(o.q, o.c,
                   ncol = 2, nrow = 1)
print(fig7)

# The rest of the quality and condition variables.


ext.q<-ggplot(data =train, aes(x =reorder(Exter.Qual, SalePrice, median) , y =SalePrice, color=Exter.Qual)) +geom_boxplot(outlier.size=0.2)+
  stat_summary(fun.data = number, geom = "text", fun.y = median)+
  scale_y_continuous(name = "Price", labels = function(y) paste0(y / 1000, "k"))+ ggtitle("Exterior Quality vs Price")+
  scale_x_discrete(name="Exterior Quality")

ext.c<-ggplot(data =train, aes(x =reorder(Exter.Cond, SalePrice, median) , y =SalePrice, color=Exter.Cond)) +geom_boxplot(outlier.size=0.2)+
  stat_summary(fun.data = number, geom = "text", fun.y = median)+
  scale_y_continuous(name = "Price", labels = function(y) paste0(y / 1000, "k"))+ ggtitle("Exterior Condition vs Price")+
  scale_x_discrete(name="Exterior Condition")

bsmt.q<-ggplot(data =train, aes(x =reorder(Bsmt.Qual, SalePrice, median) , y =SalePrice, color= Bsmt.Qual)) +geom_boxplot(outlier.size=0.2)+
  stat_summary(fun.data = number, geom = "text", fun.y = median)+
  scale_y_continuous(name = "Price", labels = function(y) paste0(y / 1000, "k"))+ ggtitle("Basement Quality vs Price")+
  scale_x_discrete(name="Basement Quality")

bsmt.c<-ggplot(data =train, aes(x =reorder(Bsmt.Cond, SalePrice, median) , y =SalePrice, color= Bsmt.Cond)) +geom_boxplot(outlier.size=0.2)+
  stat_summary(fun.data = number, geom = "text", fun.y = median)+
  scale_y_continuous(name = "Price", labels = function(y) paste0(y / 1000, "k"))+ ggtitle("Basement Condition vs Price")+
  scale_x_discrete(name="Basement Condition")

gr.q<-ggplot(data =train, aes(x =reorder(Garage.Qual, SalePrice, median) , y =SalePrice, color= Garage.Qual)) +geom_boxplot(outlier.size=0.2)+
  stat_summary(fun.data = number, geom = "text", fun.y = median)+
  scale_y_continuous(name = "Price", labels = function(y) paste0(y / 1000, "k"))+ ggtitle("Garage Quality vs Price")+
  scale_x_discrete(name="Garage Quality")

gr.c<-ggplot(data =train, aes(x =reorder(Garage.Cond, SalePrice, median) , y =SalePrice, color=Garage.Cond)) +geom_boxplot(outlier.size=0.2)+
  stat_summary(fun.data = number, geom = "text", fun.y = median)+
  scale_y_continuous(name = "Price", labels = function(y) paste0(y / 1000, "k"))+ ggtitle("Garage Condition vs Price")+
  scale_x_discrete(name="Garage Condition")

fig8 <- ggarrange(ext.q, ext.c, bsmt.q, bsmt.c, gr.q, gr.c,
                  ncol = 2, nrow = 3)
print(fig8)


##### 3. Handling missing values. #####

data_full<-rbind(train, test)

summary(data_full)

# Quite a few variables have NAs and the maximum Garage.Yr.Blt value is 2207 which is too
# much even for machine learning.

# Visualize  NAs.

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

# Setting NA to "No_ftr" (no feature) in the case of the 14 variables.

NA_cols<-c("Pool.QC", "Misc.Feature", "Alley", "Fence", "Fireplace.Qu","Garage.Type", "Garage.Qual", 
           "Garage.Finish","Garage.Cond", "BsmtFin.Type.1", "BsmtFin.Type.2","Bsmt.Qual",
           "Bsmt.Cond","Bsmt.Exposure")

length(NA_cols)

data_full[NA_cols]<-lapply(data_full[,NA_cols], function(x) fct_explicit_na(x, na_level = "No_ftr"))

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

# There are 490 NAs.

sum(is.na(data_full$Lot.Frontage))

# Replace NAs with its median value:

data_full$Lot.Frontage[is.na(data_full$Lot.Frontage)] <- median(data_full$Lot.Frontage, na.rm = TRUE)

# Regarding the rest of the variables the number of NA is negligible (less than 0.5%) so I'll 
# assume that NA are "None"(as its most commun case) for MasVnrType and 0 for MasVnrArea, basement and garage variables.

table(data_full$Mas.Vnr.Type)
data_full$Mas.Vnr.Area[is.na(data_full$Mas.Vnr.Area)] <- 0
data_full<-transform(data_full, Mas.Vnr.Type=fct_explicit_na(data_full$Mas.Vnr.Type, "None"))

data_full$Total.Bsmt.SF[is.na(data_full$Total.Bsmt.SF)] <- 0
data_full$Bsmt.Unf.SF[is.na(data_full$Bsmt.Unf.SF)] <- 0
data_full$BsmtFin.SF.2[is.na(data_full$BsmtFin.SF.2)] <- 0
data_full$BsmtFin.SF.1[is.na(data_full$BsmtFin.SF.1)] <- 0
data_full$Bsmt.Half.Bath[is.na(data_full$Bsmt.Half.Bath)] <- 0
data_full$Bsmt.Full.Bath[is.na(data_full$Bsmt.Full.Bath)] <- 0

data_full$Garage.Cars[is.na(data_full$Garage.Cars)] <- 0
data_full$Garage.Area[is.na(data_full$Garage.Area)] <- 0

# Garage Year Built

data_full$Garage.Yr.Blt[is.na(data_full$Garage.Yr.Blt)] <- 0
nrow(data_full[(data_full$Garage.Yr.Blt>2010),])

# There is just one case and it must be a typo so I am going to set it to its
# Year.Remod.Add (Remodel date)

data_full$Garage.Yr.Blt[(data_full$Garage.Yr.Blt>2010)] <- data_full[(data_full$Garage.Yr.Blt>2010),"Year.Remod.Add"]

# There is only 1 NA in "Electrical, which I am going to replace with the most commun level "SBrkr"

sum(is.na(data_full$Electrical))
table(data_full$Electrical)

data_full<-transform(data_full, Electrical=fct_explicit_na(data_full$Electrical, "SBrkr"))

sum(is.na(data_full))

##### 4. Feature engeneering part. #####

##### 4.1. Identifying and dropping unused factor levels. #####

# Identifying.

unused_levels<- function(dataset, number){
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


unused_levels(data_full, number=0)

# Dropping the levels that do not occur:

data_full[unused_levels(data_full, number = 0)]<-lapply(data_full[,unused_levels(data_full, number = 0)], function(x) droplevels(x))    

##### 4.2. Combining levels. #####

# Neighborhood levels combination.

f<-suggest_levels(SalePrice~Neighborhood,data=data_full,target=5,recode=TRUE)

# The new levels are A, B, C , E as follows:

f$Conversion2

# Creating new Neighborhood variable:

data_full$New_Neighborhood<-f$newlevels

new_neighb<-ggplot(data_full, aes(New_Neighborhood, fill=Neighborhood))+geom_histogram(stat="count")+ggtitle("New Neighborhoods")+xlab("New Neighborhood counts")+theme_bw()

new_price_neigh<-ggplot(data = data_full, aes(x =reorder(New_Neighborhood, SalePrice, median) , y =SalePrice,color= New_Neighborhood)) +geom_boxplot(outlier.size=0.2)+stat_summary(fun.data = number, geom = "text", fun.y = median)+ 
  scale_y_continuous(name = "Price", labels = function(y) paste0(y / 1000, "k"))+ ggtitle("The Neighborhood vs Price")


fig_neighb <- ggarrange(new_neighb, new_price_neigh,
                        ncol = 2, nrow = 1)
print(fig_neighb)

table(data_full$New_Neighborhood)

# Overall condition and Overall quality modifications:

table(data_full$Overall.Qual)
table(data_full$Overall.Cond)

# Overall Qaulity and Cuantity variable levels will be transformed as follows:

lev_reset<-function(vector){
  levels(vector)<-list(blw_avg=c("1","2","3", "4"),avg="5",good=c("6", "7"), exc=c("8", "9", "10"))
  vector
}

variables<-c("Overall.Qual", "Overall.Cond")

data_full[variables]<-lapply(data_full[,variables], function(x) lev_reset(x))

new_qual <- ggplot(data_full, aes(Overall.Qual, fill=Overall.Qual))+geom_histogram(stat="count")+ggtitle("Overall quality")+xlab("Rates of overall quality")+theme_bw()

new_cond <- ggplot(data_full, aes(Overall.Cond, fill=Overall.Cond))+geom_histogram(stat="count")+ggtitle("Overall condition")+xlab("Rates of overall condition")+theme_bw()

new_fig <- ggarrange(new_qual,new_cond, 
                     ncol = 2, nrow = 2)

print(new_fig)

table(data_full$Overall.Qual)
table(data_full$Overall.Cond)

# Other quality and condition variables levels combination.

# Since all the other quality and condition variables have a clear ordering and share the same legend it makes sense to modify them together.

# Reseting the levels to blw_avg, avg, abv_avg and No_ftr:

fun1<-function(vector){
  levels(vector)<-list(blw_avg=c("Fa","Po"),avg="TA",abv_avg=c("Ex", "Gd"), No_ftr=("No_ftr"))
  vector
}
list<-c("Heating.QC", "Garage.Qual", "Garage.Cond", "Bsmt.Cond", "Kitchen.Qual", "Pool.QC", "Fireplace.Qu", "Exter.Qual", "Exter.Cond", "Bsmt.Qual")

# Applying:

data_full[,list]<-lapply(data_full[,list], function(x) fun1(x))

# Vizualizing the results:

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

figure8 <- ggarrange(g_c, g_q, heat, bsmt_cond, bsmt_q, kitch, pool, fpl, extq, extc,
                     ncol = 3, nrow = 4,
                     common.legend = TRUE,
                     legend = "bottom")

print(figure8)

# Combining the levels of the rest of the categorical variables.

# To combine levels using their frequency, we first look at the frequency distribution of each level 
# and reset the levels that have frequency of less than 5% of total observation into "Other" level.

# Function to spot variables with levels that have frequency of less than 5%:

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


low.lev.var<-lowvar_levels(data_full, 5) 
print(low.lev.var) 

# Function condenseMe(vector, name, limit) sets low frequency counts into an 'Other' category:

condenseMe <- function(vector, name, limit) {
  
  toCondense <- names(which(prop.table(table(vector)) < limit))
  levels(vector)[levels(vector) %in% toCondense] <- name
  
  vector
}

# Apllying the function excluding all quality and condition variables:

new_list<-low.lev.var[ !grepl( "Q" , low.lev.var, fixed = TRUE ) & !grepl( ".Cond" , low.lev.var, fixed = FALSE)
                       & !grepl( "Neighborhood" , low.lev.var, fixed = TRUE )]

# Applying the function:

data_full[new_list]<-lapply(data_full[,new_list], function(x) condenseMe(x, limit = 0.05, name = "Other"))
summary(data_full)

##### 4.3. Creating new variables. #####

# Rather than using year variables I am going to use their modified versions such as the age of the house/garage 
# and the period of time since it was last remodeled.

data_full$Garage.age<-sapply(data_full$Garage.Yr.Blt, function(x) {ifelse((x >0), 2011 - x,  0)})
data_full$House.age <- sapply(data_full$Year.Built, function(x) 2011 - x)
data_full$Remod.age <- sapply(data_full$Year.Remod.Add, function(x) 2011 - x)


set.seed(123)

train <- subset(data_full, Order %in% train$Order)
test <- subset(data_full, Order %in% test$Order)

# Filtering out "non-normal" Sale.Condition and final drops in the train.

train<- train %>% filter(Sale.Condition == "Normal")
test<- test %>% filter(Sale.Condition == "Normal")


drops<-c("Year.Built", "Year.Remod.Add", "Garage.Yr.Blt", "Sale.Condition", "X1st.Flr.SF", 
         "X2nd.Flr.SF", "Neighborhood","Mo.Sold", "Yr.Sold", "Order", "PID", "Sale.Type")

train<-train[ , !(names(train) %in% drops)] 

# Feature selection using Boruta:

set.seed(123)

boruta.train <- Boruta(SalePrice~., data = train, doTrace = 2)

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

# 13 most important variables:

f<-rev(names(Labels))

imp.var<-f[1:13]
print(imp.var)

# Creating Log.Price variable and filtering the outlier:

train$Log.Price <- log(train$SalePrice)
train<-train[(train$Log.Price>9.48036750918924 ),]
train<-train[ , !(names(train) %in% c("SalePrice"))] 

# Creating dummy variables in train and test:

# Train.

f<-dummyVars("~ .", data=train, fullRank = FALSE)
train<- data.frame(predict(f, newdata=train))

# Test.

foo<-dummyVars("~ .", data=test, fullRank = FALSE)
test<- data.frame(predict(foo, newdata=test))


##### 5. Modeling. #####

# k-fod validation:

set.seed(123)
myControl <- trainControl(
  method = "repeatedcv", number = 10,
  repeats = 5,
  verboseIter = TRUE
)

# 1. Linear model using PCA as preprocessing method:

set.seed(123)
lm.model<-train(Log.Price~.,  data = train ,
                method = "lm", 
                trControl = myControl,
                preProcess=c("zv", "nzv", "center", "scale", "pca"))


lm.model$preProcess
lm.model$results #RMSE = 0.11177 Rsquared=0.9063358 

# Elastic net Regression.
set.seed(123)
net <-train(Log.Price~.,  data = train, 
                     method = "glmnet",
                     metric="RMSE",
                     tuneGrid= expand.grid(alpha = seq(0,1, length=20), 
                     lambda = seq(0.0001, 0.01, length = 10)),
                     trControl=myControl,preProcess=c("zv", "nzv", "center",
                                           "scale"))

# The optimal hyperparameters that minimize RMSE are alpha = 0.2631579 and lambda = 0.0023.
plot(net) 

# Final results:

arrange(net$results, RMSE) %>% head(1)

 
# 5. Simple random forest:

set.seed(123)                            
model_rf = train(Log.Price~.,  data = train,
                 metric="RMSE",
                 method = "ranger",
                 tuneGrid= expand.grid(mtry = 62, splitrule = "variance", min.node.size = 5),
                 trControl = myControl)

# Final results:

arrange(model_rf$results, RMSE) %>% head(1)

# Model 6 - stochastic gradient boosting machine

gbmTuningGrid = expand.grid(interaction.depth = c(7), 
                            n.trees = c(209:219), 
                            shrinkage = c(0.1),
                            n.minobsinnode = c(22))

set.seed(123)
model_gbm = train(Log.Price~.,  data = train,
                  metric = "RMSE",
                  method = "gbm",
                  trControl = myControl,
                  tuneGrid=gbmTuningGrid,
                  preProcess=c("zv", "nzv"))



#Final results:
arrange(model_gbm$results, RMSE) %>% head(1) # RNSE = 0.1007012 Rsquared = 0.9240502
plot(model_gbm)

# Comparing and choosing the model.

model_list <- list( LM_model =lm.model, Elastic_net_model = net, Random_f =model_rf,  GBM = model_gbm)

# Passing model_list to resamples(): 

resamples <- resamples(model_list)

# Summarize the results

print(summary(resamples))

# Visualize the results:

bwplot(resamples, metric = "RMSE")

##### 7. Predicting. #####

test$net_pred<- predict(net, test)
test$error_net <- test$net_pred -log(test$SalePrice)
RMSE_net<-sqrt(mean(test$error_net ^2))
print(RMSE_net) #  0.1024034

# Interpretting the logged RMSE:

test$exp_net<-exp(test$net_pred)
test$error_net_exp <- test$exp_net - test$SalePrice
RMSE_net_exp<-sqrt(mean(test$error_net_exp  ^2))
print(RMSE_net_exp) # 18100.87

# Visualizing the results:

net_fit<-ggplot(test, aes(x = exp_net, y = SalePrice, label = RMSE_gbm)) + 
  geom_point() +
  ggtitle(label="Elastic Net Model Predictions vs Outcomes")+ 
  scale_y_continuous(name = "Actual values",labels = function(y) paste0(y / 1000, "k"))+
  scale_x_continuous(name = "Predictions",labels = function(x) paste0(x / 1000, "k"))+
  geom_abline(color = "blue")
print(net_fit)


res_net_fit<-ggplot(test, aes(x = SalePrice  , y =error_net_exp, label = RMSE_gbm)) + 
  geom_point() +
  ggtitle(label="Elastic Net Model Residuals vs Outcomes")+ 
  scale_y_continuous(name = "Actual values",labels = function(y) paste0(y / 1000, "k"))+
  scale_x_continuous(name = "Residuals",labels = function(x) paste0(x / 1000, "k"))
 
print(res_net_fit)
