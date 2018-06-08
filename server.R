if(!require("shiny")){
  install.packages("shiny")
  library("shiny")
}
if(!require("shinydashboard")){
  install.packages("shinydashboard")
  library("shinydashboard")
}

if(!require("dplyr")){
  install.packages("dplyr")
  library("dplyr")
}

data = read.csv("AmesHousing.csv", header=TRUE, sep = ";")

set.seed(123)

split <- sample(nrow(data), floor(0.7*nrow(data)))

train<- data[split,]
test <- data[-split,]

train$House.age <- sapply(train$Year.Built, function(x) 2011 - x)
train$Remod.age <- sapply(train$Year.Remod.Add, function(x) 2011 - x)

train<- train %>% filter(Sale.Condition == "Normal")

imp_data <- train[,c("SalePrice", "Gr.Liv.Area", "Total.Bsmt.SF", "Garage.Area", "Overall.Qual", "Neighborhood", "Lot.Area",    "BsmtFin.SF.1", "Fireplaces","TotRms.AbvGrd","Garage.Cars","House.age", "Remod.age", "Year.Built")]

imp_data$SalePrice <-imp_data$SalePrice /1000
imp_data$Overall.Qual <- as.factor(imp_data$Overall.Qual)

dataset<- imp_data

shinyServer(function(input, output){
    
  output$distPlot <- renderPlot({
    
    
    hist(imp_data$Year.Built, col = "#75AADB", border = "white",xlab = "Years",
         main = "Year Built", breaks = input$bins)
    
  })
    

    output$histogram <- renderPlot({
      filtered1 <-
        imp_data %>%
        filter(
               Gr.Liv.Area >= input$Gr.Liv.Input[1],
               Gr.Liv.Area <= input$Gr.Liv.Input[2])

       hist( filtered1$SalePrice,  col = "#75AADB", border = "white",xlab = "$000",
           main = "Housing Prices, Ames, Iowa", breaks = input$bin)
    })
      

    
      output$PlotN <- renderPlot({
        ggplot(data = train, aes(x =reorder(Neighborhood, SalePrice, median) , y =SalePrice/100,color=Neighborhood)) +geom_boxplot()+ 
          scale_y_continuous(name = "Price")+ ggtitle("Neighborhood vs Price")+
          
          scale_x_discrete(name="Neighborhoods", labels=c("Mdw","BrDl","IDTR","OldT","SWSU", "BrkSd", "Edwr", "Swyr", "Blst","Lndmk","NAms", "NPKvl",
                                                         "Mtchl", "SwyrW",  "Glbrt","NWAms", "Blmgt",  "ClgCr", "Crwf", "ClrCr",
                                                          "Tmbr","Grns", "Smrst", "Veen",  "StnBr", "NrdgHt", "GrnHll", "NoRd" ))+ theme_classic()

        
      })
    
   
    
    output$coolplot <- renderPlot({
      filtered <-
        imp_data %>%
        filter(Neighborhood == input$NeighbInput,
               Gr.Liv.Area >= input$Gr.Liv.Input[1],
               Gr.Liv.Area <= input$Gr.Liv.Input[2],
               House.age >= input$House.age.Input[1],
               House.age <= input$House.age.Input[2],
               Total.Bsmt.SF >= input$Total.Bsmt.SF.Input[1],
               Total.Bsmt.SF <= input$Total.Bsmt.SF.Input[2],
               Garage.Area >= input$Garage.Area.Input[1],
               Garage.Area <= input$Garage.Area.Input[2],
               Lot.Area >= input$Lot.Area.Input[1],
               Lot.Area <= input$Lot.Area.Input[2],
               TotRms.AbvGrd>= input$TotRms.AbvGrd.Input[1],
               TotRms.AbvGrd <= input$TotRms.AbvGrd.Input[2])
 
      hist(filtered$SalePrice,col = "#75AADB", border = "white", xlab = "SalePrice($000)",
           main = "SalePrice by Neighborhood", breaks = input$buns)

      
      
    })
    
    output$coolplotQ <- renderPlot({
      filtered <-
        imp_data %>%
        filter(Overall.Qual == input$QualInput,
               Gr.Liv.Area >= input$Gr.Liv.Input[1],
               Gr.Liv.Area <= input$Gr.Liv.Input[2],
               House.age >= input$House.age.Input[1],
               House.age <= input$House.age.Input[2],
               Total.Bsmt.SF >= input$Total.Bsmt.SF.Input[1],
               Total.Bsmt.SF <= input$Total.Bsmt.SF.Input[2],
               Garage.Area >= input$Garage.Area.Input[1],
               Garage.Area <= input$Garage.Area.Input[2],
               Lot.Area >= input$Lot.Area.Input[1],
               Lot.Area <= input$Lot.Area.Input[2],
               TotRms.AbvGrd>= input$TotRms.AbvGrd.Input[1],
               TotRms.AbvGrd <= input$TotRms.AbvGrd.Input[2])
      
      hist(filtered$SalePrice,col = "#75AADB", border = "white", xlab = "SalePrice($000)",
           main = "SalePrice vs Quality",  breaks = input$bun)
      
      
      
    })
    
    
   
})



