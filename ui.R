
if(!require("shiny")){
  install.packages("shiny")
  library("shiny")
}
if(!require("shinydashboard")){
  install.packages("shinydashboard")
  library("shinydashboard")
}

if(!require('rsconnect')){
  install.packages('rsconnect')
  library('rsconnect')
}

if(!require("ggplot2")){
  install.packages("ggplot2")
  library("ggplot2")
}

#?icon for more icons Font Awsome icon
shinyUI(
  dashboardPage(
    dashboardHeader(title = "Menu"),
    dashboardSidebar(
      sidebarMenu(
      menuItem("Overall Age", tabName = "age"),
          menuSubItem("Overall Price Distribution", tabName = "dashboard"),
          menuSubItem("Neighborhoods", tabName = "neighb"),
          menuSubItem("Overall Quality", tabName = "qual"),
          menuSubItem("Location vs Area & Age", tabName = "sales")
      )),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "age",
                
                fluidRow(
                  box( status = "primary", 
                      solidHeader = T,  plotOutput("distPlot"), background ="navy",   "There was a construction
                      boom from 1950 up to 1975, then after a drop of construction in the 80's building 
                      houses continued in  the early 90's again. Most properties have been built recently."),
                  box(sliderInput( "bins","Number of bins:",1,50,30))
                  
                  
                )),
        tabItem(tabName = "dashboard",
                h1(),
                fluidRow(
                  box(sliderInput("Gr.Liv.Input", "Gr.Liv.Area", 0, 5700, c(0, 5700))),
                  box( status = "primary",
                      solidHeader = T,  plotOutput("histogram"), background ="navy", "The minimum and the 
                      maximum are $ 39 000 and $ 615 000 respectively. With the median of $ 158 000 50% of 
                      properties lie within price differences of $ 130 000 and $ 205 000."),
                  box(sliderInput( "bin","Number of bins:",1,50,30))
               
                  
                )),
       

        tabItem(tabName = "neighb",
                h2(),
                fluidRow( 
                  box( width = 13, status = "primary", solidHeader = T,  plotOutput("PlotN"), 
                       background ="navy",  "The least expensive neighborhood is Meadow with the 
                       median price below $ 100 000 and the most expensive as well as the most 
                       heterogeneous are Northridge($ 290 000), Green Hills($ 280 000),  
                       Northridge Heights($ 275 000) and Stone Brook($ 260 000).")
                  
                  
                  
                )),
        
        tabItem(tabName = "qual",
                h3("Housing Prices in realation to Area & Age characteristics by Overall Quality"),
                fluidRow(
                  box(radioButtons("QualInput", "Overall.Qual",
                                                     choices = c("1",  "2",  "3",  "4",  "5",  "6",  "7",  "8",  "9",  "10"),
                                                     selected = "6")),
                  box(sliderInput("Gr.Liv.Input", "Gr.Liv.Area", 0, 5700, c(0, 5700))),
                  box(sliderInput("House.age.Input", "House.age", 0, 136, c(0, 136), pre = "years")),
                  box(sliderInput("Total.Bsmt.SF.Input", "Total.Bsmt.SF", 0, 6111, c(0, 6111), pre = "Sq.feet")),
                  box(sliderInput("Garage.Area.Input", "Garage.Area", 0, 1488, c(0, 1488), pre = "Sq.feet")),
                  box(sliderInput("Lot.Area.Input", "Lot.Area", 1299, 216000, c(1299, 216000), pre = "Sq.feet")),
                  box(sliderInput("TotRms.AbvGrd.Input", "TotRms.AbvGrd", 1, 16, c(1, 16))),
                  box( status = "primary", solidHeader = T,  plotOutput("coolplotQ"), background ="navy"),
                  box(sliderInput( "bun","Number of bins:",1,50,30))
                  
                  
                  
                )),
        
        tabItem(tabName = "sales",
                h4("Housing Prices in realation to Area & Age characteristics by Neighborhood"),
                fluidRow(
                  box(selectInput("NeighbInput", "Neighborhood",
                                 c("Blmngtn", "Blueste", "BrDale",  "BrkSide", "ClearCr", 
                                   "CollgCr", "Crawfor" ,"Edwards", "Gilbert","Greens", "GrnHill", 
                                   "IDOTRR",  "Landmrk", "MeadowV", "Mitchel", "NAmes", "NoRidge", 
                                   "NPkVill", "NridgHt", "NWAmes",  "OldTown", "Sawyer",  "SawyerW", 
                                   "Somerst", "StoneBr", "SWISU",   "Timber","Veenker"))),
                  box(sliderInput("Gr.Liv.Input", "Gr.Liv.Area", 0, 5700, c(0, 5700))),
                  box(sliderInput("House.age.Input", "House.age", 0, 136, c(0, 136), pre = "years")),
                  box(sliderInput("Total.Bsmt.SF.Input", "Total.Bsmt.SF", 0, 6111, c(0, 6111), pre = "Sq.feet")),
                  box(sliderInput("Garage.Area.Input", "Garage.Area", 0, 1488, c(0, 1488), pre = "Sq.feet")),
                  box(sliderInput("Lot.Area.Input", "Lot.Area", 1299, 216000, c(1299, 216000), pre = "Sq.feet")),
                  box(sliderInput("TotRms.AbvGrd.Input", "TotRms.AbvGrd", 1, 16, c(1, 16))),
                  box(plotOutput("coolplot"),background ="navy"),
                  box(sliderInput( "buns","Number of bins:",1,50,30))
                  
                )
                
               
        
                 
                
                
                
                ))
                  )
      )) 
    
    

                  
                       


