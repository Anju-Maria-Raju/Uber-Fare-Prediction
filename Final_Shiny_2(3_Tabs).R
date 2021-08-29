#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#Loading Required Packages
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shiny)
library(ggplot2)
library(shinydashboard)
library(recipes)
library(DT)
library(shinyjs)
library(shinyBS)
library(grid)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#Loading Data
allsample <- read.csv("C:/lyftuber/allsample.csv", header = TRUE)
allsample <- allsample[,-c(3,8,9,11)]
allsample <- allsample[,-c(9)]
pvttable <- read.csv("C:/lyftuber/allsamplepivot.csv", header = TRUE)
allsample$lograin <- as.factor(allsample$lograin)
allsample$name <- as.factor(allsample$name)
allsample$source <- as.factor(allsample$source)
allsample$weekday <- as.factor(allsample$weekday)
allsample$hourtime <- as.factor(allsample$hourtime)
allsample$cab_type <- as.factor(allsample$cab_type)
allsample$destination <- as.factor(allsample$destination)
price_by_sm <- allsample$price/allsample$surge_multiplier
allsample <- cbind(allsample, price_by_sm)
colms <- colnames(allsample) 
numvar <- colms[c(1,8,9,10,11,12,13)]
factvar <- colms[c(2,3,4,6,7,14,15,16)]
colms
  
Udata <- allsample[which(allsample$cab_type == 'Uber'),]
Ldata <- allsample[which(allsample$cab_type == 'Lyft'),]
a = count(Udata)
b = count(Ldata)

data <- data.frame(
  y <- c(a,b),
  x <- c('Uber', 'Lyft')
)

AttributeChoices=c("distance","surge_multiplier","clouds",
                   "pressure","rain","humidity","wind","name",
                   "hourtime","weekday")


# Define UI for the application 
ui <- fluidPage( 
  useShinyjs(),
  theme=shinytheme("lumen"),
  tags$head(),
  dashboardPage(
  dashboardHeader(title = "FARE PREDICTION  ", titleWidth = 800),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "Data"),
      menuItem("Graphs", tabName = "plots1"),
      menuItem("Model", tabName = "model"),
      menuItem("Regression", tabName = "Regression")
      
    )),
             
  dashboardBody(
    # include the CSS file
    tags$head(),
    tabItems(
             #Data
             tabItem("Data", tabname="Data",
                sidebarLayout(
                  sidebarPanel(
                    pickerInput("var", "Display Options",
                                       c("Dataset" = "dataset",
                                         "Summary" = "summary"),
                                multiple="FALSE")
                    
                    
                  ),
                  
              #------------------------------------------------------------
              #TAB 1 DATA!!!
              #------------------------------------------------------------                  
                  
                mainPanel(id = "Main",
                  
                          bsButton("showpanel", "Show/hide sidebar", 
                                   type = "toggle", value = TRUE),
                  
                  conditionalPanel( condition = "input.var == 'dataset'",
               h2("The Data Set"),
              DT::dataTableOutput("mytable")),
              
                  conditionalPanel( condition = "input.var == 'summary'",
                                    verbatimTextOutput(outputId = "sum"))
              
              
                ))
             ),
             
             #------------------------------------------------------------
             #TAB 1 PLOTS!!!
             #------------------------------------------------------------
             # Show a plot of the generated distribution
             tabItem("plots1",tabname="plots1",
                     
                     sidebarLayout(
                       sidebarPanel(
                         pickerInput("var2", "Display Options",
                                     c("Time and Day" = "graph1",
                                       "Price v/s Distance" = "graph2",
                                       "Lyft and Uber" = "graph3"),
                                     multiple="FALSE")
                         
                         
                       ),
                      #icon=icon("bar-chart-o"),
                      #infoBox("Cabs",30000,icon=icon("thumbs-up")),
                      #infoBox("rides%", paste0('20%'),icon=icon("warning")),
                      #valueBox(15*200,"price",icon=icon("hourglass-3")),
                      mainPanel(
                        conditionalPanel(condtion="input.var2==graph1",
                                         plotOutput("distPlot1")),
                        conditionalPanel(condtion="input.var2==graph2",
                                         plotOutput("distPlot2")),
                        conditionalPanel(condtion="input.var2==graph3",
                                         plotOutput("distPlot3"))
                      
             ))),
             
             
             #------------------------------------------------------------
             #TAB 2 Hypothesis tests for Model
             #------------------------------------------------------------
            
            
             tabItem(tabname = "model",
                             sidebarLayout(
                               sidebarPanel( pickerInput(inputId = "indep",
                                          label = "Independent Variables", 
                                          multiple = TRUE, 
                                          choices = as.list(AttributeChoices), 
                                          selected = AttributeChoices[1])),
                               mainPanel(
                              verbatimTextOutput(outputId = "RegOut"))
                              
                              )),
             
             
             
             #------------------------------------------------------------
             #TAB 3 Price Predictor
             #------------------------------------------------------------
             tabItem("Regression", tabname = "Regression",
            ###____Sidebar with a slider input for number of bins____### 
                      sidebarLayout(
                        sidebarPanel(
                          helpText("A model to predict the fare given, 
                                   distance and weather conditions like
                                   humidity, wind speed etc."),
                          selectInput("name",
                                      "Select Cab Name",
                                      choices=list("Black","Black SUV","Lux",
                                                   "Lux Black","Lux Black XL",
                                                   "Lyft", "Lyft XL","Shared",
                                                   "UberPool","UberX","UberXL",
                                                   "WAV")),
                          sliderInput("surge_multiplier",
                                      "Select Surge Multiplier:",
                                      min = 1,
                                      max = 3,
                                      value = 1,step = 0.1),
                          sliderInput("distance",
                                      "Select Distance:",
                                      min=0,
                                      max=20,
                                      value=0.5,step = 0.1),
                          sliderInput("rain",
                                      "Select Measurement of Rain:",
                                      min=0,
                                      max=10,
                                      value=0,step = 0.1),
                          ),
                        
                        # Show the predicted 
                        mainPanel(
                          tableOutput("PredPrice")
                        )))
             ))
             )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Converting variables into factors
  
  #Toggling side bar
  observeEvent(input$showpanel, {
    
    if(input$showpanel == TRUE) {
      removeCssClass("Main", "col-sm-12")
      addCssClass("Main", "col-sm-8")
      shinyjs::show(id = "Sidebar")
      shinyjs::enable(id = "Sidebar")
    }
    else {
      removeCssClass("Main", "col-sm-8")
      addCssClass("Main", "col-sm-12")
      shinyjs::hide(id = "Sidebar")
    }
  })
  
  #Data
  output$mytable = DT::renderDataTable({allsample},
                        options = list(scrollX=TRUE)
                        )
  #Summary
  output$sum = renderPrint({summary(allsample)})
  
  #-----------------------------------------------------------------------
  #TAB 2 PLOTS!!!
  #-----------------------------------------------------------------------
  #
  p1 <- ggplot(allsample, aes(weekday))+
    geom_bar(color="red")
  

  
  
  output$distPlot1 <- renderPlot({
    ggplot(allsample, aes(hourtime))+
      geom_bar(color="blue")
  }) 
  

  
  #1.Scatter Plot (Distance v/s Price)
  output$distPlot2 <- renderPlot({ 
    ggplot(allsample, aes(x=distance, y=price)) + 
      geom_point(aes(color= factor(cab_type)))
  })
  

  
  #4
  p3 <- ggplot(allsample, aes(y=price,x = distance, color=name))+
    geom_point()+
    facet_wrap(~ cab_type, nrow = 2)
  

  
  output$distPlot3 <- renderPlot({
    ggplot(allsample, aes(y=price_by_sm,x = distance, color=name))+
      geom_point()+
      facet_wrap(~ cab_type, nrow = 2)
  })
  
  
  #-----------------------------------------------------------------------
  #TAB 2 Hypothesis tests for Model
  #-----------------------------------------------------------------------
  recipe_formula <- reactive(allsample %>%
                               recipe() %>%
                               update_role(price,
                                           new_role = "outcome") %>%
                               update_role(!!!input$indep,
                                           new_role = "predictor") %>%
                               formula())
  
  lm_reg <- reactive(
    lm(recipe_formula(),data = allsample)
  )
  
  output$RegOut = renderPrint({summary(lm_reg())})
  
  
  
  #-----------------------------------------------------------------------
  #TAB 3 Price Predictor
  #-----------------------------------------------------------------------
  output$PredPrice <- renderTable({
    Uber_DF <- data.frame(sur = allsample$surge_multiplier,
                          dist = allsample$distance,
                          price1 = allsample$price,
                          name = allsample$name,
                          rain = allsample$rain);
    Uber_DF
    Model_lm <- lm(price1 ~ sur + dist + name + rain, 
                   data=Uber_DF)
    NewPri <- data.frame (sur=input$surge_multiplier, 
                          dist=input$distance, 
                          name=input$name, 
                          rain=input$rain)
    NewPri
    Price_Value <- predict(Model_lm,NewPri)
    Price_Value
    paste("The Fare Paid :",Price_Value)
    
    #})
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
