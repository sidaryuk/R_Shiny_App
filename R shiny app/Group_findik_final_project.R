#Ezgi Cantürk 2428944
#Leylanur Demirturan 2429009
#Esen Ünal 2429371
#Sidar Yük 2291052
#Ýlayda Özcan 2429207
library(shinythemes)
library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(recipes)
library(bslib)
library(ggplot2)
getwd()
data2015 <- read.csv("2015.csv")
dataset <- read.csv("2015.csv")
happy2015 <- read.csv("2015.csv")
head(happy2015)
str(happy2015)
AttributeChoices = c("Happiness.Score", "Standard.Error", "Economy..GDP.per.Capita.",
                     "Family", "Health..Life.Expectancy.", "Freedom", "Trust..Government.Corruption.",
                     "Generosity", "Dystopia.Residual")


ui = fluidPage(
  theme = shinytheme("united"),
  setBackgroundImage(
    src = "https://img.rawpixel.com/s3fs-private/rawpixel_images/website_content/rm255-sasi-29.jpg?w=800&dpr=1&fit=default&crop=default&q=65&vib=3&con=3&usm=15&bg=F4F4F3&ixlib=js-2.2.1&s=4310027eb6ccf4f849b251cafecdff94"),
  titlePanel("Group Fýndýk - Final Project 292"),
  
  navbarPage("R Shiny Dashboard",
             tabsetPanel(
               tabPanel("Welcome",
                        tabName = "welcome",
                        icon=icon("door-open"),
                        
                        fluidPage(
                          h1("Welcome to Happiness Shiny App Dashboard!"),
                          br(),
                          p(strong(tags$u("What is this dashboard all about?"))),
                          p("Aim of this app is to show, clearly and directly relevant information about happiness score . To achieve this, we examined the subjects in terms of data visualization and statistical significance."),  
                          br(),
                          p(strong(tags$u("How can I use this dashboard?"))),
                          p("You can click on any of the tabs above to see a different analysis of the data."),
                          br(),
                          p(strong(tags$u("Prepared by: "))),
                          h6("Ezgi Cantürk 2428944"),
                          h6("Sidar Yük 2291052"),
                          h6("Leylanur Demirturan 2429009"),
                          h6("Esen Ünal 2429371"),
                          h6("Ýlayda Özcan 2429207"), 
                          br(),
                          img(src = "https://freesvg.org/img/1522766566.png",
                              width = "500px", height = "500px"),
                        )),
               
               tabPanel("Summary",
                        tabName = "Summary",
                        p(strong(tags$u("Information about our dataset: "))),
                        p("This dataset is taken from dataworld. It gives several variables that related to happiness score."),
                        p("Here is a ",a("LINK",href="https://data.world/juiche/happiness/workspace/file?filename=2015.csv"),"that you can find our dataset."),
                        br(),
                        p("       The data contains 158 observations of 12 variables. 2 of them are characters which are region and country. 
                          The other 10 are numeric variables such as happiness rank, happiness score, standard error, economy 
                          (in means of GDP per capita), family, health (in means of life expectancy), freedom, trust, government corruption, 
                          generosity, and dystopia residual. Happiness rank is the numerical collocation of each country. Happiness score is a 
                          value that is calculated between 1 and 10 and attains every country a score according to the dependents' values. Family,
                          which denotes familyization and healthy life expectancy, are mathematically calculated values between 0 and 2. Freedom 
                          is the country's freedom level calculated by other variants, trust value which stands for the ratio of the trust to the 
                          government, and generosity levels are calculated between values 0 and 1."), 
                        
                        fluidPage(
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("dataset", "Choose a dataset:",
                                          choices = c(colnames(data2015))),
                              numericInput("obs", "Number of observations to view:", 150),
                              actionButton("update", "Update view")
                              
                            ),
                            
                            mainPanel(
                              h4("Summary"),
                              verbatimTextOutput("Summary"),
                              
                              h4("Observations"),
                              tableOutput("view")
                            )
                          )
                        )),
               
               
               tabPanel("Histogram",
                        tabName="Histogram",
                        
                        
                        fluidPage(
                          h1("Happiness Rate by Region"),
                          sidebarPanel(
                            selectInput("valgtland", h3("Region"),
                                        choices = unique(data2015$Region),
                                        selected = "Western Europe")),
                          mainPanel(
                            plotOutput("salgplot")
                          )
                        )),
               
               tabPanel("Plot",
                        tabName="Plot",
                        
                        
                        fluidRow(
                          sidebarPanel(
                            sliderInput('sampleSize', 'Sample Size', 
                                        min=1, max=nrow(dataset),
                                        value=min(1, nrow(dataset)), 
                                        step=1, round=0),
                            br(),
                            
                            checkboxInput('smooth', 'Smooth'),
                            
                            selectInput('x', 'X', names(dataset)),
                            
                            selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
                            
                          ),
                          mainPanel(
                            plotOutput("scatterplot")
                          )
                        )
               ),
               
               tabPanel("Regression",
                        tabname="regression",
                        verbatimTextOutput("summary"),
                        selectInput(inputId="dependent", label = "Dependent Variables",
                                    choices = as.list(AttributeChoices)),
                        uiOutput("indep"),
                        verbatimTextOutput(outputId = "RegOut")),
               
               
               tabPanel("References", 
                        p(),
                        p("Here is a", a("LINK",  href="https://data.world/juiche/happiness/workspace/file?filename=2015.csv"), 
                          "that you can find our data taken"),
                        p("Here is a", a("LINK",  href= "https://freesvg.org/img/1522766566.png"), 
                          "that you can find our welcome page image taken"))
               
               
             )
  ))



server <- function(input, output) {
  
  
  
  
  
  output$indep <- renderUI({
    selectInput(inputId = "indep", label = "Independent Variables", 
                multiple = TRUE, choices = as.list(AttributeChoices[AttributeChoices!= input$dependent]), selected = AttributeChoices[1])
  })
  
  
  
  recipe_formula <- reactive({
    req(input$indep)
    happy2015 %>%
      recipe() %>%
      update_role(!!!input$dependent, new_role = "outcome") %>%
      update_role(!!!input$indep, new_role = "predictor") %>%
      prep() %>% 
      formula()
  })
  
  lm_reg <- reactive(
    lm(recipe_formula(),data = happy2015)
  )
  
  
  
  
  output$RegOut = renderPrint({
    summary(lm_reg())
  })
  
  dataset <- reactive({
    data2015[sample(nrow(data2015), input$sampleSize),]
  })
  
  
  output$scatterplot<- renderPlot({
    
    p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    
    
    
    if (input$smooth)
      p <- p + geom_smooth(method = lm, color="red")
    
    print(p)
    
  })
  output$selected_var <- renderText({
    paste("You have selected", input$valgtLand)
  })
  
  # Fill in the spot we created for a plot
  output$salgplot <- renderPlot({
    # Render a barplot
    dplyr::filter(data2015, Region == input$valgtland) %>%
      ggplot(., aes(x=Country, y=Happiness.Score)) + 
      geom_bar(stat="identity", color="black", fill="lightblue", size=0.5) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  datasetInput <- eventReactive(input$update, {
    switch(input$dataset,
           "Country"= data2015$Country,
           "Region"=data2015$Region,
           "Happiness.Score"=data2015$Happiness.Score,
           "Economy..GDP.per.Capita."=data2015$Economy..GDP.per.Capita.,
           "Family"=data2015$Family,
           "Health..Life.Expectancy"=data2015$Health..Life.Expectancy.,
           "Trust..Government.Corruption."=data2015$Trust..Government.Corruption.,
           "Freedom"=data2015$Freedom,
           "Dystopia.Residual"=data2015$Dystopia.Residual,
           "Generosity" = data2015$Generosity,
           "Happiness.Rank" = data2015$Happiness.Rank,
           "Standard.Error" = data2015$Standard.Error)
  }, ignoreNULL = FALSE)
  
  # Generate a summary of the dataset ----
  output$Summary <- renderPrint({
    summary( datasetInput())
  })
  
  # Show the first "n" observations ----
  # The use of isolate() is necessary because we don't want the table
  # to update whenever input$obs changes (only when the user clicks
  # the action button)
  output$view <- renderTable({
    head(datasetInput(), n = isolate(input$obs))
  })
  
}




shinyApp(ui = ui, server = server)
