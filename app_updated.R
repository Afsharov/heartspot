library(shiny)
library(shinydashboard)

train_data_01 <- read.csv2(file="/Users/abhinavsharma/datasets/new_data_01.csv",sep = ",") 
train_data_01$class <- as.factor(train_data_01$class)

set.seed(3)
tree.train_01 = tree(as.factor(train_data_01$class)~., train_data_01)

#normalize <- function(x) {
#  return ((x - min(x)) / (max(x) - min(x)))}

# Define UI (fluidPage is a flexible default choice) ----
ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "HeartSpot"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Information", tabName = "info", icon = icon("info")),
                        menuItem("Prediction", tabName = "prediction", icon = icon("heartbeat"))
                      )
                    ),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      ),
                      tabItems(
                        tabItem(tabName = "info",
                                column(width = 6,
                                       box(width = NULL, status = "warning",
                                           HTML("<h1 style='text-align: center; color:red; font-weight: bold;'>HeartSpot</h1>"),
                                           HTML("<img src='image4.png' style='display: block; margin-left: auto; margin-right: auto; width: 80%;'/>"),
                                           HTML("<p style='text-align: center; color:gray; font-weight: bold; font-style: italic;'>AI-powered Heart Disease prediction tool</p>")
                                       )
                                )
                        ),
                        tabItem(tabName = "prediction",
                                fluidRow(
                                  column(width = 8,
                                         h2("Prediciton"),
                                         box(width = NULL, status = "danger",
                                             plotOutput('plot1'),
                                             submitButton(text="submit", icon=NULL)
                                         ),
                                         h2("Interpretation"),
                                         box(width = NULL, status = "danger",
                                             textOutput("text")
                                         )
                                  ),
                                  column(width = 4,
                                         h2("Data Input"),
                                         box(width = NULL, title = "Demographics", status = "danger",
                                             numericInput('Age', 'Age','25', width = "50%"),  
                                             selectInput('Sex', 'Gender',  c('male','female', 'unknown'), width = "50%")
                                         ),
                                         box(width = NULL, title = "Symptoms", status = "danger",
                                             selectInput('CP', 'Chest Pain',  c('typical angina','atypical angina', 'non-anginal pain', 'asymptomatic'), width = "75%"),
                                             selectInput('exang', 'Exercise-induced angina', c('no','yes'), width = "75%")
                                         ),
                                         box(width = NULL, title = "ECG", status = "danger",
                                             sliderInput('oldpeak', 'ST depression',0,5,3.0,0.2),
                                             numericInput('trestbps', 'Resting blood pressure in mmHg','80', width = "75%"),
                                             numericInput('thalach', 'Maximum heart rate achieved per minute','180', width = "75%"),
                                             selectInput('restecg', 'Resting electrocardiographic results',  c('normal','having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV', 'showing probable or definite left ventricular hypertrophy by Estes\'\ criteria'), width = "75%")
                                         ),
                                         box(width = NULL, title = "Lab results", status = "danger",
                                             sliderInput('chol', 'Serum cholesterol in mg/dl',0,20,3.5,0.2),
                                             selectInput('fbs', 'Fasting blood glucose >120mg/dl', c('no','yes'), width = "50%")
                                         )
                                  )
                                )
                        )
                      )
                    )
)

# Define server logic ----
server <- function(input, output) {
  # Combine the selected variables into a new data frame
  
  sex <- reactive({
    if (input$Sex == 'male'){
      sex <- 1
    } else if(input$Sex == 'female'){
      sex <- 0
    } 
  })
  cp <- reactive({
    if (input$CP == 'typical angina'){
      cp <- 1
    } else if(input$CP == 'atypical angina'){
      cp <- 2
    } else if(input$CP == 'non-anginal pain'){
      cp <- 3
    } else if(input$CP == 'asymptomatic'){
      cp <- 4
    }
  })
  exang <- reactive({
    if (input$exang == 'no'){
      exang <- 0
    } else if(input$exang == 'yes'){
      exang <- 1
    } 
  })
  restecg <- reactive({
    if (input$restecg == 'normal'){
      restecg <- 0
    } else if(input$restecg == 'having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV'){
      restecg <- 1
    } else if(input$restecg == 'showing probable or definite left ventricular hypertrophy by Estes\'\ criteria'){
      restecg <- 2
    }
  })
  fbs <- reactive({
    if (input$fbs == 'no'){
      fbs <- 0
    } else if(input$fbs == 'yes'){
      fbs <- 1
    } 
  })
  userfeatures <- reactive({
    test_data <- data.frame(age=as.integer(input$Age), sex =
                            sex(), cp = cp(), trestbps = input$trestbps, chol = input$chol, fbs = fbs(), restecg = input$restecg, 
                            thalach = input$thalach, exang = exang(), oldpeak = input$restecg)
  })
  pred <- reactive({
    predict(tree.train_01, test_data, type="class")
    
    #execute kmeans here(need data, and number of clusters as parameters) 
    # k_means(datanorm, input$oldpeak)
  })
  
  output$plot1 <- renderPlot({ #plot output
    plot(tree.train_01)
    text(tree.train_01 ,pretty =0)
    
    # plot(datanorm, col = clusters()$cluster, pch = 16, cex =1)
    # points(clusters()$centers, pch = 10, cex = 3, lwd = 4,col = 'purple')
  })
  
  output$text <- renderText({ 
    if (pred() ==1){
      "This patient is at a HIGH risk of having or developing IHD"
    } else{
      "This patient is at a LOW risk of having or developing IHD"
    }
    
     
  })
}
# Run the app ----
shinyApp(ui = ui, server = server)

