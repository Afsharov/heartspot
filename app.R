library(shiny)
library(shinydashboard)

trainingdata <- read.csv(file="datasets/training_data.csv",sep = ",") 

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))}

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
        fluidRow(
          column(width = 6,
            box(width = NULL, status = "danger",
               HTML("<h1 style='text-align: center; color:red; font-weight: bold;'>HeartSpot</h1>"),
               HTML("<img src='image4.png' style='display: block; margin-left: auto; margin-right: auto; width: 80%;'/>"),
               p(style = "text-align: center; color:gray; font-weight: bold; font-family: 'Roboto Slab'; font-style: italic;", "AI-powered Heart Disease prediction tool")
            )
          ),
          column(width = 6,
            box(width = NULL, status = "danger",
               h3("Description"),
               HTML("<p align='justify'>The system you have accessed is an AI-powered web application build for health care professionals to predict the presence of Heart Disease 
                 in a patient. We have implemented a Decision Tree model and used the <a href='http://archive.ics.uci.edu/ml/datasets/Heart+Disease'>UCI Heart Disease dataset</a> to train and test this system.</p>"),
               h3("Usage"),
               p(align = "justify", "By navigating to the prediction tab, you will be able to enter the variables this tool needs to calculate its prediciton. In total, this tool needs information about 10 different
                 parameters. When all of them are entered, press the submit button to get the prediction."),
               h3("Background"),
               HTML("<p align='justify'>We are Health Informatics students from Karolinska Institutet in Stockholm who have build this application as part of a course project. If you are interested 
                 in our source code or would like to get in touch with us, please have a look at our <a href='https://github.com'>GitHub</a> repository.</p>")
            )
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
               textInput('Age', 'Age','25', width = "50%"),  
               selectInput('Sex', 'Gender',  c('male','female', 'unknown'), width = "50%")
            ),
            box(width = NULL, title = "Symptoms", status = "danger",
               selectInput('CP', 'Chest Pain',  c('typical angina','atypical angina', 'non-anginal pain', 'asymptomatic'), width = "75%"),
               selectInput('exang', 'Exercise-induced angina', c('no','yes'), width = "75%")
            ),
            box(width = NULL, title = "ECG", status = "danger",
               sliderInput('oldpeak', 'ST depression',0,5,3.0,0.2),
               textInput('threstbps', 'Resting blood pressure in mmHg','80', width = "75%"),
               textInput('thalach', 'Maximum heart rate achieved per minute','180', width = "75%"),
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
  clusters <- reactive({
    #execute kmeans here(need data, and number of clusters as parameters) 
    # k_means(datanorm, input$oldpeak)
  })
  
  output$plot1 <- renderPlot({ #plot output
    # plot(datanorm, col = clusters()$cluster, pch = 16, cex =1)
    # points(clusters()$centers, pch = 10, cex = 3, lwd = 4,col = 'purple')
  })
  
  output$text <- renderText({ 
    "This patient is at a HIGH risk of having or developing IHD" 
    })
}
# Run the app ----
shinyApp(ui = ui, server = server)