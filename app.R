library(shiny)
library(shinydashboard)
library(catboost)

model <- catboost.load_model("model/heartspot")

restecg.result1 = 'normal'
restecg.result2 = 'having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV'
restecg.result3 = 'showing probable or definite left ventricular hypertrophy by Estes\'\ criteria'

slope.result1 = 'upsloping'
slope.result2 = 'flat'
slope.result3 = 'downsloping'

thal.result1 = 'normal'
thal.result2 = 'fixed defect'
thal.result3 = 'reversible defect'

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
              HTML("<p style='text-align: center; color:gray; font-weight: bold; font-style: italic;'>AI-powered Heart Disease prediction tool</p>")
            )
          ),
          column(width = 6,
            box(width = NULL, status = "danger",
              h3("Description"),
              HTML("<p align='justify'>HeartSpot is an AI-powered web application built to predict the presence of Heart Disease in a patient. Primarily targeted at health care professionals, it is accessible for 
              everybody that is interested.</p>"),
              h3("Usage"),
              p(align = "justify", "By navigating to the prediction tab (left of this page), you will be able to enter the variables this tool needs to calculate its prediciton. In total, this tool needs information about 13 different
                                   parameters. When all of them are entered, press the Apply button to get the prediction."),
              h3("Background"),
              HTML("<p align='justify'>This system was built during our time as Health Informatics students at Karolinska Institutet in Stockholm and was part of a course project. If you are interested 
                   in our source code, want to contribute or would like to get in touch with us, please have a look at our <a href='https://github.com/Afsharov/heartspot'>GitHub</a> repository.</p>")
            )
          )
        ),
        fluidRow(
          column(width = 12,
            box(width = NULL, status = "danger",
              h3("For developers and those interested"),
              HTML(sprintf("<p align='justify'>The system uses a <a href='https://catboost.ai/docs/'>CatBoost</a> classification model and used the <a href='http://archive.ics.uci.edu/ml/datasets/Heart+Disease'>UCI Heart Disease dataset</a> to 
                   train and test this system. Originally, the database contains 76 attributes, but all published experiments refer to using only a subset of 14 of them and only one of the 4 available databases. We have however opted 
                   to combine all 4 databases and obtained a total of 920 observations with 13 predictor variables and 1 target variable. As most previous experiments attempted, we also only distinguish between presence (1) and absence
                   (0) of heart disease. The original database does allow for further distinction of the severity of heart disease. But the resulting multi-classification task demonstrated as a problem with no satisfactory solution. 
                   The binary problem on the other hand was succesfully modeled with an accuracy of 0.83 and an Area Under the Curve (AUC) of the Receiver Operating Characteristic (ROC) of 0.91 (see below).</p>
                   <img src='roc_curve.png' style='display: block; margin-left: auto; margin-right: auto; width: 80%%;'/>
                   <br>
                   <p align='justify'>That being said, the nature of this system is experimental and the result has to be taken with care. The full source code is available at <a href='https://github.com/Afsharov/heartspot'>GitHub</a> 
                   and contributors are always very welcome.</p>
                   "))
            )
          )
        )
      ),
      tabItem(tabName = "prediction",
        fluidRow(
          column(width = 12, h2("Data Input"))
        ),
        fluidRow(
          column(width = 4,
            box(width = NULL, title = "Demographics", status = "danger",
              numericInput('Age', 'Age','25', width = "50%"),  
              selectInput('Sex', 'Gender',  c('male','female'), width = "50%")
            ),
            box(width = NULL, title = "Symptoms", status = "danger",
              selectInput('CP', 'Chest Pain',  c('typical angina','atypical angina', 'non-anginal pain', 'asymptomatic'), width = "75%"),
              selectInput('exang', 'Exercise-induced angina', c('no','yes'), width = "75%")
            )
          ),
          column(width=4,
            box(width = NULL, title = "Lab results", status = "danger",
               numericInput('chol', 'Serum cholesterol in mg/dl','200', width = "75%"),
               selectInput('fbs', 'Fasting blood glucose >120mg/dl', c('no','yes'), width = "75%")
            ),
            box(width = NULL, title = "Clinical results", status = "danger",
               sliderInput('ca', 'Number of major vessels colored by fluoroscopy', 0, 3, 0, 1),
               selectInput('thal', 'Thalium stress test', c(thal.result1, thal.result2, thal.result3), width = "50%")
            )
          ),
          column(width=4,
            box(width = NULL, title = "ECG", status = "danger",
               sliderInput('oldpeak', 'ST depression',0,5,3.0,0.2),
               numericInput('trestbps', 'Resting blood pressure in mmHg','120', width = "75%"),
               numericInput('thalach', 'Maximum heart rate achieved per minute','180', width = "75%"),
               selectInput('restecg', 'Resting electrocardiographic results',  c(restecg.result1, restecg.result2, restecg.result3), width = "75%"),
               selectInput('slope', 'Slope of the peak exercise ST segment',  c(slope.result1, slope.result2, slope.result3), width = "75%")
            )
          )
        ),
        fluidRow(
          column(width = 12,
            actionButton(inputId="button", label="Apply to input"),
            h2("Interpretation"),
            box(width = NULL, status = "danger",
               htmlOutput("result")
            )
          )
        )
      )
    ),
    tags$script("document.getElementById('button').addEventListener('click', function(){
                alert('Disclaimer: We cannot be held accountable for the accuracy or correctness of the following prediction and any actions undertaken because of the prediction are on your own responsibility.');
    });")
  )
)

server <- function(input, output) {

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
    if (input$restecg == restecg.result1){
      restecg <- 0
    } else if(input$restecg == restecg.result2){
      restecg <- 1
    } else if(input$restecg == restecg.result3){
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
  
  slope <- reactive({
    if (input$slope == slope.result1){
      slope <- 1
    } else if(input$slope == slope.result2){
      slope <- 2
    } else if(input$slope == slope.result3){
      slope <- 3
    }
  })
  
  thal <- reactive({
    if (input$thal == thal.result1){
      thal <- 3
    } else if(input$thal == thal.result2){
      thal <- 6
    } else if(input$thal == thal.result3){
      thal <- 7
    }
  })
  
  observeEvent(input$button, {
    
    userfeatures <- data.frame(age = as.integer(input$Age), 
                               sex = as.factor(sex()), 
                               cp = as.factor(cp()), 
                               trestbps = input$trestbps, 
                               chol = input$chol, 
                               fbs = as.factor(fbs()), 
                               restecg = as.factor(restecg()), 
                               thalach = input$thalach, 
                               exang = as.factor(exang()), 
                               oldpeak = input$oldpeak,
                               slope = as.factor(slope()),
                               ca = input$ca,
                               thal = as.factor(thal())
                               )
    
    pool <- catboost.load_pool(userfeatures, cat_features = c(1, 2, 5, 6, 8, 10, 12))
    pred <- catboost.predict(model, pool, prediction_type = "Class", ntree_end = 61)
    
    output$result <- renderUI({ 
      
      result <- ""
      color <- ""
      if (pred == 1) {
        result <- "high"
        color <- "red"
      } else{
        result <- "low"
        color <- "blue"
      }
      HTML(sprintf("
           <p align='justify'>Based on your input, this patient is at a <span style='color:%s; font-weight:bold;'>%s</span> 
           risk of having or developing Heart Disease. The prediction is based on the 13 variable inputs you have provided. 
           We provide the table below to make it transparent how the variables have been used in by the prediction model.</p>
           <img src='features.png' style='display: block; margin-left: auto; margin-right: auto; width: 80%%;'/>
           ", color, result)
      )
    })
  })
}

shinyApp(ui = ui, server = server)
