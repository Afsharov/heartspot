library(shiny)
library(shinydashboard)

#demo
data <- read.csv(file="processed.cleveland.csv",sep = "") 

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))}

datasub <- data[,c(1,5)]
#replace missing values with the median of attributes
for (i in 1:ncol(datasub)){
  datasub[is.na(datasub[,i]),i]<- median(datasub[,i],na.rm =TRUE)}

datanorm <- as.data.frame(lapply(datasub, normalize))
k_means<-function(x, n=3){
  nclusters <- n #generalize
  observations <- nrow(x)
  set.seed(5) #ע??????????
  group <- sample(1:nclusters, nrow(x), replace = T) #??????
  x.cluster <- cbind(x, group)
  
  #֮???????ݼ???һ????????
  while(TRUE){ 
    centroid <- c()
    distance <- c()
    for(g in 1:nclusters){
      centroid <- c(centroid, mean(x.cluster[x.cluster[, 3] == g, 1]), mean(x.cluster[x.cluster[, 3] == g, 2]))
    }
    centroid <- matrix(centroid, nclusters, 2, byrow = T) 
    
    for(i in 1:observations){ 
      for(j in 1:nclusters){
        dis <- sqrt(sum((x[i,] - centroid[j,])^2))
        distance <- c(distance, dis)
      }
    }
    distance <- matrix(distance, observations, nclusters, byrow= T)
    centroid.label <- apply(distance, 1, which.min) 
    
    if(all(centroid.label == x.cluster[, 3])){
      km.clusters <- centroid.label
      centroid.matrix <- centroid
      break
    }else{
      x.cluster[, 3] <- centroid.label
    } 
  }
  
  #plot(x, col = km.clusters, pch = 19)
  #points(centroid.matrix, pch = 19, col = 4, cex = 2)
  return(list("cluster" = km.clusters, "centers" =centroid.matrix))
}



# Define UI (fluidPage is a flexible default choice) ----
ui <- dashboardPage(
  dashboardHeader(title = "HeartSpot"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Prediction", tabName = "prediction", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    fluidPage(
      splitLayout(cellWidths = c("50%", "50%"),
        fluidPage(
          fluidRow(
            column(6,
                   h4("Symptoms & History:"),
                   selectInput('CP', 'Chest Pain',  c('typical angina','atypical angina', 'non-anginal pain', 'asymptomatic'), width = "75%"),
                   selectInput('exang', 'Exercise-induced angina', c('no','yes'), width = "75%"),
                   selectInput('thal', 'Thallium stress test',  c('normal','fixed defect', 'reversible defect'), width = "75%")
            ),
            column(6,
              h4("Demographics:"),
              textInput('Age', 'Age','25', width = "50%"),  
              selectInput('Sex', 'Gender',  c('male','female', 'unknown'), width = "50%")
            )
          ),
          fluidRow(
              h4('ECG & Imaging:'),
              column(6,
               sliderInput('oldpeak', 'ST depression',0,5,3.0,0.2),
               selectInput('slope', 'The slope of the peak exercise ST segment', c('upsloping','flat', 'downsloping'), width = "75%"),
               selectInput('ca', 'Major vessels colored by fluoroscopy', c('0','1', '2','3'), width = "75%")
              ),
              column(6,
               textInput('threstbps', 'Resting blood pressure in mmHg','80', width = "75%"),
               textInput('thalach', 'Maximum heart rate achieved per minute','180', width = "75%"),
               selectInput('restecg', 'Resting electrocardiographic results',  c('normal','having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV', 'showing probable or definite left ventricular hypertrophy by Estes\'\ criteria'), width = "75%")
              )
          ),
          fluidRow(
            h4('Lab results:'),
            fluidRow(
              column(6,
                     sliderInput('chol', 'Serum cholesterol in mg/dl',0,20,3.5,0.2)
              ),
              column(6,
                     selectInput('fbs', 'Fasting blood glucose >120mg/dl', c('no','yes'), width = "50%")
              )
            )
          )
        ),
        verticalLayout(
          h2("Prediciton"),
          submitButton(text="submit", icon=NULL),
          plotOutput('plot1'),
          textOutput("text")
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  clusters <- reactive({
    #execute kmeans here(need data, and number of clusters as parameters) 
    k_means(datanorm, input$oldpeak)
  })
  
  output$plot1 <- renderPlot({ #plot output
    plot(datanorm, col = clusters()$cluster, pch = 16, cex =1)
    points(clusters()$centers, pch = 10, cex = 3, lwd = 4,col = 'purple')
  })
  
  output$text <- renderText({ 
    "This patient is at a HIGH risk of having or developing IHD" 
    })
}
# Run the app ----
shinyApp(ui = ui, server = server)