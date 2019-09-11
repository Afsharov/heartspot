library(shinytest)
library(testthat)

context("Test shiny app")

#open shiny app
app <- ShinyDriver$new('path_to_shiny_app')

#stop shiny app
app$stop()
