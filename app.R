library("shiny")
library("rsconnect")
library("viridis")

ui <- source("ui_spielkram.R")
server <- source("server_spielkram.R")

shinyApp(ui, server)
