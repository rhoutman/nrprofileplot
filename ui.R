library(shiny)
library(shinyWidgets)
library(plotly)

shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  
  # Application title
  titlePanel("Plot your data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width=2,
                 tabsetPanel(type = "tabs",
                             tabPanel("Data", 
                                      uiOutput("colorlist"),
                                      uiOutput("colfactselect"),
                                      uiOutput("selectgraph")
                                      ),
                             tabPanel("format", 
                                      checkboxInput("origin", label = "force origin y-axis", value = F),
                                      checkboxInput("point", label = "draw points", value = F),
                                      sliderInput("pointsize", label ="point size", min = 0, 
                                                  max = 10, value = 2, step= 0.1),
                                      checkboxInput("line", label = "draw line", value = T),
                                      sliderInput("linesize", label = "line size", min = 0, 
                                                  max = 3, value = 0.5, step=0.1),
                                      selectInput("colorscheme", label = "color scheme",
                                                  choices = list("divergent", "continuous"),
                                                  selected = 1)
                             ),
                             tabPanel("error bars", 
                                      checkboxInput("errorbars", label = "error bars", value = F),
                                      selectInput("errorbarannot", label = "parameter for errorbars", 
                                                  choices = list("error"),
                                                  selected = 1)
                             ),
                             tabPanel("significance", 
                                      checkboxInput("displayp", label = "indicate significance", value = F),
                                      checkboxInput("antagsignificance", label = "smart significance", value = F),
                                      checkboxInput("rotask", label = "rotate asterisks", value = F),
                                      sliderInput("astsize", label = "asterisk size", min = 0, 
                                                  max = 10, value =2, step=0.1),
                                      sliderInput("astoffset", label = "asterisk offset", min = 0, 
                                                  max = 3, value = 1.1, step=0.1)
                                      ),
                             tabPanel("legend", 
                                      textInput("legtit", "legend.title", ""),
                                      selectInput("leglabdig", label ="legend label digits",
                                                  choices = as.list(c(1:4)), selected = 1),
                                      sliderInput("legx", label = "horizontal legend posiition", min = -0.2, 
                                                  max = 1.3, value =0.5, step=0.02),
                                      sliderInput("legy", label = "vertical legend posiition", min = -0.2, 
                                                  max = 1.3, value =0.95, step=0.02),
                                      selectInput("legopts", label = "legend position",
                                                  choices =list("horizontal", "hide", "separate"), selected = 1)
                             ),
                             tabPanel("titles", 
                                      textInput("maintit", "main title", ""),
                                      textInput("ytit", "y-axis title", ""),
                                      textInput("xtit", "x-axis title", "")
                                      ),
                             tabPanel("facetting", 
                                      selectInput("facetx", label = "horizontal facets",
                                                  choices =list("none"), selected = 1),
                                      selectInput("facety", label = "vertical facets",
                                                  choices =list("none"), selected = 1)
                             ),
                             tabPanel("scales", 
                                      selectInput("yscale", label = "y-scale",
                                                  choices =list("global", "free", "manually"), selected = 1),
                                     uiOutput("yrange"),
                                      numericInput("manymin", label = "manual y-min", value =NULL),
                                      numericInput("manymax", label = "manual y-max", value = NULL)
                             )
                             )   



# checkboxInput("transparancy", label = "add transparency", value = F),
# selectInput("transpannot", label = h3("parameter for transparency"), 
            # choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
            # selected = 1),












# list("significance annotation", "p"),



# list("fig. width", 10.2),  
# list("fig. height", 3.5),

# list("file type", list(".png", ".pdf")),
# list("use for report", list("N", "Y")),
# list("name report folder", "report"),
# list("plotfolder", "profiles"),
# list("add to filename", ""),
# list("data split factor 1",""),
# list("data split factor 2",""),
# list("custom order 1 for factor",""),
# list("custom order 1","" ), 
# list("custom order 2 for factor",""),
# list("custom order 2","" )    


    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("rnplot"),
      dataTableOutput("test"),
      # verbatimTextOutput(("test2")),
      shinyjs::hidden(p(id = "runStatus", "Processing..."))
  
      
    )
  )
))

