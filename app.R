#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(readxl)
library(data.table)
library(tidyverse)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),
    tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: grey;  color:black}
    .tabbable > .nav > li > a[data-value='Data 1'] {background-color: OrangeRed;   color:white}
    .tabbable > .nav > li > a[data-value='Data 2'] {background-color: blue;  color:white}
    .tabbable > .nav > li > a[data-value='Dataset 1'] {background-color: OrangeRed;   color:white}
    .tabbable > .nav > li > a[data-value='Dataset 2'] {background-color: blue;  color:white}
    .tabbable > .nav > li[class=active]    > a {background-color: black; color:white}
  ")),

    # Application title
    titlePanel("App to merge data by common column"),
    hr(style = "border-top: 1px solid #000000;"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(
                id = "sidebar",
                
                tabPanel(
                         title = "Data 1",
                         div( style = "color: OrangeRed;",
                    # div( style = "border-style: solid; border-color: green; padding:5px;",
            selectizeInput("file1",
                                "Dataset 1: Select a dataset or click 'Upload' to add your own.",
                           choices = NULL),
            hidden(
            fileInput("upload1", "Upload  File 1")
            ),
            selectizeInput( "mergecol1", 
                            label = "Select Merge ID Column",
                            choices = NULL))),
            tabPanel(
                title = "Data 2",
                div( style = "color: blue;",
                # div( style = "border-style: solid; border-color: blue; padding:5px;",
            selectizeInput("file2",
                           
                           "Dataset 2: Select a dataset or click 'Upload' to add your own.",
                           choices = NULL),
            hidden(
            fileInput("upload2", "Upload File 2")),
            selectizeInput( "mergecol2", 
                            label = "Select Merge ID Column",
                            choices = NULL)))),
            hr(style = "border-top: 1px solid #000000;"),
            selectizeInput( "jointype",
                            label = "Select how to merge tables (see explanatory GIFs below)",
                            choices = c(
                                "Left",
                                "Full",
                                "Right",
                                "Inner",
                                "Anti"
                            )),
            downloadButton("DLButton",
                           "Download Merged Sheet"),
            h2(""),
            uiOutput("merge_explainer", fill = T)

            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                id = "Tables",
                tabPanel(title = "Dataset 1", dataTableOutput( "dt1")),
                tabPanel(title = "Dataset 2", dataTableOutput("dt2"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    datasets <-
        reactive(
            data.table(
                filepath = c(
                    "data/pokemon1.xlsx",
                    "data/pokemon2.xlsx"
                ),
                dataset = c(
                    "Pokemon Data 1 (from Kaggle)",
                    "Pokeomn Data 2 (from Cory)"
                )
            )
        )
    test_data <- reactive(
        read_excel( "data/sample.xlsx" )
    )
    
    observeEvent( 
        datasets(),{
        updateSelectizeInput(session, "file1", 
                 choices = c( datasets()$dataset, "Upload" ) )
        updateSelectizeInput(session, "file2", 
                 choices = c( datasets()$dataset, "Upload") ) 
        }
    )
    
    file_n1 <- reactive({
            req(input$file1)
            if(input$file1 != "Upload"){
                fp <- read_excel( datasets()[datasets()$dataset == input$file1,]$filepath)
                fp
            } else if (!is.null(input$upload1)){
                if(str_detect( input$upload1$datapath, "xls|xlsx")){
                    fp <- read_excel( input$upload1$datapath )
                } else { 
                    fp <- fread( input$upload1$datapath )
                }
                fp
            } else {
                NULL
            }

            
        })
    
    file_n2 <- reactive({
        req(input$file2)
        if(input$file2 != "Upload"){
            fp <- read_excel( datasets()[datasets()$dataset == input$file2,]$filepath)
            fp
        } else if (!is.null(input$upload2)){
            if(str_detect( input$upload2$datapath, "xls|xlsx")){
                fp <- read_excel( input$upload2$datapath )
            } else { 
                fp <- fread( input$upload2$datapath )
            }
            fp
        } else {
            NULL
        }
    })
    
    observeEvent(input$file1,{
        updateSelectizeInput(session, "mergecol1", choices = colnames(file_n1()))
    })
    
    observeEvent(input$upload1,{
        updateSelectizeInput(session, "mergecol1", choices = colnames(file_n1()))
    })
    observeEvent(input$file2,{
        updateSelectizeInput(session, "mergecol2", choices = colnames(file_n2()))
    })
    observeEvent(input$upload2,{
        updateSelectizeInput(session, "mergecol2", choices = colnames(file_n2()))
    })
    
    observeEvent(input$file1, {
        if( input$file1 == "Upload"){
            print("unhide")
            shinyjs::show("upload1")    
        } else {
            print("hide?")
            hide("upload1")
        }
        
    })
    
    observeEvent(input$file2, {
        if( input$file2 == "Upload"){
            shinyjs::show("upload2")    
        } else {
            hide("upload2")
        }
        
    })
    output$dt2 <- renderDataTable( datatable( file_n2() ))

    output$dt1 <- renderDataTable(
        datatable( file_n1() ))
    
    
    data_out <-
        reactive({
            req(input$mergecol1)
            req(input$mergecol2)
            req(input$jointype)
            if( input$jointype == "Left"){
                left_join( file_n1(), file_n2(), 
                           by = setNames( input$mergecol1, input$mergecol2 ) )
            } else if( input$jointype == "Full" ) {
                full_join( file_n1(), file_n2(), 
                            by = setNames( input$mergecol1, input$mergecol2 ) )
            } else if( input$jointype == "Right" ){
                right_join( file_n1(), file_n2(), 
                            by = setNames( input$mergecol1, input$mergecol2 ) )
            } else if( input$jointype == "Inner" ){
                inner_join( file_n1(), file_n2(), 
                           by = setNames( input$mergecol1, input$mergecol2 ) )
            } else if( input$jointype == "Anti" ){
                anti_join( file_n1(), file_n2(), 
                           by = setNames( input$mergecol1, input$mergecol2 ) )
            }
            
        })
    output$merge_explainer <-
        renderUI({
            join_dt <- data.table(
                joins = 
                    c("Left",
                "Full",
                "Right",
                "Inner",
                "Anti"),
                fp = c(
                    "https://github.com/gadenbuie/tidyexplain/blob/main/images/left-join-extra.gif?raw=true",
                    "https://github.com/gadenbuie/tidyexplain/blob/main/images/full-join.gif?raw=true",
                    "https://github.com/gadenbuie/tidyexplain/blob/main/images/right-join.gif?raw=true",
                    "https://github.com/gadenbuie/tidyexplain/blob/main/images/inner-join.gif?raw=true",
                    "https://github.com/gadenbuie/tidyexplain/blob/main/images/anti-join.gif?raw=true"
                )
            )
            tags$img(src = join_dt[join_dt$joins == input$jointype, ]$fp)
            })
    
    output$DLButton <- downloadHandler(
        filename = "merged_table.csv",
        content = function(file) {
           fwrite(data_out(), file)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
