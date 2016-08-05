# convert variable types in a data frame 

var_conv <- function() {
  
  require("shiny")
  require("miniUI")
  
  ui <- miniPage(
    gadgetTitleBar("Convert variable types in a data frame"),
    miniContentPanel(
      fluidRow(align = "center",
               column(1),
               column(10, br(),
                      br(),
                      fluidRow(
                        column(6, selectInput("dat", "Select data frame", choices = c(unlist(ls(envir = .GlobalEnv))), selected = NULL)),
                        column(6, selectInput("vars", "Select variables", choices = NULL, multiple = TRUE),
                               actionButton("clear","Clear"))
                      ),
                      br(),
                      fluidRow(
                        column(4),
                        column(4, uiOutput("buttons")),
                        column(4)
                      ),
                      br(),
                      fluidRow(verbatimTextOutput("struct")),
                      HTML('<b> Selected data frame will be updated with the variable types after clicking on "Update"<br><br>'),
                      actionButton("update", "Update data frame"),br(),
                      uiOutput("Info")),
               column(1)
      )
    )
  )
  
  server <- function(input, output, session) {
    
    values <- reactiveValues()
    values$dat <- data.frame()
    values$info <- ""
    
    observeEvent(input$dat,{
      values$dat <- get(input$dat, envir = .GlobalEnv)
      updateSelectInput(session, inputId = "vars",label = "Select variables", 
                        choices = names(values$dat), selected = NULL)
    })
    
    observeEvent(input$clear,{
      input$dat
      updateSelectInput(session, inputId = "vars", label = "Select variables",
                         choices = names(values$dat), selected = NULL)
    })
    
    output$buttons <- renderUI({
      if(!is.null(input$vars)){
        fluidRow(
          br(),
          fluidRow(align = "center", HTML("Click on the buttons below to convert the selected variables")),
          br(),
          fluidRow(
          column(4,actionButton("num","Numeric")),
          column(4,actionButton("fact","Factor")),
          column(4,actionButton("char","Character")))
        )
      }
    })
    
    observeEvent(input$num,{
      func <- function(x){
        return(as.numeric(as.character(x)))
      }
      values$dat[,input$vars]<- lapply(values$dat[,input$vars, drop = FALSE], FUN = func)
    })
    
    observeEvent(input$fact,{
      values$dat[,input$vars]<- lapply(values$dat[,input$vars, drop = FALSE], FUN = factor)
    })
    
    observeEvent(input$char,{
      func <- function(x){
        return(as.character(x))
      }
      values$dat[,input$vars]<- lapply(values$dat[,input$vars, drop = FALSE], FUN = func)
    })
    
    output$struct <- renderPrint({
      str(values$dat)
    })
    
    output$Info <- renderUI({
      HTML(values$info)
    })
    
    observeEvent(input$update,{
      if(!is.null(input$vars)){
        df_name <- c(input$dat)
        assign(df_name, values$dat, envir = .GlobalEnv)
        values$info <- "Done!"
      }
    })
    
    observeEvent(input$done, {
      stopApp()
    })
  }
  
  runGadget(ui, server, viewer = paneViewer(), stopOnCancel = TRUE)
}

