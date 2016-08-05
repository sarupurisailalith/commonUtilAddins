# Merge two data frames

df_merge <- function() {
  
  require("shiny")
  require("miniUI")
  
  ui <- miniPage(
    gadgetTitleBar("Merge data frames"),
    miniContentPanel(
      fluidRow(align = "center",br(),
               HTML('<b> Creates a data frame "df_merged" after clicking on "Done"<br><br>'),br(),
               fluidRow(column(4,selectInput("df1", "select dataframe X:", choices = c("",unlist(ls(envir = .GlobalEnv))), selected = NULL, width = "50%"),
                      selectInput("df2", "select dataframe Y", choices = c("",unlist(ls(envir = .GlobalEnv))), selected = NULL, width = "50%"),
                      br(),
                      actionButton("load", "load data frames"),
                      br(),br(),
                      fluidRow( uiOutput("options"))),
               column(8, uiOutput("data_disp"),br(),
                      HTML("<b><u>Merged data frame</u></b>"),br(),
                      dataTableOutput("merged_df")))
      )
    )
  )
  
  server <- function(input, output) {
    
    values <- reactiveValues()
    values$df1 <- data.frame()
    values$df2 <- data.frame()
    values$merged <- data.frame()
    
    observeEvent(input$load, {
      values$df1 <- get(input$df1, envir = .GlobalEnv)
      values$df2 <- get(input$df2, envir = .GlobalEnv)
    })
    
    output$data_disp <- renderUI({
      input$load
      fluidRow(
        column(6, HTML("<b><u>data frame X</u></b>"),br(),
               dataTableOutput("df1")),
        column(6, HTML("<b><u>data frame Y</u></b>"),br(),
               dataTableOutput("df2"))
      )
    })
    
    
    output$df1 <- renderDataTable({
      values$df1
    },options = list( scrollX = TRUE , ordering = FALSE , 
                      info = FALSE, searching = FALSE, pageLength = 4))
    
    output$df2 <- renderDataTable({
      values$df2
    },options = list( scrollX = TRUE, ordering = FALSE ,
                      info = FALSE, searching = FALSE, pageLength = 4))
    
    output$merged_df <- renderDataTable({
      values$merged
    },options = list( scrollX = TRUE, ordering = FALSE ,
                      info = FALSE, searching = FALSE, pageLength = 4))
    
    output$options <- renderUI({
      if(input$load > 0){
        fluidRow(
          fluidRow(
            column(1),
            column(5, selectInput("byX", "by.X", choices = names(values$df1), selected = NULL)),
            column(5, selectInput("byY", "by.Y", choices = names(values$df2), selected = NULL)),
            column(1)
          ),
          fluidRow(
            column(1),
            column(10, radioButtons("type", "", choices = c("Inner Join", "Left Join", "Right Join", "Full Join"), selected = "Inner Join")),
            column(1)
          ),
          br(),
          fluidRow(align = "center", actionButton("merge","Merge"))
        )
      }
    })
    
    observeEvent(input$merge,{
      if(input$type =="Inner Join"){
        
        values$merged <- merge(values$df1, values$df2, by.x = input$byX, by.y = input$byY, all = FALSE)
        
      } else if(input$type == "Left Join"){
        
        values$merged <- merge(values$df1, values$df2, by.x = input$byX, by.y = input$byY, all.x = TRUE)
        
      } else if(input$type == "Right Join"){
        
        values$merged <- merge(values$df1, values$df2, by.x = input$byX, by.y = input$byY, all.y = TRUE)
        
      } else {
        
        values$merged <- merge(values$df1, values$df2, by.x = input$byX, by.y = input$byY, all = TRUE)
        
      }
    })
    
    observeEvent(input$done, {
      df_name <- c(paste0(input$df1,"_",input$df2))
      assign(df_name, values$merged, envir = .GlobalEnv)
      stopApp()
    })
  }
  
  runGadget(ui, server, viewer = paneViewer())
}

