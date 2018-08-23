# Load required packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(haven)
library(data.table)
library(tidyr)
library(dplyr)
library(Cairo)
library(shinyjs)
library(shinyFilters)
library(openxlsx)
library(scales)
library(plotly)


# Load Dataset loading scrip
source("datasets_loading_script.R")


# Define Gloabl function

# Codebook 

ColAttr <- function(x, attrC, ifIsNull) {
  # Returns column attribute named in attrC, if present, else isNullC.
  atr <- attr(x, attrC, exact = TRUE)
  atr <- if (is.null(atr)) {ifIsNull} else {atr}
  atr
}

AtribLst <- function(df, attrC, isNullC){
  # Returns list of values of the col attribute attrC, if present, else isNullC
  lapply(df, ColAttr, attrC=attrC, ifIsNull=isNullC)
}


# Filtering function

filterSet <- newFilterSet('FS1') %>%
  # give each filter unique id and tell it which column to filter on
  addSelectFilter('Region','region') %>%
  addSelectFilter('Interviewer','fieldworker') %>%
  addSelectFilter('Date','date_of_interview')

filterSet2 <- newFilterSet('FS2') %>%
  # give each filter unique id and tell it which column to filter on
  addSelectFilter('Region','region') %>%
  addSelectFilter('Interviewer','fieldworker') %>%
  addSelectFilter('Date','date_of_interview')




#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "DASHBOARD")
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  tags$head(
    tags$style(HTML(".sidebar {
                    height: 90vh; overflow-y: auto;
                    }"
    ) # close HTML       
    )            # close tags$style
    ),
  sidebarUserPanel("APHRC",
                   image = "aphrc.jpg"),
  menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
           href = "http://aphrc.org/"),
  selectInput("dataset", "Select Dataset", 
              choices = names(data_files)),
  uiOutput("freq_vars1"),
  checkboxInput("crosstab", "Crosstabulation", FALSE),
  uiOutput("freq_vars2"),
  uiOutput("freq_vars3"),
  selectInput("crosstab_type", "Select the type of Crosstab table to display", 
              choices = c("Counts", "Row %", "Column %"),
              selected = "Counts"
  )
  ,
  br(),
  h2("Filters"),
  helpText("You can use the fileds",
           "below to filter the",
           "dataset."),
  # selectInput("vars_fil", "Variable to filter (graph only)",
  #             choices = names(working_df),
  #             selected = names(working_df)[1]
  # ),
  
  filterInputs(filterSet2),
  #action but to reset filters
  hr(),
  filterMaster(filterSet2),
  filterResetButton(filterSet2)
    )



frow0 <- fluidRow(valueBoxOutput("value1", width = 3),
                  valueBoxOutput("value2", width = 3),
                  valueBoxOutput("value3", width = 3),
                  valueBoxOutput("value4", width = 3))

frow1 <- fluidRow(
  box( title = "Project Description",
       status = "primary"
        ,solidHeader = TRUE
        ,collapsible = T,
       
    # downloadButton("download_summary", "Download file"),
    #                   DT::dataTableOutput("summary_tab2", height = "600px")
    htmlOutput("summary_tab2")
),
box(downloadButton("download_missingness", "Download file"),
    title = "Missingness summary"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = T,
    DT::dataTableOutput("missingness", height = "600px")
)
)


frow2a <- fluidRow(
  box(downloadButton("download_codebook", "Download codebook"),
      title = "Codebook"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE,
      DT::dataTableOutput("codebook1", height = "550px")),
  box(title = "Single Plots - Frequency"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotOutput("coolplot", height = "600px")
  ),
  box(downloadButton("download_freq_tab", "Download frequency table"),
      title = "Single variable frequency tables"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE , 
      DT::dataTableOutput("tab1_dat", height = "500px")),
  box(title = "Single Plots - Percent"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotOutput("coolplot_percent", height = "600px")
  )
)


frow2b <- fluidRow(
  box(downloadButton("download_codebook2", "Download codebook"),
      title = "Codebook"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE,
      DT::dataTableOutput("codebook2", height = "550px")),
  box(
    title = "Crosstabulation plots"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("crosstplot", height = "600px")
  ),
  box(downloadButton("download_crosstab", "Download crosstab"),
      title = "Crosstabulation tables"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE, 
      DT::dataTableOutput("tab2_dat",  height = "500px")),
  box(
    title = "Crosstabulation plots - Percentages"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("crosstplot_percent", height = "600px")
  )
)

frow4 <- fluidRow(
  mainPanel(
    downloadButton("download_data", "Download data set"),
    DT::dataTableOutput("data",height = "700px"),
    width = 12
  )
)



# combine the two fluid rows to make the body

ui <- fluidPage(
  dashboardPage(title = 'APHRC User Dashboard', header, sidebar, 
                dashboardBody( tabsetPanel(type = "tabs",
                                           tabPanel("Summary", frow0, frow1),
                                           tabPanel("One-way Plots", frow2a),
                                           tabPanel("Crosstabulation Plots", frow2b),
                                           tabPanel("Dataset", frow4)
                )), 
                skin='red')
)





# Define the server end
server <- function(input, output) {
  
  working_df <- reactive({
    dat <- data_files[[input$dataset]]
  }
  )
  
  summary_tab <- reactive({
    summary_tabs[[input$dataset]]
  })
  
  project_name <- reactive({
    project_names[[input$dataset]]
  })
  
  
  # Set up global functions
  
  # Frequency tables
  tab1Func <- function(var){
    working_df <- working_df()
    df <- working_df %>% tbl_df()
    freq <- as.data.frame(table(df[, var]))
    names(freq) <- c("Labels", "Frequency")
    return(freq)
  }
  
  # Two-way frequency tables
  tab2Func <- function(var1, var2){
    working_df <- working_df()
    df <- working_df %>% tbl_df()
    dat <- df[, c(var1, var2)]
    freq <- data.frame(table(dat))
    names(freq) <- c(var1, var2, "Frequency")
    return(freq)
  }
  
  # Tables for filtered data
  tabFillFunc <- function(vars_fil){
    df <- filter_df %>% tbl_df()
    freq <- as.data.frame(table(df[, vars_fil]))
    names(freq) <- c("Labels", "Frequency")
    return(freq)
  }
  
  # Generate codebook labels
  #codebook1 <- reactive(codebook)
  codebook <-  reactive(
    {
      working_df <- working_df()
      # Use this to extract value labels
      val_labels <- AtribLst(working_df, attrC="labels", isNullC=NA)
      val_labels <- val_labels[!is.na(val_labels)]
      
      var_labels <- AtribLst(working_df, attrC="label", isNullC="")
      
      
      # extract codebook from attributes
      codebook <- melt(var_labels)
      setnames(codebook, c("description", "var"))
      codebook <- data.frame(var=codebook$var, description=codebook$description)
      codebook$description <- ifelse(codebook$description==""|is.na(codebook$description),
                                     as.character(codebook$var), as.character(codebook$description))
      codebook
      
    }
  )
  
  codebook1 <- reactive(codebook())
  output$codebook1 <-  DT::renderDataTable(
    codebook1(),
    fillContainer = T
  )
  
  codebook2 <- reactive(codebook())
  output$codebook2 <-  DT::renderDataTable(
    codebook2(),
    fillContainer = T
  )
  
  # Downloadable codebook ----
  output$download_codebook <- downloadHandler(
    filename = function() {
      paste("generated", "_codebook", "_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(codebook1(), file, row.names = F)
    }
  )
  
  output$download_codebook2 <- downloadHandler(
    filename = function() {
      paste("generated", "_codebook", "_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(codebook2(), file, row.names = F)
    }
  )
  
  
  # Summarize missingness for every variable
  miss_dis <- reactive({
    # Remove variables with all cases missing
    working_df <- working_df()
    working_df <- working_df %>% tbl_df()
    miss <- as.data.frame(sapply(working_df[, names(working_df)], function(x) sum(is.na(x)|as.factor(x)=="")))
    colnames(miss) <- "miss.nobs"
    miss$percent.miss <- round(miss$miss.nobs/nrow(working_df)*100, 2)
    miss$var <- row.names(miss)
    miss_dis <- merge(miss, codebook(), all.x = T, by = "var")
    miss_dis <- miss_dis[, c("var", "description", "miss.nobs", "percent.miss")]
    miss_dis
  })
  

  # Missingness file 
  missingness <- reactive(miss_dis())
  output$missingness <-  DT::renderDataTable(DT::datatable(
    missingness(),
    fillContainer = T
  )
  )

  # Downloadable missing ----
  output$download_missingness <- downloadHandler(
    filename = function() {
      paste("generated", "_missingness_summary", "_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(missingness(), file, row.names = F)
    }
  )
  
  
  # # selected variables
  # - This function is intended to select only variables with less than 100% missing
  # working_df_selected <- reactive({
  #   working_df1 <- working_df()
  #   working_df1 <- working_df() %>% tbl_df()
  #   miss <- as.data.frame(sapply(working_df1[, names(working_df1)], function(x) sum(is.na(x)|as.factor(x)=="")))
  #   colnames(miss) <- "miss.nobs"
  #   selected.vars <- row.names(miss)[miss$miss.nobs<100]
  #   working_df1 <- working_df1[, selected.vars]
  #   working_df1 <- setDT(working_df1)
  #   return(working_df1)
  # })
  # working_df <- reactive({working_df_selected()})
  
  # Number of completes
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    total_respondents <- nrow(working_df())
    valueBox(
      formatC(total_respondents, format="d", big.mark=',')
      , "No. of Surveyed Respondents"
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
    
    
  })
  
  # Duplicate ID vars
  output$value2 <- renderValueBox({
    working_df <- working_df()
    total_res_dup_id <- working_df[duplicate_id_var>0, ] %>% nrow
    valueBox(
      formatC(total_res_dup_id, format="d", big.mark=',')
      ,'No. of Duplicate IDs'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "orange")
    
  })
  

  output$value3 <- renderValueBox({
    working_df <- setDT(working_df())
    total_res_miss_id <- working_df[questionnaire_id_var==""|is.na(questionnaire_id_var), ] %>% nrow
    valueBox(
      formatC(total_res_miss_id, format="d", big.mark=',')
      ,"No. of missing IDs"
      ,icon = icon("stats",lib='glyphicon')
      ,color = "red")
    
  })
  
  # Missingness percentage
 
  output$value4 <- renderValueBox({
    miss_dis <- miss_dis()
    miss_prop <- paste(round(sum(miss_dis$percent.miss)/length(miss_dis$percent.miss), 2), "%", sep = "")
    valueBox(
      formatC(miss_prop, format="d", big.mark=',')
      ,"Proportion of missingness"
      ,icon = icon("stats",lib='glyphicon')
      ,color = "red")
    
  })
  
  
  
  output$coolplot <- renderPlot({
    working_df <- working_df()
    tab <- tab1Func(input$vars)
    colnames(tab) <- c("Var", "Frequency")
    tab <- setDT(tab)
    tab <- tab[Frequency>0]
    setDF(tab)
    setorder(tab, -Frequency)
    ggplot(tab, aes(x=reorder(Var, -Frequency), y=Frequency)) +
      geom_col(fill="green1", position = position_stack()) + coord_flip() +
      ylab("Completed Interviews (n)") + xlab(codebook()$description[codebook()$var %in% input$vars]) + 
      geom_text(size=4, color="black", aes(label=Frequency, y=Frequency), position = position_stack(vjust = 0.5)) +
      ggtitle(paste("Frequency distribution of ", codebook()$description[codebook()$var %in% input$vars], sep = '')) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            title=element_text(size=9))
  })
  
  output$crosstplot <- renderPlot({
    if(input$crosstab!=F){
      working_df <- working_df()
      tab <- tab2Func(input$x_vars, input$y_vars)
      colnames(tab) <- c("Var", "Categories", "Frequency")
      tab <- setDT(tab)
      tab <- tab[Frequency>0]
      tab <- setDF(tab)
      setorder(tab, -Frequency)
      ggplot(tab, aes(x=reorder(Var, -Frequency, sum), y=Frequency, fill=Categories)) +
        geom_col(position = position_stack()) + coord_flip() +
        ylab("Completed Interviews") + xlab(codebook()$description[codebook()$var %in% input$x_vars]) + 
        scale_fill_discrete(name=codebook()$description[codebook()$var %in% input$y_vars]) + 
        geom_text(size=4, color="black", aes(label=Frequency, y=Frequency), position = position_stack(vjust = 0.5)) +
        ggtitle(paste("Frequency distribution of ", codebook()$description[codebook()$var %in% input$x_vars], " by \n", 
                      codebook()$description[codebook()$var %in% input$y_vars],
                      sep = '')) + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              title=element_text(size=9))
    }
  })
  
  
  
  # Filter plots by some key variables
  data <- reactive(working_df())
  filterSet2 <- initializeFilterSet(filterSet2, data)
  output$filterplot1 <- renderPlot({
    filter_df <- filterSet2$output()
    filter_df <- filter_df %>% tbl_df()
    tab <- as.data.frame(table(filter_df[, input$vars_fil]))
    #tab <- tabFillFunc(input$vars_fil)
    colnames(tab) <- c("Var", "Frequency")
    tab <- setDT(tab)
    tab <- tab[Frequency>0]
    setDF(tab)
    setorder(tab, -Frequency)
    ggplot(tab, aes(x=reorder(Var, -Frequency), y=Frequency)) +
      geom_col(fill="lightsteelblue4", position = position_stack()) + coord_flip() +
      ylab("Completed Interviews") + xlab(codebook()$description[codebook()$var %in% input$vars_fil]) +
      geom_text(size=5, color="white", aes(label=Frequency, y=Frequency), position = position_stack(vjust = 0.5)) +
      ggtitle(paste("Frequency distribution of ", codebook()$description[codebook()$var %in% input$vars_fil], "- filtered", sep = ''))
  })
  
  # the output is a reactive data.frame
  output$data <- DT::renderDataTable(filterSet2$output(), 
                                     fillContainer = T)
  
  # Download data ----
  output$download_data <- downloadHandler(
    filename = function() {
      paste(project_name(), "downloaded", "_data", "_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filterSet2$output(), file, row.names = F)
    }
  )

  
  # Summary table
  # summary_tab1 <- reactive(summary_tab())
  # output$summary_tab2 <-  DT::renderDataTable(DT::datatable(
  #   summary_tab1(),
  #   fillContainer = T
  # )
  # )
  
  #summary_tab1 <- reactive(summary_tab())
  output$summary_tab2 <-  renderUI(
    HTML(summary_tab())
  )

  
  
  # Downloadable table ----
  output$download_summary <- downloadHandler(
    filename = function() {
      paste("generated", "_summary", "_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(summary_tab1(), file, row.names = F)
    }
  )
  
  
  # Tables - One way
  tab1 <- reactive({
    working_df <- working_df()
    working_df2 <- working_df %>% tbl_df()
    tab <- as.data.frame(table(working_df2[, input$vars]))
    names(tab) <- c("Label", "Frequency")
    tab$Percent <- round(tab$Frequency/sum(tab$Frequency)*100, 2)
    total <- data.frame(Label="Total", Frequency=sum(tab$Frequency), Percent=sum(tab$Percent))
    rbind(tab, total)
  })
  
  output$tab1_dat <- DT::renderDataTable(DT::datatable(
    tab1(),
    fillContainer = T
  ))
  
  # Percentage plot
  output$coolplot_percent <- renderPlot({
    tab <- tab1()
    tab <- setDT(tab)
    tab <- tab[Frequency>0 & Percent<100]
    tab <- tab[, c("Label", "Percent")]
    tab <- tab[Label!="Total"]
    tab$Percent <- (tab$Percent)/100
    setDF(tab)
    setorder(tab, -Percent)
    ggplot(tab, aes(x=reorder(Label, -Percent), y=Percent)) +
      geom_col(fill="green1", position = position_stack()) + coord_flip() +
      ylab("Completed Interviews (%)") + xlab(codebook()$description[codebook()$var %in% input$vars]) + 
      geom_text(size=4, color="black", aes(label = scales::percent(Percent), y=Percent), position = position_stack(vjust = .8)) +
      scale_y_continuous(labels=(scales::percent)) +
      ggtitle(paste("Percentage distribution per ", codebook()$description[codebook()$var %in% input$vars], sep = '')) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            title=element_text(size=9))
  })
  
  # Downloadable csv of selected dataset ----
  output$download_freq_tab <- downloadHandler(
    filename = function() {
      paste(input$vars, "_freq", "_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(tab1(), file, row.names = F)
    }
  )
  
  
  # Tables - Two-ways
  choise <- reactive(input$crosstab_type)
  tab2_tabs <- reactive({
    if(input$crosstab!=F & choise()=="Counts"){
      working_df <- working_df()
      working_df2 <- working_df %>% tbl_df()
      tab2 <- addmargins(table(working_df2[, c(input$x_vars, input$y_vars)]), 
                         FUN = list(list(Total = sum)), quiet = T)
      tab2 <- as.data.frame.matrix(tab2)
      tab2$Label <- row.names(tab2)
      labs <- names(tab2)[!names(tab2) %in% "Label"]
      tab2 <- tab2 %>% tbl_df()
      tab2 <- tab2[, c("Label", labs)]
      return(tab2)
    }
    
    if(input$crosstab!=F & choise()=="Row %"){
      working_df <- working_df()
      working_df2 <- working_df %>% tbl_df()
      tab2 <- addmargins(table(working_df2[, c(input$x_vars, input$y_vars)]), 
                         FUN = list(list(Total = sum)), quiet = T)
      tab2 <- as.data.frame.matrix(tab2)
      row_names <- row.names(tab2)
      tab2_row <- round(tab2/tab2$Total*100, 1)
      tab2_row$Label <- row_names
      labs <- names(tab2_row)[!names(tab2_row) %in% "Label"]
      tab2_row <- tab2_row %>% tbl_df()
      tab2_row <- tab2_row[, c("Label", labs)]
      return(tab2_row)
    }
    
    if(input$crosstab!=F & choise()=="Column %"){
      working_df <- working_df()
      working_df2 <- working_df %>% tbl_df()
      tab2 <- addmargins(table(working_df2[, c(input$x_vars, input$y_vars)]), 
                         FUN = list(list(Total = sum)), quiet = T)
      tab2 <- as.data.frame.matrix(tab2)
      row_names <- row.names(tab2)
      t2 <- data.frame(t(tab2))
      t2_col <- round(t2/t2$Total*100, 1)
      tab2_col <- data.frame(t(t2_col))
      tab2_col$Label <- row_names
      labs <- names(tab2_col)[!names(tab2_col) %in% "Label"]
      tab2_col <- tab2_col %>% tbl_df()
      tab2_col <- tab2_col[, c("Label", labs)]
      return(tab2_col)
    }
  }
  )
  
  # Crosstab percentage plots
  output$crosstplot_percent <- renderPlot({
    if(input$crosstab!=F & (choise()=="Column %" | choise()=="Row %")){
      tab <- tab2_tabs()
      setDF(tab)
      vars <- names(tab)[!names(tab) %in% "Total"]
      tab <- tab[, vars]
      setDT(tab)
      tab <- tab[Label!="Total"]
      vars <- names(tab)[!names(tab) %in% "Label"]
      tab <- tab %>% gather(Categories, Percentage, vars)
      colnames(tab) <- c("Var", "Categories", "Percent")
      tab$Percent <- tab$Percent/100
      tab <- setDT(tab)
      tab <- tab[Percent>0]
      tab <- setDF(tab)
      setorder(tab, -Percent)
      tab$Categories <- factor(as.character(tab$Categories))
      ggplot(tab, aes(x=reorder(Var, -Percent, sum), y=Percent, fill=Categories)) +
        geom_col(position = position_stack(), show.legend=T) + coord_flip() +
        ylab("Completed Interviews (%)") + xlab(codebook()$description[codebook()$var %in% input$x_vars]) + 
        scale_fill_discrete(labels = levels(tab$Categories), name=codebook()$description[codebook()$var %in% input$y_vars]) + 
        geom_text(size=4, color="black", aes(label = scales::percent(Percent), y=Percent), position = position_stack(vjust = 0.5)) +
        scale_y_continuous(labels=(scales::percent)) +
        ggtitle(paste("Percentage distribution of ", codebook()$description[codebook()$var %in% input$x_vars], " per \n", 
                      codebook()$description[codebook()$var %in% input$y_vars],
                      sep = '')) + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              title=element_text(size=9))
    }
  })
  
  
  output$tab2_dat <- DT::renderDataTable(DT::datatable(
    tab2_tabs(),
    fillContainer = T
  )
  )
  
  # Downloadable csv of selected dataset ----
  output$download_crosstab <- downloadHandler(
    filename = function() {
      paste(input$x_vars, "-by-", input$y_vars, "_", choise(), "_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(tab2_tabs(), file,row.names = F)
    }
  )
  
  output$freq_vars1 <- renderUI({
    selectInput('vars',
                choices = names(working_df()),
                label = "Select variable to plot its frequency",
                selected = "date_of_interview")
  })
  
  output$freq_vars2 <- renderUI({
    selectInput('x_vars',
                choices = names(working_df()),
                label = "Select row variable",
                selected = "date_of_interview")
  })
  
  output$freq_vars3 <- renderUI({
    selectInput('y_vars',
                choices = names(working_df()),
                label = "Select column (key) variable",
                selected = "region")
  })
  
}

shinyApp(ui = ui, server = server)