# !diagnostics off

#REQUIRED PACKAGES!#
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(DT)
library(plotly)
library(here)

##### Read in required files ####
#Average Service Costs (Euro Converted)
path <- file.path("average_service_costs_euros.csv")
df <- read.csv(path, stringsAsFactors = TRUE ,header = TRUE)
head(df)
#Average Service Costs (By Sales Organisation Local Currency)
path2 <- file.path("average_service_costs.csv")
df2 <- read.csv(path2, stringsAsFactors = FALSE ,header = TRUE)
#Total Parts Cost
path3 <- file.path("total_parts_cost.csv")
df3 <- read.csv(path3, stringsAsFactors = TRUE ,header = TRUE)
#Total Machine Services 
path4 <- file.path("recoded_feature_data.csv")
df4 <- read.csv(path4, stringsAsFactors = TRUE ,header = TRUE)
#Cluster Centroids
path5 <- file.path("kmeans_centroids.csv")
df5 <- read.csv(path5, stringsAsFactors = TRUE ,header = TRUE)
#Cluster Data
path6 <- file.path("kmeans_data.csv")
df6 <-read.csv(path6, stringsAsFactors = TRUE ,header = TRUE)
#Feature Data Exploration
path7 <- file.path("recoded_feature_data_for_machine.csv")
df7 <-read.csv(path7, stringsAsFactors = TRUE ,header = TRUE)
path8 <- file.path("recoded_features_for_vis.csv")
df8 <- read.csv(path8, stringsAsFactors = TRUE ,header = TRUE)

#Create the single value visualisations
frow1 <- fluidRow(
  valueBoxOutput("value5")
  ,valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  
)

frow2 <- fluidRow(
  valueBoxOutput("value6")
  ,valueBoxOutput("value3")
  ,valueBoxOutput("value4")
)

frow3 <- fluidRow(
  valueBoxOutput("value7")
  ,valueBoxOutput("value8")
  ,valueBoxOutput("value9")
)

frow4 <- fluidRow(
  valueBoxOutput("value10")
  ,valueBoxOutput("value11")
  ,valueBoxOutput("value12")
)

frow5 <- fluidRow(
  valueBoxOutput("value13", width = 12)
)

frow6 <- fluidRow(
  valueBoxOutput("value14", width = 12)
)

frow7 <- fluidRow(
  valueBoxOutput("value15", width = 12)
)



# Create the UI using the header, sidebar, and body
ui <- dashboardPage(title="Service Costs and Frequency",
  dashboardHeader(title = "Service Costs and Frequency",
                  titleWidth = 450),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    #Overview Service cost/count Visualisations
    frow1,
    fluidRow(
      box(title = "Service Call Category"
          ,status = "primary"
          ,solidHeader = TRUE
          ,width = 12
          ,collapsible = TRUE 
          ,tabsetPanel(type = "tabs",
                tabPanel("Average Service Cost", plotOutput("barplot_overall1", height = "500px")),
                tabPanel("Total Service Count", plotOutput("barplot_overall2", height = "500px"))
          ))),
    fluidRow(
      box(title = "Sales Organisation"
          ,status = "primary"
          ,solidHeader = TRUE
          ,width = 12
          ,collapsible = TRUE 
          ,tabsetPanel(type = "tabs",
                       tabPanel("Average Service Cost", plotOutput("barplot_overall1_1", height = "500px")),
                       tabPanel("Total Service Count", plotOutput("barplot_overall2_1", height = "500px"))
          ))),
      
#     box(title = "Total Service Count by Category"
#          ,status = "primary"
#          ,solidHeader = TRUE 
#          ,collapsible = TRUE 
#          ,plotOutput("barplot_overall2", height = "400px"))
#    ),
    
    selectInput(inputId = "category_filter", 
                label = "Service Call Category:",
                choices = c("Accessories", "Breakdown", "Inspection / Maintenance", "Installation / Commissioning", "Instruction", "Retrofit", "Transport damages", "Welding"), 
                selected = "Accessories"),
    #Visualisations for Service Cost per Category
     
    frow2,
    #add plots
    fluidRow(
      box(title = "Average Service Cost by Organisation"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("barplot1", height = "400px")),
      
      box(title = "Total Service Count by Organisation"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("barplot2", height = "400px"))
    ),
    selectInput(inputId = "organisation_filter", 
                label = "Sales Organisation:",
                choices = c("150 Organisation Oesterreich",
                            "200 Organisation Deutschland",
                            "300 Organisation AG",
                            "320 Organisation International",
                            "400 Organisation UK",
                            "500 Organisation USA",
                            "561 Frima Deutschland",
                            "562 Frima Frankreich SAS",
                            "590 Frima International",
                            "600 Organisation Italien",
                            "610 Organisation Polen",
                            "620 Organisation Frankreich",
                            "650 Organisation Scandinavien",
                            "660 Brasilen",
                            "680 Organisation Mexiko",
                            "690 Tuerkey",
                            "700 Organisation Schweiz",
                            "800 Organisation Canada",
                            "900 Organisation Iberica"
                ), 
                selected = "200 Organisation Deutschland")
    ,
    frow3,
    fluidRow(
      box(title = "Average Service Cost by Service Call Category"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("barplot3", height = "400px")),
      
      box(title = "Total Service Count by Service Call Category"
          ,status = "primary"
          ,solidHeader = TRUE 
          ,collapsible = TRUE 
          ,plotOutput("barplot4", height = "400px"))
    ),
    selectInput(inputId = "parts_category_filter", 
                label = "Service Call Category:",
                choices = c("Accessories", "Breakdown", "Inspection / Maintenance", "Installation / Commissioning", "Instruction", "Retrofit", "Transport damages", "Welding"), 
                selected = "Accessories")
    ,
    frow4,
    DT::dataTableOutput('table'),
    frow5,
    DT::dataTableOutput('service_machines_individual'),
    frow6,
    fluidRow(
#      div(style="display: inline-block;vertical-align:top; width: 45%;",
#          valueBoxOutput("value14")),
      div(style="display: inline-block;vertical-align:top; width: 33%;",
      checkboxGroupInput("energy_type", "Energy Type:",
                         c("Electric" = "Electric",
                           "Gas" = "Gas"
                           ),inline = TRUE)),
      div(style="display: inline-block;vertical-align:top; width: 33%;",
      checkboxGroupInput("machine_type", "Machine Type:",
                         c("Single" = "Single",
                            "Mult" = "Mult"
                          ),inline = TRUE)),
      div(style="display: inline-block;vertical-align:top; width: 33%;",
      checkboxGroupInput("machine_size", "Machine Size:",
                         c("61" = "61",
                           "62" = "62",
                           "101" = "101",
                           "102" = "102",
                           "201" = "201",
                           "202" = "202"
                         ),inline = TRUE))
    )
#, #total_service_model_vis
#div(align="center",
#  box(title = "Total Services by Model"
#      ,status = "primary"
#      ,solidHeader = TRUE
#      ,width = 12
#      ,collapsible = TRUE 
#      ,collapsed = TRUE
#      ,column( 12,align="center" ,plotOutput("total_service_model_vis", height = "500px"))))
,DT::dataTableOutput('service_machines_model')    
,frow7,
    DT::dataTableOutput('clusters')
    , fluidRow(
      box(title = "Quantitative Feature Distributions"
          ,status = "primary"
          ,solidHeader = TRUE
          ,width = 6
          ,collapsible = TRUE 
          ,tabsetPanel(type = "tabs",
                       tabPanel("Breakdowns", plotOutput("exp1", height = "500px")),
                       tabPanel("Average Days b/w Services", plotOutput("exp2", height = "500px")),
                       tabPanel("Days to First Service", plotOutput("exp3", height = "500px")),
                       tabPanel("Average Service Cost", plotOutput("exp4", height = "500px"))
          )),
      box(title = "Categorical Feature Distributions"
          ,status = "primary"
          ,solidHeader = TRUE
          ,width = 6
          ,collapsible = TRUE 
          ,tabsetPanel(type = "tabs",
                       tabPanel("Machine Size", plotOutput("exp5", height = "500px")),
                       tabPanel("Energy Type", plotOutput("exp6", height = "500px")),
                       tabPanel("Machine Type", plotOutput("exp7", height = "500px")),
                       tabPanel("Installation Year", plotOutput("exp8", height = "500px"))
          ))) 
    ,div(plotlyOutput('kmeans', height = 800),align = "center")
             
    
    
  )
)


server <- function(input, output, session) {
  ##### Data manipulation to derive single values for the boxes #####
  
  #Average Service Costs (Euros Converted)
  avg_sc <- mean(df$avg_service_cost)
  total_sc <- sum(df$total_service_count)
  avg_sc_category <-reactive ({ df %>% 
                    filter(service_call_category == input$category_filter) %>% 
                    summarise(avg_sc_category = mean(avg_service_cost))})
  total_sc_category <-reactive ({ df %>% 
                      filter(service_call_category == input$category_filter) %>% 
                      summarise(total_sc_category = sum(total_service_count))})
  avg_sc_category_vis <- reactive ({ df %>% 
      filter(service_call_category == input$category_filter, avg_service_cost >0) %>% 
      select(sales_organisation,avg_service_cost)})
  
  total_sc_category_vis <- reactive ({ df %>% 
      filter(service_call_category == input$category_filter, avg_service_cost >0) %>% 
      select(sales_organisation,total_service_count)})
  
  avg_sc_vis <- reactive ({ df %>% 
      filter(avg_service_cost >0) %>% 
      select(service_call_category,avg_service_cost) %>%
    group_by(service_call_category) %>%
    summarise(avg_sc = mean(avg_service_cost))})
  
  avg_sc_vis2 <- reactive ({ df %>% 
      filter(avg_service_cost >0) %>% 
      select(sales_organisation,avg_service_cost) %>%
      group_by(sales_organisation) %>%
      summarise(avg_sc = mean(avg_service_cost))})
  
  total_sc_vis <- reactive ({ df %>% 
      filter(avg_service_cost >0) %>% 
      select(service_call_category,total_service_count) %>%
      group_by(service_call_category) %>%
      summarise(total_sc = sum(total_service_count))})
  
  total_sc_vis2 <- reactive ({ df %>% 
      filter(avg_service_cost >0) %>% 
      select(sales_organisation,total_service_count) %>%
      group_by(sales_organisation) %>%
      summarise(total_sc = sum(total_service_count))})
  
  #Average Service Costs per Organisation
  currency <- reactive ({ df2 %>% 
      filter(sales_organisation == input$organisation_filter,avg_service_cost >0) %>% 
      select(positions_currency) %>%
      distinct()})
  currency <- unlist(currency)
  

  avg_sc_organisation <-reactive ({ df2 %>% 
      filter(sales_organisation == input$organisation_filter,avg_service_cost >0) %>% 
      summarise(avg_sc_category = mean(avg_service_cost))})
  
  total_sc_organisation <-reactive ({ df2 %>% 
      filter(sales_organisation == input$organisation_filter,avg_service_cost >0) %>% 
      summarise(total_sc_category = sum(total_service_count))})
  
  avg_sc_organisation_vis <- reactive ({ df2 %>% 
      filter(sales_organisation == input$organisation_filter,avg_service_cost >0) %>% 
      select(service_call_category,avg_service_cost)})
  
  total_sc_organisation_vis <- reactive ({ df2 %>% 
      filter(sales_organisation == input$organisation_filter,avg_service_cost >0) %>% 
      select(service_call_category,total_service_count)})
  
  #Total Parts costs
  total_parts_cost <- reactive ({ df3 %>% 
      filter(service_call_category == input$parts_category_filter) %>% 
      summarise(total_parts_cost = sum(total_cost_in_euros))})#sum(df3$total_cost_in_euros)
 
   total_parts_used_in_service <- reactive ({ df3 %>% 
      filter(service_call_category == input$parts_category_filter) %>% 
      summarise(total_parts_cost = sum(total_service_count))})#sum(df3$total_service_count)
  
   parts_data_table <- reactive({df3 %>%
       filter(service_call_category == input$parts_category_filter, total_cost_in_euros >0) %>% 
       mutate(average_part_cost = total_cost_in_euros/total_service_count) %>%
       select("Positions Part Number" = positions_part_number, "Service Call Category" = service_call_category, "Average Part Cost (Euros)" = average_part_cost,"Total Cost (Euros)" = total_cost_in_euros, "Total Used in Service" = total_service_count)})
  
  #Total Services per Machine by Serial Number
   total_services_per_machine <- reactive({df4 %>% 
       select("Serial Number" = serial_number,"Sales Organisation" = sales_organisation,service_call_category,total_service_count) %>%
       spread(key = 'service_call_category', value = 'total_service_count', fill = NA, convert = FALSE) %>%
       distinct()})
  #Total Services per Model 
   total_services_per_model <- reactive({ df4 %>%
       select(model,sales_organisation,service_call_category,total_service_count, energy_type, machine_size, machine_type) %>%
       filter(energy_type %in% input$energy_type, machine_size %in% input$machine_size, machine_type %in% input$machine_type) %>%
       group_by(model,sales_organisation,service_call_category) %>%
       mutate(total_service_count = sum(total_service_count)) %>%
       select("Model" = model,"Sales Organisation" = sales_organisation,service_call_category,total_service_count) %>%
       distinct()%>%
       spread(key = 'service_call_category', value = 'total_service_count', fill = NA, convert = FALSE)})
   
   total_services_per_model_vis <- reactive({ df4 %>%
       select(model,service_call_category,total_service_count, energy_type, machine_size, machine_type) %>%
       filter(energy_type %in% input$energy_type, machine_size %in% input$machine_size, machine_type %in% input$machine_type) %>%
       group_by(service_call_category) %>%
       mutate(total_service_count = sum(total_service_count)) %>%
       select("Model" = model,service_call_category,total_service_count) %>%
       distinct()})
 
   #Clusters
   cluster <- reactive({ df5 %>% 
       mutate(Usage = recode(cluster, `1` = "Low",`4` = "Low",  `5` = "High",`6` ="High",`2` ="Medium",`3` ="Medium")) %>%
       group_by(Usage) %>%
       mutate(average_time_between_service = mean(average_time_between_service),avg_serv_costs = mean(avg_serv_costs),days_to_first_serv = mean(days_to_first_serv),
              breakdown = mean(breakdown),cluster_size = sum(cluster_size)) %>%
       select(Usage, "Days b/w Services" = average_time_between_service, "Average Service Cost" = avg_serv_costs, 
              "Days to First Service" = days_to_first_serv, "Breakdowns" = breakdown, "Cluster Size" = cluster_size) %>%
       distinct()
       })
   
   #recode classes 
   kmeans_data <- reactive({ df6 %>% 
       mutate(usage_class = recode(usage_class, `1` = "1. Low", `4` = "1. Low", `5` = "3. High",`6` ="3. High",`2` ="2. Medium",`3` ="2. Medium"))})
   
   #Qualitative Data Distribution 
   count_of_sizes <- reactive({df8 %>% 
       mutate(machine_size = factor(machine_size)) %>%
     count(machine_size)})
   
   count_of_energy_types <- reactive({df8  %>% 
     count(energy_type)})
   
   count_of_machine_types <- reactive({df8  %>%
     count(machine_type)})
   
   count_of_installation_year <- reactive({df8  %>%
     count(installation_year)})
   
  #creating the valueBoxOutput content
  #Average Overall Service Cost
  output$value1 <- renderValueBox({
    valueBox(
      paste(round(avg_sc,digits = 2), " €")
      ,"Average Overall Service Cost"
      ,icon = icon("eur",lib='glyphicon')
      ,color = "light-blue")  
  })
  
  #Total Service Count
  output$value2 <- renderValueBox({
    valueBox(
      formatC(total_sc, format="d", big.mark=',')
      ,"Total Service Counts"
      ,icon = icon("stats",lib='glyphicon')
      ,color = "blue")  
  })
  
  #Service Cost Category Filter:
  output$value3 <- renderValueBox({
    valueBox(
      paste(round(avg_sc_category(),digits= 2)," €")
      ,paste(input$category_filter, "Average Service Cost")
      ,icon = icon("eur",lib='glyphicon')
      ,color = "light-blue")  
  })
  
  #Service Count Category Filter:
  output$value4 <- renderValueBox({
    valueBox(
      formatC(total_sc_category(), format="d", big.mark=',')
      ,paste(input$category_filter, " Total Service Count")
      ,icon = icon("stats",lib='glyphicon')
      ,color = "blue")  
  })
  
  output$value5 <- renderValueBox({
    valueBox(
      "Overall View"
      ,"Complete Data Only"
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "red")  
  })
  
  output$value6 <- renderValueBox({
    valueBox(
      "Service Call Category"
      ,paste(input$category_filter, "")
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "red")  
  })
  
  output$value7 <- renderValueBox({
    valueBox(
      "Sales Organisation"
      ,input$organisation_filter
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "red")  
  })
  
  #Total Service Count
  output$value9 <- renderValueBox({
    valueBox(
      formatC(total_sc_organisation(), format="d", big.mark=',')
      ,"Total Service Counts"
      ,icon = icon("stats",lib='glyphicon')
      ,color = "blue")  
  })
  
  #Service Cost Category Filter:
  output$value8 <- renderValueBox({
    valueBox(
      paste(round(avg_sc_organisation(),digits= 2)," ", currency())
      ,paste(input$category_filter, "Average Service Cost")
      ,icon = icon("eur",lib='glyphicon')
      ,color = "light-blue")  
  })
  
  #Total Parts Cost
  output$value10 <- renderValueBox({
    valueBox(
      "Service Part Details"
      ,input$parts_category_filter
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "red")  
  })
  
  output$value11 <- renderValueBox({
    valueBox(
      paste(formatC(total_parts_cost(), format="d", big.mark=',')," €")
      ,"Total Parts Cost"
      ,icon = icon("eur",lib='glyphicon')
      ,color = "light-blue")  
  })
  
  output$value12 <- renderValueBox({
    valueBox(
      formatC(total_parts_used_in_service(), format="d", big.mark=',')
      ,"Total Parts Used in Service"
      ,icon = icon("stats",lib='glyphicon')
      ,color = "blue")  
  })
  
  #Individual Machine Services
  output$value13 <- renderValueBox({
    valueBox(
      "Services per Machine"
      ,"by Serial Number"
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "red"
      ,width = 10)  
  })
  
  
  #Model Machine Services
  output$value14 <- renderValueBox({
    valueBox(
      "Services per Machine"
      ,"by Model Number"
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "red"
      ,width = 10)  
  })
  
  #Cluster Properties
  output$value15 <- renderValueBox({
    valueBox(
      "Usage Categories"
      ,"Obtained by K-means Clustering"
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "blue"
      ,width = 10)  
  })
  
  # Create scatterplot object the plotOutput function is expecting
  #Overall Plot view (Call Category)
  output$barplot_overall1 <- renderPlot({
    ggplot(data = avg_sc_vis(), aes_string(x = reorder(avg_sc_vis()$service_call_category, avg_sc_vis()$avg_sc), y = avg_sc_vis()$avg_sc)) +
      geom_bar(stat="identity",fill = "#2977BA") + 
      geom_text(aes(label=paste(round(avg_sc,digits=0),"€")), hjust=ifelse(avg_sc_vis()$avg_sc > 0.5,0,1.5),size = 3, colour = "#000000") +
      xlab("Service Call Category") +
      ylab("Average Service Costs (Euros)") +
      #ggtitle("Average Service Cost per Category") + 
      theme(axis.text.x = element_text(hjust=1, size= 7, margin(0,5,0,5,'pt')), 
            #plot.title = element_text(size = 20,hjust = 0.5,face='bold'), 
            panel.background  = element_rect(fill = "white"),
            axis.title.x=element_text(vjust= -2),
            axis.title.y=element_text(vjust= 5),
            plot.margin = margin(.5, .5, .5, .5, "cm"),  
            plot.background = element_rect(
              fill = "grey90",
              colour = "black",
              size = 1
            ) ,
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gray"),
            panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gray")
      ) + coord_flip()
  })
  
  #Plot for total service count per category 
  output$barplot_overall2 <- renderPlot({
    ggplot(data = total_sc_vis(), aes_string(x = reorder(total_sc_vis()$service_call_category, total_sc_vis()$total_sc), y = total_sc_vis()$total_sc)) +
      geom_bar(stat="identity",fill = "#B23F41") + 
      geom_text(aes(label=paste(round(total_sc,digits=0))), hjust=ifelse(total_sc_vis()$total_sc > 0.5,0,1.5),size = 3, colour = "#000000") +
      xlab("Service Call Category") +
      ylab("Service Count") +
      #ggtitle("Total Service Count") + 
      theme(axis.text.x = element_text(hjust=1, size= 7, margin(0,5,0,5,'pt')), 
            #plot.title = element_text(size = 20,hjust = 0.5,face='bold'),  
            panel.background  = element_rect(fill = "white"),
            axis.title.x=element_text(vjust= -2),
            axis.title.y=element_text(vjust= 5),
            plot.margin = margin(.5, .5, .5, .5, "cm"),  
            plot.background = element_rect(
              fill = "grey90",
              colour = "black",
              size = 1
            ) ,
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gray"),
            panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gray")
      ) + coord_flip()
  })
  
  ###Overall Plot view by Sales Organisation #####
  #Overall Plot view
  output$barplot_overall1_1 <- renderPlot({
    ggplot(data = avg_sc_vis2(), aes_string(x = reorder(avg_sc_vis2()$sales_organisation, avg_sc_vis2()$avg_sc), y = avg_sc_vis2()$avg_sc)) +
      geom_bar(stat="identity",fill = "#2977BA") + 
      geom_text(aes(label=paste(round(avg_sc,digits=0),"€")), hjust=ifelse(avg_sc_vis2()$avg_sc > 0.5,0,1.5),size = 3, colour = "#000000") +
      xlab("Sales Organisation") +
      ylab("Average Service Costs (Euros)") +
      #ggtitle("Average Service Cost per Category") + 
      theme(axis.text.x = element_text(hjust=1, size= 7, margin(0,5,0,5,'pt')), 
            #plot.title = element_text(size = 20,hjust = 0.5,face='bold'), 
            panel.background  = element_rect(fill = "white"),
            axis.title.x=element_text(vjust= -2),
            axis.title.y=element_text(vjust= 5),
            plot.margin = margin(.5, .5, .5, .5, "cm"),  
            plot.background = element_rect(
              fill = "grey90",
              colour = "black",
              size = 1
            ) ,
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gray"),
            panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gray")
      ) + coord_flip()
  })
  
  #Plot for total service count per sales_organisation
  output$barplot_overall2_1 <- renderPlot({
    ggplot(data = total_sc_vis2(), aes_string(x = reorder(total_sc_vis2()$sales_organisation, total_sc_vis2()$total_sc), y = total_sc_vis2()$total_sc)) +
      geom_bar(stat="identity",fill = "#B23F41") + 
      geom_text(aes(label=paste(round(total_sc,digits=0))), hjust=ifelse(total_sc_vis2()$total_sc > 0.5,0,1.5),size = 3, colour = "#000000") +
      xlab("Sales Organisation") +
      ylab("Service Count") +
      #ggtitle("Total Service Count") + 
      theme(axis.text.x = element_text(hjust=1, size= 7, margin(0,5,0,5,'pt')), 
            #plot.title = element_text(size = 20,hjust = 0.5,face='bold'),  
            panel.background  = element_rect(fill = "white"),
            axis.title.x=element_text(vjust= -2),
            axis.title.y=element_text(vjust= 5),
            plot.margin = margin(.5, .5, .5, .5, "cm"),  
            plot.background = element_rect(
              fill = "grey90",
              colour = "black",
              size = 1
            ) ,
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gray"),
            panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gray")
      ) + coord_flip()
  })
  
  
  #Plot for avg service cost by sales organisation (filter by sales category)
  output$barplot1 <- renderPlot({
    ggplot(data = avg_sc_category_vis(), aes_string(x = reorder(avg_sc_category_vis()$sales_organisation, avg_sc_category_vis()$avg_service_cost), y = avg_sc_category_vis()$avg_service_cost)) +
      geom_bar(stat="identity",fill = "#2977BA") + 
      geom_text(aes(label=paste(round(avg_service_cost,digits=0),"€")), hjust=ifelse(avg_sc_category_vis()$avg_service_cost > 0.5,0,1.5),size = 3, colour = "#000000") +
      xlab("Sales Organisation") +
      ylab("Average Service Costs (Euros)") +
      #ggtitle("Average Service Cost by Organisation") + 
      theme(axis.text.x = element_text(hjust=1, size= 7, margin(0,5,0,5,'pt')), 
            #plot.title = element_text(size = 20,hjust = 0.5,face='bold'), 
            panel.background  = element_rect(fill = "white"),
            axis.title.x=element_text(vjust= -2),
            axis.title.y=element_text(vjust= 5),
            plot.margin = margin(.5, .5, .5, .5, "cm"),  
            plot.background = element_rect(
                fill = "grey90",
                colour = "black",
                size = 1
               ) ,
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gray"),
            panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gray")
            ) + coord_flip()
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$barplot2 <- renderPlot({
    ggplot(data = total_sc_category_vis(), aes_string(x = reorder(total_sc_category_vis()$sales_organisation, total_sc_category_vis()$total_service_count), y = total_sc_category_vis()$total_service_count)) +
      geom_bar(stat="identity",fill = "#B23F41") + 
      geom_text(aes(label=paste(round(total_service_count,digits=0))), hjust=ifelse(total_sc_category_vis()$total_service_count > 0.5,0,1.5),size = 3, colour = "#000000") +
      xlab("Sales Organisation") +
      ylab("Service Count") +
      #ggtitle("Total Service Count") + 
      theme(axis.text.x = element_text(hjust=1, size= 7, margin(0,5,0,5,'pt')), 
            #plot.title = element_text(size = 20,hjust = 0.5,face='bold'),  
            panel.background  = element_rect(fill = "white"),
            axis.title.x=element_text(vjust= -2),
            axis.title.y=element_text(vjust= 5),
            plot.margin = margin(.5, .5, .5, .5, "cm"),  
            plot.background = element_rect(
              fill = "grey90",
              colour = "black",
              size = 1
            ) ,
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gray"),
            panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gray")
      ) + coord_flip()
  })
  
  ##### Organisation level visualisations
  #Plot for avg service cost by category (filter by sales organisation)
  output$barplot3 <- renderPlot({
    ggplot(data = avg_sc_organisation_vis(), aes_string(x = reorder(avg_sc_organisation_vis()$service_call_category, avg_sc_organisation_vis()$avg_service_cost), y = avg_sc_organisation_vis()$avg_service_cost)) +
      geom_bar(stat="identity",fill = "#2977BA") + 
      geom_text(aes(label=paste(round(avg_service_cost,digits=0))), hjust=ifelse(avg_sc_organisation_vis()$avg_service_cost > 0.5,0,1.5),size = 3, colour = "#000000") +
      xlab("Service Call Category") +
      ylab("Average Service Costs (Euros)") +
      #ggtitle("Average Service Cost by Organisation") + 
      theme(axis.text.x = element_text(hjust=1, size= 7, margin(0,5,0,5,'pt')), 
            #plot.title = element_text(size = 20,hjust = 0.5,face='bold'), 
            panel.background  = element_rect(fill = "white"),
            axis.title.x=element_text(vjust= -2),
            axis.title.y=element_text(vjust= 5),
            plot.margin = margin(.5, .5, .5, .5, "cm"),  
            plot.background = element_rect(
              fill = "grey90",
              colour = "black",
              size = 1
            ) ,
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gray"),
            panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gray")
      ) + coord_flip()
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$barplot4 <- renderPlot({
    ggplot(data = total_sc_organisation_vis(), aes_string(x = reorder(total_sc_organisation_vis()$service_call_category, total_sc_organisation_vis()$total_service_count), y = total_sc_organisation_vis()$total_service_count)) +
      geom_bar(stat="identity",fill = "#B23F41") + 
      geom_text(aes(label=paste(round(total_service_count,digits=0))), hjust=ifelse(total_sc_organisation_vis()$total_service_count > 0.5,0,1.5),size = 3, colour = "#000000") +
      xlab("Service Call Category") +
      ylab("Service Count") +
      #ggtitle("Total Service Count") + 
      theme(axis.text.x = element_text(hjust=1, size= 7, margin(0,5,0,5,'pt')), 
            #plot.title = element_text(size = 20,hjust = 0.5,face='bold'),  
            panel.background  = element_rect(fill = "white"),
            axis.title.x=element_text(vjust= -2),
            axis.title.y=element_text(vjust= 5),
            plot.margin = margin(.5, .5, .5, .5, "cm"),  
            plot.background = element_rect(
              fill = "grey90",
              colour = "black",
              size = 1
            ) ,
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gray"),
            panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gray")
      ) + coord_flip()
  })
  
  output$table <- DT::renderDataTable(parts_data_table())
  
  output$service_machines_individual <- DT::renderDataTable(total_services_per_machine())
  
  output$service_machines_model <- DT::renderDataTable(total_services_per_model())
  
  #total_services_per_model_vis
  output$total_service_model_vis <- renderPlot({
    ggplot(data = total_services_per_model_vis(), aes_string(x = reorder(total_services_per_model_vis()$service_call_category, total_services_per_model_vis()$total_service_count), y = total_services_per_model_vis()$total_service_count)) +
      geom_bar(stat="identity",fill = "#B23F41") + 
      geom_text(aes(label=paste(round(total_service_count,digits=0))), hjust=ifelse(total_services_per_model_vis()$total_service_count > 0.5,0,1.5),size = 3, colour = "#000000") +
      xlab("Service Call Category") +
      ylab("Service Count") +
      #ggtitle("Total Service Count") + 
      theme(axis.text.x = element_text(hjust=1, size= 7, margin(0,5,0,5,'pt')), 
            #plot.title = element_text(size = 20,hjust = 0.5,face='bold'),  
            panel.background  = element_rect(fill = "white"),
            axis.title.x=element_text(vjust= -2),
            axis.title.y=element_text(vjust= 5),
            plot.margin = margin(.5, .5, .5, .5, "cm"),  
            plot.background = element_rect(
              fill = "grey90",
              colour = "black",
              size = 1
            ) ,
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gray"),
            panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gray")
      ) + coord_flip()
  })
  
  output$clusters <- DT::renderDataTable(cluster())
  
  output$kmeans <- renderPlotly({
    plot_ly(kmeans_data(), x = ~average_time_between_service, y = ~breakdown, z = ~days_to_first_serv, marker = list(size = 3),color = ~usage_class, colors = c('#00AFBB','#fed65e', '#FC4E07')) %>%
      add_markers() %>%
      layout(title = 'Cluster Visualisation',
             scene = list(xaxis = list(title = 'Avg Days b/w Services',
                                       #gridcolor = 'rgb(255, 255, 255)',
                                       zerolinewidth = 1,
                                       gridwith = 2
             ),
             yaxis = list(title = 'Breakdowns'),
             #gridcolor = 'rgb(255, 255, 255)'),
             zaxis = list(title = 'Days to FS')),
             #gridcolor = 'rgb(255, 255, 255)'),
             paper_bgcolor = 'rgb(243, 243, 243)',
             plot_bgcolor = 'white',
             autosize = F,width = 1000, height = 800, ypad = 0
      ) 
  })
  
  #Breakdown per Machine
  output$exp1 <- renderPlot({
    ggplot(df7, aes(x=breakdown)) + 
    geom_histogram(binwidth = 5, color = "black", fill = "grey") + 
    geom_vline(aes(xintercept = mean(breakdown)), color = "#FC4E07", linetype = "dashed", size = 1) + 
    theme_minimal() + 
    xlab("Number of Breakdowns") + ylab("Machine Count") +
    theme(
      panel.background  = element_rect(fill = "white"),
      axis.title.x=element_text(vjust= -2),
      axis.title.y=element_text(vjust= 5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),  
      plot.background = element_rect(
        fill = "grey90",
        colour = "black",
        size = 1
       
    ))})
  #majority of machines have less than 10 breakdowns, highly right skewed
  
  #Average Days Between Services
  output$exp2 <- renderPlot({
    ggplot(df7, aes(x=average_time_between_service)) + 
    geom_histogram(binwidth = 30, color = "black", fill = "grey") + 
    geom_vline(aes(xintercept = mean(average_time_between_service)), color = "#FC4E07", linetype = "dashed", size = 1) + 
    theme_minimal() +   
    xlab("Average Days") + ylab("Machine Count") +
    theme(
      panel.background  = element_rect(fill = "white"),
      axis.title.x=element_text(vjust= -2),
      axis.title.y=element_text(vjust= 5),
      plot.margin = margin(.5, .5, .5, .5, "cm"),  
      plot.background = element_rect(
        fill = "grey90",
        colour = "black",
        size = 1
       
    ))})
  #majority of machines get serviced around once every half year - yearly , somewhat normal distribution but right skewed
  
  #Average Days to First Service
  output$exp3 <- renderPlot({
    ggplot(df7, aes(x=days_to_first_serv)) + geom_histogram(binwidth = 30, color = "black", fill = "grey") + 
    geom_vline(aes(xintercept = mean(days_to_first_serv)), color = "#FC4E07", linetype = "dashed", size = 1) + 
    theme_minimal() +  
    xlab("Days") + ylab("Machine Count") +
    theme(          panel.background  = element_rect(fill = "white"),
                    axis.title.x=element_text(vjust= -2),
                    axis.title.y=element_text(vjust= 5),
                    plot.margin = margin(.5, .5, .5, .5, "cm"),  
                    plot.background = element_rect(
                      fill = "grey90",
                      colour = "black",
                      size = 1
                     )) })
  #majority of machines have their first service around the first 6 months - this might be because of accessories installation
  #highly right skewed
  
  #Average Service Costs
  output$exp4 <- renderPlot({
    ggplot(df7, aes(x=avg_serv_costs)) + geom_histogram(binwidth = 100, color = "black", fill = "grey") + 
    geom_vline(aes(xintercept = mean(avg_serv_costs)), color = "#FC4E07", linetype = "dashed", size = 1) + 
    theme_minimal() + 
    xlab("Euros") + ylab("Machine Count") +
    theme(panel.background  = element_rect(fill = "white"),
          axis.title.x=element_text(vjust= -2),
          axis.title.y=element_text(vjust= 5),
          plot.margin = margin(.5, .5, .5, .5, "cm"),  
          plot.background = element_rect(
            fill = "grey90",
            colour = "black",
            size = 1
          ) )})
  #most service costs range from 0 to 1000, shows a normal distribution, average at around 400 euros
  
  output$exp5 <- renderPlot({
    ggplot(count_of_sizes(), aes(x=machine_size, y=n)) + geom_bar(stat="identity",color="black",fill = "#B23F41") + 
    geom_text(aes(label=paste(round(n,digits=0))), vjust=-0.5,color = "#000000") +
    xlab("Sizes") +
    ylab("Machine Count") +
    #ggtitle("Total Service Count") + 
    theme(axis.text.x = element_text(hjust=1, size= 7, margin(0,5,0,5,'pt')), 
          #plot.title = element_text(size = 20,hjust = 0.5,face='bold'),  
          panel.background  = element_rect(fill = "white"),
          axis.title.x=element_text(vjust= -2),
          axis.title.y=element_text(vjust= 5),
          plot.margin = margin(.5, .5, .5, .5, "cm"),  
          plot.background = element_rect(
            fill = "grey90",
            colour = "black",
            size = 1
          ) ,
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray"),
          panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray")
    )})
  
  output$exp6 <- renderPlot({
    ggplot(count_of_energy_types(), aes(x=energy_type, y=n)) + geom_bar(stat="identity",color = "black",fill = c("#B23F41", "#2977BA") ) + 
    geom_text(aes(label=paste(round(n,digits=0))), vjust=-0.5,color = "#000000") +
    xlab("Energy Type") +
    ylab("Machine Count") +
    #ggtitle("Total Service Count") + 
    theme(axis.text.x = element_text(hjust=1, size= 7, margin(0,5,0,5,'pt')), 
          #plot.title = element_text(size = 20,hjust = 0.5,face='bold'),  
          panel.background  = element_rect(fill = "white"),
          axis.title.x=element_text(vjust= -2),
          axis.title.y=element_text(vjust= 5),
          plot.margin = margin(.5, .5, .5, .5, "cm"),  
          plot.background = element_rect(
            fill = "grey90",
            colour = "black",
            size = 1
          ) ,
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray"),
          panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray"))})
  
  output$exp7 <- renderPlot({ggplot(
    count_of_machine_types(), aes(x=machine_type, y=n)) + geom_bar(stat="identity",color = 
                                                                     "black", fill = c("#B23F41", "#2977BA")) + 
    geom_text(aes(label=paste(round(n,digits=0))), vjust=-0.5,color = "#000000") +
    xlab("Machine Type") +
    ylab("Machine Count") +
    #ggtitle("Total Service Count") + 
    theme(axis.text.x = element_text(hjust=1, size= 7, margin(0,5,0,5,'pt')), 
          #plot.title = element_text(size = 20,hjust = 0.5,face='bold'),  
          panel.background  = element_rect(fill = "white"),
          axis.title.x=element_text(vjust= -2),
          axis.title.y=element_text(vjust= 5),
          plot.margin = margin(.5, .5, .5, .5, "cm"),  
          plot.background = element_rect(
            fill = "grey90",
            colour = "black",
            size = 1
          ) ,
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray"),
          panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray"))})
  
  output$exp8 <- renderPlot({
    ggplot(count_of_installation_year(), aes(x=installation_year, y=n)) + geom_bar(stat="identity", color = "black",fill = "#B23F41") + 
    geom_text(aes(label=paste(round(n,digits=0))), vjust=-0.5,color = "#000000") +
    xlab("Installation Year") +
    ylab("Machine Count") +
    #ggtitle("Total Service Count") + 
    theme(axis.text.x = element_text(hjust=1, size= 7, margin(0,5,0,5,'pt')), 
          #plot.title = element_text(size = 20,hjust = 0.5,face='bold'),  
          panel.background  = element_rect(fill = "white"),
          axis.title.x=element_text(vjust= -2),
          axis.title.y=element_text(vjust= 5),
          plot.margin = margin(.5, .5, .5, .5, "cm"),  
          plot.background = element_rect(
            fill = "grey90",
            colour = "black",
            size = 1
          ) ,
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray"),
          panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray"))})
  
}

shinyApp(ui, server)

