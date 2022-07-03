pacman::p_load('shiny', 'shinythemes', 
              'plotly', 'tidyverse', 
              'treemapify', 'ggthemes', 'ggplot2', 'sf', 'tmap', 'leaflet')


Fin <- read_rds('data/Fin.rds')
occupancy_DT <- readRDS("data/occupancy_DT.rds")
cust_rev <- readRDS("data/cust_rev.rds")
jobs_employers <- read_csv("data/jobs_employers.csv")
jobs_employers_1 <- read_csv("data/jobs_employers_1.csv")
jobs_employers_1$jobs_count <- as.character(jobs_employers_1$jobs_count)
buildings <- read_sf("data/Buildings.csv",
                     options = "GEOM_POSSIBLE_NAMES=location")
employers <- read_sf("data/employers_revised.csv",
                     options = "GEOM_POSSIBLE_NAMES=location")
employers$turnovers <- sub('0', '0.5', employers$turnovers)
employers$turnovers <- as.numeric(employers$turnovers)
employers$level <- factor(employers$level, levels=c('Low Rate', 'Medium Rate', 'High Rate'))


ui <- navbarPage(
  title = "VC Economic Times",
  fluid = TRUE,
  theme=shinytheme("united"),
  id = "navbarID",
  tabPanel("Home",
           shiny::h3("Welcome to VAST Challenge 2022 Economic Times."),
           tags$br(),
           img(src='economics.jpg', align = "centre",
               style="height:400px; width:100%;"),
           tags$br(),
           tags$br(),
           shiny::h4("In this web-based application, we aim to build an interactive vizualisation tool using open-source R on Shiny.io.
                      As part of Challenge 3 of VAST Challenge 2022, 
                      various visual analytic techniques will be used to explore the economic patterns of the City of Engagement, Ohio USA.
                      Please click on the respective tabs from the navigation bar to find out more."),
           ),
  navbarMenu(tags$b(tags$span(style="color:black", "Business")),
             tabPanel("Customers - Revenue Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "Variable_Aloy_1",
                                      label = "Point Shape:",
                                      choices = list("Square" = 15,
                                                     "Circle" = 16,
                                                     "Triangle" = 17,
                                                     "Diamond" = 18)),
                          sliderInput(inputId = "Variable_Aloy_4",
                                      label = "Month",
                                      min = 1,
                                      max = 12,
                                      value = 1,
                                      step = 1),
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("scatterPlot")
                        )
                      )
             ),
             tabPanel("Occupancy Rate Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "Variable_Aloy_2",
                                      label = "Pub Id:",
                                      choices = list("442" = 442,
                                                     "443" = 443,
                                                     "444" = 444,
                                                     "892" = 892,
                                                     "893" = 893,
                                                     "894" = 894,
                                                     "1342" = 1342,
                                                     "1343" = 1343,
                                                     "1344" = 1344,
                                                     "1798" = 1798,
                                                     "1799" = 1799,
                                                     "1800" = 1800)),
                          selectInput(inputId = "Variable_Aloy_3",
                                      label = "line type (2022-03-01 to 2022-03-31)",
                                      choices = list("twodash" = "twodash",
                                                     "solid" = "solid",
                                                     "longdash" = "longdash",
                                                     "dotted" = "dotted",
                                                     "dotdash" = "dotdash",
                                                     "dashed" = "dashed"),
                                      selected = "solid"),
                          selectInput(inputId = "Variable_Aloy_5",
                                      label = "line type (2023-04-25 to 2023-05-24)",
                                      choices = list("twodash" = "twodash",
                                                     "solid" = "solid",
                                                     "longdash" = "longdash",
                                                     "dotted" = "dotted",
                                                     "dotdash" = "dotdash",
                                                     "dashed" = "dashed"),
                                      selected = "dashed"),
                          sliderInput(inputId = "Variable_Aloy_6",
                                      label = "Line Thickness",
                                      min = 0.5,
                                      max = 3,
                                      value = 0.5,
                                      step = 0.5)
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("linePlot")
                        )
                      )
             )),
  navbarMenu(tags$b(tags$span(style="color:black", "Financials")),
             tabPanel("Wage - Cost of Living Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = 'variable_yn_1',
                                      label = 'Age',
                                      min = 18,
                                      max = 60,
                                      value = c(25,35)),
                          checkboxGroupInput(inputId = 'variable_yn_2',
                                             label = 'Have Kids?',
                                             choices = c('Yes' = 'TRUE',
                                                         'No' = 'FALSE'),
                                             selected = 'TRUE'),
                          checkboxGroupInput(inputId = 'variable_yn_3',
                                             label = 'Education Level',
                                             choices = c('Graduate' = 'Graduate',
                                                         'Bachelor' = 'Bachelors',
                                                         'High School / College' = 'HighSchoolOrCollege',
                                                         'Low' = 'Low'),
                                             selected = 'Bachelors')
                          
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("linegraph"),
                          plotOutput('ratio')
                        )
                      ))
            
  ),
  navbarMenu(tags$b(tags$span(style="color:black", "Employment")),
             tabPanel("Average Hourly Rate Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "variable_che_1",
                                      label = "Colour Scheme:",
                                      choices = list("Blues" = "Blues",
                                                     "Red-Purple" = "RdPu",
                                                     "Yellow-Brown" = "YlOrBr",
                                                     "Yellow-Green" = "YlGn",
                                                     "Orange-Red" = "OrRd"),
                                      selected = "Blues"),
                          radioButtons(inputId = "variable_che_2",
                                       label = "Direction:",
                                       choices = list("Forward" = "1",
                                                      "Backward" = "-1"),
                                       selected = "1")
                        ),
                        mainPanel(
                          plotOutput("treemapPlot")
                        )
                      )
              ),
             tabPanel("Employers - Jobs Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "variable_che_3",
                                      label = "Plot Type:",
                                      choices = list("Bar Plot" = "bar",
                                                     "Dot Plot" = "scatter",
                                                     "Line Plot" = "violin"),
                                      selected = "bar"
                          ),
                          checkboxInput(inputId = "showData",
                                        label = "Show data table",
                                        value = TRUE)
                        ),
                        mainPanel(
                          plotlyOutput("barPlot"),
                          DT::dataTableOutput(outputId = "table")
                        )
                      ))
             ,
             tabPanel("Turnover Rate Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(inputId = "variable_che_4",
                                       label = "Colour Scheme:",
                                       choices = list("Orange" = "Oranges",
                                                      "Green" = "Greens",
                                                      "Purple" = "Purples"),
                                       selected = "Oranges")
                        ),
                        mainPanel(
                          leafletOutput("mapPlot"),
                          textOutput("text")
                        )
                      ))
             ),
  navbarMenu("About",
             tabPanel("User Guide",
                      tags$iframe(style="height:600px; width:100%; scrolling = yes",
                                  src="User_Guide.pdf")
             ),
             tabPanel("Proposal",
                      tags$iframe(style="height:600px; width:100%; scrolling = yes",
                                  src="Proposal.pdf")
             ),
             tabPanel("Poster",
                      img(src='Poster.png', align = "centre",
                          style="height:600px; width:100%;")
             ),
             tabPanel("About Us",
                      shiny::h1("Group Members:"),
                      tags$br(),
                      img(src='group_members.png', align = "centre",
                          style="height:300px; width:500px;"),
                      shiny::h3("1. Chu Yi-Ning"),
                      shiny::h5("Email: ynchu.2021@mitb.smu.edu.sg"),
                      tags$a(href="https://www.linkedin.com/in/yi-ning-chu", 
                             "Check me out!"),
                      tags$br(),
                      shiny::h3("2. Aloysius Teng"),
                      shiny::h5("Email: aloysiust.2021@mitb.smu.edu.sg"),
                      tags$a(href="https://www.linkedin.com/in/aloysius-teng-32477716a", 
                             "Check me out!"),
                      tags$br(),
                      shiny::h3("3. Che Xuan"),
                      shiny::h5("Email: xuan.che.2021@mitb.smu.edu.sg"),
                      tags$a(href="https://www.linkedin.com/in/jacob-che-xuan-b646a9123/", 
                             "Check me out!")
                      ))
)

#========================#
###### Shiny Server ######
#========================#

server <- function(input, output, session){
  
  ##### Shiny Server: Customers - Revenue Analysis #####
  filtered_1 <- reactive({
    dplyr::filter(cust_rev, timestep == input$Variable_Aloy_4)})
  
  lst <- c('Mar 22', 'Apr 22', 'May 22', 'Jun 22', 'Jul 22', 'Aug 22', 'Sep 22', 'Oct 22', 'Nov 22', 'Dec 22', 'Jan 23', 'Feb 23', 'Mar 23', 'Apr 23', 'May 23')
  
  month_year <- reactive({lst[input$Variable_Aloy_4]})
  sizeMax <- reactive({(max(dplyr::filter(cust_rev, timestep == input$Variable_Aloy_4)$revenue)/max(cust_rev$revenue))*12})
  sizeMin <- reactive({(min(dplyr::filter(cust_rev, timestep == input$Variable_Aloy_4)$revenue)/min(cust_rev$revenue))*2})
  
  output$scatterPlot <- renderPlot({
    ggplot(filtered_1(), aes(x = customers, y = revenuePerCustomer,
                           size = revenue,
                           colour = pubId)) +
      geom_point(alpha = 0.7, 
                 show.legend = TRUE,
                 shape=as.numeric(input$Variable_Aloy_1)) +
      scale_size(range = c(sizeMin(), sizeMax()))+
      scale_colour_brewer(palette = "Set3")+
      labs(x = 'Number of customers', 
           y = 'Revenue per Customer')+
      xlim(0, 7000) +  
      ylim(10, 25) +  
      ggtitle(month_year())
  })
  
  ##### Shiny Server: Occupancy Rate Analysis #####
    filtered_2 <- reactive({
      dplyr::filter(occupancy_DT, pubId == input$Variable_Aloy_2)})
    
    output$linePlot <- renderPlot({
      ggplot(data=filtered_2(),
             aes(x = base_hour)) +
        geom_line(aes(y = first), 
                  linetype = input$Variable_Aloy_3,
                  size = input$Variable_Aloy_6) +
        geom_line(aes(y = last), 
                  linetype = input$Variable_Aloy_5,
                  size = input$Variable_Aloy_6)+
        coord_cartesian(xlim=c(0,23), ylim=c(0.0,1.0))+
        labs(x = 'Hour', 
             y = 'Average Occupancy Rate')
    })
  
  ##### Shiny Server: Wage - Cost of Living Analysis #####  
  ## filter the data
  filtered <- reactive({
    dplyr::filter(Fin, age == input$variable_yn_1 & 
                    haveKids == input$variable_yn_2 &
                    educationLevel == input$variable_yn_3) %>%
      group_by(yearmonth) %>%
      summarise(Wage = median(Wage),
                CostofLiving = median(CostofLiving)) %>%
      mutate(ratio = CostofLiving/Wage)
  })
  
  output$linegraph <- renderPlot({
    ggplot(filtered(), aes(x = yearmonth)) +
      geom_line(aes(y = Wage, color = 'Wage'), size = 1) +
      geom_line(aes(y = CostofLiving, color = 'Cost of Living'), size = 1) +
      ylab('Median of Amount (USD)') +
      scale_color_manual(name = 'Category', values = c('Wage' = 'aquamarine3', 
                                                       'Cost of Living' = 'darksalmon')) +
      theme_stata()
    
    
  })
  
  output$ratio <- renderPlot({
    ggplot(filtered(), aes(x = yearmonth)) +
      geom_line(aes(y = ratio, color = '% of cost of living accounts for wage'),
                size = 1) +
      ylab('Cost of Living / Wage') +
      scale_color_manual(name = ' ', values = c('% of cost of living accounts for wage' =  'cornflowerblue')) +
      theme_stata()
  })
  
  
##### Shiny Server: Average Hourly Rate Analysis #####
  output$treemapPlot <- renderPlot({
    ggplot(jobs_employers, aes(area = avg_hourly_rate, fill = avg_hourly_rate, label = avg_hourly_rate)) +
      geom_treemap() +
      geom_treemap_text(fontface = "italic", colour = "black", place = "centre", grow = F, size = 10) +
      scale_fill_distiller(name = "Avg Hourly Rate", palette = input$variable_che_1, direction = input$variable_che_2)
  })


##### Shiny Server: Employers - Jobs Analysis #####
  output$barPlot <- renderPlotly({
    p <- jobs_employers_1 %>%
      plot_ly( x = ~jobs_count,
               y = ~companies_count,
               type = input$variable_che_3,
               orientation = 'v') %>%
      layout(xaxis = list(categoryorder = "total descending",
                          title = 'No. of Jobs Required'),
             yaxis = list(title = 'No. of Employers'))

  })

  output$table <- DT::renderDataTable({
    d <- event_data("plotly_click")
    if(input$showData){
      DT::datatable(data = d[, c(4, 3)],
                    colnames = c('There are ___ numbers of employers', 'having ___ numbers of jobs'),
                    rownames = FALSE,
                    options = list(
                      columnDefs = list(list(className = 'dt-center', targets = 0:1))))
    }

  })


##### Shiny Server: Turnover Rate Analysis #####
  output$mapPlot <- renderLeaflet({
    map <- tm_shape(buildings)+
      tm_polygons(col = "white",
                  size = 1,
                  border.col = "grey",
                  border.lwd = 1) +
      tm_shape(employers) +
      tm_dots(col = "level", size = "turnovers",
              palette = input$variable_che_4)

    tmap_leaflet(map)

  })

  output$text <- renderText({"Note: Turnover rate of 0 is being represented by 0.5 on this map"})
  
  
}

shinyApp(ui = ui, server = server)
