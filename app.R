#
# This is a Shiny Dashboard for Superstore Sales Dataset is a part of a Global Superstore dataset
# found in Keggle https://www.kaggle.com/datasets/rohitsahoo/sales-forecasting.
#
# You can run the application by clicking the 'Run App' button above.
#
   

## libraries to install
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(highcharter)
library(dplyr)
library(plotly)
library(RColorBrewer)
library(ggplot2)
library(tidyr)
library(lubridate)
library(stringr) 


# Define UI for application that draws a histogram
ui <- dashboardPage(
  title = "Superstore Sales DashBoard",
  dashboardHeader(title = div(img(src="superStore_Logo.png", height = 45, width = 50),"Superstore Sales DashBoard"),titleWidth  = "25%"),
  dashboardSidebar(collapsed = F, 
                   dateInput("reportDate", label = h3("Report Date"), value = "2018-12-31"),
                   sidebarMenu(
                     menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
                     menuItem("Customer Segment", tabName = "customer", icon = icon("users")),
                     menuItem("Product Items", icon = icon("boxes-stacked"), tabName = "item"),
                     menuItem("Shipping Mode", icon = icon("truck-fast"), tabName = "shipping")
                     )
                   ),
  
  dashboardBody(
    ### CSS files for the 4 value boxes in the summary tab
    tags$head(tags$style("#revenueYTD,#revenueMonthly,#OrdersYTD,#OrdersMonthly{color: gray;
                             font-size: 2em;
                         font-weight: bold;
                         }"
                         )
              ),
    
    tabItems(
      tabItem(tabName = "summary",
    
      fluidRow(# 4 Value boxes in the summary tab
        box(title=strong("Sales Revenue YTD"),width = 3, height=150, solidHeader = TRUE, status = "primary",div(icon("coins"),style="float: right;clear: right;font-size: 400%;margin: 0px 0px 0px 20px;padding:0px;opacity: 0.2"),
            textOutput("revenueYTD"),div(hr(style="margin-bottom:0px")),
            div(strong(htmlOutput("revGrowthYTD")),style="text-align: right")
            
            ),
        
        box(title=strong("Sales Revenue Month"),width = 3, height=150, solidHeader = TRUE, status = "primary",div(icon("coins"),style="float: right;clear: right;font-size: 400%;margin: 0px 0px 0px 20px;padding:0px;opacity: 0.2"),
            textOutput("revenueMonthly"),div(hr(style="margin-bottom:0px")),
            div(strong(htmlOutput("revGrowthM")),style="text-align: right")
            
        ),
        
        box(title=strong("Orders YTD"),width = 3, height=150, solidHeader = TRUE, status = "primary",div(icon("cart-shopping"),style="float: right;clear: right;font-size: 400%;margin: 0px 0px 0px 20px;padding:0px;opacity: 0.2"),
            textOutput("OrdersYTD"),div(hr(style="margin-bottom:0px")),
            div(strong(htmlOutput("ordGrowthYTD")),style="text-align: right")
            
        ),
        
        box(title=strong("Orders Month"),width = 3, height=150, solidHeader = TRUE, status = "primary",div(icon("cart-shopping"),style="float: right;clear: right;font-size: 400%;margin: 0px 0px 0px 20px;padding:0px;opacity: 0.2"),
            textOutput("OrdersMonthly"),div(hr(style="margin-bottom:0px")),
            div(strong(htmlOutput("ordGrowthM")),style="text-align: right")
        )
            
      ),
    
    
      fluidRow(
        column(width = 6,
               fluidRow(## 2 Customer and Items bar charts and Monthly revenue scatter plot
                      box(title=strong("Revenue by Customer Segment"),width = 6, height=250,solidHeader = TRUE, status = "primary",
                          plotlyOutput("bySegment",width = "100%", height=190)
                      ),
               
               
                      box(title=strong("Revenue by Item Category"),width = 6, height=250, solidHeader = TRUE, status = "primary",
                          plotlyOutput("byCategory",width = "100%", height=190)
                      ),
               ),
               fluidRow(
                      box(title=strong("Revenue Monthly Distribution"),width = 12, height=360,solidHeader = TRUE, status = "primary",
                          plotlyOutput("byMonthly",width = "100%", height=300)
                          
                      ),
               ),
        ),
        column(width = 6,## Distribution by State
          box(title=strong("Revenue Distribution by States"),width = NULL, height=470, solidHeader = TRUE, status = "primary",
              plotlyOutput("byState",width = "100%", height=400)
          )
        )
        
      ),
      
    ),
      
    tabItem(tabName = "customer",
            fluidRow(## Boxes of Customer segment tab
              column(width = 6,
                     
                     fluidRow(
                       box(title=strong("Top 10 Customers"),width = 12, height=650,solidHeader = TRUE, status = "primary",
                           plotlyOutput("topCustomers",width = "100%", height=550)
                           
                       )
                     ),
              ),
              column(width = 6,
                     fluidRow(
                       box(title=strong("Monthly Customer Activities"),width = 12, height=315,solidHeader = TRUE, status = "primary",
                           tabBox(width = "100%",height="100%",
                             tabPanel("Revenue", plotlyOutput("customerActivity.Rev",width = "100%", height=190)),
                             tabPanel("Order Count", plotlyOutput("customerActivity.count",width = "100%", height=190))
                           ),
                       ),
                       
                     ),
                     fluidRow(
                       box(title=strong("Customer Order Distribution"),width = 12, height=315,solidHeader = TRUE, status = "primary",
                           plotlyOutput("customerOrdersDist",width = "100%", height=250)
  
                       )
                     )
              )
            )
      
    ),
    
    tabItem(tabName = "item",
            fluidRow(## Boxes of Product Items tab
              column(width = 6,
                     
                     fluidRow(
                       box(title=strong("Revenue Generation Distribution by Product Items"),width = 12, height=650,solidHeader = TRUE, status = "primary",
                           plotlyOutput("itemCat_SubCat",width = "100%", height=550)
                           
                       )
                     ),
              ),
              column(width = 6,
                     fluidRow(
                       box(title=strong("Top 10 Products Items"),width = 12, height=650,solidHeader = TRUE, status = "primary",
                           plotlyOutput("topProducts",width = "100%", height=550)
                           
                       ),
                       
                     ),
                     # fluidRow(
                     #   box(title=strong("Customer Order Distribution"),width = 12, height=315,solidHeader = TRUE, status = "primary",
                     #       #plotlyOutput("customerOrdersDist",width = "100%", height=250)
                     #       
                     #   )
                     # )
              )
            )
    ),
    
    tabItem(tabName = "shipping",
            fluidRow(## Boxes of Shipping Mode tabs
              column(width = 6,
                     
                     fluidRow(
                       box(title=strong("Revenue Generation Distribution by Shipping Mode"),width = 12, height=650,solidHeader = TRUE, status = "primary",
                           plotlyOutput("byShippingMode",width = "100%", height=550)
                           
                       )
                     ),
              ),
              column(width = 6,
                     fluidRow(
                       box(title=strong("Time taken for Shipment by Shipping Mode"),width = 12, height=265,solidHeader = TRUE, status = "primary",
                           plotlyOutput("time2Shipment",width = "100%", height=200)
                           
                       ),
                       
                     ),
                     fluidRow(
                       box(title=strong("Orders for Product Items by Shipping Mode"),width = 12, height=365,solidHeader = TRUE, status = "primary",
                           plotlyOutput("time2shipByItems",width = "100%", height=300)

                       )
                     )
              )
            )
    )
    
    )
    )
)


# Define server logic required to Dashboard
server <- function(input, output, session) {
  
  ##Read Data for Dashboard from data folder
  salesDataOrig=read.csv("data/sales_data_cleaned.csv")
  ##Convert Order.Date to date format###
  salesDataOrig$Order.Date = format(as.POSIXct(strptime(salesDataOrig$Order.Date,"%Y-%m-%d",tz="")) ,format = "%Y-%m-%d")
  salesDataOrig$Order.Date <- as.Date(salesDataOrig$Order.Date, format="%Y-%m-%d")
  ###Convert Ship.Date to date format###
  salesDataOrig$Ship.Date = format(as.POSIXct(strptime(salesDataOrig$Ship.Date,"%Y-%m-%d")) ,format = "%Y-%m-%d")
  salesDataOrig$Ship.Date <- as.Date(salesDataOrig$Ship.Date, format="%Y-%m-%d")
  
  ## Reactive function for filter out the dates according to selected Report date
  salesData<- reactive({
    req(input$reportDate)
    salesDataOrig %>% filter(format(Order.Date,"%Y-%m-%d")<=format(input$reportDate,"%Y-%m-%d"))
    
  }) 
  
  ############  Render Functions for output variables ############
  
  ############ Summary Tab ##################
  ##Revenue YTD Box
  output$revenueYTD <- renderText({
    paste("$",format(round(salesData() %>% filter(format(Order.Date,"%Y")==format(input$reportDate,"%Y"))%>% summarise(RevenueYTD=sum(Sales,na.rm = TRUE)), 0),big.mark=","))
  })
  
  output$revGrowthYTD <- renderText({
    CY=salesData() %>% filter(format(Order.Date,"%Y")==format(input$reportDate,"%Y"))%>% summarise(RevenueYTD=sum(Sales,na.rm = TRUE))
    PY=salesData() %>% filter(format(Order.Date,"%Y")==as.character(as.numeric(format(input$reportDate,"%Y"))-1))%>% summarise(RevenueYTD=sum(Sales,na.rm = TRUE))
    
    G=sprintf("%0.1f%%",(CY-PY)/PY*100)
    ifelse(G < 0,paste("<span style=\"color:red\">",G,"</span>"),paste("<span style=\"color:green\">",G,"</span>"))
    
  })
  
  ## Revenue Month Box
  output$revenueMonthly <- renderText({
    paste("$",format(round(salesData() %>% filter(format(Order.Date,"%Y-%m")==format(input$reportDate,"%Y-%m"))%>%summarise(RevenueMonthly=sum(Sales,na.rm = TRUE)), 0),big.mark=","))
  })
  
  output$revGrowthM <- renderText({
    CY=salesData() %>% filter(format(Order.Date,"%Y-%m")==format(input$reportDate,"%Y-%m"))%>%summarise(RevenueMonthly=sum(Sales,na.rm = TRUE))
    PY=salesData() %>% filter(format(Order.Date,"%Y-%m")==format(input$reportDate %m-% months(1),"%Y-%m"))%>% summarise(RevenueMonthly=sum(Sales,na.rm = TRUE))
    
    G=sprintf("%0.1f%%",(CY-PY)/PY*100)
    ifelse(G < 0,paste("<span style=\"color:red\">",G,"</span>"),paste("<span style=\"color:green\">",G,"</span>"))
    
  })
  
  ## Orders YTD Box
  output$OrdersYTD <- renderText({
    format(as.numeric(salesData() %>% filter(format(Order.Date,"%Y")==format(input$reportDate,"%Y"))%>% summarise(OrdersYTD=n_distinct(Order.ID,na.rm = TRUE))),big.mark=",")
  })
  
  output$ordGrowthYTD <- renderText({
    CY=salesData() %>% filter(format(Order.Date,"%Y")==format(input$reportDate,"%Y"))%>% summarise(OrdersYTD=n_distinct(Order.ID,na.rm = TRUE))
    PY=salesData() %>% filter(format(Order.Date,"%Y")==as.character(as.numeric(format(input$reportDate,"%Y"))-1))%>% summarise(OrdersYTD=n_distinct(Order.ID,na.rm = TRUE))
    
    G=sprintf("%0.1f%%",(CY-PY)/PY*100)
    ifelse(G < 0,paste("<span style=\"color:red\">",G,"</span>"),paste("<span style=\"color:green\">",G,"</span>"))
    
  })
  
  ## Orders Month Box
  output$OrdersMonthly <- renderText({
    format(as.numeric(salesData() %>% filter(format(Order.Date,"%Y-%m")==format(input$reportDate,"%Y-%m"))%>% summarise(OrdersMonthly=n_distinct(Order.ID,na.rm = TRUE))),big.mark=",")
  })
  
  output$ordGrowthM <- renderText({
    CY=salesData() %>% filter(format(Order.Date,"%Y-%m")==format(input$reportDate,"%Y-%m"))%>% summarise(OrdersMonthly=n_distinct(Order.ID,na.rm = TRUE))
    PY=salesData() %>% filter(format(Order.Date,"%Y-%m")==format(input$reportDate %m-% months(1),"%Y-%m"))%>% summarise(OrdersMonthly=n_distinct(Order.ID,na.rm = TRUE))
    
    G=sprintf("%0.1f%%",(CY-PY)/PY*100)
    ifelse(G < 0,paste("<span style=\"color:red\">",G,"</span>"),paste("<span style=\"color:green\">",G,"</span>"))
    
  })
  
  ## Revenue by customer Segment Chart
  output$bySegment <- renderPlotly({
    
    # Create Data Set
    Rev.bySegment <- salesData() %>% filter(format(Order.Date,"%Y")==format(input$reportDate,"%Y")) %>% group_by(Segment) %>% summarise(Revenue = round(sum(Sales,na.rm = TRUE)), 0) 
    
    cols <- RColorBrewer::brewer.pal(3, "Blues")
    # Make the plot
    pbySeg=plot_ly(data=Rev.bySegment, labels = ~Segment, values = ~Revenue, textinfo = 'label+percent', textposition = 'auto',
                   hoverinfo='text',text=~paste('$', format(Revenue,big.mark=",")) ,marker = list(colors = cols,line = list(color = '#FFFFFF', width = 2))) #
    pbySeg <- pbySeg %>% add_pie(hole = 0.6)
    pbySeg <- pbySeg %>% layout(showlegend = F, margin=list(l=0,r = 0),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) 
    
    pbySeg
    
  })
  
  ## Revenue by Item Category Chart
  output$byCategory <- renderPlotly({
    
    # Create Data Set
    Rev.byCategory <- salesData() %>% filter(format(Order.Date,"%Y")==format(input$reportDate,"%Y")) %>% group_by(Category) %>% summarise(Revenue = round(sum(Sales,na.rm = TRUE)), 0) #%>% arrange(desc(Revenue))
    
    cols <- RColorBrewer::brewer.pal(3, "Blues")
    # Make the plot
    pbyCat=plot_ly(data=Rev.byCategory, labels = ~Category, values = ~Revenue,colors = "Blues",textinfo = 'label+percent', textposition = 'auto',
                   hoverinfo='text',text=~paste('$', format(Revenue,big.mark=",")) ,rotation=90,marker = list(colors = cols,line = list(color = '#FFFFFF', width = 2))) 
    pbyCat <- pbyCat %>% add_pie(hole = 0.6)
    pbyCat <- pbyCat %>% layout(showlegend = F, margin=list(l=0,r = 0),
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    pbyCat
    
  })
  
  ## Revenue by Sub.Category Chart
  output$bySubCategory <- renderPlotly({
    
    # Create Data Set
    Rev.bySubCategory <- salesData() %>% filter(format(Order.Date,"%Y")==format(input$reportDate,"%Y")) %>% group_by(Sub.Category) %>% summarise(Revenue = round(sum(Sales,na.rm = TRUE)), 0) #%>% arrange(desc(Revenue))
    
    # Make the plot
    pbySubCat=plot_ly(data=Rev.bySubCategory, x = ~Sub.Category, y = ~Revenue, type = 'bar',
                      text = ~paste0("$",round(Revenue/ 1e3,0),"K"),
                      textposition = "outside") 
    
    pbySubCat
    
  })
  
  
  ## Revenue by Monthly Distribution Scatter Plot
  output$byMonthly <- renderPlotly({
    
    # Create Data Set
    Rev.byMonthly <- salesData() %>% filter(format(Order.Date,"%Y")==format(input$reportDate,"%Y"))%>% group_by(Month=format(Order.Date,"%m")) %>% summarise(Revenue_Current = round(sum(Sales,na.rm = TRUE))) 
    Rev.byMonthly_PreYear <- salesData() %>% filter(format(Order.Date,"%Y")==as.character(as.numeric(format(input$reportDate,"%Y"))-1))%>% group_by(Month=format(Order.Date,"%m")) %>% summarise(Revenue_Pre = round(sum(Sales,na.rm = TRUE))) 
    Rev.byMonthly=merge(Rev.byMonthly,Rev.byMonthly_PreYear, by="Month",all=TRUE)
    Rev.byMonthly$Month=(month.abb[as.numeric(Rev.byMonthly$Month)])
    
    # Make the plot
    pbyMonth=plot_ly(data=Rev.byMonthly, x = ~factor(Month, levels = month.abb), y = ~Revenue_Current, name = paste("Year ",format(input$reportDate,"%Y")),type = 'scatter', mode = 'lines') 
    pbyMonth <- pbyMonth %>% add_trace(y = ~Revenue_Pre, name = paste("Year ",as.numeric(format(input$reportDate,"%Y"))-1), line = list(color = 'rgb(22, 96, 167)', dash = 'dot'))
    pbyMonth <- pbyMonth %>% layout(yaxis = list (title = "Revenue"),xaxis = list(title = "Months"),legend = list(orientation = 'h',x = 0.1, y = 0.9))#
                          
    
    pbyMonth
    
  })
  
  
  ## Revenue Distribution by States
  output$byState <- renderPlotly({
    
    # Create Data Set
    Rev.byState <- salesData() %>% filter(format(Order.Date,"%Y")==format(input$reportDate,"%Y"))%>% group_by(Code) %>% summarise(Revenue = round(sum(Sales,na.rm = TRUE))) 
    
    # Make the plot
    pbyState=plot_geo(data=Rev.byState, locationmode="USA-states") %>% 
      add_trace(z = ~Revenue, zmin = 0, zmax = max(Rev.byState$Revenue),locations = ~Code, color = ~Revenue, colors = 'Blues',hoverinfo='text',text=~paste(Code,'<br>','$', format(Revenue,big.mark=","))) %>%
      layout(geo = list(scope = 'usa',projection = list(type = 'albers usa')))%>%
      colorbar(tickprefix = '$')
    
    pbyState
    
  })
  
  
  #################  Customer Tab ###########################
  ## Top 10 customers Chart
  output$topCustomers <- renderPlotly({
    
    # Create Data Set
    Rev.bytopCustomers <- salesData() %>% filter(format(Order.Date,"%Y")==format(input$reportDate,"%Y"))%>% group_by(Customer.Name) %>% summarise(Revenue = round(sum(Sales,na.rm = TRUE))) %>% arrange(desc(Revenue)) %>% slice(1:10)
    
    # Make the plot
    pbyTopCustomers=plot_ly(data=Rev.bytopCustomers, y = ~reorder(Customer.Name,Revenue), x = ~Revenue, type = 'bar', orientation = 'h',
                            hoverinfo='text',text=~paste('$', format(Revenue,big.mark=",")) ,marker = list(colors = '#3182BD'))
    pbyTopCustomers <- pbyTopCustomers %>% layout(yaxis = list(title = "",showgrid = FALSE, showline = FALSE, showticklabels = TRUE),
                            xaxis = list(title = "",zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE), bargap = 0.5)
    pbyTopCustomers
    
  })
  
  
  
  
  ## Customers Order Distribution
  output$customerOrdersDist <- renderPlotly({
    
    # Create Data Set
    Rev.bycustomerOrdersDist <- salesData() %>% filter(format(Order.Date,"%Y")==format(input$reportDate,"%Y"))%>% group_by(Order.ID) %>% summarise(Ordercount = n())
    
    # Make the plot
    pbycustomerOrdersDist=plot_ly(Rev.bycustomerOrdersDist,x = ~Ordercount,type = "histogram",alpha = 0.6)
    pbycustomerOrdersDist <- pbycustomerOrdersDist %>% layout(xaxis = list(title = "Orders per Visit"))
    pbycustomerOrdersDist
    
  })
  
  
  
  ## Customer Activity by Revenue
  output$customerActivity.Rev <- renderPlotly({
    
    # Create Data Set
    Rev.bycustomerActivity <- salesData() %>% filter(format(Order.Date,"%Y")==format(input$reportDate,"%Y"))%>% group_by(Month=format(Order.Date,"%m"),Segment) %>% summarise(Revenue = round(sum(Sales,na.rm = TRUE))) %>% as.data.frame()
    Rev.bycustomerActivity$Month=(month.abb[as.numeric(Rev.bycustomerActivity$Month)])
    
    # Make the plot
    pbyRevAct=plot_ly(data=Rev.bycustomerActivity, x = ~factor(Month, levels = month.abb), y = ~Revenue, color = ~Segment,type = 'scatter', mode = 'lines') 
    pbyRevAct <- pbyRevAct %>% layout(yaxis = list (title = "Revenue"),xaxis = list(title = "Month"),legend = list(orientation = 'h',x = 0.1, y = 1.2))#
    
    
    pbyRevAct
    
  })
  
  
  
  ## Customer Activity by Count
  output$customerActivity.count <- renderPlotly({
    
    # Create Data Set
    count.bycustomerActivity <- salesData() %>% filter(format(Order.Date,"%Y")==format(input$reportDate,"%Y"))%>% group_by(Month=format(Order.Date,"%m"),Segment) %>% summarise(OrderCount=n_distinct(Order.ID,na.rm = TRUE)) %>% as.data.frame()
    count.bycustomerActivity$Month=(month.abb[as.numeric(count.bycustomerActivity$Month)])
    
    # Make the plot
    pbyCountAct=plot_ly(data=count.bycustomerActivity, x = ~factor(Month, levels = month.abb), y = ~OrderCount, color = ~Segment,type = 'scatter', mode = 'lines') 
    pbyCountAct <- pbyCountAct %>% layout(yaxis = list (title = "Order Count"),xaxis = list(title = "Month"),legend = list(orientation = 'h',x = 0.1, y = 1.2))#
    
    
    pbyCountAct
    
  })
  
  ######################## Product Item Tab ############################
  
  ## Revenue Generation Distribution by Product Items
  output$itemCat_SubCat <- renderPlotly({
    
    # Create Data Set
    Rev.itemCat_SubCat <- salesData() %>% filter(format(Order.Date,"%Y")==format(input$reportDate,"%Y"))%>% group_by(Category,Sub.Category) %>% summarise(Revenue = (sum(Sales,na.rm = TRUE)))%>% as.data.frame()
    Rev.itemCat <- salesData() %>% filter(format(Order.Date,"%Y")==format(input$reportDate,"%Y"))%>% group_by(Category) %>% summarise(Revenue = (sum(Sales,na.rm = TRUE)))%>% mutate(Sub.Category=Category,Category="Item Category") %>% as.data.frame()
    Rev.total <- salesData() %>% filter(format(Order.Date,"%Y")==format(input$reportDate,"%Y"))%>%  summarise(Revenue = (sum(Sales,na.rm = TRUE)))%>% mutate(Sub.Category="Item Category",Category="") %>% as.data.frame()
    Rev.itemCat_SubCat=rbind(Rev.itemCat,Rev.itemCat_SubCat)
    Rev.itemCat_SubCat=rbind(Rev.total,Rev.itemCat_SubCat)
    
    # Make the plot
    PBYitemCat_SubCat <- plot_ly(
      labels = Rev.itemCat_SubCat$Sub.Category,
      parents = Rev.itemCat_SubCat$Category,
      values = Rev.itemCat_SubCat$Revenue,
      type = 'sunburst',
      branchvalues = 'total',textinfo = 'label+percent parent',
      hoverinfo='percent parent+text',text=~paste('$', format(round(Rev.itemCat_SubCat$Revenue),big.mark=","))#,marker = list(colorscale = "Spectral")
    )
    PBYitemCat_SubCat <- PBYitemCat_SubCat %>% layout(margin=list(l=0,r = 0), colorway = c("#C6DBEF", "#4292C6", "#08306B"))

    PBYitemCat_SubCat
    
  })
  
  
  
  
  ## Top 10 Product Items Chart
  output$topProducts <- renderPlotly({
    
    # Create Data Set
    Rev.bytopProducts <- salesData() %>% filter(format(Order.Date,"%Y")==format(input$reportDate,"%Y"))%>% group_by(Product.Name) %>% summarise(Revenue = round(sum(Sales,na.rm = TRUE))) %>% arrange(desc(Revenue)) %>% slice(1:10)%>% mutate(Product.Name = str_wrap(Product.Name, width = 25))
    
    # Make the plot
    pbytopProducts=plot_ly(data=Rev.bytopProducts, y = ~reorder(Product.Name,Revenue), x = ~Revenue, type = 'bar', orientation = 'h',
                            hoverinfo='text',text=~paste('$', format(Revenue,big.mark=",")) ,marker = list(colors = '#3182BD'))
    pbytopProducts <- pbytopProducts %>% layout(yaxis = list(title = "",showgrid = FALSE, showline = FALSE, showticklabels = TRUE),
                                                  xaxis = list(title = "",zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE), bargap = 0.5)
    pbytopProducts
    
  })
  
  ##################### Shipment Mode Tab ############################
  
  ## Revenue by Shipment Mode
  output$byShippingMode <- renderPlotly({
    
    # Create Data Set
    Rev.byShippingMode <- salesData() %>% filter(format(Order.Date,"%Y")==format(input$reportDate,"%Y")) %>% group_by(Ship.Mode) %>% summarise(Revenue = round(sum(Sales,na.rm = TRUE)), 0) #%>% arrange(desc(Revenue))
    
    cols <- RColorBrewer::brewer.pal(3, "Blues")
    # Make the plot
    pbyShippingMode=plot_ly(data=Rev.byShippingMode, labels = ~Ship.Mode, values = ~Revenue, textinfo = 'label+percent+value', textposition = 'auto',
                   hoverinfo='text',text=~paste('$', format(Revenue,big.mark=",")) ,marker = list(colors = cols,line = list(color = '#FFFFFF', width = 2))) #
    pbyShippingMode <- pbyShippingMode %>% add_pie(hole = 0.6)
    pbyShippingMode <- pbyShippingMode %>% layout(showlegend = F, margin=list(l=0,r = 0),
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) 
    
    pbyShippingMode
    
  })
  
  
  ## Time Taken for Shipment by Shipping Mode
  output$time2Shipment <- renderPlotly({
    
    # Create Data Set
    time2Ship <- salesData() %>% mutate(days2ready=round(as.numeric(difftime(Ship.Date,Order.Date, units = "days")))) %>% filter(format(Order.Date,"%Y")==format(input$reportDate,"%Y"))%>% group_by(Ship.Mode)  %>% summarise(meanDays = round(mean(days2ready)),minDays=min(days2ready),maxDays=max(days2ready))
    
    # Make the plot
    pbytime2Ship=plot_ly(data=time2Ship, y = ~meanDays, x = ~Ship.Mode, type = 'scatter', #orientation = 'h',
                         hoverinfo='text',text=~paste('Mean- ',meanDays,'<br>','Min- ',minDays,'<br>','Max- ',maxDays,'<br>'),marker = list(colors = '#3182BD'),
                         error_y = list(
                           type = "data", 
                           symmetric = FALSE, 
                           arrayminus = time2Ship$minDays,
                           array = time2Ship$maxDays))
    pbytime2Ship <- pbytime2Ship %>% layout(yaxis = list(title = "Days",showgrid = FALSE, showline = FALSE, showticklabels = TRUE),
                                                  xaxis = list(title = "Shipping Mode",zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE), bargap = 0.5)
    pbytime2Ship
    
  })
  
  
  ## Time Taken to Shipment by Product Items
  output$time2shipByItems <- renderPlotly({
    
    # Create Data Set
    time2ShipItems <- salesData()  %>% filter(format(Order.Date,"%Y")==format(input$reportDate,"%Y"))%>% count(Sub.Category,Ship.Mode)
    
    # Make the plot
    pbytime2ShipItems=plot_ly(data=time2ShipItems, y = ~n, x = ~Ship.Mode, type = 'bar',color=~Sub.Category, 
                         hoverinfo='text',text=~paste('Item- ',Sub.Category,'<br>','Orders- ',n)
                         )
    pbytime2ShipItems <- pbytime2ShipItems %>% layout(yaxis = list(title = "Orders",showgrid = FALSE, showline = FALSE, showticklabels = TRUE),
                                            xaxis = list(title = "Shipping Mode",zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE), legend = list(orientation = 'h',x = 0.1, y = 1.2))
    pbytime2ShipItems
    
  })
  
  

    
}

# Run the application 
shinyApp(ui = ui, server = server)
