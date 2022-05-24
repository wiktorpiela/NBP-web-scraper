library("httr")
library("jsonlite")
library("tidyverse")
library("lubridate")
library("shiny")
library("shinydashboard")
library("rvest")
library("kableExtra")
source("func.R", encoding = "UTF-8")

ui <- dashboardPage(
  
  dashboardHeader(title = "NBP web scraper"),
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Currency code dictionary", tabName = "curr_code_dict", icon = icon("book")),
      menuItem("Gold price", tabName = "gold_price", icon = icon("signal")),
      menuItem("Gold price converter", tabName = "gold_price_converter", icon = icon("calculator")),
      menuItem("Currency rate", icon = icon("euro"), tabName = "currency_rate",
               badgeLabel = "new", badgeColor = "green"),
      menuItem("Currency converter", icon = icon("retweet"), tabName = "curr_conv")
      
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "gold_price",
              
              sidebarLayout(
                
                sidebarPanel(
                  
                  sliderInput("date_range", label="Select date range",
                              min = min(as.Date(get_gold_price_date_range()$data)),
                              max = max(as.Date(get_gold_price_date_range()$data)),
                              value = c(median(as.Date(get_gold_price_date_range()$data)),median(as.Date(get_gold_price_date_range()$data))+100)),
                  br(),
                  br(),
                  
                  fluidRow(
                    column(3, downloadButton("save_plot", "Save plot",
                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                    column(3, downloadButton("save_gold_table", "Save data",
                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                  )
                  
                ),
                
                mainPanel(
                  
                  plotOutput("plot"),
                  
                  br(),
                  
                  fluidRow(
                    column(6,tableOutput("tab")),
                    column(6, tableOutput("full_tab"))
                  )
                  
                )
                
              )
              
      ),
      
      tabItem(tabName = "currency_rate",
              
              sidebarLayout(
                
                sidebarPanel(
                  
                  dateRangeInput("currency_dates", label = "Select date range",
                                 min=min(get_currency_data()$date),
                                 max = max(get_currency_data()$date),
                                 start = get_correct_default_date2()-12, 
                                 end = max(get_currency_data()$date),
                                 weekstart = 1),
                  selectInput("curr_list", "Select currency",
                              choices = unique(get_currency_data()$code)),
                  downloadButton("save_curr_plot", "Save plot",
                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  downloadButton("save_curr_data", "Save data",
                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                  
                ),
                
                mainPanel(
                  
                  plotOutput("curr_plot"),
                  br(),
                  tableOutput("curr_table")
                  
                )
              ),
      ),
      
      tabItem(tabName = "curr_conv",
              
              sidebarLayout(
                
                sidebarPanel(
                  
                  dateInput("currency_date", "Select currency date",
                            min = min(get_currency_data()$date),
                            max = max(get_currency_data()$date),
                            value = max(get_currency_data()$date),
                            weekstart = 1,
                            daysofweekdisabled = c(0,6)),
                  selectInput("curr1","Select start currency",
                              choices = append(unique(get_currency_data()$code),"PLN"),
                              selected = "USD"),
                  selectInput("curr2", "Select destination currency",
                              choices = append(unique(get_currency_data()$code),"PLN"),
                              selected = "EUR"),
                  numericInput("amount_curr1", "Type start currency amount",
                               min=0,
                               value=1),
                  actionButton("convert_currency", "Convert",
                               icon = icon("retweet"),
                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                  
                ),
                
                mainPanel(
                  
                  textOutput("exchange")
                  
                )
              ),
              
              downloadButton("save_curr_matrix", "Download up to date currency matrix",
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              br(),
              br()
              
      ),
      
      tabItem(tabName = "curr_code_dict",
              
              sidebarLayout(
                
                sidebarPanel(
                  
                  tableOutput("dictionary_table"),
                  actionButton("click_nbp", "Visit offical NBP website",
                               icon = icon("globe"),
                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                ),
                
                mainPanel()
              )),
      
      tabItem(tabName = "gold_price_converter",
              
              sidebarPanel(
                
                dateInput("one_date_converter",label="Select date",
                          value = max(as.Date(get_gold_price_date_range()$data)), 
                          weekstart = 1, 
                          daysofweekdisabled = c(0,6),
                          min=min(as.Date(get_gold_price_date_range()$data)),
                          max=max(as.Date(get_gold_price_date_range()$data))),
                
                radioButtons("select_conv_type", "Type conversion direction",
                             choices = c("mass to amount" = "mass",
                                         "amount to mass" = "amount")),
                
                numericInput("type_quant", label = "Type quantity",
                             value = 1,
                             min=0),
                
                actionButton("just_convert", 'Click to convert value',
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                             icon = icon("calculator"))
                
              ),
              
              mainPanel(
                
                textOutput("conv_result"),
                
                br(),
                br(),
                
                fluidRow(
                  
                  column(6, tableOutput("current_gold_price"))
                  
                  
                )
                
              )
              
      )
      
    )
    
  )
  
)

server <- function(input, output,session) {
  
  full_table <- reactive({
    
    req(min(input$date_range))
    req(max(input$date_range))
    
    get_gold_price_date_range() %>% 
      filter(data >= min(input$date_range),
             data<= max(input$date_range)) %>% 
      rename(date = "data",
             `price per 1g` = "cena") %>% 
      arrange(desc(as.Date(date)))
    
  })
  
  table <- reactive({
    
    get_gold_price_date_range() %>% 
      filter(data<=max(input$date_range),
             data>=min(input$date_range)) %>%
      create_react_table() %>% 
      mutate(cena = format(cena, digits=3)) %>% 
      rename(date="data",
             `price per 1g` = "cena")
    
  })
  
  curr_table <- reactive({
    
    req(input$currency_dates)
    req(input$curr_list)
    
    get_currency_data() %>% 
      left_join(get_curr_code_dictionary(), by=("code")) %>%
      select(date,code,Curr_name,bid,ask,mid) %>%
      filter(date>=min(input$currency_dates),
             date<= max(input$currency_dates),
             code==input$curr_list) %>% 
      arrange(desc(date)) %>% 
      mutate(date = as.character(date),
             bid = format(bid, digits = 3),
             ask = format(ask, digit = 3),
             mid = format(mid, digit = 3)) %>% 
      select(-c(bid,ask)) %>% 
      rename(`medium rate` = "mid",
             `currency name` = "Curr_name")
    
  })
  
  
  plot <- reactive({
    
    get_gold_price_date_range() %>% filter(data<=max(input$date_range),
                                           data>=min(input$date_range)) %>% 
      ggplot(aes(as.Date(data), cena, group=1)) +
      geom_line(color = "red")+
      scale_x_date(date_breaks="1 week",
                   expand = c(0,0))+
      labs(x="Date", y="Price",
           title = paste("Gold price PLN per 1g -",max(input$date_range)-min(input$date_range),"days"))+
      ggthemes::theme_economist()+
      theme(axis.text.x = element_text(angle=90),
            plot.title = element_text(hjust=0.5))
  })
  
  currency_plot <- reactive({
    
    req(min(input$currency_dates))
    req(max(input$currency_dates))
    req(input$curr_list)
    
    get_currency_data() %>% 
      filter(date>=min(input$currency_dates),
             date<= max(input$currency_dates),
             code== input$curr_list) %>% 
      ggplot(aes(as.Date(date),mid,group=1))+
      geom_line(color="red")+
      scale_x_date(date_breaks = "1 day",
                   expand = c(0,0))+
      labs(x="Date", y="Price",
           title = paste("Price in PLN for 1",input$curr_list,"-",max(input$currency_dates)-min(input$currency_dates),"days"))+
      ggthemes::theme_economist()+
      theme(axis.text.x = element_text(angle=90),
            plot.title = element_text(hjust=0.5))
    
  })
  
  current_value <- reactive({
    
    req(input$one_date_converter)
    
    get_gold_price_date_range() %>%
      filter(data == input$one_date_converter) %>% 
      rename(date ="data",
             price="cena")
    
  })
  
  
  conv_gold <- reactive({
    
    req(input$one_date_converter)
    req(input$type_quant)
    
    get_gold_price_date_range() %>% 
      filter(data == input$one_date_converter) %>% 
      pull(cena)
    
  }) 
  
  exch_rate_react <- reactive({
    
    req(input$currency_date)
    req(input$curr1)
    req(input$amount_curr1)
    
    get_exchange_rate(input$currency_date,
                      input$curr1,
                      input$curr2)*input$amount_curr1
    
  })
  
  currency_matrix <- reactive({
    
    get_currency_matrix() %>% 
      arrange(desc(date),code) %>% 
      select(1,3,2,4:17)
    
  })
  
  output$full_tab <- function(){
    
    full_table() %>% 
      kable() %>% 
      row_spec(0, background = "#77bdd8") %>% 
      add_header_above(header=c(paste("Rows: ",nrow(full_table()))," "),
                       background = "#77bdd8") %>% 
      kable_styling(c("striped","bordered"))
  }
  
  
  output$plot <- renderPlot(plot())
  
  # output$tab <- renderTable(table())
  
  output$tab <- function(){
    
    table() %>% 
      kable() %>% 
      row_spec(0, background = "#77bdd8") %>% 
      add_header_above(header=c(paste("Period summarise:",min(input$date_range),"-",max(input$date_range))," "," "),
                       background = "#77bdd8") %>% 
      kable_styling(c("striped","bordered"))
    
  }
  
  # output$current_gold_price <- renderTable(current_value())
  
  output$current_gold_price <- function() {
    
    current_value() %>% 
      kable() %>%
      row_spec(0, background = "#77bdd8") %>% 
      add_header_above(header=c("Price as of day:"," "),
                       background = "#77bdd8") %>% 
      kable_styling(c("striped","bordered"))
    
  }
  
  output$curr_plot <- renderPlot(currency_plot())
  
  # output$curr_table <- renderTable(curr_table())
  
  output$curr_table <- function(){
    
    curr_table() %>% 
      kable() %>%
      row_spec(0, background = "#77bdd8") %>% 
      kable_styling(c("striped","bordered")) 
  }
  
  output$exchange <- renderText({
    input$convert_currency
    paste(isolate(input$amount_curr1),isolate(input$curr1),"as of",isolate(input$currency_date),
          "is equal to", format(isolate(exch_rate_react()),digits=3),isolate(input$curr2))
  })
  
  output$dictionary_table <- renderTable(get_curr_code_dictionary() %>% 
                                           filter(code%in%unique(get_currency_data()$code)) %>% 
                                           add_row(code = "PLN", Curr_name = "Polish zloty") %>% 
                                           rename(`currency name` = 'Curr_name'))
  
  output$conv_result <- renderText({
    input$just_convert
    case_when(isolate(input$select_conv_type) == "mass" ~ paste("As of",isolate(input$one_date_converter),",",
                                                                isolate(input$type_quant),"g of gold you can sell for",
                                                                format(isolate(input$type_quant)*isolate(conv_gold()), big.mark=" "),"PLN"),
              
              TRUE ~ paste("As of",isolate(input$one_date_converter),", for",
                           isolate(input$type_quant),"PLN you can buy",
                           format(isolate(input$type_quant)/isolate(conv_gold()),digits=3, big.mark=" "),"g of gold")
    )
  })
  
  output$save_gold_table <- downloadHandler(
    
    filename = "gold_price_table.xlsx",
    
    content = function(file) writexl::write_xlsx(full_table(), file)
  )
  
  output$save_plot <- downloadHandler(
    
    filename = "gold_price_plot.png",
    
    content = function(file) {
      
      png(file, width = 1000, height = 600)
      print(plot())
      dev.off()
      
    }
    
  )
  
  output$save_curr_data <- downloadHandler(
    
    filename = "currency_rate_table.xlsx",
    
    content = function(file) writexl::write_xlsx(curr_table(), file)
  )
  
  output$save_curr_plot <- downloadHandler(
    
    filename = "currency_rate_plot.png",
    
    content = function(file) {
      
      png(file, width = 1000, height = 600)
      print(currency_plot())
      dev.off()
      
    }
    
  )
  
  output$save_curr_matrix <- downloadHandler(
    
    filename = "currency_rate_matrix.xlsx",
    
    content = function(file) {
      
      showModal(modalDialog("Data is preparing to download...", footer=NULL))
      on.exit(removeModal())
      writexl::write_xlsx(currency_matrix(), file)
      
    }
    
  )
  
  observeEvent(input$click_nbp,
               browseURL("https://www.nbp.pl/"))
  
}

shinyApp(ui, server)







