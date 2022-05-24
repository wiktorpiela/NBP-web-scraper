library("httr")
library("jsonlite")
library("tidyverse")
library("shiny")
library("rvest")


get_gold_price_date_range <- function(){
  
  res <- GET(paste0("http://api.nbp.pl/api/cenyzlota/last/",255,"/?format=json"))
  
  df <- fromJSON(rawToChar(res$content)) %>% as_tibble()
  
  return(df)
}



get_currency_data <- function(){
  
  res <- GET(paste0("http://api.nbp.pl/api/exchangerates/tables/C/",Sys.Date()-93,"/",Sys.Date(),"/"))
  
  main_df <- fromJSON(rawToChar(res$content)) %>% 
    as_tibble() %>% 
    select(effectiveDate,rates) 
  
  df_curr <- unnest(main_df, cols=c(rates)) %>% 
    rename(date = "effectiveDate") %>% 
    rowwise() %>% 
    mutate(mid = mean(c(bid,ask))) %>% 
    ungroup()
  
  return(df_curr)
}


create_react_table <- function(x){
  
  x1 <- x %>% 
    filter(cena == min(cena) | 
             cena == max(cena)) %>% 
    mutate(measure = c("min","max"))
  
  x2 <- x %>% 
    summarise(avg = mean(cena),
              sd = sd(cena),
              median = median(cena)) %>% 
    pivot_longer(cols=1:3,
                 names_to = "measure",
                 values_to = "cena") %>% 
    mutate(data = "")
  
  return(bind_rows(x1,x2))
}

get_correct_default_date <- function(x){

    ifelse(weekdays(x)=="sobota",x-1,
          ifelse(weekdays(x)=="niedziela",x-2,x))%>% 
    as.Date(origin="1970-01-01")
}

get_correct_default_date2 <- function(){
  
  case_when(as.numeric(format(as.POSIXct(Sys.time()), format = "%H")) & !weekdays(Sys.Date())%in%c("sobota","niedziela") < 8 ~ as.Date(Sys.Date()-1),
            weekdays(Sys.Date()) == "sobota" ~ as.Date(Sys.Date()-1),
            weekdays(Sys.Date()) == "niedziela" ~ as.Date(Sys.Date()-2),
            TRUE~as.Date(Sys.Date(), origin = "1970-01-01"))
  
}

get_curr_code_dictionary <- function(){
  
  content <- read_html("https://en.wikipedia.org/wiki/ISO_4217") %>% 
    html_table(fill=TRUE)
  
  content <- content[[2]][,c(1,4)] %>% 
    rename(code = "Code",
      Curr_name = "Currency") 
  
  return(content)
}

get_proper_curr_rate <- function(code_id, day){
  
  x <- get_currency_data() %>% 
    filter(code == code_id &
      date==as.Date(day)) 
  
  x <- x[[ncol(x)]]
  
  return(x)
}

get_exchange_rate <- function(day,start_curr,dest_curr){
  
  x <- get_currency_data() %>% 
    filter(code == start_curr,
           date == as.Date(day))
  
  x <- pull(x[,ncol(x)])
  
  y <- get_currency_data() %>% 
    filter(code == dest_curr,
           date == as.Date(day))
  
  y <- pull(y[,ncol(y)])
  
  return(x/y)
    
}

get_exchange_rate <- function(day,start_curr,dest_curr){
  
  if(start_curr!="PLN" & dest_curr!="PLN"){
    
    x <- get_currency_data() %>% 
      filter(code == start_curr,
             date == as.Date(day))
    
    x <- pull(x[,ncol(x)])
    
    y <- get_currency_data() %>% 
      filter(code == dest_curr,
             date == as.Date(day))
    
    y <- pull(y[,ncol(y)])
    
    rate <- x/y
    
    }
    
    if(start_curr=="PLN" & dest_curr!="PLN"){
    
    x <- get_currency_data() %>% 
      filter(code == dest_curr,
             date == as.Date(day)) %>% 
      pull(mid)
    
    rate <- 1/x
    
    }
    
  if(start_curr!="PLN" & dest_curr=="PLN") {
    
    x <- get_currency_data() %>% 
      filter(code == start_curr,
             date == as.Date(day)) %>% 
      pull(mid)
    
    rate <- x
    
  } 
  
  if(start_curr=="PLN" & dest_curr=="PLN"){

    rate <- 1

    }

    return(rate)
}
 
get_gold_value <- function(day){
  
  x <- get_gold_price_date_range() %>% 
    filter(data==as.Date(day)) %>% 
    pull(cena)
  
  return(x)
}

get_currency_matrix <- function(){
  
  dates <- unique(get_currency_data()$date)
  onedays <- list()
  
  for(x in seq_along(dates)){
    
    onedays[[x]] <- get_currency_data() %>% 
      filter(date == dates[x]) %>% 
      select(code, mid) %>%
      add_row(code="PLN",
              mid=1)
    
    onedays[[x]] <- bind_cols(code = onedays[[x]]$code,
                              map2_dfc(
                                .x = onedays[[x]]$code,
                                .y = onedays[[x]]$mid,
                                .f = ~ transmute(onedays[[x]],!!.x := onedays[[x]]$mid / .y)
                              ))
    
    onedays[[x]] <- mutate(onedays[[x]], date = dates[x]) %>% 
      select(16,1:15)
    
    
    
  }
  
  onedays <- reduce(onedays, bind_rows) %>% 
    left_join(get_curr_code_dictionary(), by="code") %>% 
    select(1,2,17,3:16) %>% 
    rename(`currency name` = "Curr_name") %>% 
    arrange(desc(date))
  
  return(onedays)
  
}