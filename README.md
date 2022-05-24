# NBP-web-scraper
The app gets data from NBP datasets using API on daily basis (bank holidays excluded) and presents the data on user demand. It has been stored om shiny apps server - you can run it clicking on this hfref - https://wpiela.shinyapps.io/NBP_web_scrapper/

In order to open this app on your desktop, run below code in RStudio:

if (!require("shiny")) install.packages("shiny")

list.of.packages <- c("ggplot2", "Rcpp")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

``
shiny::runGitHub("NBP-web-scraper","wiktorpiela")
``
or use this link because app is located on shiny apps server :



