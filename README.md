# NBP-web-scraper
The app gets data from NBP datasets using API on daily basis (bank holidays excluded) and presents the data on user demand. It has been stored om shiny apps server - you can run it clicking on this href - https://wpiela.shinyapps.io/NBP_web_scrapper/

In order to open this app on your desktop, run below code in RStudio:

``
req_packages <- c("httr","jsonlite","tidyverse","shiny","rvest")
new_packages <- req_packages[!(req_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

shiny::runGitHub("NBP-web-scraper","wiktorpiela")
``
or use this link because app is located on shiny apps server :



