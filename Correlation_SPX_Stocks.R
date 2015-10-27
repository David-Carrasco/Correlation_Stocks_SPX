library(quantmod)
library(scales)
library(dygraphs)
library(RDRToolbox)
library(rgl)
library(metricsgraphics)
library(corrplot)
library(caret)
library(rvest)

#########################
###### CONSTANTS ########
######################### 

#start.date and end.date in order to filter the dataframe according to the user's preferences
start.date <- as.Date('20-01-01')
end.date <- as.Date('2015-10-01')

#########################
###### FUNCTIONS ########
######################### 

adapt.indicator.daily <- function(ticker, name.indicator, source.macro.indicators, 
                                  min.scale, max.scale){
  
  #Get the indicator information via source.macro.indicators
  tmp <- getSymbols(ticker, src = source.macro.indicators, 
                    auto.assign=FALSE, from = '1950-01-01')
  tmp <- tmp[,ncol(tmp)]
  colnames(tmp) <- name.indicator
  
  #Rescale data with min and max of SPX
  tmp <- scale(tmp, center = T, scale = T)
  
  return(tmp)
  
}

######################

set.seed(123)

#Get historical close price SPX
getSymbols('^GSPC', src='yahoo', from = '1950-01-01')
SPX <- scale(GSPC[,ncol(GSPC)], center = T, scale = T)
colnames(SPX) <- c('SP500')

#Scrapping List with the SP500 companies from wikipedia 
url_list_companies_SPX <- 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'

list_companies_SPX <- read_html(url_list_companies_SPX) %>%
  html_nodes('table') %>%
  .[[1]] %>%
  html_table()

#Getting information from Yahoo for every ticker and SPX, altogether.
#DISCLAIMER: This pint will take some time.
spx_set <- cbind(SPX, do.call(cbind, lapply(list_companies_SPX$`Ticker symbol`, function(symbol){
  adapt.indicator.daily(symbol, symbol, 'yahoo', min.scale, max.scale)
})))

#Filter the dataframe by the start and end dates and removing the NAs
SPX_correlations <- as.data.frame(spx_set[paste0(start.date, '::', end.date)])

#Removing the columns with NAs after filtering the dataframe by the two dates
SPX_correlations <- SPX_correlations[, colSums(is.na(SPX_correlations)) == 0]

#Calculating the correlation each other symbols
#Keep in mind that some symbols were not included in the past in the index but they will appear in these correlations
SPX_correlations <- cor(SPX_correlations)
#corrplot(SPX_correlations, method = 'square')






