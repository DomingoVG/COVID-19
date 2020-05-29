## ---------------------------
##
## Script name: getData.R
##
## Purpose of script: Scrape data from gitHub repository established to track nCov20
##
## Author: Domingo Velazquez
##
## Date Created: 2020-03-20
## Date Updated: 2020-04-11
##
## ---------------------------
##
## Notes:
##
## --------------------------
## load up the packages we will need:  
##
## ---------------------------
## load up functions

source("functions.R")

## ---------------------------

## Get data

tsConf    <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
tsConfUS  <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
tsDeath   <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
tsDeathUS <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
tsRec     <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
tsConfMX  <- "https://raw.githubusercontent.com/DomingoVG/COVID-19/master/Casos_Diarios_Confirmados_MX_final.csv"
tsDeathMX <- "https://raw.githubusercontent.com/DomingoVG/COVID-19/master/Casos_Diarios_Defunciones_MX_final.csv"  

timeSeriesInfections   <- loadData(tsConf)
timeSeriesInfectionsUS <- loadData(tsConfUS)
timeSeriesDeaths       <- loadData(tsDeath)
timeSeriesDeathsUS     <- loadData(tsDeathUS)
timeSeriesRecoveries   <- loadData(tsRec)
timeSeriesInfectionsMX <- read.csv(file = tsConfMX, stringsAsFactors = FALSE)
timeSeriesDeathMX      <- read.csv(file = tsDeathMX, stringsAsFactors = FALSE)


timeSeriesInfections   <- timeSeriesInfections[,-99]
timeSeriesInfectionsUS <- timeSeriesInfectionsUS[,-99]
timeSeriesDeaths       <- timeSeriesDeaths[,-99]
timeSeriesDeathsUS     <- timeSeriesDeathsUS[,-99]
timeSeriesRecoveries   <- timeSeriesRecoveries[,-99]

  
rm(tsConf, tsConfUS, tsDeath, tsDeathUS, tsRec, tsConfMX, tsDeathMX) # tidy up

# Filter original data of MX

timeSeriesDeaths <- timeSeriesDeaths[!(timeSeriesDeaths$Country.Region == "Mexico"),]
timeSeriesInfections <- timeSeriesInfections[!(timeSeriesInfections$Country.Region == "Mexico"),]

# Change MX data colnames to fit the standard 

colnames(timeSeriesDeathMX) <- colnames(timeSeriesDeaths)
colnames(timeSeriesInfectionsMX) <- colnames(timeSeriesInfections)

# Append State level data of MX  to Global

timeSeriesDeaths <- rbind(timeSeriesDeaths, timeSeriesDeathMX)
timeSeriesInfections <- rbind(timeSeriesInfections, timeSeriesInfectionsMX)

# Aggregate US data to Province.State

timeSeriesInfectionsUS <-regionAgg(timeSeriesInfectionsUS, regionCol = timeSeriesInfectionsUS$Province.State, regionName = "Province.State")
timeSeriesInfectionsUS$Country.Region <- rep("US", nrow(timeSeriesInfectionsUS))
timeSeriesInfectionsUS <- timeSeriesInfectionsUS[c(ncol(timeSeriesInfectionsUS), 1:(ncol(timeSeriesInfectionsUS)-1))] 
timeSeriesDeathsUS <-regionAgg(timeSeriesDeathsUS, regionCol = timeSeriesDeathsUS$Province.State, regionName = "Province.State")
timeSeriesDeathsUS$Country.Region <- rep("US", nrow(timeSeriesDeathsUS))
timeSeriesDeathsUS <- timeSeriesDeathsUS[c(ncol(timeSeriesDeathsUS), 1:(ncol(timeSeriesDeathsUS)-1))] 

# Merge US data with global dataframes
  
timeSeriesInfections <- rbind(subset(timeSeriesInfections, timeSeriesInfections$Country.Region!="US") , timeSeriesInfectionsUS)
timeSeriesDeaths <- rbind(subset(timeSeriesDeaths, timeSeriesDeaths$Country.Region!="US") , timeSeriesDeathsUS)

rm(timeSeriesDeathsUS, timeSeriesInfectionsUS) #tidy up

# take US, Mexico data, and generate recovery data assuming ttr

infSub   <- subset(timeSeriesInfections, timeSeriesInfections$Country.Region %in% c("Canada", "Mexico", "US"))
deathSub <- subset(timeSeriesDeaths,     timeSeriesDeaths$Country.Region     %in% c("Canada", "Mexico","US"))
recSub   <- recLag(infSub, deathSub, active = FALSE)

# Merge US, Mexico estimated recoveries on to known recoveries

timeSeriesRecoveries <- rbind(subset(timeSeriesRecoveries, !(timeSeriesRecoveries$Country.Region %in% c("Canada", "Mexico","US"))) , recSub)

rm(infSub, deathSub, recSub) # tidy up

## standardise

## get Date range

dCols <- dateCols(timeSeriesInfections)
dates <- as.Date(colnames(timeSeriesInfections)[dCols], format = "%m.%d.%y")

# Standardise dataframes and compute active cases

std <- activeCases(timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries)

# Create a list to hold all data

available_countries <- c("China", "Mexico", "US") # countries available for drill-down
dataList <- vector(mode = "list", length = length(available_countries)+1)
names(dataList) <- c("Global", available_countries)

###### GLOBAL ######

timeSeriesInfections <- regionAgg(std$tsI, regionCol = std$tsI$Country.Region, regionName = "Region") # aggregated to country
timeSeriesDeaths     <- regionAgg(std$tsD, regionCol = std$tsD$Country.Region, regionName = "Region") 
timeSeriesRecoveries <- regionAgg(std$tsR, regionCol = std$tsR$Country.Region, regionName = "Region")
timeSeriesActive     <- regionAgg(std$tsA, regionCol = std$tsA$Country.Region, regionName = "Region")

## Define menus
# Get region names with 20 or more cases as of yesterday

ddNames <- sort(timeSeriesActive$Region[timeSeriesActive[[ncol(timeSeriesActive)-1]]>19], decreasing = FALSE)
ddReg <- ddNames
names(ddReg) <- ddNames

## Write data caches out

save(ddReg, ddNames, dates, file = "dat/Global/menuData.RData")
save(timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries, timeSeriesActive, dates, file = "dat/Global/cacheData.RData")
  
## Run deconvolution to estimate undiagnosed cases from cached data

source("detection/estGlobalV2.R")
load("dat/Global/estDeconv.RData")

dataList$Global <- list(timeSeriesInfections = timeSeriesInfections,
                        timeSeriesDeaths = timeSeriesDeaths,
                        timeSeriesRecoveries = timeSeriesRecoveries,
                        timeSeriesActive = timeSeriesActive,
                        dates = dates,
                        ddReg = ddReg,
                        ddNames = ddNames,
                        cumulative.infections = cumulative.infections,
                        active.cases = active.cases)

###### LOCAL ######

for(focusCountry in available_countries) {

    print(focusCountry)
    # set dataframes back to standards
    tsI <- std$tsI
    tsD <- std$tsD
    tsR <- std$tsR
    tsA <- std$tsA
    
    # subset to focusCountry
    tsI <- subset(tsI, tsI$Country.Region == focusCountry)
    tsD <- subset(tsD, tsD$Country.Region == focusCountry)
    tsR <- subset(tsR, tsR$Country.Region == focusCountry)
    tsA <- subset(tsA, tsA$Country.Region == focusCountry)
    
    # aggregate to region
    tsI <- regionAgg(tsI, regionCol = tsI$Province.State)
    tsD <- regionAgg(tsD, regionCol = tsD$Province.State)
    tsR <- regionAgg(tsR, regionCol = tsR$Province.State)
    tsA <- regionAgg(tsA, regionCol = tsA$Province.State)
    
    timeSeriesInfections <- natAgg(tsI)
    timeSeriesDeaths <- natAgg(tsD)
    timeSeriesRecoveries <- natAgg(tsR)
    timeSeriesActive <- natAgg(tsA)

    ## Define menus
    # get region names 
    ddNames      <- timeSeriesActive$Region
    ddReg        <- ddNames
    names(ddReg) <- ddNames

    ## write data caches out
    save(ddReg, ddNames, file = paste0("dat/",focusCountry,"/menuData.RData"))
    save(timeSeriesInfections, timeSeriesDeaths, timeSeriesRecoveries, timeSeriesActive, dates, file = paste0("dat/",focusCountry,"/cacheData.RData"))
    
    source("detection/estLocalV2.R")
    load(paste0("dat/",focusCountry,"/estDeconv.RData"))
    dataList[[focusCountry]] <- list(timeSeriesInfections = timeSeriesInfections,
                                     timeSeriesDeaths = timeSeriesDeaths,
                                     timeSeriesRecoveries = timeSeriesRecoveries,
                                     timeSeriesActive = timeSeriesActive,
                                     dates = dates,
                                     ddReg = ddReg,
                                     ddNames = ddNames,
                                     cumulative.infections = cumulative.infections,
                                     active.cases = active.cases)
}



save(dataList, file = "dat/dataList.RData")

