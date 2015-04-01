## transform xls data to list
setwd('/Users/Yuji/Workspace/R/Visburgh/data')
library(gdata)

data.transform <- function(data){
  for(i in 1:ncol(data)){
    if(sum(grepl("\\$", data[[i]])) > 0){
      data[i] <- gsub("\\$", "", data[[i]])
    }
    if(sum(grepl(",", data[[i]])) > 0){
      data[i] <- as.numeric(gsub(",", "", data[[i]]))
    }
    if(sum(grepl("%", data[[i]])) > 0){
      data[i] <- as.numeric(gsub("%", "", data[[i]]))/100
    }
  }
  data
}

readXLS <- function(file){
  data.list <- list()
  sheetList <- sheetNames(file)
  for(i in 1:length(sheetList)){
    data.list[[sheetList[i]]] <- data.transform(read.xls(file, sheet = sheetList[i], na.strings = "N/A", check.names = FALSE))
  }
  data.list
}

xls.file <- "PGHSNAP.xls"
data.list <- readXLS(xls.file)
# colnames(newdata) <- gsub("\\.+",".",colnames(newdata))
save(data.list, file = "pitt.RData")

## process and merge county level data
setwd('/Users/Yuji/Workspace/R/Visburgh/data')
crime <- get(load('ICPSR_35019/DS0001/35019-0001-Data.rda'))
crime <- subset(crime, COVIND >= 90)
price <- read.csv('County_MedianSoldPrice_AllHomes.csv', check.names = F)
value <- read.csv('County_Zhvi_AllHomes.csv', check.names = F)
price.2012 <- price[c(1:5, 195:206)]
price.2012 <- price.2012[apply(price.2012, 1, function(x){mean(!is.na(x)) >= 0.5}), ]
value.2012 <- value[c(1:5, 195:206)]
value.2012 <- value.2012[apply(value.2012, 1, function(x){mean(!is.na(x)) >= 0.5}), ]

price.2012$Median.Sale.Price <- 
  apply(price.2012[-c(1:5)], 1, 
        function(x){
          median(x, na.rm=T)
        })
value.2012$Median.Home.Value <- 
  apply(value.2012[-c(1:5)], 1, 
        function(x){
          median(x, na.rm=T)
        })

county <- merge(price.2012[-c(6:17)], value.2012[-c(1:3, 6:17)], by = c("StateCodeFIPS", "MunicipalCodeFIPS"))
county <- merge(county, crime[-c(1:4)], 
                by.x = c("StateCodeFIPS", "MunicipalCodeFIPS"), 
                by.y = c("FIPS_ST", "FIPS_CTY"))

StateCodeFIPS <- formatC(county[[1]], width = 2, format = "d", flag = "0")
MunicipalCodeFIPS <- formatC(county[[2]], width = 3, format = "d", flag = "0")
FIPS <- paste(StateCodeFIPS, MunicipalCodeFIPS, sep = "")
row.names(county) <- FIPS
save(county, file = "county.level.RData")
