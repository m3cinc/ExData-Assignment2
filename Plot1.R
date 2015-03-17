## Assignment2 - Plot1.R
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
str(NEI)
#>'data.frame':        6497651 obs. of  6 variables:
#>        $ fips     : chr  "09001" "09001" "09001" "09001" ...
#>$ SCC      : chr  "10100401" "10100404" "10100501" "10200401" ...
#>$ Pollutant: chr  "PM25-PRI" "PM25-PRI" "PM25-PRI" "PM25-PRI" ...
#>$ Emissions: num  15.714 234.178 0.128 2.036 0.388 ...
#>$ type     : chr  "POINT" "POINT" "POINT" "POINT" ...
#>$ year     : int  1999 1999 1999 1999 1999 1999 1999 1999 1999 1999 ...
head(NEI)
#> fips      SCC Pollutant Emissions  type year
#> 4  09001 10100401  PM25-PRI    15.714 POINT 1999
#> 8  09001 10100404  PM25-PRI   234.178 POINT 1999
#> 12 09001 10100501  PM25-PRI     0.128 POINT 1999
#> 16 09001 10200401  PM25-PRI     2.036 POINT 1999
#> 20 09001 10200504  PM25-PRI     0.388 POINT 1999
#> 24 09001 10200602  PM25-PRI     1.490 POINT 1999
## Recast NEI$year as.Date
NEI$year<-as.character(NEI$year)
NEI$year<-year(as.Date(NEI$year,"%Y"))
NEI$type<-as.factor(NEI$type)
NEI$Pollutant<-as.factor(NEI$Pollutant)
NEI$SCC<-as.factor(NEI$SCC)
NEI<-as.data.frame(NEI)
str(NEI)
#> 'data.frame':        6497651 obs. of  6 variables:
#> $ fips     : chr  "09001" "09001" "09001" "09001" ...
#> $ SCC      : Factor w/ 5386 levels "10100101","10100102",..: 26 27 30 88 96 99 100 155 159 162 ...
#> $ Pollutant: Factor w/ 1 level "PM25-PRI": 1 1 1 1 1 1 1 1 1 1 ...
#> $ Emissions: num  15.714 234.178 0.128 2.036 0.388 ...
#> $ type     : Factor w/ 4 levels "NON-ROAD","NONPOINT",..: 4 4 4 4 4 4 4 4 4 4 ...
#> $ year     : num  1999 1999 1999 1999 1999 ...
dfsel<-data.frame()
dfsel<-select(NEI,-Pollutant)
dfsel<-NEI[which(NEI$year %in% c(1999,2002,2005,2008)),]
by_year<-group_by(dfsel,year)
data1<-summarize(by_year,Total_PM25=sum(Emissions)/1e6)
##
## Begin plot
##
myPNGfile<-"plot1.png"
png(filename=myPNGfile,width=480,height=480) ## open png device for plot1.png 480x480 pix

with(data1, plot(year,Total_PM25,
                 main="Total Annual PM2.5 Emissions Released",
                 type="b",
                 xlim=c(1999,2008),
                 xlab="Year",
                 ylab="Total Annual PM2.5 Emissions (in Mt)")
)

dev.off() # close png device
##
## verify PNG file exists and indicate its file.info()
print(file.exists(myPNGfile))
#> [1] TRUE
print(file.info(myPNGfile))
