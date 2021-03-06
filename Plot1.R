## Assignment2 - Plot1.R
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
library(plyr)
library(dplyr)
library(tidyr)
str(NEI)
#>'data.frame':        6497651 obs. of  6 variables:
#>$ fips     : chr  "09001" "09001" "09001" "09001" ...
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
NEI$type<-as.factor(NEI$type)
NEI$SCC<-as.factor(NEI$SCC)
NEI$Pollutant<-as.factor(NEI$Pollutant)
str(NEI)
#> 'data.frame':        6497651 obs. of  6 variables:
#> $ fips     : chr  "09001" "09001" "09001" "09001" ...
#> $ SCC      : Factor w/ 5386 levels "10100101","10100102",..: 26 27 30 88 96 99 100 155 159 162 ...
#> $ Pollutant: Factor w/ 1 level "PM25-PRI": 1 1 1 1 1 1 1 1 1 1 ...
#> $ Emissions: num  15.714 234.178 0.128 2.036 0.388 ...
#> $ type     : Factor w/ 4 levels "NON-ROAD","NONPOINT",..: 4 4 4 4 4 4 4 4 4 4 ...
#> $ year     : num  1999 1999 1999 1999 1999 ...
## This confirms there's only one level of Pollutant: "PM25-PRI" in NEI data frame
## Building the query
dfsel<-data.frame()
dfsel<-select(NEI,-Pollutant)                       # exclude this single factor
dfsel<-dfsel[which(dfsel$year %in% c(1999:2008)),]  # using Which avoids dealing with NA if present
by_year<-group_by(dfsel,year)
data1<-summarize(by_year,Total_PM25=sum(Emissions)/1e6) # convert sum to Mt to drop many 0s in display
data1<-as.matrix(data1)                             # recast as matrix type needed to display barplot
##
## Begin plot
##
myPNGfile<-"plot1.png"
png(filename=myPNGfile,width=480,height=480)        # open png device for plot1.png 480x480 pix

barplot(data1[,2],       
        beside=TRUE,
        names.arg=data1[,1],                                    # recall names from matrix 1st row
        col="blue",
        xlab="Year",
        ylab="Total PM2.5 Emissions (in Mt)",
        ylim=c(0,ceiling(max(data1[,2]))),                      # rescale y-axis with easy integers
        main="Total Annual PM2.5 Emissions Released - US")

data1<-as.data.frame(data1)                                     # recast for abline as data frame
data1$year<-as.numeric(factor(data1$year))                      # needed to proper rescaling of year
abline(lm(Total_PM25 ~ year, data = data1),                     # use linear model for trendline
        col="orange",
        lty = 1, 
        lwd = 2)

text(x=3,y=0.7+data1[3,2],labels="Trendline",col="orange")      # indicate the trendline
arrows(3,0.5+data1[3,2],3,5,col='orange', length=0.1, lwd=2)    # place arrow

dev.off() # close png device
##
## verify PNG file exists and indicate its file.info()
print(file.exists(myPNGfile))
#> [1] TRUE
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot1.png 5095 FALSE  666 2015-03-22 11:28:37 2015-03-22 10:59:56 2015-03-22 10:59:56  no

