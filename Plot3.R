## Assignment2 - Plot3.R
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
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
## This confirms there's only one level of Pollutant: PM25-PRI" in NEI data frame
## Building the query
dfsel<-data.frame()
dfsel<-select(NEI,-Pollutant)                       # exclude this single factor
dfsel<-dfsel[which(dfsel$year %in% c(1999:2008)),]    # using Which avoids dealing with NA if present
dfsel<-dfsel[which(dfsel$fips=="24510"),]           # only for Baltimore City, Maryland fips==24510
by_year_type<-group_by(dfsel,year,type)             # summarize by year and by type
data3<-summarize(by_year_type,Total_PM25=sum(Emissions)/1e3)
data3$year<-factor(data3$year)
data3A<-as.data.frame(data3)                        # second dataset for trend lines
data3A$year<-as.numeric(factor(data3A$year))
##
## Begin plot
##
myPNGfile<-"plot3.png"
png(filename=myPNGfile,width=480,height=480)        # open png device for plot1.png 480x480 pix
ggplot (data3,
        aes(factor(year),y=Total_PM25))+
        geom_bar(stat="identity",
                 color="blue",fill="blue")+
        stat_smooth(data=data3A,
                    aes(x=year,y=Total_PM25,group=type),
                    fill="blue",
                    color="orange",
                    size=1,
                    method=lm,
                    se=FALSE)+
        facet_wrap(~type)+
        labs(title="Total Yearly PM_25 Emission and linear trendlines\nBaltimore City, MD")+
        xlab("Year")+
        ylab("Total Yearly PM_25 Emission (kt)")

dev.off() # close png device
##
## verify PNG file exists and indicate its file.info()
print(file.exists(myPNGfile))
#> [1] TRUE
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot3.png 7081 FALSE  666 2015-03-22 10:35:03 2015-03-16 20:20:57 2015-03-16 20:20:57  no
