## Assignment2 - Plot6.R
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
##
SCC <- readRDS("Source_Classification_Code.rds")
SCC<-as.data.frame(SCC)
## search strategy: select "[Vv]eh" RegEx to subset the query match
dfsel<-data.frame()
dfsel<-SCC[which(SCC$Short.Name %in% SCC$Short.Name[grep("[Vv]eh",SCC$Short.Name)]),]
nrow(dfsel);head(dfsel$Short.Name,5);tail(dfsel$Short.Name,5)
#> [1] 1185
#> [1] Highway Veh - Gasoline - Light Duty Vehicles (LDGV) - Total: All Road Types   
#> [2] Highway Veh - Gasoline - Light Duty Vehicles (LDGV) - Rural Interstate: Total 
#> [3] Highway Veh - Gasoline - Light Duty Vehicles (LDGV) - Interstate: Rural Time 1
#> [4] Highway Veh - Gasoline - Light Duty Vehicles (LDGV) - Interstate: Rural Time 2
#> [5] Highway Veh - Gasoline - Light Duty Vehicles (LDGV) - Interstate: Rural Time 3
#> 11238 Levels:  ... Zinc Production /Zinc Melting
#> [1] Lime Manuf /Vehicle Traffic                                                                 
#> [2] Petrol Trans & Marketg /Filling Vehicle Gas Tanks - Stage II /Vapor Loss w/o Controls       
#> [3] Petrol Trans & Marketg /Filling Vehicle Gas Tanks - Stage II /Liquid Spill Loss w/o Controls
#> [4] Petrol Trans & Marketg /Filling Vehicle Gas Tanks - Stage II /Vapor Loss w/o Controls       
#> [5] Petrol Trans & Marketg /Filling Vehicle Gas Tanks - Stage II /Not Classified **             
#> 11238 Levels:  ... Zinc Production /Zinc Meltingdfsel2<-SCC[which(SCC$Short.Name %in% SCC$Short.Name[grep("[Cc]omb( .*)[Cc]oal",SCC$Short.Name)]),] 
##
## the query returned 1185 matches
##
selection<-dfsel$SCC                                # holds the matching SCC subset
dfsel<-select(NEI,-Pollutant)                       # exclude this single factor
dfsel<-dfsel[which(NEI$year %in% c(1999:2008)),]    # using Which avoids dealing with NA if present
dfsel<-dfsel[which(dfsel$SCC %in% selection),]
dfsel<-dfsel[which(dfsel$fips %in% c("06037","24510")),]  # select Los Angeles and Baltimore
by_year_city<-group_by(dfsel,year,fips)
data6<-summarize(by_year_city,Total_PM25=sum(Emissions)/1e3)
data6$fips<-as.factor(x = data6$fips)               # need to recast fips as factor
levels(data6$fips)<-c("Los Angeles County, CA","Baltimore City,MD")     # with levels LA and BAL
##
## Begin plot
##
myPNGfile<-"plot6.png"
png(filename=myPNGfile,width=480,height=480) ## open png device for plot1.png 480x480 pix

qplot(year,Total_PM25,data=data6,facets=.~fips,binwidth=2,
                 main="Total Annual PM2.5 Motor Vehicle Emissions Comparison",
                 xlim=c(1999,2008),
                 type="b",
                 xlab="Year",
                 ylab="Total Annual PM2.5 Motor Vehicle Emissions Comparison (in kt)")

dev.off() # close png device
##
## verify PNG file exists and indicate its file.info()
print(file.exists(myPNGfile))
#> [1] TRUE
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot6.png 6468 FALSE  666 2015-03-17 17:21:32 2015-03-17 15:44:08 2015-03-17 15:44:08  no
