## Assignment2 - Plot4.R
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
## 1st search strategy: select "[Cc]oal" and "[Cc]omb" RegEx in succession to subset the query match
dfsel<-data.frame()
dfsel<-SCC[which(SCC$Short.Name %in% SCC$Short.Name[grep("[Cc]oal",SCC$Short.Name)]),]
dfsel<-dfsel[which(dfsel$Short.Name %in% dfsel$Short.Name[grep("[Cc]omb",dfsel$Short.Name)]),]
dfsel$Short.Name
nrow(dfsel);head(dfsel$Short.Name,5);tail(dfsel$Short.Name,5)
#> [1] 91
#> [1] "Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal"                         
#> [2] "Ext Comb /Electric Gen /Anthracite Coal /Traveling Grate (Overfeed) Stoker"       
#> [3] "Ext Comb /Electric Gen /Bituminous Coal /Pulverized Coal: Wet Bottom"             
#> [4] "Ext Comb /Electric Gen /Bituminous Coal /Pulverized Coal: Dry Bottom"             
#> [5] "Ext Comb /Electric Gen /Bituminous Coal /Cyclone Furnace"                         
#> [1] "Stationary Fuel Comb /Residential /Anthracite Coal /Total: All Combustor Types"                 
#> [2] "Stationary Fuel Comb /Residential /Bituminous/Subbituminous Coal /Total: All Combustor Types"   
#> [3] "Stationary Fuel Comb /Total Area Source /Anthracite Coal /Total: All Boiler Types"              
#> [4] "Stationary Fuel Comb /Total Area Source /Bituminous/Subbituminous Coal /Total: All Boiler Types"
#> [5] "Stationary Fuel Comb /Total Area Source /Lignite Coal /Total: All Boiler Types"                 
##
## the query returned 91 matches
##
## Now try alternate strategy for single shot retrieval using "[Cc]omb( .*)[Cc]oal" RegEx
##
dfsel2<-SCC[which(SCC$Short.Name %in% SCC$Short.Name[grep("[Cc]omb( .*)[Cc]oal",SCC$Short.Name)]),] 
identical(dfsel,dfsel2)
#> [1] TRUE
## since the two queries are identical, let's discard one copy
rm(dfsel2)      
selection<-dfsel$SCC                                # holds the matching SCC subset
dfsel<-select(NEI,-Pollutant)                       # exclude this single factor
dfsel<-dfsel[which(dfsel$year %in% c(1999:2008)),]  # using Which avoids dealing with NA if present
dfsel<-dfsel[which(dfsel$SCC %in% selection),]
dfsel<-arrange(dfsel, desc(Emissions))
by_year_type<-group_by(dfsel,year,type)
data4<-summarize(by_year_type,Total_PM25=sum(Emissions)/1e3)
data4$year<-factor(data4$year)
dfsel<-select(dfsel,-type)                      
by_year<-group_by(dfsel,year)                          # second set for total Emissions PM_25 trendline
data4A<-summarize(by_year,Total_PM25=sum(Emissions)/1e3)
data4A$year<-factor(data4A$year)
data4A<-as.data.frame(data4A)                     
data4A$year<-as.numeric(factor(data4A$year))
##
## Begin plot
##
myPNGfile<-"plot4.png"
png(filename=myPNGfile,width=480,height=480) ## open png device for plot1.png 480x480 pix
ggplot(data=data4,aes(factor(year),y=Total_PM25,fill=type))+
        geom_bar(stat="identity",color="black")+
        scale_fill_brewer()+
        stat_smooth(data=data4A,
                    aes(x=year,y=Total_PM25),
                    fill="blue",
                    color="orange",
                    size=1,
                    method=lm,
                    se=FALSE)+
        labs(title="Total Annual PM2.5 Coal Combustion Related Emissions - US\n with Linear Total Trendline")+
        xlab("Year")+
        ylab("PM2.5 Coal Combustion Related Emissions (in kt)")
dev.off() # close png device
##
## verify PNG file exists and indicate its file.info()
print(file.exists(myPNGfile))
#> [1] TRUE
print(file.info(myPNGfile))
#> size isdir mode               mtime               ctime               atime exe
#> plot4.png 6504 FALSE  666 2015-03-22 10:35:52 2015-03-17 12:54:03 2015-03-17 12:54:03  no
