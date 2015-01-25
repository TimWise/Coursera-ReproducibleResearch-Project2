# What types of Weather Events cause the most Casualties and Economic Impact?
Tim Wise  
January 25, 2015  

###  Synopsis

This is an analysis of the National Oceanic and Atmospheric Administration's (NOAA) storm database to find out which types of weather events in the United States cause the most casualties, including injuries and fatalities, and which cause the most economic impact to property and crops.

The analysis shows that for the years 1995-2011 **tornados** have caused the most injuries and total casualties while **extreme heat** has caused the most fatalities.  **Floods** and **hurricanes** have caused the most property damage while **drought** has caused the most crop damage.

The following sections describe the details of the analysis.

###  Data Processing

The data for this analysis was from the [U.S. National Oceanic and Atmospheric Administration (NOAA)](http://www.ncdc.noaa.gov/swdi/#Intro). The actual data set used can be downloaded here:

* [Storm Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2)  

Documentation on the fields in the data set is here:

* [National Weather Service Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)  
* [National Climatic Data Center Storm Events FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC Storm Events-FAQ Page.pdf)

Once we read in the data set, the major processing steps in this analysis are:

1. **Select events from 1995 and later**: Around the mid-1990's, there was a significant increase in the total number of events reports and in the types of events recorded. To make a fair comparison among event types.

1. **Compute the economic impact**: A small number of the economic impact values were not encoded correctly in the data set. We excluded those that were not.

1. **Standardize the event types**: The NOAA data recording guidelines list 48 standard event types, yet the data set includes over 900 event types. We selected all the standard event types, plus we mapped a few obviously misspelled or misnamed categories to the standard set. 

The following sections discuss these steps in more detail.

#### Read in the data set

Load the require libraries:

```r
library(knitr)
library(dplyr)
library(reshape2)
library(ggplot2)
```
Download the data file, if necessary:

```r
datafile <- "repdata-data-StormData.csv.bz2"
if (!file.exists(datafile)) {
  url  = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(url, datafile)
}
```
Read the raw data set:

```r
raw <- read.csv("repdata-data-StormData.csv.bz2", stringsAsFactors=F)
#str(raw)
```

Select the columns we need for this analysis and filter to get the events with casualties or economic damage. 

```r
df <- raw %>% 
  select (BGN_DATE,
          EVTYPE,
          INJURIES, 
          FATALITIES, 
          PROPDMG, 
          PROPDMGEXP,
          CROPDMG, 
          CROPDMGEXP
          ) %>%
  filter (INJURIES   > 0 | 
          FATALITIES > 0 | 
          PROPDMG    > 0 | 
          CROPDMG    > 0  
          ) %>%
  mutate (YEAR = format(strptime(BGN_DATE, format='%m/%d/%Y'), format='%Y'),
          EVTYPE = tolower(EVTYPE),
          PROPDMGEXP = tolower(PROPDMGEXP),
          CROPDMGEXP = tolower(CROPDMGEXP)
          )
#str(df)
```

#### Select events from 1995 and later

Looking at the number of weather events for each year, we see there was a significant increase in the mid-1990's. The increase from 1992 to 1995 is over 10x: 

```r
t(t(table(df$YEAR)))
```

```
##       
##        [,1] 
##   1950   201
##   1951   241
##   1952   233
##   1953   421
##   1954   491
##   1955   441
##   1956   428
##   1957   824
##   1958   543
##   1959   505
##   1960   556
##   1961   627
##   1962   411
##   1963   380
##   1964   594
##   1965   724
##   1966   423
##   1967   683
##   1968   524
##   1969   458
##   1970   517
##   1971   714
##   1972   579
##   1973  1026
##   1974   884
##   1975   748
##   1976   707
##   1977   693
##   1978   620
##   1979   655
##   1980   728
##   1981   578
##   1982  1128
##   1983  1019
##   1984   920
##   1985   575
##   1986   690
##   1987   563
##   1988   678
##   1989   791
##   1990   986
##   1991   879
##   1992   990
##   1993  5838
##   1994  9644
##   1995 10457
##   1996 10040
##   1997 10322
##   1998 14013
##   1999 10609
##   2000 11508
##   2001 10298
##   2002 10432
##   2003 11015
##   2004 10484
##   2005 10014
##   2006 11974
##   2007 11953
##   2008 17633
##   2009 14434
##   2010 16019
##   2011 20570
```

And for 1992 and earlier, the there were only 3 event types recorded:

```r
unique(df[df$YEAR <= '1992', 'EVTYPE'])
```

```
## [1] "tornado"   "tstm wind" "hail"
```

To ensure a fair comparison of event types, select only events from 1995 and later

```r
df <- df %>% filter (YEAR >= '1995')
#str(df)
```

#### Compute economic impact of each event

In this section, we convert property and crop damages to billions of dollars.

To compute dollar amounts, we need to decode the (string-based) exponent. As outlined in ..., the exponent is suppose to be:  

* 'b' for billions of dollars  
* 'm' for millions of dollars  
* 'k' for thousands of dollars  

For those that are not correctly encoded, we're going effectively exclude them by assuming they are 'dollars'. 
 
Looking at the number of events for each encoding value, we see that most events are correctly encoded: 

```r
t(t(table(c(df$PROPDMGEXP, df$CROPDMGEXP))))
```

```
##    
##     [,1]  
##     121609
##   -      1
##   ?      4
##   +      4
##   0    189
##   2      1
##   3      1
##   4      3
##   5     17
##   6      3
##   7      3
##   b     42
##   h      6
##   k 291823
##   m   9844
```

Decode the string-based exponent to an integer power:

```r
df$PropDmgPower <- 0

df[df$PROPDMGEXP == 'b','PropDmgPower'] <- 9
df[df$PROPDMGEXP == 'm','PropDmgPower'] <- 6
df[df$PROPDMGEXP == 'k','PropDmgPower'] <- 3
#table(df$PROPDMGEXP, df$PropDmgPower)

df$CropDmgPower <- 0

df[df$CROPDMGEXP == 'b','CropDmgPower'] <- 9
df[df$CROPDMGEXP == 'm','CropDmgPower'] <- 6
df[df$CROPDMGEXP == 'k','CropDmgPower'] <- 3
#table(df$CROPDMGEXP, df$CropDmgPower)
```

Compute the property and crop damages in billions of dollars:

```r
df$PropDmgB <- df$PROPDMG / (10 ^ (9 - df$PropDmgPower))
df$CropDmgB <- df$CROPDMG / (10 ^ (9 - df$CropDmgPower))
```

#### Standardize event types

The standard events types are defined in ... We define those standard event types here:

```r
validEvents <- c('Astronomical Low Tide',
                 'Avalanche',
                 'Blizzard',
                 'Coastal Flood',
                 'Cold/Wind Chill',
                 'Debris Flow',
                 'Dense Fog',
                 'Dense Smoke',
                 'Drought',
                 'Dust Devil',
                 'Dust Storm',
                 'Excessive Heat',
                 'Extreme Cold/Wind Chill',
                 'Flash Flood',
                 'Flood',
                 'Frost/Freeze',
                 'Funnel Cloud',
                 'Freezing Fog',
                 'Hail',
                 'Heat',
                 'Heavy Rain',
                 'Heavy Snow',
                 'High Surf',
                 'High Wind',
                 'Hurricane (Typhoon)',
                 'Ice Storm',
                 'Lake-Effect Snow',
                 'Lakeshore Flood',
                 'Lightning',
                 'Marine Hail',
                 'Marine High Wind',
                 'Marine Strong Wind',
                 'Marine Thunderstorm Wind',
                 'Rip Current',
                 'Seiche',
                 'Sleet',
                 'Storm Surge/Tide',
                 'Strong Wind',
                 'Thunderstorm Wind',
                 'Tornado',
                 'Tropical Depression',
                 'Tropical Storm',
                 'Tsunami',
                 'Volcanic Ash',
                 'Waterspout',
                 'Wildfire',
                 'Winter Storm',
                 'Winter Weather')

validEvents <- tolower(validEvents)
```

There are some non-standard event types that are obvious misspellings or miscodings yet represented a significant number of events. We include those event types in the list of valid event types:

```r
validEvents <- c(validEvents,
                 'tstm wind',
                 'thunderstorm winds',
                 'hurricane',
                 'hurricane/typhoon')
```

Select only these event types:

```r
nbefore <- nrow(df)

df <- df %>% filter(EVTYPE %in% validEvents)

nafter <- nrow(df)
```
This selection captures 206981 of 211775, or 98%, of the number of events.

Now lets map the non-standard event types we included to the standard ones:

```r
df$EventType <- df$EVTYPE

df[df$EVTYPE == 'tstm wind',          'EventType'] <- 'Thunderstorm Wind'
df[df$EVTYPE == 'thunderstorm winds', 'EventType'] <- 'Thunderstorm Wind'
df[df$EVTYPE == 'hurricane',          'EventType'] <- 'Hurricane (Typhoon)'
df[df$EVTYPE == 'hurricane/typhoon',  'EventType'] <- 'Hurricane (Typhoon)'

df$EventType <- tolower(df$EventType)
```

Let's look at the number of each type of events:

```r
t(t(rev(sort(table(df$EventType)))))
```

```
##                           
##                              [,1]
##   thunderstorm wind        110167
##   hail                      23860
##   flash flood               19850
##   tornado                   13000
##   lightning                 12026
##   flood                      9712
##   high wind                  5416
##   strong wind                3370
##   winter storm               1479
##   heavy snow                 1134
##   heavy rain                 1080
##   wildfire                    853
##   excessive heat              698
##   ice storm                   653
##   tropical storm              412
##   winter weather              407
##   rip current                 389
##   avalanche                   266
##   drought                     264
##   blizzard                    230
##   heat                        203
##   hurricane (typhoon)         198
##   lake-effect snow            194
##   coastal flood               156
##   high surf                   132
##   frost/freeze                117
##   extreme cold/wind chill     111
##   dust storm                   98
##   dust devil                   90
##   cold/wind chill              90
##   dense fog                    64
##   storm surge/tide             47
##   marine strong wind           46
##   tropical depression          35
##   marine thunderstorm wind     33
##   waterspout                   30
##   marine high wind             19
##   tsunami                      14
##   seiche                        9
##   funnel cloud                  9
##   freezing fog                  7
##   lakeshore flood               5
##   volcanic ash                  2
##   marine hail                   2
##   astronomical low tide         2
##   sleet                         1
##   dense smoke                   1
```

#### Format data for plots

Create data for the casualty chart: 

1. Sum the total number fatalities and injuries for each storm type 

1. Compute the total number of causalities and take the top 10 weather events 

1. Reshape the data into narrow format where Category is the Injury/Fatality factor and NumberOfPeopleAffected is the value


```r
df.casualties <- df %>% 
  group_by(EventType
           ) %>% 
  summarise(Injuries   = sum(INJURIES),
            Fatalities = sum(FATALITIES),
            Casualties = Injuries + Fatalities
            ) %>% 
  arrange(desc(Casualties))
#str(df.casualties)

df.casualties <- df.casualties[c(1:10),]

df.casualties <- melt(df.casualties, 
                      id.vars=c('EventType', 'Casualties'), 
                      variable.name="Category",
                      value.name='NumberOfPeopleAffected'
                      )
#str(df.casualties)
```

Create data for the economic damages chart: 

1. Sum the total property and crop damages for each storm type 

1. Compute the total economic damage and take the top 10 weather events 

1. Reshape the data into narrow format where Category is the Property/Crop factor and BillionsOfDollars is the value  


```r
df.economic <- df %>% 
  group_by(EventType
           ) %>% 
  summarise(Property   = sum(PropDmgB),
            Crops      = sum(CropDmgB),
            TotalDamages = Property + Crops
            ) %>% 
  arrange(desc(TotalDamages))
#str(df.economic)

df.economic <- df.economic[c(1:10),]

df.economic <- melt(df.economic, 
                      id.vars=c('EventType', 'TotalDamages'), 
                      variable.name="Category",
                      value.name='BillionsOfDollars'
                      )
#str(df.economic)
```

###  Results

####  Which types of events are most harmful to population health?


```r
qplot(geom="bar",
      data=df.casualties,
      x=EventType,
      weight=NumberOfPeopleAffected,
      facets=~Category,
      main="Weather Events causing most Casualties from 1995-2011",
      xlab="",
      ylab='Number of Persons Affected'
) + 
  coord_flip()
```

![](RepData_PeerAssessment2_files/figure-html/unnamed-chunk-18-1.png) 

The figure above shows the top 10 weather events in terms of total number of casualties (injuries plus fatalities.) [TODO: Order the chart, Woodrow.]  

From this chart we see:  

* **Tornados** cause, by far, the most number of injuries and, as such, the greatest number of casualties.  

* **Excessive heat** causes the most number of deaths. That number would be even higher if it were grouped with the fatalites from **heat**.  

####  Which types of events have the greatest economic consequences?

```r
qplot(geom="bar",
      data=df.economic,
      x=EventType,
      weight=BillionsOfDollars,
      facets=~Category,
      main="Weather Events causing most Economic Damage from 1995-2011",
      xlab="",
      ylab='Billions of US Dollars'
) + 
  coord_flip()
```

![](RepData_PeerAssessment2_files/figure-html/unnamed-chunk-19-1.png) 

The figure above shows the top 10 weather events in terms of total number of damages to crops and property. [TODO: Not again, Woodrow? Shees.] 

From this chart we see: 

* **Drought** causes the most damage to crops, followed by **flooding** and **hurricanes**.  

* **Floods** cause, by far, the largest amount of property damage, followed by **hurricanes** and **tornadoes**.  

###  Caveats

This data set is quite dirty and this analysis took the data set pretty much at face value. As such, there are several caveats for this analysis that should be addressed in a more in-depth study, including:  

* No adjustment for inflation - Dollar amounts for different years were simply added and not adjusted for inflation  

* Not all event types not standardized - We made an attempt standardize event types, but not all events with casualties or economic impact were capture. The impact of missing events is unknown.

* No outlier analysis - There was one event, a flood in Napa in 2006, that caused the most economic impact. No attempt was made to see if excluding or modifying this event affected the analysis. 
 

