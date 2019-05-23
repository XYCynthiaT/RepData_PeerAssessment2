---
title: "Impacts of Weather Events on Population Health and Economics in the U.S."
author: "Cynthia Tang"
date: "May 22, 2019"
output: 
  html_document:
    keep_md: true
---


```r
knitr::opts_chunk$set(echo = TRUE)
```

## Synopsis

In this report we aim to figure out the types of weather events that 1) are most
harmful to population health and 2) have the worst economic consequences. To 
investigate these questions, we explored 
the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. 
The database recorded measurements for public health and economic consequences with
the estimates of any fatalities, injuries, and property damage.
We found that the tornado was the most harmful to overall public health in the U.S. from
1950 to 2011. The flood caused the greatest property and overall economic damage. 
The drought caused most crop damage.

## Downloading files

We obtained the U.S. National Oceanic and Atmospheric Administration's (NOAA) 
storm database. Data are collected from April 1950 to November 2011.


```r
if(!file.exists("./data")) dir.create("data")
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
              "./data/stormData.csv.bz2", mode = "wb")
```

## Loading packages

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(gridExtra)
```

```
## Warning: package 'gridExtra' was built under R version 3.5.3
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```


## Data processing

Reading in the data and check the first few rows in the dataset. Missing values 
in the dataset are denoted as empty. After loading the data, we transferred the 
column names and values of evtypes to lower cases.


```r
# loading data
data <- read.csv("./data/stormData.csv.bz2", na.strings = "", stringsAsFactors = FALSE)
dim(data)
```

```
## [1] 902297     37
```

```r
names(data) <- tolower(names(data))
data$evtype <- tolower(data$evtype)
head(data)
```

```
##   state__           bgn_date bgn_time time_zone county countyname state
## 1       1  4/18/1950 0:00:00     0130       CST     97     MOBILE    AL
## 2       1  4/18/1950 0:00:00     0145       CST      3    BALDWIN    AL
## 3       1  2/20/1951 0:00:00     1600       CST     57    FAYETTE    AL
## 4       1   6/8/1951 0:00:00     0900       CST     89    MADISON    AL
## 5       1 11/15/1951 0:00:00     1500       CST     43    CULLMAN    AL
## 6       1 11/15/1951 0:00:00     2000       CST     77 LAUDERDALE    AL
##    evtype bgn_range bgn_azi bgn_locati end_date end_time county_end
## 1 tornado         0    <NA>       <NA>     <NA>     <NA>          0
## 2 tornado         0    <NA>       <NA>     <NA>     <NA>          0
## 3 tornado         0    <NA>       <NA>     <NA>     <NA>          0
## 4 tornado         0    <NA>       <NA>     <NA>     <NA>          0
## 5 tornado         0    <NA>       <NA>     <NA>     <NA>          0
## 6 tornado         0    <NA>       <NA>     <NA>     <NA>          0
##   countyendn end_range end_azi end_locati length width f mag fatalities
## 1         NA         0    <NA>       <NA>   14.0   100 3   0          0
## 2         NA         0    <NA>       <NA>    2.0   150 2   0          0
## 3         NA         0    <NA>       <NA>    0.1   123 2   0          0
## 4         NA         0    <NA>       <NA>    0.0   100 2   0          0
## 5         NA         0    <NA>       <NA>    0.0   150 2   0          0
## 6         NA         0    <NA>       <NA>    1.5   177 2   0          0
##   injuries propdmg propdmgexp cropdmg cropdmgexp  wfo stateoffic zonenames
## 1       15    25.0          K       0       <NA> <NA>       <NA>      <NA>
## 2        0     2.5          K       0       <NA> <NA>       <NA>      <NA>
## 3        2    25.0          K       0       <NA> <NA>       <NA>      <NA>
## 4        2     2.5          K       0       <NA> <NA>       <NA>      <NA>
## 5        2     2.5          K       0       <NA> <NA>       <NA>      <NA>
## 6        6     2.5          K       0       <NA> <NA>       <NA>      <NA>
##   latitude longitude latitude_e longitude_ remarks refnum
## 1     3040      8812       3051       8806    <NA>      1
## 2     3042      8755          0          0    <NA>      2
## 3     3340      8742          0          0    <NA>      3
## 4     3458      8626          0          0    <NA>      4
## 5     3412      8642          0          0    <NA>      5
## 6     3450      8748          0          0    <NA>      6
```

```r
names(data)
```

```
##  [1] "state__"    "bgn_date"   "bgn_time"   "time_zone"  "county"    
##  [6] "countyname" "state"      "evtype"     "bgn_range"  "bgn_azi"   
## [11] "bgn_locati" "end_date"   "end_time"   "county_end" "countyendn"
## [16] "end_range"  "end_azi"    "end_locati" "length"     "width"     
## [21] "f"          "mag"        "fatalities" "injuries"   "propdmg"   
## [26] "propdmgexp" "cropdmg"    "cropdmgexp" "wfo"        "stateoffic"
## [31] "zonenames"  "latitude"   "longitude"  "latitude_e" "longitude_"
## [36] "remarks"    "refnum"
```

The `fatalities` and `injuries` columns contain the health measurements. The `propdmg`
and `cropdmg` columns contain the economic consequence measurements. The `propdmgex`
and `cropdmgex` indicated the exponent of property damage and crop damage. The `evtype` 
column contains event types. Here, we extra these colums and make a brief summary of these columns.


```r
subdata <- data[, c(8, 23:28)]
str(subdata)
```

```
## 'data.frame':	902297 obs. of  7 variables:
##  $ evtype    : chr  "tornado" "tornado" "tornado" "tornado" ...
##  $ fatalities: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ injuries  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ propdmg   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ propdmgexp: chr  "K" "K" "K" "K" ...
##  $ cropdmg   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ cropdmgexp: chr  NA NA NA NA ...
```

```r
length(unique(subdata$evtype))
```

```
## [1] 898
```

```r
unique(subdata$propdmgex)
```

```
##  [1] "K" "M" NA  "B" "m" "+" "0" "5" "6" "?" "4" "2" "3" "h" "7" "H" "-"
## [18] "1" "8"
```

```r
unique(subdata$cropdmgex)
```

```
## [1] NA  "M" "K" "m" "B" "?" "0" "k" "2"
```

Then, we calculated the actual values with given exponent. 


```r
subdata$propdmgexp <- tolower(subdata$propdmgexp)
subdata$cropdmgexp <- tolower(subdata$cropdmgexp)
trans <- function(exp = "") {
        switch(exp, 
               "-" = 0.1,
               "?" = 1,
               "+" = 1,
               "1" = 1,
               "2" = 100,
               "3" = 1000,
               "4" = 10000,
               "5" = 10^5,
               "6" = 10^6,
               "7" = 10^7,
               "8" = 10^8,
               "h" = 100,
               "k" = 1000,
               "m" = 10^6,
               "b" = 10^9,
               0
               )
}
actual <- function(x, exp){
        x * trans(exp)
}
subdata$propdmga <- purrr::map2_dbl(subdata[, 4], subdata[, 5], actual)
subdata$cropdmga <- purrr::map2_dbl(subdata[, 6], subdata[, 7], actual)
```

## Results

### Impacts of Severe Weather Event on Population health

Here, we calculated the sum of fatalities and injuries by each type of events.
Also, we calculated the overall health impact by adding the sum of fatalities and 
injuries together. Then, we selected the top 10 event types with the primary index,
overall health impact, the secondary index, fatalities, and the third index, injuries.  

The figure below showed the top 10 event types that caused the greatest damage to
public health and the corresponding number of people suffered. 
The y-aixs is shown in log10 scale.


```r
health <- subdata %>%
        group_by(evtype) %>%
        summarise(fatalities = sum(fatalities, na.rm = T), 
                  injuries = sum(injuries, na.rm = T),
                  both = fatalities + injuries) %>%
        arrange(desc(both), desc(fatalities), desc(injuries))
health10 <- health[1:10,]

both <- ggplot(data = health10, aes(reorder(evtype, both), both)) + 
        geom_bar(stat = "identity", fill = "blue") +
        scale_y_log10()+
        coord_flip() +
        labs(y = "Total number of fatalities and injuries", x = "event type") 
fatal <- ggplot(data = health10, aes(reorder(evtype, fatalities), fatalities)) + 
        geom_bar(stat = "identity", fill = "lightblue") +
        scale_y_log10()+
        coord_flip() +
        labs(y = "Total number of fatalities", x = "event type") 
injury <- ggplot(data = health10, aes(reorder(evtype, injuries), injuries)) +
        geom_bar(stat = "identity", fill = "lightblue") +
        scale_y_log10()+
        coord_flip() +
        labs(y = "Total number of injuries", x = "event type") 
grid.arrange(both, fatal, injury, 
             top = "The top 10 types of weather events that caused severe population health problems")
```

![](stormData_files/figure-html/health-1.png)<!-- -->

the tornado was most harmful to population health in the U.S. from 1950 to 
2011. as it caused most fatalities and injuries. 
The excessive heat caused the second most fatalities and the TSTM wind
caused the second most injuries.

### Impacts of Severe Weather Event on economic damage

Similar to the population health part, we calculated the sum of property 
damage and crop damage as well as the sum of both by each type of events.
Then, we selected the top 10 event types using the primary index,
overall economic damage, the secondary index, property damage, and the third 
index, crop damage.  

The figure below showed the top 10 event types that caused the greatest economic 
damage and the estimates of corresponding damage in billion dollars. 


```r
economy <- subdata %>%
        group_by(evtype) %>%
        summarise(propdmg = sum(propdmga, na.rm = T)/10^9, 
                  cropdmg = sum(cropdmga, na.rm = T)/10^9,
                  both = propdmg + cropdmg) %>%
        arrange(desc(both), desc(propdmg), desc(cropdmg))
economy10 <- economy[1:10,]

both <- ggplot(data = economy10, aes(reorder(evtype, both), both)) + 
        geom_bar(stat = "identity", fill = "blue") +
        coord_flip() +
        labs(y = "Total number of property and crop damage (in billion dolars)", x = "event type")
prop <- ggplot(data = economy10, aes(reorder(evtype, propdmg), propdmg)) + 
        geom_bar(stat = "identity", fill = "lightblue") +
        coord_flip() +
        labs(y = "Total number of property damage (in billion dolars)", x = "event type") 
crop <- ggplot(data = economy10, aes(reorder(evtype, cropdmg), cropdmg)) +
        geom_bar(stat = "identity", fill = "lightblue") +
        coord_flip() +
        labs(y = "Total number of crop damage (in billoin dolars)", x = "event type") 
grid.arrange(both, prop, crop,
             top = "The top 10 types of weather events that cause economic problems")
```

![](stormData_files/figure-html/economy-1.png)<!-- -->

The flood caused the greatest property damage and overall economic damage in the U.S. 
from 1950 to 2011. The drought, however, caused the greatest crop damage, followed by the flood. 
the hurricane/typhoon caused the second greatest property and overall damage.
