# Reproducible Research: Peer Assessment 1
Remco Keijser - JAN-2016  


## Loading and preprocessing the data

```r
import.data <- read.csv(file="activity.csv", header=TRUE)
import.data$date <- as.Date(as.character(import.data$date, format = "%Y/%m/%d"))
```


```r
# INSTALL PACKAGES
# install.packages("dplyr")             # Install dplyr package
# install.packages("timeDate")          # Install timeDate package
# install.packages("lattice")           # Install lattice package
```

## What is mean total number of steps taken per day?

```r
library("dplyr")
STATS <- summarise(group_by(import.data, date),
                   sum_NA = sum(is.na(steps)),
                   sum_steps = sum(steps, na.rm=TRUE),
                   mean_steps = mean(steps, na.rm=TRUE),
                   median_steps = median(steps, na.rm = TRUE)
)
```


```r
hist(STATS$sum_steps,
     main="Total number of steps taken each day\nWITH MISSING VALUES",
     xlab="Number of steps",
     border="black",
     col="grey",
     xlim=c(0,25000),
     las=1,
     breaks=5)
abline( v = mean(STATS$sum_steps), col=1, lty = 1)
abline( v = median(STATS$sum_steps), col=2, lty = 2)
legend("topright", legend = c("Mean", "Median"), col=c(1,2), lty=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)\


```r
library(knitr)
kable(STATS, digits=2)
```



date          sum_NA   sum_steps   mean_steps   median_steps
-----------  -------  ----------  -----------  -------------
2012-10-01       288           0          NaN             NA
2012-10-02         0         126         0.44              0
2012-10-03         0       11352        39.42              0
2012-10-04         0       12116        42.07              0
2012-10-05         0       13294        46.16              0
2012-10-06         0       15420        53.54              0
2012-10-07         0       11015        38.25              0
2012-10-08       288           0          NaN             NA
2012-10-09         0       12811        44.48              0
2012-10-10         0        9900        34.38              0
2012-10-11         0       10304        35.78              0
2012-10-12         0       17382        60.35              0
2012-10-13         0       12426        43.15              0
2012-10-14         0       15098        52.42              0
2012-10-15         0       10139        35.20              0
2012-10-16         0       15084        52.38              0
2012-10-17         0       13452        46.71              0
2012-10-18         0       10056        34.92              0
2012-10-19         0       11829        41.07              0
2012-10-20         0       10395        36.09              0
2012-10-21         0        8821        30.63              0
2012-10-22         0       13460        46.74              0
2012-10-23         0        8918        30.97              0
2012-10-24         0        8355        29.01              0
2012-10-25         0        2492         8.65              0
2012-10-26         0        6778        23.53              0
2012-10-27         0       10119        35.14              0
2012-10-28         0       11458        39.78              0
2012-10-29         0        5018        17.42              0
2012-10-30         0        9819        34.09              0
2012-10-31         0       15414        53.52              0
2012-11-01       288           0          NaN             NA
2012-11-02         0       10600        36.81              0
2012-11-03         0       10571        36.70              0
2012-11-04       288           0          NaN             NA
2012-11-05         0       10439        36.25              0
2012-11-06         0        8334        28.94              0
2012-11-07         0       12883        44.73              0
2012-11-08         0        3219        11.18              0
2012-11-09       288           0          NaN             NA
2012-11-10       288           0          NaN             NA
2012-11-11         0       12608        43.78              0
2012-11-12         0       10765        37.38              0
2012-11-13         0        7336        25.47              0
2012-11-14       288           0          NaN             NA
2012-11-15         0          41         0.14              0
2012-11-16         0        5441        18.89              0
2012-11-17         0       14339        49.79              0
2012-11-18         0       15110        52.47              0
2012-11-19         0        8841        30.70              0
2012-11-20         0        4472        15.53              0
2012-11-21         0       12787        44.40              0
2012-11-22         0       20427        70.93              0
2012-11-23         0       21194        73.59              0
2012-11-24         0       14478        50.27              0
2012-11-25         0       11834        41.09              0
2012-11-26         0       11162        38.76              0
2012-11-27         0       13646        47.38              0
2012-11-28         0       10183        35.36              0
2012-11-29         0        7047        24.47              0
2012-11-30       288           0          NaN             NA

## What is the average daily activity pattern?


```r
STATS2 <- summarise(group_by(import.data, interval),
                    average_steps = mean(steps, na.rm=TRUE))
```


```r
plot(STATS2$interval, STATS2$average_steps, type='l',
     main="Average daily activity pattern",
     xlab="Interval",
     ylab="Total Steps",
     col="blue",
     xlim=c(0,2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)\


```r
max_steps <- max(STATS2$average_steps)
max_interval <- STATS2[STATS2$average_steps==max_steps,]
max_interval
```

```
## Source: local data frame [1 x 2]
## 
##   interval average_steps
##      (int)         (dbl)
## 1      835      206.1698
```
#### **Interval 835 contains the maximum number of steps.**

## Imputing missing values


```r
library(knitr)
kable(summary(import.data))
```

         steps             date               interval    
---  ---------------  -------------------  ---------------
     Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0 
     1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8 
     Median :  0.00   Median :2012-10-31   Median :1177.5 
     Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5 
     3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2 
     Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0 
     NA's   :2304     NA                   NA             

#### **In total 2304 missing values in variable steps. 8 days are missing step values**


```r
kable(STATS[c(1,8,32,35,40,41,45,61),c(1,2)])
```



date          sum_NA
-----------  -------
2012-10-01       288
2012-10-08       288
2012-11-01       288
2012-11-04       288
2012-11-09       288
2012-11-10       288
2012-11-14       288
2012-11-30       288

#### **Stratergy chosen to impute missing values: use the mean for that 5-minute interval.**


```r
import.data.imputed <- merge(import.data, STATS2, by.x = "interval", by.y = "interval")
import.data.imputed <- import.data.imputed[order(as.Date(import.data.imputed$date, format="%d/%m/%Y")),]
import.data.imputed$steps.imputed <- NA

for(i in 1:length(import.data.imputed$steps)){
        
        if(is.na(import.data.imputed[i,2])) {
                
                import.data.imputed[i,5] <- import.data.imputed[i,4]
        }
        
        if(!is.na(import.data.imputed[i,2])) {
                
                import.data.imputed[i,5] <- import.data.imputed[i,2]
        }
}

import.data.imputed <- import.data.imputed[,c(2,5,3,1)]
```


```r
library(knitr)
kable(head(import.data.imputed, n=50))
```

        steps   steps.imputed  date          interval
-----  ------  --------------  -----------  ---------
1          NA       1.7169811  2012-10-01           0
63         NA       0.3396226  2012-10-01           5
128        NA       0.1320755  2012-10-01          10
205        NA       0.1509434  2012-10-01          15
264        NA       0.0754717  2012-10-01          20
327        NA       2.0943396  2012-10-01          25
376        NA       0.5283019  2012-10-01          30
481        NA       0.8679245  2012-10-01          35
495        NA       0.0000000  2012-10-01          40
552        NA       1.4716981  2012-10-01          45
620        NA       0.3018868  2012-10-01          50
716        NA       0.1320755  2012-10-01          55
770        NA       0.3207547  2012-10-01         100
840        NA       0.6792453  2012-10-01         105
880        NA       0.1509434  2012-10-01         110
924        NA       0.3396226  2012-10-01         115
1018       NA       0.0000000  2012-10-01         120
1097       NA       1.1132075  2012-10-01         125
1141       NA       1.8301887  2012-10-01         130
1183       NA       0.1698113  2012-10-01         135
1230       NA       0.1698113  2012-10-01         140
1320       NA       0.3773585  2012-10-01         145
1388       NA       0.2641509  2012-10-01         150
1443       NA       0.0000000  2012-10-01         155
1507       NA       0.0000000  2012-10-01         200
1547       NA       0.0000000  2012-10-01         205
1630       NA       1.1320755  2012-10-01         210
1690       NA       0.0000000  2012-10-01         215
1731       NA       0.0000000  2012-10-01         220
1774       NA       0.1320755  2012-10-01         225
1882       NA       0.0000000  2012-10-01         230
1946       NA       0.2264151  2012-10-01         235
2000       NA       0.0000000  2012-10-01         240
2048       NA       0.0000000  2012-10-01         245
2130       NA       1.5471698  2012-10-01         250
2179       NA       0.9433962  2012-10-01         255
2224       NA       0.0000000  2012-10-01         300
2280       NA       0.0000000  2012-10-01         305
2349       NA       0.0000000  2012-10-01         310
2405       NA       0.0000000  2012-10-01         315
2444       NA       0.2075472  2012-10-01         320
2550       NA       0.6226415  2012-10-01         325
2581       NA       1.6226415  2012-10-01         330
2675       NA       0.5849057  2012-10-01         335
2720       NA       0.4905660  2012-10-01         340
2788       NA       0.0754717  2012-10-01         345
2856       NA       0.0000000  2012-10-01         350
2898       NA       0.0000000  2012-10-01         355
2942       NA       1.1886792  2012-10-01         400
3018       NA       0.9433962  2012-10-01         405


```r
STATS3 <- summarise(group_by(import.data.imputed, date),
                    sum_NA = sum(is.na(steps)),
                    sum_NA.imputed = sum(is.na(steps.imputed)),
                    sum_steps = sum(steps, na.rm=TRUE),
                    mean_steps = mean(steps, na.rm=TRUE),
                    median_steps = median(steps, na.rm = TRUE),
                    
                    sum_steps.imputed = sum(steps.imputed, na.rm=TRUE),
                    mean_steps.imputed = mean(steps.imputed, na.rm=TRUE),
                    median_steps.imputed = median(steps.imputed, na.rm = TRUE))
```


```r
hist(STATS3$sum_steps.imputed,
     main="Total number of steps taken each day\nMISSING VALUES IMPUTED",
     xlab="Number of steps",
     border="black",
     col="grey",
     xlim=c(0,25000),
     las=1,
     breaks=5)
abline( v = mean(STATS3$sum_steps.imputed), col=1, lty = 1)
abline( v = median(STATS3$sum_steps.imputed), col=2, lty = 2)
legend("topright", legend = c("Mean", "Median"), col=c(1,2), lty=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)\


```r
kable(STATS3)
```



date          sum_NA   sum_NA.imputed   sum_steps   mean_steps   median_steps   sum_steps.imputed   mean_steps.imputed   median_steps.imputed
-----------  -------  ---------------  ----------  -----------  -------------  ------------------  -------------------  ---------------------
2012-10-01       288                0           0          NaN             NA            10766.19           37.3825996               34.11321
2012-10-02         0                0         126    0.4375000              0              126.00            0.4375000                0.00000
2012-10-03         0                0       11352   39.4166667              0            11352.00           39.4166667                0.00000
2012-10-04         0                0       12116   42.0694444              0            12116.00           42.0694444                0.00000
2012-10-05         0                0       13294   46.1597222              0            13294.00           46.1597222                0.00000
2012-10-06         0                0       15420   53.5416667              0            15420.00           53.5416667                0.00000
2012-10-07         0                0       11015   38.2465278              0            11015.00           38.2465278                0.00000
2012-10-08       288                0           0          NaN             NA            10766.19           37.3825996               34.11321
2012-10-09         0                0       12811   44.4826389              0            12811.00           44.4826389                0.00000
2012-10-10         0                0        9900   34.3750000              0             9900.00           34.3750000                0.00000
2012-10-11         0                0       10304   35.7777778              0            10304.00           35.7777778                0.00000
2012-10-12         0                0       17382   60.3541667              0            17382.00           60.3541667                0.00000
2012-10-13         0                0       12426   43.1458333              0            12426.00           43.1458333                0.00000
2012-10-14         0                0       15098   52.4236111              0            15098.00           52.4236111                0.00000
2012-10-15         0                0       10139   35.2048611              0            10139.00           35.2048611                0.00000
2012-10-16         0                0       15084   52.3750000              0            15084.00           52.3750000                0.00000
2012-10-17         0                0       13452   46.7083333              0            13452.00           46.7083333                0.00000
2012-10-18         0                0       10056   34.9166667              0            10056.00           34.9166667                0.00000
2012-10-19         0                0       11829   41.0729167              0            11829.00           41.0729167                0.00000
2012-10-20         0                0       10395   36.0937500              0            10395.00           36.0937500                0.00000
2012-10-21         0                0        8821   30.6284722              0             8821.00           30.6284722                0.00000
2012-10-22         0                0       13460   46.7361111              0            13460.00           46.7361111                0.00000
2012-10-23         0                0        8918   30.9652778              0             8918.00           30.9652778                0.00000
2012-10-24         0                0        8355   29.0104167              0             8355.00           29.0104167                0.00000
2012-10-25         0                0        2492    8.6527778              0             2492.00            8.6527778                0.00000
2012-10-26         0                0        6778   23.5347222              0             6778.00           23.5347222                0.00000
2012-10-27         0                0       10119   35.1354167              0            10119.00           35.1354167                0.00000
2012-10-28         0                0       11458   39.7847222              0            11458.00           39.7847222                0.00000
2012-10-29         0                0        5018   17.4236111              0             5018.00           17.4236111                0.00000
2012-10-30         0                0        9819   34.0937500              0             9819.00           34.0937500                0.00000
2012-10-31         0                0       15414   53.5208333              0            15414.00           53.5208333                0.00000
2012-11-01       288                0           0          NaN             NA            10766.19           37.3825996               34.11321
2012-11-02         0                0       10600   36.8055556              0            10600.00           36.8055556                0.00000
2012-11-03         0                0       10571   36.7048611              0            10571.00           36.7048611                0.00000
2012-11-04       288                0           0          NaN             NA            10766.19           37.3825996               34.11321
2012-11-05         0                0       10439   36.2465278              0            10439.00           36.2465278                0.00000
2012-11-06         0                0        8334   28.9375000              0             8334.00           28.9375000                0.00000
2012-11-07         0                0       12883   44.7326389              0            12883.00           44.7326389                0.00000
2012-11-08         0                0        3219   11.1770833              0             3219.00           11.1770833                0.00000
2012-11-09       288                0           0          NaN             NA            10766.19           37.3825996               34.11321
2012-11-10       288                0           0          NaN             NA            10766.19           37.3825996               34.11321
2012-11-11         0                0       12608   43.7777778              0            12608.00           43.7777778                0.00000
2012-11-12         0                0       10765   37.3784722              0            10765.00           37.3784722                0.00000
2012-11-13         0                0        7336   25.4722222              0             7336.00           25.4722222                0.00000
2012-11-14       288                0           0          NaN             NA            10766.19           37.3825996               34.11321
2012-11-15         0                0          41    0.1423611              0               41.00            0.1423611                0.00000
2012-11-16         0                0        5441   18.8923611              0             5441.00           18.8923611                0.00000
2012-11-17         0                0       14339   49.7881944              0            14339.00           49.7881944                0.00000
2012-11-18         0                0       15110   52.4652778              0            15110.00           52.4652778                0.00000
2012-11-19         0                0        8841   30.6979167              0             8841.00           30.6979167                0.00000
2012-11-20         0                0        4472   15.5277778              0             4472.00           15.5277778                0.00000
2012-11-21         0                0       12787   44.3993056              0            12787.00           44.3993056                0.00000
2012-11-22         0                0       20427   70.9270833              0            20427.00           70.9270833                0.00000
2012-11-23         0                0       21194   73.5902778              0            21194.00           73.5902778                0.00000
2012-11-24         0                0       14478   50.2708333              0            14478.00           50.2708333                0.00000
2012-11-25         0                0       11834   41.0902778              0            11834.00           41.0902778                0.00000
2012-11-26         0                0       11162   38.7569444              0            11162.00           38.7569444                0.00000
2012-11-27         0                0       13646   47.3819444              0            13646.00           47.3819444                0.00000
2012-11-28         0                0       10183   35.3576389              0            10183.00           35.3576389                0.00000
2012-11-29         0                0        7047   24.4687500              0             7047.00           24.4687500                0.00000
2012-11-30       288                0           0          NaN             NA            10766.19           37.3825996               34.11321

#### **The mean and median differ of the imputed day.**
#### **As shown in the two histograms the mean and the median also changes slighly of the sum of the steps across al days**

## Are there differences in activity patterns between weekdays and weekends?


```r
library("timeDate")
import.data.imputed$weekday <- isWeekday(import.data.imputed$date, wday = 1:5)
import.data.imputed$weekend <- isWeekend(import.data.imputed$date, wday = 1:5)
import.data.imputed$day <- import.data.imputed$weekday
import.data.imputed$day[import.data.imputed$day == TRUE] <- "WEEKDAY"
import.data.imputed$day[import.data.imputed$day == FALSE] <- "WEEKEND"
kable(head(import.data.imputed, n=10))
```

       steps   steps.imputed  date          interval  weekday   weekend   day     
----  ------  --------------  -----------  ---------  --------  --------  --------
1         NA       1.7169811  2012-10-01           0  TRUE      FALSE     WEEKDAY 
63        NA       0.3396226  2012-10-01           5  TRUE      FALSE     WEEKDAY 
128       NA       0.1320755  2012-10-01          10  TRUE      FALSE     WEEKDAY 
205       NA       0.1509434  2012-10-01          15  TRUE      FALSE     WEEKDAY 
264       NA       0.0754717  2012-10-01          20  TRUE      FALSE     WEEKDAY 
327       NA       2.0943396  2012-10-01          25  TRUE      FALSE     WEEKDAY 
376       NA       0.5283019  2012-10-01          30  TRUE      FALSE     WEEKDAY 
481       NA       0.8679245  2012-10-01          35  TRUE      FALSE     WEEKDAY 
495       NA       0.0000000  2012-10-01          40  TRUE      FALSE     WEEKDAY 
552       NA       1.4716981  2012-10-01          45  TRUE      FALSE     WEEKDAY 


```r
library("lattice")
xyplot(steps.imputed  ~ interval| day, data=import.data.imputed,
       type = "l",
       layout = c(1, 2),
       par.strip.text=list(cex=0.8),
       xlab = "Interval",
       ylab=list(
               label="Number of steps",
               cex=1),
       main=list(
               label="Interval vs number of steps",
               cex=1),
       scales=list(cex=0.8)
)
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)\
