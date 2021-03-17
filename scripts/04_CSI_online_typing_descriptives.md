04 CSI online typing: Descriptives
================
Kirsten Stark
17 März, 2021

## Load packages

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)

rm(list = ls())
```

## Load and preprocess data

``` r
options( "encoding" = "UTF-8" )

# input
input <- "data_long.csv"

# load data
df <- read.csv(here::here("data", input), sep = ",",  na = "")
```

## Description of participant pool

``` r
table(df$gender)/160
```

    ## 
    ##  1  2 
    ## 10 23

``` r
mean(df$age); sd(df$age); range(df$age)
```

    ## [1] 25.87879

    ## [1] 4.584713

    ## [1] 18 35

``` r
table(df$handedness)/160 ; sum(df$handedness == 2)/nrow(df)
```

    ## 
    ##  1  2 
    ##  4 29

    ## [1] 0.8787879

``` r
table(df$fingers_l)/160; table(df$fingers_r)/160
```

    ## 
    ##  1  2  3  4  5  6 
    ##  1  4  8  7 11  2

    ## 
    ## 1 2 3 4 5 6 
    ## 1 9 4 8 9 2

## Average typing speed

## Browser info

## Attention checks

1)  Item vs. non-item

<!-- end list -->

``` r
## Item vs. non-item
# CH01_01 (Taube), CH01_02 (Apfel), CH02_01 (Luftballon) and CH02_02 (Biene) are items and 2 should be selected,
# CH02_03 (Radio), CH02_04 (Sparschwein), CH02_03 (Laptop) and CH02_04 (Wattestäbchen) are non-items and 1 should be selected
## Did participants cheat
# CH03 = 1 - yes, I worked through it till the end,
# CH03 = 2 - no, I stopped or cheated midway
# CH03 = -9 - no answer

attcheck <- data.frame(subject = unique(df$subject)) 

df <- df %>% mutate(itemvsnonitem1 = 
                      case_when(CH01_01==2 & CH01_02==2 & CH01_03==1 & CH01_04==1 ~2,
                                CH01_01==2 || CH01_02==2 ~1, 
                                CH01_01!=2 & CH01_02!=2 ~0)) %>%
  mutate(itemvsnonitem2 = 
                      case_when(CH02_01==2 & CH02_02==2 & CH02_03==1 & CH02_04==1 ~2,
                                CH02_01==2 || CH02_02==2 ~1, 
                                CH02_01!=2 & CH02_02!=2 ~0))
table(df$itemvsnonitem1)/160
```

    ## 
    ##  1  2 
    ##  6 27

``` r
table(df$itemvsnonitem2)/160
```

    ## 
    ##  2 
    ## 33

``` r
# attcheck <- data.frame(subject = unique(df$subject))
# 
# 
# data <- data %>% mutate(attcheck =
#                   ifelse(CH01_01 == 2 & CH01_02 == 2 & CH02_01 == 2 & CH02_02 == 2 &
#                   CH01_03 == 1 & CH01_04 == 1 & CH02_03 == 1 & CH02_04 == 1, 1, 0)) %>%
#                    mutate(cheat = ifelse(CH03 == 1,1,ifelse(CH03 == 2,2,0)))
# data.frame(data$subject, )
# table(data$attcheck)
# table(data$cheat)
# 
# # get prolific IDs of participants who failed the attention check
# #pretest %>% subset(attcheck == 0 & cheat == 2 ) %>%
# #  pull(SD24_01) # SD24_01 is prolific ID
# 
# # subset to participants who passed only
# valid <- pretest  %>% filter(attcheck == 1 & cheat != 2)
# 
# inspect <- data.frame(df$subject, df$word, df$fam_typed)
```

2)  Cheating

## Mother tongue

## Keyboard

## Comments

## Fully anonymize data
