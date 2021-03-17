05 CSI online typing: Plotting and analysis
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
library(lme4)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

``` r
library(lmerTest)
```

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

``` r
library(Rmisc)
```

    ## Loading required package: lattice

    ## Loading required package: plyr

    ## ------------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## ------------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

``` r
library(Cairo)
#library(strengejacke)
library(ggplot2)

options(scipen=999)

rm(list = ls())
```

## Load and preprocess data

``` r
options( "encoding" = "UTF-8" )

# input 
input = "data_long_final.csv"

# load data
df <- read.csv(here::here("data", input))
```

Check amount of participants and trials

``` r
# no. of participants: 
length(unique(df$subject))
```

    ## [1] 30

``` r
# no. of trials is 160 per participant? 
nrow(df) == 160 * length(unique(df$subject))
```

    ## [1] TRUE

Factorize columns

``` r
# factorize columns
is.numeric(df$timing.01)
```

    ## [1] TRUE

``` r
df$PosOr <- as.factor(df$PosOr)
df$subject <- as.factor(df$subject)

# exclude rows with filler trials
#df <- df %>% filter(category != "Filler")
#nrow(df)/length(unique(df$subject)) # 120 Trials per participant
```

# Plotting

Make plots suitable for APA format, font sizes can be adjusted

``` r
apatheme <- theme_bw()+
  theme(plot.title=element_text(family="Arial",size=22,hjust = .5),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.border=element_blank(),axis.line=element_line(),
        text=element_text(family="Arial",size=16))
```

## Correct trials only:

We only consider trials with (almost) correct answers

``` r
sum(!is.na(df$correct))
```

    ## [1] 4297

``` r
sum(!is.na(df$correct[df$category != "Filler"]))
```

    ## [1] 3192

``` r
# nrow(df) - (sum(df$answercode == "correct") + sum(df$answercode == "almostcorrect"))
# df$timing.01[is.na(df$correct)] <- NA
# sum(is.na(df$timing.01))
```

### Descriptives

``` r
(means_final<- df %>% filter(!is.na(correct)) %>% 
   filter(category != "Filler") %>% 
   Rmisc::summarySEwithin(.,"timing.01",idvar = "subject",
                          withinvars = "PosOr", na.rm = T))
```

    ##   PosOr   N timing.01       sd       se       ci
    ## 1     1 648  1151.499 442.9892 17.40226 34.17172
    ## 2     2 637  1224.975 548.8173 21.74493 42.70055
    ## 3     3 642  1247.021 523.0775 20.64422 40.53847
    ## 4     4 637  1285.339 593.7701 23.52603 46.19808
    ## 5     5 628  1319.104 561.7945 22.41804 44.02354

### RTs by ordinal position

``` r
(plot_rt <- df %>% filter(!is.na(correct)) %>% 
   filter(category != "Filler") %>% 
    ggplot(., aes(x=PosOr, y=timing.01)) +
    stat_summary(fun=mean,  geom="point", size = 2)+
    stat_summary(fun=mean,  geom="line", size = 1) +
    apatheme+
    labs(x="Ordinal Position ",y ="RT (ms)")+
  annotate(geom="text", x=2, y=1200, label="n = 30", 
           color="black", size = 8))
```

    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?

![](05_CSI_online_typing_analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

### … with fillers for control

``` r
(plot_rt_fillers <- df %>% filter(!is.na(correct)) %>% 
    mutate(kind = case_when(category == "Filler" ~"Filler",
                          category != "Filler" ~"Experimental")) %>%
    ggplot(., aes(x=PosOr, y=timing.01, group=kind, color=kind)) +
    stat_summary(fun=mean,  geom="point", size = 2)+
    stat_summary(fun=mean,  geom="line", size = 1) +
    apatheme+
    labs(x="Ordinal Position ",y ="RT (ms)", color = "Trial type")+
  annotate(geom="text", x=2, y=1300, label="n = 30", 
           color="black", size = 8))
```

![](05_CSI_online_typing_analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
# ggsave(plot = plot, file = "figures/RT_across_conditions.pdf", device = cairo_pdf, dpi = 300)
# embedFonts(file = "figures/RT_across_conditions_CSI_online_typing.pdf")
```

### Plot by subcategory

``` r
(plot_rt_by_cat <- df %>% filter(!is.na(correct)) %>% 
   filter(category != "Filler") %>% 
    ggplot(., aes(x=PosOr, y=timing.01)) +
    stat_summary(fun=mean,  geom="point", size = 2)+
    stat_summary(fun=mean,  geom="line", size = 1) +
    facet_wrap(~category) +
    apatheme+
    labs(x="Ordinal Position ",y ="RT (ms)"))
```

    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?

![](05_CSI_online_typing_analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
#ggsave(plot = plot30_facet, file = "Plots/CSI_subcats.pdf", device = cairo_pdf,dpi = 300)
#embedFonts(file = "Plots/CSI_online30_subcats.pdf")
```

### Plot by subject

``` r
(plot_rt_by_subject <- df %>% filter(!is.na(correct)) %>% 
   filter(category != "Filler") %>% 
    ggplot(., aes(x=PosOr, y=timing.01)) +
    stat_summary(fun=mean,  geom="point", size = 2) +  
    stat_summary(fun=mean,  geom="line", size = 1) +  
    facet_wrap(~subject) +
    apatheme+
    labs(x="Ordinal Position ",y ="RT (ms)"))
```

    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?

![](05_CSI_online_typing_analysis_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
#ggsave(plot = plot30_facet, file = "Plots/CSI_subcats.pdf", device = cairo_pdf,dpi = 300)
#embedFonts(file = "Plots/CSI_online30_subcats.pdf")
```

### Control: Plot RTs accross the experiment

All trials including uncorrect trials

``` r
(plot_RTs_all <- ggplot(data=df, aes(x=trial, y=timing.01)) +
  stat_summary(fun=mean,  geom="point", size = 2)+
  stat_summary(fun=mean,  geom="line", size = 1) +
  apatheme+
  labs(x="Trial ",y ="RT ms")+
  annotate(geom="text", x=20, y=1570, label="n = 30", 
           color="black", size = 8))
```

    ## Warning: Removed 93 rows containing non-finite values (stat_summary).
    
    ## Warning: Removed 93 rows containing non-finite values (stat_summary).

![](05_CSI_online_typing_analysis_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Correct trials only, including fillers

``` r
(plot_RTs_correct <- df %>% filter(!is.na(correct)) %>% 
   #filter(category != "Filler") %>% 
    ggplot(., aes(x=trial, y=timing.01)) +
    stat_summary(fun=mean,  geom="point", size = 2)+
    stat_summary(fun=mean,  geom="line", size = 1) +
    apatheme+
    labs(x="Trial ",y ="RT ms")+
    annotate(geom="text", x=20, y=1570, label="n = 30", 
             color="black", size = 8))
```

![](05_CSI_online_typing_analysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

# Regression analyses

``` r
hist(df$timing.01)
```

![](05_CSI_online_typing_analysis_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Full GLMM with polynomial contrasts

``` r
contrasts(df$PosOr) <- contr.poly(5)
# m1 <- glmer(timing.01 ~ PosOr + (PosOr|subject) +(PosOr|category) ,
#              data = df[df$category != "Filler"  & df$correct == 1,], family =Gamma(link ="identity"), control=glmerControl(optimizer = "bobyqa"))
#model does not converge
m1 <- afex::lmer_alt(timing.01 ~ PosOr + (PosOr||subject) +(1|category), 
                  df[df$category != "Filler"  & df$correct == 1,],
                  family =Gamma(link ="identity"),
                  control=glmerControl(optimizer = "bobyqa", 
                                       optCtrl = list(maxfun = 2*10^5)))
```

    ## Registered S3 methods overwritten by 'car':
    ##   method                          from
    ##   influence.merMod                lme4
    ##   cooks.distance.influence.merMod lme4
    ##   dfbeta.influence.merMod         lme4
    ##   dfbetas.influence.merMod        lme4

    ## Warning: Due to missing values, reduced number of observations to 3192

``` r
summary(m1)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: Gamma  ( identity )
    ## Formula: timing.01 ~ PosOr + (1 + re1.PosOr.L + re1.PosOr.Q + re1.PosOr.C +  
    ##     re1.PosOr.4 || subject) + (1 | category)
    ##    Data: data
    ## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2 *  
    ##     10^5))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  46716.7  46789.5 -23346.3  46692.7     3180 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.6065 -0.5806 -0.2509  0.2672  8.3161 
    ## 
    ## Random effects:
    ##  Groups    Name        Variance  Std.Dev.
    ##  subject   (Intercept) 8036.2841 89.6453 
    ##  subject.1 re1.PosOr.L 6580.9424 81.1230 
    ##  subject.2 re1.PosOr.Q 5734.0848 75.7237 
    ##  subject.3 re1.PosOr.C 7575.4007 87.0368 
    ##  subject.4 re1.PosOr.4 7579.0059 87.0575 
    ##  category  (Intercept) 9348.7900 96.6891 
    ##  Residual                 0.1243  0.3526 
    ## Number of obs: 3192, groups:  subject, 30; category, 24
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value            Pr(>|z|)    
    ## (Intercept) 1301.534      9.852 132.106 <0.0000000000000002 ***
    ## PosOr.L      119.714      8.506  14.074 <0.0000000000000002 ***
    ## PosOr.Q       -7.727     12.996  -0.595               0.552    
    ## PosOr.C       12.245      7.786   1.573               0.116    
    ## PosOr^4      -11.232      7.728  -1.453               0.146    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##         (Intr) PsOr.L PsOr.Q PsOr.C
    ## PosOr.L -0.138                     
    ## PosOr.Q  0.226 -0.284              
    ## PosOr.C  0.115  0.048  0.054       
    ## PosOr^4 -0.031 -0.153 -0.030 -0.090

Linear trend only

``` r
fixef_terms <- model.matrix( ~ PosOr,df) 
df$PosOr.L <- fixef_terms[,2]
m2 <- glmer(timing.01 ~ PosOr.L + (PosOr.L|subject) +(PosOr.L|category) ,
             data = df[df$category != "Filler"  & df$correct == 1,], 
            family =Gamma(link ="identity"), 
            control=glmerControl(optimizer = "bobyqa"))
summary(m2)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: Gamma  ( identity )
    ## Formula: timing.01 ~ PosOr.L + (PosOr.L | subject) + (PosOr.L | category)
    ##    Data: df[df$category != "Filler" & df$correct == 1, ]
    ## Control: glmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  46748.4  46803.1 -23365.2  46730.4     3183 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.5334 -0.5804 -0.2603  0.2555  8.4459 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev. Corr 
    ##  subject  (Intercept) 8202.6703 90.569        
    ##           PosOr.L     6551.0763 80.939   -0.02
    ##  category (Intercept) 9771.1760 98.849        
    ##           PosOr.L     4710.2675 68.631   0.26 
    ##  Residual                0.1282  0.358        
    ## Number of obs: 3192, groups:  subject, 30; category, 24
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value            Pr(>|z|)    
    ## (Intercept) 1296.418      8.804   147.3 <0.0000000000000002 ***
    ## PosOr.L      134.797      8.641    15.6 <0.0000000000000002 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##         (Intr)
    ## PosOr.L 0.252

Ordinal position as a continuous predictor variable

``` r
df$PosOr.cont <- scale(as.numeric(as.character(df$PosOr)), 
                       center = T, scale = F)
m3 <- glmer(timing.01 ~ PosOr.cont + 
               (PosOr.cont|subject) +(PosOr.cont|category),
             data = df[df$category != "Filler"  & df$correct == 1,], 
            family =Gamma(link ="identity"), 
            control=glmerControl(optimizer = "bobyqa"))
summary(m3)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: Gamma  ( identity )
    ## Formula: timing.01 ~ PosOr.cont + (PosOr.cont | subject) + (PosOr.cont |  
    ##     category)
    ##    Data: df[df$category != "Filler" & df$correct == 1, ]
    ## Control: glmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  46748.4  46803.1 -23365.2  46730.4     3183 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.5334 -0.5804 -0.2603  0.2555  8.4459 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev. Corr 
    ##  subject  (Intercept) 8202.6667 90.569        
    ##           PosOr.cont   655.1035 25.595   -0.02
    ##  category (Intercept) 9771.1170 98.849        
    ##           PosOr.cont   471.0233 21.703   0.26 
    ##  Residual                0.1282  0.358        
    ## Number of obs: 3192, groups:  subject, 30; category, 24
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value             Pr(>|z|)    
    ## (Intercept) 1296.418     10.270  126.23 < 0.0000000000000002 ***
    ## PosOr.cont    42.627      6.568    6.49      0.0000000000856 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##            (Intr)
    ## PosOr.cont 0.096

# Plotting before data preprocessing

# ALL TRIALS

### Plot RTs by OrPos

``` r
is.numeric(df$timing.01)
```

    ## [1] TRUE

``` r
df$PosOr <- as.factor(df$PosOr)
df$subject <- as.factor(df$subject)
# descriptives 
(means <- Rmisc::summarySEwithin(df[df$category != "Filler" ,],"timing.01",idvar = "subject",withinvars = "PosOr", na.rm = T))
```

    ##   PosOr   N timing.01       sd       se       ci
    ## 1     1 704  1186.420 526.2459 19.83364 38.94026
    ## 2     2 706  1274.262 625.8178 23.55297 46.24235
    ## 3     3 707  1299.173 616.1524 23.17279 45.49584
    ## 4     4 702  1335.876 678.5977 25.61202 50.28546
    ## 5     5 707  1392.315 693.3562 26.07634 51.19646

``` r
plotting <- df %>%
  mutate(kind = case_when(category == "Filler" ~"Filler",
                          category != "Filler" ~"Experimental"))
(plot <- ggplot(data=df[df$category != "Filler" ,], aes(x=PosOr, y=timing.01)) +
  stat_summary(fun=mean,  geom="point", size = 2)+
  stat_summary(fun=mean,  geom="line", size = 1) +
  apatheme+
  labs(x="Ordinal Position ",y ="RT ms")+
  annotate(geom="text", x=2, y=1200, label="n = 4", color="black", size = 8))
```

    ## Warning: Removed 74 rows containing non-finite values (stat_summary).
    
    ## Warning: Removed 74 rows containing non-finite values (stat_summary).

    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?

![](05_CSI_online_typing_analysis_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
(plot <- ggplot(data=plotting, aes(x=PosOr, y=timing.01, group = kind, color = kind)) +
  stat_summary(fun=mean,  geom="point", size = 2)+
  stat_summary(fun=mean,  geom="line", size = 1) +
  apatheme+
  labs(x="Ordinal Position ",y ="RT ms", color = "Trial type")+
  annotate(geom="text", x=2, y=1380, label="n = 30", color="black", size = 8))
```

    ## Warning: Removed 93 rows containing non-finite values (stat_summary).

    ## Warning: Removed 93 rows containing non-finite values (stat_summary).

![](05_CSI_online_typing_analysis_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->

``` r
# ggsave(plot = plot, file = "figures/RT_across_conditions.pdf", device = cairo_pdf, dpi = 300)
# embedFonts(file = "figures/RT_across_conditions_CSI_online_typing.pdf")
```

Plot by subcategory

``` r
(plot_by_cat <- ggplot(data=df[df$category != "Filler" ,], aes(x=PosOr, y=timing.01)) +
  stat_summary(fun=mean,  geom="point", size = 2)+
  stat_summary(fun=mean,  geom="line", size = 1) +
  facet_wrap(~category) +
  apatheme+
  labs(x="Ordinal Position ",y ="RT ms"))
```

    ## Warning: Removed 74 rows containing non-finite values (stat_summary).
    
    ## Warning: Removed 74 rows containing non-finite values (stat_summary).

    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?

![](05_CSI_online_typing_analysis_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
#ggsave(plot = plot30_facet, file = "Plots/CSI_subcats.pdf", device = cairo_pdf,dpi = 300)
#embedFonts(file = "Plots/CSI_online30_subcats.pdf")
```

Plot by subject:

``` r
(plot_by_cat <- ggplot(data=df[df$category != "Filler" ,], aes(x=PosOr, y=timing.01)) +
  stat_summary(fun=mean,  geom="point", size = 2)+
  stat_summary(fun=mean,  geom="line", size = 1) +
  facet_wrap(~subject) +
  apatheme+
  labs(x="Ordinal Position ",y ="RT ms"))
```

    ## Warning: Removed 74 rows containing non-finite values (stat_summary).
    
    ## Warning: Removed 74 rows containing non-finite values (stat_summary).

    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?
    ## geom_path: Each group consists of only one observation. Do you need to adjust
    ## the group aesthetic?

![](05_CSI_online_typing_analysis_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
#ggsave(plot = plot30_facet, file = "Plots/CSI_subcats.pdf", device = cairo_pdf,dpi = 300)
#embedFonts(file = "Plots/CSI_online30_subcats.pdf")
```
