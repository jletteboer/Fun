# Tasting Chocolate
John Letteboer  
11/29/2015  


Q1: Is there a difference taste score between female and male.

- H~0~: man = female
- H~1~: man $\ne$ female
- Confidence interval is .95
- $\alpha$ = .05

Loading required packages.

```r
library(ggplot2)
library(RColorBrewer)
library(psych)
```

Load the dataset.

```r
df <- read.csv("Tasting_chocolate.csv", header=TRUE, sep = ";")
```

After loading the dataset let's have a look at the data.

```r
dim(df)
```

```
## [1] 150   4
```

```r
head(df, 2)
```

```
##                  type gender gender.1 taste
## 1 Own brand Carrefour female        1    -2
## 2 Own brand Carrefour female        1    -3
```

```r
str(df)
```

```
## 'data.frame':	150 obs. of  4 variables:
##  $ type    : Factor w/ 3 levels "Chocolate bar Van Houten",..: 3 3 3 3 3 3 3 3 3 3 ...
##  $ gender  : Factor w/ 2 levels "female","male": 1 1 1 1 1 1 1 1 1 1 ...
##  $ gender.1: int  1 1 1 1 1 1 1 1 1 1 ...
##  $ taste   : int  -2 -3 2 -1 2 2 -1 -1 1 1 ...
```

```r
summary(df)
```

```
##                         type       gender      gender.1       taste      
##  Chocolate bar Van Houten :50   female:75   Min.   :0.0   Min.   :-3.00  
##  Chocolate milk Van Houten:50   male  :75   1st Qu.:0.0   1st Qu.: 0.00  
##  Own brand Carrefour      :50               Median :0.5   Median : 1.00  
##                                             Mean   :0.5   Mean   : 0.82  
##                                             3rd Qu.:1.0   3rd Qu.: 2.00  
##                                             Max.   :1.0   Max.   : 3.00
```

We can see the dataset has 150 objects and 4 variables, `type`, `gender`, `gender.1` and `taste`.

## t-Test
### First Step
#### 1. Are there missing values?

```r
any(is.na(df))
```

```
## [1] FALSE
```

As we can see there are no missiong values.

#### 2. Are there outliers?
Let's make a boxplot to see if there are any outliers.


```r
boxplot(df$taste ~ df$gender, col="gray", outcol="red")
```

![](Tasting_chocolate_files/figure-html/unnamed-chunk-5-1.png) 

There are no outliers.

#### 3. Are the data normal distrubuted
Let make some histograms

```r
cols <- brewer.pal(11,"Set1")
```

```
## Warning in brewer.pal(11, "Set1"): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```r
ggplot(df, aes(taste, fill=gender)) + 
    facet_grid( type ~ gender) + 
    geom_histogram(binwidth=.5, alpha=.5, position="identity") + 
    theme_bw() + 
    scale_fill_manual(values = cols) + 
    theme(legend.position="none")
```

![](Tasting_chocolate_files/figure-html/unnamed-chunk-6-1.png) 

From the histograms we can see that the data is looks normal distrubuted.

Now calculate this with the Kurtosis and Skewness.

- The skewness is a measure of symmetry. As a rule, negative skewness indicates that the mean of the data values is less than the median, and the data distribution is left-skewed. Positive skewness would indicates that the mean of the data values is larger than the median, and the data distribution is right-skewed.
- The kurtosis is a measure of the peakedness of the data distribution. Negative kurtosis would indicates a flat data distribution, which is said to be platykurtic. Positive kurtosis would indicates a peaked distribution, which is said to be leptokurtic. Incidentally, the normal distribution has zero kurtosis, and is said to be mesokurtic.


```r
kurtosis <- aggregate(taste ~ gender + type, df, kurtosi)
skewness <- aggregate(taste ~ gender + type, df, skew)
kurtosis
```

```
##   gender                      type      taste
## 1 female  Chocolate bar Van Houten  0.0140670
## 2   male  Chocolate bar Van Houten -0.6342203
## 3 female Chocolate milk Van Houten -1.0208554
## 4   male Chocolate milk Van Houten  0.1333481
## 5 female       Own brand Carrefour  0.3555704
## 6   male       Own brand Carrefour -0.4656000
```

```r
skewness
```

```
##   gender                      type       taste
## 1 female  Chocolate bar Van Houten -0.22261218
## 2   male  Chocolate bar Van Houten  0.09153785
## 3 female Chocolate milk Van Houten -0.07045565
## 4   male Chocolate milk Van Houten -0.44511111
## 5 female       Own brand Carrefour -0.72288889
## 6   male       Own brand Carrefour  0.15676734
```

### Second step
Determine equality of variances by doing a F-test.

```r
var.test(df$taste[df$gender=="female"], df$taste[df$gender=="male"])
```

```
## 
## 	F test to compare two variances
## 
## data:  df$taste[df$gender == "female"] and df$taste[df$gender == "male"]
## F = 1.0174, num df = 74, denom df = 74, p-value = 0.9409
## alternative hypothesis: true ratio of variances is not equal to 1
## 95 percent confidence interval:
##  0.6428011 1.6104328
## sample estimates:
## ratio of variances 
##           1.017442
```

The p-value > $\alpha$ (.05), we accept H~0~

### Third step
Let's do a independent 2-group t-test of `taste` over `gender`

```r
t.test(taste ~ gender, df)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  taste by gender
## t = -0.19996, df = 147.99, p-value = 0.8418
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.4352959  0.3552959
## sample estimates:
## mean in group female   mean in group male 
##                 0.80                 0.84
```

## ANOVA
First resize lenght of `type`, because it's better for plotting.

```r
df$type_short[df$type == "Own brand Carrefour"] <- "own"
df$type_short[df$type == "Chocolate bar Van Houten"] <- "bar"
df$type_short[df$type == "Chocolate milk Van Houten"] <- "milk"
df$type_short <- as.factor(df$type_short)
```

Create a boxplot to see if there are outliers per type

```r
boxplot(df$taste ~ df$type_short, col="gray", outcol="red")
```

![](Tasting_chocolate_files/figure-html/unnamed-chunk-11-1.png) 

Create histograms to see if the data is normal distrubuted

```r
ggplot(df, aes(taste, fill=type)) + 
    facet_grid(type~.) + 
    geom_histogram(binwidth=.5, alpha=.5, position="identity") + 
    theme_bw() + 
    scale_fill_manual(values = cols) + 
    theme(legend.position="none")
```

![](Tasting_chocolate_files/figure-html/unnamed-chunk-12-1.png) 

There are some outliers but not that bad.

Create ANOVA

```r
aov1 <- aov(taste ~ type_short, df)
summary(aov1)
```

```
##              Df Sum Sq Mean Sq F value   Pr(>F)    
## type_short    2  27.36  13.680   10.32 6.37e-05 ***
## Residuals   147 194.78   1.325                     
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
TukeyHSD(aov1)
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = taste ~ type_short, data = df)
## 
## $type_short
##           diff        lwr        upr     p adj
## milk-bar  0.12 -0.4250909  0.6650909 0.8610935
## own-bar  -0.84 -1.3850909 -0.2949091 0.0010593
## own-milk -0.96 -1.5050909 -0.4149091 0.0001527
```

```r
plot(TukeyHSD(aov1))
```

![](Tasting_chocolate_files/figure-html/unnamed-chunk-13-1.png) 

```r
aov2 <- aov(taste ~ gender, df)
summary(aov2)
```

```
##              Df Sum Sq Mean Sq F value Pr(>F)
## gender        1   0.06    0.06    0.04  0.842
## Residuals   148 222.08    1.50
```

```r
TukeyHSD(aov2)
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = taste ~ gender, data = df)
## 
## $gender
##             diff        lwr       upr     p adj
## male-female 0.04 -0.3552957 0.4352957 0.8417833
```

```r
plot(TukeyHSD(aov2))
```

![](Tasting_chocolate_files/figure-html/unnamed-chunk-13-2.png) 

```r
par(mar = c(5,12,4,2), cex = 0.7)
aov3 <- aov(taste ~ gender * type_short, df)
summary(aov3)
```

```
##                    Df Sum Sq Mean Sq F value   Pr(>F)    
## gender              1   0.06   0.060   0.044    0.833    
## type_short          2  27.36  13.680  10.125 7.69e-05 ***
## gender:type_short   2   0.16   0.080   0.059    0.943    
## Residuals         144 194.56   1.351                     
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
TukeyHSD(aov3)
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = taste ~ gender * type_short, data = df)
## 
## $gender
##             diff        lwr       upr     p adj
## male-female 0.04 -0.3351831 0.4151831 0.8333945
## 
## $type_short
##           diff        lwr        upr     p adj
## milk-bar  0.12 -0.4305458  0.6705458 0.8635813
## own-bar  -0.84 -1.3905458 -0.2894542 0.0012067
## own-milk -0.96 -1.5105458 -0.4094542 0.0001805
## 
## $`gender:type_short`
##                         diff        lwr         upr     p adj
## male:bar-female:bar     0.04 -0.9096391  0.98963912 0.9999962
## female:milk-female:bar  0.08 -0.8696391  1.02963912 0.9998815
## male:milk-female:bar    0.20 -0.7496391  1.14963912 0.9902930
## female:own-female:bar  -0.80 -1.7496391  0.14963912 0.1518375
## male:own-female:bar    -0.84 -1.7896391  0.10963912 0.1154632
## female:milk-male:bar    0.04 -0.9096391  0.98963912 0.9999962
## male:milk-male:bar      0.16 -0.7896391  1.10963912 0.9965724
## female:own-male:bar    -0.84 -1.7896391  0.10963912 0.1154632
## male:own-male:bar      -0.88 -1.8296391  0.06963912 0.0863095
## male:milk-female:milk   0.12 -0.8296391  1.06963912 0.9991375
## female:own-female:milk -0.88 -1.8296391  0.06963912 0.0863095
## male:own-female:milk   -0.92 -1.8696391  0.02963912 0.0634551
## female:own-male:milk   -1.00 -1.9496391 -0.05036088 0.0327048
## male:own-male:milk     -1.04 -1.9896391 -0.09036088 0.0229513
## male:own-female:own    -0.04 -0.9896391  0.90963912 0.9999962
```

```r
plot(TukeyHSD(aov3), las = 2)
```

![](Tasting_chocolate_files/figure-html/unnamed-chunk-13-3.png) ![](Tasting_chocolate_files/figure-html/unnamed-chunk-13-4.png) ![](Tasting_chocolate_files/figure-html/unnamed-chunk-13-5.png) 
