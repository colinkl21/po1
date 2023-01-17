Po 1- Boxplots
================
Colin Li
1/17/2023

``` r
library(haven)
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
Adata <- read_sav("C:/Users/Colin/Documents/GitHub/po1/AQ.sav")
Adf <- Adata %>% 
  filter(Asian == "1" & Conditions == "Ingroup")

t.test(Adf$Q95, Adf$Asian_Warmth, paired = TRUE, alternative = "two.sided")
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  Adf$Q95 and Adf$Asian_Warmth
    ## t = 2.2494, df = 91, p-value = 0.0269
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  0.02669492 0.42982682
    ## sample estimates:
    ## mean difference 
    ##       0.2282609

``` r
library(ggplot2)
library(ggpubr)
Before <-Adf$Q95
After <-Adf$Asian_Warmth
d <- data.frame(Before = Before, After = After)
         ggpaired(d, cond1 = "Before", cond2 = "After",
         fill = "condition", line.color = "gray", line.size = 0.4,
         palette = "rainbow") +
 stat_compare_means(vjust = -0.4, method = "t.test", paired = TRUE)
```

    ## Warning: `gather_()` was deprecated in tidyr 1.2.0.
    ## ℹ Please use `gather()` instead.
    ## ℹ The deprecated feature was likely used in the ggpubr package.
    ##   Please report the issue at <]8;;https://github.com/kassambara/ggpubr/issueshttps://github.com/kassambara/ggpubr/issues]8;;>.

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

![](po1_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ tibble  3.1.8     ✔ purrr   0.3.5
    ## ✔ tidyr   1.2.1     ✔ stringr 1.5.0
    ## ✔ readr   2.1.3     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(rafalib)

mypar(1,1)
dat <- list(Before=Adf$Q95, After=Adf$Asian_Warmth)
dat %>%
   boxplot(xlab = "Condition",
           ylab = "Warmth",
           cex = 0)
 dat %>%
   stripchart(
     vertical = TRUE,
     method = "jitter",
     pch = 16,
     add = TRUE,
     col = 1
   )
mtext(text="p = .027", side = 3, adj = 1, col = 1, cex = 1.25, font = 4)
```

![](po1_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
