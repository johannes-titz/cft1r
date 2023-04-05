## Quick overview

This analysis accompanies the following paper: Titz & Ackermann,
Deviations From Perfect Scaling as a Path to Perfect Scaling: An
Intelligence Test Example

The general idea of this analysis is to show that deviations from
perfect scaling (unidimensionality) should not be treated as noise but
as valuable information. This information can lead to a substantial
improvement of items, which is demonstrated with the CFT 1-R
intelligence test.

The data includes 3 Tests (Subtests 4 to 6), which supposedly measure
inductive reasoning. Factor analysis results in a good 1-factor
solution. Since all correlations are positive, STA will always result in
a monotonic model and unidimensionality. In this case STA is extremely
conservative regarding unidimensionality. A better solution is to use
Guttman scaling to check unidimensionality. The novelity in this paper
is to use Guttman scaling to find out which items are problematic and
which persons produce these answers and why.

A concrete example is the item pair 12 (subtest 5) and 15 (subtest 6),
which show the most violations of undimensionality. The standard
procedure is to ignore these violations and treat them as random noise.
But when we look at the data more carefully, we see that these
violations are systematic. First of all, because of the speed character
several participants did not even reach the item 12. We do not know
their ability on this item, but treat the item as unsolved. Second, the
distribution of distractors is not uniform. Most participants choose the
same distractor.

## libs

``` r
# please uncomment this line if you do not have librarian yet
# install.packages(librarian)
librarian::shelf(tidyverse, johannes-titz/zysno, plot.matrix, colorspace,
                 foreign, psych, psych, simpleCache, xtable)
setCacheDir("cache")
```

## read data

pre.csv and post.csv contain all answers to all items from subtests 4-6.
pre is the pre test, post is the post test. For our analysis no other
information is required.

``` r
pre <- readr::read_csv("pre.csv")
post <- readr::read_csv("post.csv")
```

## Factor Analysis with pre data set

For FA, 0-variance items must be removed, so difficulties must be larger
than 0 and smaller than 1. Otherwise, correlations cannot be calculated
and fa is not happy with NA values.

``` r
difficulty <- colMeans(pre, na.rm = TRUE)
filter <- which(difficulty < 1 & difficulty > 0)
pre2 <- pre %>% select(filter)
```

    ## Warning: Using an external vector in selections was deprecated in tidyselect 1.1.0.
    ## ℹ Please use `all_of()` or `any_of()` instead.
    ##   # Was:
    ##   data %>% select(filter)
    ## 
    ##   # Now:
    ##   data %>% select(all_of(filter))
    ## 
    ## See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

Out of 45 variables, 45 have item difficulties \< 0 and \< 1 and are
used in the analysis.

``` r
cors <- cor(pre2, use = "pairwise.complete.obs")
cors <- na.omit(cors)
fa1 <- fa(r = cors, nfactors = 1)
fa1
```

    ## Factor Analysis using method =  minres
    ## Call: fa(r = cors, nfactors = 1)
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##               MR1    h2   u2 com
    ## Subtest_4_1  0.33 0.112 0.89   1
    ## Subtest_4_2  0.39 0.154 0.85   1
    ## Subtest_4_3  0.42 0.172 0.83   1
    ## Subtest_4_4  0.58 0.335 0.67   1
    ## Subtest_4_5  0.45 0.201 0.80   1
    ## Subtest_4_6  0.50 0.250 0.75   1
    ## Subtest_4_7  0.61 0.371 0.63   1
    ## Subtest_4_8  0.50 0.253 0.75   1
    ## Subtest_4_9  0.66 0.439 0.56   1
    ## Subtest_4_10 0.50 0.250 0.75   1
    ## Subtest_4_11 0.56 0.312 0.69   1
    ## Subtest_4_12 0.55 0.305 0.70   1
    ## Subtest_4_13 0.39 0.155 0.84   1
    ## Subtest_4_14 0.27 0.073 0.93   1
    ## Subtest_4_15 0.40 0.160 0.84   1
    ## Subtest_5_1  0.25 0.062 0.94   1
    ## Subtest_5_2  0.34 0.117 0.88   1
    ## Subtest_5_3  0.40 0.160 0.84   1
    ## Subtest_5_4  0.37 0.135 0.87   1
    ## Subtest_5_5  0.37 0.140 0.86   1
    ## Subtest_5_6  0.33 0.107 0.89   1
    ## Subtest_5_7  0.32 0.104 0.90   1
    ## Subtest_5_8  0.42 0.177 0.82   1
    ## Subtest_5_9  0.38 0.142 0.86   1
    ## Subtest_5_10 0.35 0.121 0.88   1
    ## Subtest_5_11 0.39 0.154 0.85   1
    ## Subtest_5_12 0.27 0.074 0.93   1
    ## Subtest_5_13 0.43 0.185 0.82   1
    ## Subtest_5_14 0.25 0.061 0.94   1
    ## Subtest_5_15 0.37 0.137 0.86   1
    ## Subtest_6_1  0.10 0.010 0.99   1
    ## Subtest_6_2  0.33 0.111 0.89   1
    ## Subtest_6_3  0.51 0.256 0.74   1
    ## Subtest_6_4  0.51 0.258 0.74   1
    ## Subtest_6_5  0.43 0.184 0.82   1
    ## Subtest_6_6  0.55 0.301 0.70   1
    ## Subtest_6_7  0.44 0.191 0.81   1
    ## Subtest_6_8  0.55 0.301 0.70   1
    ## Subtest_6_9  0.48 0.231 0.77   1
    ## Subtest_6_10 0.62 0.384 0.62   1
    ## Subtest_6_11 0.57 0.320 0.68   1
    ## Subtest_6_12 0.53 0.283 0.72   1
    ## Subtest_6_13 0.44 0.192 0.81   1
    ## Subtest_6_14 0.43 0.186 0.81   1
    ## Subtest_6_15 0.37 0.139 0.86   1
    ## 
    ##                 MR1
    ## SS loadings    8.77
    ## Proportion Var 0.19
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 factor is sufficient.
    ## 
    ## df null model =  990  with the objective function =  13.45
    ## df of  the model are 945  and the objective function was  5.91 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.07 
    ## The df corrected root mean square of the residuals is  0.07 
    ## 
    ## Fit based upon off diagonal values = 0.89
    ## Measures of factor score adequacy             
    ##                                                    MR1
    ## Correlation of (regression) scores with factors   0.96
    ## Multiple R square of scores with factors          0.92
    ## Minimum correlation of possible factor scores     0.84

``` r
loadings <- unclass(fa1$loadings)
cors2 <- round(cors, 3)
mynames <- colnames(cors2)
colnames(cors2) <- substr(mynames, 9, 12)
rownames(cors2) <- substr(mynames, 9, 12)
breaks <- seq(-0.1, max(cors2[upper.tri(cors2)]), 0.1)
plot(cors2,
     col = sequential_hcl(length(breaks),
                          rev = TRUE, palette = "Blues"),
     axis.col = list(side = 1, las = 2, cex.axis = 0.5),
     axis.row = list(side = 2, las = 2, cex.axis = 0.5),
     key = list(cex.axis = 1, side = 3), spacing.key = c(2, 2, 0.5),
     breaks = breaks,
     na.col = "white", main = "", na.cell = FALSE, xlab="", ylab="")
```

![](plots/FA-1.pdf)

What can be seen is that within a subtest the correlations are generally
higher than across subtests. But overall a 1-factor solution seems fine,
judging from the factor loadings.

Regarding STA, this data set is clearly unidimensional because there are
almost no negative correlations (red) and the few ones can be explained
with random noise.

``` r
pre3 <- pre2 %>%
  select(Subtest_5_12, Subtest_6_12)
cors <- cor(pre3, use = "pairwise.complete.obs")
cors <- na.omit(cors)
fa1 <- fa(r = cors, nfactors = 1)
fa1
loadings <- unclass(fa1$loadings)
cbind(pre3, factor.scores(scale(pre3), fa1)$scores)
```

## Ordinal analyses

Guttman scaling with homogeneity index and Zysno’s scalability index.
This takes some time, so we cache it with simpleCache.

``` r
simpleCache("zys", zysnotize(as.matrix(pre)))
            #recreate = TRUE)
zys$scalability
```

    ## [1] 0.4260071

``` r
simpleCache("lv", suppressWarnings(loevenize(as.matrix(pre))))
            #, recreate = TRUE)
lv$h
```

    ## [1] 0.3238132

Zysno’s scalability for the whole test is medium (0.43) and Loevinger’s
homogenity is even lower (0.32). This is not necessarily a contradiction
to the factor analysis. We have to keep in mind that there are many
random influences, such as guessing, attention fluctuations, etc.. The
question is rather: what can be gained from ordinal analyses beyond FA?

Plot results. We just use the number of errors because this is
straightforward and intuitive.

``` r
item_matrix <- lv$error_matrix
colnames(item_matrix) <- substr(names(pre), 9, 12)
rownames(item_matrix) <- substr(names(pre), 9, 12)
item_matrix <- round(item_matrix, 3)
breaks <- seq(min(item_matrix), max(item_matrix), 5)
plot(item_matrix, asp = FALSE,
     col = sequential_hcl(length(breaks), rev = TRUE, palette = "Blues"),
     xlab = "", ylab = "", main = "",
     axis.col = list(side = 1, las = 2, cex.axis = 0.5),
     axis.row = list(side = 2, las = 2, cex.axis = 0.5),
     key = list(cex.axis = 1, side = 3), spacing.key = c(2, 2, 0.5),
     breaks = breaks, na.cell = FALSE
)
```

![](plots/ordinal%20plot-1.pdf)

Make a table with loadings and number of errors:

``` r
loadings2 <- loadings
colnames(loadings2) <- "factor loading"
rownames(loadings2) <- substr(rownames(loadings2), 9, 12)
item_matrix2 <- item_matrix
avg_viol <- colMeans(item_matrix2)
plot(as.numeric(loadings2), avg_viol)
```

![](plots/unnamed-chunk-3-1.pdf)

``` r
cor(as.numeric(loadings2), avg_viol)
```

    ## [1] 0.2978589

``` r
lowest_cors <- apply(cors, 2, min)
highest_viol <- apply(item_matrix2, 2, max)
tbl <- cbind(loadings2, avg_viol, highest_viol)
rownames(tbl) <- rownames(loadings2)
print(xtable(tbl, caption = "Factor loadings of unidimensional factor analysis model.", label = "tab:fa1", digits = c(0, 2, 0, 0)),
      file = "tables/fa1.tex",
      booktabs = TRUE, size="\\fontsize{9pt}{10pt}\\selectfont")
cors <- ifelse(cors == 1, NA, cors)
```

min unidim: 0 max unidim: 75

Once again, within a subtest there are fewer violations than between.

## Most problematic item pairs

What are the largest violations?

``` r
em <- lv$error_matrix
colnames(em) <- colnames(pre)
rownames(em) <- colnames(pre)
em2 <- reshape2::melt(em)
em2 %>%
  filter(value >= 70)
```

    ##             row          col value
    ## 1   Subtest_6_7 Subtest_4_10    74
    ## 2   Subtest_6_9 Subtest_4_10    72
    ## 3   Subtest_6_6  Subtest_5_9    73
    ## 4   Subtest_6_5 Subtest_5_10    72
    ## 5  Subtest_6_15 Subtest_5_12    75
    ## 6  Subtest_5_10  Subtest_6_5    72
    ## 7   Subtest_5_9  Subtest_6_6    73
    ## 8  Subtest_4_10  Subtest_6_7    74
    ## 9  Subtest_4_10  Subtest_6_9    72
    ## 10 Subtest_5_12 Subtest_6_15    75

## Find persons that answered inconsistent on 5_12/6_15

``` r
table(pre$Subtest_5_12, pre$Subtest_6_15)
```

    ##    
    ##       0   1
    ##   0 173  76
    ##   1  75  39

Scaling:

``` r
vec <- paste0(pre$Subtest_5_12, pre$Subtest_6_15)
table(vec)
```

    ## vec
    ##  00  01  10  11 
    ## 173  76  75  39

``` r
colMeans(pre[, c("Subtest_5_12", "Subtest_6_15")])
```

    ## Subtest_5_12 Subtest_6_15 
    ##    0.3140496    0.3168044

10, in this case, means that they have solved the difficult item but not
the easy one.

Now, we had to look into the raw data again and check all inconsistent
answers. This data is stored in pair.csv.

## Pair 5_12 and 6_15

``` r
p <- read.csv2("pair.csv")
names(p) <- c("attempted", "answer", "next_attempted_item",
              "last_attempted_item", "attempted_all_items", "comment")
p$not_reached <- p$attempted == 0 & p$next_attempted_item == 0
p$answer2 <- ifelse(p$not_reached, "not_reached", p$answer)
```

Answers

``` r
table(p$answer2, useNA = "always")
```

    ## 
    ##           1           3           4           5 not_reached        <NA> 
    ##           1           4          16          38          14           4

``` r
round(prop.table(table(p$answer2, useNA = "always")), 3)
```

    ## 
    ##           1           3           4           5 not_reached        <NA> 
    ##       0.013       0.052       0.208       0.494       0.182       0.052

``` r
chisq.test(c(0, 4, 16, 38))
```

    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  c(0, 4, 16, 38)
    ## X-squared = 60.345, df = 3, p-value = 4.961e-13

1 participant actually solved the item, but it did not count because the
time was up.

14 participants did not reach item 12. If item 12 was not attempted and
all other items after that were also not attempted, we counted it as
“not reached”. Regarding the general intelligence ability, we need to
exclude these participants because we do not know whether they have the
ability to solve the item or not. The violation of undimensionality is
clear: the second factor is the speed dimension.

4 participants skipped item 12. These cannot be scaled.

None of the participants chose distractor 2 and only 3 chose distractor
3.

Option 5 turns out to be an attractor with almost half of the
participants choosing this option. What is wrong with option 5?

2 Dimensions are relevant, the shape and the filling state (percentage
of black in the shape). To adults, it should be fairly clear that the
filling state is the decisive factor and item option 1 does not belong.
But many children do not necessarily think so. They look at the shape
and the rhombus is the most peculiar shape. But even the rectangle
(option 4) deviates from basic geometric forms.

## Conclusion, so far

We cannot scale

-   4 participants that skipped the item
-   4 participants choosing option 3

If we assume that options 4 and 5 are actually valid, all other
participants can be scaled. Even if only option 5 is considered valid,
this makes it possible to scale 38 additional participants. With FA they
would be regarded as noise.

Note that this has no effect on other participants. If somebody chose
option 4 or 5 for item 12, his or her answers were consistent before and
are still consistent now. These participants would simply move from cell
a to cell c. Of course their scale position should be more accurate now,
but the scalability does not change overall.

If some participants did not reach item 15 on subtest 6, they might need
to be removed because we do not know whether they are able to solve the
item. This could reduce scalablity. The best way would be to make a new
study.

## Same procedure with the post data set

``` r
vec <- paste0(post$Subtest_5_12_post, post$Subtest_6_15_post)
table(vec)
```

    ## vec
    ##  00  01  10  11 
    ## 139  67  87  70

``` r
colMeans(post[, c("Subtest_5_12_post", "Subtest_6_15_post")])
```

    ## Subtest_5_12_post Subtest_6_15_post 
    ##         0.4325069         0.3774105

``` r
table(post$Subtest_5_12_post, post$Subtest_6_15_post)
```

    ##    
    ##       0   1
    ##   0 139  67
    ##   1  87  70

Indeed, 5_12 is easier than 6_15! 67 are not scalable, this is only a
little less than before.

Now, we had to check these cases in the the raw data. The results are
stored in pairPost.csv.

``` r
p <- read.csv2("pairPost.csv")
names(p) <- c("answer", "next_attempted_item")
p$not_reached <- is.na(p$answer) & is.na(p$next_attempted_item)
p$answer2 <- ifelse(p$not_reached, "not_reached", p$answer)
```

Answers

``` r
table(p$answer2, useNA = "always")
```

    ## 
    ##    1    2    3    4    5 <NA> 
    ##    2    2    5   10   45    3

``` r
round(prop.table(table(p$answer2, useNA = "always")), 3)
```

    ## 
    ##     1     2     3     4     5  <NA> 
    ## 0.030 0.030 0.075 0.149 0.672 0.045

``` r
chisq.test(c(2, 5, 10, 45))
```

    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  c(2, 5, 10, 45)
    ## X-squared = 76.968, df = 3, p-value < 2.2e-16

## FA post

This is not part of the paper, but might still be of interest to some.

For FA, 0-variance items must be removed, so difficulties must be larger
than 0 and smaller than 1. Otherwise, correlations cannot be calculated
and fa is not happy with NA values.

``` r
difficulty <- colMeans(post, na.rm = TRUE)
filter <- which(difficulty < 1 & difficulty > 0)
post2 <- post %>% select(filter)
```

Out of variables, 45 have item difficulties \< 0 and \< 1 and are used
in the analysis.

Now, the actual analysis:

``` r
cors <- cor(post2, use = "pairwise.complete.obs")
cors <- na.omit(cors)
fa1 <- fa(r = cors, nfactors = 1)
fa1
```

    ## Factor Analysis using method =  minres
    ## Call: fa(r = cors, nfactors = 1)
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                    MR1      h2   u2 com
    ## Subtest_4_1_post  0.40 0.15794 0.84   1
    ## Subtest_4_2_post  0.47 0.22513 0.77   1
    ## Subtest_4_3_post  0.47 0.22305 0.78   1
    ## Subtest_4_4_post  0.54 0.28921 0.71   1
    ## Subtest_4_5_post  0.49 0.23724 0.76   1
    ## Subtest_4_6_post  0.54 0.28992 0.71   1
    ## Subtest_4_7_post  0.67 0.45379 0.55   1
    ## Subtest_4_8_post  0.59 0.34655 0.65   1
    ## Subtest_4_9_post  0.57 0.32794 0.67   1
    ## Subtest_4_10_post 0.58 0.33403 0.67   1
    ## Subtest_4_11_post 0.62 0.38158 0.62   1
    ## Subtest_4_12_post 0.50 0.25102 0.75   1
    ## Subtest_4_13_post 0.53 0.27741 0.72   1
    ## Subtest_4_14_post 0.37 0.13649 0.86   1
    ## Subtest_4_15_post 0.40 0.15960 0.84   1
    ## Subtest_5_1_post  0.02 0.00036 1.00   1
    ## Subtest_5_2_post  0.33 0.10868 0.89   1
    ## Subtest_5_3_post  0.40 0.16046 0.84   1
    ## Subtest_5_4_post  0.26 0.06755 0.93   1
    ## Subtest_5_5_post  0.36 0.12604 0.87   1
    ## Subtest_5_6_post  0.31 0.09713 0.90   1
    ## Subtest_5_7_post  0.37 0.14013 0.86   1
    ## Subtest_5_8_post  0.40 0.15748 0.84   1
    ## Subtest_5_9_post  0.53 0.27912 0.72   1
    ## Subtest_5_10_post 0.39 0.15180 0.85   1
    ## Subtest_5_11_post 0.47 0.21853 0.78   1
    ## Subtest_5_12_post 0.33 0.11167 0.89   1
    ## Subtest_5_13_post 0.47 0.22090 0.78   1
    ## Subtest_5_14_post 0.28 0.07694 0.92   1
    ## Subtest_5_15_post 0.46 0.20718 0.79   1
    ## Subtest_6_1_post  0.15 0.02256 0.98   1
    ## Subtest_6_2_post  0.33 0.10593 0.89   1
    ## Subtest_6_3_post  0.42 0.17600 0.82   1
    ## Subtest_6_4_post  0.45 0.20228 0.80   1
    ## Subtest_6_5_post  0.50 0.24848 0.75   1
    ## Subtest_6_6_post  0.61 0.36885 0.63   1
    ## Subtest_6_7_post  0.49 0.23685 0.76   1
    ## Subtest_6_8_post  0.57 0.32907 0.67   1
    ## Subtest_6_9_post  0.46 0.20711 0.79   1
    ## Subtest_6_10_post 0.65 0.42817 0.57   1
    ## Subtest_6_11_post 0.62 0.38522 0.61   1
    ## Subtest_6_12_post 0.61 0.37011 0.63   1
    ## Subtest_6_13_post 0.52 0.27199 0.73   1
    ## Subtest_6_14_post 0.43 0.18331 0.82   1
    ## Subtest_6_15_post 0.31 0.09790 0.90   1
    ## 
    ##                 MR1
    ## SS loadings    9.85
    ## Proportion Var 0.22
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 factor is sufficient.
    ## 
    ## df null model =  990  with the objective function =  14.06
    ## df of  the model are 945  and the objective function was  5.19 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.06 
    ## The df corrected root mean square of the residuals is  0.06 
    ## 
    ## Fit based upon off diagonal values = 0.93
    ## Measures of factor score adequacy             
    ##                                                    MR1
    ## Correlation of (regression) scores with factors   0.97
    ## Multiple R square of scores with factors          0.93
    ## Minimum correlation of possible factor scores     0.86

``` r
loadings <- unclass(fa1$loadings)
```
