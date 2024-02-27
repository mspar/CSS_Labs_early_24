---
title: 'Lab 6: Conditional Logistic Regression Models'
author: "Benjamin Jarvis"
date: "27 February 2024"
output: pdf_document
fontsize: 11pt
institute: |
  | Institute for Analytical Sociology
  | Linköping University
---



## Objectives

- Reshape data from wide to long format for estimating choice models.

- Estimate and interpret conditional logistic regression models.

- Estimate and interpret decision-maker by alternative-specific covariate interactions.

- Produce in and out of sample predicted probabilities. 

## Reshaping (a.k.a., pivoting) data

- We use pivoting when we want to take data spread across many columns and instead spread that data across rows in a data set, and vice versa.

  - Taking data spread across columns and spreading it across (new) rows is called **reshaping** or **pivoting** to **long format**.

  - Taking data spread across rows and spreading it across (new) columns is called **reshaping** or **pivoting** to **wide format**.
  
- Sometimes this is called "spreading" and "gathering", or "folding" and "unfolding". 
  
- `tidyr` (from `tidyverse`) has the functions `pivot_longer()` and `pivot_wider()` to accomplish this.

- Another popular approach is to use the `melt()` and `dcast()` functions in the `data.table` package.


## Pivoting wide to long^[https://www.garrickadenbuie.com/project/tidyexplain/#spread-and-gather]
  
:::: {.cols data-latex=""}

::: {.col data-latex="{0.32\textwidth}"}
\includegraphics[width=\textwidth]{wide} 
:::

::: {.col data-latex="{0.01\textwidth}"}
\vspace{1ex}
:::

::: {.col data-latex="{0.32\textwidth}"}
\includegraphics[width=\textwidth]{pivot_longer} 
:::

::: {.col data-latex="{0.01\textwidth}"}
\vspace{1ex}
:::

::: {.col data-latex="{0.32\textwidth}"}
\includegraphics[width=\textwidth]{long} 
:::

::::


## Pivoting long to wide^[https://www.garrickadenbuie.com/project/tidyexplain/#spread-and-gather]
  
:::: {.cols data-latex=""}

::: {.col data-latex="{0.32\textwidth}"}
\includegraphics[width=\textwidth]{long} 
:::

::: {.col data-latex="{0.01\textwidth}"}
\vspace{1ex}
:::

::: {.col data-latex="{0.32\textwidth}"}
\includegraphics[width=\textwidth]{pivot_wider} 
:::

::: {.col data-latex="{0.01\textwidth}"}
\vspace{1ex}
:::

::: {.col data-latex="{0.32\textwidth}"}
\includegraphics[width=\textwidth]{wide} 
:::

::::

## Packages we'll use today


```r
library(tidyverse)
library(tidymodels)
library(survival)
library(kableExtra)
library(RColorBrewer)
```

You also should install, but do not need to load, the `Ecdat` package.

## Pivoting wide to long using `tidyr`, 1A


 \footnotesize


```r
relig_income |> slice(1:10)
```

```
## # A tibble: 10 x 11
##    religion `<$10k` `$10-20k` `$20-30k` `$30-40k` `$40-50k` `$50-75k` `$75-100k`
##    <chr>      <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>      <dbl>
##  1 Agnostic      27        34        60        81        76       137        122
##  2 Atheist       12        27        37        52        35        70         73
##  3 Buddhist      27        21        30        34        33        58         62
##  4 Catholic     418       617       732       670       638      1116        949
##  5 Don’t k~      15        14        15        11        10        35         21
##  6 Evangel~     575       869      1064       982       881      1486        949
##  7 Hindu          1         9         7         9        11        34         47
##  8 Histori~     228       244       236       238       197       223        131
##  9 Jehovah~      20        27        24        24        21        30         15
## 10 Jewish        19        19        25        25        30        95         69
## # i 3 more variables: `$100-150k` <dbl>, `>150k` <dbl>,
## #   `Don't know/refused` <dbl>
```

 \normalsize

## Pivoting wide to long using `tidyr`, 1B


 \footnotesize


```r
relig_income |> 
  pivot_longer(!religion, 
               names_to = "income", 
               values_to = "count")
```

```
## # A tibble: 180 x 3
##    religion income             count
##    <chr>    <chr>              <dbl>
##  1 Agnostic <$10k                 27
##  2 Agnostic $10-20k               34
##  3 Agnostic $20-30k               60
##  4 Agnostic $30-40k               81
##  5 Agnostic $40-50k               76
##  6 Agnostic $50-75k              137
##  7 Agnostic $75-100k             122
##  8 Agnostic $100-150k            109
##  9 Agnostic >150k                 84
## 10 Agnostic Don't know/refused    96
## # i 170 more rows
```

 \normalsize

## Pivoting wide to long using `tidyr`, 2A


 \footnotesize


```r
billboard |> slice(1:7)
```

```
## # A tibble: 7 x 79
##   artist      track date.entered   wk1   wk2   wk3   wk4   wk5   wk6   wk7   wk8
##   <chr>       <chr> <date>       <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 2 Pac       Baby~ 2000-02-26      87    82    72    77    87    94    99    NA
## 2 2Ge+her     The ~ 2000-09-02      91    87    92    NA    NA    NA    NA    NA
## 3 3 Doors Do~ Kryp~ 2000-04-08      81    70    68    67    66    57    54    53
## 4 3 Doors Do~ Loser 2000-10-21      76    76    72    69    67    65    55    59
## 5 504 Boyz    Wobb~ 2000-04-15      57    34    25    17    17    31    36    49
## 6 98^0        Give~ 2000-08-19      51    39    34    26    26    19     2     2
## 7 A*Teens     Danc~ 2000-07-08      97    97    96    95   100    NA    NA    NA
## # i 68 more variables: wk9 <dbl>, wk10 <dbl>, wk11 <dbl>, wk12 <dbl>,
## #   wk13 <dbl>, wk14 <dbl>, wk15 <dbl>, wk16 <dbl>, wk17 <dbl>, wk18 <dbl>,
## #   wk19 <dbl>, wk20 <dbl>, wk21 <dbl>, wk22 <dbl>, wk23 <dbl>, wk24 <dbl>,
## #   wk25 <dbl>, wk26 <dbl>, wk27 <dbl>, wk28 <dbl>, wk29 <dbl>, wk30 <dbl>,
## #   wk31 <dbl>, wk32 <dbl>, wk33 <dbl>, wk34 <dbl>, wk35 <dbl>, wk36 <dbl>,
## #   wk37 <dbl>, wk38 <dbl>, wk39 <dbl>, wk40 <dbl>, wk41 <dbl>, wk42 <dbl>,
## #   wk43 <dbl>, wk44 <dbl>, wk45 <dbl>, wk46 <dbl>, wk47 <dbl>, wk48 <dbl>, ...
```

 \normalsize


## Pivoting wide to long using `tidyr`, 2B


```r
billboard |> 
    pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk",
    values_to = "rank",
    values_drop_na = TRUE
  )
```

## Pivoting wide to long using `tidyr`, 2C


 \footnotesize


```
## # A tibble: 5,307 x 5
##    artist  track                   date.entered week   rank
##    <chr>   <chr>                   <date>       <chr> <dbl>
##  1 2 Pac   Baby Don't Cry (Keep... 2000-02-26   1        87
##  2 2 Pac   Baby Don't Cry (Keep... 2000-02-26   2        82
##  3 2 Pac   Baby Don't Cry (Keep... 2000-02-26   3        72
##  4 2 Pac   Baby Don't Cry (Keep... 2000-02-26   4        77
##  5 2 Pac   Baby Don't Cry (Keep... 2000-02-26   5        87
##  6 2 Pac   Baby Don't Cry (Keep... 2000-02-26   6        94
##  7 2 Pac   Baby Don't Cry (Keep... 2000-02-26   7        99
##  8 2Ge+her The Hardest Part Of ... 2000-09-02   1        91
##  9 2Ge+her The Hardest Part Of ... 2000-09-02   2        87
## 10 2Ge+her The Hardest Part Of ... 2000-09-02   3        92
## # i 5,297 more rows
```

 \normalsize

## Pivoting wide to long using `tidyr`, 3A

 \footnotesize


```r
who |> filter(year>2000) |> slice(1:5)
```

```
## # A tibble: 5 x 60
##   country   iso2  iso3   year new_sp_m014 new_sp_m1524 new_sp_m2534 new_sp_m3544
##   <chr>     <chr> <chr> <dbl>       <dbl>        <dbl>        <dbl>        <dbl>
## 1 Afghanis~ AF    AFG    2001         129          379          349          274
## 2 Afghanis~ AF    AFG    2002          90          476          481          368
## 3 Afghanis~ AF    AFG    2003         127          511          436          284
## 4 Afghanis~ AF    AFG    2004         139          537          568          360
## 5 Afghanis~ AF    AFG    2005         151          606          560          472
## # i 52 more variables: new_sp_m4554 <dbl>, new_sp_m5564 <dbl>,
## #   new_sp_m65 <dbl>, new_sp_f014 <dbl>, new_sp_f1524 <dbl>,
## #   new_sp_f2534 <dbl>, new_sp_f3544 <dbl>, new_sp_f4554 <dbl>,
## #   new_sp_f5564 <dbl>, new_sp_f65 <dbl>, new_sn_m014 <dbl>,
## #   new_sn_m1524 <dbl>, new_sn_m2534 <dbl>, new_sn_m3544 <dbl>,
## #   new_sn_m4554 <dbl>, new_sn_m5564 <dbl>, new_sn_m65 <dbl>,
## #   new_sn_f014 <dbl>, new_sn_f1524 <dbl>, new_sn_f2534 <dbl>, ...
```

 \normalsize


## Pivoting wide to long using `tidyr`, 3B


```r
who %>% select(-iso2,-iso3) |> 
  filter(year>2000) |> 
  pivot_longer(
  cols = new_sp_m014:newrel_f65,
  names_to = c("diagnosis", "gender", "age"),
  names_pattern = "new_?(.*)_(.)(.*)",
  values_to = "count"
)
```

## Pivoting wide to long using `tidyr`, 3C


 \footnotesize


```
## # A tibble: 156,128 x 6
##    country      year diagnosis gender age   count
##    <chr>       <dbl> <chr>     <chr>  <chr> <dbl>
##  1 Afghanistan  2001 sp        m      014     129
##  2 Afghanistan  2001 sp        m      1524    379
##  3 Afghanistan  2001 sp        m      2534    349
##  4 Afghanistan  2001 sp        m      3544    274
##  5 Afghanistan  2001 sp        m      4554    204
##  6 Afghanistan  2001 sp        m      5564    139
##  7 Afghanistan  2001 sp        m      65      103
##  8 Afghanistan  2001 sp        f      014     146
##  9 Afghanistan  2001 sp        f      1524    799
## 10 Afghanistan  2001 sp        f      2534    888
## # i 156,118 more rows
```

 \normalsize


## Revisiting the cars data from the `Ecdat` package

- We were working with data from a survey of California residents from 1996.

- Respondents were solicited to choose a car they would buy from a menu of 6 fictive cars with varying attributes.

- Focus was on assessing the appeal of alternative fuel vehicles.

- Mix of decision-maker level and alternative-level covariates.

## Column names in the `Car` data.


 \footnotesize


```r
Car<-as_tibble(Ecdat::Car)
names(Car)
```

```
##  [1] "choice"     "college"    "hsg2"       "coml5"      "type1"     
##  [6] "type2"      "type3"      "type4"      "type5"      "type6"     
## [11] "fuel1"      "fuel2"      "fuel3"      "fuel4"      "fuel5"     
## [16] "fuel6"      "price1"     "price2"     "price3"     "price4"    
## [21] "price5"     "price6"     "range1"     "range2"     "range3"    
## [26] "range4"     "range5"     "range6"     "acc1"       "acc2"      
## [31] "acc3"       "acc4"       "acc5"       "acc6"       "speed1"    
## [36] "speed2"     "speed3"     "speed4"     "speed5"     "speed6"    
## [41] "pollution1" "pollution2" "pollution3" "pollution4" "pollution5"
## [46] "pollution6" "size1"      "size2"      "size3"      "size4"     
## [51] "size5"      "size6"      "space1"     "space2"     "space3"    
## [56] "space4"     "space5"     "space6"     "cost1"      "cost2"     
## [61] "cost3"      "cost4"      "cost5"      "cost6"      "station1"  
## [66] "station2"   "station3"   "station4"   "station5"   "station6"
```

 \normalsize

## A view onto some of the `Car` data


 \scriptsize


```
## # A tibble: 16 x 71
##       id college  hsg2 coml5 choice  type1   type2 type3 type4 type5 type6 fuel1
##    <int>   <dbl> <dbl> <dbl> <fct>   <fct>   <fct> <fct> <fct> <fct> <fct> <fct>
##  1     1       0     0     0 choice1 van     regc~ van   stwa~ van   truck cng  
##  2     2       1     1     1 choice2 regcar  van   regc~ stwa~ regc~ truck meth~
##  3     3       0     1     0 choice5 regcar  truck regc~ van   regc~ stwa~ cng  
##  4     4       0     0     1 choice5 regcar  truck regc~ van   regc~ stwa~ meth~
##  5     5       0     1     0 choice5 regcar  truck regc~ van   regc~ stwa~ cng  
##  6     6       0     0     0 choice5 truck   regc~ truck van   truck stwa~ cng  
##  7     7       1     1     1 choice2 regcar  van   regc~ stwa~ regc~ truck meth~
##  8     8       1     0     1 choice5 regcar  van   regc~ stwa~ regc~ truck meth~
##  9     9       0     0     0 choice5 sportuv spor~ spor~ regc~ spor~ truck meth~
## 10    10       1     0     0 choice2 regcar  truck regc~ van   regc~ stwa~ meth~
## 11    11       1     0     0 choice1 regcar  truck regc~ van   regc~ stwa~ meth~
## 12    12       1     1     0 choice2 truck   stwa~ truck regc~ truck van   meth~
## 13    13       1     0     0 choice5 sportc~ truck spor~ spor~ spor~ regc~ meth~
## 14    14       0     0     0 choice5 regcar  stwa~ regc~ truck regc~ van   meth~
## 15    15       1     0     0 choice3 regcar  stwa~ regc~ truck regc~ van   meth~
## 16    16       1     0     0 choice6 truck   van   truck stwa~ truck regc~ meth~
## # i 59 more variables: fuel2 <fct>, fuel3 <fct>, fuel4 <fct>, fuel5 <fct>,
## #   fuel6 <fct>, price1 <dbl>, price2 <dbl>, price3 <dbl>, price4 <dbl>,
## #   price5 <dbl>, price6 <dbl>, range1 <dbl>, range2 <dbl>, range3 <dbl>,
## #   range4 <dbl>, range5 <dbl>, range6 <dbl>, acc1 <dbl>, acc2 <dbl>,
## #   acc3 <dbl>, acc4 <dbl>, acc5 <dbl>, acc6 <dbl>, speed1 <dbl>, speed2 <dbl>,
## #   speed3 <dbl>, speed4 <dbl>, speed5 <dbl>, speed6 <dbl>, pollution1 <dbl>,
## #   pollution2 <dbl>, pollution3 <dbl>, pollution4 <dbl>, pollution5 <dbl>, ...
```

 \normalsize


## To estimate a `clogit()` model, we need to reshape this data to long format


 \footnotesize


```r
CarLong <- Car |> 
  mutate(id=row_number()) |> # adding an id for respondents
  mutate(choice=str_extract(as.character(choice),"[1-6]"),
    choice=as.integer(choice)) |> # make choice var a #
  pivot_longer(
    cols=!matches(c("id","college","hsg2","coml5","choice")),
    names_pattern="(.+)([1-6])",  # parse colnames w/ regex
    names_to=c(".value","alt")) |> # colnames from "()" above
  mutate(alt=as.integer(alt)) |> 
  relocate(id,college,hsg2,coml5,alt) # rearrange for display
```

 \normalsize

## Result of pivot


 \footnotesize


```
## # A tibble: 12 x 17
##       id college  hsg2 coml5   alt choice type    fuel   price range   acc speed
##    <int>   <dbl> <dbl> <dbl> <int>  <int> <fct>   <fct>  <dbl> <dbl> <dbl> <dbl>
##  1     1       0     0     0     1      1 van     cng     4.18   250   4      95
##  2     1       0     0     0     2      1 regcar  cng     4.18   250   4      95
##  3     1       0     0     0     3      1 van     elect~  4.82   400   6     110
##  4     1       0     0     0     4      1 stwagon elect~  4.82   400   6     110
##  5     1       0     0     0     5      1 van     gasol~  5.14   250   2.5   140
##  6     1       0     0     0     6      1 truck   gasol~  5.14   250   2.5   140
##  7     2       1     1     1     1      2 regcar  metha~  3.31   125   2.5    85
##  8     2       1     1     1     2      2 van     metha~  3.31   125   2.5    85
##  9     2       1     1     1     3      2 regcar  cng     3.59   300   4     140
## 10     2       1     1     1     4      2 stwagon cng     3.59   300   4     140
## 11     2       1     1     1     5      2 regcar  gasol~  4.41   300   6      95
## 12     2       1     1     1     6      2 truck   gasol~  4.41   300   6      95
## # i 5 more variables: pollution <dbl>, size <dbl>, space <dbl>, cost <dbl>,
## #   station <dbl>
```

 \normalsize

## We still need to make our dependent variable!


```r
CarLong <- CarLong %>% 
  mutate(choice=as.integer(choice==alt))
```


## Result, with select columns re-arranged


 \footnotesize


```r
CarLong %>% 
  slice(1:12)
```

```
## # A tibble: 12 x 17
##       id college  hsg2 coml5   alt choice type    fuel   price range   acc speed
##    <int>   <dbl> <dbl> <dbl> <int>  <int> <fct>   <fct>  <dbl> <dbl> <dbl> <dbl>
##  1     1       0     0     0     1      1 van     cng     4.18   250   4      95
##  2     1       0     0     0     2      0 regcar  cng     4.18   250   4      95
##  3     1       0     0     0     3      0 van     elect~  4.82   400   6     110
##  4     1       0     0     0     4      0 stwagon elect~  4.82   400   6     110
##  5     1       0     0     0     5      0 van     gasol~  5.14   250   2.5   140
##  6     1       0     0     0     6      0 truck   gasol~  5.14   250   2.5   140
##  7     2       1     1     1     1      0 regcar  metha~  3.31   125   2.5    85
##  8     2       1     1     1     2      1 van     metha~  3.31   125   2.5    85
##  9     2       1     1     1     3      0 regcar  cng     3.59   300   4     140
## 10     2       1     1     1     4      0 stwagon cng     3.59   300   4     140
## 11     2       1     1     1     5      0 regcar  gasol~  4.41   300   6      95
## 12     2       1     1     1     6      0 truck   gasol~  4.41   300   6      95
## # i 5 more variables: pollution <dbl>, size <dbl>, space <dbl>, cost <dbl>,
## #   station <dbl>
```

 \normalsize

## Check the number of responses, alternatives and choices


```r
CarLong |> 
  group_by(id) |> 
  summarize(nalts=n(),choices=sum(choice)) |> 
  with(data=_,table(nalts,choices))
```

```
##      choices
## nalts    1
##     6 4654
```

## Summarizing the data using `gtsummary`


 \small


```r
library(gtsummary)
CarLong |> 
  mutate(chof=factor(choice,
                     labels=c("Unchosen","Chosen"))) |> 
  select(chof,type, fuel, price, range) |> 
  tbl_summary(by=chof, type=list(range~"continuous")) |>
  as_gt()
```

 \normalsize

## Data summarized using `gtsummary`


 \small

\setlength{\LTpost}{0mm}
\begin{longtable}{lcc}
\toprule
\textbf{Characteristic} & \textbf{Unchosen}, N = 23,270\textsuperscript{\textit{1}} & \textbf{Chosen}, N = 4,654\textsuperscript{\textit{1}} \\ 
\midrule\addlinespace[2.5pt]
type &  &  \\ 
    regcar & 8,190 (35\%) & 2,740 (59\%) \\ 
    sportuv & 806 (3.5\%) & 242 (5.2\%) \\ 
    sportcar & 708 (3.0\%) & 172 (3.7\%) \\ 
    stwagon & 4,141 (18\%) & 305 (6.6\%) \\ 
    truck & 5,063 (22\%) & 565 (12\%) \\ 
    van & 4,362 (19\%) & 630 (14\%) \\ 
fuel &  &  \\ 
    gasoline & 5,648 (24\%) & 1,310 (28\%) \\ 
    methanol & 6,161 (26\%) & 791 (17\%) \\ 
    cng & 5,954 (26\%) & 1,062 (23\%) \\ 
    electric & 5,507 (24\%) & 1,491 (32\%) \\ 
price & 4.04 (3.04, 5.24) & 4.04 (2.91, 5.14) \\ 
range & 250 (125, 300) & 300 (250, 300) \\ 
\bottomrule
\end{longtable}
\begin{minipage}{\linewidth}
\textsuperscript{\textit{1}}n (\%); Median (IQR)\\
\end{minipage}

 \normalsize

## A model

 \footnotesize


```r
m1<-clogit(choice~type+fuel+cost+strata(id),data=CarLong)
m1
```

```
## Call:
## clogit(choice ~ type + fuel + cost + strata(id), data = CarLong)
## 
##                coef exp(coef) se(coef)   z      p
## typesportuv   0.821     2.273    0.140   6  5e-09
## typesportcar  0.630     1.877    0.147   4  2e-05
## typestwagon  -1.430     0.239    0.062 -23 <2e-16
## typetruck    -1.013     0.363    0.049 -21 <2e-16
## typevan      -0.808     0.446    0.047 -17 <2e-16
## fuelmethanol -0.654     0.520    0.049 -13 <2e-16
## fuelcng      -0.257     0.773    0.044  -6  7e-09
## fuelelectric  0.154     1.166    0.042   4  3e-04
## cost         -0.078     0.925    0.007 -10 <2e-16
## 
## Likelihood ratio test=1549  on 9 df, p=<2e-16
## n= 27924, number of events= 4654
```

 \normalsize

## Presenting model coefficients as odds using `gtsummary`


 \small


```r
tbl_regression(m1, exponentiate = TRUE) |> 
  modify_header(list(label="**Covariate**",
                     estimate="**Odds**")) |>
  modify_footnote(update = list(estimate ~ NA), 
                  abbreviation=TRUE)
```

 \normalsize

## Model coefficients presented using `gtsummary`


 \footnotesize

\setlength{\LTpost}{0mm}
\begin{longtable}{lccc}
\toprule
\textbf{Covariate} & \textbf{Odds} & \textbf{95\% CI}\textsuperscript{\textit{1}} & \textbf{p-value} \\ 
\midrule\addlinespace[2.5pt]
type &  &  &  \\ 
    regcar & — & — &  \\ 
    sportuv & 2.27 & 1.73, 2.99 & <0.001 \\ 
    sportcar & 1.88 & 1.41, 2.51 & <0.001 \\ 
    stwagon & 0.24 & 0.21, 0.27 & <0.001 \\ 
    truck & 0.36 & 0.33, 0.40 & <0.001 \\ 
    van & 0.45 & 0.41, 0.49 & <0.001 \\ 
fuel &  &  &  \\ 
    gasoline & — & — &  \\ 
    methanol & 0.52 & 0.47, 0.57 & <0.001 \\ 
    cng & 0.77 & 0.71, 0.84 & <0.001 \\ 
    electric & 1.17 & 1.07, 1.27 & <0.001 \\ 
cost & 0.93 & 0.91, 0.94 & <0.001 \\ 
\bottomrule
\end{longtable}
\begin{minipage}{\linewidth}
\textsuperscript{\textit{1}}CI = Confidence Interval\\
\end{minipage}

 \normalsize



## YOUR TURN

Identify a variable that might explain why methanol or cng vehicles are so much less preferred than gasoline vehicles. 

- Hint: check the correlation between fuel and other variables in the dataset.

Estimate a model where you add in the expected mediator.

Interpret the cng and methanol coefficients before and after you include the mediator.


## SOLUTION

Any suggestions? What did you all find?

## In-Sample Predictions

 - In sample predictions are easy to produce
 - Specify `type="expected"` to get probabilities!
- "Binding" it to the data set is helpful for display


 \footnotesize


```r
CarLong %>% bind_cols(Pr=predict(m1,type="expected")) %>% 
  select(id,alt,choice,type,fuel,cost,Pr)
```

```
## # A tibble: 27,924 x 7
##       id   alt choice type    fuel      cost     Pr
##    <int> <int>  <int> <fct>   <fct>    <dbl>  <dbl>
##  1     1     1      1 van     cng          4 0.144 
##  2     1     2      0 regcar  cng          4 0.323 
##  3     1     3      0 van     electric     6 0.186 
##  4     1     4      0 stwagon electric     6 0.0998
##  5     1     5      0 van     gasoline     8 0.136 
##  6     1     6      0 truck   gasoline     8 0.111 
##  7     2     1      0 regcar  methanol     4 0.185 
##  8     2     2      1 van     methanol     4 0.0823
##  9     2     3      0 regcar  cng          8 0.201 
## 10     2     4      0 stwagon cng          8 0.0481
## # i 27,914 more rows
```

 \normalsize

## YOUR TURN

Write some R code that confirms that the probabilities sum to one within values of `id`

## Out of Sample Predictions

- To predict with a new dataset, we need the same covariates used in our model (including `id`!).
- Doesn't even have to be the same size choice set as in the original data.
- Let's see how well our model encapsulates your worst stereotype of American car owners.


 \footnotesize


```r
NewCar<-tibble(id=rep(1,3),
               type=factor(c("sportcar","sportuv","regcar")),
               fuel=factor(c("gasoline","gasoline","electric")),
               cost=c(8,8,1))
```

 \normalsize

## Out of Sample Predictions continued

Unfortunately, the `type="expected"` option doesn't seem to work when using new data, so we have some extra work to do using the linear predictions


 \footnotesize


```r
NewCar %>% bind_cols(Za=predict(m1,type="lp",newdata=.)) %>% 
  group_by(id) %>% 
  mutate(Pr=exp(Za)/sum(exp(Za)))
```

```
## # A tibble: 3 x 6
## # Groups:   id [1]
##      id type     fuel      cost    Za    Pr
##   <dbl> <fct>    <fct>    <dbl> <dbl> <dbl>
## 1     1 sportcar gasoline     8  1.32 0.305
## 2     1 sportuv  gasoline     8  1.51 0.369
## 3     1 regcar   electric     1  1.39 0.327
```

 \normalsize


## Interactions (1/6)

- What if we want to test the hypothesis that larger households care more about cargo space than smaller households?
 
- We put an appropriate interaction into our model


 \footnotesize


```r
m2<-clogit(choice~type+fuel+cost+space+space*hsg2+strata(id)
           ,data=CarLong)
```

 \normalsize

## Interactions (2/6)


 \footnotesize


```
## Call:
## clogit(choice ~ type + fuel + cost + space + space * hsg2 + strata(id), 
##     data = CarLong)
## 
##                coef exp(coef) se(coef)   z      p
## typesportuv   0.820     2.270    0.140   6  5e-09
## typesportcar  0.629     1.876    0.147   4  2e-05
## typestwagon  -1.429     0.239    0.062 -23 <2e-16
## typetruck    -1.013     0.363    0.049 -21 <2e-16
## typevan      -0.808     0.446    0.047 -17 <2e-16
## fuelmethanol -0.610     0.543    0.056 -11 <2e-16
## fuelcng      -0.215     0.806    0.052  -4  3e-05
## fuelelectric  0.153     1.166    0.042   4  3e-04
## cost         -0.078     0.925    0.007 -10 <2e-16
## space         0.398     1.488    0.200   2   0.05
## hsg2             NA        NA    0.000  NA     NA
## space:hsg2   -0.448     0.639    0.333  -1   0.18
## 
## Likelihood ratio test=1553  on 11 df, p=<2e-16
## n= 27924, number of events= 4654
```

 \normalsize

## Interactions (3/6)

- Note the omitted category for the `hsg2` variable.

- We now have two interpretations to make with the `space` variable.
  - A small household has $e^{0.398}=1.488$ times higher probability to choose a car that has $100\%$ more cargo space than a typical, full size, gas powered car, all else equal.
  - A large household is $e^{0.398-0.448} = 1.488 \times 0.639 =  0.951$ times lower probability to choose a car with $100\%$ more carego space than a typical gas powered car, all else equal.
  
- Actually, we find the opposite of what we expected... but our model is quite poorly specified.

- For interactions with continuous covariates, we might select a few typical values (e.g., 1st quartile, median, and 3rd quartile) of the individual variable and calculate odds for these select values.
 
## Interactions (4/6)

- We could have estimated a model with the interactions specified slightly differently


 \footnotesize


```r
m3<-clogit(choice~type+fuel+cost+space:factor(hsg2)+strata(id)
           ,data=CarLong)
```

 \normalsize

## Interactions (5/6)


 \footnotesize


```
## Call:
## clogit(choice ~ type + fuel + cost + space:factor(hsg2) + strata(id), 
##     data = CarLong)
## 
##                       coef exp(coef) se(coef)     z      p
## typesportuv          0.820     2.270    0.140   5.8  5e-09
## typesportcar         0.629     1.876    0.147   4.3  2e-05
## typestwagon         -1.429     0.239    0.062 -23.1 <2e-16
## typetruck           -1.013     0.363    0.049 -20.7 <2e-16
## typevan             -0.808     0.446    0.047 -17.2 <2e-16
## fuelmethanol        -0.610     0.543    0.056 -10.9 <2e-16
## fuelcng             -0.215     0.806    0.052  -4.1  3e-05
## fuelelectric         0.153     1.166    0.042   3.6  3e-04
## cost                -0.078     0.925    0.007 -10.5 <2e-16
## space:factor(hsg2)0  0.398     1.488    0.200   2.0   0.05
## space:factor(hsg2)1 -0.051     0.951    0.316  -0.2   0.87
## 
## Likelihood ratio test=1553  on 11 df, p=<2e-16
## n= 27924, number of events= 4654
```

 \normalsize

## Interactions (6/6)

- But notice that these models are actually the same model!


 \footnotesize


```
## # A tibble: 2 x 2
##   model logLik
##   <chr>  <dbl>
## 1 m2    -7562.
## 2 m3    -7562.
```

 \normalsize


 \footnotesize


```
## # A tibble: 27,924 x 2
##     pr_m2  pr_m3
##     <dbl>  <dbl>
##  1 0.138  0.138 
##  2 0.310  0.310 
##  3 0.192  0.192 
##  4 0.103  0.103 
##  5 0.141  0.141 
##  6 0.115  0.115 
##  7 0.191  0.191 
##  8 0.0850 0.0850
##  9 0.204  0.204 
## 10 0.0489 0.0489
## # i 27,914 more rows
```

 \normalsize

## YOUR TURN

Hypothesize and test another decion-maker by alternative interaction. Produce *one* fictive choice set and predictions for that choice set for fictive people who differ in the decision-maker specific attribute you used for your interaction.

## YOUR TURN

Experiment more with `clogit()`. Try:

- Additional covariates
- Predictions based on new choice sets
- Making the "best" model you can.
- Compare predictions between simple models and your "best" model.
- etc.

## Next Up

- Large choice sets
- Linear hypothesis testing
- Model fit
