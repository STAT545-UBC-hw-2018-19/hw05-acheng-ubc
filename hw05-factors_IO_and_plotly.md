STAT545 Assignment 5
================
Alex
October 19, 2018

-   [Factor Management](#factor-management)
    -   [Dropping a factor](#dropping-a-factor)
    -   [Reordering Levels with arrange() and the forcats package](#reordering-levels-with-arrange-and-the-forcats-package)
-   [File I/O](#file-io)
    -   [Writing files](#writing-files)
    -   [Reading files](#reading-files)
    -   [Does arranging the factor levels with arrange() survive a R/W cycle?](#does-arranging-the-factor-levels-with-arrange-survive-a-rw-cycle)
    -   [Does reordering levels using the forcats package survive a R/W cycle?](#does-reordering-levels-using-the-forcats-package-survive-a-rw-cycle)
-   [Visualization design](#visualization-design)
    -   [Old Plot](#old-plot)
    -   [New Plot](#new-plot)
    -   [Plotly](#plotly)
-   [Writing figures to file](#writing-figures-to-file)

``` r
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
```

    ## -- Attaching packages ---------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts ------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
library(ggplot2)
library(gapminder)
library(kableExtra)
library(plotly)
```

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
library(scales)
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

Factor Management
-----------------

To explore factor management we will be looking at the `gapminder` dataset.

### Dropping a factor

Let's use `tidyverse` to drop *Oceania* from the **continents** factor. First, here is how the gapminder dataset looks like:

``` r
head(gapminder) %>%
  kable("html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", full_width = F))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
country
</th>
<th style="text-align:left;">
continent
</th>
<th style="text-align:right;">
year
</th>
<th style="text-align:right;">
lifeExp
</th>
<th style="text-align:right;">
pop
</th>
<th style="text-align:right;">
gdpPercap
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Afghanistan
</td>
<td style="text-align:left;">
Asia
</td>
<td style="text-align:right;">
1952
</td>
<td style="text-align:right;">
28.801
</td>
<td style="text-align:right;">
8425333
</td>
<td style="text-align:right;">
779.4453
</td>
</tr>
<tr>
<td style="text-align:left;">
Afghanistan
</td>
<td style="text-align:left;">
Asia
</td>
<td style="text-align:right;">
1957
</td>
<td style="text-align:right;">
30.332
</td>
<td style="text-align:right;">
9240934
</td>
<td style="text-align:right;">
820.8530
</td>
</tr>
<tr>
<td style="text-align:left;">
Afghanistan
</td>
<td style="text-align:left;">
Asia
</td>
<td style="text-align:right;">
1962
</td>
<td style="text-align:right;">
31.997
</td>
<td style="text-align:right;">
10267083
</td>
<td style="text-align:right;">
853.1007
</td>
</tr>
<tr>
<td style="text-align:left;">
Afghanistan
</td>
<td style="text-align:left;">
Asia
</td>
<td style="text-align:right;">
1967
</td>
<td style="text-align:right;">
34.020
</td>
<td style="text-align:right;">
11537966
</td>
<td style="text-align:right;">
836.1971
</td>
</tr>
<tr>
<td style="text-align:left;">
Afghanistan
</td>
<td style="text-align:left;">
Asia
</td>
<td style="text-align:right;">
1972
</td>
<td style="text-align:right;">
36.088
</td>
<td style="text-align:right;">
13079460
</td>
<td style="text-align:right;">
739.9811
</td>
</tr>
<tr>
<td style="text-align:left;">
Afghanistan
</td>
<td style="text-align:left;">
Asia
</td>
<td style="text-align:right;">
1977
</td>
<td style="text-align:right;">
38.438
</td>
<td style="text-align:right;">
14880372
</td>
<td style="text-align:right;">
786.1134
</td>
</tr>
</tbody>
</table>
``` r
dim(gapminder)
```

    ## [1] 1704    6

With dim() we see that the gapminder dataset has 1704 rows over 6 variables before we alter anything.

How many levels are there in the continent variable?

``` r
nlevels(gapminder$continent)
```

    ## [1] 5

``` r
levels(gapminder$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"   "Oceania"

The function nlevels() tells us that there are 5 levels in the factor continent, and levels() tells us that they are Africa, Americas, Asia, Europe, and Oceania.

We can use the filter() function to remove Oceania:

``` r
filterOc <- gapminder %>%
  filter(continent != "Oceania")
# check number of rows, cols, and levels after filtering
dim(filterOc)
```

    ## [1] 1680    6

``` r
levels(filterOc$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"   "Oceania"

``` r
nlevels(filterOc$continent)
```

    ## [1] 5

We see that after filtering out Oceania, we are left with 1680 rows, down from 1704. However, nlevels() and levels() tell us that Oceania still exists as a level in the *continents* factor.

There are situations where we would rather not have this unused level in the factor. We can get rid of it using the droplevels() function:

``` r
dropOc <- filterOc %>%
  droplevels()
nlevels(dropOc$continent)
```

    ## [1] 4

``` r
levels(dropOc$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"

Now nlevels() and levels() shows us that Oceania level has indeed been dropped from the continent factor.

### Reordering Levels with arrange() and the forcats package

The arrange() function and the forcats package allows us to change the order of factor levels. This is useful when making graphs, as you can then reorder levels in a logical way (e.g. ascending, descending, etc.).

Let's look at the mean life expectancy of each continent in 2007 from the gapminder dataset:

``` r
my_theme <- theme(axis.text=element_text(size=14, face="bold"),
        axis.title=element_text(size=14,face="bold"))

gapminder %>%
  filter(year == "2007") %>%
  group_by(continent) %>%
  summarize(muLE = mean(lifeExp)) %>% 
  ggplot(aes(continent, muLE)) +
  geom_col(aes(fill = continent)) +
  guides(fill=FALSE) +
  labs(x = "", y = "Life Expectancy in 2007") +
  theme_bw() +
  my_theme
```

![](hw05-factors_IO_and_plotly_files/figure-markdown_github/unnamed-chunk-5-1.png)

Although this bar plot correctly tells us the life expectancy of each continent in 2007, it would make a lot more sense to plot these in some logical order such as by descending life expectancy. Does arrange() do this?

``` r
gapminder %>%
  filter(year == "2007") %>%
  group_by(continent) %>%
  summarize(muLE = mean(lifeExp)) %>% 
  arrange(desc(muLE)) %>% #### arrange by descending mean life exp
  ggplot(aes(continent, muLE)) +
  geom_col(aes(fill = continent)) +
  guides(fill=FALSE) +
  labs(x = "", y = "Life Expectancy in 2007") +
  theme_bw() +
  my_theme
```

![](hw05-factors_IO_and_plotly_files/figure-markdown_github/unnamed-chunk-6-1.png)

We see that arrange() actually **does not** affect the figure, even though it will affect how it would be displayed in a table (below):

``` r
gapminder %>%
  filter(year == "2007") %>%
  group_by(continent) %>%
  summarize(muLE = mean(lifeExp)) %>%
  arrange(desc(muLE)) %>%
  kable("html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", full_width = F))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
continent
</th>
<th style="text-align:right;">
muLE
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Oceania
</td>
<td style="text-align:right;">
80.71950
</td>
</tr>
<tr>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
77.64860
</td>
</tr>
<tr>
<td style="text-align:left;">
Americas
</td>
<td style="text-align:right;">
73.60812
</td>
</tr>
<tr>
<td style="text-align:left;">
Asia
</td>
<td style="text-align:right;">
70.72848
</td>
</tr>
<tr>
<td style="text-align:left;">
Africa
</td>
<td style="text-align:right;">
54.80604
</td>
</tr>
</tbody>
</table>
The forcats package allows us to reorder factors that will actually affect the figure:

``` r
gapminder %>%
  filter(year == "2007") %>%
  group_by(continent) %>%
  summarize(muLE = mean(lifeExp)) %>% 
  mutate(continent = fct_reorder(continent, muLE)) %>% #### arrange by descending mean life exp, but using forcats package this time
  ggplot(aes(continent, muLE)) +
  geom_col(aes(fill = continent)) +
  guides(fill=FALSE) +
  labs(x = "", y = "Life Expectancy in 2007") +
  theme_bw() +
  my_theme
```

![](hw05-factors_IO_and_plotly_files/figure-markdown_github/unnamed-chunk-8-1.png)

We can use fct\_rev() to show descending mean life exp instead:

``` r
gapminder %>%
  filter(year == "2007") %>%
  group_by(continent) %>%
  summarize(muLE = mean(lifeExp)) %>% 
  mutate(continent = fct_reorder(continent, muLE)) %>% 
  mutate(continent = fct_rev(continent)) %>%
  ggplot(aes(continent, muLE)) +
  geom_col(aes(fill = continent)) +
  guides(fill=FALSE) +
  labs(x = "", y = "Life Expectancy in 2007") +
  theme_bw() +
  my_theme
```

![](hw05-factors_IO_and_plotly_files/figure-markdown_github/unnamed-chunk-9-1.png)

**Note** arrange() can reorder factors for presentation on a table, but fct\_reorder() will actually not change anything when presenting the data in a table!

File I/O
--------

### Writing files

We can write a variable into a comma-separated variables file using the write\_csv() function. Let's make a new dataset with the life expectancies of all countries in the Americas in 2007 and write it into a file called "america\_lifeexp\_2007.csv"

``` r
am_le_2007 <- gapminder %>%
  filter(continent == "Americas" & year == "2007") %>%
  select(country, lifeExp)
  
am_le_2007 %>%
  kable("html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", full_width = F))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
country
</th>
<th style="text-align:right;">
lifeExp
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:right;">
75.320
</td>
</tr>
<tr>
<td style="text-align:left;">
Bolivia
</td>
<td style="text-align:right;">
65.554
</td>
</tr>
<tr>
<td style="text-align:left;">
Brazil
</td>
<td style="text-align:right;">
72.390
</td>
</tr>
<tr>
<td style="text-align:left;">
Canada
</td>
<td style="text-align:right;">
80.653
</td>
</tr>
<tr>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:right;">
78.553
</td>
</tr>
<tr>
<td style="text-align:left;">
Colombia
</td>
<td style="text-align:right;">
72.889
</td>
</tr>
<tr>
<td style="text-align:left;">
Costa Rica
</td>
<td style="text-align:right;">
78.782
</td>
</tr>
<tr>
<td style="text-align:left;">
Cuba
</td>
<td style="text-align:right;">
78.273
</td>
</tr>
<tr>
<td style="text-align:left;">
Dominican Republic
</td>
<td style="text-align:right;">
72.235
</td>
</tr>
<tr>
<td style="text-align:left;">
Ecuador
</td>
<td style="text-align:right;">
74.994
</td>
</tr>
<tr>
<td style="text-align:left;">
El Salvador
</td>
<td style="text-align:right;">
71.878
</td>
</tr>
<tr>
<td style="text-align:left;">
Guatemala
</td>
<td style="text-align:right;">
70.259
</td>
</tr>
<tr>
<td style="text-align:left;">
Haiti
</td>
<td style="text-align:right;">
60.916
</td>
</tr>
<tr>
<td style="text-align:left;">
Honduras
</td>
<td style="text-align:right;">
70.198
</td>
</tr>
<tr>
<td style="text-align:left;">
Jamaica
</td>
<td style="text-align:right;">
72.567
</td>
</tr>
<tr>
<td style="text-align:left;">
Mexico
</td>
<td style="text-align:right;">
76.195
</td>
</tr>
<tr>
<td style="text-align:left;">
Nicaragua
</td>
<td style="text-align:right;">
72.899
</td>
</tr>
<tr>
<td style="text-align:left;">
Panama
</td>
<td style="text-align:right;">
75.537
</td>
</tr>
<tr>
<td style="text-align:left;">
Paraguay
</td>
<td style="text-align:right;">
71.752
</td>
</tr>
<tr>
<td style="text-align:left;">
Peru
</td>
<td style="text-align:right;">
71.421
</td>
</tr>
<tr>
<td style="text-align:left;">
Puerto Rico
</td>
<td style="text-align:right;">
78.746
</td>
</tr>
<tr>
<td style="text-align:left;">
Trinidad and Tobago
</td>
<td style="text-align:right;">
69.819
</td>
</tr>
<tr>
<td style="text-align:left;">
United States
</td>
<td style="text-align:right;">
78.242
</td>
</tr>
<tr>
<td style="text-align:left;">
Uruguay
</td>
<td style="text-align:right;">
76.384
</td>
</tr>
<tr>
<td style="text-align:left;">
Venezuela
</td>
<td style="text-align:right;">
73.747
</td>
</tr>
</tbody>
</table>
``` r
write_csv(am_le_2007, "america_lifeexp_2007.csv")
```

### Reading files

We can read a file into a variable using the read\_csv() function. Let's read the file we just created back into a variable called "data\_unordered":

``` r
data_unordered <- read_csv("america_lifeexp_2007.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   lifeExp = col_double()
    ## )

``` r
data_unordered %>%
  kable(col.names = c("Country", "Life Expectancy (Years)"), "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", full_width = F))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Country
</th>
<th style="text-align:right;">
Life Expectancy (Years)
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:right;">
75.320
</td>
</tr>
<tr>
<td style="text-align:left;">
Bolivia
</td>
<td style="text-align:right;">
65.554
</td>
</tr>
<tr>
<td style="text-align:left;">
Brazil
</td>
<td style="text-align:right;">
72.390
</td>
</tr>
<tr>
<td style="text-align:left;">
Canada
</td>
<td style="text-align:right;">
80.653
</td>
</tr>
<tr>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:right;">
78.553
</td>
</tr>
<tr>
<td style="text-align:left;">
Colombia
</td>
<td style="text-align:right;">
72.889
</td>
</tr>
<tr>
<td style="text-align:left;">
Costa Rica
</td>
<td style="text-align:right;">
78.782
</td>
</tr>
<tr>
<td style="text-align:left;">
Cuba
</td>
<td style="text-align:right;">
78.273
</td>
</tr>
<tr>
<td style="text-align:left;">
Dominican Republic
</td>
<td style="text-align:right;">
72.235
</td>
</tr>
<tr>
<td style="text-align:left;">
Ecuador
</td>
<td style="text-align:right;">
74.994
</td>
</tr>
<tr>
<td style="text-align:left;">
El Salvador
</td>
<td style="text-align:right;">
71.878
</td>
</tr>
<tr>
<td style="text-align:left;">
Guatemala
</td>
<td style="text-align:right;">
70.259
</td>
</tr>
<tr>
<td style="text-align:left;">
Haiti
</td>
<td style="text-align:right;">
60.916
</td>
</tr>
<tr>
<td style="text-align:left;">
Honduras
</td>
<td style="text-align:right;">
70.198
</td>
</tr>
<tr>
<td style="text-align:left;">
Jamaica
</td>
<td style="text-align:right;">
72.567
</td>
</tr>
<tr>
<td style="text-align:left;">
Mexico
</td>
<td style="text-align:right;">
76.195
</td>
</tr>
<tr>
<td style="text-align:left;">
Nicaragua
</td>
<td style="text-align:right;">
72.899
</td>
</tr>
<tr>
<td style="text-align:left;">
Panama
</td>
<td style="text-align:right;">
75.537
</td>
</tr>
<tr>
<td style="text-align:left;">
Paraguay
</td>
<td style="text-align:right;">
71.752
</td>
</tr>
<tr>
<td style="text-align:left;">
Peru
</td>
<td style="text-align:right;">
71.421
</td>
</tr>
<tr>
<td style="text-align:left;">
Puerto Rico
</td>
<td style="text-align:right;">
78.746
</td>
</tr>
<tr>
<td style="text-align:left;">
Trinidad and Tobago
</td>
<td style="text-align:right;">
69.819
</td>
</tr>
<tr>
<td style="text-align:left;">
United States
</td>
<td style="text-align:right;">
78.242
</td>
</tr>
<tr>
<td style="text-align:left;">
Uruguay
</td>
<td style="text-align:right;">
76.384
</td>
</tr>
<tr>
<td style="text-align:left;">
Venezuela
</td>
<td style="text-align:right;">
73.747
</td>
</tr>
</tbody>
</table>
This table may look more presentable if we order the countries by highest to lowest life expectancy. Let's do that and then save it into another file called "america\_lifeexp\_2007\_ordered.csv":

``` r
data_ordered <- data_unordered %>%
  arrange(desc(lifeExp)) 

data_ordered %>%
  kable("html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", full_width = F))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
country
</th>
<th style="text-align:right;">
lifeExp
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Canada
</td>
<td style="text-align:right;">
80.653
</td>
</tr>
<tr>
<td style="text-align:left;">
Costa Rica
</td>
<td style="text-align:right;">
78.782
</td>
</tr>
<tr>
<td style="text-align:left;">
Puerto Rico
</td>
<td style="text-align:right;">
78.746
</td>
</tr>
<tr>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:right;">
78.553
</td>
</tr>
<tr>
<td style="text-align:left;">
Cuba
</td>
<td style="text-align:right;">
78.273
</td>
</tr>
<tr>
<td style="text-align:left;">
United States
</td>
<td style="text-align:right;">
78.242
</td>
</tr>
<tr>
<td style="text-align:left;">
Uruguay
</td>
<td style="text-align:right;">
76.384
</td>
</tr>
<tr>
<td style="text-align:left;">
Mexico
</td>
<td style="text-align:right;">
76.195
</td>
</tr>
<tr>
<td style="text-align:left;">
Panama
</td>
<td style="text-align:right;">
75.537
</td>
</tr>
<tr>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:right;">
75.320
</td>
</tr>
<tr>
<td style="text-align:left;">
Ecuador
</td>
<td style="text-align:right;">
74.994
</td>
</tr>
<tr>
<td style="text-align:left;">
Venezuela
</td>
<td style="text-align:right;">
73.747
</td>
</tr>
<tr>
<td style="text-align:left;">
Nicaragua
</td>
<td style="text-align:right;">
72.899
</td>
</tr>
<tr>
<td style="text-align:left;">
Colombia
</td>
<td style="text-align:right;">
72.889
</td>
</tr>
<tr>
<td style="text-align:left;">
Jamaica
</td>
<td style="text-align:right;">
72.567
</td>
</tr>
<tr>
<td style="text-align:left;">
Brazil
</td>
<td style="text-align:right;">
72.390
</td>
</tr>
<tr>
<td style="text-align:left;">
Dominican Republic
</td>
<td style="text-align:right;">
72.235
</td>
</tr>
<tr>
<td style="text-align:left;">
El Salvador
</td>
<td style="text-align:right;">
71.878
</td>
</tr>
<tr>
<td style="text-align:left;">
Paraguay
</td>
<td style="text-align:right;">
71.752
</td>
</tr>
<tr>
<td style="text-align:left;">
Peru
</td>
<td style="text-align:right;">
71.421
</td>
</tr>
<tr>
<td style="text-align:left;">
Guatemala
</td>
<td style="text-align:right;">
70.259
</td>
</tr>
<tr>
<td style="text-align:left;">
Honduras
</td>
<td style="text-align:right;">
70.198
</td>
</tr>
<tr>
<td style="text-align:left;">
Trinidad and Tobago
</td>
<td style="text-align:right;">
69.819
</td>
</tr>
<tr>
<td style="text-align:left;">
Bolivia
</td>
<td style="text-align:right;">
65.554
</td>
</tr>
<tr>
<td style="text-align:left;">
Haiti
</td>
<td style="text-align:right;">
60.916
</td>
</tr>
</tbody>
</table>
``` r
write_csv(data_ordered, "america_lifeexp_2007_ordered.csv")
```

### Does arranging the factor levels with arrange() survive a R/W cycle?

Let's read back our ordered table and see if it is still in order of descending life expectancy:

``` r
read_arranged <- read_csv("america_lifeexp_2007_ordered.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   lifeExp = col_double()
    ## )

``` r
read_arranged %>%
  kable("html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", full_width = F))
```

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
country
</th>
<th style="text-align:right;">
lifeExp
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Canada
</td>
<td style="text-align:right;">
80.653
</td>
</tr>
<tr>
<td style="text-align:left;">
Costa Rica
</td>
<td style="text-align:right;">
78.782
</td>
</tr>
<tr>
<td style="text-align:left;">
Puerto Rico
</td>
<td style="text-align:right;">
78.746
</td>
</tr>
<tr>
<td style="text-align:left;">
Chile
</td>
<td style="text-align:right;">
78.553
</td>
</tr>
<tr>
<td style="text-align:left;">
Cuba
</td>
<td style="text-align:right;">
78.273
</td>
</tr>
<tr>
<td style="text-align:left;">
United States
</td>
<td style="text-align:right;">
78.242
</td>
</tr>
<tr>
<td style="text-align:left;">
Uruguay
</td>
<td style="text-align:right;">
76.384
</td>
</tr>
<tr>
<td style="text-align:left;">
Mexico
</td>
<td style="text-align:right;">
76.195
</td>
</tr>
<tr>
<td style="text-align:left;">
Panama
</td>
<td style="text-align:right;">
75.537
</td>
</tr>
<tr>
<td style="text-align:left;">
Argentina
</td>
<td style="text-align:right;">
75.320
</td>
</tr>
<tr>
<td style="text-align:left;">
Ecuador
</td>
<td style="text-align:right;">
74.994
</td>
</tr>
<tr>
<td style="text-align:left;">
Venezuela
</td>
<td style="text-align:right;">
73.747
</td>
</tr>
<tr>
<td style="text-align:left;">
Nicaragua
</td>
<td style="text-align:right;">
72.899
</td>
</tr>
<tr>
<td style="text-align:left;">
Colombia
</td>
<td style="text-align:right;">
72.889
</td>
</tr>
<tr>
<td style="text-align:left;">
Jamaica
</td>
<td style="text-align:right;">
72.567
</td>
</tr>
<tr>
<td style="text-align:left;">
Brazil
</td>
<td style="text-align:right;">
72.390
</td>
</tr>
<tr>
<td style="text-align:left;">
Dominican Republic
</td>
<td style="text-align:right;">
72.235
</td>
</tr>
<tr>
<td style="text-align:left;">
El Salvador
</td>
<td style="text-align:right;">
71.878
</td>
</tr>
<tr>
<td style="text-align:left;">
Paraguay
</td>
<td style="text-align:right;">
71.752
</td>
</tr>
<tr>
<td style="text-align:left;">
Peru
</td>
<td style="text-align:right;">
71.421
</td>
</tr>
<tr>
<td style="text-align:left;">
Guatemala
</td>
<td style="text-align:right;">
70.259
</td>
</tr>
<tr>
<td style="text-align:left;">
Honduras
</td>
<td style="text-align:right;">
70.198
</td>
</tr>
<tr>
<td style="text-align:left;">
Trinidad and Tobago
</td>
<td style="text-align:right;">
69.819
</td>
</tr>
<tr>
<td style="text-align:left;">
Bolivia
</td>
<td style="text-align:right;">
65.554
</td>
</tr>
<tr>
<td style="text-align:left;">
Haiti
</td>
<td style="text-align:right;">
60.916
</td>
</tr>
</tbody>
</table>
We see that indeed it does! What if we had reordered using the forcats package?

### Does reordering levels using the forcats package survive a R/W cycle?

``` r
data_order_fct <- data_unordered %>%
  mutate(country = fct_reorder(country, lifeExp))

data_order_fct %>%
  ggplot(aes(lifeExp, country)) +
  geom_point() +
  guides(fill=FALSE) +
  labs(x = "Life Expectancy in 2007 (Years)", y = "") +
  theme_bw() +
  my_theme
```

![](hw05-factors_IO_and_plotly_files/figure-markdown_github/unnamed-chunk-14-1.png)

We check to see that indeed the plot above has countries ordered in descending life expectancy.

``` r
write_csv(data_order_fct, "ordered_with_fct.csv")
read_order_fct <- read_csv("ordered_with_fct.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   lifeExp = col_double()
    ## )

``` r
read_order_fct %>%
  ggplot(aes(lifeExp, country)) +
  geom_point() +
  guides(fill=FALSE) +
  labs(x = "Life Expectancy in 2007 (Years)", y = "") +
  theme_bw() +
  my_theme
```

![](hw05-factors_IO_and_plotly_files/figure-markdown_github/unnamed-chunk-15-1.png)

Here we see that the reordering of factors using fct\_reorder() actually does not persist after a read/write cycle!

Visualization design
--------------------

### Old Plot

Let's use what we learned so far to remake an old figure. I have chosen a figure I made from Assignemnt 1:

``` r
plot(gapminder$pop, gapminder$lifeExp,
     xlab="Population (persons)",
     ylab="Life Expectancy (years)")
```

![](hw05-factors_IO_and_plotly_files/figure-markdown_github/unnamed-chunk-16-1.png)

After a month and a half of STAT545, I can see that this figure is both quite plain and quite bad in terms of presenting any meaningful conclusions! In addition, I see that I used base R to generate this plot, where as ggplot is much easier to use and has much more readable code! Let's remake this figure.

### New Plot

The reasoning behind the above figure is not very good. We have life expectancy for every single year in the gapminder dataset plotted, which is not very meaningful and is leading to overplotting. Let's just plot the data from 2007. Let's also put population on the y axis instead:

``` r
p <- gapminder %>%
  filter(year == "2007") %>%
  ggplot(aes(lifeExp, pop)) +
  geom_point(aes(colour = continent)) +
  scale_y_log10(labels=comma_format()) +
  labs(y = "Population in 2007", x = "Life Expectancy (Years)") +
  scale_colour_discrete(name = "Continent") + # Capitalization!
  theme_bw() +
  my_theme
p
```

![](hw05-factors_IO_and_plotly_files/figure-markdown_github/unnamed-chunk-17-1.png)

I think this looks **much** better than before; now it looks like something I would be proud to present on a poster!

### Plotly

Plotly allows us to insert some basic interactivity into our plots. Let's do this for the one we just made above!

``` r
p %>%
  ggplotly() %>%
  htmlwidgets::saveWidget("pop_vs_lifeexp_2007_plotly.html")
```

Opening up the html file "pop\_vs\_lifeexp\_2007\_plotly.html", we see that plotly gives us a lot of interesting functionality such as the ability to zoom in, and hovering over points will give us useful information about that point such as the country it belongs to, as well as the exact population and life expectancy value (something which would be hard to glean from the graph visually.)

Writing figures to file
-----------------------

Using RStudio's *Export image* feature is not very reproducible. Instead, we should use ggsave(). Let's save the plot of Population vs. Life Expectancy in 2007 to a file called "pop\_vs\_lifeexp\_2007.png":

``` r
# ggsave saves the last plot displayed, but we can specify exactly which plot we want to save using the "p" option. This is useful if we don't want to save the last plot we saved, but a version made previously

ggsave("pop_vs_lifeexp_2007.png", plot = p, scale = 1, width = 8, height = 6, units = "in")
```

We can then load the image using `![Alt text](/image_path)` in Markdown language:

![Population and Life Expectancy in 2007 from the Gapminder dataset](C:\Users\Kiko0\Desktop\git_docs\hw05-acheng-ubc\pop_vs_lifeexp_2007.png)
