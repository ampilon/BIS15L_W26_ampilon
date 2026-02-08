---
title: "Homework 8"
author: "Your Name Here"
date: "2026-02-04"
output:
  html_document: 
    theme: spacelab
    keep_md: yes
---

## Instructions
Answer the following questions and/or complete the exercises in RMarkdown. Please embed all of your code and push the final work to your repository. Your report should be organized, clean, and run free from errors. Remember, you must remove the `#` for any included code chunks to run.  

## Load the libraries

``` r
library("tidyverse")
library("janitor")
#library("naniar")
options(scipen = 999)
```

## About the Data
For this assignment we are going to work with a data set from the [United Nations Food and Agriculture Organization](https://www.fao.org/fishery/en/collection/capture) on world fisheries. These data were downloaded and cleaned using the `fisheries_clean.Rmd` script.  

Load the data `fisheries_clean.csv` as a new object titled `fisheries_clean`.

``` r
fisheries_clean <- read_csv("data/fisheries_clean.csv")
```

1. Explore the data. What are the names of the variables, what are the dimensions, are there any NA's, what are the classes of the variables, etc.? You may use the functions that you prefer.

``` r
names(fisheries_clean)
```

```
## [1] "period"          "continent"       "geo_region"      "country"        
## [5] "scientific_name" "common_name"     "taxonomic_code"  "catch"          
## [9] "status"
```

``` r
dim(fisheries_clean)
```

```
## [1] 1055015       9
```

``` r
glimpse(fisheries_clean)
```

```
## Rows: 1,055,015
## Columns: 9
## $ period          <dbl> 1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, …
## $ continent       <chr> "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia"…
## $ geo_region      <chr> "Southern Asia", "Southern Asia", "Southern Asia", "So…
## $ country         <chr> "Afghanistan", "Afghanistan", "Afghanistan", "Afghanis…
## $ scientific_name <chr> "Osteichthyes", "Osteichthyes", "Osteichthyes", "Ostei…
## $ common_name     <chr> "Freshwater fishes NEI", "Freshwater fishes NEI", "Fre…
## $ taxonomic_code  <chr> "1990XXXXXXXX106", "1990XXXXXXXX106", "1990XXXXXXXX106…
## $ catch           <dbl> 100, 100, 100, 100, 100, 200, 200, 200, 200, 200, 200,…
## $ status          <chr> "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",…
```

``` r
sum(is.na(fisheries_clean))
```

```
## [1] 50468
```

2. Convert the following variables to factors: `period`, `continent`, `geo_region`, `country`, `scientific_name`, `common_name`, `taxonomic_code`, and `status`.

``` r
fish_final <- fisheries_clean %>%
  mutate(period= as.factor(period),
         continent= as.factor(continent),
         geo_region= as.factor(geo_region),
         country= as.factor(country),
         scientific_name= as.factor(scientific_name),
         common_name= as.factor(common_name),
         taxonomic_code= as.factor(taxonomic_code),
         status= as.factor(status))
```

3. Are there any missing values in the data? If so, which variables contain missing values and how many are missing for each variable?

``` r
#fish_final%>%
 # filter(across(c(period, continent, geo_region, country, scientific_name, common_name, taxonomic_code, catch, status)), sum(is.na()))
```

4. How many countries are represented in the data?

``` r
fish_final %>%
  distinct(country)
```

```
## # A tibble: 249 × 1
##    country            
##    <fct>              
##  1 Afghanistan        
##  2 Albania            
##  3 Algeria            
##  4 American Samoa     
##  5 Andorra            
##  6 Angola             
##  7 Anguilla           
##  8 Antigua and Barbuda
##  9 Argentina          
## 10 Armenia            
## # ℹ 239 more rows
```

``` r
# 249 countries
```

5. The variables `common_name` and `taxonomic_code` both refer to species. How many unique species are represented in the data based on each of these variables? Are the numbers the same or different?

``` r
fish_final %>%
  distinct(common_name)
```

```
## # A tibble: 3,390 × 1
##    common_name            
##    <fct>                  
##  1 Freshwater fishes NEI  
##  2 Crucian carp           
##  3 Common carp            
##  4 Grass carp(=White amur)
##  5 Silver carp            
##  6 Bighead carp           
##  7 Wuchang bream          
##  8 Bleak                  
##  9 Orfe(=Ide)             
## 10 Common dace            
## # ℹ 3,380 more rows
```

``` r
fish_final %>%
  distinct(taxonomic_code)
```

```
## # A tibble: 3,722 × 1
##    taxonomic_code 
##    <fct>          
##  1 1990XXXXXXXX106
##  2 140014109002   
##  3 140014113401   
##  4 140018102601   
##  5 140018104601   
##  6 140018104602   
##  7 140018105801   
##  8 140023102602   
##  9 140023114204   
## 10 140023114205   
## # ℹ 3,712 more rows
```

``` r
#the numbers are different there are 3722 taxonomic codes and 3390 common names.
```
6. In 2023, what were the top five countries that had the highest overall catch?

``` r
fish_final %>%
  select(period, catch, country)%>%
  filter(period== "2023") %>%
  group_by(country) %>%
  summarise(totall_catch = sum(catch, na.rm = TRUE)) %>%
  arrange(desc(totall_catch))%>%
  slice_head(n=5)
```

```
## # A tibble: 5 × 2
##   country                  totall_catch
##   <fct>                           <dbl>
## 1 China                       13424705.
## 2 Indonesia                    7820833.
## 3 India                        6177985.
## 4 Russian Federation           5398032 
## 5 United States of America     4623694
```

``` r
#China, Indonesia, India, Russian Federation, United States of America
```

7. In 2023, what were the top 10 most caught species? To keep things simple, assume `common_name` is sufficient to identify species. What does `NEI` stand for in some of the common names? How might this be concerning from a fisheries management perspective?

``` r
fish_final %>%
  filter(period== "2023") %>%
  group_by(common_name) %>%
  summarise(total_catch = sum(catch, na.rm = TRUE)) %>%
  arrange(desc(total_catch)) %>%
  slice_head(n=10)
```

```
## # A tibble: 10 × 2
##    common_name                    total_catch
##    <fct>                                <dbl>
##  1 Marine fishes NEI                 8553907.
##  2 Freshwater fishes NEI             5880104.
##  3 Alaska pollock(=Walleye poll.)    3543411.
##  4 Skipjack tuna                     2954736.
##  5 Anchoveta(=Peruvian anchovy)      2415709.
##  6 Blue whiting(=Poutassou)          1739484.
##  7 Pacific sardine                   1678237.
##  8 Yellowfin tuna                    1601369.
##  9 Atlantic herring                  1432807.
## 10 Scads NEI                         1344190.
```

``` r
# NEI are fish species that can not be identified individually
```

8. For the species that was caught the most above (not NEI), which country had the highest catch in 2023?

``` r
fish_final %>%
  filter(common_name== "Alaska pollock(=Walleye poll.)", period== "2023") %>%
  group_by(country) %>%
  summarise(total_catch = sum(catch, na.rm = TRUE)) %>%
  arrange(desc(total_catch)) %>%
  slice_head(n=1)
```

```
## # A tibble: 1 × 2
##   country            total_catch
##   <fct>                    <dbl>
## 1 Russian Federation     1893924
```

9. How has fishing of this species changed over the last decade (2013-2023)? Create a  plot showing total catch by year for this species.

``` r
alaska_fish_over_decade <- fish_final %>%
  filter(common_name== "Alaska pollock(=Walleye poll.)") %>%
  group_by(period) %>%
  summarise(total_catch = sum(catch, na.rm = T)) %>%
  arrange(desc(period)) %>%
  slice_head(n=11)
```

``` r
alaska_fish_over_decade %>%
  ggplot(mapping = aes(x= period, y=total_catch))+
  geom_point()+ 
  labs(title="Allaska Pollock catch from 2013-2023", x="year", y="catch count")+
  geom_smooth(method="gam", se=F)
```

```
## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'
```

![](hw8_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

``` r
#fishing of this species has increased
```
10. Perform one exploratory analysis of your choice. Make sure to clearly state the question you are asking before writing any code.

``` r
# which geo region caught the most fish in 1975?
fish_final %>%
  filter(period== "1975") %>%
  group_by(geo_region) %>%
  summarise(total_catch = sum(catch, na.rm = TRUE)) %>%
  arrange(desc(total_catch)) %>%
  slice_head(n=1)
```

```
## # A tibble: 1 × 2
##   geo_region   total_catch
##   <fct>              <dbl>
## 1 Eastern Asia    16378684
```

``` r
# The geo region Eastern Asia caught the most fish in 1975
```

## Knit and Upload
Please knit your work as an .html file and upload to Canvas. Homework is due before the start of the next lab. No late work is accepted. Make sure to use the formatting conventions of RMarkdown to make your report neat and clean!  
