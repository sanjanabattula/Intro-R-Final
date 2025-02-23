IntrotoR_Final
================
Sanjana Battula

2023-10-19

1.  Please answer the following questions regarding the
    us_contagious_diseases dataset in dslabs package. (a). How many
    observations and variables are there? What are the types of each
    variable?

``` r
library(dslabs)
# Loading dataset
data("us_contagious_diseases") 
# Number of observations 
nrow(us_contagious_diseases)
```

    ## [1] 16065

``` r
# Number of variables
ncol(us_contagious_diseases)
```

    ## [1] 6

``` r
sapply(us_contagious_diseases, typeof)
```

    ##         disease           state            year weeks_reporting           count 
    ##       "integer"       "integer"        "double"        "double"        "double" 
    ##      population 
    ##        "double"

(b). Are there missing data? If yes, which variable(s) contain missing
data, and how many missing data values?

``` r
missing_data <- colSums(is.na(us_contagious_diseases))
print(missing_data)
```

    ##         disease           state            year weeks_reporting           count 
    ##               0               0               0               0               0 
    ##      population 
    ##             214

**There is a total of 214 missing entries in the population column.**

(c). Compute the frequency of each type of disease and visualize the
proportion using barchart.

``` r
library(ggplot2)

ggplot(us_contagious_diseases, aes(x = disease)) +
  geom_bar() +
  labs(title = "Frequency of Contagious Diseases", x = "Disease", y = "Frequency") 
```

![](<IntroR_Final_files/Plots/unnamed-chunk-3-1.png>)
(d).
Computer the 0.1, 0.5, 0.9 quantiles of the population for each type of
disease. Write a paragraph to compare the quantiles of the population
across the diseases.

``` r
library(dslabs)
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
# Define a custom function to compute quantiles while handling missing values
compute_quantiles <- function(x) {
  quantile(x, c(0.1, 0.5, 0.9), na.rm = TRUE)
}

# Compute quantiles for each disease
quantiles_df <- us_contagious_diseases %>%
  group_by(disease) %>%
  summarise(Q10 = compute_quantiles(population)[1],
            Q50 = compute_quantiles(population)[2],
            Q90 = compute_quantiles(population)[3])

# Print the quantiles
print(quantiles_df)
```

    ## # A tibble: 7 × 4
    ##   disease         Q10      Q50       Q90
    ##   <fct>         <dbl>    <dbl>     <dbl>
    ## 1 Hepatitis A 654776. 3294476. 11431841 
    ## 2 Measles     564659  2576878   9373370 
    ## 3 Mumps       643694. 3184352  11377152.
    ## 4 Pertussis   619278. 2901225  10742506.
    ## 5 Polio       479734. 2180049   7400511 
    ## 6 Rubella     642682. 3146641  11353650.
    ## 7 Smallpox    463827. 2002299   6634637.

**When comparing these diseases based on their quantiles, several trends
become apparent. Hepatitis A exhibits the highest median (Q50)
population impact, with a value of approximately 3,294,476, suggesting a
substantial median burden on the population. Mumps and Rubella also
display elevated median impacts, similar to Hepatitis A. In contrast,
diseases such as Polio and Smallpox have lower median impacts, implying
a comparatively milder median burden. Furthermore, the 10th percentile
(Q10) values demonstrate that even at the lower end of the impact
spectrum, Hepatitis A, Mumps, and Rubella can have substantial effects,
while Polio and Smallpox remain relatively low. The 90th percentile
(Q90) values reveal the potential for high population impacts, with
Hepatitis A, Mumps, and Rubella again standing out. In summary, the 10th
and 90th percentiles further illustrate the variability in the potential
impact of these diseases across different populations.**

2.  Find the top 3 states with the most “Mumps” cases over the 10 years
    from 1991 to 2000 (both years inclusive). Find the bottom 3 states
    with the least Hepatitis A cases over the 5 years from 1994 to 1998
    (both years inclusive).

``` r
# Filter the dataset for the relevant years and diseases
mumps_data <- us_contagious_diseases %>%
  filter(year >= 1991, year <= 2000, disease == "Mumps")

hepatitis_a_data <- us_contagious_diseases %>%
  filter(year >= 1994, year <= 1998, disease == "Hepatitis A")

# Find the top 3 states with the most Mumps cases
top_mumps_states <- mumps_data %>%
  group_by(state) %>%
  summarise(total_cases = sum(count)) %>%
  arrange(desc(total_cases)) %>%
  head(3)

# Find the bottom 3 states with the least Hepatitis A cases
bottom_hepatitis_a_states <- hepatitis_a_data %>%
  group_by(state) %>%
  summarise(total_cases = sum(count)) %>%
  arrange(total_cases) %>%
  head(3)
# Print the results
print(top_mumps_states)
```

    ## # A tibble: 3 × 2
    ##   state      total_cases
    ##   <fct>            <dbl>
    ## 1 California        1516
    ## 2 Texas             1416
    ## 3 Florida            853

``` r
print(bottom_hepatitis_a_states)
```

    ## # A tibble: 3 × 2
    ##   state         total_cases
    ##   <fct>               <dbl>
    ## 1 Vermont                33
    ## 2 West Virginia          52
    ## 3 Delaware               54

The top 3 states with the most Mumps cases between 1991-2000 are
**California**, **Texas** and **Florida.** The bottom 3 states with the
least Hepatitis A cases from 1994-1998 are **Vermont**, **West
Virginia** and **Delaware.**

3.  For the State of Texas,

<!-- -->

1)  Add a variable ave_count, representing the average reported case
    count per weeks_reporting for each year

``` r
library(dplyr)
# Filter the dataset for the State of Texas
texas_data <- us_contagious_diseases %>% filter(state == "Texas")

# Group the data by year and calculate the average case count per weeks_reporting
texas_data <- texas_data %>%
  group_by(year, disease) %>%
  mutate(ave_count = mean(count / weeks_reporting, na.rm = TRUE))

# View the updated dataset with the 'ave_count' variable
print(texas_data)
```

    ## # A tibble: 315 × 7
    ## # Groups:   year, disease [315]
    ##    disease     state  year weeks_reporting count population ave_count
    ##    <fct>       <fct> <dbl>           <dbl> <dbl>      <dbl>     <dbl>
    ##  1 Hepatitis A Texas  1966              52  1808   10470937      34.8
    ##  2 Hepatitis A Texas  1967              51  2727   10628322      53.5
    ##  3 Hepatitis A Texas  1968              50  2190   10798697      43.8
    ##  4 Hepatitis A Texas  1969              50  2312   10986554      46.2
    ##  5 Hepatitis A Texas  1970              51  2741   11196730      53.7
    ##  6 Hepatitis A Texas  1971              51  3731   11433080      73.2
    ##  7 Hepatitis A Texas  1972              46  3407   11694123      74.1
    ##  8 Hepatitis A Texas  1973              48  4569   11976810      95.2
    ##  9 Hepatitis A Texas  1974              43  3200   12277800      74.4
    ## 10 Hepatitis A Texas  1975              49  2845   12593389      58.1
    ## # ℹ 305 more rows

2)  Plot ave_count against the year while using different colors for
    different diseases.

``` r
ggplot(texas_data, aes(x = year, y = ave_count, color = disease)) +
  geom_point() + geom_smooth()
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 4 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 4 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](<IntroR_Final_files/Plots/unnamed-chunk-7-1.png>)

``` r
  labs(x = "Year", y = "Average Reported Case Count per Weeks Reporting") +
  scale_color_discrete(name = "Disease")
```

    ## NULL

3)  Remove all the observations for disease “Measles” and redo the plot
    in (b).

``` r
# Filter out observations with disease "Measles"
texas_data_filtered <- texas_data %>% filter(disease != "Measles")

# Create a plot using ggplot2 with the filtered dataset
ggplot(texas_data_filtered, aes(x = year, y = ave_count, color = disease)) +
  geom_point() + geom_smooth()
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 2 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 2 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](<IntroR_Final_files/Plots/unnamed-chunk-8-1.png>)

``` r
  labs(x = "Year", y = "Average Reported Case Count per Weeks Reporting") +
  scale_color_discrete(name = "Disease")
```

    ## NULL

4.  Redo Problem 3 for the State of New York. Write a paragraph to
    compare the results of the two states. The paragraph should clearly
    describe your findings for each state, and how your findings differ
    in the two states. Also provide a reason why the two states are
    different. The reason can be subjective only reflecting your own
    opinion.

``` r
# Filter the dataset for the State of Texas
new_york_data <- us_contagious_diseases %>% filter(state == "New York")

# Group the data by year and calculate the average case count per weeks_reporting
new_york_data <- new_york_data %>%
  group_by(year, disease) %>%
  mutate(ave_count = mean(count / weeks_reporting, na.rm = TRUE))

# View the updated dataset with the 'ave_count' variable
print(new_york_data)
```

    ## # A tibble: 315 × 7
    ## # Groups:   year, disease [315]
    ##    disease     state     year weeks_reporting count population ave_count
    ##    <fct>       <fct>    <dbl>           <dbl> <dbl>      <dbl>     <dbl>
    ##  1 Hepatitis A New York  1966              50  2435   17895985      48.7
    ##  2 Hepatitis A New York  1967              52  3394   18025684      65.3
    ##  3 Hepatitis A New York  1968              52  3728   18128492      71.7
    ##  4 Hepatitis A New York  1969              49  3976   18200269      81.1
    ##  5 Hepatitis A New York  1970              51  5024   18236967      98.5
    ##  6 Hepatitis A New York  1971              50  4825   18236388      96.5
    ##  7 Hepatitis A New York  1972              46  3438   18203314      74.7
    ##  8 Hepatitis A New York  1973              47  2821   18144367      60.0
    ##  9 Hepatitis A New York  1974              46  2193   18066218      47.7
    ## 10 Hepatitis A New York  1975              49  1932   17975503      39.4
    ## # ℹ 305 more rows

``` r
# Plot
ggplot(new_york_data, aes(x = year, y = ave_count, color = disease)) +
  geom_point() + geom_smooth()
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 22 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 22 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](<IntroR_Final_files/Plots/unnamed-chunk-9-1.png>)

``` r
# Plot removing Mumps disease
# Filter out observations with disease "Measles"
new_york_data_filtered <- new_york_data %>% filter(disease != "Measles")

# Create a plot using ggplot2 with the filtered dataset
ggplot(new_york_data_filtered, aes(x = year, y = ave_count, color = disease)) +
  geom_point() + geom_smooth()
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 19 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 19 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](IntroR_Final_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->
**Comparing the data for both states Texas and New York the plots show
variations in average count for each disease. When Measles is filtered
out the plots are magnified and we can see up close the trends of data.
Pertussis and Mumps seem to have the most counts in the state of Texas
after Measles. While in New York it is Pertussis.** **The differences in
data can be influences to variations in factors like population density,
vaccination rates, climate, healthcare infrastructure or even public
health measures like quarantine.**

5.  For each state and year, find

<!-- -->

1)  The total count of all diseases for the given state and year.

``` r
# Calculate the total count of all diseases for each state and year
total_count_by_state_year <- us_contagious_diseases %>%
  group_by(state, year) %>%
  summarise(total_count = sum(count, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'state'. You can override using the
    ## `.groups` argument.

``` r
# View the result
print(total_count_by_state_year)
```

    ## # A tibble: 4,284 × 3
    ## # Groups:   state [51]
    ##    state    year total_count
    ##    <fct>   <dbl>       <dbl>
    ##  1 Alabama  1928        9246
    ##  2 Alabama  1929        3395
    ##  3 Alabama  1930        4415
    ##  4 Alabama  1931        9283
    ##  5 Alabama  1932         772
    ##  6 Alabama  1933        1845
    ##  7 Alabama  1934       15922
    ##  8 Alabama  1935        7317
    ##  9 Alabama  1936        1002
    ## 10 Alabama  1937         758
    ## # ℹ 4,274 more rows

2)  The disease count density, which is defined by the total count of
    all diseases divided by the population for the given state and year.

``` r
# Calculate the disease count density for each state and year
disease_count_density <- us_contagious_diseases %>%
  group_by(state, year) %>%
  summarise(density = sum(count) / sum(population))
```

    ## `summarise()` has grouped output by 'state'. You can override using the
    ## `.groups` argument.

``` r
# View the result
print(disease_count_density)
```

    ## # A tibble: 4,284 × 3
    ## # Groups:   state [51]
    ##    state    year   density
    ##    <fct>   <dbl>     <dbl>
    ##  1 Alabama  1928 0.00119  
    ##  2 Alabama  1929 0.000432 
    ##  3 Alabama  1930 0.000556 
    ##  4 Alabama  1931 0.00116  
    ##  5 Alabama  1932 0.0000956
    ##  6 Alabama  1933 0.000227 
    ##  7 Alabama  1934 0.00194  
    ##  8 Alabama  1935 0.000887 
    ##  9 Alabama  1936 0.000121 
    ## 10 Alabama  1937 0.0000908
    ## # ℹ 4,274 more rows

6.  Find

<!-- -->

1)  the three (state, year) pairs that have the largest total count of
    all diseases.

``` r
# Calculate the total count of all diseases for each (state, year) pair
total_count_by_state_year <- us_contagious_diseases %>%
  group_by(state, year) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count))
```

    ## `summarise()` has grouped output by 'state'. You can override using the
    ## `.groups` argument.

``` r
# Get the top three (state, year) pairs with the largest total count of all diseases
top_total_count <- head(total_count_by_state_year, 3)
# Print the results
print(top_total_count)
```

    ## # A tibble: 3 × 3
    ## # Groups:   state [2]
    ##   state         year total_count
    ##   <fct>        <dbl>       <dbl>
    ## 1 Pennsylvania  1938      146097
    ## 2 New York      1941      123598
    ## 3 Pennsylvania  1941      116071

2)  the three (state, year) pairs that have the largest disease count
    density. \# remove NA; try and fix repeats; no sum for population

``` r
 library(dplyr)
# Calculate the disease count density for each (state, year) pair
disease_count_density <- us_contagious_diseases %>%
  group_by(state, year, population) %>%
  summarise(density = sum(count) / sum(population)) %>%
  arrange(desc(density))
```

    ## `summarise()` has grouped output by 'state', 'year'. You can override using the
    ## `.groups` argument.

``` r
# Get the top three (state, year) pairs with the largest disease count density
top_density <- head(disease_count_density, 3)
print(top_density)
```

    ## # A tibble: 3 × 4
    ## # Groups:   state, year [3]
    ##   state      year population density
    ##   <fct>     <dbl>      <dbl>   <dbl>
    ## 1 Vermont    1936     358248 0.00989
    ## 2 Utah       1957     834210 0.00868
    ## 3 Wisconsin  1958    3842268 0.00834
