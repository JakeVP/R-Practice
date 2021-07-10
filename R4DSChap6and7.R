library(dplyr)
library(nycflights13)
library(tidyverse)
library(hexbin)

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))


# To examine the distribution of a categorical variable, use a bar chart:

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

# The height of the bars displays how many observations occurred with each x value. You can compute these values manually with dplyr::count():

diamonds %>% 
  count(cut)
#> # A tibble: 5 x 2
#>   cut           n
#>   <ord>     <int>
#> 1 Fair       1610
#> 2 Good       4906
#> 3 Very Good 12082
#> 4 Premium   13791
#> 5 Ideal     21551

# A variable is continuous if it can take any of an infinite set of ordered values. Numbers and date-times are two examples of continuous variables. To examine the distribution of a continuous variable, use a histogram:

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

# You can compute this by hand by combining dplyr::count() and ggplot2::cut_width():

diamonds %>% 
  count(cut_width(carat, 0.5))
#> # A tibble: 11 x 2
#>   `cut_width(carat, 0.5)`     n
#>   <fct>                   <int>
#> 1 [-0.25,0.25]              785
#> 2 (0.25,0.75]             29498
#> 3 (0.75,1.25]             15977
#> 4 (1.25,1.75]              5313
#> 5 (1.75,2.25]              2002
#> 6 (2.25,2.75]               322
#> # … with 5 more rows

# You can set the width of the intervals in a histogram with the binwidth argument, which is measured in the units of the x variable.

# You should always explore a variety of binwidths when working with histograms, as different binwidths can reveal different patterns. For example, here is how the graph above looks when we zoom into just the diamonds with a size of less than three carats and choose a smaller binwidth.

smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

# overlay multiple histograms

ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

# overlapping lines is easier to understand then overlapping histograms

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

# looks like there is some subgrouping going on...

# old faithful erruption time:

ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25)

# when outliers force you to be so zoomed out its hard to see the outliers at all
# you can zoom in on them like so:

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

# now we can decide what to do with the outliers
# below we decide to throw them out

unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>%
  arrange(y)
unusual
#> # A tibble: 9 x 4
#>   price     x     y     z
#>   <int> <dbl> <dbl> <dbl>
#> 1  5139  0      0    0   
#> 2  6381  0      0    0   
#> 3 12800  0      0    0   
#> 4 15686  0      0    0   
#> 5 18034  0      0    0   
#> 6  2130  0      0    0   
#> 7  2130  0      0    0   
#> 8  2075  5.15  31.8  5.12
#> 9 12210  8.09  58.9  8.06

# 7.4 Missing values
# you can drop missing values
diamonds2 <- diamonds %>% 
  filter(between(y, 3, 20))

# which isn't recommended,
# or replace them with NA
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

# ifelse has 3 arguments
# the test, which should be a logical vector
# the second argument is what happens when the test is TRUE
# the third is when the argument is FALSE

# instead of ifelse you can use dplyr::case_when()

# ggplot tells you when it drop NAs
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()
#> Warning: Removed 9 rows containing missing values (geom_point).

# you can set na.rm=TRUE to stop that
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE)

# you can also use in.na() to see if there are any patterns in the NAs

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)


# 7.5 Covariation

# can be hard to see relationships if the overall count for one variable
# is very large, and another is very small

ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)
ggplot(diamonds) + 
  geom_bar(mapping = aes(x = cut))

# to make the comparison easier instead of displaying count we can display density

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)


# it looks like fair diamonds have the highest average price?

# we can also use a boxplot

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

# it's easier to compare the boxplots

# you can reorder the boxs to make them more informative using reorder()
# reorder() arranges them in order of the median
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))

# if you have long variable names
# you can flip the graph 90 degrees using coord_flip()

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()

diamonds
ggplot(data = diamonds) +
  geom_boxplot(mapping = aes(x = reorder(clarity, price, FUN = median), y = price))

ggplot(data = diamonds) +
  geom_boxplot(mapping = aes(x = reorder(color, price, FUN = median), y = price))

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + 
  geom_point() + 
  geom_smooth()

ggplot(data = diamonds) +
  geom_boxplot(mapping = aes(x = reorder(cut, carat, FUN = median), y = carat))
# looks like fair has the highest price, because the fair cuts have the highest carat value on average

# for two categorical values you can count the number of observations between each combination

ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

# you can also compute the count with dplyr

diamonds %>% 
  count(color, cut)
#> # A tibble: 35 x 3
#>   color cut           n
#>   <ord> <ord>     <int>
#> 1 D     Fair        163
#> 2 D     Good        662
#> 3 D     Very Good  1513
#> 4 D     Premium    1603
#> 5 D     Ideal      2834
#> 6 E     Fair        224
#> # … with 29 more rows

# also you can do it in this pretty way
diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))
# but you may want to order them, and for larger plots you might want a heat map
# you can use d3heatmap or heatmaply packages to create interactive plots

# for two contiuous variables you can use scatterplots

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

# this one has added transparency to make it easier to understand
ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100)

# you can also use bins to help show the data
# the color of the bins reflects how many points fall in that area

ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

# install.packages("hexbin")
ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))

# you can also bin just one of the two continuous variables
# so that it acts like a categorical one

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

# to show the number of variables that fall within each bin, or boxplot
# you can make the boxplots proportional to how many fall in them
# using varwidth = TRUE

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

# Patterns in your data provide clues about relationships. If a systematic relationship exists between two variables it will appear as a pattern in the data. If you spot a pattern, ask yourself:
  
  # Could this pattern be due to coincidence (i.e. random chance)?
  
  # How can you describe the relationship implied by the pattern?
  
  # How strong is the relationship implied by the pattern?
  
  # What other variables might affect the relationship?
  
  # Does the relationship change if you look at individual subgroups of the data?
  
# A scatterplot of Old Faithful eruption lengths versus the wait time between eruptions shows a pattern: longer wait times are associated with longer eruptions. The scatterplot also displays the two clusters that we noticed above.

ggplot(data = faithful) + 
  geom_point(mapping = aes(x = eruptions, y = waiting))

# The following code fits a model that predicts price from carat and then computes the residuals (the difference between the predicted value and the actual value). The residuals give us a view of the price of the diamond, once the effect of carat has been removed.

library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))

# now we can see that relative to their size, better quality diamonds are more expensive

ggplot(data = diamonds2) + 
  geom_boxplot(mapping = aes(x = cut, y = resid))

# for brevity you can write
ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_freqpoly(binwidth = 0.25)
# as:
ggplot(faithful, aes(eruptions)) + 
  geom_freqpoly(binwidth = 0.25)
