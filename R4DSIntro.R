# Install tidyverse
install.packages("tidyverse")

# load tidyverse
library(tidyverse)

# Other packages we'll use
install.packages(c("nycflights13", "gapminder", "Lahman"))

# Data Visualization
# 3.1 learning ggplot2

# the mpg data frame is found within ggplot2

mpg
# displ is a car's engine size in litres, 
# hwy is fuel efficiency on the highway
# to learn more about mpg run ?mpg

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
# ggplot() creates a coordinate system you can add layers to
# data=mpg says what dataset will be used in the coordinate system
# geom_point() adds a layer of points to the plot
# geom functions take a mapping argument, this defines how variables are mapped to visual properties
# the mapping argument is always paired with aes(), and the x and y of aes() specify which variables to map on the x and y axes

# heres a template for making maps, replace the bracketed sections for it to work
## ggplot(data = <DATA>) + 
  ## <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))
# we'll keep adding to this template to make cool graphs

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(data=mpg)
# this creates a blank map
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = hwy, y = cyl))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = class, y = drv))
# categorical variables like this don't make interesting or useful graphs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
# the class variable classifies cars into compact, midsize, and SUV
# you can add class to the plot by mapping it to an aesthetic

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# Above we see the classes by color, which is helpful, below creates a less helpful plot

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))
#> Warning: Using size for a discrete variable is not advised.

# transparency - a bit better than the size one, but I still prefer the colors
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# shapes, this seems about as useful as the colors, except SUVs got dropped cause ggplot2 only does 6 shapes at a time
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# can also change aesthetics manually

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

# make sure the paranthesies are right
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# if you run something, and the console has a '+' R things the expression is incomplete, hit 'ESCAPE" and start over

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Similar to aesthetics is facets, which split your plots up by group

# to use facet_wrap() the first argument should be a forula which you create with '~' followed by a variable name

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~class, nrow = 2)
# can also do this by two variables using facet_grid()

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

# different geoms change how plots show the data by a lot

# points
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# smooth
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

#ggplot cheatsheat: https://www.rstudio.com/resources/cheatsheets/

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))
# a cleaner way of writing to avoid duplicates
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

# you can still change just one layer at a time even with the global mappings

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.6.1 Exersices

# line chart
ggplot(data = mpg) +
  geom_freqpoly(mapping = aes(x = hwy))

# boxplot
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = class, y = hwy))

# histogram
ggplot(data = mpg, mapping = aes(x = hwy)) + 
  geom_histogram() 
  
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))
# 3.7 Statistical Transformations

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

# to make this plot R uses the statistical transformation, shortened to stat
# called the count stat, which counts the number of rows in a bin for diamonds in the fair, good, very good, premiem, and ideal categories
# it then shows this on the bar graph
# you can learn which stat a geom uses by doing ?geom_bar

?geom_bar
# shows stat = "count"

# you can often use geoms and stats interchangeably

ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut))

# because every geom has a default stat, and every stat a geom

# can be useful to change the default stat, here the code counts the frequency instead of number of rows


demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")

# this bar chart is by proportion, not count

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))

# 3.8 colorful bar charts!

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")
ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")

# compare proportions across groups
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

# although the car data has 234 observations, R only plotted 126 points
# this is because many points would overlap, but avoiding overplotting
# might make it hard to see where the mass of the data is
# maybe there is one point that would hae 109 values?

# to avoid this gridding by setting position adjustment to 'jitter'
# this adds a bit of random noise to each point

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

# Adding randomness makes your graph less accurate at small scales, 
# more revealing at large scales.

# 3.9 Coordinate Systems

# coord_flip() flips x and y, cool if you want horizontal boxplots
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()

# coord_quickmap() sets aspect ratio correctly for maps
nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

# coord_polar() uses polar coordinates, and they reveal an interesting connection between a bar chart and a Coxcomb chart
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

# 3.10  Grammer of Graphics
# template:

# ggplot(data = <DATA>) + 
#<GEOM_FUNCTION>(
#  mapping = aes(<MAPPINGS>),
#  stat = <STAT>, 
#  position = <POSITION>
#) +
#  <COORDINATE_FUNCTION> +
#  <FACET_FUNCTION>
