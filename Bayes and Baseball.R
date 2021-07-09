# Understanding Empirical Bayes Estimation
# Using Baseball Statistics
# From David Robinson: http://varianceexplained.org/r/empirical_bayes_baseball/

library(tidyverse)
library(Lahman)

# first lets filter out pitchers, and summarize players across years
# to get their career Hits (H) and At Bats (AB)

career <- Batting %>%
  filter(AB > 0) %>%
  anti_join(Pitching, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB)) %>%
  mutate(average = H / AB)

# now lets add in player names

career <- People %>%
  as_tibble() %>%
  select(playerID, nameFirst, nameLast) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(career, by = "playerID") %>%
  select(-playerID)

career

# so if we just arrange by highest average these top people clearly just got lucky

career %>% arrange(desc(average))
career %>% arrange(average)

# here is a histogram with those with less than 500 at bats filtered out

career %>%
  filter(AB >= 500) %>%
  ggplot(mapping = aes( x = average)) +
  geom_histogram(binwidth = .005)

career %>%
  filter(AB >= 500) %>%
  ggplot(aes(average)) +
  geom_histogram(binwidth = .005)

# Since this data only has one peak we'll use a beta distribution
# X ~ Beta(a0, b0)
# we just need to pick a0 and b0, which are called hyper-parameters of our model
# in r we can use optim, mle, or bbmle and others to fit a probability distribution
# we'll use the fitdistr function from MASS

# just like the graph, we have to filter for the players we actually
# have a decent estimate of
career_filtered <- career %>%
  filter(AB >= 500)

m <- MASS::fitdistr(career_filtered$average, dbeta,
                    start = list(shape1 = 1, shape2 = 10))
alpha0 <- m$estimate[1]
beta0 <- m$estimate[2]

career_filtered %>%
  ggplot(mapping = aes( x = average)) +
  geom_histogram(aes( average, y = ..density..), binwidth = .005) +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0), color= "red", size = 1) +
  xlab("Batting average")

# Now we'll use the distribution as a prior for each individual estimate
# we'll do this by adding a0 to the number of hits
# and addingb0 a0 + b0 to the number of total at bats
# so assuming some batter had 1000 at bats, and 300 hits we would to:

# (300 + a0) / (1000 + a0 + b0)
# which would be
(300 + 79.5) / (1000 + 79.5 + 228) 

# compared to someone who got 4 out of 10

# (4 + a0) / (1000 + a0 + b0)
(4 + 79.5) / (1000 + 79.5 + 228)

# So we can see the first batter is much better! because although
# 4/10 > 300/1000, 0.29 > 0.063!

# lets add this to the dataframe for everyone

career_eb <- career %>%
  mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0))

career %>% arrange(desc(average))
career_eb %>% arrange(desc(eb_estimate))
career_eb %>% arrange(eb_estimate)

# Lets plot how this changes our estimates
# this is often called shrinkage because we move the estimates closer to the average
# the horizontal line is y = a0/a0+b0
# the diagnal is x=y, points that lie close to it are ones that didn't get shrunk
# much because there is enought at bats for us to believe the average

ggplot(career_eb, aes(average, eb_estimate, color = AB)) +
  geom_hline(yintercept = alpha0 / (alpha0 + beta0), color = "red", lty = 2) +
  geom_point() +
  geom_abline(color = "red") +
  scale_colour_gradient(trans = "log", breaks = 10 ^ (1:5)) +
  xlab("Batting average") +
  ylab("Empirical Bayes batting average")

