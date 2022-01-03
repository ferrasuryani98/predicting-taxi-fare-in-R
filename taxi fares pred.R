install.packages('tidyverse')
install.packages('ggmap')
install.packages('viridis')
install.packages('tree')
install.packages('lubridate')
install.packages('caret')
library(tidyverse)


taxi <- read_csv('taxi.csv')
head(taxi)
nrow(taxi)

# Feature Engineering
# renaming the location variables,dropping any journeys with zero fares and zero tips, creating the total variable as the log sum of fare and tip
# taking the log means we remedy the effect of outliers by making really large numbers smaller
taxi <- taxi %>%
  rename(lat = pickup_latitude, long = pickup_longitude) %>%
  filter(fare_amount > 0 | tip_amount > 0) %>%
  mutate(total = log(fare_amount + tip_amount))

# Reducing the data to taxi trips starting in Manhattan
taxi <- taxi  %>% 
  filter(between(lat, 40.70, 40.83) & between(long, -74.025, -73.93))

# plotting a density map with the number of journey start locations
library(ggmap)
library(viridis)
manhattan <- readRDS("manhattan.rds")
ggmap(manhattan, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') +
  geom_bin2d(data = taxi, aes(x = long, y = lat), bins = 60, alpha = 0.6) + 
  labs(x = 'Longitude', y = 'Latitude', fill = 'Journeys')

# Generate three additional time variables
library(lubridate)
taxi <- taxi %>% 
  mutate(hour = hour(pickup_datetime), 
         wday = wday(pickup_datetime, label = TRUE), 
         month = month(pickup_datetime, label = TRUE))
head(taxi)

# build model with regression trees
library(tree)
fitted_tree <- tree(total ~ lat + long + hour + wday + month, data = taxi)
plot(fitted_tree)
text(fitted_tree)

# result with regression trees
summary(fitted_tree)

# build model with random forest for comparison
library(randomForest)
fitted_forest <- randomForest(total ~ lat + long + hour + wday + month, 
                              data = taxi, ntree = 80, sampsize = 10000)
varImpPlot(fitted_forest)

# result with random forest
summary(fitted_forest)
fitted_forest

# findings: both models are showing similar Residual mean deviance. Both only explain about 3% of the variance

# Extracting the prediction from fitted_forest
taxi$pred_total <- fitted_forest$predicted

# Plotting the predicted mean trip prices from according to the random forest
ggmap(manhattan, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') +
  stat_summary_2d(data = taxi, aes(x = long, y = lat, z= pred_total), bins = 60, alpha = 0.6, fun = mean) + 
  labs(x = 'Longitude', y = 'Latitude', fill = 'predicted mean trip price (log fare+tip)')

# findings: Looking at the map, we see that fares in downtown Manhattan are predicted to be high, while midtown is lower.

# create a function that returns the mean if there are 15 or more datapoints
mean_if_enough_data <- function(x) { 
  ifelse( length(x) >= 15, mean(x), NA) 
}

# Plotting the actual fate: mean trip prices from the data
ggmap(manhattan, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') +
  stat_summary_2d(data = taxi, aes(x = long, y = lat, z= total), bins = 60, alpha = 0.6, fun = mean_if_enough_data) + 
  labs(x = 'Longitude', y = 'Latitude', fill = 'predicted mean trip price (log fare+tip)')

# findings: people are spending the most on their taxi trips in downtown