### Datacamp Data Scientist Professional Project

### February 2024
### Patrick Brown

require(tidyverse)
recipe <- read.csv("~/Documents/Github/Data-Scientist-Professional-Project/recipe_site_traffic_2212.csv")

colSums(is.na(recipe)) #check for missing data
#recipe[recipe == ''] <- NA #replace missing values with NA

ggplot(recipe, aes(carbohydrate)) +
  geom_histogram()

recipe$high_traffic <- replace_na(recipe$high_traffic, "Not high")
recipe$high_traffic <- factor(recipe$high_traffic)

recipe <- recipe %>% filter(!is.na(carbohydrate) | high_traffic == "High")

recipe$calories <- recipe$calories %>% 
  replace_na(median(recipe$calories, na.rm = TRUE))

recipe$carbohydrate <- recipe$carbohydrate %>% 
  replace_na(median(recipe$carbohydrate, na.rm = TRUE))

recipe$sugar <- recipe$sugar %>% 
  replace_na(median(recipe$sugar, na.rm = TRUE))

recipe$protein <- recipe$protein %>% 
  replace_na(median(recipe$protein, na.rm = TRUE))

recipe$servings <- as.numeric(gsub(".*?([0-9]+).*", "\\1", recipe$servings))
recipe$servings <- factor(recipe$servings, ordered = TRUE)
levels(recipe$servings) # check levels

recipe$category <- factor(recipe$category)
levels(recipe$category) #check levels - has 11, should have 10
recipe$category <- str_replace(recipe$category, "Chicken Breast", "Chicken")
recipe$category <- factor(recipe$category)
levels(recipe$category) 

ggplot(recipe, aes(log(carbohydrate))) +
  geom_histogram()

ggplot(recipe, aes(log(calories))) +
  geom_histogram()

ggplot(recipe, aes(log(sugar))) +
  geom_histogram()

ggplot(recipe, aes(log(protein))) +
  geom_histogram()

ggplot(recipe, aes(high_traffic, category)) +
  geom_boxplot() 

# Modeling with Caret

# Fit glm model: model

# Set seed
set.seed(42)

# Shuffle row indices: rows
rows <- sample(nrow(recipe))

# Randomly order data
shuffled_recipe <- recipe[rows, ]

# Determine row to split on: split
split <- round(nrow(recipe) * 0.75)

# Create train
train <- recipe[1:split, ]

# Create test
test <- recipe[(split + 1):nrow(recipe), ]

# Fit lm model on train: model
model <- glm(high_traffic ~ calories + carbohydrate + protein + 
               sugar + category, train, family = "binomial")

# Predict on test data: p
p <- predict(model, newdata = test)

# Compute errors: error
error <- p - test[["price"]]

# Calculate RMSE
sqrt(mean(error ^ 2))

