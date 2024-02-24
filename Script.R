### Datacamp Data Scientist Professional Project

### February 2024
### Patrick Brown

require(tidyverse)
require(caret)
require(caTools)
require(glmnet)
require(class)

recipe <- read.csv("~/Documents/Github/Data-Scientist-Professional-Project/recipe_site_traffic_2212.csv")

#check for missing data
colSums(is.na(recipe))

# Check for duplicate rows:
recipe %>%
  group_by_all() %>%
  filter(n()>1) %>%
  ungroup() 

# Replace missing values
recipe$high_traffic <- replace_na(recipe$high_traffic, "False")
recipe$high_traffic <- str_replace(recipe$high_traffic, "High", "True") 
recipe$high_traffic <- as.factor(recipe$high_traffic)

# Filter out all rows with missing nutrient data AND "False" high_traffic classification
recipe <- recipe %>% filter(!is.na(carbohydrate) | high_traffic == "True")
recipe2 <- recipe %>% filter(!is.na(carbohydrate))

colSums(is.na(recipe2)) #check again for missing data

# Replace missing values
recipe$calories <- recipe$calories %>% 
  replace_na(median(recipe$calories, na.rm = TRUE))

recipe$carbohydrate <- recipe$carbohydrate %>% 
  replace_na(median(recipe$carbohydrate, na.rm = TRUE))

recipe$sugar <- recipe$sugar %>% 
  replace_na(median(recipe$sugar, na.rm = TRUE))

recipe$protein <- recipe$protein %>% 
  replace_na(median(recipe$protein, na.rm = TRUE))

recipe$servings <- as.numeric(gsub(".*?([0-9]+).*", "\\1", recipe$servings)) #remove text from servings column

# Reclassify columns with appropriate data types
recipe$category <- factor(recipe$category)
levels(recipe$category) #check levels - has 11, should have 10
recipe$category <- str_replace(recipe$category, "Chicken Breast", "Chicken")
recipe$category <- factor(recipe$category)
levels(recipe$category) 

# Check structure to confirm cleaning is complete
str(recipe)

# Plot nutrient data histograms
carbhist <- ggplot(recipe, aes(carbohydrate)) +
  geom_histogram() 

calhist <- ggplot(recipe, aes(calories)) +
  geom_histogram()

sughist <- ggplot(recipe, aes(sugar)) +
  geom_histogram()

prothist <- ggplot(recipe, aes(protein)) +
  geom_histogram()

grid.arrange(carbhist, calhist, sughist, prothist)

# Calculate proportion of true/false classifications by category
high_by_cat <- recipe %>% group_by(category) %>% filter(high_traffic == "True") %>% 
  count()
low_by_cat <- recipe %>% group_by(category) %>% filter(high_traffic == "False") %>% 
  count()
traf_by_cat <- left_join(high_by_cat, low_by_cat, by = "category") %>% 
  rename("True" = "n.x", "False" = "n.y")
traf_by_cat2 <- traf_by_cat %>% group_by(category) %>% mutate(props = True / False)

# Plot classification proportions by category 
ggplot(traf_by_cat2, aes(category, props, fill = category)) +
  geom_col() +
  scale_color_brewer() +
  labs(x = "Category", y = "Proportions",
       title = "Proportions of High to Low Traffic Recipes by Category") +
  theme(legend.title = element_blank())

# Calculate proportion of true/false classifications by serving
high_by_serv <- recipe %>% group_by(servings) %>% filter(high_traffic == "True") %>% 
  count()
low_by_serv <- recipe %>% group_by(servings) %>% filter(high_traffic == "False") %>% 
  count()
traf_by_serv <- left_join(high_by_serv, low_by_serv, by = "servings") %>% 
  rename("True" = "n.x", "False" = "n.y")
traf_by_serv2 <- traf_by_serv %>% group_by(servings) %>% mutate(props = True / False)

# Plot classification proportions by serving
ggplot(traf_by_serv2, aes(servings, props, fill = servings)) +
  geom_col() +
  scale_color_brewer() +
  labs(x = "Servings", y = "Proportions",
       title = "Proportions of High to Low Traffic Recipes by Serving") +
  theme(legend.title = element_blank())


# Modeling with Caret

# Fit model 1: logistic regression model with train/test split

# Set seed
set.seed(22)

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
model1 <- glm(high_traffic ~ calories + carbohydrate + protein + 
               sugar + category, train, family = "binomial")
summary(model1)

# Predict on test data: p
p1 <- predict(model1, newdata = test, type = "response") #predicted probabilities
test$prediction1 <- ifelse(p1 > 0.5, 1, 0)

# Calculate model accuracy
test$ht <- ifelse(test$high_traffic == "True", 1, 0) 
mean(test$ht == test$prediction1)


# Confusion Matrix: 
p_class1 <- factor(ifelse(p1 >0.5, "True", "False"))
table(p_class1, test[["high_traffic"]])
confusionMatrix(p_class1, test[["high_traffic"]])

colAUC(p1, test[["high_traffic"]], plotROC = TRUE)

#custom train control for logistic regression with 10-fold cross validation:
myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  verboseIter = TRUE
)

#fit GLM model with caret and cross validation
set.seed(22)
model2 <- train(
  high_traffic ~ category + protein + carbohydrate + sugar + 
    calories + servings,
  data = recipe,
  family = "binomial",
  method = "glm",
  trControl = myControl
)
summary(model2)

# Predict on full data: p2
p2 <- predict(model2, recipe, type = "prob") #predicted probabilities
recipe$prediction2 <- ifelse(p2$True > 0.5, 1, 0)

# Calculate model accuracy
recipe$ht <- ifelse(recipe$high_traffic == "True", 1, 0) 
mean(recipe$ht == recipe$prediction2)


# Confusion Matrix: 
p_class2 <- factor(ifelse(p2$True >0.5, "True", "False"))
table(p_class2, recipe[["high_traffic"]])
confusionMatrix(p_class2, recipe[["high_traffic"]], positive = "True")


colAUC(recipe$prediction2, recipe[["high_traffic"]], plotROC = TRUE)


#fit random forest model 
set.seed(22)
model3 <- train(
  high_traffic ~ category + protein + carbohydrate + sugar + 
    calories + servings,
  data = recipe,
  family = "binomial",
  method = "ranger",
  trControl = myControl
)
model3

# Predict on full data: p3
p3 <- predict(model3, recipe, type = "prob") #predicted probabilities
recipe$prediction3 <- ifelse(p3$True > 0.5, 1, 0)

# Calculate model accuracy
mean(recipe$ht == recipe$prediction3)


# Confusion Matrix: 
p_class3 <- factor(ifelse(p3$True >0.5, "True", "False"))
table(p_class3, recipe[["high_traffic"]])
confusionMatrix(p_class3, recipe[["high_traffic"]], positive = "True")

set.seed(23)
new_recipe <- slice_sample(recipe, n = 1)

predict(model2, newdata = new_recipe, type = "prob") #run model on new recipe

recipe$prediction3 <- ifelse(p3$True > 0.5, 1, 0)


