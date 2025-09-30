# Load necessary libraries
# install.packages("e1071") # Uncomment and run if you don't have e1071 installed
# install.packages("dplyr") # Uncomment and run if you don't have dplyr installed
library(e1071) # For SVM
library(dplyr) # For data manipulation
library(ggplot2)
library(caret)
# --- 1. Load the datasets ---
# The zoo.csv file contains the animal features and numeric class type
zoo_data <- read.csv("zoo.csv")

# The class.csv file contains the mapping from numeric class type to character class type
class_data <- read.csv("class.csv")

# Display the first few rows of each dataset to understand their structure
print("First few rows of zoo_data:")
print(head(zoo_data))

print("Structure of zoo_data:")
print(str(zoo_data))

print("First few rows of class_data:")
print(head(class_data))

print("Structure of class_data:")
print(str(class_data))

# --- 2. Data Preprocessing ---

# Rename 'class_type' in zoo_data to match 'Class_Number' in class_data for merging
# Note: It appears 'class_type' in zoo_data actually refers to the numeric class,
# and 'Class_Number' in class.csv is the corresponding numeric identifier.
# We will merge on these columns.
#zoo_data <- zoo_data %>%
 # rename(Class_Number = class_type)

# Merge the datasets to include the actual Class_Type string
# We will merge by 'Class_Number' from zoo_data and 'Class_Number' from class_data
# We only need 'Class_Type' from class_data for the target variable.
# We also need to keep the 'animal.name' from zoo_data if we want to refer to animals,
# but for prediction, it should be removed as it's an identifier.

# First, create a mapping from numeric class to class name
#class_mapping <- class_data %>%
 # select(Class_Number, Class_Type)
#print(class_mapping)

# Merge the zoo data with the class type names
# By default, merge will use common column names for joining.
# Here, it will use 'Class_Number'.
#full_data <- merge(zoo_data, class_mapping, by = "Class_Number", all.x = TRUE)
#print(full_data)

# Remove the 'animal.name' column as it's an identifier and not a feature for prediction
# Also remove 'Class_Number' as we now have 'Class_Type' which is our target
# and 'Class_Number' is redundant for the model.
#data_for_model <- full_data %>%
 # select(-animal_name, -Class_Number)

# Convert the target variable 'Class_Type' to a factor
#data_for_model$Class_Type <- as.factor(data_for_model$Class_Type)
#print(data_for_model$Class_Type)

#print("First few rows of data_for_model after merging and cleaning:")
#print(head(data_for_model))

#print("Structure of data_for_model:")
#print(str(data_for_model))

#print("Levels of the target variable (Class_Type):")
#print(levels(data_for_model$Class_Type))

# Check for any missing values (should be none based on dataset description)
#print("Summary of missing values in data_for_model:")
#print(sapply(data_for_model, function(x) sum(is.na(x))))


colnames(zoo_data)<-tolower(colnames(zoo_data))
print(colnames(zoo_data))
colnames(class_data)<-c("class_type","class_name","animal_name","no_of_animals_in_class")
class_data<- select(class_data,class_type,class_name)
zoo_data$class_type<-as.factor(zoo_data$class_type)
print(zoo_data)
print(class_data)


# --- 3. Split the data into training and testing sets ---
# Set seed for reproducibility
set.seed(501)

# Create a random sample of row indices for the training set
# Using 70% of the data for training and 30% for testing
intraining<-createDataPartition(y=zoo_data$class_type, p=.7,list = F)

train_data <- zoo_data[intraining, ]
test_data <- zoo_data[-intraining, ]

print(paste("Training data size:", nrow(train_data)))
print(paste("Testing data size:", nrow(test_data)))

# --- 4. Train the SVM model ---
# For classification, 'Class_Type' is the dependent variable.
# All other columns (features) are independent variables.
# The formula is 'Class_Type ~ .' which means 'Class_Type' explained by all other columns.

# Initial SVM model with a radial kernel (a common choice for non-linear relationships)
# and default parameters.
# The `scale = FALSE` argument is important here because our features are already binary (0/1)
# or small integers (legs), and scaling them might not be necessary or could even distort.
# However, for general SVM practice, scaling features is often recommended.
# Given the nature of these specific features, we might not need extensive scaling.
print("Training initial SVM model...")
svm_model <- svm(class_type ~ ., data = train_data[, -1], kernel = "radial", scale = FALSE)  # remove animal_name

print("SVM model trained.")
print(summary(svm_model))

# --- 5. Make predictions on the test set ---
print("Making predictions on the test set...")
predictions <- predict(svm_model, test_data)
print(predictions)

# --- 6. Evaluate the model ---
# Create a confusion matrix to evaluate the model's performance
predictions <- predict(svm_model, newdata = test_data[, -c(1, ncol(test_data))])  # remove animal_name and actual class_type
conf_matrix <- confusionMatrix(predictions, test_data$class_type)
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix$table)) / sum(conf_matrix$table)
print(paste("Accuracy of the SVM model on the test set:", round(accuracy, 4)))


str(train_data)


# --- Optional: Hyperparameter Tuning using tune.svm ---
# This step helps find the best 'cost' and 'gamma' parameters for a radial kernel SVM
# by performing cross-validation. This can be computationally intensive.
print("Starting hyperparameter tuning (this might take a while)...")

train_data$animal_name <- as.character(train_data$animal_name)

# Drop 'animal_name' from training data
train_data_clean <- subset(train_data, select = -animal_name)

# Now run hyperparameter tuning on the clean data
tuned_svm <- tune.svm(
  class_type ~ .,
  data = train_data_clean,
  kernel = "radial",
  cost = c(0.1, 1, 10, 100),
  gamma = c(0.01, 0.1, 0.5, 1, 2),
  scale = FALSE
)



print("Best parameters found by tuning:")
print(summary(tuned_svm))

# Train the SVM model with the best parameters found by tuning
print("Training SVM model with best tuned parameters...")
best_svm_model <- tuned_svm$best.model
print("Best SVM model trained.")
print(summary(best_svm_model))

# Make predictions with the best model
predictions_best_model <- predict(best_svm_model, test_data)

# Evaluate the best model
confusion_matrix_best <- table(Actual = test_data$class_type, Predicted = predictions_best_model)
print("Confusion Matrix with Best Tuned Model:")
print(confusion_matrix_best)

accuracy_best <- sum(diag(confusion_matrix_best)) / sum(confusion_matrix_best)
print(paste("Accuracy of the Best Tuned SVM model on the test set:", round(accuracy_best, 4)))

# Display actual vs predicted for a few test samples
print("Actual vs Predicted for first 10 test samples (Best Tuned Model):")
comparison_df <- data.frame(Actual = test_data$class_type[1:10], Predicted = predictions_best_model[1:10])
print(comparison_df)







# --- VISUALIZATION 1: Distribution of Animal Classes ---
# This bar plot shows how many animals belong to each class type.
# It helps to identify if there's an imbalance in the number of animals per class.
print("Generating bar plot for Class Type distribution...")
class_distribution_plot <- ggplot(train_data, aes(x = class_type, fill = class_type)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, color="black") +
  theme_minimal() +
  labs(
    title = "Distribution of Animal Classes in the Dataset",
    x = "Animal Class Type",
    y = "Number of Animals"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")


print(class_distribution_plot)
cat("\n(Visualization 1: Bar plot of Class Distribution is displayed above)\n\n")


# --- VISUALIZATION 2: Confusion Matrix Heatmap (for the Best Tuned Model) ---
# This heatmap visually represents the confusion matrix, making it easier to identify
# where the model is performing well (darker cells on the diagonal) and where it's
# confusing classes (darker cells off-diagonal).
# --- VISUALIZATION 2: Confusion Matrix Heatmap (for the Best Tuned Model) ---
print("Generating heatmap for Confusion Matrix (Best Tuned Model)...")

# Use caret::confusionMatrix properly (note the double colon for safety)
caret_confusion_best <- caret::confusionMatrix(predictions_best_model, test_data$class_type)

# Convert confusion matrix table to dataframe for ggplot
plot_confusion <- as.data.frame(as.table(caret_confusion_best$table))
colnames(plot_confusion) <- c("Actual", "Predicted", "Freq")

# Create heatmap plot
confusion_heatmap <- ggplot(plot_confusion, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 4) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  labs(
    title = "Confusion Matrix Heatmap (Best Tuned SVM Model)",
    x = "Predicted Class",
    y = "Actual Class"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5) # Center plot title
  )

print(confusion_heatmap)

cat("\n(Visualization 2: Confusion Matrix Heatmap for the Best Tuned Model is displayed above)\n")




print("Generating pie chart for Class Type distribution...")
class_counts <- train_data %>%
  count(class_type) %>%
  mutate(percentage = n / sum(n) * 100,
         label = paste0(class_type, " (", round(percentage, 1), "%)"))
print(class_counts)

class_distribution_pie <- ggplot(class_counts, aes(x = "", y = n, fill = class_type)) +
  geom_bar(width = 1, stat="identity") +
  coord_polar(theta="y") +
  theme_void() +
  geom_text(aes(label=label), position=position_stack(vjust=0.5), size=3) +
  labs(title = "Animal Class Distribution Pie Chart") +
  scale_fill_brewer(palette = "Pastel1")
print(class_distribution_pie)


print("Generating lollipop chart for Class Type distribution...")
class_counts <- train_data %>%
  count(class_type) %>%
  arrange(desc(n))

class_distribution_lollipop <- ggplot(class_counts, aes(x = reorder(class_type, n), y = n)) +
  geom_segment(aes(xend = class_type, y = 0, yend = n), color="gray") +
  geom_point(size=5, color="darkorange") +
  geom_text(aes(label=n), vjust=-0.5) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Animal Class Distribution (Lollipop Chart)",
    x = "Animal Class Type",
    y = "Number of Animals"
  )
print(class_distribution_lollipop)

