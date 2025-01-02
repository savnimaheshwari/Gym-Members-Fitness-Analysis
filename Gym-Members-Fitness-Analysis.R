# Load necessary libraries
library(ggplot2)
library(dplyr)   # data manipulation
library(e1071)   # skewness and kurtosis

# Load the data. Need to change the filepath to a local directory
gym_data <- read.csv("/Users/savni/Downloads/gym_members_exercise_tracking.csv")

# Renaming columns with units in their names to make it easier
colnames(gym_data) <- gsub("Weight..kg.", "Weight", colnames(gym_data))
colnames(gym_data) <- gsub("Height..m.", "Height", colnames(gym_data))
colnames(gym_data) <- gsub("Session_Duration..hours.", "Session_Duration", colnames(gym_data))
colnames(gym_data) <- gsub("Water_Intake..liters.", "Water_Intake", colnames(gym_data))
colnames(gym_data) <- gsub("Fat_Percentage", "Fat_Percentage", colnames(gym_data))

# checking after renaming
colnames(gym_data)

# analyzing variables with insights
# 1. Age
cat("Age Analysis\n")
summary(gym_data$Age)
sd(gym_data$Age, na.rm = TRUE)  # Standard deviation
var(gym_data$Age, na.rm = TRUE) # Variance


# boxplot
boxplot(gym_data$Age, main = "Boxplot of Age", col = "lightblue", ylab = "Age")
ggplot(gym_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +  # Add density curve
  labs(title = "Distribution of Age", x = "Age", y = "Frequency")

#2 - Gender  
ggplot(gym_data, aes(x = Gender)) +
  geom_bar(fill = c("pink", "blue")) +
  labs(title = "Count of Gender", x = "Gender", y = "Count")
summary(gym_data$Gender)

# 3. Weight
cat("\nWeight Analysis\n")
summary(gym_data$Weight)

# 4. Height
summary(gym_data$Height)  # Mean, median, min, max
boxplot(gym_data$Height, main = "Boxplot of Height", col = "lightgreen", ylab = "Height (cm)")  # Boxplot

# 5. Avg BPM
summary(gym_data$Avg_BPM)  # Mean, median, min, max

plot(gym_data$Weight, gym_data$Avg_BPM,
     main = "Average BPM vs. Weight",
     xlab = "Weight (kg)",
     ylab = "Average BPM (beats per minute)",
     pch = 16,            # Solid circles
     col = "blue")

# 6. Session Duration
cat("Session Duration Analysis\n")
summary(gym_data$Session_Duration)

# additional stuff
sd(gym_data$Session_Duration, na.rm = TRUE)  # Standard deviation
var(gym_data$Session_Duration, na.rm = TRUE) # Variance
range(gym_data$Session_Duration, na.rm = TRUE) # Minimum and maximum

# 7. Calories Burned
summary(gym_data$Calories_Burned)

# Boxplot for Calories Burned by Session Duration
boxplot(gym_data$Calories_Burned ~ gym_data$Session_Duration,
        main = "Calories Burned by Session Duration",
        xlab = "Session Duration (hours)",
        ylab = "Calories Burned",
        col = "lightblue",
        border = "blue")

# 8. Workout_Type
cat("Workout Type Summary\n")
workout_type_summary <- table(gym_data$Workout_Type)
print(workout_type_summary)

# prop
workout_type_proportion <- prop.table(workout_type_summary) * 100
cat("\nProportion (%) for each Workout Type:\n")
print(round(workout_type_proportion, 2))

# Histogram
barplot(workout_type_summary,
        main = "Distribution of Workout Types",
        xlab = "Workout Types",
        ylab = "Frequency",
        col = "lightcoral",
        border = "black",
        ylim = c(0, max(workout_type_summary) + 5))  # Add space above bars

# 9. Water_Intake
cat("Water Intake Summary (Liters)\n")
summary(gym_data$Water_Intake)

# Average Water Intake by Workout Type
avg_water_intake <- tapply(gym_data$Water_Intake, gym_data$Workout_Type, mean, na.rm = TRUE)

# Barplot of Average Water Intake by Workout Type
barplot(avg_water_intake,
        main = "Average Water Intake by Workout Type",
        xlab = "Workout Type",
        ylab = "Average Water Intake (Liters)",
        col = "skyblue",
        border = "blue")

# Load required libraries
library(corrplot)

# choosing only the numeric columns we want to correlate
fitness_vars <- c("Age", "Weight", "Height", "Avg_BPM", 
                  "Session_Duration", "Water_Intake", "Calories_Burned")

#correlation matrix
corr_matrix <- cor(gym_data[fitness_vars])

# corrplot/heatmap type
corrplot(corr_matrix,
         method = "color",      # Color for correlation values
         type = "upper",        # Show only upper triangle
         order = "hclust",      # Order by hierarchical clustering
         addCoef.col = "black", # Add correlation coefficients
         tl.col = "black",      # Text label color
         tl.srt = 45,          # Rotate text labels
         diag = TRUE)          # Show diagonal



#Part 2 - Research Question 1


# Loading necessary library
library(dplyr)

# data by gender
cat("Checking Gender Distribution:\n")
table(gym_data$Gender)  # Verify the counts of Male and Female

#  Calories_Burned for each Gender (in advance)
cat("\nSummary of Calories Burned by Gender:\n")
gym_data %>%
  group_by(Gender) %>%
  summarise(
    Mean = mean(Calories_Burned, na.rm = TRUE),
    SD = sd(Calories_Burned, na.rm = TRUE),
    Count = n()
  ) -> summary_table
print(summary_table)

#t-test (equal variances)
t_test_equal <- t.test(Calories_Burned ~ Gender, data = gym_data, 
                       var.equal = TRUE)
print(t_test_equal)

# check Normality (Shapiro-Wilk Test)
cat("\nNormality Test (Shapiro-Wilk):\n")
shapiro.test(gym_data$Calories_Burned[gym_data$Gender == "Male"])
shapiro.test(gym_data$Calories_Burned[gym_data$Gender == "Female"])

# Q-Q plot for Calories Burned by Gender
# For Males
qqnorm(gym_data$Calories_Burned[gym_data$Gender == "Male"])
qqline(gym_data$Calories_Burned[gym_data$Gender == "Male"], col = "red")

# For Females
qqnorm(gym_data$Calories_Burned[gym_data$Gender == "Female"])
qqline(gym_data$Calories_Burned[gym_data$Gender == "Female"], col = "blue")

# (Levene's Test) - homegenity
cat("\nLevene's Test for Equal Variances:\n")
library(car)
leveneTest(Calories_Burned ~ Gender, data = gym_data)

# Welch's t-test
welch_t_test <- t.test(Calories_Burned ~ Gender, data = gym_data, var.equal = FALSE)
cat("\nWelch's T-test Result:\n")
print(welch_t_test)

# Mann-Whitney U Test
mann_whitney_test <- wilcox.test(Calories_Burned ~ Gender, data = gym_data)
cat("\nMann-Whitney U Test Result:\n")
print(mann_whitney_test)

#Research Question 2
# Check Normality for Session Duration and Calories Burned (Shapiro-Wilk Test) using Q-Q plots

# Q-Q plot for Session Duration by Workout Type
par(mfrow = c(2, 2))  # plot arrrangement

qqnorm(gym_data$Session_Duration[gym_data$Workout_Type == "Cardio"], main = "Q-Q plot for Session Duration (Cardio)")
qqline(gym_data$Session_Duration[gym_data$Workout_Type == "Cardio"], col = "red")

qqnorm(gym_data$Session_Duration[gym_data$Workout_Type == "Strength"], main = "Q-Q plot for Session Duration (Strength)")
qqline(gym_data$Session_Duration[gym_data$Workout_Type == "Strength"], col = "red")

qqnorm(gym_data$Session_Duration[gym_data$Workout_Type == "Yoga"], main = "Q-Q plot for Session Duration (Yoga)")
qqline(gym_data$Session_Duration[gym_data$Workout_Type == "Yoga"], col = "red")

qqnorm(gym_data$Session_Duration[gym_data$Workout_Type == "HIIT"], main = "Q-Q plot for Session Duration (HIIT)")
qqline(gym_data$Session_Duration[gym_data$Workout_Type == "HIIT"], col = "red")

# Q-Q plot for Calories Burned by Workout Type
par(mfrow = c(2, 2))  # 

qqnorm(gym_data$Calories_Burned[gym_data$Workout_Type == "Cardio"], main = "Q-Q plot for Calories Burned (Cardio)")
qqline(gym_data$Calories_Burned[gym_data$Workout_Type == "Cardio"], col = "red")

qqnorm(gym_data$Calories_Burned[gym_data$Workout_Type == "Strength"], main = "Q-Q plot for Calories Burned (Strength)")
qqline(gym_data$Calories_Burned[gym_data$Workout_Type == "Strength"], col = "red")

qqnorm(gym_data$Calories_Burned[gym_data$Workout_Type == "Yoga"], main = "Q-Q plot for Calories Burned (Yoga)")
qqline(gym_data$Calories_Burned[gym_data$Workout_Type == "Yoga"], col = "red")

qqnorm(gym_data$Calories_Burned[gym_data$Workout_Type == "HIIT"], main = "Q-Q plot for Calories Burned (HIIT)")
qqline(gym_data$Calories_Burned[gym_data$Workout_Type == "HIIT"], col = "red")

# Step 2: 
library(car)

# Bartlett's Test - new test try this time
bartlett.test(Calories_Burned ~ Workout_Type, data = gym_data)

bartlett.test(gym_data$Session_Duration ~ gym_data$Workout_Type)

# Perform One-Way ANOVA for Session Duration across Workout Types
anova_session_duration <- aov(gym_data$Session_Duration ~ gym_data$Workout_Type, data = gym_data)
cat("\nOne-Way ANOVA for Session Duration:\n")
summary(anova_session_duration)

# Perform One-Way ANOVA for Calories Burned across Workout Types
anova_calories_burned <- aov(gym_data$Calories_Burned ~ gym_data$Workout_Type, data = gym_data)
cat("\nOne-Way ANOVA for Calories Burned:\n")
summary(anova_calories_burned)

#Research Question 3
#Checking assumptions
# Fit a linear model for Calories_Burned
model <- lm(Calories_Burned ~ Age + Weight + Height + Avg_BPM + Session_Duration + Workout_Type + Water_Intake, data = gym_data)
model
# Check residuals for normality (Q-Q plot)
qqnorm(resid(model))
qqline(resid(model), col = "red")

# Residuals vs Fitted plot
plot(model$fitted.values, resid(model), main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

# Fit the model
model <- lm(Calories_Burned ~ Age + Weight + Height + Avg_BPM + Session_Duration + Workout_Type + Water_Intake, data = gym_data)

# Summary of the model
summary(model)

# Full Model
full_model <- lm(Calories_Burned ~ Age + Weight + Height + Avg_BPM + Session_Duration + Workout_Type + Water_Intake, data = gym_data)

# stepwise Backward Selection
backward_model <- step(full_model, direction = "backward", trace = 1)

# Summary of the reduced model
summary(backward_model)

# Including interactions
interaction_model <- lm(Calories_Burned ~ (Age + Weight + Height + Avg_BPM + Session_Duration + Water_Intake + Workout_Type)^2, 
                        data = gym_data)
# Backward stepwise regression again to compare with no interaction
stepwise_interaction_model <- step(interaction_model, direction = "backward")
summary(stepwise_interaction_model)





