# Imporing libraries
library(psych) # Load the psych package
library(readxl) # Load the readxl package
library(dplyr)
library(corrplot)
library(ggplot2)
library(car)

# load the file without headers
data <- read_excel("C:\\Users\\girir\\OneDrive - University of Salford\\assignment asdv\\R assignments\\Energy Efficiency Data.xlsx", col_names = FALSE)

# Set column names and remove the first row
colnames(data) <- data[1, ]
data <- data[-1, ]

# Convert to data frame and replace spaces in column names
data <- as.data.frame(data)
colnames(data) <- gsub(" ", "_", colnames(data))

# EDA ---------------------------------------------------------------------------------------------------

# Check the structure and basic summary statistics
summary(data)

# Describing data
describe(data)

# Checking data types
str(data)

# Convert all columns to numeric
data <- data %>% mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))

# Check for missing values
colSums(is.na(data))

# Box plot For Relative Compactness 
boxplot(data$Relative_Compactness, main = "Boxplot of Relative Compactness", ylab = "Relative Compactness")
boxplot(data$Heating_Load, main = "Boxplot of Heating Load", ylab = "Heating Load")

# Density plot for glazing area
plot(density(data$Glazing_Area), main = "Density Plot of Glazing Area", xlab = "Glazing Area")

# Plot histograms for Heating Load and Cooling Load
ggplot(data, aes(x = Heating_Load)) + 
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  ggtitle("Distribution of Heating Load") + theme_minimal()

ggplot(data, aes(x = Cooling_Load)) + 
  geom_histogram(binwidth = 2, fill = "lightgreen", color = "black") +
  ggtitle("Distribution of Cooling Load") + theme_minimal()

# correlation analysis--------------------------------------------------------------------------------------------------------------

# Calculate and round the pearson correlation matrix
pearson_corr <- round(cor(data, method = "pearson", use = "complete.obs"), digits = 2)
summary(pearson_corr)
print(pearson_corr)

# Visualize the correlation matrix
corrplot(pearson_corr, method = "color", type = "full", 
         tl.col = "black", tl.srt = 45,          # Label color and rotation
         addCoef.col = "black",                  # Add correlation coefficients in black
         number.cex = 0.7,                       # Size of the correlation coefficients
         col = colorRampPalette(c("red", "white", "blue"))(200)) # Color gradient

# Pearson correlation test
correlation_test <- cor.test(data$Heating_Load, data$Cooling_Load, method = "pearson")
print(correlation_test)
# Scatter plot for linearity
plot(data$Heating_Load, data$Cooling_Load, main = "Scatterplot of Heating Load vs. Cooling Load", xlab = "Heating Load", ylab = "Cooling Load")

# Normality TESING------------------------------------------------------------------------------------------------------------------

ggplot(data = data, aes(sample = Heating_Load)) +
  geom_qq(size = 2, color = "blue") +   # This creates the QQ plot points
  geom_qq_line(color = "red")            # Adds the reference line


# Replace 'data' with the actual name of your dataset
ggplot(data = data, aes(sample = Cooling_Load)) +
  geom_qq(size = 2, color = "blue") +   # This creates the QQ plot points
  geom_qq_line(color = "red")            # Adds the reference line


# Shapiro-Wilk normality test
shapiro.test(data$Heating_Load)
shapiro.test(data$Cooling_Load)

# applying log transform 
data$Cooling_Load_log <- log(data$Cooling_Load + 1)  # Add 1 to avoid log(0)
data$Heating_Load_log <- log(data$Heating_Load + 1)

# normality test (SW)
shapiro.test(data$Heating_Load_log)
shapiro.test(data$Cooling_Load_log)


# REGRESSION--------------------------------------------------------------------------------------------------------------

# Train MLR models with selected features
mlr_model_heating <- lm(Heating_Load_log ~ Surface_Area + Wall_Area + Roof_Area + Glazing_Area + Glazing_Area_Distribution, data = data)
mlr_model_cooling <- lm(Cooling_Load_log ~ Surface_Area + Wall_Area + Roof_Area + Glazing_Area + Glazing_Area_Distribution, data = data)
summary(mlr_model_heating)
summary(mlr_model_cooling)

# Predictions
mlr_pred_heating <- predict(mlr_model_heating, data)
mlr_pred_cooling <- predict(mlr_model_cooling, data)

# RMSE
cat("MLR RMSE for Heating Load:", sqrt(mean((mlr_pred_heating - data$Heating_Load_log)^2)), "\n")
cat("MLR RMSE for Cooling Load:", sqrt(mean((mlr_pred_cooling - data$Cooling_Load_log)^2)), "\n")

# Diagnostic Plots
par(mfrow = c(2, 2)) # Set up 2x2 grid
plot(mlr_model_heating, main = "Heating Load Diagnostics") # Diagnostic plots for heating
plot(mlr_model_cooling, main = "Cooling Load Diagnostics") # Diagnostic plots for cooling
par(mfrow = c(1, 1)) # Reset plotting layout

# Load library
library(gbm)

# Train GBM models for Heating and Cooling Load using selected features
gbm_model_heating <- gbm(Heating_Load_log ~ Surface_Area + Wall_Area + Roof_Area + Glazing_Area + Glazing_Area_Distribution, 
                         data = data, distribution = "gaussian", n.trees = 100, interaction.depth = 3, 
                         shrinkage = 0.01, cv.folds = 5)

gbm_model_cooling <- gbm(Cooling_Load_log ~ Surface_Area + Wall_Area + Roof_Area + Glazing_Area + Glazing_Area_Distribution, 
                         data = data, distribution = "gaussian", n.trees = 100, interaction.depth = 3, 
                         shrinkage = 0.01, cv.folds = 5)
# Predictions
gbm_pred_heating <- predict(gbm_model_heating, data, n.trees = 100)
gbm_pred_cooling <- predict(gbm_model_cooling, data, n.trees = 100)

# RMSE
cat("GBM RMSE for Heating Load:", sqrt(mean((gbm_pred_heating - data$Heating_Load_log)^2)), "\n")
cat("GBM RMSE for Cooling Load:", sqrt(mean((gbm_pred_cooling - data$Cooling_Load_log)^2)), "\n")

# Load necessary library
library(e1071)
# Train SVR model
svr_model_heating <- svm(Heating_Load_log ~ Surface_Area + Wall_Area + Roof_Area + Glazing_Area + Glazing_Area_Distribution, 
                         data = data, type = "eps-regression", kernel = "radial")
svr_model_cooling <- svm(Cooling_Load_log ~ Surface_Area + Wall_Area + Roof_Area + Glazing_Area + Glazing_Area_Distribution, 
                         data = data, type = "eps-regression", kernel = "radial")

# Predictions
svr_pred_heating <- predict(svr_model_heating, data)
svr_pred_cooling <- predict(svr_model_cooling, data)

# RMSE
cat("SVR RMSE for Heating Load:", sqrt(mean((svr_pred_heating - data$Heating_Load_log)^2)), "\n")
cat("SVR RMSE for Cooling Load:", sqrt(mean((svr_pred_cooling - data$Cooling_Load_log)^2)), "\n")
                                             
# Create residuals for SVR models
residuals_heating <- svr_pred_heating - data$Heating_Load_log
residuals_cooling <- svr_pred_cooling - data$Cooling_Load_log

# Plot Actual vs Predicted and Residuals vs Fitted
library(gridExtra)

grid.arrange(
  ggplot(data.frame(Actual = data$Heating_Load_log, Predicted = svr_pred_heating), aes(x = Actual, y = Predicted)) + geom_point() + geom_abline(intercept = 0, slope = 1, color = "red") + ggtitle("Heating Load: Actual vs Predicted"),
  ggplot(data.frame(Fitted = svr_pred_heating, Residuals = residuals_heating), aes(x = Fitted, y = Residuals)) + geom_point() + geom_hline(yintercept = 0, color = "red") + ggtitle("Heating Load: Residuals vs Fitted"),
  ggplot(data.frame(Actual = data$Cooling_Load_log, Predicted = svr_pred_cooling), aes(x = Actual, y = Predicted)) + geom_point() + geom_abline(intercept = 0, slope = 1, color = "red") + ggtitle("Cooling Load: Actual vs Predicted"),
  ggplot(data.frame(Fitted = svr_pred_cooling, Residuals = residuals_cooling), aes(x = Fitted, y = Residuals)) + geom_point() + geom_hline(yintercept = 0, color = "red") + ggtitle("Cooling Load: Residuals vs Fitted"),
  ncol = 2
)


# Hypothese testing--------------------------------------------------------------------------------------------------------------

# Define threshold for Surface_Area based on median
threshold <- median(data$Surface_Area)

# Subset data for Heating Load for Surface Area above and below the median
group1_surface_heating <- data[data$Surface_Area > threshold, "Heating_Load"]
group2_surface_heating <- data[data$Surface_Area <= threshold, "Heating_Load"]

# Perform Mann-Whitney U test for Heating Load
wilcox.test(group1_surface_heating, group2_surface_heating)

# Subset data for Cooling Load for Surface Area above and below the median
group1_surface_cooling <- data[data$Surface_Area > threshold, "Cooling_Load"]
group2_surface_cooling <- data[data$Surface_Area <= threshold, "Cooling_Load"]

# Perform Mann-Whitney U test for Cooling Load
wilcox.test(group1_surface_cooling, group2_surface_cooling)

# Kruskal-Wallis test for Heating Load by Orientation
kruskal.test(Heating_Load ~ as.factor(Orientation), data = data)

# Kruskal-Wallis test for Cooling Load by Orientation
kruskal.test(Cooling_Load ~ as.factor(Orientation), data = data)

# Convert Wall_Area to categories based on quantiles
data$Wall_Area_Category <- cut(data$Wall_Area, 
                               breaks = quantile(data$Wall_Area, probs = 0:4/4), 
                               include.lowest = TRUE, 
                               labels = c("Low", "Medium-Low", "Medium-High", "High"))

# Kruskal-Wallis test for Wall_Area by Glazing_Area_Distribution
kruskal.test(Wall_Area ~ as.factor(Glazing_Area_Distribution), data = data)





