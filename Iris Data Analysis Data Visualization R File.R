# Name : Marcelline Cathrine Wilison
# NIM : 2702210604
# Class : LG09
# Individual Assignment Data Mining and Visualization : Code (R)
# Due Date : 17 Apr 2024, 23:00

# Load All the Libraries Needed
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)

# Load the Iris Dataset
iris

# Show Top 6 Rows from the Dataset
head(iris)

# Show the Details of the Dataset
str(iris)

# 1. Data Manipulation
# 1.1. Select Specific Column
# 1.1.1. Select Sepal Length Column
SepalLength <- iris %>%
  select(., Sepal.Length)
# Display the Selected Column
print(SepalLength)

# 1.1.2. Select Sepal Width Column
SepalWidth <- iris %>%
  select(., Sepal.Width)
# Display the Selected Column
print(SepalWidth)

# 1.1.3. Select Petal Length Column
PetalLength <- iris %>%
  select(., Petal.Length)
# Display the Selected Column
print(PetalLength)

# 1.1.4. Select Petal Width Column
PetalWidth <- iris %>%
  select(., Petal.Width)
# Display the Selected Column
print(PetalWidth)

# 1.1.5. Select Species Column
Species <- iris %>%
  select(., Species)
# Display the Selected Column
print(Species)

# 1.2. Rename Columns
# 1.2.1. Rename Sepal Length Column
a <- iris %>%
  rename(., "Sepal(L)" = Sepal.Length)
# Display the Dataset with Renamed Column
print(a)

# 1.2.2. Rename Sepal Width Column
b <- iris %>%
  rename(., "Sepal(W)" = Sepal.Width)
# Display the Dataset with Renamed Column
print(b)

# 1.2.3. Rename Petal Length Column
c <- iris %>%
  rename(., "Petal(L)" = Petal.Length)
# Display the Dataset with Renamed Column
print(c)

# 1.2.4. Rename Petal Width Column
d <- iris %>%
  rename(., "Petal(W)" = Petal.Width)
# Display the Dataset with Renamed Column
print(d)

# 1.2.5. Rename Species Column
e <- iris %>%
  rename(., "Iris Species" = Species)
# Display the Dataset with Renamed Column
print(e)

# 1.2.6. Rename All Columns
# Check the names of the columns
names(iris)

# Change the names
names(iris) = c("Sepal(L)", "Sepal(W)", "Petal(L)", "Petal(W)", "Iris Species")
# Display the Dataset with Renamed Columns
print(iris)

# 1.3. Handle Missing Values
# Check total of missing values
print(sum(is.na(iris)))

# Since there are no missing values, there is none to be handled
# If there are missing values, there are 2 choices to handle it
# 1.3.1. Delete the data
deleted_na <- iris %>%
  drop_na(.)
# Display the clean dataset (but it has no difference with the original iris dataset, since it has no NA values)
print(deleted_na)

# OR

deleted_na_2 <- iris %>%
  na.omit(.)
# Display the clean dataset (but it has no difference with the original iris dataset, since it has no NA values)
print(deleted_na_2)

# 1.3.2. Replace the values
# 1.3.2.1. Numerical Values
# Replace values with mean
clean_iris <- iris %>%
  mutate_if(., is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .))

# Display the replaced dataset (but it has no difference with the original iris dataset, since it has no NA values)
print(clean_iris)
# Replace values with median
clean_iris_2 <- iris %>%
  mutate_if(., is.numeric, ~ifelse(is.na(.), median(., na.rm = TRUE), .))

# Display the replaced dataset (but it has no difference with the original iris dataset, since it has no NA values)
print(clean_iris_2)

# 1.3.2.2. Categorical Variables
# Make a new column to store categorical variable in numeric
clean_iris$Species <- clean_iris$`Iris Species` %>%
  as.integer(.)

# Search for the mode
mode <- clean_iris$Species %>%
  table(.) %>%
  which.max(.) %>%
  names(.) %>%
  as.integer(.)

# Replace the NA values
clean_iris$Species <- clean_iris$Species %>%
  nafill(., fill = mode)

# Change the values back to string
clean_iris$Species <- clean_iris$Species %>%
  as.factor(.)

levels(clean_iris$Species) = levels(clean_iris$`Iris Species`)

# Remove the raw column
clean_iris <- clean_iris[, -which(names(clean_iris) == "Iris Species")]

# Display the replaced dataset (but it has no difference with the original iris dataset, since it has no NA values)
print(clean_iris)

# 1.4. Remove or Adjust Outliers
# Sepal Length Column
# Find the interquartile range
q1_sl <- quantile(iris$`Sepal(L)`, probs = c(0.25))
q3_sl <- quantile(iris$`Sepal(L)`, probs = c(0.75))
iqr_sl <- q3_sl - q1_sl

# Calculate the lower and upper bounds
lb_sl <- round(q1_sl - 1.5 * iqr_sl, digits = 1)
ub_sl <- round(q3_sl + 1.5 * iqr_sl, digits = 1)

# Find the outlier positions
outliers_low_sl <- which(iris$`Sepal(L)` < lb_sl)
outliers_high_sl <- which(iris$`Sepal(L)` > ub_sl)

# Adjust Outliers
iris <- iris %>%
  mutate(., `Sepal(L)` = ifelse(row_number() %in% outliers_low_sl, lb_sl, `Sepal(L)`))
iris <- iris %>%
  mutate(., `Sepal(L)` = ifelse(row_number() %in% outliers_high_sl, ub_sl, `Sepal(L)`))

# Display the Adjusted Dataset
print(iris)

# Sepal Width Column
# Find the interquartile range
q1_sw <- quantile(iris$`Sepal(W)`, probs = c(0.25))
q3_sw <- quantile(iris$`Sepal(W)`, probs = c(0.75))
iqr_sw <- q3_sw - q1_sw

# Calculate the lower and upper bounds
lb_sw <- round(q1_sw - 1.5 * iqr_sw, digits = 1)
ub_sw <- round(q3_sw + 1.5 * iqr_sw, digits = 1)

# Find the outlier positions
outliers_low_sw <- which(iris$`Sepal(W)` < lb_sw)
outliers_high_sw <- which(iris$`Sepal(W)` > ub_sw)

# Adjust Outliers
iris <- iris %>%
  mutate(., `Sepal(W)` = ifelse(row_number() %in% outliers_low_sw, lb_sw, `Sepal(W)`))
iris <- iris %>%
  mutate(., `Sepal(W)` = ifelse(row_number() %in% outliers_high_sw, ub_sw, `Sepal(W)`))

# Display the Adjusted Dataset
print(iris)

# Petal Length Column
# Find the interquartile range
q1_pl <- quantile(iris$`Petal(L)`, probs = c(0.25))
q3_pl <- quantile(iris$`Petal(L)`, probs = c(0.75))
iqr_pl <- q3_pl - q1_pl

# Calculate the lower and upper bounds
lb_pl <- round(q1_pl - 1.5 * iqr_pl, digits = 1)
ub_pl <- round(q3_pl + 1.5 * iqr_pl, digits = 1)

# Find the outlier positions
outliers_low_pl <- which(iris$`Petal(L)` < lb_pl)
outliers_high_pl <- which(iris$`Petal(L)` > ub_pl)

# Adjust Outliers
iris <- iris %>%
  mutate(., `Petal(L)` = ifelse(row_number() %in% outliers_low_pl, lb_pl, `Petal(L)`))
iris <- iris %>%
  mutate(., `Petal(L)` = ifelse(row_number() %in% outliers_high_pl, ub_pl, `Petal(L)`))

# Display the Adjusted Dataset
print(iris)

# Petal Width Column
# Find the interquartile range
q1_pw <- quantile(iris$`Petal(W)`, probs = c(0.25))
q3_pw <- quantile(iris$`Petal(W)`, probs = c(0.75))
iqr_pw <- q3_pw - q1_pw

# Calculate the lower and upper bounds
lb_pw <- round(q1_pw - 1.5 * iqr_pw, digits = 1)
ub_pw <- round(q3_pw + 1.5 * iqr_pw, digits = 1)

# Find the outlier positions
outliers_low_pw <- which(iris$`Petal(W)` < lb_pw)
outliers_high_pw <- which(iris$`Petal(W)` > ub_pw)

# Adjust Outliers
iris <- iris %>%
  mutate(., `Petal(W)` = ifelse(row_number() %in% outliers_low_pw, lb_pw, `Petal(W)`))
iris <- iris %>%
  mutate(., `Petal(W)` = ifelse(row_number() %in% outliers_high_pw, ub_pw, `Petal(W)`))

# Display the Adjusted Dataset
print(iris)

# 2. Data Exploration
# 2.1. Generate Statistical Summary
# Get statistical summary of each column
statsum <- iris %>%
  summary()

# Display the statistical summaries
print(statsum)

# 2.2. Generate Histogram
# Sepal Length Histogram
# Generate histogram
hist_1 <- iris %>%
  ggplot(., aes(x = `Sepal(L)`, fill = `Iris Species`)) +
  geom_histogram(color = 'black', bins = 15) +
  labs(title = "Sepal Length Distribution", x = "Sepal Length", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

# Display histogram
print(hist_1)

# Sepal Width Histogram
# Generate histogram
hist_2 <- iris %>%
  ggplot(., aes(x = `Sepal(W)`, fill = `Iris Species`)) +
  geom_histogram(color = 'black', bins = 15) +
  labs(title = "Sepal Width Distribution", x = "Sepal Width", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

# Display histogram
print(hist_2)

# Petal Length Histogram
# Generate histogram
hist_3 <- iris %>%
  ggplot(., aes(x = `Petal(L)`, fill = `Iris Species`)) +
  geom_histogram(color = 'black', bins = 15) +
  labs(title = "Petal Length Distribution", x = "Petal Length", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

# Display histogram
print(hist_3)

# Petal Width Histogram
# Generate histogram
hist_4 <- iris %>%
  ggplot(., aes(x = `Petal(W)`, fill = `Iris Species`)) +
  geom_histogram(color = 'black', bins = 15) +
  labs(title = "Petal Width Distribution", x = "Petal Width", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

# Display histogram
print(hist_4)

# 2.3. Generate Scatter Plot
# Sepal Length and Sepal Width Scatter Plot
# Generate scatter plot
splot_slsw <- iris %>%
  ggplot(., aes(x = `Sepal(L)`, y = `Sepal(W)`, color = `Iris Species`)) +
  geom_point() +
  labs(title = "Sepal Length vs Sepal Width", x = "Sepal Length", y = "Sepal Width") +
  theme(plot.title = element_text(hjust = 0.5))

# Display the scatter plot
print(splot_slsw)

# Petal Length and Petal Width Scatter Plot
# Generate scatter plot
splot_plpw <- iris %>%
  ggplot(., aes(x = `Petal(L)`, y = `Petal(W)`, color = `Iris Species`)) +
  geom_point() +
  labs(title = "Petal Length vs Petal Width", x = "Petal Length", y = "Petal Width") +
  theme(plot.title = element_text(hjust = 0.5))

# Display the scatter plot
print(splot_plpw)

# Sepal Length and Petal Length Scatter Plot
# Generate scatter plot
splot_slpl <- iris %>%
  ggplot(., aes(x = `Sepal(L)`, y = `Petal(L)`, color = `Iris Species`)) +
  geom_point() +
  labs(title = "Sepal Length vs Petal Length", x = "Sepal Length", y = "Petal Length") +
  theme(plot.title = element_text(hjust = 0.5))

# Display the scatter plot
print(splot_slpl)

# Sepal Width and Petal Width Scatter Plot
# Generate scatter plot
splot_swpw <- iris %>%
  ggplot(., aes(x = `Sepal(W)`, y = `Petal(W)`, color = `Iris Species`)) +
  geom_point() +
  labs(title = "Sepal Width vs Petal Width", x = "Sepal Width", y = "Petal Width") +
  theme(plot.title = element_text(hjust = 0.5))

# Display the scatter plot
print(splot_swpw)

# 3. Correlation Analysis
# 3.1. Pearson Correlation
# Check Normality of Each Column
shapiro.test(iris$`Sepal(L)`)
shapiro.test(iris$`Sepal(W)`)
shapiro.test(iris$`Petal(L)`)
shapiro.test(iris$`Petal(W)`)

# Normalize the Sepal Length Column
iris$`Sepal(L)`
iris$`Sepal(L)` <- scale(iris$`Sepal(L)`) %>% 
  round(., digits = 1)

# Normalize the Petal Length Column
iris$`Petal(L)`
iris$`Petal(L)` <- scale(iris$`Petal(L)`) %>% 
  round(., digits = 1)

# Normalize the Petal Width Column
iris$`Petal(W)`
iris$`Petal(W)` <- scale(iris$`Petal(W)`) %>% 
  round(., digits = 1)

# Sepal Length and Sepal Width
# Test the correlation
slsw <- cor.test(iris$`Sepal(L)`, iris$`Sepal(W)`, method = "pearson")
# Display the correlation coefficient
print(slsw$estimate)

plot(iris$`Petal(L)`, iris$`Petal(W)`)

# Petal Length and Petal Width
# Test the correlation
plpw <- cor.test(iris$`Petal(L)`, iris$`Petal(W)`, method = "pearson")
# Display the correlation coefficient
print(plpw$estimate)

# Sepal Length and Petal Length
# Test the correlation
slpl <- cor.test(iris$`Sepal(L)`, iris$`Petal(L)`, method = "pearson")

# Display the correlation coefficient
print(slpl$estimate)

# Sepal Width and Petal Width
# Test the correlation
swpw <- cor.test(iris$`Sepal(W)`, iris$`Petal(W)`, method = "pearson")

# Display the correlation coefficient
print(swpw$estimate)

# 4. Linear Regression
# 4.1. Simple Linear Regression (Sepal Length as dependent, Petal Length and Petal Width as independent)
# y = a + bx1 + bx2 (x1 petal length, x2 petal width)
# Perform linear regression
linearreg <- iris %>%
  lm(formula = `Sepal(L)` ~ `Petal(L)` + `Petal(W)`, .)

# Display the summary
print(summary(linearreg))

# Make the regression plot
# Sepal Length and Petal Length
reg_slpl <- iris %>%
  ggplot(., aes(x = `Petal(L)`, y = `Sepal(L)`, color = `Iris Species`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = '#C6BB5E') +
  labs(title = "Linear Regression : Sepal Length vs Petal Length", x = "Petal Length", y = "Sepal Length")

# Display the regression plot
print(reg_slpl)

# Sepal Length and Petal Width
reg_slpw <- iris %>%
  ggplot(., aes(x = `Petal(W)`, y = `Sepal(L)`, color = `Iris Species`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = '#C6BB5E') +
  labs(title = "Linear Regression : Sepal Length vs Petal Width", x = "Petal Width", y = "Sepal Length")

# Display the regression plot
print(reg_slpw)

# 5. Hypothesis Testing (Petal Length)
# 5.1. Test that b != 0
# Slice the p-value from the summary
pl_p_value <- summary(linearreg)$coefficients["`Petal(L)`", "Pr(>|t|)"]

# Display the p-value
print(pl_p_value)
