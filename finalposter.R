# Load required libraries
install.packages(ggplot2)
install.packages(dplyr)
install.packages(readr)
library(ggplot2)
library(dplyr)

# Load and prepare data
#df <- read.csv(file.choose())
colnames(df) <- gsub("\\.", "_", tolower(colnames(df)))
df$average_score <- rowMeans(df[, c("math_score", "reading_score", "writing_score")])

# Optional: map test prep to numeric for scatter layout
df$test_prep_numeric <- ifelse(df$test_preparation_course == "completed", 1, 0)

# Plot: Scatter + Regression by lunch type
ggplot(df, aes(x = test_prep_numeric, y = average_score, color = lunch)) +
  geom_jitter(width = 0.1, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, aes(group = lunch), size = 1) +
  scale_color_manual(values = c("standard" = "#8EAF9D", "free/reduced" = "#4A4A4A")) +
  scale_x_continuous(breaks = c(0, 1), labels = c("None", "Completed")) +
  labs(
    title = "Effect of Test Preparation on Average Score",
    subtitle = "Colored by Lunch Type (Socioeconomic Proxy)",
    x = "Test Preparation Course",
    y = "Average Score",
    color = "Lunch Type"
  ) +
  theme_minimal()

ggsave("main_plot.png", width = 6, height = 4, dpi = 300)

#--------------------------------------------------------------------
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)

# Load required libraries
library(ggplot2)
library(dplyr)

# Load the data
df <- read.csv(file.choose())

# Calculate average score
df$average_score <- rowMeans(df[, c("math.score", "reading.score", "writing.score")])

# Order levels of parental education
df$parental.level.of.education <- factor(df$parental.level.of.education,
                                         levels = c("some high school", "high school", "some college", 
                                                    "associate's degree", "bachelor's degree", "master's degree")
)

# Define custom color palette (6 colors)
edu_colors <- c("#D7DAE5", "#B9CDDA", "#A6D8D4", "#8EAF9D", "#6B7D7D", "#4A4A4A")

# Create the plot
ggplot(df, aes(x = parental.level.of.education, y = average_score, 
               color = parental.level.of.education)) +
  geom_jitter(width = 0.2, alpha = 0.7, size = 2.5) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", 
               linetype = "dashed", size = 1) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3, shape = 18) +
  scale_color_manual(values = edu_colors) +
  labs(title = "Parental Education vs Average Score",
       x = "Parental Level of Education",
       y = "Average Score",
       color = "Parental Education") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#--------------------------------------------------------------------

# Load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Load the data
#df <- read.csv("StudentsPerformance.csv")

# Rename columns for easier access
colnames(df) <- gsub("\\.", "_", tolower(colnames(df)))

# Convert to long format for plotting
df_long <- df %>%
  pivot_longer(cols = c(math_score, reading_score, writing_score),
               names_to = "subject", values_to = "score")

# Capitalize subject names for labels
df_long$subject <- factor(df_long$subject,
                          levels = c("math_score", "reading_score", "writing_score"),
                          labels = c("Math", "Reading", "Writing"))

# Plot score distributions by gender and subject
ggplot(df_long, aes(x = score, fill = gender)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 20) +
  facet_wrap(~ subject, scales = "free_y") +
  scale_fill_manual(values = c("#8EAF9D", "#6B7D7D")) +
  labs(title = "Score Distributions by Gender and Subject",
       x = "Score",
       y = "Count",
       fill = "Gender") +
  theme_minimal()

#-----------------------------------------------------------------

# Load required libraries
library(ggplot2)
library(dplyr)

# Load and clean data
#df <- read.csv("StudentsPerformance.csv")
colnames(df) <- gsub("\\.", "_", tolower(colnames(df)))

# Calculate average score
df$average_score <- rowMeans(df[, c("math_score", "reading_score", "writing_score")])

# Plot: Lunch Type vs Average Score
ggplot(df, aes(x = lunch, y = average_score, fill = lunch)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("standard" = "#8EAF9D", "free/reduced" = "#6B7D7D")) +
  labs(title = "Lunch Type and Average Student Performance",
       x = "Lunch Type (Socioeconomic Status Proxy)",
       y = "Average Score") +
  theme_minimal()

#-----------------------------------------------------------------
# Load libraries
library(ggplot2)
library(dplyr)

# Load and clean data
#df <- read.csv("StudentsPerformance.csv")
colnames(df) <- gsub("\\.", "_", tolower(colnames(df)))

# Calculate average score
df$average_score <- rowMeans(df[, c("math_score", "reading_score", "writing_score")])



# Set color palette for race/ethnicity
race_colors <- c(
  "group A" = "#D7DAE5",
  "group B" = "#B9CDDA",
  "group C" = "#A6D8D4",
  "group D" = "#6B7D7D",
  "group E" = "#4A4A4A"
)

# Boxplot with jittered points
ggplot(df, aes(x = race_ethnicity, y = average_score, fill = race_ethnicity)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(color = "black", size = 1.5, width = 0.2, alpha = 0.4) +
  scale_fill_manual(values = race_colors) +
  labs(
    title = "Average Score by Race/Ethnicity Group",
    x = "Race/Ethnicity",
    y = "Average Score",
    fill = "Race/Ethnicity"
  ) +
  theme_minimal()




#-----------------------------------------------------------------
#D7DAE5, #B9CDDA, #A6D8D4, #8EAF9D, #6B7D7D
