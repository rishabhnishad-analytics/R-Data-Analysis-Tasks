## Task 4: Student Performance & Academic Behavior Analytics

## Loading Required Libraries
library(readxl)
library(dplyr)
library(ggplot2)

## 1. Loading data (Using a relative path and descriptive variable name)
student_data <- read_excel("Practice_Dataset_500.xlsx", sheet = "StudentData")

## 2. Inspecting the first 10 rows of the dataset
head(student_data, 10)

## 3. Reviewing structural metadata and column data types
str(student_data)

## 4. Evaluating dataset dimensions (Rows vs Columns)
dim(student_data)
nrow(student_data)
ncol(student_data)

## 5. Listing all feature column attributes
names(student_data)

## 6. Generating descriptive summary statistics for all variables
summary(student_data)

## 7. Calculating global mean cumulative GPA (CGPA)
mean(student_data$CGPA, na.rm = TRUE)

## 8. Identifying extreme boundaries for CGPA metrics
max(student_data$CGPA, na.rm = TRUE)
min(student_data$CGPA, na.rm = TRUE)

## 9. Frequency distribution of students across academic programs
table(student_data$Program)

## 10. Demographic breakdown by gender orientation
table(student_data$Gender)

## 11. Evaluating mean attendance percentages across distinct academic programs
program_attendance <- student_data %>% 
  group_by(Program) %>% 
  summarise(avg_attendance = mean(AttendancePct, na.rm = TRUE), .groups = 'drop')
print(program_attendance)

## 12. Segmenting average CGPA metrics by gender profile
gender_cgpa <- student_data %>% 
  group_by(Gender) %>% 
  summarise(avg_cgpa = mean(CGPA, na.rm = TRUE), .groups = 'drop')
print(gender_cgpa)

## 13. Filtering cohort: identifying high-achieving students (CGPA > 8)
high_achievers <- student_data %>% 
  filter(CGPA > 8)
head(high_achievers)

## 14. Querying specific segments: BBA students maintaining over 85% attendance
target_bba_cohort <- student_data %>% 
  filter(Program == "BBA", AttendancePct > 85)
head(target_bba_cohort)

## 15. Sorting overall dataset chronologically by CGPA rank
sorted_by_cgpa <- student_data %>% 
  arrange(desc(CGPA))

## 16. Isolating the top 10 highest-performing students globally
top_10_students <- student_data %>% 
  arrange(desc(CGPA)) %>% 
  head(10)
print(top_10_students)

## 17. Feature Engineering: Calculating a composite average internal assessment score
student_data <- student_data %>% 
  mutate(TotalScore = (AssignmentScore + MidtermScore + FinalExamScore + ProjectScore) / 4)

## 18. Conditional Logic: Segmenting performance bands based on strict CGPA brackets
student_data <- student_data %>% 
  mutate(PerformanceLevel = ifelse(CGPA >= 8, "High", 
                                   ifelse(CGPA >= 6, "Medium", "Low")))

## 19. Summary breakdown of generated Performance Level cohorts
table(student_data$PerformanceLevel)

## 20. Investigating dedication metrics: mean weekly study hours by performance category
performance_study_hours <- student_data %>% 
  group_by(PerformanceLevel) %>% 
  summarise(avg_study_hours = mean(StudyHoursPerWeek, na.rm = TRUE), .groups = 'drop')
print(performance_study_hours)

## 21. Statistical Correlation: Evaluating linear relationship between study hours and CGPA
study_cgpa_corr <- cor(student_data$StudyHoursPerWeek, student_data$CGPA, use = "complete.obs")
print(study_cgpa_corr)

## 22. Data Visualization: Distributional density histogram of student CGPA metrics
ggplot(student_data, aes(x = CGPA)) + 
  geom_histogram(binwidth = 0.3, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Student Cumulative GPA", x = "CGPA", y = "Student Count")

## 23. Variance Analysis: Cross-categorical boxplot mapping CGPA across academic tracks
ggplot(student_data, aes(x = Program, y = CGPA, fill = Program)) + 
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Academic Performance Variance Across Programs", x = "Degree Program", y = "CGPA")

## 24. Linear Relationship: Scatter plot charting Attendance Density against CGPA
ggplot(student_data, aes(x = AttendancePct, y = CGPA)) + 
  geom_point(alpha = 0.6, color = "darkblue") +
  theme_minimal() +
  labs(title = "Impact of Class Attendance on CGPA", x = "Attendance Percentage", y = "CGPA")

## 25. Inferential Modeling: Fitting an Ordinary Least Squares (OLS) Linear Regression Model
attendance_model <- lm(CGPA ~ AttendancePct, data = student_data)
summary(attendance_model)

