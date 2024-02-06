students_data <- read.csv('/home/addis/Desktop/Projects/R/StudentsPerformance.csv')
View(students_data)

# Nominal Variables
gender_freq <- table(students_data$gender)
race_freq <- table(students_data$race.ethnicity)
lunch_freq <- table(students_data$lunch)
test_prep_freq <- table(students_data$test.preparation.course)

# Ordinal Variable
parental_education_freq <- table(students_data$parental.level.of.education)
parental_education_mode <- as.character(names(which.max(parental_education_freq)))

# Ratio Variables
math_summary <- summary(students_data$math.score)
reading_summary <- summary(students_data$reading.score)
writing_summary <- summary(students_data$writing.score)

# Display the results
print("Nominal Variables:")
print(gender_freq)
print(race_freq)
print(lunch_freq)
print(test_prep_freq)

print("Ordinal Variable:")
print(parental_education_freq)
print(paste("Mode:", parental_education_mode))

print("Ratio Variables:")
print(math_summary)
print(reading_summary)
print(writing_summary)

