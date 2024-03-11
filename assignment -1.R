#import the database
project <- read.csv("RPRO.csv")

project

#funcion to calculate body mass index(BMI)
body_mass_index <- function(height,weight)
{
  weight / height ^ 2
}
body_mass_index(1.67,89)

#splitting the database according to gender
f <- project[project$Gender == "Female", ]
m <- project[project$Gender == "Male", ]
f
m

# Function to calculate BMR
calculate_bmr <- function(Weight, Height, Age, Gender) {
  # Check Gender
  if (Gender == "male") {
    bmr <- 88.362 + (13.397 * Weight) + (4.799 * Height) - (5.677 * Age)
  } else if (Gender == "female") {
    bmr <- 447.593 + (9.247 * Weight) + (3.098 * Height) - (4.330 * Age)
  } else {
    stop("Invalid gender. Please specify 'male' or 'female'.")
  }
  
  # Round to two decimal places
  bmr <- round(bmr, 2)
  
  return(bmr)
}
bmr_male <- calculate_bmr(64, 167, 46, "male")
bmr_female <- calculate_bmr(72, 158, 46, "female")
bmr_female
bmr_male

#applying formula acccording to gender to calculate the mainetenance weight
f$maintenance <- 447.593 + (9.247 * f$Weight) + (3.098 * f$Height) - (4.330 * f$Age)
f
m$maintenance <- 88.362 + (13.397 * m$Weight) + (4.799 * m$Height) - (5.677 * m$Age)
m

#combining the two subsets back into one table
x <- rbind(f,m)

#randomizing the rows
b <- sample(nrow(x))
finally <- x[b, ]
finally

#applying formula to calculate BMR
finally$BMR <- finally$Activity.level * finally$maintenance
finally


library(dplyr)
dplyr

#calculating average BMI
average_fvalue <- mean(f$Bmi)
average_fvalue

average_mvalue <- mean(m$Bmi)
average_mvalue

#function to calulate weight change in 1 month
weight_change <- function(calories){
  ((calories * 30)/3500) * 0.45
}
weight_change(-450)

finally$calorie_difference <- finally$Calorie.intake - finally$BMR
finally

finally$weight_difference <- ((finally$difference * 30)/3500) * 0.45
finally

install.packages("ggplot2")
library("ggplot2")


# creating multiple bar plots in R
library(ggplot2)

# creating a dummy dataset
bmI <- finally$Bmi

AGE <- c(15,16,17,18,19,20,21,22)

gender <- finally$Gender

# creating data frame 
circle <- data.frame(bmI,AGE,gender)

# creating plot using the above data
ggplot(circle, aes(AGE, bmI, fill = gender)) +
  geom_bar(stat="identity", position = "dodge") + 
  labs(title="Multiple Bar plots")

#3D scatterplot representing age,BMI and BMR
plot3d(finally$Age, finally$Bmi, finally$Activity.level, col = "blue", size = 1, type = 's',
       xlab = "Age", ylab = "BMI", zlab = "Activity Level")bkjkjnskjb qk sgwi

#rearrangeing the rows according to age in ascending order
finally <- finally %>% arrange(Age)
finally

