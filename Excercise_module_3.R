################
#This code is related to the exercises as part of EdX course on Impact Evaluation Methods with applications in low and middle-income countries
#The data set is taken from: Bertrand, M. and Mullainathan, S. (2004). Are Emily and Greg More Employable than Lakisha and Jamal? A Field Experiment on Labor Market Discrimination. American Economic Review, 94(4), 991-1013. DOI: 10.1257/0002828042002561
################

# Install packages
# install.packages("tidyverse")
# install.packages("ggplot2")

library(tidyverse)
library(ggplot2)
library(haven)

# Define the root directory path
current_dir <- getwd()

# Data must be downloaded from https://www.aeaweb.org/articles?id=10.1257/0002828042002561
data_file <- "data/lakisha_aer.dta"

# Load data
full_path <- file.path(current_dir, data_file)
lakisha_aer <- read_dta(full_path)

## Descriptive statistics
# Calculate descriptive statistics for the var "yearsexp"
summary(lakisha_aer$yearsexp)
sd(lakisha_aer$yearsexp)
var(lakisha_aer$yearsexp)


## Visualisation
# Create a histogram for the var "yearexp"
ggplot(data = lakisha_aer, aes(x = yearsexp)) + 
  geom_histogram(bins = 36)

# Create a histogram for the var "yearexp" with normal distribution overlay
ggplot(lakisha_aer, aes(x = yearsexp)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 36, fill = "light blue", color = "grey") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(lakisha_aer$yearsexp), sd = sd(lakisha_aer$yearsexp)), 
                color = "navy", linewidth = 0.5)

# Create two histograms for the var "yearexp" by race
ggplot(data = lakisha_aer, aes(x = yearsexp, fill = race)) +
  geom_histogram(alpha = 0.6, position = "dodge", bins = 36, color = "black")


## Factor variables
# Create a numeric variable "race_numeric" that takes on a value of 0 if `race=="w"` and 1 if `race=="b"
lakisha_aer$race_numeric <- factor(if_else(lakisha_aer$race == 'w', 0, 1))

# Create a new data set with a new column "race_numeric"
lakisha_aer_with_factor_var <- lakisha_aer

# Double check that the new data set and new variable exist
print(lakisha_aer_with_factor_var)
ls()
print(lakisha_aer_with_factor_var$race_numeric)

# Count how many participants were in each level of "race"
lakisha_aer %>%
  count(race)

# Count how many participants received a call depending on "race"
lakisha_aer %>%
  count(race, wt = call)

# Show full stats how many received and not received a call depending on 'race'
lakisha_aer %>%
  group_by(race) %>%
  summarise(received = sum(call), not_received = n() - sum(call))
