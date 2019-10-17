library(tidyverse)
library(readr)
# Print the first rows of the data
comics <- read_csv('../DataScience/Exploratory data analysis/comics.csv', col_types = cols(
                   id    = col_factor(),
                   align = col_factor(),
                   hair  = col_factor(),
                   gender= col_factor(),
                   alive = col_factor() 
                   ))

# Check levels of align
levels(comics$align)

# Check the levels of gender
levels(comics$gender)

# Create a 2-way contingency table
table(comics$align, comics$gender)



