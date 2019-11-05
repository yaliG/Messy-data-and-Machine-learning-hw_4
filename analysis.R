library(readr)
library(dplyr)
library(tidyverse)

# A2.2

# Read data created in A2.1
crime <- read_csv('question_a2_1.csv')

# Create tibble for total counts for each type of crime
crime_counts <- crime %>% 
        count(crime) %>% 
        rename(count = n)

# Create index for misspellings crime types
gunfire_idx <- agrepl("gunfire", crime_counts$crime, ignore.case = T)
shooting_idx <- agrepl("shooting", crime_counts$crime, ignore.case = T)

# Correct misspelling crime types
crime_counts$crime[gunfire_idx] <- "Gunfire"
crime_counts$crime[shooting_idx] <- "Shooting"

# Re-summarize crime counts
crime_counts <- crime_counts %>% 
        group_by(crime) %>% 
        mutate(count = sum(count)) %>% 
        distinct() 

# Return crimes with top 5 frequencies
top5_crime <- crime_counts %>% 
        ungroup(count) %>% 
        arrange(desc(count)) %>% 
        top_n(5)

# Save data
write.csv(top5_crime, './hw4_Gao_Gu_Zhou/data/question_a2_2.csv')

