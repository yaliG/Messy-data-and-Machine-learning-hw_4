library(readr)
library(dplyr)
library(ROCR)
library(tidyr)
library(ggplot2)

set.seed(2048)
sqf_full <- read_csv("./data_hw4/sqf_08_16.csv")

# B1.2
# Create a df for the names of columns
relevant_cols <- c('id', 'year', 
                   'found.weapon', 'suspect.age', 'suspect.build', 
                   'suspect.sex', 'suspect.height', 'suspect.weight',
                   'stopped.bc.desc', 'stopped.bc.violent', 'stopped.bc.other', 
                   'stopped.bc.object', 'stopped.bc.casing', 
                   'stopped.bc.lookout', 'stopped.bc.drugs', 
                   'stopped.bc.clothing', 'stopped.bc.furtive', 
                   'stopped.bc.bulge', 
                   'precinct', 'inside', 'location.housing', 
                   'observation.period', 'officer.uniform', 'additional.report', 
                   'additional.investigation', 'additional.proximity', 
                   'additional.evasive', 'additional.associating', 
                   'additional.direction', 'additional.highcrime', 
                   'additional.time', 'additional.sights', 'additional.other', 
                   'radio.run', 'day', 'month', 'time.period')

sqf_data <- sqf_full %>% 
        filter(year >= 2013 & year <= 2015 & suspected.crime == "cpw") %>% 
        select(relevant_cols) %>% 
        filter(complete.cases(.)) %>% 
        mutate(precinct = factor(precinct), 
               location.housing = factor(location.housing), 
               suspect.build = factor(suspect.build),
               suspect.sex = factor(suspect.sex),
               day = factor(day),
               month = factor(month),
               time.period = factor(time.period))

# B1.3
# shuffle whole dataset
sqf_data <- sqf_data %>% slice(sample(1:n()))

# Set the split size to be 60-20-20
split_train = floor(nrow(sqf_data)*0.6)
split_valid = floor(nrow(sqf_data)*0.2)

# Create training data
train_sqf <- sqf_data %>% slice(1:split_train)

# Create validation data
validation_sqf <- sqf_data %>% slice((split_train + 1):(split_train + split_valid))

# Create testing data
test_sqf <- sqf_data %>% slice((split_train + split_valid + 1):n())

# B2.1
# Create function to get auc
get_auc <- function (x, train, test) {
        test$predicted.prob <- predict(glm(paste0("found.weapon ~ precinct +", x[1], "+", x[2]), 
                                                     family = "binomial", data = train), 
                                                 newdata = test, type = "response")
        test.pred <- prediction(test$predicted.prob, test$found.weapon)
        test.perf <- performance(test.pred, "auc")
        test.perf@y.values[[1]]
}

# Create df to store model performance for each possible combination of covariates
model_performance <- train_sqf %>% 
        select(-id, -year, -found.weapon, -precinct) %>% 
        colnames() %>% 
        combn(2) %>%    # Find all possible pairs of covariates in the dataset
        t() %>%    # Transpose rows and columns
        data.frame() %>% 
        sample_n(n()) %>%   # Re-shuffle rows
        mutate_if(is.factor, as.character) %>%    # de-factor
        rename(feature_one = X1, feature_two = X2) %>% 
        cbind(., validation_auc = apply(., 1, get_auc, train = train_sqf, test = validation_sqf))

# model_performance <- cbind(model_performance, validation_auc = apply(model_performance, 1, get_auc, a = train_sqf, b = validation_sqf))


# B2.2
# Find the set of features (plus precinct) that gets highest AUC 
max_valid_auc <- model_performance[which.max(model_performance$validation_auc), ]

# B2.3
# Combine training and validation datasets
combine_sqf <- rbind(train_sqf, validation_sqf)

# Use the same set of features (plus precinct) in B2.2 to fit model on the combined dataset, 
# then test the model on the test dataset
corr_test_auc <- get_auc(max_valid_auc, train = combine_sqf, test = test_sqf)

# B2.4
# Histogram
p2 <- ggplot(model_performance, aes(x = validation_auc)) + 
        geom_histogram(color = "steelblue", fill = "steelblue", bins = 40, alpha = 0.7)

# Two lines for AUC
p2 <- p2 + geom_vline(aes(xintercept = max_valid_auc$validation_auc, linetype = "60 %"), color = "red", size = 0.5) + 
        geom_vline(aes(xintercept = corr_test_auc, linetype = "80 %"), color = "red", size = 0.5) + 
        scale_linetype_manual(name = "AUC", values = c(2, 1)) +
        ggtitle("Distribution of AUC score for all possible combinations of covariates") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme_minimal()
p2
ggsave("./hw4_Gao_Gu_Zhou/figures/question_b2.png", p2)

# B2.5

# B3.1
sqf_pre_2015 <- sqf_data %>% 
        filter(year != 2015) %>% 
        select(-id, -year)

sqf_2015 <- sqf_data %>% 
        filter(year == 2015) %>% 
        select(-id, -year)

# B3.2
# Shuffle data for 2013 and 2014
sqf_pre_2015 <- sqf_pre_2015 %>% slice(sample(1:n()))

# Set split size to 50%
split_50 = floor(nrow(sqf_pre_2015)*0.5)

# Create traning data
sqf_pre_train <- sqf_pre_2015 %>% slice(1:split_50)

# Create testing data
sqf_pre_test <- sqf_pre_2015 %>% slice((split_50 + 1):n())

# B3.3
# Fit model on 50% of data for 2013 and 2014
mod <- glm(found.weapon ~ ., family = 'binomial', data = sqf_pre_train)

get_auc_b3 <- function (test) {
        test$predicted.prob <- predict(mod, newdata = test, type = 'response')
        test.pred <- prediction(test$predicted.prob, test$found.weapon)
        test.perf <- performance(test.pred, "auc")
        cat('the auc score is', test.perf@y.values[[1]], "\n")
}

# B3.4
# AUC when making prediction on the rest 50% of data for 2013 and 2014
get_auc_b3(sqf_pre_test)

# AUC when making prediction on full data for 2015
get_auc_b3(sqf_2015)

# B4.1
# Keep only data with suspected.crime == "cpw" and prcinct not equal 121
sqf_data_cpw <- sqf_full %>% 
        filter(suspected.crime == "cpw" & precinct != 121) %>% 
        select(relevant_cols) %>% 
        filter(complete.cases(.)) %>% 
        mutate(precinct = factor(precinct), 
               location.housing = factor(location.housing), 
               suspect.build = factor(suspect.build),
               suspect.sex = factor(suspect.sex),
               day = factor(day),
               month = factor(month),
               time.period = factor(time.period))

# Create function to get auc score for input year(s)
get_auc_2008 <- function (year) {
        auc_year <- data.frame(year = 0, auc = 0)
        for (i in 1:length(year)) {
                test <- sqf_data_cpw[which(sqf_data_cpw$year == year[i]),]
                test$predicted.prob <- predict(glm(found.weapon ~ . - id - year, family = "binomial", data = sqf_data_cpw, subset = (year == 2008)), 
                                               newdata = test, type = 'response')
                test.pred <- prediction(test$predicted.prob, test$found.weapon)
                test.perf <- performance(test.pred, "auc")
                auc_year[i,] <- c(year[i], test.perf@y.values[[1]])
        }
        auc_year
}

# Get auc score for each year over 2008
year_09_16 <- c(2009:2016)
auc_09_16 <- get_auc_2008(year_09_16)

# B4.2
p4 <- ggplot(auc_09_16, aes(x = year, y = auc)) + 
        geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
        geom_line() +
        geom_point(shape = 17) +
        ylim(0, 1) + 
        scale_x_discrete(name = "Year", limits=c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)) +
        ggtitle("AUC score for each year over 2008") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme_minimal()
p4
ggsave("./hw4_Gao_Gu_Zhou/figures/question_b4.png", p4)
