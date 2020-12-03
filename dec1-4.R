#### packages used ####

library(readr)
library(tidyverse)

#### Dec 1 ####
expense <- read_table("data/expense-report.txt", col_names = "expense")

# creating a df with pairs of numbers
exp_pairs <- expense %>%
  pull(expense) %>% # getting a numberical vector of number pairs
  combn(2) %>% 
  t() %>% # transposing
  as_data_frame() %>%  # making a df
  mutate(sum = V1+V2, #making the sum of each pair
         sum2020 = ifelse(sum == 2020, 1, 0), # identifying the sum that adds to 2020
         product = ifelse(sum2020 == 1, V1*V2, NA)) # getting the product

table(exp_pairs$product, useNA = "always") # checking that there is only one answer

# 1983*37 = 73371

# 2nd problem, same thing but with 3 numbers
exp_threes <- expense %>%
  pull(expense) %>% 
  combn(3) %>% 
  t() %>% 
  as_data_frame() %>%  
  mutate(sum = V1+V2+V3, 
         sum2020 = ifelse(sum == 2020, 1, 0), 
         product = ifelse(sum2020 == 1, V1*V2*V3, NA))

table(exp_threes$product, useNA = "always") 

# 401*1390*229 = 127642310

#### Dec 2 ####
passwords <- read_table("data/passwords.txt", col_names = "passwords")

passw <- passwords %>% 
  mutate(policy = str_extract_all(passwords, '.*:'),
         letter = str_extract_all(policy, "[a-z]"),
         min = str_extract_all(policy, ".*-"),
         min = str_remove_all(min, "-"),
         min = as.numeric(min),
         max = str_extract_all(policy, "-.*"),
         max = str_remove_all(max, "-"),
         max = str_remove_all(max, "[^0-9.-]"),
         max = as.numeric(max),
         code = str_remove_all(passwords, ".*:"),
         condition = str_count(code, paste(letter)),
         filled = ifelse(condition >= min & condition <= max, 1, 0)) %>% 
  filter(filled == 1)

nrow(passw)

# 2nd problem
passw <- passwords %>% 
  mutate(policy = str_extract_all(passwords, '.*:'),
         letter = str_extract_all(policy, "[a-z]"),
         position1 = str_extract_all(policy, ".*-"),
         position1 = str_remove_all(position1, "-"),
         position1 = as.numeric(position1),
         position2 = str_extract_all(policy, "-.*"),
         position2 = str_remove_all(position2, "-"),
         position2 = str_remove_all(position2, "[^0-9.-]"),
         position2 = as.numeric(position2),
         code = str_remove_all(passwords, ".*:"),
         position1_ch = str_sub(code, position1+1, position1+1),
         position2_ch = str_sub(code, position2+1, position2+1),
         either = ifelse(position1_ch == letter | position2_ch == letter, 1, 0),
         both = ifelse(position1_ch == letter & position2_ch == letter, 1, 0)) %>% 
  filter(both != 1,
         either == 1) 

nrow(passw)

#### Dec 3 ####
trees<- read_table("data/trees.txt", col_names = "trees")

seq = seq(from = 1, to = nrow(trees)*3, by = 3)

t <- trees %>% 
  mutate(pattern = str_dup(trees, 1000),
         n = seq,
         tree = str_sub(pattern, n, n),
         crash = ifelse(tree == "#", 1, 0)) %>% 
  filter(crash == 1)

nrow(t)

#### Dec 4 ####