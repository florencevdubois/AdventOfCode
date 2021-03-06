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

# 2nd problem
seq1 = seq(from = 1, to = nrow(trees)*1, by = 1)
seq2 = seq(from = 1, to = nrow(trees)*3, by = 3)
seq3 = seq(from = 1, to = nrow(trees)*5, by = 5)
seq4 = seq(from = 1, to = nrow(trees)*7, by = 7)
seq5 = c(seq(from = 1, to = nrow(trees)*.5, by = .5), nrow(trees)*.5+.5)

# this also works for seq 5
# seq5 = rep(1:nrow(trees)*.5, each=2)*2 
# seq5 = seq5[-646:-324]

t <- trees %>% 
  mutate(trees = str_dup(trees, 1000),
         n1 = seq1,
         n2 = seq2, 
         n3 = seq3, 
         n4 = seq4, 
         n5 = seq5) %>% 
  gather(seq, number, n1:n5) %>% 
  filter(!(seq == "n5" & row_number() %% 2 == 0)) %>% # remove every other row for the fifth slope
  mutate(tree = ifelse(str_detect(str_sub(trees, number, number), "#"), 1, 0)) %>% # identifying moments when crashing into a tree
  group_by(seq) %>% 
  mutate(sum = sum(tree)) %>% # summing the crashes
  ungroup() %>% 
  distinct(seq, sum) %>% 
  spread(seq, sum) %>% 
  mutate(produce = n1*n2*n3*n4*n5)

t$produce

#### Dec 4 ####
passports <- read.table("data/passports.txt",  blank.lines.skip=F, comment.char = "") # the # characters were sneaky here!

passp <- passports %>% 
  mutate(V1 = ifelse(V1 == "", 0, as.character(V1)),
         ID = ifelse(V1 == 0, row_number(), NA)) %>% 
  fill(ID, .direction = "up") %>% 
  filter(V1 != 0) %>% 
  group_by(ID) %>% 
  gather(value, key, V1:V5) %>% 
  filter(key != "") %>% 
  mutate(valid = ifelse(str_detect(key, "byr|iyr|eyr|hgt|hcl|ecl|pid"),1,0),
         sum = sum(valid)) %>% 
  ungroup() %>% 
  distinct(ID, sum) %>% 
  filter(sum == 7)

# 2nd problem
passp <- passports %>% 
  mutate(V1 = ifelse(V1 == "", 0, as.character(V1)),
         ID = ifelse(V1 == 0, row_number(), NA)) %>% 
  fill(ID, .direction = "up") %>% 
  filter(V1 != 0) %>% 
  group_by(ID) %>% 
  gather(value, key, V1:V5) %>% 
  filter(key != "") %>% 
  separate(key, c("key", "value"), ":") %>% 
  mutate(string_count = str_count(value),
         height = ifelse(key == "hgt", str_remove_all(value, "[a-z]"), NA),
         measure = ifelse(key == "hgt", str_remove_all(value, "[0-9]"), NA),
         valid_value = ifelse(key == "byr" & (value >= 1920 & value <= 2002), 1, 0),
         valid_value = ifelse(key == "iyr" & (value >= 2010 & value <= 2020), 1, valid_value),
         valid_value = ifelse(key == "eyr" & (value >= 2020 & value <= 2030), 1, valid_value),
         valid_value = ifelse(key == "hgt" & measure == "cm" & (height >= 150 & height <= 193), 1, valid_value),
         valid_value = ifelse(key == "hgt" & measure == "in" & (height >= 59 & height <= 76), 1, valid_value),
         valid_value = ifelse(key == "hcl" & str_detect(value, "#") & string_count == 7, 1, valid_value),
         valid_value = ifelse(key == "ecl" & str_detect(value, "amb|blu|brn|gry|grn|hzl|oth"), 1, valid_value),
         valid_value = ifelse(key == "pid" & string_count == 9, 1, valid_value)) %>% 
  filter(key != "cid") %>% 
  group_by(ID) %>% 
  mutate(valid_pass = sum(valid_value)) %>% 
  ungroup() %>% 
  filter(valid_pass == 7) %>% 
  distinct(ID)
  
  
  
  
  
  
  
