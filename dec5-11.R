#### packages used ####
library(tidyverse)
library(readr)

#### dec 5 ####
# hardest for me so far
# never worked in binary, so I was definitely not thinking in the most efficient way possible
# consequence: this is not a clean code, but I learned so much from other people's code afterwards (e.g. strtoi()) !
# eh, I guess that's the whole point of AdventOfCode !
rows <- read_table("data/seats.txt", col_names = "seats") %>% 
  slice(rep(0:n(), each = 128)) %>% 
  group_by(seats) %>% 
  mutate(row = row_number()-1) %>% 
  filter(ifelse((str_sub(seats, 1, 1) == "F"), row_number() <= 64, row_number() > 64)) %>% 
  filter(ifelse((str_sub(seats, 2, 2) == "F"), row_number() <= 32, row_number() > 32)) %>% 
  filter(ifelse((str_sub(seats, 3, 3) == "F"), row_number() <= 16, row_number() > 16)) %>% 
  filter(ifelse((str_sub(seats, 4, 4) == "F"), row_number() <= 8, row_number() > 8)) %>% 
  filter(ifelse((str_sub(seats, 5, 5) == "F"), row_number() <= 4, row_number() > 4)) %>% 
  filter(ifelse((str_sub(seats, 6, 6) == "F"), row_number() <= 2, row_number() > 2)) %>% 
  filter(ifelse((str_sub(seats, 7, 7) == "F"), row_number() <= 1, row_number() > 1))

seats <- read_table("data/seats.txt", col_names = "seats") %>% 
  slice(rep(0:n(), each = 8)) %>% 
  group_by(seats) %>% 
  mutate(column = row_number()-1) %>% 
  filter(ifelse((str_sub(seats, 8, 8) == "L"), row_number() <= 4, row_number() > 4)) %>% 
  filter(ifelse((str_sub(seats, 9, 9) == "L"), row_number() <= 2, row_number() > 2)) %>% 
  filter(ifelse((str_sub(seats, 10, 10) == "L"), row_number() <= 1, row_number() > 1)) %>% 
  full_join(rows) %>% 
  mutate(ID = row*8+column)

# 2nd problem
# this was easier for me
setdiff(min(seats$ID):max(seats$ID), seats$ID)

#### dec 6 ####
forms <- read.table("data/forms.txt",  blank.lines.skip=F) %>% 
  mutate(V1 = ifelse(V1 == "", 0, as.character(V1)),
         ID = ifelse(V1 == 0, row_number(), NA)) %>% 
  fill(ID, .direction = "up") %>% 
  filter(V1 != 0) %>% 
  mutate(letters = strsplit(V1, "")) %>% 
  unnest(letters) %>% 
  distinct(ID, letters) %>% 
  group_by(ID) %>% 
  mutate(sum = sum(str_count(letters))) %>% # I could stop here and count the number of rows, which is equal to the sum of unique 'yes' answers
  ungroup() %>% 
  distinct(ID, sum)

sum(forms$sum)

# 2nd problem
forms <- read.table("data/forms.txt",  blank.lines.skip=F) %>% 
  mutate(V1 = ifelse(V1 == "", 0, as.character(V1)),
         ID = ifelse(V1 == 0, row_number(), NA)) %>% 
  fill(ID, .direction = "up") %>% # ID for groups
  mutate(ID_person = row_number()) %>% 
  filter(V1 != 0) %>% 
  group_by(ID) %>% 
  mutate(nb_people = n()) %>% # calculate nb of people in each group
  ungroup() %>% 
  mutate(letters = strsplit(V1, "")) %>% # split strings into individual letters
  unnest(letters) %>% # make each character (letter) its own row 
  group_by(ID, letters) %>% # group by letter
  mutate(sum_letters = n()) %>% # to find how many times each letter has been used by each group
  ungroup() %>% 
  distinct(ID, nb_people, letters, sum_letters) %>% 
  mutate(match = ifelse(nb_people == sum_letters, 1, 0)) %>% # identify the letters that match the number of people
  filter(match == 1) # keep only the letters that match

nrow(forms)






