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

#### dec 7 ####
df <- read_table("data/bags.txt", col_names = F) %>% # get colors containing shiny gold
  separate(X1, c("beg", "end"), sep = "contain") 

level0 <- df %>% 
  filter(str_detect(end, "shiny gold")) %>%
  mutate(beg = str_remove_all(beg, "[0-9]|\\bbags?\\b|\\.")) %>% 
  mutate(beg = str_trim(beg)) %>%
  distinct(beg) %>% 
  pull()

level1 <- df %>% 
  mutate(valid = str_count(beg, paste(level0, collapse = "|")),
         valid2 = str_count(end, paste(level0, collapse = "|"))) %>% 
  filter(valid >= 1 | valid2 >=  1) %>%
  mutate(beg = str_remove_all(beg, "[0-9]|\\bbags?\\b|\\.")) %>% 
  mutate(beg = str_trim(beg)) %>% 
  distinct(beg) %>% 
  pull()

level2 <- df %>% 
  mutate(valid = str_count(beg, paste(level1, collapse = "|")),
         valid2 = str_count(end, paste(level1, collapse = "|"))) %>% 
  filter(valid >= 1 | valid2 >=  1) %>%
  mutate(beg = str_remove_all(beg, "[0-9]|\\bbags?\\b|\\.")) %>% 
  mutate(beg = str_trim(beg)) %>% 
  distinct(beg) %>% 
  pull()

level3 <- df %>% 
  mutate(valid = str_count(beg, paste(level2, collapse = "|")),
         valid2 = str_count(end, paste(level2, collapse = "|"))) %>% 
  filter(valid >= 1 | valid2 >=  1) %>%
  mutate(beg = str_remove_all(beg, "[0-9]|\\bbags?\\b|\\.")) %>% 
  mutate(beg = str_trim(beg))%>% 
  distinct(beg) %>% 
  pull()

level4 <- df %>% 
  mutate(valid = str_count(beg, paste(level3, collapse = "|")),
         valid2 = str_count(end, paste(level3, collapse = "|"))) %>% 
  filter(valid >= 1 | valid2 >=  1) %>%
  mutate(beg = str_remove_all(beg, "[0-9]|\\bbags?\\b|\\.")) %>% 
  mutate(beg = str_trim(beg))%>% 
  distinct(beg) %>% 
  pull()

level5 <- df %>% 
  mutate(valid = str_count(beg, paste(level4, collapse = "|")),
         valid2 = str_count(end, paste(level4, collapse = "|"))) %>% 
  filter(valid >= 1 | valid2 >=  1) %>%
  mutate(beg = str_remove_all(beg, "[0-9]|\\bbags?\\b|\\.")) %>% 
  mutate(beg = str_trim(beg))%>% 
  distinct(beg) %>% 
  pull()

level6 <- df %>% 
  mutate(valid = str_count(beg, paste(level5, collapse = "|")),
         valid2 = str_count(end, paste(level5, collapse = "|"))) %>% 
  filter(valid >= 1 | valid2 >=  1) %>%
  mutate(beg = str_remove_all(beg, "[0-9]|\\bbags?\\b|\\.")) %>% 
  mutate(beg = str_trim(beg))%>% 
  distinct(beg) %>% 
  pull()

level7 <- df %>% 
  mutate(valid = str_count(beg, paste(level6, collapse = "|")),
         valid2 = str_count(end, paste(level6, collapse = "|"))) %>% 
  filter(valid >= 1 | valid2 >=  1) %>%
  mutate(beg = str_remove_all(beg, "[0-9]|\\bbags?\\b|\\.")) %>% 
  mutate(beg = str_trim(beg))%>% 
  distinct(beg) %>% 
  pull()

level8 <- df %>% 
  mutate(valid = str_count(beg, paste(level7, collapse = "|")),
         valid2 = str_count(end, paste(level7, collapse = "|"))) %>% 
  filter(valid >= 1 | valid2 >=  1) %>%
  mutate(beg = str_remove_all(beg, "[0-9]|\\bbags?\\b|\\.")) %>% 
  mutate(beg = str_trim(beg))%>% 
  distinct(beg) %>% 
  pull()

level9 <- df %>% 
  mutate(valid = str_count(beg, paste(level8, collapse = "|")),
         valid2 = str_count(end, paste(level8, collapse = "|"))) %>% 
  filter(valid >= 1 | valid2 >=  1) %>%
  mutate(beg = str_remove_all(beg, "[0-9]|\\bbags?\\b|\\.")) %>% 
  mutate(beg = str_trim(beg))%>% 
  distinct(beg) %>% 
  pull()

bags <- df %>% 
  mutate(valid = str_count(beg, paste(level7, collapse = "|")),
         valid2 = str_count(end, paste(level7, collapse = "|"))) %>% 
  filter(valid >= 1 | valid2 >=  1) %>%
  mutate(beg = str_remove_all(beg, "[0-9]|\\bbags?\\b|\\.")) %>% 
  mutate(beg = str_trim(beg)) %>% 
  distinct(beg) 

# 2nd problem
dictionary <- read_table("data/bags.txt", col_names = F) %>% # get colors containing shiny gold
  separate(X1, c("bag", "content"), sep = "contain") %>% 
  mutate(content = str_split(content, ","),
         bag = str_remove_all(bag, "[0-9]|\\bbags?\\b|\\."))

level1 <- dictionary %>% filter(str_detect(bag, "shiny gold")) %>% 
  unnest(content) %>% 
  mutate(contain = str_extract_all(content, "[0-9]"), 
         contain = ifelse(str_detect(content, "no"), 0, contain),
         content = str_remove_all(content, "[0-9]|\\bbags?\\b|\\."),
         content = str_trim(content),
         bag = as.character(bag)) %>% as.data.frame()

list <- c(level1$content)

level2 <- dictionary %>% filter(str_detect(bag, paste(list, collapse = "|"))) %>% 
  unnest(content) %>% 
  mutate(contain = str_extract_all(content, "[0-9]"), 
         contain = ifelse(str_detect(content, "no"), 0, contain),
         content = str_remove_all(content, "[0-9]|\\bbags?\\b|\\."),
         content = str_trim(content),
         bag = as.character(bag))%>% as.data.frame()

list <- c(level2$content)

level3 <- dictionary %>% filter(str_detect(bag, paste(list, collapse = "|"))) %>% 
  unnest(content) %>% 
  mutate(contain = str_extract_all(content, "[0-9]"), 
         contain = ifelse(str_detect(content, "no"), 0, contain),
         content = str_remove_all(content, "[0-9]|\\bbags?\\b|\\."),
         content = str_trim(content),
         bag = as.character(bag))%>% as.data.frame()

list <- c(level3$content)

level4 <- dictionary %>% filter(str_detect(bag, paste(list, collapse = "|"))) %>% 
  unnest(content) %>% 
  mutate(contain = str_extract_all(content, "[0-9]"), 
         contain = ifelse(str_detect(content, "no"), 0, contain),
         content = str_remove_all(content, "[0-9]|\\bbags?\\b|\\."),
         content = str_trim(content),
         bag = as.character(bag)) %>% as.data.frame()

list <- c(level4$content)

level5 <- dictionary %>% filter(str_detect(bag, paste(list, collapse = "|"))) %>% 
  unnest(content) %>% 
  mutate(contain = str_extract_all(content, "[0-9]"), 
         contain = ifelse(str_detect(content, "no"), 0, contain),
         content = str_remove_all(content, "[0-9]|\\bbags?\\b|\\."),
         content = str_trim(content),
         bag = as.character(bag)) %>% as.data.frame()

list <- c(level5$content)

level6 <- dictionary %>% filter(str_detect(bag, paste(list, collapse = "|"))) %>% 
  unnest(content) %>% 
  mutate(contain = str_extract_all(content, "[0-9]"), 
         contain = ifelse(str_detect(content, "no"), 0, contain),
         content = str_remove_all(content, "[0-9]|\\bbags?\\b|\\."),
         content = str_trim(content),
         bag = as.character(bag)) %>% as.data.frame()

list <- c(level6$content)

level7 <- dictionary %>% filter(str_detect(bag, paste(list, collapse = "|"))) %>% 
  unnest(content) %>% 
  mutate(contain = str_extract_all(content, "[0-9]"), 
         contain = ifelse(str_detect(content, "no"), 0, contain),
         content = str_remove_all(content, "[0-9]|\\bbags?\\b|\\."),
         content = str_trim(content),
         bag = as.character(bag)) %>% as.data.frame()

list <- c(level7$content)

level8 <- dictionary %>% filter(str_detect(bag, paste(list, collapse = "|"))) %>% 
  unnest(content) %>% 
  mutate(contain = str_extract_all(content, "[0-9]"), 
         contain = ifelse(str_detect(content, "no"), 0, contain),
         content = str_remove_all(content, "[0-9]|\\bbags?\\b|\\."),
         content = str_trim(content),
         bag = as.character(bag)) %>% as.data.frame()

level8

## count
level8 <- level8 %>% mutate(count = 1) %>% distinct(bag, count)

level7 <- level7 %>%
  mutate(count1 = ifelse(contain == 0, 1, as.numeric(contain))) %>%
  group_by(bag) %>%
  mutate(count1 = sum(count1))  %>% 
  distinct(bag, count1) %>% 
  full_join(level8) %>% 
  mutate(count = count*count1) # check

level6 <- level6 %>%
  mutate(count2 = ifelse(contain == 0, 1, as.numeric(contain))) %>%
  group_by(bag) %>%
  mutate(count2 = sum(count2))  %>% 
  distinct(bag, count2) %>% 
  full_join(level7)

level5 <- level5 %>%
  mutate(count3 = ifelse(contain == 0, 1, as.numeric(contain))) %>%
  group_by(bag) %>%
  mutate(count3 = sum(count3))  %>% 
  distinct(bag, count3) %>% 
  full_join(level6)

level4 <- level4 %>%
  mutate(count4 = ifelse(contain == 0, 1, as.numeric(contain))) %>%
  group_by(bag) %>%
  mutate(count4 = sum(count4))  %>% 
  distinct(bag, count4) %>% 
  full_join(level5)

level3 <- level3 %>%
  mutate(count5 = ifelse(contain == 0, 1, as.numeric(contain))) %>%
  group_by(bag) %>%
  mutate(count5 = sum(count5))  %>% 
  distinct(bag, count5) %>% 
  full_join(level4)

level2 <- level2 %>%
  mutate(count6 = ifelse(contain == 0, 1, as.numeric(contain))) %>%
  group_by(bag) %>%
  mutate(count6 = sum(count6))  %>% 
  distinct(bag, count6) %>% 
  full_join(level3)

level1 <- level1 %>%
  mutate(count7 = ifelse(contain == 0, 1, as.numeric(contain))) %>%
  group_by(bag) %>%
  mutate(count7 = sum(count7))  %>% 
  distinct(bag, count7) %>% 
  full_join(level2)













  