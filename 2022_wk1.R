library(readr)
library(tidyverse)

# Dec 1
dec1 <- read.table("data/2022/dec1_1.txt", blank.lines.skip=F)

# puzzle 1
d = dec1 %>% 
  mutate(num = ifelse(is.na(V1), row_number(), NA)) %>%  
  fill(num, .direction = "up") %>% 
  group_by(num) %>% 
  summarise(sum = sum(V1, na.rm = T)) %>% 
  filter(sum == max(sum)) 

# puzzle 2
d = dec1 %>% 
  mutate(num = ifelse(is.na(V1), row_number(), NA)) %>%  
  fill(num, .direction = "up") %>% 
  group_by(num) %>% 
  summarise(sum = sum(V1, na.rm = T)) %>% 
  top_n(sum, n = 3) %>% 
  mutate(sumtotal = sum(sum))

# Dec 2
dec2 <- read.table("data/2022/dec2.txt", blank.lines.skip=F)

# puzzle 1
d = dec2 %>% 
  mutate(win = case_when(
    (V2 == "Z" & V1 == "B") | (V2 == "X" & V1 == "C") | (V2 == "Y" & V1 == "A") ~ 6,
    (V2 == "Z" & V1 == "C") | (V2 == "Y" & V1 == "B") | (V2 == "X" & V1 == "A") ~ 3,
    (V2 == "X" & V1 == "B") | (V2 == "Y" & V1 == "C") | (V2 == "Z" & V1 == "A") ~ 0
  ),
  choice = case_when(
    V2 == "X" ~ 1,
    V2 == "Y" ~ 2,
    V2 == "Z" ~ 3
  )) %>% 
  summarise(sum = sum(win + choice))

# puzzle 2
d = dec2 %>% 
  mutate(choice = case_when(
    (V1 == "B" & V2 == "X") | (V1 == "A" & V2 == "Y") | (V1 == "C" & V2 == "Z") ~ 1,
    (V1 == "C" & V2 == "X") | (V1 == "B" & V2 == "Y") | (V1 == "A" & V2 == "Z") ~ 2,
    (V1 == "A" & V2 == "X") | (V1 == "C" & V2 == "Y") | (V1 == "B" & V2 == "Z") ~ 3 
  ),
  score = case_when(
    V2 == "X" ~ 0,
    V2 == "Y" ~ 3,
    V2 == "Z" ~ 6
  ),
  score2 = choice + score) %>% 
  summarise(sum = sum(score2))

#  Dec 3
dec3 <- read.table("data/2022/dec3.txt", blank.lines.skip=F)
dict = data.frame(value = c(letters,LETTERS), numbers = c(1:52))

# puzzle 1
d = dec3 %>% 
  mutate(first = str_sub(V1, 1, str_length(V1)/2),
         second = str_sub(V1, (str_length(V1)/2)+1, str_length(V1)))

first = strsplit(d$first, split = "")
second = strsplit(d$second, split = "")
out = c()
first_n = list()

for (i in 1:length(first)) {
  first_n[[i]] = unique(first[[i]])
  for (letter in first_n[[i]])
    if (letter %in% second[[i]])
      out = c(out, letter)
}

df = as_tibble(out) %>% 
  left_join(dict) %>% 
  summarise(sum = sum(numbers))

# puzzle 2
d = dec3 %>% 
  mutate(group = rep(1:100, each=3),
         V1 = as.character(V1)) %>% 
  group_by(group) %>% 
  mutate(id = c("a", "b", "c")) %>% 
  pivot_wider(names_from = id,
              values_from = V1) %>% 
  ungroup()

a = strsplit(d$a, split = "")
b = strsplit(d$b, split = "")
c = strsplit(d$c, split = "")
out = c()
a_n = list()
#  b_n = list()

for (i in 1:length(a)) {
  a_n[[i]] = unique(a[[i]])
  # b_n[[i]] = unique(b[[i]])
  for (letter in a_n[[i]])
    if (letter %in% b[[i]]) 
      if (letter %in% c[[i]])
        out = c(out, letter)
}

df = as_tibble(out) %>% 
  left_join(dict) %>% 
  summarise(sum = sum(numbers))

# Dec 4
dec4 <- read.table("data/2022/dec4.txt", blank.lines.skip=F) %>% 
  separate(V1, c("elf1","elf2"), sep = ',') %>% 
  separate(elf1, c("elf1_1", "elf1_2"), sep = "-") %>% 
  separate(elf2, c("elf2_1", "elf2_2"), sep = "-") %>% 
  mutate_all(list(~as.numeric(as.character(.))))

# puzzle 1
d = dec4  %>% 
  mutate(within = ifelse(((elf1_1 >= elf2_1) & (elf1_2 <= elf2_2)) |
                           ((elf2_1 >= elf1_1) & (elf2_2 <= elf1_2)), 1, 0)) %>% 
  summarise(sum = sum(within))

# puzzle 2
d = dec4 %>% 
  mutate(within = ifelse(((elf1_1 >= elf2_1) & (elf1_2 <= elf2_2)) |
                           ((elf2_1 >= elf1_1) & (elf2_2 <= elf1_2)) |
                           ((elf1_2 >= elf2_1) & !(elf1_1 >= elf2_1)) |
                           ((elf2_2 >= elf1_1) & !(elf2_1 >= elf1_2)), 1, 0)) %>% 
  summarise(sum = sum(within))

# puzzle 1
# puzzle 2

# puzzle 1
# puzzle 2

# puzzle 1
# puzzle 2

# puzzle 1
# puzzle 2

# puzzle 1
# puzzle 2

# puzzle 1
# puzzle 2

# puzzle 1
# puzzle 2

# puzzle 1
# puzzle 2

# puzzle 1
# puzzle 2
  
