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
  
