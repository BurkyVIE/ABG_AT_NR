library(tidyverse)
library(lubridate)

# Sitzfleisch
Abg_AT_NR_single %>%
  unnest() %>% 
  mutate(Dauer = interval(von, bis) %>% time_length(unit = "day")) %>%
  mutate(Dauer = case_when(GP <= 14 ~ Dauer,       # Unterschiedliche Darstellung der Perioden bis GP 14,
                           TRUE ~ Dauer + 1)) %>%  # und danach (Überschneidungstag)
  group_by(Name) %>% 
  summarise(GP = paste(sort(unique(GP)), collapse = ", "),
            Dauer = sum(Dauer)) %>% 
  arrange(desc(Dauer))

#Ältester Lebende(r) Abgeordnete(r)
Abg_AT_NR %>% 
  filter(is.na(Gestorben)) %>% 
  mutate(Alter = interval(Geboren, today()) %>% time_length(unit = "year")) %>% 
  arrange(desc(Alter))
