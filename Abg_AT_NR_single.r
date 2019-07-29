library(tidyverse)
library(lubridate)

# Funktion um durch Übeschneiden der GP von-bis und der Mandat von-bis GP-weise Daten zu erhalten
do <- function(von = ymd("1977-3-21"), bis = ymd("1982-9-9"))
  {
  lubridate::intersect(interval(von, bis), GP$Periode) %>%
    enframe(name = NULL) %>%
    bind_cols(GP = GP$IDdec, .) %>% # Hinzufügen der dezimalen GP
    filter(!is.na(value)) %>%       # No-macth = NA; herausfiltern
    arrange(GP) %>%                 # Aufsteigende GP
    transmute(GP, von = int_start(value) %>% date(), bis = int_end(value) %>% date())
}

# Anwenden der Funktion auf den Datensatz
Abg_AT_NR_single <- 
Abg_AT_NR[1:4,] %>%
  unnest() %>%
  mutate(New = map2(von, bis, ~ do(..1, ..2))) %>%
  unnest() %>% 
  select(PAD:Gestorben, GP = GP1, von = von1, bis = bis1, Klub) %>% 
  nest(GP:Klub)
