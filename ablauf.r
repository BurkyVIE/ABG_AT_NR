# Nötige Packages
library(tidyverse)
library(lubridate)

# Einlesen aller Abgeordneten
base <- read_csv2("alle Abg 20190727.txt", locale =  locale(encoding = "WINDOWS-1252"))

# Funktion zum scrapen laden
source("scraping.r")

# # Hole Daten aus dem Internet
# first <- base$PAD[-1513] %>% map_df(., ~do_abg(.)) #1513 = 88386 Sobotka
# 
# # 88386 Sobotka händisch
# first <- bind_rows(first,
#                    tibble(
#                      PAD = "88386",
#                      Name = "Mag. Wolfgang Sobotka",
#                      Geschlecht = factor("m", levels = c("w", "m"), labels = c("weibl.", "männl.")),
#                      Geboren = ymd("1956-1-5"),
#                      Gestorben = NA,
#                      Mandat = list(
#                        tibble(
#                        GP = "XXVI.",
#                        von = ymd("2017-11-9"),
#                        bis = today(),
#                        Klub = "ÖVP")
#                        )
#                      ))
# 
# dump("first", "first.rsav")

Abg_AT_NR <- first

### Fehlerbehebung
# 880 Maria Köstler, gestorben im November 1965 -> Annahme Tag = 15.
he <- Abg_AT_NR %>% filter(PAD == "00880")       # Kopiere Zeile
he["Gestorben"] <- dmy("15.11.1965")             # Korrigiere
Abg_AT_NR <- Abg_AT_NR %>% filter(PAD != he$PAD) # Lösche alte Zeile
Abg_AT_NR <- bind_rows(Abg_AT_NR, he)            # Füge korrigierte Zeile an
rm(he)

# 1738 Friedrich Schmidt, Sterbedatum nicht feststellbar -> Annahme Alter = 65
he <- Abg_AT_NR %>% filter(PAD == "01738")       # Kopiere Zeile
he["Gestorben"] <- dmy("17.07.1939")             # Korrigiere
Abg_AT_NR <- Abg_AT_NR %>% filter(PAD != he$PAD) # Lösche alte Zeile
Abg_AT_NR <- bind_rows(Abg_AT_NR, he)            # Füge korrigierte Zeile an
rm(he)

# Ãœberschneidung KPÖ und LB in GP V. und VI. -> Lösche 'V.' & 'LB' sowie 'VI.' & 'KPÖ'
Abg_AT_NR <-
  Abg_AT_NR %>%
  unnest() %>% 
  filter(!(GP == "V." & Klub == "LB")) %>% 
  filter(!(GP == "VI." & Klub == "KPÃ–")) %>% 
  nest(GP:Klub, .key = "Mandat")
  
# Keine Unterbrechung zwischen GP III. und IV. -> 01.11.1930 liegt im Bereich
# Abg_AT_NR %>% unnest() %>% filter(ymd("1930-11-1") %within% interval(von, bis))
# A tibble: 6 x 9
# PAD   Name                   Geschlecht Geboren    Gestorben  GP       von        bis        Klub 
# <chr> <chr>                  <fct>      <date>     <date>     <chr>    <date>     <date>     <chr>
# 1 00119 Franz Birbaumer        männl.     1871-10-04 1931-09-17 I.–IV.   1920-12-07 1931-09-17 CSP  
he <- Abg_AT_NR %>% filter(PAD == "00119")
new = tibble(PAD = he$PAD, Name = he$Name, Geschlecht = he$Geschlecht, Geboren = he$Geboren, Gestorben = he$Gestorben,
             Mandat = list(tibble(GP = c("I.-III.", "IV."),
                                  von = ymd(c("1920-12-7", "1930-12-2")),
                                  bis = ymd(c("1930-10-1", "1931-9-17")),
                                  Klub = c("CSP", "CSP"))))
Abg_AT_NR <- Abg_AT_NR %>% filter(PAD != he$PAD)
Abg_AT_NR <- bind_rows(Abg_AT_NR, new)
rm(he, new)
# 2 00239 DDr. Karl Drexel       männl.     1872-07-21 1954-03-14 II.–IV.  1923-11-20 1931-10-16 CSP  
he <- Abg_AT_NR %>% filter(PAD == "00239")
new = tibble(PAD = he$PAD, Name = he$Name, Geschlecht = he$Geschlecht, Geboren = he$Geboren, Gestorben = he$Gestorben,
             Mandat = list(tibble(GP = c("II.-III.", "IV."),
                                  von = ymd(c("1923-11-20", "1930-12-2")),
                                  bis = ymd(c("1930-10-1", "1931-10-16")),
                                  Klub = c("CSP", "CSP"))))
Abg_AT_NR <- Abg_AT_NR %>% filter(PAD != he$PAD)
Abg_AT_NR <- bind_rows(Abg_AT_NR, new)
rm(he, new)
# 3 00247 Matthias Duscher       männl.     1891-01-09 1967-12-14 III.–IV. 1927-05-18 1934-05-02 CSP  
he <- Abg_AT_NR %>% filter(PAD == "00247")
new = tibble(PAD = he$PAD, Name = he$Name, Geschlecht = he$Geschlecht, Geboren = he$Geboren, Gestorben = he$Gestorben,
             Mandat = list(tibble(GP = c("III.", "IV."),
                                  von = ymd(c("1927-5-18", "1930-12-2")),
                                  bis = ymd(c("1930-10-1", "1934-5-2")),
                                  Klub = c("CSP", "CSP"))))
Abg_AT_NR <- Abg_AT_NR %>% filter(PAD != he$PAD)
Abg_AT_NR <- bind_rows(Abg_AT_NR, new)
rm(he, new)
# 4 01957 Felix Kern             männl.     1892-05-21 1955-10-23 III.–IV. 1927-05-18 1931-09-30 CSP  
he <- Abg_AT_NR %>% filter(PAD == "01957")
new = tibble(PAD = he$PAD, Name = he$Name, Geschlecht = he$Geschlecht, Geboren = he$Geboren, Gestorben = he$Gestorben,
             Mandat = list(tibble(GP = c("III.", "IV."),
                                  von = ymd(c("1927-5-18", "1930-12-2")),
                                  bis = ymd(c("1930-10-1", "1931-9-30")),
                                  Klub = c("CSP", "CSP"))))
Abg_AT_NR <- Abg_AT_NR %>% filter(PAD != he$PAD)
Abg_AT_NR <- bind_rows(Abg_AT_NR, new)
rm(he, new)
# 5 01231 Franz Plasser          männl.     1893-09-10 1970-10-01 III.–IV. 1927-05-18 1934-02-17 SdP  
he <- Abg_AT_NR %>% filter(PAD == "01231")
new = tibble(PAD = he$PAD, Name = he$Name, Geschlecht = he$Geschlecht, Geboren = he$Geboren, Gestorben = he$Gestorben,
             Mandat = list(tibble(GP = c("III.", "IV."),
                                  von = ymd(c("1927-5-18", "1930-12-2")),
                                  bis = ymd(c("1930-10-1", "1934-2-17")),
                                  Klub = c("SdP", "SdP"))))
Abg_AT_NR <- Abg_AT_NR %>% filter(PAD != he$PAD)
Abg_AT_NR <- bind_rows(Abg_AT_NR, new)
rm(he, new)
# 6 01480 Ing. DDDr. Julius Raab männl.     1891-11-29 1964-01-08 III.–IV. 1927-05-18 1934-05-02 CSP 
he <- Abg_AT_NR %>% filter(PAD == "01480")
new = tibble(PAD = he$PAD, Name = he$Name, Geschlecht = he$Geschlecht, Geboren = he$Geboren, Gestorben = he$Gestorben,
             Mandat = list(tibble(GP = c("III.", "IV."),
                                  von = ymd(c("1927-5-18", "1930-12-2")),
                                  bis = ymd(c("1930-10-1", "1934-5-2")),
                                  Klub = c("CSP", "CSP"))))
Abg_AT_NR <- Abg_AT_NR %>% filter(PAD != he$PAD)
Abg_AT_NR <- bind_rows(Abg_AT_NR, new)
rm(he, new)

# Name mit führendem Nachnamen hinzufügen
Abg_AT_NR <-
  left_join(Abg_AT_NR, base, by = "PAD", suffix = c("", "2")) %>% 
  select(PAD, Name, Name2, Geschlecht:Mandat)

# Funktion um konsolidierte Werte für ein Datum zu bekommen
kons <- function(d = lubridate::ymd("1977-3-21")){
  library(lubridate)
  Abg_AT_NR %>%
    unnest() %>%
    filter(d %within% interval(von, bis)) %>% 
    mutate(Alter = interval(Geboren, d) %>% time_length(unit = "year"),
           Weibl = Geschlecht == "weibl.") %>% 
    summarise(N = n(),
              DAlter = mean(Alter),
              Jung = min(Alter),
              Weibl = mean(Weibl) * 100) %>% 
    mutate(N = case_when(N == 0 ~ NA_integer_,
                         TRUE ~ N),
           Jung = case_when(Jung > 250 ~ NA_real_,
                            TRUE ~ Jung))
}

# Erzeuge single GP-Mandats Zeiträume
source("Abg_AT_NR_single.r")

# Erzeuge tibble der Zusammenfassung über die GP I. bis 25.
Abg_AT_NR_kons <-
  tibble(Date = full_seq(c(ymd("1920-11-10"), ymd("2017-11-8")), 1)) %>% # Alle Tage zwischen ertser Tag I. GP und letzter Tag XXV. GP
  mutate(Date = floor_date(Date, unit = "month"), week_start = 1) %>%    # Monatserste dieser Serie
  group_by(Date) %>% summarise() %>% tail(-1) %>%                        # Der 1.11.1920 liegt außerhalb der Serie
  bind_cols(., map_dfr(.$Date, kons))

# Plot
p <- Abg_AT_NR_kons %>%
  rename(Durchschnittsalter = DAlter, 'Jüngste(r) Abg.' = Jung, 'Anzahl Abg.' = N, Frauenanteil = Weibl) %>% 
  gather(-Date, key = "Was", value = "Wert") %>%
  ggplot(mapping = aes(x = Date, y = Wert)) +
  geom_vline(xintercept = GP$von, color = "grey75", linetype = "dashed") +
  geom_smooth(color = "pink", se = FALSE) +
  geom_line(color = "deepskyblue", size = 1.5) +
  facet_grid(Was ~ ., scales = "free_y") +
  theme_bw()

plot(p)
rm(p)
