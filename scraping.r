do_abg <- function(file) {
  library(tidyverse)
  library(lubridate)
  library(rvest)
  
  
  name <- html_text(html_nodes(read_html(file), "h1"))
  
  he <- html_text(html_nodes(read_html(file), "p"))
  i <- which(regexpr("Geb.:", he) > 0)
  geb <- he[i] %>% substr(7, 16) %>% dmy()
  j <- max(regexpr("Verst.:", he))
  gest <- NA
  if (j > 0)
    gest <- he[i] %>% substr(j + 8, j + 17) %>% dmy()
  rm(i, j)
  
  he <- html_text(html_nodes(read_html(file), "li"))
  mandat <-
    (regexpr("Abgeordneter? zum Nationalrat \\(", he) > 0) %>% which() %>% he[.] %>% gsub("\\n", "", .)
  if (substr(mandat, 12, 12)[1] == "r")
    geschl <- "m"
  else
    geschl <- "w"
  mandat <-
    gsub("Abgeordneter? zum Nationalrat \\((.+) GP\\), (\\D+)(.+)",
         "\\1, \\3, \\2",
         mandat)
  mandat <- strsplit(mandat, split = ", ") %>% do.call(rbind, .)
  mandat <-
    cbind(mandat,
          substr(mandat[, 2], 1, 10),
          substr(mandat[, 2], nchar(mandat[, 2]) - 9, nchar(mandat[, 2]))) # komplizierter Weg falls nur ein Tag
  mandat <-
    tibble(
      GP = mandat[, 1],
      von = mandat[, 4] %>% dmy(),
      bis = mandat[, 5] %>% dmy(),
      Klub = mandat[, 3]
    ) %>% arrange(von) %>% mutate(bis = coalesce(bis, today()))
  rm(he)
  
  res <-
    tibble(
      ID = substr(file, 6, 10),
      Name = name,
      Geschlecht = factor(geschl, levels = c("w", "m"), labels = c("weibl.", "männl.")),
      Geboren = geb,
      Gestorben = gest,
      Mandat = list(mandat)
    )
  rm(file, name, geschl, geb, gest, mandat)
  
  #unnest(res)
  res
}
  


dir()[dir() %>% regexpr("index", .) == 1] %>% map_df(., ~ do_abg(.))
