do_abg <- function(PAD) { 
  library(tidyverse)
  library(lubridate)
  library(rvest)
  
  # PAD muss als Text kommen
  if(!is.character(PAD)) PAD <- sprintf("%05d", PAD)
  
  # Erzeuge URL des Abgeordneten
  file <- paste0("https://www.parlament.gv.at/WWER/PAD_", PAD, "/index.shtml")
  
  # Extrahiere Name des ABgeordneten
  name <- html_text(html_nodes(read_html(file), "h1"))
  
  # Extrahiere Geburts- und ggf. Sterbedatum des Abgeordneten
  he <- html_text(html_nodes(read_html(file), "p"))
  i <- which(regexpr("Geb.:", he) > 0)     # Welche Zeile enthält die Daten
  geb <- he[i] %>% substr(7, 16) %>% dmy() # Geburtsdatum
  j <- max(regexpr("Verst.:", he))         # Ab wo Sterbedaum, falls es eines gibt
  gest <- NA
  if (j > 0)
    gest <- he[i] %>% substr(j + 8, j + 17) %>% dmy() #Sterbedatum
  
  # Extrahiere Informationen zum Mandat
  he <- html_text(html_nodes(read_html(file), "li"))
  mandat <-
    (regexpr("Abgeordneter? zum Nationalrat \\(", he) > 0) %>% which() %>% he[.] %>% gsub("\\n", "", .) # Welche Zeilen beziehen sich auf den Nationalrat (inkl. Klammer wegen allf. gleichlautender Berufsbezeichnung)
  if (substr(mandat, 12, 12)[1] == "r") geschl <- "m" else geschl <- "w"
  mandat <-
    gsub("Abgeordneter? zum Nationalrat \\((.+) GP\\), (\\D+)(.+)", "\\1<>\\2<>\\3", mandat)
  mandat <- strsplit(mandat, split = "<>") %>% do.call(rbind, .)
  mandat <-
    cbind(mandat,
          substr(mandat[, 3], 1, 10),  # von Datum
          substr(mandat[, 3], 12, 12), # gibt es ein –
          substr(mandat[, 3], 14, 23)) # bis Datum
  mandat <-
    cbind(mandat,
          paste0(substr(mandat[, 6], 3, 3), substr(mandat[, 6], 6, 6))) # ist das bis Datum ein Datum (Punkte an den richtigen Stellen)
  mandat[mandat[, 5] == "", 6] <- mandat[mandat[, 5] == "", 4] # nur ein Tag
  mandat[mandat[, 5] == "–" & mandat[, 7] != "..", 6] <- as.character(today() %>% format("%d.%m.%Y")) # offenes Interval
  # Rückgabe Mandat als tibble
  mandat <-
    tibble(
      GP = mandat[, 1],
      von = mandat[, 4] %>% dmy(),
      bis = mandat[, 6] %>% dmy(),
      Klub = mandat[, 2]
    ) %>% arrange(von)
  rm(he)
  
  # Rückgabe
  res <-
    tibble(
      PAD = PAD,
      Name = name,
      Geschlecht = factor(geschl, levels = c("w", "m"), labels = c("weibl.", "männl.")),
      Geboren = geb,
      Gestorben = gest,
      Mandat = list(mandat)
    )
  res
}