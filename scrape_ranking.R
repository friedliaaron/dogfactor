# Holt die Rangliste einer Gruppe aus dem Swiss Volley Game Center
# und haengt sie als Tages-Snapshot an app/data/ranking_history.csv an.
#
# Die Seite laedt die Tabelle per JavaScript nach, deshalb wird sie
# mit einem Headless-Chrome gerendert (rvest::read_html_live, via chromote).
# Voraussetzungen: install.packages(c("rvest", "chromote", "readr", "dplyr", "stringr"))
# und ein installiertes Google Chrome / Chromium.

library(rvest)
library(dplyr)
library(stringr)
library(readr)

# --- Konfiguration ------------------------------------------------------
url <- paste0(
  "https://www.volleyball.ch/de/game-center?sport=indoor&gender=m&season=2026",
  "&i_tab=Meisterschaft&i_region=SV&i_league=7094&i_phase=14040&i_group=29470"
)
group_label <- "1L Maenner, Qualifikation, Gruppe B"

# Pfad relativ zum Repo-Root (dort wird das Skript lokal und in GitHub Actions gestartet)
out_file <- file.path("app", "data", "ranking_history.csv")
dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)

# --- Seite rendern und Tabelle auslesen ---------------------------------
row_selector <- "#ranking-list section.table-body > div"

page <- read_html_live(url)
on.exit(try(page$session$close(), silent = TRUE), add = TRUE)

rows <- NULL
for (i in 1:30) {                       # bis zu 30 s warten, bis die Tabelle da ist
  rows <- html_elements(page, row_selector)
  if (length(rows) > 0) break
  Sys.sleep(1)
}
if (length(rows) == 0) stop("Rangliste nicht gefunden. Hat sich die Seitenstruktur geaendert?")

cells <- lapply(rows, function(r) html_text2(html_children(r)))
if (any(lengths(cells) != 14)) stop("Unerwartete Spaltenzahl in der Rangliste.")

split_pair <- function(x, i) as.integer(str_trim(str_split_fixed(x, ":", 2)[, i]))

ranking <- as.data.frame(do.call(rbind, cells), stringsAsFactors = FALSE) |>
  setNames(c("rang", "team", "spiele", "punkte", "siege", "siege_klar", "siege_tb",
             "niederlagen", "niederlagen_tb", "niederlagen_klar",
             "saetze", "satzquotient", "baelle", "ballquotient")) |>
  mutate(
    across(c(rang, spiele, punkte, siege, siege_klar, siege_tb,
             niederlagen, niederlagen_tb, niederlagen_klar), as.integer),
    across(c(satzquotient, ballquotient), as.numeric),
    saetze_gewonnen = split_pair(saetze, 1),
    saetze_verloren = split_pair(saetze, 2),
    baelle_gewonnen = split_pair(baelle, 1),
    baelle_verloren = split_pair(baelle, 2),
    gruppe = group_label,
    datum = Sys.Date()
  ) |>
  select(datum, gruppe, rang, team, spiele, punkte, siege, siege_klar, siege_tb,
         niederlagen, niederlagen_tb, niederlagen_klar,
         saetze_gewonnen, saetze_verloren, satzquotient,
         baelle_gewonnen, baelle_verloren, ballquotient)

# --- Anhaengen (ein Snapshot pro Tag; erneuter Lauf am selben Tag ersetzt ihn) ---
if (file.exists(out_file)) {
  history <- read_csv(out_file, show_col_types = FALSE) |>
    filter(!(datum == Sys.Date() & gruppe == group_label))
  ranking <- bind_rows(history, ranking)
}
write_csv(ranking, out_file)
message(sprintf("%s: %d Teams gespeichert in %s", Sys.Date(), length(rows), out_file))
