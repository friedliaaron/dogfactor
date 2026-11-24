# Spiel-Datenbank 1. Liga Maenner (alle Gruppen, alle Phasen) aus dem Swiss Volley Game Center.
#
# Aufruf (im Repo-Root):
#   Rscript scrape_games.R backfill   # einmalig: alle Saisons unten komplett einlesen (dauert ~1 h)
#   Rscript scrape_games.R daily      # taeglich: neue Spiele + Resultate der laufenden Saison
#
# Ablauf:
#   1. Spiel-IDs aus der Wochenansicht holen. Diese laedt per JavaScript nach, daher
#      Headless-Chrome via rvest::read_html_live() (chromote).
#   2. Jede Spielseite (/de/game-center/game/<id>) normal mit rvest lesen, das Resultat
#      steht dort direkt im HTML.
#
# Pakete: rvest, chromote, xml2, dplyr, readr, stringr, purrr

suppressPackageStartupMessages({
  library(rvest); library(xml2); library(dplyr); library(readr)
  library(stringr); library(purrr)
})

# --- Konfiguration ---------------------------------------------------------
# saison = Startjahr (2026 = Saison 2026/27); liga = i_league-Parameter fuer "1L" Maenner
saisons <- tibble::tribble(
  ~saison, ~liga,
     2022,  5040,
     2023,  5491,
     2024,  6074,
     2025,  6609,
     2026,  7094
)
aktuelle_saison <- max(saisons$saison)
out_file <- file.path("app", "data", "games.csv")
pause_s  <- 1          # Pause zwischen Spielseiten (Server schonen)
base     <- "https://www.volleyball.ch"

mode <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(mode)) mode <- "daily"
stopifnot(mode %in% c("backfill", "daily"))

num <- function(x) suppressWarnings(as.integer(x))

# --- 1. Spiel-IDs aus der Wochenansicht -------------------------------------
week_url <- function(saison, liga, week) {
  sprintf("%s/de/game-center?sport=indoor&gender=m&season=%d&i_tab=Meisterschaft&i_region=SV&i_league=%d&i_phase=all&i_group=all&i_week=%d",
          base, saison, liga, week)
}

# Rendert eine Wochenseite. Gibt Spiel-IDs mit Gruppe ("Gruppe A" usw.) und die Anzahl Wochen zurueck.
# Aufbau der Seite: <div><div>Gruppe A</div><div><a href=".../game/123">...</a>...</div></div>
read_week <- function(saison, liga, week, max_wait = 15) {
  page <- read_html_live(week_url(saison, liga, week))
  on.exit(try(page$session$close(), silent = TRUE), add = TRUE)
  res <- tibble(game_id = integer(0), gruppe = character(0)); n_weeks <- NA_integer_
  last_n <- -1
  for (i in seq_len(max_wait)) {
    Sys.sleep(1)
    # Momentaufnahme des gerenderten DOM als normales xml2-Dokument
    doc <- read_html(page$session$Runtime$evaluate("document.documentElement.outerHTML")$result$value)
    links <- xml_find_all(doc, "//a[contains(@href, '/game-center/game/')]")
    labels <- xml_text(xml_find_all(doc, "//button"))
    wk <- sum(str_detect(str_squish(labels), "^\\d{2}\\.\\d{2}\\.\\d{2}\\s*[\u2013-]"))
    if (wk > 0) n_weeks <- wk
    # erst uebernehmen, wenn die Anzahl Spiele zwei Sekunden in Folge gleich bleibt
    # (sonst erwischt man eine halb geladene Seite)
    stabil <- length(links) > 0 && length(links) == last_n
    last_n <- length(links)
    if (stabil && !is.na(n_weeks)) {
      res <- tibble(
        game_id = num(str_match(xml_attr(links, "href"), "/game/(\\d+)")[, 2]),
        gruppe  = map_chr(links, function(a) {
          h <- xml_child(xml_parent(xml_parent(a)), 1)
          if (inherits(h, "xml_missing")) NA_character_ else str_squish(xml_text(h))
        })
      ) |> distinct(game_id, .keep_all = TRUE)
      break
    }
  }
  list(games = res, n_weeks = n_weeks)
}

# Alle Spiel-IDs einer Saison (i_week ist 0-basiert)
scan_season <- function(saison, liga) {
  message(sprintf("Saison %d/%d: Wochen scannen ...", saison, saison + 1))
  first <- read_week(saison, liga, 0)
  n <- first$n_weeks
  if (is.na(n)) stop("Wochenleiste nicht gefunden (Seitenstruktur geaendert?)")
  res <- first$games
  for (w in seq_len(n - 1)) {
    res <- bind_rows(res, read_week(saison, liga, w)$games)
  }
  res <- distinct(res, game_id, .keep_all = TRUE) |> mutate(saison = saison)
  message(sprintf("  %d Wochen, %d Spiele", n, nrow(res)))
  res
}

# --- 2. Eine Spielseite parsen ----------------------------------------------

parse_game <- function(game_id, doc = read_html(sprintf("%s/de/game-center/game/%d", base, game_id))) {
  grid <- xml_find_first(doc, "//div[contains(@class, 'grid-cols-[2fr_1fr_2fr]')]")
  if (inherits(grid, "xml_missing")) stop("Kopfbereich nicht gefunden")
  cols <- xml_children(grid)

  link_id <- function(node, what) {
    href <- xml_attr(xml_find_first(node, ".//a[contains(@href, '/team/')]"), "href")
    num(str_match(href, paste0("/", what, "/(\\d+)"))[, 2])
  }
  leaves <- function(node) {
    t <- str_squish(xml_text(xml_find_all(node, ".//text()")))
    t[t != ""]
  }

  # Mitte: "3 : 0 25 : 12 25 : 21 ..." oder "vs." bei noch nicht gespielten Spielen
  zahlen <- num(leaves(cols[[2]]))
  zahlen <- zahlen[!is.na(zahlen)]
  gespielt <- length(zahlen) >= 2
  saetze <- if (gespielt) matrix(zahlen[-(1:2)], ncol = 2, byrow = TRUE) else matrix(integer(0), ncol = 2)
  set_vals <- setNames(rep(NA_integer_, 10), paste0("s", rep(1:5, each = 2), c("_h", "_g")))
  if (nrow(saetze) > 0) {
    k <- min(nrow(saetze), 5)
    set_vals[paste0("s", 1:k, "_h")] <- saetze[1:k, 1]
    set_vals[paste0("s", 1:k, "_g")] <- saetze[1:k, 2]
  }

  # Details: Label gefolgt von Werten (Datum, Ort, Liga, Phase)
  info <- leaves(xml_parent(grid))
  after <- function(label, n = 1) {
    i <- match(label, info)
    if (is.na(i)) return(rep(NA_character_, n))
    info[i + seq_len(n)]
  }
  datum_txt <- after("Datum", 2)
  ort_i  <- match("Ort", info); liga_i <- match("Liga", info)
  ort    <- if (!is.na(ort_i) && !is.na(liga_i) && liga_i > ort_i + 1) info[(ort_i + 1):(liga_i - 1)] else NA_character_
  # Adresse nach der Halle, z. B. "Zuercherstrasse 67 9000 St. Gallen": PLZ = 4 Ziffern vor dem Ortsnamen
  plz_ort <- str_match(paste(ort[-1], collapse = " "), "(\\d{4})\\s+(\\D.*)$")

  tibble(
    game_id     = game_id,
    datum       = as.Date(str_extract(datum_txt[1], "\\d{2}\\.\\d{2}\\.\\d{4}"), "%d.%m.%Y"),
    zeit        = str_extract(datum_txt[2], "\\d{1,2}:\\d{2}"),
    phase       = after("Phase"),
    liga_txt    = after("Liga"),
    heim        = leaves(cols[[1]])[1],
    heim_id     = link_id(cols[[1]], "team"),
    heim_club   = link_id(cols[[1]], "club"),
    gast        = leaves(cols[[3]])[1],
    gast_id     = link_id(cols[[3]], "team"),
    gast_club   = link_id(cols[[3]], "club"),
    gespielt    = gespielt,
    saetze_heim = if (gespielt) zahlen[1] else NA_integer_,
    saetze_gast = if (gespielt) zahlen[2] else NA_integer_,
    !!!as.list(set_vals),
    baelle_heim = if (nrow(saetze) > 0) sum(saetze[, 1]) else NA_integer_,
    baelle_gast = if (nrow(saetze) > 0) sum(saetze[, 2]) else NA_integer_,
    halle       = ort[1],
    plz         = plz_ort[, 2],
    ort         = plz_ort[, 3],
    abgerufen   = Sys.Date()
  )
}

safe_parse <- function(id) {
  Sys.sleep(pause_s)
  tryCatch(parse_game(id), error = function(e) {
    message(sprintf("  Spiel %d: %s", id, conditionMessage(e))); NULL
  })
}

if (identical(Sys.getenv("SCRAPE_TEST"), "1")) stop("Testmodus: nur Funktionen geladen", call. = FALSE)

# --- 3. Ablauf ---------------------------------------------------------------
ids_file <- file.path("app", "data", "game_ids.csv")   # Zwischenspeicher der gescannten IDs
dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)

games <- if (file.exists(out_file)) {
  read_csv(out_file, show_col_types = FALSE, col_types = cols(.default = col_guess(), zeit = col_character()))
} else NULL
if (!is.null(games) && !all(c("gruppe", "heim_club", "plz") %in% names(games))) {
  message("Alte Datenbank ohne Gruppe/Club/PLZ: alle Spiele werden neu abgerufen.")
  games <- NULL
}
id_cache <- if (file.exists(ids_file)) read_csv(ids_file, show_col_types = FALSE,
                                                col_types = cols(game_id = "i", saison = "i", gruppe = "c")) else
  tibble(game_id = integer(0), saison = integer(0), gruppe = character(0))

# Welche Saisons scannen?
if (mode == "backfill") {
  # nur Saisons, die noch nicht im ID-Zwischenspeicher sind (so kann ein abgebrochener Lauf weitermachen)
  to_scan <- filter(saisons, !saison %in% id_cache$saison)
} else {
  # laufende Saison: montags neu scannen (neue Phasen, verschobene Spiele) oder falls noch leer
  leer <- !any(id_cache$saison == aktuelle_saison)
  to_scan <- if (leer || format(Sys.Date(), "%u") == "1") filter(saisons, saison == aktuelle_saison) else saisons[0, ]
}

for (k in seq_len(nrow(to_scan))) {
  neu_ids  <- scan_season(to_scan$saison[k], to_scan$liga[k])
  id_cache <- bind_rows(neu_ids, id_cache) |> distinct(game_id, .keep_all = TRUE)
  write_csv(id_cache, ids_file, na = "")          # nach jeder Saison sichern
}

# Zu holen: neue IDs + noch nicht gespielte Spiele, deren Datum erreicht ist
known <- if (is.null(games)) integer(0) else games$game_id
offen <- if (is.null(games)) integer(0) else
  games$game_id[!games$gespielt & (is.na(games$datum) | games$datum <= Sys.Date())]
todo  <- id_cache |> filter(!game_id %in% known | game_id %in% offen)
message(sprintf("%d Spielseiten abrufen ...", nrow(todo)))

speichern <- function(games, neu) {
  if (nrow(neu) == 0) return(games)
  games <- bind_rows(if (!is.null(games)) filter(games, !game_id %in% neu$game_id), neu) |>
    relocate(saison, gruppe, .after = game_id) |>
    arrange(datum, game_id)
  write_csv(games, out_file, na = "")
  games
}

# In Paketen zu 50 Spielen abrufen und jedes Mal speichern: ein Abbruch verliert hoechstens 50 Spiele
pakete <- split(seq_len(nrow(todo)), ceiling(seq_len(nrow(todo)) / 50))
for (j in seq_along(pakete)) {
  idx <- pakete[[j]]
  neu <- pmap_dfr(list(todo$game_id[idx], todo$saison[idx], todo$gruppe[idx]), function(id, s, gr) {
    g <- safe_parse(id)
    if (!is.null(g)) { g$saison <- s; g$gruppe <- gr }
    g
  })
  games <- speichern(games, neu)
  message(sprintf("  %d / %d abgerufen", max(idx), nrow(todo)))
}

message(sprintf("Fertig: %d Spiele in %s (%d gespielt)",
                if (is.null(games)) 0 else nrow(games), out_file,
                if (is.null(games)) 0 else sum(games$gespielt)))
