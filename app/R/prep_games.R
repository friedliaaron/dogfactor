# Aufbereitung der Spiel-Datenbank (data/games.csv) fuer App und Analysen.
#
# Shiny laedt alle Dateien in app/R/ automatisch. In Analyse-Skripten (aus dem Projektordner):
#   source("app/R/prep_games.R")
#   spiele <- read_games("app/data/games.csv") |> team_sicht()
#
# Kernidee: jedes gespielte Spiel ergibt zwei Zeilen, eine aus Sicht jedes Teams
# (Spalten *_fuer / *_gegen). Teams werden ueber team_id identifiziert, als Name
# dient der neuste Name dieser ID (Namen aendern sich ueber die Saisons).

resultat_stufen <- c("3:0", "3:1", "3:2", "2:3", "1:3", "0:3")

saison_label <- function(s) sprintf("%d/%02d", s, (s + 1) %% 100)

read_games <- function(file) {
  g <- read.csv(file, fileEncoding = "UTF-8", stringsAsFactors = FALSE,
                colClasses = c(zeit = "character", plz = "character"))
  g$datum <- as.Date(g$datum)
  g$gespielt <- as.logical(g$gespielt)
  g
}

# Neuster Name pro Team-ID (aus Heim- und Gastrolle)
aktuelle_namen <- function(games) {
  rbind(data.frame(id = games$heim_id, name = games$heim, datum = games$datum),
        data.frame(id = games$gast_id, name = games$gast, datum = games$datum)) |>
    dplyr::filter(!is.na(id), !is.na(name)) |>
    dplyr::arrange(dplyr::desc(datum)) |>
    dplyr::distinct(id, .keep_all = TRUE) |>
    dplyr::select(id, name)
}

# Lange Form: eine Zeile pro Team und gespieltem Spiel
team_sicht <- function(games) {
  g <- dplyr::filter(games, gespielt, !is.na(saetze_heim), !is.na(saetze_gast))

  eine_seite <- function(heim) {
    pick <- function(h, a) if (heim) g[[h]] else g[[a]]
    d <- tibble::tibble(
      game_id      = g$game_id,
      saison       = g$saison,
      gruppe       = g$gruppe,
      phase        = g$phase,
      datum        = g$datum,
      zeit         = g$zeit,
      heim_ausw    = if (heim) "Heim" else "Auswärts",
      team_id      = pick("heim_id", "gast_id"),
      team_name    = pick("heim", "gast"),        # Name zum Spielzeitpunkt
      gegner_id    = pick("gast_id", "heim_id"),
      gegner_name  = pick("gast", "heim"),
      saetze_fuer  = pick("saetze_heim", "saetze_gast"),
      saetze_gegen = pick("saetze_gast", "saetze_heim"),
      baelle_fuer  = pick("baelle_heim", "baelle_gast"),
      baelle_gegen = pick("baelle_gast", "baelle_heim"),
      halle        = g$halle,
      plz          = g$plz,
      spielort     = g$ort
    )
    for (i in 1:5) {
      d[[sprintf("s%d_fuer", i)]]  <- pick(sprintf("s%d_h", i), sprintf("s%d_g", i))
      d[[sprintf("s%d_gegen", i)]] <- pick(sprintf("s%d_g", i), sprintf("s%d_h", i))
    }
    d
  }

  namen <- aktuelle_namen(games)

  dplyr::bind_rows(eine_seite(TRUE), eine_seite(FALSE)) |>
    dplyr::mutate(
      saison_lbl     = saison_label(saison),
      team           = namen$name[match(team_id, namen$id)],
      gegner         = namen$name[match(gegner_id, namen$id)],
      resultat       = factor(paste0(saetze_fuer, ":", saetze_gegen), levels = resultat_stufen),
      sieg           = saetze_fuer > saetze_gegen,
      tiebreak       = saetze_fuer + saetze_gegen == 5,
      punkte         = dplyr::case_when(sieg & !tiebreak ~ 3L, sieg ~ 2L,
                                        tiebreak ~ 1L, TRUE ~ 0L),
      satzdiff       = saetze_fuer - saetze_gegen,
      ballpunktdiff  = baelle_fuer - baelle_gegen,
      # BQ pro Spiel; NA wenn keine Ballpunkte vorliegen (z. B. Forfait)
      bq             = ifelse(is.na(baelle_gegen) | baelle_gegen == 0, NA_real_,
                              baelle_fuer / baelle_gegen),
      satz1_gewonnen = s1_fuer > s1_gegen
    ) |>
    dplyr::arrange(dplyr::desc(datum), game_id, dplyr::desc(heim_ausw == "Heim"))
}

# Aggregation der (gefilterten) Spiele zu einer Tabelle.
# pro_saison = TRUE: eine Zeile pro Team und Saison, sonst ueber alle Saisons zusammen.
# SQ/BQ sind NA, wenn keine Saetze bzw. Baelle abgegeben wurden (Division durch 0).
team_tabelle <- function(d, pro_saison = TRUE) {
  gruppen <- c(if (pro_saison) c("saison", "saison_lbl"), "team_id", "team")
  d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(gruppen))) |>
    dplyr::summarise(
      gruppen      = paste(sort(unique(stats::na.omit(gruppe))), collapse = ", "),
      saisons      = dplyr::n_distinct(saison),
      spiele       = dplyr::n(),
      heimspiele   = sum(heim_ausw == "Heim"),
      punkte       = sum(punkte),
      siege        = sum(sieg),
      niederlagen  = sum(!sieg),
      r30          = sum(resultat == "3:0", na.rm = TRUE),
      r31          = sum(resultat == "3:1", na.rm = TRUE),
      r32          = sum(resultat == "3:2", na.rm = TRUE),
      r23          = sum(resultat == "2:3", na.rm = TRUE),
      r13          = sum(resultat == "1:3", na.rm = TRUE),
      r03          = sum(resultat == "0:3", na.rm = TRUE),
      saetze_fuer  = sum(saetze_fuer),
      saetze_gegen = sum(saetze_gegen),
      baelle_fuer  = sum(baelle_fuer, na.rm = TRUE),
      baelle_gegen = sum(baelle_gegen, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      punkte_pro_spiel = punkte / spiele,
      siegquote        = siege / spiele,
      sq               = ifelse(saetze_gegen > 0, saetze_fuer / saetze_gegen, NA_real_),
      bq               = ifelse(baelle_gegen > 0, baelle_fuer / baelle_gegen, NA_real_)
    ) |>
    dplyr::arrange(dplyr::across(dplyr::any_of("saison"), dplyr::desc),
                   dplyr::desc(punkte_pro_spiel), dplyr::desc(sq))
}
