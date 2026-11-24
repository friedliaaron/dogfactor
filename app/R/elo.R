# Elo-Rating 1. Liga Maenner: Kern fuer App und Analysen.
# Shiny laedt diese Datei automatisch; analysen/03_elo.qmd sourct sie (vor analysen/R/elo_funktionen.R).
#
# Erwartung Heimteam:  E = 1 / (1 + 10^(-(R_heim + H - R_gast) / 400))   (H = 0 an neutralem Ort)
# Update:              R_heim += K * (S - E),  R_gast -= K * (S - E)
# Saisonwechsel:       R_neu = 1500 + c * (R_alt - 1500); neue Teams starten bei 1500 + neu
# Prognose Sieg:       p = plogis(g * logit(E))   (g kalibriert Varianten mit abgestuftem S)
# Ergebnis S (Ball-Variante): plogis(a * logit(Ballanteil))

# Parameter aus analysen/03_elo.qmd (Ball-Variante, geschaetzt auf 2023/24 bis 2025/26,
# fuer den Hold-out 2026/27 eingefroren). Nicht ohne neue Schaetzung aendern.
elo_par <- c(K = 91.1, H = 10.4, c = 0.8, neu = -25.5, g = 5.826)
elo_a   <- 2

# Alle gespielten Spiele (alle Phasen), chronologisch, mit Ergebnisvarianten
elo_daten <- function(games, saisons = 2022:2025) {
  g <- games |>
    dplyr::filter(gespielt, saison %in% saisons, !is.na(saetze_heim)) |>
    dplyr::arrange(datum, game_id)
  # Heimort = haeufigste PLZ der Heimspiele (Qualifikation) pro Teamsaison
  heimplz <- g |> dplyr::filter(phase == "Qualifikation") |>
    dplyr::count(saison, heim_id, plz) |> dplyr::group_by(saison, heim_id) |>
    dplyr::slice_max(n, n = 1, with_ties = FALSE) |> dplyr::ungroup() |>
    dplyr::select(saison, heim_id, heimplz = plz)
  namen <- aktuelle_namen(games)
  g |>
    dplyr::left_join(heimplz, by = c("saison", "heim_id")) |>
    dplyr::mutate(
      heim_team = namen$name[match(heim_id, namen$id)],
      gast_team = namen$name[match(gast_id, namen$id)],
      quali     = phase == "Qualifikation",
      # Qualifikation: immer Heimspiel (Ausweichhallen zeigten gleichen Heimvorteil);
      # Final-/Abstiegsspiele: neutral, wenn nicht am Heimort des Heimteams
      neutral   = !quali & (is.na(heimplz) | plz != heimplz),
      sieg      = as.integer(saetze_heim > saetze_gast),
      s_satz    = saetze_heim / (saetze_heim + saetze_gast),
      ball_anteil = baelle_heim / (baelle_heim + baelle_gast)
    ) |>
    dplyr::group_by(saison) |> dplyr::mutate(spiel_nr_saison = dplyr::row_number()) |> dplyr::ungroup()
}

# Ball-Variante: S = plogis(a * logit(Ballanteil)); a steuert, wie stark abgestuft S ist
# (a klein: S nahe am Ballanteil, a gross: S nahe am Sieg)
s_ball <- function(e, a) plogis(a * qlogis(pmin(pmax(e$ball_anteil, 0.01), 0.99)))

# Ein Elo-Lauf. par = c(K, H, c, neu, g); s = Ergebnisvektor (gleiche Reihenfolge wie e)
elo_lauf <- function(e, s, par, start = 1500, verlauf = FALSE) {
  K <- par[1]; H <- par[2]; cc <- par[3]; neu <- par[4]
  s[is.na(s)] <- e$sieg[is.na(s)]                    # Fallback ohne Ballpunkte (Forfait)
  ids <- sort(unique(c(e$heim_id, e$gast_id)))
  R <- rep(NA_real_, length(ids)); names(R) <- ids
  hi <- match(e$heim_id, ids); gi <- match(e$gast_id, ids)
  n <- nrow(e); E <- numeric(n); rh <- rg <- numeric(n)
  saison_vorher <- e$saison[1]
  for (i in seq_len(n)) {
    if (e$saison[i] != saison_vorher) {              # Saisonwechsel: Richtung 1500 ziehen
      R <- start + cc * (R - start)
      saison_vorher <- e$saison[i]
    }
    for (j in c(hi[i], gi[i])) if (is.na(R[j])) R[j] <- start + neu
    d <- R[hi[i]] + (if (e$neutral[i]) 0 else H) - R[gi[i]]
    E[i] <- 1 / (1 + 10^(-d / 400))
    rh[i] <- R[hi[i]]; rg[i] <- R[gi[i]]
    delta <- K * (s[i] - E[i])
    R[hi[i]] <- R[hi[i]] + delta
    R[gi[i]] <- R[gi[i]] - delta
  }
  out <- list(E = E, ratings = R)
  if (verlauf) out$verlauf <- tibble::tibble(
    game_id = e$game_id, datum = e$datum, saison = e$saison,
    heim_id = e$heim_id, gast_id = e$gast_id,
    r_heim_vor = rh, r_gast_vor = rg, E = E,
    r_heim_nach = rh + K * (s - E), r_gast_nach = rg - K * (s - E))
  out
}

# Siegprognose aus E mit Kalibrierung g
elo_prob <- function(E, g) plogis(g * qlogis(pmin(pmax(E, 1e-6), 1 - 1e-6)))

# Siegwahrscheinlichkeit Heimteam aus zwei Ratings
elo_p_heim <- function(r_heim, r_gast, neutral = FALSE, par = elo_par) {
  d <- r_heim + ifelse(neutral, 0, par["H"]) - r_gast
  elo_prob(1 / (1 + 10^(-d / 400)), par["g"])
}

# Kompletter Lauf fuer die App: alle Saisons, Ball-Variante, eingefrorene Parameter.
# Gibt Verlauf in Teamsicht (eine Zeile pro Team und Spiel) und aktuelle Ratings zurueck.
elo_app <- function(games) {
  e <- elo_daten(games, saisons = sort(unique(games$saison)))
  lauf <- elo_lauf(e, s_ball(e, elo_a), elo_par, verlauf = TRUE)
  v <- lauf$verlauf
  namen <- aktuelle_namen(games)
  lang <- dplyr::bind_rows(
    tibble::tibble(game_id = v$game_id, datum = v$datum, saison = v$saison, team_id = v$heim_id,
                   gegner_id = v$gast_id, r_vor = v$r_heim_vor, r_nach = v$r_heim_nach),
    tibble::tibble(game_id = v$game_id, datum = v$datum, saison = v$saison, team_id = v$gast_id,
                   gegner_id = v$heim_id, r_vor = v$r_gast_vor, r_nach = v$r_gast_nach)) |>
    dplyr::mutate(team = namen$name[match(team_id, namen$id)],
                  gegner = namen$name[match(gegner_id, namen$id)]) |>
    dplyr::arrange(datum, game_id)
  # Spielebene: Prognose vor jedem Spiel (fuer die Auswertung der Prognoseguete)
  sp <- e |>
    dplyr::transmute(game_id, saison, datum, phase, gruppe, quali, neutral,
                     heim_id, gast_id, heim_team, gast_team,
                     resultat = paste0(saetze_heim, ":", saetze_gast),
                     baelle = paste0(baelle_heim, ":", baelle_gast),
                     sieg, r_heim_vor = v$r_heim_vor, r_gast_vor = v$r_gast_vor,
                     p_heim = elo_prob(lauf$E, elo_par[["g"]]))
  list(verlauf = lang, spiele = sp,
       ratings = tibble::tibble(team_id = as.integer(names(lauf$ratings)), rating = unname(lauf$ratings)),
       letzte_saison = max(e$saison))
}
