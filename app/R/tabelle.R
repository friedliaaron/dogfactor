# Ranglisten aus den Spieldaten (Qualifikation einer Gruppe und Saison).
# Spaltennamen wie in data/ranking_history.csv, damit die Rangliste-Seite beides lesen kann.
# Reihenfolge: Punkte, Siege, Satzquotient, Ballquotient
# (fuer Gruppe B 2026/27 mit der offiziellen Rangliste abgeglichen: 12 von 12 Teams gleich).

# Tabelle aus Spielen in Teamsicht (team_sicht()); alle_teams: Teams mit 0 Spielen ergaenzen
tabelle_aus_spielen <- function(s, alle_teams = NULL) {
  t <- s |>
    dplyr::group_by(team_id, team) |>
    dplyr::summarise(
      spiele = dplyr::n(), punkte = sum(punkte),
      siege = sum(sieg), siege_klar = sum(sieg & !tiebreak), siege_tb = sum(sieg & tiebreak),
      niederlagen = sum(!sieg), niederlagen_tb = sum(!sieg & tiebreak),
      niederlagen_klar = sum(!sieg & !tiebreak),
      saetze_gewonnen = sum(saetze_fuer), saetze_verloren = sum(saetze_gegen),
      baelle_gewonnen = sum(baelle_fuer, na.rm = TRUE), baelle_verloren = sum(baelle_gegen, na.rm = TRUE),
      .groups = "drop")
  if (!is.null(alle_teams)) {
    fehlt <- dplyr::filter(alle_teams, !team_id %in% t$team_id)
    if (nrow(fehlt) > 0) {
      for (v in setdiff(names(t), names(fehlt))) fehlt[[v]] <- 0L   # Teams ohne Spiel: alles 0
      t <- dplyr::bind_rows(t, fehlt)
    }
  }
  t |>
    dplyr::mutate(
      # 0 verlorene Saetze/Baelle: Quotient unendlich (sortiert zuoberst), 0/0 = 0
      satzquotient = ifelse(saetze_verloren == 0, ifelse(saetze_gewonnen > 0, Inf, 0),
                            saetze_gewonnen / saetze_verloren),
      ballquotient = ifelse(baelle_verloren == 0, ifelse(baelle_gewonnen > 0, Inf, 0),
                            baelle_gewonnen / baelle_verloren)) |>
    dplyr::arrange(dplyr::desc(punkte), dplyr::desc(siege), dplyr::desc(satzquotient),
                   dplyr::desc(ballquotient), team) |>
    dplyr::mutate(rang = dplyr::row_number())
}

# Qualifikationsspiele einer Gruppe und Saison (Teamsicht)
gruppe_spiele <- function(spiele, saison, gruppe) {
  spiele[spiele$saison == saison & spiele$phase == "Qualifikation" & spiele$gruppe %in% gruppe, ]
}

# Tabelle nach jedem Spieltag (Datum mit mindestens einem Spiel)
tabelle_verlauf <- function(spiele, saison, gruppe) {
  s <- gruppe_spiele(spiele, saison, gruppe)
  if (nrow(s) == 0) return(NULL)
  teams <- dplyr::distinct(s, team_id, team)
  tage <- sort(unique(s$datum))
  dplyr::bind_rows(lapply(tage, function(d)
    dplyr::mutate(tabelle_aus_spielen(s[s$datum <= d, ], teams), datum = d)))
}
