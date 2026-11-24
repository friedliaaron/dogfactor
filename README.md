# Volleyball 1. Liga Männer: Ranglisten, Elo und Matchups

> **Hinweis:** Dies ist ein privates Freizeitprojekt, entstanden als Übung im Programmieren
> und in der Datenanalyse mit R. Es ist weder Forschung noch ein kommerzielles Angebot und
> verfolgt keinerlei kommerzielle Interessen. Es besteht keine Verbindung zu Swiss Volley,
> zu Vereinen oder zu anderen Organisationen.
>
> Die Inhalte erheben keinen Anspruch auf Richtigkeit, Vollständigkeit oder Aktualität;
> es wird keine Gewähr übernommen. Elo-Ratings und Siegwahrscheinlichkeiten sind
> Spielerei und nicht als Grundlage für Entscheidungen oder Wetten gedacht.
> Massgebend sind ausschliesslich die offiziellen Angaben von Swiss Volley.
>
> Die Spieldaten stammen aus dem öffentlich zugänglichen Swiss Volley Game Center
> (volleyball.ch); alle Rechte daran liegen bei den jeweiligen Rechteinhabern.
> Wer mit der Darstellung nicht einverstanden ist oder etwas entfernt haben möchte,
> kann gerne ein Issue in diesem Repository eröffnen.

Shiny-App zu den Spielen der 1. Liga Männer (alle Gruppen, ab Saison 2022/23). Sie läuft
über shinylive direkt im Browser und wird auf GitHub Pages veröffentlicht.

**Seiten der App**

- **Rangliste:** Tabelle jeder Gruppe und Saison zu jedem Spieltag, Rang- und Punkteverlauf,
  Resultatprofil, Satz- und Ballquotient, Restprogramm
- **Spiel-Explorer:** alle Spiele mit Filtern (Saison, Team, Heim/Auswärts, Resultat, Ballquotient usw.)
- **Elo-Rating:** Ratings, Verlauf, Prognosen für offene Spiele, Prognosegüte und eine Erklärung der Berechnung
- **Matchup:** Direktvergleich zweier Teams und Vergleich über gemeinsame Gegner

```
├── app/
│   ├── app.R                  Shiny-App
│   ├── R/                     Hilfsfunktionen (Aufbereitung, Rangliste, Elo), von Shiny automatisch geladen
│   └── data/games.csv         Spiel-Datenbank (eine Zeile pro Spiel)
├── scrape_games.R             holt Spielplan und Resultate aus dem Game Center
└── .github/workflows/
    ├── scrape.yml             jeden Montag 05:00 UTC: Spiele aktualisieren, CSV committen
    └── deploy.yml             nach jedem Push und jedem Scrape: App auf GitHub Pages
```

## Arbeitsablauf

Änderungen committen und pushen, dann wird die App automatisch neu veröffentlicht
(dauert ein paar Minuten). Weil der Bot jeden Montag einen Commit mit neuen Daten macht,
**vor dem Pushen immer zuerst `git pull`**.

Den Scraper von Hand starten: **Actions › Spiele aktualisieren › Run workflow**.

## Lokal ausführen

```r
install.packages(c("shiny", "bslib", "DT", "ggplot2", "dplyr", "tidyr", "ggrepel", "readr",
                   "rvest", "chromote", "xml2", "stringr", "purrr"))
# im Repo-Root:
#   Rscript scrape_games.R daily      # laufende Saison aktualisieren (braucht Google Chrome)
#   Rscript scrape_games.R backfill   # alle Saisons neu einlesen (dauert rund eine Stunde)
shiny::runApp("app")
```
