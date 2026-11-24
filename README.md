# Rangliste 1L Männer, Gruppe B (Swiss Volley)

Shiny-App mit der Rangliste aus dem Swiss Volley Game Center. Sie läuft über shinylive
im Browser und wird auf GitHub Pages veröffentlicht. Die Daten werden täglich per
GitHub Action abgerufen.

```
├── app/
│   ├── app.R                      Shiny-App
│   └── data/ranking_history.csv   Verlauf (ein Snapshot pro Tag)
├── scrape_ranking.R               holt die Rangliste, hängt Tages-Snapshot an
└── .github/workflows/
    ├── scrape.yml                 täglich 04:17 UTC: Scraper laufen lassen, CSV committen
    └── deploy.yml                 bei jedem Push auf main (und nach dem Scraper): App auf Pages
```

## Einrichten (einmalig)

1. Auf GitHub ein neues Repository anlegen (öffentlich, denn GitHub Pages ist bei privaten Repos nur mit bezahltem Plan verfügbar).
2. Lokal im Ordner:
   ```bash
   git init -b main
   git add .
   git commit -m "Erste Version"
   git remote add origin https://github.com/<user>/<repo>.git
   git push -u origin main
   ```
3. Auf GitHub: **Settings › Pages › Build and deployment › Source: "GitHub Actions"**.
4. **Actions › App auf GitHub Pages veroeffentlichen › Run workflow** (oder einfach nochmals pushen).
   Die App ist danach unter `https://<user>.github.io/<repo>/` erreichbar.
5. Optional den Scraper einmal von Hand testen: **Actions › Rangliste abrufen › Run workflow**.

## Arbeitsablauf

Änderungen an `app/app.R` committen und pushen, dann wird die App automatisch neu veröffentlicht (dauert ein paar Minuten).

Weil der Bot täglich einen Commit mit neuen Daten macht, **vor dem Pushen immer zuerst `git pull`**
(sonst wird der Push abgelehnt).

## Lokal ausführen

```r
install.packages(c("shiny", "bslib", "DT", "ggplot2", "dplyr", "tidyr", "ggrepel",
                   "rvest", "chromote", "readr", "stringr"))
# im Repo-Root:
source("scrape_ranking.R")   # braucht Google Chrome
shiny::runApp("app")
```

Hunde-Symbole: `remotes::install_github("R-CoderDotCom/ggdogs")`. Lokal zeigt die App dann Hunde.
In der GitHub-Pages-Version (webR) ist ggdogs nicht verfügbar, dort erscheinen normale Punkte.

## Andere Gruppe

In `scrape_ranking.R` die `url` (Parameter `i_league`, `i_phase`, `i_group`) und `group_label` anpassen,
im `app/app.R` den Titel.
