# Shiny-App: 1. Liga Maenner (Swiss Volley Game Center)
# Liest data/games.csv (scrape_games.R); data/ranking_history.csv (scrape_ranking.R) dient nur
# noch zum Abgleich der berechneten Rangliste mit der offiziellen (Gruppe B).
# Funktionen in R/ (von Shiny automatisch geladen): prep_games.R, tabelle.R, elo.R
# Pakete: install.packages(c("shiny", "bslib", "DT", "ggplot2", "dplyr", "tidyr", "ggrepel", "readr"))

library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

data_file <- file.path("data", "ranking_history.csv")

accent <- "#C8102E"   # Swiss-Volley-aehnliches Rot fuer das hervorgehobene Team
grey   <- "#B8BCC2"

result_cols <- c(
  "Sieg 3:0 / 3:1"      = "#1B7F5A",
  "Sieg 3:2"            = "#7CC4A4",
  "Niederlage 2:3"      = "#F2A97E",
  "Niederlage 0:3 / 1:3" = "#C0392B"
)

# Plot-Thema; auf schmalen Bildschirmen kleinere Schrift und Legende unten
basis_thema <- function(mobil = FALSE) {
  theme_minimal(base_size = if (mobil) 11 else 14) +
    theme(panel.grid.minor = element_blank(),
          plot.title.position = "plot",
          legend.position = if (mobil) "bottom" else "top",
          plot.subtitle = element_text(size = if (mobil) 8.5 else 11, colour = "grey30"))
}
theme_set(basis_thema())

# Teamfarben fuer Verlaufsgrafiken: 8 gut unterscheidbare Farben (auch bei Farbschwaeche
# in dieser Reihenfolge getestet). Mehr als 8 Teams: Farben wiederholt, aber gestrichelt.
# Die Farbe haengt am Team (alphabetisch), nicht am Rang, damit sie stabil bleibt.
team_pal <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")
team_skalen <- function(teams, hl_team = NULL, legende = FALSE) {
  teams <- sort(unique(teams)); i <- seq_along(teams)
  farbe <- setNames(team_pal[(i - 1) %% length(team_pal) + 1], teams)
  linie <- setNames(ifelse(i > length(team_pal), "22", "solid"), teams)
  g <- if (legende) "legend" else "none"   # Farbe und Linientyp teilen sich eine Legende
  list(scale_colour_manual(values = farbe, guide = g, name = NULL),
       scale_linetype_manual(values = linie, guide = g, name = NULL),
       # hervorgehobenes Team dicker, die anderen etwas zurueckgenommen
       scale_linewidth_manual(values = c(`FALSE` = 0.8, `TRUE` = 2), guide = "none"),
       scale_alpha_manual(values = c(`FALSE` = if (is.null(hl_team)) 1 else 0.75, `TRUE` = 1), guide = "none"))
}
# DT: breite Tabellen seitlich scrollbar statt die Seite zu sprengen
options(DT.options = list(scrollX = TRUE))

# --- Regressionsplot mit Residuen -----------------------------------------
# y_log = TRUE: Regression auf log10(y), Residuen als % relativ zur Erwartung.
# y_log = FALSE: lineare Regression, Residuen in Einheiten von y.
reg_plot <- function(d, yvar, y_log, title, y_lab, res_fmt) {
  d$y <- d[[yvar]]
  # nur Teams mit endlichen Werten (z. B. ohne verlorenen Satz: SQ unendlich)
  d <- d[is.finite(d$y) & is.finite(d$ballquotient) & d$spiele > 0 & (!y_log | d$y > 0), ]
  validate(need(nrow(d) >= 3, "Zu wenige Teams mit Spielen für eine Regression."))
  fit <- if (y_log) lm(log10(y) ~ ballquotient, data = d) else lm(y ~ ballquotient, data = d)
  d <- d |>
    mutate(erwartet = if (y_log) 10^fitted(fit) else fitted(fit),
           res      = residuals(fit),
           res_lab  = sprintf(res_fmt, team, if (y_log) 100 * (10^res - 1) else res),
           richtung = ifelse(res > 0, "über Erwartung", "unter Erwartung"))
  r2 <- summary(fit)$r.squared

  # Gut sichtbare Punkte: gefuellt, weisser Rand, hervorgehobenes Team groesser und rot
  punkte_layers <- list(
    geom_point(data = filter(d, !hl), shape = 21, size = 5.5, stroke = 1.2,
               fill = "grey25", colour = "white"),
    geom_point(data = filter(d, hl), shape = 21, size = 7.5, stroke = 1.5,
               fill = accent, colour = "white"))

  ggplot(d, aes(ballquotient, y)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey80") +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                colour = "grey25", fill = "grey85", linewidth = 0.8) +
    geom_segment(aes(xend = ballquotient, yend = erwartet, colour = richtung),
                 linewidth = 0.9) +
    punkte_layers +
    geom_text_repel(aes(label = res_lab), size = 3.6, seed = 1,
                             colour = ifelse(d$hl, accent, "grey30"),
                             fontface = ifelse(d$hl, "bold", "plain"),
                             box.padding = 0.6, min.segment.length = Inf) +
    scale_colour_manual(values = c("über Erwartung" = "#1B7F5A",
                                   "unter Erwartung" = "#C0392B"),
                        name = "Residuum") +
    (if (y_log) scale_y_log10() else scale_y_continuous()) +
    scale_x_continuous(expand = expansion(mult = 0.08)) +
    labs(title = title,
         subtitle = sprintf("%s, R² = %.2f. Senkrechte Linien = Residuen.",
                            if (y_log) sprintf("lm(log10(%s) ~ ballquotient)", yvar) else sprintf("lm(%s ~ ballquotient)", yvar), r2),
         x = "Ballquotient (gewonnene / verlorene Bälle)", y = y_lab)
}

games_file <- file.path("data", "games.csv")

# Hintergrund- und Schriftfarbe pro Resultat (Spiel-Explorer)
res_bg <- c("3:0" = "#1B7F5A", "3:1" = "#1B7F5A", "3:2" = "#7CC4A4",
            "2:3" = "#F2A97E", "1:3" = "#C0392B", "0:3" = "#C0392B")
res_fg <- c("white", "white", "black", "black", "white", "white")

# JS-Darstellung fuer Quotienten: NA (keine Saetze/Baelle abgegeben) als Unendlich,
# beim Sortieren ganz oben
js_quotient <- JS(
  "function(data, type) {",
  "  if (data === null || data === '') return type === 'display' ? '∞' : 1e9;",
  "  return type === 'display' ? Number(data).toFixed(2) : data;",
  "}")

seite_rangliste <- layout_sidebar(
  sidebar = sidebar(
    # Handy: Auswahl immer sichtbar ueber dem Inhalt statt versteckt
    open = list(desktop = "open", mobile = "always-above"),
    selectInput("rl_saison", "Saison", choices = NULL),
    selectInput("rl_gruppe", "Gruppe", choices = NULL),
    selectInput("datum", "Stand nach Spieltag", choices = NULL),
    selectInput("team", "Team hervorheben", choices = NULL),
    hr(),
    uiOutput("stand"),
    tags$small(class = "text-muted",
               "Berechnet aus allen Spielen der Qualifikation (Quelle: volleyball.ch Game Center).",
               "Reihenfolge: Punkte, Siege, Satzquotient, Ballquotient.")
  ),
  navset_card_underline(
    nav_panel("Tabelle", DTOutput("tabelle")),
    nav_panel("Punkte & Resultate", plotOutput("resultate", height = "520px")),
    nav_panel("Rangverlauf", plotOutput("verlauf", height = "520px")),
    nav_panel("Punkteverlauf", plotOutput("punkteverlauf", height = "520px")),
    nav_panel("Satz- vs. Ballquotient", plotOutput("quotienten", height = "560px")),
    nav_panel("Punkte/Spiel vs. Ballquotient", plotOutput("punkte_bq", height = "560px")),
    nav_panel("Restprogramm", DTOutput("restprogramm"))
  )
)

seite_matchup <- layout_sidebar(
  sidebar = sidebar(
    width = 300,
    open = list(desktop = "open", mobile = "always-above"),
    selectizeInput("mu_a", "Team A", choices = NULL),
    selectizeInput("mu_b", "Team B", choices = NULL),
    actionButton("mu_tausch", "A und B tauschen", class = "btn-sm btn-outline-secondary"),
    selectizeInput("mu_saison", "Saisons (leer = alle)", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "alle", plugins = list("remove_button"))),
    tags$small(class = "text-muted",
               "Direktvergleich: alle Spiele A gegen B (alle Phasen). Gemeinsame Gegner: Teams,",
               "gegen die A und B in derselben Saison gespielt haben.")
  ),
  navset_card_underline(
    nav_panel("Direktvergleich",
              uiOutput("mu_bilanz"),
              uiOutput("mu_naechstes"),
              plotOutput("mu_plot", height = "300px", fill = FALSE),
              DTOutput("mu_duelle")),
    nav_panel("Gemeinsame Gegner",
              uiOutput("mu_gg_text"),
              DTOutput("mu_gg_tab")),
    nav_panel("Elo-Verlauf", plotOutput("mu_elo", height = "480px"))
  )
)

seite_explorer <- layout_sidebar(
  sidebar = sidebar(
    width = 320,
    radioButtons("ex_ansicht", "Ansicht",
                 c("Spiele" = "spiele", "Teams pro Saison" = "saison",
                   "Teams über alle gewählten Saisons" = "gesamt")),
    selectizeInput("ex_saison", "Saison", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "alle")),
    selectizeInput("ex_gruppe", "Gruppe", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "alle")),
    selectizeInput("ex_phase", "Phase", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "alle")),
    selectizeInput("ex_team", "Team", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "alle")),
    selectizeInput("ex_gegner", "Gegner", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "alle")),
    checkboxGroupInput("ex_ort", "Heim / Auswärts", c("Heim", "Auswärts"),
                       selected = c("Heim", "Auswärts"), inline = TRUE),
    checkboxGroupInput("ex_resultat", "Resultat (aus Teamsicht)", resultat_stufen,
                       selected = resultat_stufen, inline = TRUE),
    radioButtons("ex_satz1", "1. Satz", c("alle", "gewonnen", "verloren"), inline = TRUE),
    dateRangeInput("ex_datum", "Zeitraum", start = NULL, end = NULL,
                   format = "dd.mm.yyyy", language = "de", separator = "bis"),
    sliderInput("ex_bq", "Ballquotient im Spiel", min = 0, max = 3, value = c(0, 3), step = 0.05),
    sliderInput("ex_bpdiff", "Ballpunktdifferenz im Spiel", min = -75, max = 75,
                value = c(-75, 75), step = 1),
    actionButton("ex_reset", "Filter zurücksetzen", class = "btn-sm btn-outline-secondary"),
    downloadButton("ex_download", "Tabelle als CSV", class = "btn-sm"),
    tags$small(class = "text-muted",
               "Jedes Spiel erscheint zweimal (Sicht beider Teams). Für eine Zeile pro Spiel",
               "nur \"Heim\" wählen. Die Filter wirken auf die Spiele, die Teamansichten",
               "zählen die gefilterten Spiele zusammen (z. B. nur Heimspiele = Heimtabelle).",
               "Teamnamen: jeweils neuster Name.")
  ),
  card(
    card_header(textOutput("ex_info", inline = TRUE)),
    DTOutput("ex_tabelle")
  )
)

# Erklaerung der Elo-Berechnung (Tab "So wird gerechnet"); Zahlen aus elo_par
elo_erklaerung_ui <- function() {
  K <- elo_par[["K"]]; H <- elo_par[["H"]]; cc <- elo_par[["c"]]
  neu <- elo_par[["neu"]]; g <- elo_par[["g"]]
  fz <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = "'")
  schritt <- function(nr, titel, ...) card(card_header(tags$strong(sprintf("Schritt %d: %s", nr, titel))), ...)

  withMathJax(div(
    style = "max-width: 980px;",
    p("Jedes Team hat eine Elo-Zahl. Der Durchschnitt liegt bei 1500. Nach jedem Spiel gibt",
      "der Gewinner Punkte vom Verlierer ab, und zwar umso mehr, je überraschender das Ergebnis war.",
      "Die Summe aller Ratings bleibt dabei gleich. Die Berechnung läuft chronologisch über alle",
      "Spiele seit 2022/23, über alle Gruppen und Phasen; jedes Team wird über seine Team-ID",
      "verfolgt, auch bei Namenswechseln."),

    schritt(1, "Erwartung vor dem Spiel",
      p("Aus der Differenz der Ratings plus Heimvorteil ergibt sich die Erwartung \\(E\\) des Heimteams:"),
      p("$$E = \\frac{1}{1 + 10^{-(R_\\text{Heim} + H - R_\\text{Gast})/400}}$$"),
      p(sprintf("Der Heimvorteil beträgt H = %s Elo-Punkte (an neutralen Orten, z. B. Finalturnieren, 0).", fz(H, 1)),
        "Bei gleich starken Teams ist \\(E\\) etwas über 0.5. Eine Differenz von 100 Punkten ergibt",
        "\\(E \\approx 0.64\\), 200 Punkte \\(E \\approx 0.76\\)."),
      p(tags$em("E ist eine Erwartung für das Ergebnis S aus Schritt 2, nicht direkt die",
                "Siegwahrscheinlichkeit (siehe Schritt 5)."))),

    schritt(2, "Ergebnis S aus den Bällen",
      p("Massgebend ist nicht der Sieg, sondern der Anteil gewonnener Bälle \\(b\\) im Spiel.",
        "Daraus wird ein Ergebnis \\(S\\) zwischen 0 und 1:"),
      p("$$S = \\frac{b^2}{b^2 + (1-b)^2}$$"),
      p("Das Quadrat verstärkt kleine Unterschiede etwas: 52% der Bälle ergeben S = 0.54,",
        "55% ergeben S = 0.60, 60% ergeben S = 0.69."),
      p(tags$strong("Wichtig:"), "Die Sätze zählen nicht direkt. Ein 3:2-Sieg mit weniger Bällen als",
        "der Gegner ergibt S < 0.5 und kostet Elo-Punkte. Grund: Die Analyse",
        "\"Können vs. Glück\" zeigte, dass Ballpunkte die Leistung am zuverlässigsten messen,",
        "während Satz- und Spielresultate bei gleicher Ballleistung stark vom Zufall abhängen",
        "(knappe Sätze, Tie-Breaks). Ratings mit Bällen sagen künftige Spiele besser voraus."),
      tableOutput("elo_erkl_s_tab")),

    schritt(3, "Rating anpassen",
      p("$$\\Delta = K \\cdot (S - E), \\qquad R_\\text{Heim} \\leftarrow R_\\text{Heim} + \\Delta,",
        "\\quad R_\\text{Gast} \\leftarrow R_\\text{Gast} - \\Delta$$"),
      p(sprintf("Mit K = %s. Spielt ein Team genau wie erwartet (S = E), ändert sich nichts.", fz(K, 1)),
        "Spielt es besser als erwartet, gewinnt es Punkte, sonst verliert es. Ein Favorit muss also",
        "deutlich gewinnen, um zuzulegen; ein Aussenseiter gewinnt schon mit einer knappen Niederlage",
        "Punkte, wenn er besser mithält als erwartet.")),

    schritt(4, "Saisonwechsel und neue Teams",
      p("$$R_\\text{neu} = 1500 + c \\cdot (R_\\text{alt} - 1500)$$"),
      p(sprintf("Mit c = %s: %s%% des Abstands zu 1500 werden übernommen. Ein Team mit 1700 startet",
                fz(cc, 2), fz(100 * cc)),
        sprintf("die neue Saison mit %s.", fz(1500 + cc * 200)),
        "So fliesst die Vorsaison ein, Kaderwechsel werden aber berücksichtigt.",
        sprintf("Neue Teams (Aufsteiger) starten mit %s.", fz(1500 + neu)))),

    schritt(5, "Siegwahrscheinlichkeit",
      p("Weil \\(S\\) auf Bällen beruht, liegt \\(E\\) näher bei 0.5 als die Siegchance (ein Team mit",
        "52% der Bälle gewinnt meistens). Darum wird \\(E\\) für die Prognose gestreckt:"),
      p("$$P(\\text{Heimsieg}) = \\frac{1}{1 + \\left(\\frac{1-E}{E}\\right)^{g}}$$"),
      p(sprintf("mit g = %s. Bei gleich starken Teams ergibt das %s%% Heimsiegchance.",
                fz(g, 2), fz(100 * elo_p_heim(1500, 1500))))),

    card(card_header(tags$strong("Ausprobieren: Wie wirkt sich ein Resultat aus?")),
      p("Teams im Matchup-Rechner links wählen, dann ein Resultat in Bällen eingeben."),
      layout_columns(
        col_widths = c(3, 3, 6), fill = FALSE,
        numericInput("elo_erkl_bh", "Bälle Heim", 100, min = 0, max = 200),
        numericInput("elo_erkl_bg", "Bälle Gast", 92, min = 0, max = 200),
        uiOutput("elo_erkl_rechnung")
      ),
      plotOutput("elo_erkl_plot", height = "320px", fill = FALSE)),

    card(card_header(tags$strong("Beispiel: letztes Spiel des hervorgehobenen Teams")),
      uiOutput("elo_erkl_beispiel")),

    card(card_header(tags$strong("Woher die Parameter kommen")),
      p("K, H, c, g und die Abstufung der Bälle wurden so gewählt, dass die Siegprognosen der",
        "Saisons 2023/24 und 2024/25 möglichst gut waren (Log-Loss). Geprüft wurde an 2025/26,",
        "das nicht zur Schätzung verwendet wurde: Die Ball-Variante sagte dort 77% der Spiele",
        "richtig voraus und war besser als Elo mit reinen Siegen und als ein Modell ohne",
        "Vorsaison. Die Parameter sind seither eingefroren. Details: analysen/03_elo.qmd."))
  ))
}

seite_elo <- layout_sidebar(
  sidebar = sidebar(
    width = 300,
    selectInput("elo_saison", "Saison", choices = NULL),
    selectInput("elo_gruppe", "Gruppe", choices = NULL),
    selectInput("elo_team", "Team hervorheben", choices = NULL),
    hr(),
    tags$strong("Matchup-Rechner"),
    selectizeInput("elo_heim", "Heimteam", choices = NULL),
    selectizeInput("elo_gast", "Gastteam", choices = NULL),
    checkboxInput("elo_neutral", "Neutraler Ort", FALSE),
    uiOutput("elo_matchup"),
    tags$small(class = "text-muted",
               "Elo mit Ballanteil als Ergebnis, Heimvorteil und Übernahme von 80% des",
               "Vorsaison-Abstands zu 1500. Parameter geschätzt in analysen/03_elo.qmd",
               "(2023/24 bis 2025/26). Ratings laufen über alle Phasen und Saisons.")
  ),
  navset_card_underline(
    nav_panel("Tabelle", DTOutput("elo_tabelle")),
    nav_panel("Verlauf", plotOutput("elo_verlauf", height = "540px")),
    nav_panel("Alle Saisons",
              selectizeInput("elo_lang_teams", "Teams (Vorauswahl: gewählte Gruppe)", choices = NULL,
                             multiple = TRUE, width = "100%",
                             options = list(placeholder = "Teams wählen", plugins = list("remove_button"))),
              plotOutput("elo_lang", height = "580px", fill = FALSE)),
    nav_panel("Nächste Spiele",
              checkboxInput("elo_prog_alle", "Alle Gruppen", FALSE),
              DTOutput("elo_prognosen")),
    nav_panel("Prognosegüte",
              p(class = "text-muted mb-1",
                "Wie gut sagt Elo die Sieger voraus? Jede Prognose nutzt nur Spiele vor dem Spiel.",
                "Saison oben links wählen."),
              checkboxInput("elo_guete_gruppe", "Nur die gewählte Gruppe", FALSE),
              uiOutput("elo_guete_hinweis"),
              uiOutput("elo_guete_boxen"),
              # fill = FALSE: sonst quetscht der Tab-Container die Plots auf wenige Pixel
              # zusammen ("figure margins too large")
              layout_columns(
                col_widths = c(6, 6), fill = FALSE,
                plotOutput("elo_guete_kalib", height = "360px", fill = FALSE),
                plotOutput("elo_guete_verlauf", height = "360px", fill = FALSE)),
              uiOutput("elo_guete_tab_titel"),
              tableOutput("elo_guete_baender"),
              tags$h6(class = "mt-3", "Grösste Überraschungen (Favorit mit mindestens 75% verlor)"),
              DTOutput("elo_guete_upsets")),
    nav_panel("So wird gerechnet", elo_erklaerung_ui())
  )
)

# --- Anpassungen fuer kleine Bildschirme (Handy) -----------------------------
mobil_css <- "
@media (max-width: 700px) {
  /* Tab-Leisten: nicht umbrechen, sondern seitlich wischen */
  .card-header .nav, .nav-underline, .nav-tabs {
    flex-wrap: nowrap; overflow-x: auto; overflow-y: hidden; white-space: nowrap;
    -webkit-overflow-scrolling: touch; scrollbar-width: thin;
  }
  .card-header .nav .nav-link { padding: .35rem .55rem; font-size: .9rem; }
  /* kompaktere Karten und Tabellen */
  .card-body { padding: .5rem; }
  .bslib-sidebar-layout > .main { padding: .5rem; }
  table.dataTable, .dataTables_wrapper, .table { font-size: .78rem; }
  table.dataTable td, table.dataTable th { padding: 4px 6px !important; }
  .dataTables_wrapper .dataTables_filter input { width: 8em; }
  .bslib-value-box .value-box-value { font-size: 1.5rem; }
  .bslib-value-box .value-box-title { font-size: .85rem; }
  .alert { font-size: .85rem; }
  h5 { font-size: 1rem; } h6 { font-size: .9rem; }
  .navbar-brand { font-size: 1rem; }
}
"
# Banner: Bild als Hintergrund der Navigationsleiste (keine zusaetzliche Hoehe, nichts verschiebt sich)
banner_css <- "
.navbar {
  background-image: linear-gradient(rgba(0,0,0,.45), rgba(0,0,0,.45)), url('big_dawg.jpg') !important;
  background-size: cover !important;
  background-position: center 55% !important;
}
.navbar .navbar-brand, .navbar .nav-link { color: #fff !important; text-shadow: 0 1px 3px rgba(0,0,0,.7); }
.navbar .nav-link.active { border-bottom: 2px solid #fff; }
#sound_knopf { font-size: 1.15rem; line-height: 1; cursor: pointer; }
"

# Bellen beim Oeffnen: Browser erlauben Ton erst nach einer Interaktion. Darum zuerst
# versuchen, sonst beim ersten Klick/Tipp abspielen. Stummschaltung merkt sich der Browser.
sound_js <- "
(function() {
  var bellen = new Audio('bark.mp3'); bellen.volume = 0.6;
  var KEY = 'volley_sound_stumm', erledigt = false;
  function stumm() { try { return localStorage.getItem(KEY) === '1'; } catch (e) { return false; } }
  function setzeStumm(s) { try { localStorage.setItem(KEY, s ? '1' : '0'); } catch (e) {} }
  function knopf() { $('#sound_knopf').text(stumm() ? '\U0001F507' : '\U0001F50A'); }
  function abspielen() { bellen.currentTime = 0; return bellen.play(); }
  function ersterTipp() {
    document.removeEventListener('pointerdown', ersterTipp, true);
    if (!erledigt && !stumm()) { erledigt = true; abspielen().catch(function() {}); }
  }
  $(function() {
    knopf();
    if (!stumm()) {
      abspielen().then(function() { erledigt = true; })
                 .catch(function() { document.addEventListener('pointerdown', ersterTipp, true); });
    }
  });
  $(document).on('click', '#sound_knopf', function(e) {
    e.preventDefault();
    var s = !stumm(); setzeStumm(s); knopf();
    if (s) { bellen.pause(); } else { erledigt = true; abspielen().catch(function() {}); }
  });
})();
"

# Fensterbreite an den Server melden (Plots passen Schrift und Legende daran an)
mobil_js <- "
$(document).on('shiny:connected', function() { Shiny.setInputValue('breite', window.innerWidth); });
var breiteTimer;
$(window).on('resize', function() {
  clearTimeout(breiteTimer);
  breiteTimer = setTimeout(function() { Shiny.setInputValue('breite', window.innerWidth); }, 300);
});
"

ui <- page_navbar(
  title = "Swiss Volley 1. Liga Männer",
  theme = bs_theme(version = 5, primary = accent),
  header = tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML(mobil_css)),
    tags$style(HTML(banner_css)),
    tags$script(HTML(mobil_js)),
    tags$script(HTML(sound_js))),
  navbar_options = navbar_options(theme = "dark"),
  nav_panel("Rangliste", seite_rangliste),
  nav_panel("Spiel-Explorer", seite_explorer),
  nav_panel("Elo-Rating", seite_elo),
  nav_panel("Matchup", seite_matchup),
  nav_spacer(),
  nav_item(tags$a(id = "sound_knopf", class = "nav-link", href = "#", title = "Ton an/aus",
                  `aria-label` = "Ton an oder aus", "\U0001F50A"))
)

server <- function(input, output, session) {

  # Schmaler Bildschirm (Handy)? Breite kommt per JS (mobil_js) als input$breite
  mobil <- reactive(isTRUE(input$breite < 700))
  thema_anwenden <- function() theme_set(basis_thema(mobil()))

  # === Rangliste (aus den Spieldaten, R/tabelle.R) ==========================
  # Offizielle Rangliste Gruppe B (scrape_ranking.R), nur noch zum Abgleich
  offiziell <- reactiveFileReader(60 * 1000, session, data_file, function(f) {
    read.csv(f, fileEncoding = "UTF-8", stringsAsFactors = FALSE) |>
      mutate(datum = as.Date(datum))
  })

  rl_gruppen <- reactive(spiele() |> filter(phase == "Qualifikation") |> distinct(saison, gruppe))

  observe({
    s <- sort(unique(rl_gruppen()$saison), decreasing = TRUE)
    alt <- isolate(input$rl_saison)
    updateSelectInput(session, "rl_saison", choices = setNames(s, saison_label(s)),
                      selected = if (isTRUE(alt %in% s)) alt else s[1])
  })

  observeEvent(input$rl_saison, {
    gr <- sort(rl_gruppen()$gruppe[rl_gruppen()$saison == as.integer(input$rl_saison)])
    sel <- if (isTRUE(input$rl_gruppe %in% gr)) input$rl_gruppe else
      if ("Gruppe B" %in% gr) "Gruppe B" else gr[1]
    updateSelectInput(session, "rl_gruppe", choices = gr, selected = sel)
  })

  # Tabelle nach jedem Spieltag der gewaehlten Gruppe und Saison
  history <- reactive({
    req(input$rl_saison, input$rl_gruppe)
    h <- tabelle_verlauf(spiele(), as.integer(input$rl_saison), input$rl_gruppe)
    validate(need(!is.null(h), "Noch keine gespielten Spiele in dieser Gruppe."))
    h
  })

  observeEvent(history(), {
    h <- history()
    tage <- sort(unique(h$datum), decreasing = TRUE)
    dl <- setNames(as.character(tage), format(tage, "%d.%m.%Y"))
    updateSelectInput(session, "datum", choices = dl,
                      selected = if (isTRUE(input$datum %in% dl)) input$datum else dl[1])
    teams <- h |> filter(datum == max(datum)) |> arrange(rang) |> pull(team)
    updateSelectInput(session, "team", choices = teams,
                      selected = if (isTRUE(input$team %in% teams)) input$team else teams[1])
  })

  current <- reactive({
    req(input$datum)
    d <- history() |> filter(datum == as.Date(input$datum)) |> arrange(rang)
    req(nrow(d) > 0)
    d
  })

  output$stand <- renderUI({
    h <- history(); d <- current()
    txt <- sprintf("Stand nach Spieltag %s (%d von %d Spieltagen).",
                   format(d$datum[1], "%d.%m.%Y"),
                   match(d$datum[1], sort(unique(h$datum))), length(unique(h$datum)))
    # Abgleich mit der offiziellen Rangliste (nur dort vorhanden: Gruppe B, laufende Saison)
    abgleich <- NULL
    o <- tryCatch(offiziell(), error = function(e) NULL)
    if (!is.null(o) && input$rl_gruppe == "Gruppe B" &&
        as.integer(input$rl_saison) == max(rl_gruppen()$saison) && d$datum[1] == max(h$datum)) {
      o <- o[o$datum == max(o$datum), ]
      m <- merge(d, o, by = "team")
      gleich <- sum(m$rang.x == m$rang.y & m$punkte.x == m$punkte.y)
      abgleich <- sprintf("Abgleich mit offizieller Rangliste vom %s: %d von %d Teams mit gleichem Rang und gleichen Punkten.",
                          format(max(o$datum), "%d.%m.%Y"), gleich, nrow(d))
    }
    tagList(tags$small(txt), if (!is.null(abgleich)) tags$small(class = "d-block mt-1", abgleich))
  })

  # --- Tabelle -----------------------------------------------------------
  output$tabelle <- renderDT({
    d <- current() |>
      transmute(`#` = rang, Team = team, Sp. = spiele, Pu. = punkte,
                S = siege, `S+` = siege_klar, `S-` = siege_tb,
                N = niederlagen, `N+` = niederlagen_tb, `N-` = niederlagen_klar,
                `Sätze` = paste(saetze_gewonnen, ":", saetze_verloren),
                SQ = ifelse(is.infinite(satzquotient), NA, satzquotient),
                `Bälle` = paste(baelle_gewonnen, ":", baelle_verloren),
                BQ = ifelse(is.infinite(ballquotient), NA, ballquotient))
    quot <- which(names(d) %in% c("SQ", "BQ")) - 1
    datatable(d, rownames = FALSE,
              options = list(dom = "t", pageLength = 20, ordering = TRUE,
                             columnDefs = list(list(targets = quot, render = js_quotient)))) |>
      formatStyle("Team", target = "row",
                  fontWeight = styleEqual(input$team, "bold"),
                  backgroundColor = styleEqual(input$team, "#FBE7EA"))
  })

  # --- Punkte & Resultatprofil ------------------------------------------
  output$resultate <- renderPlot({
    thema_anwenden()
    d <- current() |>
      mutate(team = factor(team, levels = rev(team))) |>
      select(team, punkte,
             `Sieg 3:0 / 3:1` = siege_klar, `Sieg 3:2` = siege_tb,
             `Niederlage 2:3` = niederlagen_tb,
             `Niederlage 0:3 / 1:3` = niederlagen_klar) |>
      pivot_longer(-c(team, punkte), names_to = "resultat", values_to = "n") |>
      mutate(resultat = factor(resultat, levels = names(result_cols)))

    pts <- distinct(d, team, punkte)

    ggplot(d, aes(x = n, y = team, fill = resultat)) +
      geom_col(width = 0.7, position = position_stack(reverse = TRUE)) +
      geom_text(data = pts, aes(x = -0.3, y = team, label = paste(punkte, "Pt.")),
                inherit.aes = FALSE, hjust = 1, size = 4,
                fontface = ifelse(pts$team == input$team, "bold", "plain")) +
      scale_fill_manual(values = result_cols, name = NULL) +
      scale_x_continuous(expand = expansion(mult = c(0.12, 0.02)),
                         breaks = function(l) seq(0, ceiling(l[2]), by = 1)) +
      labs(title = "Spiele nach Resultat", subtitle = "Links: Punkte in der Rangliste",
           x = "Anzahl Spiele", y = NULL)
  })

  # --- Rangverlauf -------------------------------------------------------
  output$verlauf <- renderPlot({
    thema_anwenden()
    h <- history()
    validate(need(length(unique(h$datum)) > 1,
                  "Der Verlauf erscheint ab dem zweiten Spieltag."))
    h <- h |> mutate(hl = team == input$team)
    last <- h |> filter(datum == max(datum))

    ggplot(h, aes(datum, rang, group = team, colour = team, linetype = team)) +
      geom_vline(xintercept = as.Date(input$datum), colour = "grey85", linetype = "dashed") +
      geom_line(aes(linewidth = hl, alpha = hl)) +
      geom_point(data = filter(h, hl), size = 2.5) +
      geom_point(data = last, size = 3) +
      geom_text(data = last, aes(label = team), colour = "grey15",
                fontface = ifelse(last$hl, "bold", "plain"),
                hjust = -0.12, size = if (mobil()) 3 else 3.8) +
      team_skalen(h$team, input$team) +
      scale_y_reverse(breaks = 1:max(h$rang)) +
      scale_x_date(expand = expansion(mult = c(0.02, 0.3))) +
      labs(title = sprintf("Rang über die Zeit, %s %s", input$rl_gruppe, saison_label(as.integer(input$rl_saison))),
           subtitle = "Gestrichelt: gewählter Spieltag", x = NULL, y = "Rang")
  })

  output$punkteverlauf <- renderPlot({
    thema_anwenden()
    h <- history() |> mutate(hl = team == input$team)
    validate(need(length(unique(h$datum)) > 1, "Der Verlauf erscheint ab dem zweiten Spieltag."))
    last <- h |> filter(datum == max(datum))
    ggplot(h, aes(datum, punkte, group = team, colour = team, linetype = team)) +
      geom_vline(xintercept = as.Date(input$datum), colour = "grey85", linetype = "dashed") +
      geom_step(aes(linewidth = hl, alpha = hl)) +
      geom_point(data = last, size = 3) +
      geom_text_repel(data = last, aes(label = paste0(team, " (", punkte, ")")), colour = "grey15",
                      fontface = ifelse(last$hl, "bold", "plain"),
                      direction = "y", hjust = 0, nudge_x = 3, size = if (mobil()) 3 else 3.6,
                      segment.colour = "grey85", seed = 1) +
      team_skalen(h$team, input$team) +
      scale_x_date(expand = expansion(mult = c(0.02, 0.3))) +
      labs(title = sprintf("Punkte über die Zeit, %s %s", input$rl_gruppe, saison_label(as.integer(input$rl_saison))),
           subtitle = "Kumulierte Meisterschaftspunkte nach jedem Spieltag", x = NULL, y = "Punkte")
  })

  output$restprogramm <- renderDT({
    sz <- as.integer(input$rl_saison)
    g <- games_roh()
    offen <- g |> filter(!gespielt, saison == sz, phase == "Qualifikation", gruppe == input$rl_gruppe)
    validate(need(nrow(offen) > 0, "Keine offenen Qualifikationsspiele in dieser Gruppe."))
    zwei <- bind_rows(
      offen |> transmute(datum, team_id = heim_id, gegner_id = gast_id, heim = TRUE),
      offen |> transmute(datum, team_id = gast_id, gegner_id = heim_id, heim = FALSE)) |>
      mutate(r_team = elo_rating_von(team_id), r_gegner = elo_rating_von(gegner_id),
             p_sieg = ifelse(heim, elo_p_heim(r_team, r_gegner), 1 - elo_p_heim(r_gegner, r_team)))
    namen <- aktuelle_namen(g)
    stand <- history() |> filter(datum == max(datum)) |> select(team_id, rang, punkte, spiele)
    rest <- zwei |>
      arrange(datum) |>
      group_by(team_id) |>
      summarise(rest = n(), heim = sum(heim), elo = mean(r_team), elo_gegner = mean(r_gegner),
                erw_siege = sum(p_sieg),
                naechster = paste0(namen$name[match(first(gegner_id), namen$id)],
                                   ifelse(first(heim), " (H)", " (A)"),
                                   ", ", format(first(datum), "%d.%m.")),
                .groups = "drop") |>
      left_join(stand, by = "team_id") |>
      mutate(team = namen$name[match(team_id, namen$id)]) |>
      arrange(rang)
    d <- rest |> transmute(Rang = rang, Team = team, Pu. = punkte, Sp. = spiele,
                           `Rest-Spiele` = rest, `davon Heim` = heim,
                           `Elo` = round(elo), `Ø Elo Gegner` = round(elo_gegner),
                           `Erwartete Siege` = round(erw_siege, 1),
                           `Erwartete Siegquote` = erw_siege / rest,
                           `Nächstes Spiel` = naechster)
    datatable(d, rownames = FALSE,
              caption = tags$caption(style = "caption-side: bottom; font-size: 0.85em;",
                "Erwartete Siege: Summe der Elo-Siegchancen in den offenen Spielen (aktuelle Ratings).",
                "Ø Elo Gegner hoch = schwieriges Restprogramm. H = Heimspiel, A = Auswärtsspiel."),
              options = list(dom = "t", pageLength = 30)) |>
      formatPercentage("Erwartete Siegquote", 0) |>
      formatStyle("Ø Elo Gegner", background = styleColorBar(range(d$`Ø Elo Gegner`) + c(-20, 0), "#F2C4CC"),
                  backgroundSize = "98% 70%", backgroundRepeat = "no-repeat", backgroundPosition = "center") |>
      formatStyle("Team", target = "row",
                  fontWeight = styleEqual(input$team, "bold"),
                  backgroundColor = styleEqual(input$team, "#FBE7EA"))
  })

  # --- Regressionsplots -------------------------------------------------
  output$quotienten <- renderPlot({
    thema_anwenden()
    req(input$team)
    reg_plot(current() |> mutate(hl = team == input$team),
             "satzquotient", y_log = TRUE,
             title = "Ballquotient vs. Satzquotient",
             y_lab = "Satzquotient (log-Skala)",
             res_fmt = "%s (%+.0f%%)")
  })

  output$punkte_bq <- renderPlot({
    thema_anwenden()
    req(input$team)
    reg_plot(current() |> mutate(hl = team == input$team,
                                 punkte_pro_spiel = punkte / spiele),
             "punkte_pro_spiel", y_log = FALSE,
             title = "Ballquotient vs. Punkte pro Spiel",
             y_lab = "Punkte pro Spiel",
             res_fmt = "%s (%+.2f Pt./Spiel)")
  })

  # === Matchup-Explorer =====================================================
  observe({
    namen <- aktuelle_namen(games_roh())
    ch <- setNames(namen$id, namen$name)[order(namen$name)]
    alt_a <- isolate(input$mu_a); alt_b <- isolate(input$mu_b)
    updateSelectizeInput(session, "mu_a", choices = ch,
                         selected = if (isTRUE(alt_a %in% ch)) alt_a else ch[1])
    updateSelectizeInput(session, "mu_b", choices = ch,
                         selected = if (isTRUE(alt_b %in% ch)) alt_b else ch[2])
    s <- sort(unique(games_roh()$saison), decreasing = TRUE)
    alt <- isolate(input$mu_saison)
    # Vorauswahl: laufende Saison (leer lassen = alle Saisons)
    updateSelectizeInput(session, "mu_saison", choices = setNames(s, saison_label(s)),
                         selected = if (length(alt)) alt else s[1])
  })
  observeEvent(input$mu_tausch, {
    a <- input$mu_a; b <- input$mu_b
    updateSelectizeInput(session, "mu_a", selected = b)
    updateSelectizeInput(session, "mu_b", selected = a)
  })

  mu_ids <- reactive({
    req(input$mu_a, input$mu_b)
    a <- as.integer(input$mu_a); b <- as.integer(input$mu_b)
    validate(need(a != b, "Bitte zwei verschiedene Teams wählen."))
    namen <- aktuelle_namen(games_roh())
    list(a = a, b = b, na = namen$name[match(a, namen$id)], nb = namen$name[match(b, namen$id)])
  })
  mu_saisons <- reactive(if (length(input$mu_saison)) as.integer(input$mu_saison) else unique(games_roh()$saison))

  # Alle gespielten Duelle aus Sicht von A, mit Elo-Prognose vor dem Spiel
  mu_duelle <- reactive({
    id <- mu_ids()
    g <- games_roh() |>
      filter(gespielt, saison %in% mu_saisons(),
             (heim_id == id$a & gast_id == id$b) | (heim_id == id$b & gast_id == id$a))
    if (nrow(g) == 0) return(NULL)
    p <- elo()$spiele |> select(game_id, p_heim)
    g |> left_join(p, by = "game_id") |>
      arrange(datum) |>
      mutate(a_heim = heim_id == id$a,
             sa = ifelse(a_heim, saetze_heim, saetze_gast), sb = ifelse(a_heim, saetze_gast, saetze_heim),
             ba = ifelse(a_heim, baelle_heim, baelle_gast), bb = ifelse(a_heim, baelle_gast, baelle_heim),
             sieg_a = sa > sb, tb = sa + sb == 5,
             pkt_a = ifelse(sieg_a, ifelse(tb, 2L, 3L), ifelse(tb, 1L, 0L)), pkt_b = 3L - pkt_a,
             p_a = ifelse(a_heim, p_heim, 1 - p_heim),
             saetze_txt = apply(cbind(s1_h, s1_g, s2_h, s2_g, s3_h, s3_g, s4_h, s4_g, s5_h, s5_g), 1,
                                function(r) {
                                  m <- matrix(r, ncol = 2, byrow = TRUE); m <- m[!is.na(m[, 1]), , drop = FALSE]
                                  paste(sprintf("%d:%d", m[, 1], m[, 2]), collapse = ", ")
                                }))
  })

  output$mu_bilanz <- renderUI({
    id <- mu_ids(); d <- mu_duelle()
    validate(need(!is.null(d), sprintf("%s und %s haben in den gewählten Saisons nie gegeneinander gespielt.",
                                       id$na, id$nb)))
    zeile <- function(x, label) {
      if (nrow(x) == 0) return(NULL)
      tags$tr(tags$td(label), tags$td(nrow(x)),
              tags$td(sprintf("%d : %d", sum(x$sieg_a), sum(!x$sieg_a))),
              tags$td(sprintf("%d : %d", sum(x$sa), sum(x$sb))),
              tags$td(sprintf("%d : %d (%.1f%%)", sum(x$ba, na.rm = TRUE), sum(x$bb, na.rm = TRUE),
                              100 * sum(x$ba, na.rm = TRUE) / sum(x$ba + x$bb, na.rm = TRUE))),
              tags$td(sprintf("%d : %d", sum(x$pkt_a), sum(x$pkt_b))))
    }
    tagList(
      tags$h5(sprintf("%s gegen %s", id$na, id$nb)),
      div(class = "table-responsive", tags$table(class = "table table-sm",
        tags$thead(tags$tr(tags$th(""), tags$th("Spiele"), tags$th("Siege A : B"), tags$th("Sätze"),
                           tags$th("Bälle (Anteil A)"), tags$th("Punkte"))),
        tags$tbody(zeile(d, "Total"),
                   zeile(filter(d, a_heim), sprintf("bei %s", id$na)),
                   zeile(filter(d, !a_heim), sprintf("bei %s", id$nb))))))
  })

  output$mu_naechstes <- renderUI({
    id <- mu_ids()
    k <- games_roh() |>
      filter(!gespielt, (heim_id == id$a & gast_id == id$b) | (heim_id == id$b & gast_id == id$a)) |>
      arrange(datum)
    if (nrow(k) == 0) return(NULL)
    k <- k[1, ]
    p_heim <- elo_p_heim(elo_rating_von(k$heim_id), elo_rating_von(k$gast_id),
                         neutral = k$phase != "Qualifikation")
    p_a <- if (k$heim_id == id$a) p_heim else 1 - p_heim
    div(class = "alert alert-light py-2",
        tags$strong("Nächstes Duell: "),
        sprintf("%s, %s %s. Elo-Siegchance %s: %.0f%%, %s: %.0f%%.",
                format(k$datum, "%d.%m.%Y"), if (k$heim_id == id$a) id$na else id$nb, "(Heim)",
                id$na, 100 * p_a, id$nb, 100 * (1 - p_a)))
  })

  output$mu_plot <- renderPlot({
    thema_anwenden()
    id <- mu_ids(); d <- mu_duelle()
    req(!is.null(d))
    d <- d |> mutate(anteil = ba / (ba + bb), ort = ifelse(a_heim, paste("bei", id$na), paste("bei", id$nb)))
    ggplot(d, aes(datum, anteil)) +
      geom_hline(yintercept = 0.5, colour = "grey70") +
      geom_line(colour = "grey75") +
      geom_point(aes(fill = sieg_a, shape = ort), size = 5, stroke = 1, colour = "white") +
      geom_text(aes(label = paste0(sa, ":", sb)), vjust = -1.2, size = 3.4, colour = "grey30") +
      scale_fill_manual(values = c(`TRUE` = "#1B7F5A", `FALSE` = "#C0392B"),
                        labels = c(`TRUE` = paste(id$na, "gewinnt"), `FALSE` = paste(id$nb, "gewinnt")), name = NULL) +
      scale_shape_manual(values = c(21, 24), name = NULL) +
      guides(fill = guide_legend(override.aes = list(shape = 21))) +
      scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"),
                         expand = expansion(mult = c(0.1, 0.2))) +
      labs(title = sprintf("Ballanteil %s in jedem Duell", id$na), x = NULL, y = "Ballanteil")
  })

  output$mu_duelle <- renderDT({
    id <- mu_ids(); d <- mu_duelle()
    req(!is.null(d))
    t <- d |> arrange(desc(datum)) |>
      transmute(Datum = datum, Saison = saison_label(saison), Phase = phase,
                Heim = ifelse(a_heim, id$na, id$nb),
                Resultat = paste0(saetze_heim, ":", saetze_gast), `Sätze` = saetze_txt,
                `Bälle` = paste0(baelle_heim, ":", baelle_gast),
                `Elo-Chance A` = p_a,
                `Favorit gewann` = ifelse(is.na(p_a), "", ifelse((p_a > 0.5) == sieg_a, "ja", "nein")))
    datatable(t, rownames = FALSE, options = list(dom = "tip", pageLength = 15),
              caption = tags$caption(style = "caption-side: bottom; font-size: 0.85em;",
                sprintf("Resultat, Sätze und Bälle aus Sicht des Heimteams. Elo-Chance A: Siegchance von %s vor dem Spiel.", id$na))) |>
      formatDate("Datum", method = "toLocaleDateString", params = list("de-CH")) |>
      formatPercentage("Elo-Chance A", 0)
  })

  # Gemeinsame Gegner: gleiche Saison, gleicher Gegner (ohne die Duelle A gegen B)
  mu_gg <- reactive({
    id <- mu_ids()
    s <- spiele() |> filter(saison %in% mu_saisons(), !gegner_id %in% c(id$a, id$b))
    ag <- s |> filter(team_id == id$a) |> group_by(saison, gegner_id, gegner) |>
      summarise(sp_a = n(), pps_a = mean(punkte), ball_a = sum(baelle_fuer, na.rm = TRUE) /
                  sum(baelle_fuer + baelle_gegen, na.rm = TRUE), sieg_a = sum(sieg), .groups = "drop")
    bg <- s |> filter(team_id == id$b) |> group_by(saison, gegner_id) |>
      summarise(sp_b = n(), pps_b = mean(punkte), ball_b = sum(baelle_fuer, na.rm = TRUE) /
                  sum(baelle_fuer + baelle_gegen, na.rm = TRUE), sieg_b = sum(sieg), .groups = "drop")
    inner_join(ag, bg, by = c("saison", "gegner_id")) |> arrange(desc(saison), gegner)
  })

  output$mu_gg_text <- renderUI({
    id <- mu_ids(); g <- mu_gg()
    validate(need(nrow(g) > 0, "Keine gemeinsamen Gegner in den gewählten Saisons."))
    diff_p <- mean(g$pps_a - g$pps_b); diff_b <- mean(g$ball_a - g$ball_b)
    besser <- if (diff_b >= 0) id$na else id$nb
    div(class = "alert alert-light py-2",
        sprintf("%d gemeinsame Gegner (je Saison gezählt). Gegen diese holte %s im Schnitt %.2f Punkte pro Spiel und %.1f%% der Bälle, %s %.2f Punkte und %.1f%%.",
                nrow(g), id$na, mean(g$pps_a), 100 * mean(g$ball_a), id$nb, mean(g$pps_b), 100 * mean(g$ball_b)),
        tags$br(),
        tags$strong(sprintf("Differenz: %+.2f Punkte pro Spiel, %+.1f Prozentpunkte Ballanteil zugunsten von %s.",
                            diff_p, 100 * abs(diff_b), besser)),
        " Jeder Gegner zählt gleich viel.")
  })

  output$mu_gg_tab <- renderDT({
    id <- mu_ids(); g <- mu_gg()
    req(nrow(g) > 0)
    t <- g |> transmute(Saison = saison_label(saison), Gegner = gegner,
                        `Siege A` = paste0(sieg_a, "/", sp_a), `Pu./Sp. A` = round(pps_a, 2), `Bälle A` = ball_a,
                        `Siege B` = paste0(sieg_b, "/", sp_b), `Pu./Sp. B` = round(pps_b, 2), `Bälle B` = ball_b,
                        `Diff. Pu./Sp.` = round(pps_a - pps_b, 2))
    datatable(t, rownames = FALSE, options = list(dom = "tip", pageLength = 15),
              caption = tags$caption(style = "caption-side: bottom; font-size: 0.85em;",
                                     sprintf("A = %s, B = %s. Bälle = Ballanteil gegen diesen Gegner.", id$na, id$nb))) |>
      formatPercentage(c("Bälle A", "Bälle B"), 1) |>
      formatStyle("Diff. Pu./Sp.", color = styleInterval(c(-0.001, 0.001), c("#C0392B", "grey", "#1B7F5A")))
  })

  output$mu_elo <- renderPlot({
    thema_anwenden()
    id <- mu_ids()
    v <- elo()$verlauf |> filter(team_id %in% c(id$a, id$b), saison %in% mu_saisons()) |>
      mutate(team = ifelse(team_id == id$a, id$na, id$nb))
    validate(need(nrow(v) > 0, "Keine Spiele in den gewählten Saisons."))
    duelle <- v |> filter(team_id == id$a, gegner_id == id$b)
    ggplot(v, aes(datum, r_nach, colour = team, group = interaction(team, saison))) +
      geom_hline(yintercept = 1500, colour = "grey85") +
      geom_vline(data = duelle, aes(xintercept = datum), colour = "grey80", linetype = "dotted") +
      geom_step(linewidth = 1) +
      scale_colour_manual(values = setNames(c(accent, "grey30"), c(id$na, id$nb)), name = NULL) +
      labs(title = "Elo-Verlauf", subtitle = "Gepunktete Linien: Direktduelle", x = NULL, y = "Elo")
  })

  # === Spiel-Explorer =====================================================
  # games.csv in Teamsicht (zwei Zeilen pro Spiel), Funktionen in R/prep_games.R
  spiele <- reactiveFileReader(60 * 1000, session, games_file,
                               function(f) team_sicht(read_games(f)))

  # Wertebereiche der Schieberegler; Filter greifen nur, wenn der Regler verschoben ist
  # (sonst wuerden Spiele ohne Ballpunkte, BQ = NA, wegfallen)
  ex_rng <- reactiveVal(NULL)

  # Auswahllisten fuellen bzw. alle Filter zuruecksetzen
  observeEvent(list(spiele(), input$ex_reset), {
    s <- spiele()
    teams <- s |> distinct(team_id, team) |> arrange(team)
    team_ch <- setNames(teams$team_id, teams$team)
    saisons <- sort(unique(s$saison), decreasing = TRUE)
    leer <- character(0)
    updateSelectizeInput(session, "ex_saison", selected = leer,
                         choices = setNames(saisons, saison_label(saisons)))
    updateSelectizeInput(session, "ex_gruppe", selected = leer,
                         choices = sort(unique(na.omit(s$gruppe))))
    updateSelectizeInput(session, "ex_phase", selected = leer,
                         choices = sort(unique(na.omit(s$phase))))
    updateSelectizeInput(session, "ex_team", choices = team_ch, selected = leer)
    updateSelectizeInput(session, "ex_gegner", choices = team_ch, selected = leer)
    updateCheckboxGroupInput(session, "ex_ort", selected = c("Heim", "Auswärts"))
    updateCheckboxGroupInput(session, "ex_resultat", selected = resultat_stufen)
    updateRadioButtons(session, "ex_satz1", selected = "alle")
    d_rng <- range(s$datum, na.rm = TRUE)
    updateDateRangeInput(session, "ex_datum", start = d_rng[1], end = d_rng[2],
                         min = d_rng[1], max = d_rng[2])
    bq  <- range(s$bq, na.rm = TRUE)
    bq  <- c(floor(bq[1] * 20) / 20, ceiling(bq[2] * 20) / 20)
    bpd <- range(s$ballpunktdiff, na.rm = TRUE)
    updateSliderInput(session, "ex_bq", min = bq[1], max = bq[2], value = bq)
    updateSliderInput(session, "ex_bpdiff", min = bpd[1], max = bpd[2], value = bpd)
    ex_rng(list(bq = bq, bpd = bpd))
  })

  ex_gefiltert <- reactive({
    s <- spiele()
    rng <- ex_rng()
    req(rng, input$ex_datum)
    if (length(input$ex_saison)) s <- filter(s, saison %in% as.integer(input$ex_saison))
    if (length(input$ex_gruppe)) s <- filter(s, gruppe %in% input$ex_gruppe)
    if (length(input$ex_phase))  s <- filter(s, phase %in% input$ex_phase)
    if (length(input$ex_team))   s <- filter(s, team_id %in% as.integer(input$ex_team))
    if (length(input$ex_gegner)) s <- filter(s, gegner_id %in% as.integer(input$ex_gegner))
    s <- filter(s, heim_ausw %in% input$ex_ort, resultat %in% input$ex_resultat)
    if (input$ex_satz1 == "gewonnen") s <- filter(s, satz1_gewonnen %in% TRUE)
    if (input$ex_satz1 == "verloren") s <- filter(s, satz1_gewonnen %in% FALSE)
    s <- filter(s, is.na(datum) | (datum >= input$ex_datum[1] & datum <= input$ex_datum[2]))
    if (!isTRUE(all.equal(input$ex_bq, rng$bq)))
      s <- filter(s, !is.na(bq), bq >= input$ex_bq[1], bq <= input$ex_bq[2])
    if (!isTRUE(all.equal(input$ex_bpdiff, rng$bpd)))
      s <- filter(s, !is.na(ballpunktdiff), ballpunktdiff >= input$ex_bpdiff[1],
                  ballpunktdiff <= input$ex_bpdiff[2])
    s
  })

  satz_txt <- function(a, b) ifelse(is.na(a), "", paste0(a, ":", b))

  # Angezeigte Tabelle je nach Ansicht
  ex_anzeige <- reactive({
    s <- ex_gefiltert()
    if (input$ex_ansicht == "spiele") {
      s |> transmute(
        Datum = datum, Saison = saison_lbl, Gruppe = gruppe, Phase = phase,
        Team = team, `H/A` = ifelse(heim_ausw == "Heim", "H", "A"), Gegner = gegner,
        Resultat = as.character(resultat), Pu. = punkte,
        `Bälle` = satz_txt(baelle_fuer, baelle_gegen), BQ = bq, `Ball-Diff` = ballpunktdiff,
        `Satz 1` = satz_txt(s1_fuer, s1_gegen), `Satz 2` = satz_txt(s2_fuer, s2_gegen),
        `Satz 3` = satz_txt(s3_fuer, s3_gegen), `Satz 4` = satz_txt(s4_fuer, s4_gegen),
        `Satz 5` = satz_txt(s5_fuer, s5_gegen),
        Ort = spielort)
    } else {
      pro_saison <- input$ex_ansicht == "saison"
      t <- team_tabelle(s, pro_saison = pro_saison) |>
        transmute(
          Saison = if (pro_saison) saison_lbl else NULL,
          Team = team, Gruppen = gruppen,
          Saisons = if (pro_saison) NULL else saisons,
          Sp. = spiele, Pu. = punkte, `Pu./Sp.` = punkte_pro_spiel,
          S = siege, N = niederlagen, `Sieg-%` = siegquote,
          `3:0` = r30, `3:1` = r31, `3:2` = r32, `2:3` = r23, `1:3` = r13, `0:3` = r03,
          `Sätze` = paste0(saetze_fuer, ":", saetze_gegen), SQ = sq,
          `Bälle` = paste0(baelle_fuer, ":", baelle_gegen), BQ = bq,
          `Heimsp.` = heimspiele)
      t
    }
  })

  output$ex_info <- renderText({
    s <- ex_gefiltert()
    sprintf("%d Spiele gefiltert (%d Teamzeilen), Tabelle: %d Zeilen",
            n_distinct(s$game_id), nrow(s), nrow(ex_anzeige()))
  })

  output$ex_tabelle <- renderDT({
    d <- ex_anzeige()
    spiele_ansicht <- input$ex_ansicht == "spiele"
    quot <- which(names(d) %in% c("SQ", "BQ")) - 1          # 0-basierte Spaltenindizes
    col_defs <- if (spiele_ansicht) list() else list(list(targets = quot, render = js_quotient))
    dt <- datatable(
      d, rownames = FALSE, filter = "top", extensions = "Buttons",
      options = list(
        dom = "Bfrtip", pageLength = 25, scrollX = TRUE,
        buttons = list(list(extend = "colvis", text = "Spalten ein-/ausblenden")),
        columnDefs = col_defs,
        language = list(url = "https://cdn.datatables.net/plug-ins/1.13.6/i18n/de-DE.json")
      ))
    if (spiele_ansicht) {
      dt |>
        formatDate("Datum", method = "toLocaleDateString", params = list("de-CH")) |>
        formatRound("BQ", 2) |>
        formatStyle("Resultat", fontWeight = "bold",
                    backgroundColor = styleEqual(names(res_bg), unname(res_bg)),
                    color = styleEqual(names(res_bg), res_fg))
    } else {
      dt |>
        formatRound("Pu./Sp.", 2) |>
        formatPercentage("Sieg-%", 0)
    }
  })

  output$ex_download <- downloadHandler(
    filename = function() sprintf("volley_%s_%s.csv", input$ex_ansicht, Sys.Date()),
    content = function(file) {
      d <- ex_anzeige()
      rows <- input$ex_tabelle_rows_all            # inkl. Spaltenfilter und Suche der Tabelle
      if (!is.null(rows)) d <- d[rows, , drop = FALSE]
      readr::write_excel_csv(d, file, na = "")
    }
  )

  # === Elo-Rating =========================================================
  # Kern in R/elo.R; eingefrorene Parameter elo_par, Ball-Variante
  games_roh <- reactiveFileReader(60 * 1000, session, games_file, read_games)
  elo <- reactive(elo_app(games_roh()))

  # Gruppe pro Teamsaison (aus der Qualifikation)
  elo_gruppen <- reactive({
    games_roh() |>
      filter(phase == "Qualifikation") |>
      select(saison, gruppe, heim_id, gast_id) |>
      pivot_longer(c(heim_id, gast_id), values_to = "team_id") |>
      distinct(saison, team_id, .keep_all = TRUE) |>
      select(saison, team_id, gruppe)
  })

  # Aktuelle Ratings; vor dem ersten Spiel einer neuen Saison Carryover anwenden
  elo_aktuell <- reactive({
    r <- elo()$ratings
    s_neu <- max(games_roh()$saison)
    if (s_neu > elo()$letzte_saison) r$rating <- 1500 + elo_par["c"] * (r$rating - 1500)
    r
  })
  elo_rating_von <- function(ids) {
    r <- elo_aktuell()
    x <- r$rating[match(ids, r$team_id)]
    ifelse(is.na(x), 1500 + elo_par["neu"], x)                   # unbekannt: Aufsteiger-Start
  }

  observe({
    s <- sort(unique(elo()$verlauf$saison), decreasing = TRUE)
    updateSelectInput(session, "elo_saison", choices = setNames(s, saison_label(s)),
                      selected = isolate(input$elo_saison) %||% s[1])
    namen <- aktuelle_namen(games_roh())
    team_ch <- setNames(namen$id, namen$name)[order(namen$name)]
    updateSelectizeInput(session, "elo_heim", choices = team_ch,
                         selected = isolate(input$elo_heim) %||% team_ch[1])
    updateSelectizeInput(session, "elo_gast", choices = team_ch,
                         selected = isolate(input$elo_gast) %||% team_ch[2])
  })

  observeEvent(input$elo_saison, {
    gr <- elo_gruppen() |> filter(saison == as.integer(input$elo_saison)) |> pull(gruppe) |>
      unique() |> sort()
    sel <- if (isTRUE(input$elo_gruppe %in% gr)) input$elo_gruppe else
      if ("Gruppe B" %in% gr) "Gruppe B" else gr[1]
    updateSelectInput(session, "elo_gruppe", choices = gr, selected = sel)
  })

  # Ratings der Teams einer Gruppe in der gewaehlten Saison
  elo_gruppe_tab <- reactive({
    req(input$elo_saison, input$elo_gruppe)
    sz <- as.integer(input$elo_saison)
    ids <- elo_gruppen() |> filter(saison == sz, gruppe == input$elo_gruppe) |> pull(team_id)
    elo()$verlauf |>
      filter(saison == sz, team_id %in% ids) |>
      group_by(team_id, team) |>
      summarise(start = first(r_vor), ende = last(r_nach), hoch = max(r_nach), tief = min(r_nach),
                spiele = n(), letzte5 = last(r_nach) - nth(r_vor, max(1, n() - 4)),
                .groups = "drop") |>
      arrange(desc(ende))
  })

  observeEvent(elo_gruppe_tab(), {
    t <- elo_gruppe_tab()
    ch <- setNames(t$team_id, t$team)
    sel <- if (isTRUE(input$elo_team %in% ch)) input$elo_team else ch[1]
    updateSelectInput(session, "elo_team", choices = ch, selected = sel)
  })

  output$elo_tabelle <- renderDT({
    t <- elo_gruppe_tab()
    aktuell <- as.integer(input$elo_saison) == max(games_roh()$saison)
    # Tatsaechliche Tabelle der Qualifikation dieser Gruppe (Stand: alle gespielten Spiele)
    # Reihenfolge: Punkte, Siege, Satzquotient, Ballquotient
    tab <- spiele() |>
      filter(saison == as.integer(input$elo_saison), phase == "Qualifikation",
             gruppe == input$elo_gruppe) |>
      team_tabelle() |>
      arrange(desc(punkte), desc(siege), desc(coalesce(sq, Inf)), desc(coalesce(bq, Inf))) |>
      mutate(tab_rang = row_number())
    t <- t |> mutate(elo_rang = row_number()) |>
      left_join(select(tab, team_id, tab_rang, tab_punkte = punkte), by = "team_id")
    d <- t |> transmute(`#` = elo_rang, Team = team,
                        Elo = round(ende),
                        `Tabelle` = tab_rang, Pu. = tab_punkte,
                        `Elo vs. Tabelle` = tab_rang - elo_rang,
                        `Δ Saison` = round(ende - start),
                        `Δ letzte 5 Sp.` = round(letzte5),
                        `Saisonstart` = round(start), `Höchst` = round(hoch), `Tiefst` = round(tief),
                        Sp. = spiele, team_id = team_id)
    names(d)[3] <- if (aktuell) "Elo aktuell" else "Elo Saisonende"
    hl_id <- suppressWarnings(as.integer(input$elo_team))
    if (length(hl_id) == 0 || is.na(hl_id)) hl_id <- -1L
    datatable(d, rownames = FALSE,
              caption = tags$caption(style = "caption-side: bottom; font-size: 0.85em;",
                "#: Rang nach Elo. Tabelle: tatsächlicher Platz in der Qualifikation (Punkte, Siege,",
                "Satzquotient, Ballquotient). Elo vs. Tabelle: positiv (grün) = Elo sieht das Team",
                "besser als seinen Tabellenplatz, negativ (rot) = schlechter."),
              options = list(dom = "t", pageLength = 30,
                             columnDefs = list(list(visible = FALSE, targets = ncol(d) - 1)))) |>
      formatStyle(c("Δ Saison", "Δ letzte 5 Sp.", "Elo vs. Tabelle"),
                  color = styleInterval(c(-0.5, 0.5), c("#C0392B", "grey", "#1B7F5A"))) |>
      formatStyle("Tabelle", fontWeight = "bold") |>
      formatStyle("team_id", target = "row",
                  fontWeight = styleEqual(hl_id, "bold"),
                  backgroundColor = styleEqual(hl_id, "#FBE7EA"))
  })

  output$elo_verlauf <- renderPlot({
    thema_anwenden()
    t <- elo_gruppe_tab()
    req(nrow(t) > 0)
    sz <- as.integer(input$elo_saison)
    v <- elo()$verlauf |> filter(team_id %in% t$team_id, saison == sz)
    # Startpunkt jeder Saison (Rating vor dem ersten Spiel) plus Rating nach jedem Spiel
    pkt <- bind_rows(
      v |> group_by(team_id, team, saison) |> slice(1) |> ungroup() |>
        transmute(team_id, team, saison, datum = datum - 1, r = r_vor),
      v |> transmute(team_id, team, saison, datum, r = r_nach)) |>
      arrange(datum) |>
      mutate(hl = team_id == as.integer(input$elo_team))
    last <- pkt |> group_by(team_id) |> slice_max(datum, n = 1, with_ties = FALSE) |> ungroup()
    ggplot(pkt, aes(datum, r, group = interaction(team_id, saison), colour = team, linetype = team)) +
      geom_hline(yintercept = 1500, colour = "grey85") +
      geom_step(aes(linewidth = hl, alpha = hl)) +
      geom_point(data = last, size = 3) +
      geom_text_repel(data = last, aes(label = team), colour = "grey15",
                      fontface = ifelse(last$hl, "bold", "plain"), direction = "y",
                      hjust = 0, nudge_x = 5, size = if (mobil()) 3 else 3.6, segment.colour = "grey80",
                      seed = 1) +
      team_skalen(pkt$team, input$elo_team) +
      scale_x_date(expand = expansion(mult = c(0.02, 0.25))) +
      labs(title = sprintf("Elo-Verlauf %s", input$elo_gruppe),
           subtitle = paste(saison_label(sz), "(alle Saisons: Tab \"Alle Saisons\")"),
           x = NULL, y = "Elo")
  })

  # --- Langzeitverlauf ueber alle Saisons ------------------------------------
  observe({
    namen <- aktuelle_namen(games_roh())
    ch <- setNames(namen$id, namen$name)[order(namen$name)]
    updateSelectizeInput(session, "elo_lang_teams", choices = ch,
                         selected = isolate(input$elo_lang_teams))
  })
  # Vorauswahl: Teams der gewaehlten Gruppe und Saison
  observeEvent(elo_gruppe_tab(), {
    updateSelectizeInput(session, "elo_lang_teams", selected = as.character(elo_gruppe_tab()$team_id))
  })

  output$elo_lang <- renderPlot({
    thema_anwenden()
    ids <- suppressWarnings(as.integer(input$elo_lang_teams))
    validate(need(length(ids) > 0, "Bitte mindestens ein Team wählen."))
    v <- elo()$verlauf |> filter(team_id %in% ids)
    validate(need(nrow(v) > 0, "Für diese Teams gibt es keine Spiele."))
    pkt <- bind_rows(
      v |> group_by(team_id, team, saison) |> slice(1) |> ungroup() |>
        transmute(team_id, team, saison, datum = datum - 1, r = r_vor),
      v |> transmute(team_id, team, saison, datum, r = r_nach)) |>
      arrange(datum) |>
      mutate(hl = coalesce(team_id == suppressWarnings(as.integer(input$elo_team)), FALSE),
             saison_lbl = saison_label(saison))
    leg <- guide_legend(nrow = if (mobil()) 6 else 3, byrow = TRUE,
                        override.aes = list(linewidth = 1.2))
    letzte <- pkt |> group_by(team_id) |> slice_max(datum, n = 1, with_ties = FALSE) |> ungroup()
    ggplot(pkt, aes(datum, r, group = interaction(team_id, saison), colour = team, linetype = team)) +
      geom_hline(yintercept = 1500, colour = "grey85") +
      geom_step(aes(linewidth = hl, alpha = hl)) +
      geom_point(data = letzte, size = 2.5) +
      # Saisons nebeneinander ohne Sommerpause; der Sprung zwischen den Feldern ist die
      # Rueckfuehrung Richtung 1500 beim Saisonwechsel
      facet_grid(~ saison_lbl, scales = "free_x", space = "free_x") +
      team_skalen(pkt$team, input$elo_team, legende = TRUE) +
      scale_x_date(date_breaks = "2 months", date_labels = "%m") +
      guides(colour = leg, linetype = leg) +   # gleiche Einstellungen: eine gemeinsame Legende
      theme(legend.position = "bottom", panel.spacing.x = unit(0.4, "lines"),
            strip.text = element_text(face = "bold"),
            panel.border = element_rect(colour = "grey90", fill = NA)) +
      labs(title = "Elo-Verlauf über alle Saisons",
           subtitle = "Ein Feld pro Saison (Monate unten); Punkt = letztes Spiel des Teams",
           x = NULL, y = "Elo")
  })

  output$elo_matchup <- renderUI({
    req(input$elo_heim, input$elo_gast)
    rh <- elo_rating_von(as.integer(input$elo_heim)); rg <- elo_rating_von(as.integer(input$elo_gast))
    p <- elo_p_heim(rh, rg, neutral = isTRUE(input$elo_neutral))
    tags$div(class = "mt-1 mb-2",
             tags$div(sprintf("Elo %d vs. %d", round(rh), round(rg))),
             tags$div(style = sprintf("font-size:1.4em; font-weight:600; color:%s", accent),
                      sprintf("P(Heimsieg) = %.0f%%", 100 * p)))
  })

  # --- Prognoseguete ----------------------------------------------------------
  # Spiele der gewaehlten Saison mit Elo-Prognose und Vergleichsprognosen
  elo_guete_daten <- reactive({
    req(input$elo_saison)
    sz <- as.integer(input$elo_saison)
    sp <- elo()$spiele |> filter(saison == sz)
    if (isTRUE(input$elo_guete_gruppe)) {
      ids <- elo_gruppen() |> filter(saison == sz, gruppe == input$elo_gruppe) |> pull(team_id)
      sp <- filter(sp, heim_id %in% ids | gast_id %in% ids)
    }
    # Tabelle vor dem Spiel: Punkte pro Spiel in dieser Saison bis zum Vortag
    ts <- spiele() |> filter(saison == sz) |> select(team_id, datum, punkte)
    pps_vor <- function(id, dt) {
      x <- ts$punkte[ts$team_id == id & ts$datum < dt]
      if (length(x) == 0) NA_real_ else mean(x)
    }
    sp |> mutate(
      pps_h = mapply(pps_vor, heim_id, datum), pps_g = mapply(pps_vor, gast_id, datum),
      tipp_elo = as.integer(p_heim > 0.5),
      tipp_tab = ifelse(is.na(pps_h) | is.na(pps_g) | pps_h == pps_g, 1L, as.integer(pps_h > pps_g)),
      p_fav = pmax(p_heim, 1 - p_heim),
      elo_richtig = tipp_elo == sieg, tab_richtig = tipp_tab == sieg, heim_richtig = sieg == 1)
  })

  output$elo_guete_hinweis <- renderUI({
    sz <- as.integer(input$elo_saison)
    if (sz >= 2026) {
      div(class = "alert alert-success py-2",
          tags$strong("Echter Test: "),
          "Die Parameter wurden vor dieser Saison festgelegt (aus 2023/24 bis 2025/26) und seither",
          "nicht mehr verändert. Diese Zahlen zeigen die tatsächliche Prognosegüte.")
    } else if (sz >= 2023) {
      div(class = "alert alert-warning py-2",
          tags$strong("Schätzzeitraum: "),
          "Die Parameter wurden mit den Spielen dieser Saison bestimmt, die Werte sind darum etwas",
          "zu optimistisch. Unabhängige Prüfung (Parameter nur aus 2023/24 und 2024/25): 77% Treffer",
          "in der Saison 2025/26.")
    } else {
      div(class = "alert alert-secondary py-2",
          tags$strong("Einlaufsaison: "),
          "In der ersten Saison der Daten starten alle Teams bei 1500. Die Prognosen werden erst im",
          "Verlauf der Saison brauchbar.")
    }
  })

  output$elo_guete_boxen <- renderUI({
    d <- elo_guete_daten()
    validate(need(nrow(d) > 0, "Noch keine gespielten Spiele."))
    ll <- -mean(d$sieg * log(d$p_heim) + (1 - d$sieg) * log(1 - d$p_heim))
    box <- function(titel, wert, text, theme) value_box(title = titel, value = wert, p(text), theme = theme)
    layout_columns(
      col_widths = c(3, 3, 3, 3), fill = FALSE,
      box("Treffer Elo", sprintf("%.0f%%", 100 * mean(d$elo_richtig)),
          sprintf("%d von %d Spielen", sum(d$elo_richtig), nrow(d)), "primary"),
      box("Treffer Tabelle", sprintf("%.0f%%", 100 * mean(d$tab_richtig)),
          "Team mit mehr Punkten pro Spiel gewinnt", "light"),
      box("Treffer Heimteam", sprintf("%.0f%%", 100 * mean(d$heim_richtig)),
          "Heimteam gewinnt immer", "light"),
      box("Erwartet laut Elo", sprintf("%.0f%%", 100 * mean(d$p_fav)),
          sprintf("mittlere Favoritenchance; Log-Loss %.3f", ll), "light"))
  })

  output$elo_guete_kalib <- renderPlot({
    thema_anwenden()
    d <- elo_guete_daten()
    req(nrow(d) > 0)
    k <- d |>
      mutate(band = cut(p_fav, c(0.5, 0.6, 0.7, 0.8, 0.9, 1), include.lowest = TRUE)) |>
      group_by(band) |>
      summarise(prognose = mean(p_fav), treffer = mean(elo_richtig), n = n(), .groups = "drop")
    ggplot(k, aes(prognose, treffer)) +
      geom_abline(colour = "grey70", linetype = "dashed") +
      geom_line(colour = "grey60") +
      geom_point(aes(size = n), colour = accent) +
      geom_text(aes(label = paste0("n=", n)), vjust = -1.3, size = 3.3, colour = "grey30") +
      scale_size_area(max_size = 7, guide = "none") +
      scale_x_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0.5, 1)) +
      scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0.3, 1.05)) +
      labs(title = "Stimmen die Wahrscheinlichkeiten?",
           subtitle = "Punkte auf der Diagonale = gut kalibriert",
           x = "Elo-Siegchance des Favoriten", y = "Favorit hat tatsächlich gewonnen")
  })

  output$elo_guete_verlauf <- renderPlot({
    thema_anwenden()
    d <- elo_guete_daten() |> arrange(datum, game_id)
    req(nrow(d) > 0)
    kum <- d |>
      mutate(n = row_number(),
             Elo = cumsum(elo_richtig) / n, Tabelle = cumsum(tab_richtig) / n,
             Heimteam = cumsum(heim_richtig) / n) |>
      select(datum, n, Elo, Tabelle, Heimteam) |>
      tidyr::pivot_longer(c(Elo, Tabelle, Heimteam), names_to = "Prognose", values_to = "quote") |>
      filter(n >= 10)
    validate(need(nrow(kum) > 0, "Zu wenige Spiele für den Verlauf."))
    ggplot(kum, aes(datum, quote, colour = Prognose)) +
      geom_line(linewidth = 1) +
      scale_colour_manual(values = c(Elo = accent, Tabelle = "grey35", Heimteam = "grey70"), name = NULL) +
      scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
      labs(title = "Trefferquote im Saisonverlauf (kumuliert)",
           subtitle = "ab dem 10. Spiel", x = NULL, y = "Treffer bisher")
  })

  output$elo_guete_tab_titel <- renderUI(tagList(
    tags$h6(class = "mt-3", "Treffer nach Sicherheit der Prognose"),
    p(class = "text-muted small mb-1",
      "Der Favorit ist das Team, dem Elo vor dem Spiel die grössere Siegchance gab. Die Spiele sind",
      "danach gruppiert, wie gross diese Chance war. Stimmen die Wahrscheinlichkeiten, gewinnt der",
      "Favorit in jeder Zeile etwa so oft, wie Elo erwartet hat. Bei wenigen Spielen pro Zeile",
      "schwankt das stark.")))

  output$elo_guete_baender <- renderTable({
    d <- elo_guete_daten()
    req(nrow(d) > 0)
    d |>
      mutate(band = cut(p_fav, c(0.5, 0.6, 0.7, 0.8, 0.9, 1), include.lowest = TRUE,
                        labels = c("50 bis 60%", "60 bis 70%", "70 bis 80%", "80 bis 90%", "über 90%"))) |>
      group_by(`Siegchance des Favoriten laut Elo` = band) |>
      summarise(`Anzahl Spiele` = n(),
                `Ø Siegchance laut Elo` = sprintf("%.0f%%", 100 * mean(p_fav)),
                `Favorit hat tatsächlich gewonnen` =
                  sprintf("%.0f%% (%d von %d)", 100 * mean(elo_richtig), sum(elo_richtig), n()),
                .groups = "drop")
  }, align = "lccc")

  output$elo_guete_upsets <- renderDT({
    d <- elo_guete_daten() |> filter(p_fav >= 0.75, !elo_richtig) |> arrange(desc(p_fav))
    validate(need(nrow(d) > 0, "Keine Überraschungen dieser Grösse."))
    t <- d |> transmute(Datum = datum, Gruppe = gruppe, Heim = heim_team, Gast = gast_team,
                        Resultat = resultat, `Bälle` = baelle,
                        Favorit = ifelse(p_heim > 0.5, heim_team, gast_team), `Siegchance Favorit` = p_fav)
    datatable(t, rownames = FALSE, options = list(pageLength = 10, dom = "tip")) |>
      formatDate("Datum", method = "toLocaleDateString", params = list("de-CH")) |>
      formatPercentage("Siegchance Favorit", 0)
  })

  # --- Erklaerung: Rechenschritte ---------------------------------------------
  # Alle Schritte fuer ein Spiel aus Sicht des Heimteams
  elo_schritt <- function(rh, rg, bh, bg, neutral = FALSE) {
    H <- if (neutral) 0 else elo_par[["H"]]
    E <- 1 / (1 + 10^(-(rh + H - rg) / 400))
    b <- bh / (bh + bg)
    S <- plogis(elo_a * qlogis(min(max(b, 0.01), 0.99)))
    list(H = H, E = E, b = b, S = S, delta = elo_par[["K"]] * (S - E),
         p = elo_prob(E, elo_par[["g"]]))
  }
  f0 <- function(x) formatC(x, format = "f", digits = 0)
  f2 <- function(x) formatC(x, format = "f", digits = 2)
  f3 <- function(x) formatC(x, format = "f", digits = 3)
  vz <- function(x, d = 1) sprintf(paste0("%+.", d, "f"), x)

  output$elo_erkl_s_tab <- renderTable({
    b <- c(0.40, 0.45, 0.48, 0.50, 0.52, 0.55, 0.60)
    data.frame(`Ballanteil b` = sprintf("%.0f%%", 100 * b),
               `Beispiel (von 180 Bällen)` = sprintf("%d:%d", round(180 * b), 180 - round(180 * b)),
               `Ergebnis S` = f2(plogis(elo_a * qlogis(b))), check.names = FALSE)
  }, align = "c")

  elo_erkl_teams <- reactive({
    req(input$elo_heim, input$elo_gast)
    namen <- aktuelle_namen(games_roh())
    ih <- as.integer(input$elo_heim); ig <- as.integer(input$elo_gast)
    list(rh = elo_rating_von(ih), rg = elo_rating_von(ig),
         nh = namen$name[match(ih, namen$id)], ng = namen$name[match(ig, namen$id)])
  })

  output$elo_erkl_rechnung <- renderUI({
    t <- elo_erkl_teams()
    req(input$elo_erkl_bh, input$elo_erkl_bg, input$elo_erkl_bh + input$elo_erkl_bg > 0)
    x <- elo_schritt(t$rh, t$rg, input$elo_erkl_bh, input$elo_erkl_bg, isTRUE(input$elo_neutral))
    tags$table(class = "table table-sm mb-0",
      tags$tr(tags$td("Ratings"), tags$td(sprintf("%s %s vs. %s %s", t$nh, f0(t$rh), t$ng, f0(t$rg)))),
      tags$tr(tags$td("1. Erwartung E"),
              tags$td(sprintf("1 / (1 + 10^(-(%s + %s - %s) / 400)) = %s",
                              f0(t$rh), formatC(x$H, format = "f", digits = 1), f0(t$rg), f3(x$E)))),
      tags$tr(tags$td("2. Ergebnis S"),
              tags$td(sprintf("Ballanteil %s:%s = %.1f%%, S = %s", input$elo_erkl_bh, input$elo_erkl_bg,
                              100 * x$b, f3(x$S)))),
      tags$tr(tags$td("3. Änderung"),
              tags$td(tags$strong(sprintf("%s · (%s - %s) = %s", formatC(elo_par[["K"]], format = "f", digits = 1),
                                          f3(x$S), f3(x$E), vz(x$delta))),
                      sprintf(" → %s %s, %s %s", t$nh, f0(t$rh + x$delta), t$ng, f0(t$rg - x$delta)))),
      tags$tr(tags$td("Vor dem Spiel"), tags$td(sprintf("P(Heimsieg) = %.0f%%", 100 * x$p))))
  })

  output$elo_erkl_plot <- renderPlot({
    thema_anwenden()
    t <- elo_erkl_teams()
    neutral <- isTRUE(input$elo_neutral)
    kurve <- tibble(b = seq(0.38, 0.62, by = 0.002)) |>
      mutate(delta = vapply(b, function(bb) elo_schritt(t$rh, t$rg, bb, 1 - bb, neutral)$delta, numeric(1)))
    E <- elo_schritt(t$rh, t$rg, 1, 1, neutral)$E
    b0 <- sqrt(E) / (sqrt(E) + sqrt(1 - E))     # Ballanteil, bei dem S = E (keine Aenderung)
    bh <- input$elo_erkl_bh; bg <- input$elo_erkl_bg
    punkt <- if (isTRUE(bh + bg > 0)) tibble(b = bh / (bh + bg),
                                            delta = elo_schritt(t$rh, t$rg, bh, bg, neutral)$delta)
    ggplot(kurve, aes(b, delta)) +
      geom_hline(yintercept = 0, colour = "grey70") +
      geom_vline(xintercept = 0.5, colour = "grey85", linetype = "dashed") +
      geom_vline(xintercept = b0, colour = accent, linetype = "dotted") +
      geom_line(linewidth = 1) +
      { if (!is.null(punkt)) geom_point(data = punkt, colour = accent, size = 4) } +
      annotate("text", x = b0, y = max(kurve$delta), hjust = -0.05, vjust = 1, size = 3.6, colour = accent,
               label = sprintf("keine Änderung bei %.1f%% der Bälle", 100 * b0)) +
      scale_x_continuous(labels = function(x) paste0(round(100 * x), "%")) +
      labs(title = sprintf("Elo-Änderung für %s je nach Ballanteil", t$nh),
           subtitle = sprintf("gegen %s; %s gibt genau gleich viele Punkte ab", t$ng, t$ng),
           x = sprintf("Ballanteil %s", t$nh), y = "Änderung Elo Heimteam")
  })

  output$elo_erkl_beispiel <- renderUI({
    req(input$elo_team)
    id <- as.integer(input$elo_team)
    v <- elo()$verlauf |> filter(team_id == id)
    req(nrow(v) > 0)
    z <- v |> slice_max(datum, n = 1, with_ties = FALSE)
    g <- games_roh() |> filter(game_id == z$game_id)
    heim <- g$heim_id == id
    # Rechnung aus Sicht des Heimteams, Darstellung fuer das gewaehlte Team
    rh <- if (heim) z$r_vor else elo()$verlauf$r_vor[elo()$verlauf$game_id == z$game_id & elo()$verlauf$team_id == g$heim_id]
    rg <- if (heim) elo()$verlauf$r_vor[elo()$verlauf$game_id == z$game_id & elo()$verlauf$team_id == g$gast_id] else z$r_vor
    # Neutraler Ort, wenn die gespeicherte Aenderung nur ohne Heimvorteil aufgeht
    neutral <- abs((z$r_nach - z$r_vor) - (if (heim) 1 else -1) *
                   elo_schritt(rh, rg, g$baelle_heim, g$baelle_gast, FALSE)$delta) > 0.01
    x <- elo_schritt(rh, rg, g$baelle_heim, g$baelle_gast, neutral)
    d_team <- if (heim) x$delta else -x$delta
    sets <- paste0(g$saetze_heim, ":", g$saetze_gast)
    tagList(
      p(sprintf("%s, %s: %s (Heim) gegen %s (Gast), %s, Bälle %d:%d.",
                format(g$datum, "%d.%m.%Y"), g$phase, g$heim, g$gast, sets, g$baelle_heim, g$baelle_gast)),
      tags$ol(
        tags$li(sprintf("Ratings vor dem Spiel: Heim %s, Gast %s. Heimvorteil %s. Erwartung Heim E = %s (Siegchance %.0f%%).",
                        f0(rh), f0(rg), if (neutral) "0 (neutraler Ort)" else formatC(x$H, format = "f", digits = 1),
                        f3(x$E), 100 * x$p)),
        tags$li(sprintf("Ballanteil Heim %.1f%%, daraus S = %s.", 100 * x$b, f3(x$S))),
        tags$li(sprintf("Änderung Heim = %s · (%s - %s) = %s.", formatC(elo_par[["K"]], format = "f", digits = 1),
                        f3(x$S), f3(x$E), vz(x$delta)))),
      p(tags$strong(sprintf("Für %s: %s → %s (%s).",
                            aktuelle_namen(games_roh())$name[match(id, aktuelle_namen(games_roh())$id)],
                            f0(z$r_vor), f0(z$r_nach), vz(d_team)))),
      if ((x$S > 0.5) != (g$saetze_heim > g$saetze_gast))
        p(class = "text-muted", "Hinweis: Hier passen Satzresultat und Ballanteil nicht zusammen.",
          "Elo folgt den Bällen, nicht dem Satzresultat.")
    )
  })

  output$elo_prognosen <- renderDT({
    g <- games_roh()
    s_neu <- max(g$saison)
    k <- g |> filter(!gespielt, saison == s_neu)
    if (!isTRUE(input$elo_prog_alle)) k <- filter(k, gruppe == input$elo_gruppe)
    validate(need(nrow(k) > 0, "Keine offenen Spiele."))
    k <- k |>
      arrange(datum, zeit) |>
      mutate(namen_h = aktuelle_namen(g)$name[match(heim_id, aktuelle_namen(g)$id)],
             namen_g = aktuelle_namen(g)$name[match(gast_id, aktuelle_namen(g)$id)],
             rh = elo_rating_von(heim_id), rg = elo_rating_von(gast_id),
             p = elo_p_heim(rh, rg, neutral = phase != "Qualifikation"))
    d <- k |> transmute(Datum = datum, Zeit = zeit, Gruppe = gruppe, Heim = namen_h, Gast = namen_g,
                        `Elo Heim` = round(rh), `Elo Gast` = round(rg), `P(Heimsieg)` = p)
    datatable(d, rownames = FALSE, filter = "top",
              options = list(pageLength = 25, dom = "frtip")) |>
      formatDate("Datum", method = "toLocaleDateString", params = list("de-CH")) |>
      formatPercentage("P(Heimsieg)", 0) |>
      formatStyle("P(Heimsieg)",
                  background = styleColorBar(c(0, 1), "#F2C4CC"),
                  backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })
}

shinyApp(ui, server)
