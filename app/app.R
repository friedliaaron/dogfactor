# Shiny-App: Rangliste 1L Maenner, Gruppe B (Swiss Volley Game Center)
# Liest data/ranking_history.csv, die scrape_ranking.R taeglich ergaenzt.
# Pakete: install.packages(c("shiny", "bslib", "DT", "ggplot2", "dplyr", "tidyr", "ggrepel"))
# Optional fuer Hunde-Symbole (nur lokal, nicht in der GitHub-Pages-Version):
#   remotes::install_github("R-CoderDotCom/ggdogs")

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

theme_set(theme_minimal(base_size = 14) +
            theme(panel.grid.minor = element_blank(),
                  plot.title.position = "plot",
                  legend.position = "top"))

# --- Regressionsplot mit Residuen und ggdogs-Symbolen ---------------------
# y_log = TRUE: Regression auf log10(y), Residuen als % relativ zur Erwartung.
# y_log = FALSE: lineare Regression, Residuen in Einheiten von y.
reg_plot <- function(d, yvar, y_log, title, y_lab, res_fmt) {
  d$y <- d[[yvar]]
  fit <- if (y_log) lm(log10(y) ~ ballquotient, data = d) else lm(y ~ ballquotient, data = d)
  d <- d |>
    mutate(erwartet = if (y_log) 10^fitted(fit) else fitted(fit),
           res      = residuals(fit),
           res_lab  = sprintf(res_fmt, team, if (y_log) 100 * (10^res - 1) else res),
           richtung = ifelse(res > 0, "über Erwartung", "unter Erwartung"))
  r2 <- summary(fit)$r.squared

  # Hunde aus ggdogs als Symbole (Fallback: normale Punkte)
  # (indirekt aufgerufen, damit shinylive ggdogs nicht als Abhaengigkeit sucht)
  dog_pkg <- "ggdogs"
  dog_layers <- if (requireNamespace(dog_pkg, quietly = TRUE)) {
    geom_dog <- getExportedValue(dog_pkg, "geom_dog")
    list(geom_dog(data = filter(d, !hl), dog = "doge", size = 4),
         geom_dog(data = filter(d, hl),  dog = "pug",  size = 5.5))
  } else {
    list(geom_point(aes(fill = hl), shape = 21, size = 4, colour = "white",
                    show.legend = FALSE),
         scale_fill_manual(values = c(`FALSE` = "grey45", `TRUE` = accent)))
  }

  ggplot(d, aes(ballquotient, y)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey80") +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                colour = "grey25", fill = "grey85", linewidth = 0.8) +
    geom_segment(aes(xend = ballquotient, yend = erwartet, colour = richtung),
                 linewidth = 0.9) +
    dog_layers +
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

ui <- page_sidebar(
  title = "Rangliste 1. Liga Männer, Gruppe B",
  theme = bs_theme(version = 5, primary = accent),
  sidebar = sidebar(
    selectInput("team", "Team hervorheben", choices = NULL),
    selectInput("datum", "Stand vom", choices = NULL),
    hr(),
    textOutput("stand", inline = TRUE),
    tags$small(class = "text-muted",
               "Quelle: volleyball.ch Game Center, täglich abgerufen.")
  ),
  navset_card_underline(
    nav_panel("Tabelle", DTOutput("tabelle")),
    nav_panel("Punkte & Resultate", plotOutput("resultate", height = "520px")),
    nav_panel("Rangverlauf", plotOutput("verlauf", height = "520px")),
    nav_panel("Satz- vs. Ballquotient", plotOutput("quotienten", height = "560px")),
    nav_panel("Punkte/Spiel vs. Ballquotient", plotOutput("punkte_bq", height = "560px"))
  )
)

server <- function(input, output, session) {

  # Liest die CSV neu ein, sobald sie sich aendert (Pruefung jede Minute)
  history <- reactiveFileReader(60 * 1000, session, data_file, function(f) {
    read.csv(f, fileEncoding = "UTF-8", stringsAsFactors = FALSE) |>
      mutate(datum = as.Date(datum))
  })

  observe({
    h <- history()
    dates <- sort(unique(h$datum), decreasing = TRUE)
    teams <- h |> filter(datum == max(datum)) |> arrange(rang) |> pull(team)
    updateSelectInput(session, "datum", choices = as.character(dates),
                      selected = isolate(input$datum) %||% as.character(dates[1]))
    updateSelectInput(session, "team", choices = teams,
                      selected = isolate(input$team) %||% teams[1])
  })

  current <- reactive({
    req(input$datum)
    history() |> filter(datum == as.Date(input$datum)) |> arrange(rang)
  })

  output$stand <- renderText({
    paste("Letzter Abruf:", format(max(history()$datum), "%d.%m.%Y"))
  })

  # --- Tabelle -----------------------------------------------------------
  output$tabelle <- renderDT({
    d <- current() |>
      transmute(`#` = rang, Team = team, Sp. = spiele, Pu. = punkte,
                S = siege, `S+` = siege_klar, `S-` = siege_tb,
                N = niederlagen, `N+` = niederlagen_tb, `N-` = niederlagen_klar,
                `Sätze` = paste(saetze_gewonnen, ":", saetze_verloren),
                SQ = satzquotient,
                `Bälle` = paste(baelle_gewonnen, ":", baelle_verloren),
                BQ = ballquotient)
    datatable(d, rownames = FALSE,
              options = list(dom = "t", pageLength = 20, ordering = TRUE)) |>
      formatRound(c("SQ", "BQ"), 2) |>
      formatStyle("Team", target = "row",
                  fontWeight = styleEqual(input$team, "bold"),
                  backgroundColor = styleEqual(input$team, "#FBE7EA"))
  })

  # --- Punkte & Resultatprofil ------------------------------------------
  output$resultate <- renderPlot({
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
    h <- history()
    validate(need(length(unique(h$datum)) > 1,
                  "Der Verlauf erscheint, sobald Daten von mindestens zwei Tagen vorliegen."))
    h <- h |> mutate(hl = team == input$team)
    last <- h |> filter(datum == max(datum))

    ggplot(h, aes(datum, rang, group = team)) +
      geom_line(data = filter(h, !hl), colour = grey, linewidth = 0.8) +
      geom_line(data = filter(h, hl), colour = accent, linewidth = 1.6) +
      geom_point(data = filter(h, hl), colour = accent, size = 2.5) +
      geom_text(data = last, aes(label = team, colour = hl),
                hjust = -0.08, size = 3.8, show.legend = FALSE) +
      scale_colour_manual(values = c(`FALSE` = "grey40", `TRUE` = accent)) +
      scale_y_reverse(breaks = 1:max(h$rang)) +
      scale_x_date(expand = expansion(mult = c(0.02, 0.3))) +
      labs(title = "Rang über die Zeit", x = NULL, y = "Rang")
  })

  # --- Regressionsplots -------------------------------------------------
  output$quotienten <- renderPlot({
    reg_plot(current() |> mutate(hl = team == input$team),
             "satzquotient", y_log = TRUE,
             title = "Ballquotient vs. Satzquotient",
             y_lab = "Satzquotient (log-Skala)",
             res_fmt = "%s (%+.0f%%)")
  })

  output$punkte_bq <- renderPlot({
    reg_plot(current() |> mutate(hl = team == input$team,
                                 punkte_pro_spiel = punkte / spiele),
             "punkte_pro_spiel", y_log = FALSE,
             title = "Ballquotient vs. Punkte pro Spiel",
             y_lab = "Punkte pro Spiel",
             res_fmt = "%s (%+.2f Pt./Spiel)")
  })
}

shinyApp(ui, server)
