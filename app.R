# NZ Carbon Pathways: a shinyMobile PWA for testing carbon-reduction projects.
# Run with: shiny::runApp()   (see README.md for setup and PWA notes)

library(shiny)
library(shinyMobile)
library(ggplot2)
# R/model.R and R/plots.R are sourced automatically by Shiny.

PROJECTS_PATH <- "data/projects.yml"
BASELINE      <- load_baseline("data/baseline.yml")
SECTORS       <- names(BASELINE$sectors)

stat_item <- function(label, value, sub = NULL, warn = FALSE) {
  tags$div(
    class = paste("stat", if (warn) "warn"),
    tags$div(class = "stat-value", value),
    tags$div(class = "stat-label", label),
    if (!is.null(sub)) tags$div(class = "stat-sub", sub)
  )
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- f7Page(
  title = "NZ Carbon Pathways",
  options = list(theme = "auto", dark = FALSE, color = "#0b6e4f", filled = TRUE),
  allowPWA = FALSE,   # manifest + service worker are wired up manually below

  tags$head(
    tags$link(rel = "manifest", href = "manifest.json"),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$link(rel = "apple-touch-icon", href = "icons/icon-192.png"),
    tags$meta(name = "theme-color", content = "#0b6e4f"),
    tags$meta(name = "mobile-web-app-capable", content = "yes"),
    tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
    tags$meta(name = "apple-mobile-web-app-title", content = "NZ Carbon"),
    tags$script(HTML(
      "if ('serviceWorker' in navigator) {
         window.addEventListener('load', function () {
           navigator.serviceWorker.register('sw.js').catch(function (e) { console.warn('SW failed', e); });
         });
       }"
    ))
  ),

  f7TabLayout(
    navbar = f7Navbar(title = "NZ Carbon Pathways"),

    f7Tabs(
      id = "tabs", animated = TRUE,

      # ---- 1. Choose projects --------------------------------------------
      f7Tab(
        title = "Projects", tabName = "projects", icon = f7Icon("list_bullet"), active = TRUE,
        div(class = "split",
          div(class = "sticky",
            uiOutput("mini_stats"),
            f7Card(title = "Live pathway", plotOutput("pathway_mini", height = "230px")),
            f7Card(
              title = "Scenario assumptions",
              f7Slider("bau_decline", "Baseline decline (% per year)",
                       min = 0, max = 3, value = BASELINE$default_decline_pct, step = 0.1),
              f7Slider("overlap", "Overlap discount between projects (%)",
                       min = 0, max = 50, value = 10, step = 5),
              div(class = "hint",
                  "Overlap trims each project's reduction to allow for projects that ",
                  "tackle the same emissions. Reductions in a sector are also capped at 90% of its baseline.")
            )
          ),
          div(
            f7Card(
              title = "Choose projects to fund", class = "picker-card",
#              f7Row(
#                f7Col(f7Button("sel_all",  "Select all",  size = "small")),
#                f7Col(f7Button("sel_none", "Clear",       size = "small"))
#              ),
              uiOutput("project_picker")
            )
          )
        )
      ),

      # ---- 2. National impact ------------------------------------------
      f7Tab(
        title = "Impact", tabName = "impact", icon = f7Icon("chart_bar"),
        uiOutput("impact_stats"),
        div(class = "plot-grid",
          f7Card(plotOutput("pathway_plot", height = "320px")),
          f7Card(plotOutput("sector_plot",  height = "320px")),
          f7Card(plotOutput("abatement_plot", height = "auto")),
          f7Card(plotOutput("cost_plot",      height = "auto")),
          f7Card(plotOutput("spend_plot", height = "300px"))
        ),
        f7Card(title = "Project detail", div(class = "table-wrap", tableOutput("project_table")))
      ),

      # ---- 3. Add / manage projects --------------------------------------
      f7Tab(
        title = "Add", tabName = "add", icon = f7Icon("plus_circle"),
        f7Card(
          title = "Propose a new project",
          f7Text("new_name", "Project name", placeholder = "e.g. Electric ferry network"),
          f7Select("new_sector", "Sector", choices = SECTORS),
          f7TextArea("new_desc", "Description (optional)", placeholder = "What does it do?"),
          f7Text("new_cost", "Total cost (NZD million)", placeholder = "e.g. 850"),
          f7Stepper("new_years", "Implementation time (years)", min = 1, max = 30, value = 5, step = 1),
          f7Stepper("new_start", "Start year", min = 2026, max = 2045, value = 2027, step = 1),
          f7Text("new_reduction", "Reduction at full effect (kt CO2e per year)", placeholder = "e.g. 400"),
          div(class = "hint",
              "Benefits ramp up in a straight line over the implementation time, ",
              "and the cost is spread evenly across those years."),
          f7Button("add_project", "Add project")
        ),
        f7Card(
          title = "Manage the project list",
          uiOutput("delete_picker"),
          f7Button("delete_btn", "Remove selected project", color = "red"),
          br(),
          downloadButton("dl_yaml", "Download projects.yml", class = "button button-outline")
        )
      ),

      # ---- 4. About -------------------------------------------------------
      f7Tab(
        title = "About", tabName = "about", icon = f7Icon("info_circle"),
        f7Card(
          title = "How it works",
          p("Pick projects, and the model adds up each one's cost and emissions reduction year by year ",
            "to show the combined effect on New Zealand's gross emissions."),
          tags$ul(
            tags$li("Benefits ramp up linearly across each project's implementation period."),
            tags$li("Cost is spread evenly across the implementation years."),
            tags$li("Cost per tonne = total spend in the period / total tonnes abated. It ignores operating savings and discounting."),
            tags$li("Projects live in ", tags$code("data/projects.yml"), ", and the baseline in ", tags$code("data/baseline.yml"), ".")
          )
        ),
        f7Card(
          title = "Data caveats",
          p(BASELINE$note),
          p("All seed project costs and reductions are illustrative estimates, not official figures.")
        ),
        f7Card(
          title = "Install as an app",
          p("Open this page in Chrome, Edge or Safari over HTTPS and choose ",
            tags$b("Install app"), " or ", tags$b("Add to Home Screen"), ".")
        )
      )
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  projects_rv <- reactiveVal(load_projects(PROJECTS_PATH, SECTORS))
  selected_rv <- reactiveVal(isolate(projects_rv())$id)   # start with everything selected
  picker_nonce <- reactiveVal(0)                          # bump to rebuild the checkbox list
  bump <- function() picker_nonce(picker_nonce() + 1)

  # ---- Project picker ----------------------------------------------------
  output$project_picker <- renderUI({
    picker_nonce()
    df  <- projects_rv()
#    sel <- isolate(selected_rv())
    if (nrow(df) == 0) return(div(class = "hint", "No projects yet. Add one on the Add tab."))
    labels <- sprintf("%s · %s · %d yr · %s kt/yr",
                      df$name, vapply(df$cost_nzd_m, fmt_nzd, ""),
                      df$implementation_years, format(df$annual_reduction_kt, big.mark = ",", trim = TRUE))
    f7CheckboxGroup("selected", "Projects", choices = stats::setNames(df$id, labels))
#        f7CheckboxGroup("selected", "Projects", choices = stats::setNames(df$id, labels), selected = sel)
  })

  observeEvent(input$selected, selected_rv(input$selected), ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$sel_all,  { selected_rv(projects_rv()$id); bump() })
  observeEvent(input$sel_none, { selected_rv(character());      bump() })

  # ---- Scenario ----------------------------------------------------------
  scenario <- reactive({
    req(input$bau_decline, input$overlap)
    run_scenario(projects_rv(), BASELINE, selected_rv(),
                 decline = input$bau_decline / 100, overlap = input$overlap / 100)
  })

  # ---- Stat tiles --------------------------------------------------------
  output$mini_stats <- renderUI({
    s <- scenario()$summary
    div(class = "stat-grid",
      stat_item("Selected", s$n_projects),
      stat_item("Total cost", fmt_nzd(s$total_cost_m)),
      stat_item(sprintf("Emissions %d", s$horizon), fmt_mt(s$em_end),
                sprintf("%+.0f%% vs %d", 100 * (s$em_end / s$base_mt - 1), BASELINE$base_year))
    )
  })

  output$impact_stats <- renderUI({
    s <- scenario()$summary
    gap_txt <- if (is.na(s$gap_end)) "n/a" else sprintf("%s %s", fmt_mt(abs(s$gap_end)),
                                                        if (s$gap_end > 0) "above" else "below")
    div(class = "stat-grid",
      stat_item("Projects selected", s$n_projects),
      stat_item("Total cost", fmt_nzd(s$total_cost_m)),
      stat_item(sprintf("Abated to %d", s$horizon), fmt_mt(s$cum_mt), "cumulative"),
      stat_item("Average cost", if (is.na(s$cost_per_t)) "n/a" else sprintf("$%s / t", format(round(s$cost_per_t), big.mark = ","))),
      stat_item("Emissions 2030", fmt_mt(s$em_2030),
                sprintf("%+.0f%% vs %d", 100 * (s$em_2030 / s$base_mt - 1), BASELINE$base_year)),
      stat_item(sprintf("Emissions %d", s$horizon), fmt_mt(s$em_end),
                sprintf("%.1f Mt lower than baseline", s$bau_end - s$em_end)),
      stat_item(sprintf("vs %d target", s$horizon), gap_txt, "indicative target", warn = isTRUE(s$gap_end > 0))
    )
  })

  # ---- Plots -------------------------------------------------------------
  output$pathway_mini    <- renderPlot(plot_pathway(scenario(), 11) + theme(plot.title = element_blank()), res = 96)
  output$pathway_plot    <- renderPlot(plot_pathway(scenario()), res = 96)
  output$sector_plot     <- renderPlot(plot_sectors(scenario()), res = 96)
  output$spend_plot      <- renderPlot(plot_spend(scenario()),   res = 96)
  bar_height <- function() 90 + 36 * max(3, nrow(scenario()$by_project))
  output$abatement_plot  <- renderPlot(plot_projects(scenario()),           res = 96, height = bar_height)
  output$cost_plot       <- renderPlot(plot_cost_effectiveness(scenario()), res = 96, height = bar_height)

  output$project_table <- renderTable({
    d <- scenario()$by_project
    if (nrow(d) == 0) return(data.frame(Message = "No projects selected."))
    data.frame(
      Project = d$name, Sector = d$sector,
      `Cost (NZD m)` = format(round(d$cost_m), big.mark = ","),
      `Abated (Mt)`  = sprintf("%.1f", d$abatement_mt),
      `NZD per t`    = ifelse(is.na(d$cost_per_t), "n/a", format(round(d$cost_per_t), big.mark = ",")),
      check.names = FALSE
    )
  }, striped = TRUE, width = "100%")

  # ---- Add a project -----------------------------------------------------
  observeEvent(input$add_project, {
    p <- list(
      name = trimws(input$new_name %||% ""),
      sector = input$new_sector,
      description = trimws(input$new_desc %||% ""),
      cost_nzd_m = suppressWarnings(as.numeric(gsub(",", "", input$new_cost %||% ""))),
      implementation_years = input$new_years,
      start_year = input$new_start,
      annual_reduction_kt = suppressWarnings(as.numeric(gsub(",", "", input$new_reduction %||% "")))
    )
    errs <- validate_project(p, SECTORS)
    if (length(errs) > 0) {
      f7Dialog(id = "validation_msg", title = "Please check the form",
               text = paste(errs, collapse = " "), type = "alert")
      return()
    }
    cur <- projects_rv()
    p$id <- make_unique_id(slugify(p$name), cur$id)
    updated <- rbind(cur, project_to_row(p))
    ok <- tryCatch({ save_projects(updated, PROJECTS_PATH); TRUE }, error = function(e) FALSE)
    if (!ok) {
      f7Toast("Could not save to data/projects.yml (is the folder writable?)", position = "center")
      return()
    }
    projects_rv(updated)
    selected_rv(c(selected_rv(), p$id))
    bump()
    updateF7Text("new_name", value = "")
    updateF7TextArea("new_desc", value = "")
    updateF7Text("new_cost", value = "")
    updateF7Text("new_reduction", value = "")
    f7Toast(paste0("Added \"", p$name, "\" and selected it."), position = "center")
  })

  # ---- Remove a project --------------------------------------------------
  output$delete_picker <- renderUI({
    df <- projects_rv()
    if (nrow(df) == 0) return(div(class = "hint", "Nothing to remove."))
    f7Select("del_id", "Project", choices = stats::setNames(df$id, df$name))
  })

  observeEvent(input$delete_btn, {
    req(input$del_id)
    nm <- projects_rv()$name[projects_rv()$id == input$del_id]
    f7Dialog(id = "confirm_del", title = "Remove project?",
             text = paste0("\"", nm, "\" will be deleted from projects.yml."), type = "confirm")
  })

  observeEvent(input$confirm_del, {
    req(isTRUE(input$confirm_del), input$del_id)
    updated <- projects_rv()[projects_rv()$id != input$del_id, , drop = FALSE]
    save_projects(updated, PROJECTS_PATH)
    projects_rv(updated)
    selected_rv(setdiff(selected_rv(), input$del_id))
    bump()
    f7Toast("Project removed.", position = "center")
  })

  # ---- Download ----------------------------------------------------------
  output$dl_yaml <- downloadHandler(
    filename = "projects.yml",
    content  = function(file) file.copy(PROJECTS_PATH, file, overwrite = TRUE)
  )
}

shinyApp(ui, server)
