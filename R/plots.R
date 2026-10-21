# ggplot2 charts for the dashboard. Each takes the list from run_scenario().

SECTOR_COLOURS <- c(Agriculture = "#7a9e3b", Transport = "#2b6cb0", Energy = "#e08a1e",
                    Industry = "#7b5ea7", Waste = "#8c6d4f")

theme_nz <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor   = element_blank(),
      legend.position    = "bottom",
      legend.title       = element_blank(),
      plot.title.position = "plot",
      plot.title         = element_text(face = "bold", colour = "#17302a")
    )
}

empty_plot <- function(msg = "Select at least one project") {
  ggplot() +
    annotate("text", x = 0, y = 0, label = msg, size = 5, colour = "#55706a") +
    theme_void()
}

plot_pathway <- function(sc, base_size = 13) {
  pw <- sc$pathway
  long <- rbind(
    data.frame(year = pw$year, mt = pw$bau_mt,    series = "Baseline (no new projects)"),
    data.frame(year = pw$year, mt = pw$with_mt,   series = "With selected projects"),
    data.frame(year = pw$year, mt = pw$target_mt, series = "Indicative target")
  )
  long$series <- factor(long$series,
    levels = c("Baseline (no new projects)", "With selected projects", "Indicative target"))
  ggplot(long, aes(year, mt, colour = series, linetype = series)) +
    geom_line(linewidth = 1.1) +
    scale_colour_manual(values = c("#8a9d98", "#0b6e4f", "#d97706")) +
    scale_linetype_manual(values = c("solid", "solid", "dashed")) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(x = NULL, y = "Gross emissions (MtCO2e)",
         title = "New Zealand gross emissions pathway") +
    theme_nz(base_size)
}

plot_projects <- function(sc, base_size = 13) {
  d <- sc$by_project
  if (nrow(d) == 0) return(empty_plot())
  d$name <- factor(d$name, levels = rev(d$name))
  ggplot(d, aes(abatement_mt, name, fill = sector)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = sprintf("%.1f", abatement_mt)), hjust = -0.15, size = base_size * 0.27) +
    scale_fill_manual(values = SECTOR_COLOURS, drop = TRUE) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(x = sprintf("Cumulative abatement to %d (MtCO2e)", sc$horizon), y = NULL,
         title = "Abatement by project") +
    theme_nz(base_size)
}

plot_cost_effectiveness <- function(sc, base_size = 13) {
  d <- sc$by_project
  d <- d[!is.na(d$cost_per_t), , drop = FALSE]
  if (nrow(d) == 0) return(empty_plot())
  d <- d[order(d$cost_per_t), , drop = FALSE]
  d$name <- factor(d$name, levels = rev(d$name))
  ggplot(d, aes(cost_per_t, name, fill = sector)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = sprintf("$%s", format(round(cost_per_t), big.mark = ","))),
              hjust = -0.15, size = base_size * 0.27) +
    scale_fill_manual(values = SECTOR_COLOURS, drop = TRUE) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(x = "NZD per tonne CO2e abated (lower is better)", y = NULL,
         title = "Cost-effectiveness") +
    theme_nz(base_size)
}

plot_spend <- function(sc, base_size = 13) {
  d <- sc$spend
  if (nrow(d) == 0) return(empty_plot())
  ggplot(d, aes(year, cost_m, fill = sector)) +
    geom_col(width = 0.8) +
    scale_fill_manual(values = SECTOR_COLOURS, drop = TRUE) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ","),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(x = NULL, y = "Spend (NZD million per year)", title = "Spending profile") +
    theme_nz(base_size)
}

plot_sectors <- function(sc, base_size = 13) {
  d <- sc$sector_end
  d$series <- factor(d$series, levels = c("Baseline", "With selected projects"))
  ggplot(d, aes(sector, mt, fill = series)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(values = c("#8a9d98", "#0b6e4f")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(x = NULL, y = "MtCO2e", title = sprintf("Emissions by sector in %d", sc$horizon)) +
    theme_nz(base_size)
}
