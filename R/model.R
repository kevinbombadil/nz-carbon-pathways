# ---------------------------------------------------------------------------
# Data loading, validation and the scenario model.
# Shiny sources everything in R/ automatically, and none of this needs the UI.
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
}

# ---- Baseline --------------------------------------------------------------

load_baseline <- function(path = "data/baseline.yml") {
  b  <- yaml::read_yaml(path)
  tg <- do.call(rbind, lapply(b$targets, function(t) {
    data.frame(year = as.integer(t$year), gross_mt = as.numeric(t$gross_mt))
  }))
  list(
    base_year          = as.integer(b$base_year),
    sectors            = unlist(b$sectors),            # named numeric, MtCO2e
    default_decline_pct = as.numeric(b$bau_decline_pct_per_year %||% 0.5),
    targets            = tg[order(tg$year), , drop = FALSE],
    note               = as.character(b$note %||% "")
  )
}

# ---- Projects (YAML <-> data.frame) ----------------------------------------

empty_projects <- function() {
  data.frame(
    id = character(), name = character(), sector = character(),
    description = character(), cost_nzd_m = numeric(),
    implementation_years = integer(), start_year = integer(),
    annual_reduction_kt = numeric(), stringsAsFactors = FALSE
  )
}

project_to_row <- function(p) {
  data.frame(
    id                   = as.character(p$id %||% ""),
    name                 = as.character(p$name %||% ""),
    sector               = as.character(p$sector %||% ""),
    description          = as.character(p$description %||% ""),
    cost_nzd_m           = as.numeric(p$cost_nzd_m %||% NA),
    implementation_years = as.integer(p$implementation_years %||% NA),
    start_year           = as.integer(p$start_year %||% NA),
    annual_reduction_kt  = as.numeric(p$annual_reduction_kt %||% NA),
    stringsAsFactors = FALSE
  )
}

# Returns a character vector of problems (empty if the project is valid).
validate_project <- function(p, sectors = NULL) {
  errs <- character()
  num  <- function(x) suppressWarnings(as.numeric(x %||% NA))
  if (!nzchar(trimws(as.character(p$name %||% "")))) errs <- c(errs, "Enter a project name.")
  if (!is.null(sectors) && !(as.character(p$sector %||% "") %in% sectors))
    errs <- c(errs, "Choose a valid sector.")
  cost <- num(p$cost_nzd_m)
  if (!is.finite(cost) || cost <= 0) errs <- c(errs, "Cost must be a number above 0 (NZD million).")
  yrs <- num(p$implementation_years)
  if (!is.finite(yrs) || yrs < 1 || yrs > 30) errs <- c(errs, "Implementation time must be 1-30 years.")
  st <- num(p$start_year)
  if (!is.finite(st) || st < 2025 || st > 2050) errs <- c(errs, "Start year must be between 2025 and 2050.")
  red <- num(p$annual_reduction_kt)
  if (!is.finite(red) || red <= 0) errs <- c(errs, "Emissions reduction must be a number above 0 (kt CO2e per year).")
  errs
}

load_projects <- function(path = "data/projects.yml", sectors = NULL) {
  if (!file.exists(path)) return(empty_projects())
  raw   <- yaml::read_yaml(path)
  items <- raw$projects
  if (is.null(items) || length(items) == 0) return(empty_projects())

  keep <- vapply(items, function(p) length(validate_project(p, sectors)) == 0 && nzchar(p$id %||% ""), logical(1))
  if (any(!keep)) warning(sum(!keep), " project(s) in ", path, " were invalid and skipped.")
  items <- items[keep]
  if (length(items) == 0) return(empty_projects())

  df <- do.call(rbind, lapply(items, project_to_row))
  df[!duplicated(df$id), , drop = FALSE]
}

# Write atomically so a crash never leaves a half-written file.
save_projects <- function(df, path = "data/projects.yml") {
  items <- lapply(seq_len(nrow(df)), function(i) {
    r <- as.list(df[i, , drop = FALSE])
    r$implementation_years <- as.integer(r$implementation_years)
    r$start_year           <- as.integer(r$start_year)
    r
  })
  tmp <- paste0(path, ".tmp")
  yaml::write_yaml(list(projects = items), tmp)
  file.rename(tmp, path)
}

slugify <- function(x) {
  s <- gsub("[^a-z0-9]+", "_", tolower(x))
  s <- gsub("^_+|_+$", "", s)
  substr(if (nzchar(s)) s else "project", 1, 40)
}

make_unique_id <- function(base, existing) {
  id <- base; i <- 2
  while (id %in% existing) { id <- paste0(base, "_", i); i <- i + 1 }
  id
}

# ---- Scenario model --------------------------------------------------------
#
# For each selected project and year:
#   ramp      = clamp((year - start_year + 1) / implementation_years, 0, 1)
#   reduction = annual_reduction_kt * ramp * (1 - overlap)
#   cost      = cost_nzd_m / implementation_years, during the implementation years only
# Then, per sector and year, total reductions are capped at `cap_share` of that
# sector's baseline, so stacking projects can never remove more than exists.
#
# Baseline: each sector declines by `decline` per year from the base year.

run_scenario <- function(projects, baseline, selected_ids,
                         decline = 0.005, overlap = 0.10,
                         horizon = 2050L, cap_share = 0.9) {
  years   <- baseline$base_year:horizon
  sectors <- names(baseline$sectors)

  bau <- expand.grid(year = years, sector = sectors, stringsAsFactors = FALSE)
  bau$bau_kt <- unname(baseline$sectors[bau$sector]) * 1000 *
    (1 - decline)^(bau$year - baseline$base_year)

  proj <- projects[projects$id %in% selected_ids, , drop = FALSE]

  if (nrow(proj) > 0) {
    traj <- do.call(rbind, lapply(seq_len(nrow(proj)), function(i) {
      p <- proj[i, ]
      n <- max(p$implementation_years, 1L)
      ramp <- pmin(pmax((years - p$start_year + 1) / n, 0), 1)
      data.frame(
        id = p$id, name = p$name, sector = p$sector, year = years,
        raw_kt = p$annual_reduction_kt * ramp * (1 - overlap),
        cost_m = ifelse(years >= p$start_year & years < p$start_year + n, p$cost_nzd_m / n, 0),
        stringsAsFactors = FALSE
      )
    }))
    traj <- merge(traj, bau, by = c("year", "sector"))
    traj$sec_total    <- ave(traj$raw_kt, traj$year, traj$sector, FUN = sum)
    cap               <- cap_share * traj$bau_kt
    traj$reduction_kt <- traj$raw_kt * ifelse(traj$sec_total > cap, cap / traj$sec_total, 1)
  } else {
    traj <- data.frame(
      year = integer(), sector = character(), id = character(), name = character(),
      raw_kt = numeric(), cost_m = numeric(), bau_kt = numeric(),
      sec_total = numeric(), reduction_kt = numeric(), stringsAsFactors = FALSE
    )
  }

  # National pathway
  bau_year <- aggregate(bau_kt ~ year, bau, sum)
  if (nrow(traj) > 0) {
    red_year <- aggregate(reduction_kt ~ year, traj, sum)
    pw <- merge(bau_year, red_year, by = "year", all.x = TRUE)
  } else {
    pw <- bau_year; pw$reduction_kt <- 0
  }
  pw$reduction_kt[is.na(pw$reduction_kt)] <- 0
  pw$bau_mt  <- pw$bau_kt / 1000
  pw$with_mt <- (pw$bau_kt - pw$reduction_kt) / 1000

  # Indicative target path: linear between the base year total and each waypoint.
  base_total <- sum(baseline$sectors)
  tg <- baseline$targets
  pw$target_mt <- approx(c(baseline$base_year, tg$year), c(base_total, tg$gross_mt),
                         xout = pw$year, rule = 2)$y

  # Per-project totals
  if (nrow(traj) > 0) {
    by_project <- aggregate(cbind(reduction_kt, cost_m) ~ id + name + sector, traj, sum)
    by_project$abatement_mt <- by_project$reduction_kt / 1000
    by_project$cost_per_t   <- ifelse(by_project$reduction_kt > 0,
                                      by_project$cost_m * 1000 / by_project$reduction_kt, NA_real_)
    by_project <- by_project[order(-by_project$abatement_mt), , drop = FALSE]
    spend <- aggregate(cost_m ~ year + sector, traj, sum)
    spend <- spend[spend$cost_m > 0, , drop = FALSE]
  } else {
    by_project <- data.frame(id = character(), name = character(), sector = character(),
                             reduction_kt = numeric(), cost_m = numeric(),
                             abatement_mt = numeric(), cost_per_t = numeric())
    spend <- data.frame(year = integer(), sector = character(), cost_m = numeric())
  }

  # Sector view in the final year
  end <- bau[bau$year == horizon, c("sector", "bau_kt")]
  if (nrow(traj) > 0) {
    r_end <- aggregate(reduction_kt ~ sector, traj[traj$year == horizon, ], sum)
    end <- merge(end, r_end, by = "sector", all.x = TRUE)
  } else end$reduction_kt <- 0
  end$reduction_kt[is.na(end$reduction_kt)] <- 0
  sector_end <- rbind(
    data.frame(sector = end$sector, series = "Baseline",              mt = end$bau_kt / 1000),
    data.frame(sector = end$sector, series = "With selected projects", mt = (end$bau_kt - end$reduction_kt) / 1000)
  )

  at <- function(col, yr) pw[[col]][pw$year == yr]
  total_cost_m <- sum(traj$cost_m)
  cum_kt       <- sum(traj$reduction_kt)

  summary <- list(
    n_projects    = nrow(proj),
    total_cost_m  = total_cost_m,
    cum_mt        = cum_kt / 1000,
    cost_per_t    = if (cum_kt > 0) total_cost_m * 1000 / cum_kt else NA_real_,
    base_mt       = base_total,
    em_2030       = at("with_mt", 2030),
    em_end        = at("with_mt", horizon),
    bau_end       = at("bau_mt", horizon),
    target_end    = at("target_mt", horizon),
    horizon       = horizon
  )
  summary$gap_end <- summary$em_end - summary$target_end   # > 0 means above target

  list(pathway = pw, by_project = by_project, spend = spend,
       sector_end = sector_end, summary = summary, horizon = horizon)
}

# ---- Formatting helpers ----------------------------------------------------

fmt_nzd <- function(m) {
  if (is.na(m)) return("n/a")
  if (m >= 1000) sprintf("$%.1f b", m / 1000) else sprintf("$%.0f m", m)
}
fmt_mt  <- function(x) if (is.na(x)) "n/a" else sprintf("%.1f Mt", x)
