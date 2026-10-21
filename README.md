# NZ Carbon Pathways

A shinyMobile PWA for testing how a set of government projects would change New Zealand's gross greenhouse-gas emissions. It works on phones and on desktop.

## Run it

```r
install.packages(c("shiny", "shinyMobile", "yaml", "ggplot2"))   # shinyMobile >= 2.0
shiny::runApp()          # from this folder
```

## Layout

```
app.R                  UI and server
R/model.R              YAML load/save/validation + scenario model (no Shiny code)
R/plots.R              ggplot2 charts
data/projects.yml      the project list: edit by hand or via the Add tab
data/baseline.yml      NZ baseline by sector + indicative target waypoints
www/                   manifest.json, sw.js, offline.html, styles.css, icons/
```

## Project format

```yaml
projects:
- id: ev_incentive              # unique; generated automatically by the form
  name: EV purchase incentive
  sector: Transport             # Agriculture | Transport | Energy | Industry | Waste
  description: Optional text
  cost_nzd_m: 1200.0            # PRICE: total cost, NZD million
  implementation_years: 5       # TIMEFRAME: benefits ramp up over this period
  start_year: 2027
  annual_reduction_kt: 600.0    # EMISSIONS: kt CO2e per year at full effect
```

Add projects in the app (**Add** tab) or edit `data/projects.yml`; invalid entries are skipped with a warning. Restart the app (or re-select) after hand edits.

## How aggregation works

Per project and year: `reduction = annual_reduction_kt x ramp x (1 - overlap)`, where `ramp` rises linearly from 0 to 1 over `implementation_years`. Cost is spread evenly over those years. Reductions are summed by sector and capped at 90% of that sector's baseline. The totals are subtracted from a baseline that declines by a chosen % per year, giving the national pathway, 2030 and 2050 emissions, cumulative abatement, total spend and NZD per tonne.

## PWA notes

- Browsers only install PWAs and register service workers over **HTTPS** (or on `localhost`).
- `www/sw.js` caches the static shell and shows `offline.html` when the server is unreachable. Shiny computes on the server, so scenarios need a connection.
- Deploy to Posit Connect, shinyapps.io, or ShinyProxy / Shiny Server behind HTTPS.
- The project list is one file on the server, so every user of a deployment shares it. For multiple instances, or to give each user their own list, swap `load_projects` / `save_projects` for a database.

## Data caveats

Baseline sector figures, target waypoints and all seed projects are **illustrative placeholders**. Replace them with the MfE greenhouse gas inventory, ERP / NDC settings and costed proposals before drawing conclusions. Forestry removals and ETS effects are not modelled.
