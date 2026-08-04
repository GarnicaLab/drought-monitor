# ============================================================================
#  Garnica Lab -- drought tracking for the Washington Columbia Basin
#  potato-producing counties
# ----------------------------------------------------------------------------
#  Statewide USDM numbers are diluted by the Cascades and the wet west side.
#  This aggregates the county-level record across the irrigated Columbia Basin
#  counties where Washington potatoes are actually grown.
#
#  Writes:
#    drought-basin-current.png      this week vs. 3 months and 1 year ago
#    drought-basin-timeseries.png   stacked area, % of basin area by category
#    drought-basin-dsci.png         Drought Severity and Coverage Index
#    drought-basin-weekly.csv       the aggregated weekly record
#    drought-basin-by-county.csv    the same, county by county
#
#  Run:  open in RStudio and click Source, or  Rscript drought_basin_plots.R
#
#  Data: U.S. Drought Monitor (NDMC-UNL, USDA, NOAA). Free to use with credit.
# ============================================================================

## ---- which counties count as the basin -------------------------------------
# Core Columbia Basin potato counties. Adjust this to match how you define the
# region; the script does not care how many you list.
COUNTIES <- c(
  "53025" = "Grant",
  "53021" = "Franklin",
  "53001" = "Adams",
  "53005" = "Benton",
  "53071" = "Walla Walla",
  "53077" = "Yakima"
)
# Optional additions with smaller acreage. Uncomment to include:
# COUNTIES <- c(COUNTIES,
#   "53017" = "Douglas", "53043" = "Lincoln", "53039" = "Klickitat")

REGION_NAME <- "Washington Columbia Basin"
START_YEAR <- 2000
OUT_DIR    <- Sys.getenv("OUT_DIR", unset = getwd())   # CI sets this to docs/
W_PX <- 1600; H_PX <- 900; DPI <- 150

## brand
PINE <- "#2F6B63"; FOREST <- "#1E3B37"; TERRA <- "#BE7350"
INK  <- "#3A4A46"; MUTED  <- "#6E7D78"; RULE  <- "#DDD7CB"

## official USDM category colors
USDM_COLS <- c("D0" = "#FFFF00", "D1" = "#FCD37F", "D2" = "#FFAA00",
               "D3" = "#E60000", "D4" = "#730000")
USDM_LABS <- c("D0" = "D0  Abnormally dry",
               "D1" = "D1  Moderate drought",
               "D2" = "D2  Severe drought",
               "D3" = "D3  Extreme drought",
               "D4" = "D4  Exceptional drought")

## ---- packages --------------------------------------------------------------
need <- c("httr", "jsonlite", "ggplot2", "dplyr", "tidyr", "scales")
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) {
  message("Installing: ", paste(miss, collapse = ", "))
  install.packages(miss, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(ggplot2)
  library(dplyr); library(tidyr); library(scales)
})

UA <- user_agent("GarnicaLab-drought/1.1 (mailto:vinicius.garnica@wsu.edu)")
num <- function(x) suppressWarnings(as.numeric(as.character(x)))

parse_usdm_date <- function(x) {
  x <- as.character(x)
  d <- suppressWarnings(as.Date(substr(x, 1, 10)))
  if (all(is.na(d))) d <- suppressWarnings(as.Date(x, "%Y%m%d"))
  d
}

## ---- generic fetch ---------------------------------------------------------
#  service = "CountyStatistics" or "StateStatistics"
#  method  = "GetDroughtSeverityStatisticsByArea"        (square miles)
#            "GetDroughtSeverityStatisticsByAreaPercent" (percent)
fetch_year <- function(service, method, aoi, yr) {
  url <- paste0(
    "https://usdmdataservices.unl.edu/api/", service, "/", method,
    "?aoi=", aoi,
    "&startdate=", sprintf("1/1/%d", yr),
    "&enddate=",   sprintf("12/31/%d", yr),
    "&statisticsType=1"
  )
  r <- try(GET(url, accept_json(), UA, timeout(60)), silent = TRUE)
  if (inherits(r, "try-error") || http_error(r)) return(NULL)
  d <- try(fromJSON(content(r, "text", encoding = "UTF-8")), silent = TRUE)
  if (inherits(d, "try-error") || !is.data.frame(d) || !nrow(d)) return(NULL)
  names(d) <- tolower(names(d))
  d
}

tidy_usdm <- function(d) {
  date_col <- grep("date", names(d), value = TRUE)[1]
  want <- c("none", "d0", "d1", "d2", "d3", "d4")
  missing <- setdiff(want, names(d))
  if (is.na(date_col) || length(missing)) {
    stop("Unexpected columns from the service.\n  got: ",
         paste(names(d), collapse = ", "))
  }
  d %>%
    mutate(date = parse_usdm_date(.data[[date_col]]),
           across(all_of(want), num)) %>%
    filter(!is.na(date))
}

this_year <- as.integer(format(Sys.Date(), "%Y"))
years <- START_YEAR:this_year

## ---- 1. county square miles ------------------------------------------------
# Using AREA (square miles) rather than percent means the counties aggregate
# correctly by simple addition. No weighting assumptions needed.
message("Fetching county-level USDM area records (", length(COUNTIES),
        " counties x ", length(years), " years)")

cty <- list()
for (i in seq_along(COUNTIES)) {
  fips <- names(COUNTIES)[i]
  message("  ", COUNTIES[i], " (", fips, ")")
  for (yr in years) {
    d <- fetch_year("CountyStatistics", "GetDroughtSeverityStatisticsByArea",
                    fips, yr)
    if (!is.null(d)) {
      d$fips <- fips
      d$county <- unname(COUNTIES[i])
      cty[[length(cty) + 1]] <- d
    }
    Sys.sleep(0.2)
  }
}
if (!length(cty)) stop("No county data returned. Check your connection.")

cty <- bind_rows(cty) %>% tidy_usdm()

## ---- 2. aggregate to the basin --------------------------------------------
basin <- cty %>%
  group_by(date) %>%
  summarise(across(c(none, d0, d1, d2, d3, d4), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  arrange(date)

total_sqmi <- basin$none[1] + basin$d0[1]     # cumulative: none + (d0 or worse)
message("\nBasin extent: ", format(round(total_sqmi), big.mark = ","),
        " square miles across ", length(COUNTIES), " counties")

basin <- basin %>%
  mutate(total = none + d0,
         across(c(none, d0, d1, d2, d3, d4), ~ 100 * .x / total)) %>%
  select(-total) %>%
  rename(None = none, D0 = d0, D1 = d1, D2 = d2, D3 = d3, D4 = d4) %>%
  mutate(DSCI = D0 + D1 + D2 + D3 + D4)

message(nrow(basin), " weekly records, ",
        format(min(basin$date)), " to ", format(max(basin$date)))

# cumulative -> exclusive bands, for stacking
excl <- basin %>%
  transmute(date, D0 = D0 - D1, D1 = D1 - D2, D2 = D2 - D3,
            D3 = D3 - D4, D4 = D4) %>%
  pivot_longer(-date, names_to = "cat", values_to = "pct") %>%
  mutate(pct = pmax(pct, 0),
         cat = factor(cat, levels = c("D4", "D3", "D2", "D1", "D0")))

## ---- 3. theme --------------------------------------------------------------
theme_lab <- function(base = 13) {
  theme_minimal(base_size = base) +
    theme(
      plot.background   = element_rect(fill = "white", colour = NA),
      panel.background  = element_rect(fill = "white", colour = NA),
      panel.grid.minor  = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = RULE, linewidth = 0.35),
      axis.text  = element_text(colour = MUTED, size = base - 2),
      axis.title = element_text(colour = INK, size = base - 1),
      plot.title = element_text(colour = FOREST, face = "bold",
                                size = base + 5, margin = margin(b = 4)),
      plot.subtitle = element_text(colour = MUTED, size = base - 1,
                                   margin = margin(b = 14)),
      plot.caption  = element_text(colour = MUTED, size = base - 4, hjust = 0,
                                   margin = margin(t = 14)),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(colour = INK, size = base - 2),
      legend.key.height = unit(9, "pt"),
      plot.margin = margin(18, 22, 14, 18)
    )
}

CAPTION <- paste0(
  "Counties included: ", paste(sort(unname(COUNTIES)), collapse = ", "), ".\n",
  "Data: U.S. Drought Monitor, a joint product of the National Drought ",
  "Mitigation Center (UNL), USDA, and NOAA. Figure: Garnica Lab, WSU."
)

save_png <- function(p, file) {
  ggsave(file.path(OUT_DIR, file), p,
         width = W_PX / DPI, height = H_PX / DPI, dpi = DPI, bg = "white")
  message("  wrote ", file)
}

fill_usdm <- list(
  scale_fill_manual(values = USDM_COLS[c("D4","D3","D2","D1","D0")],
                    labels = USDM_LABS[c("D4","D3","D2","D1","D0")],
                    breaks = c("D0","D1","D2","D3","D4")),
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))
)

## ---- 4. figure 1: stacked area --------------------------------------------
p1 <- ggplot(excl, aes(date, pct, fill = cat)) +
  geom_area(colour = NA) +
  fill_usdm +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  labs(title = paste0("Drought conditions in the ", REGION_NAME),
       subtitle = paste0("Percent of land area by U.S. Drought Monitor category, weekly, ",
                         format(min(basin$date), "%Y"), " to present"),
       x = NULL, y = "Percent of basin area", caption = CAPTION) +
  theme_lab()
save_png(p1, "drought-basin-timeseries.png")

## ---- 5. figure 2: DSCI ----------------------------------------------------
p2 <- ggplot(basin, aes(date, DSCI)) +
  geom_hline(yintercept = seq(100, 400, 100), colour = RULE, linewidth = 0.35) +
  geom_line(colour = PINE, linewidth = 0.8) +
  geom_point(data = slice_max(basin, date, n = 1), colour = TERRA, size = 3) +
  geom_text(data = slice_max(basin, date, n = 1),
            aes(label = sprintf("  %.0f", DSCI)),
            colour = TERRA, hjust = 0, fontface = "bold", size = 4.2) +
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 100),
                     expand = expansion(mult = c(0.01, 0.06))) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Drought Severity and Coverage Index",
       subtitle = paste0(REGION_NAME,
                         ". DSCI folds extent and severity into a single 0 to 500 scale."),
       x = NULL, y = "DSCI", caption = CAPTION) +
  theme_lab()
save_png(p2, "drought-basin-dsci.png")

## ---- 6. figure 3: now vs. recent history ----------------------------------
latest <- max(basin$date)
pick <- function(target) basin$date[which.min(abs(basin$date - target))]
marks <- c("This week" = latest,
           "3 months ago" = pick(latest - 91),
           "1 year ago" = pick(latest - 365))

cur <- excl %>%
  filter(date %in% marks) %>%
  mutate(when = factor(names(marks)[match(date, marks)],
                       levels = rev(names(marks))))

p3 <- ggplot(cur, aes(when, pct, fill = cat)) +
  geom_col(width = 0.55) + coord_flip() + fill_usdm +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Columbia Basin drought status",
       subtitle = paste0("Percent of basin area by category. Week of ",
                         format(latest, "%B %d, %Y"), "."),
       x = NULL, y = "Percent of basin area", caption = CAPTION) +
  theme_lab() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = RULE, linewidth = 0.35))
save_png(p3, "drought-basin-current.png")

## ---- 7. summary ------------------------------------------------------------
now <- basin %>% slice_max(date, n = 1)
message("\n---- ", REGION_NAME, ", week of ", format(now$date, "%B %d, %Y"), " ----")
for (k in c("D0","D1","D2","D3","D4")) {
  message(sprintf("  %s or worse: %5.1f%% of basin area", k, now[[k]]))
}
message(sprintf("  DSCI: %.0f", now$DSCI))

write.csv(basin, file.path(OUT_DIR, "drought-basin-weekly.csv"), row.names = FALSE)
write.csv(cty,   file.path(OUT_DIR, "drought-basin-by-county.csv"), row.names = FALSE)
message("\n  wrote drought-basin-weekly.csv and drought-basin-by-county.csv")
