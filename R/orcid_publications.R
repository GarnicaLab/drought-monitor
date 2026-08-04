# ============================================================================
#  Garnica Lab -- build the WordPress publication list from ORCID
# ----------------------------------------------------------------------------
#  What it does
#    1. Reads every work on your public ORCID record.
#    2. Pulls full metadata from Crossref for each DOI (complete author list,
#       journal, volume, pages, year).
#    3. Formats APS-style citations, newest first, with your name in bold.
#    4. Writes WordPress block markup you paste into the Code editor.
#
#  How to run
#    Open in RStudio and click Source. Or from a terminal:
#      Rscript orcid_to_wordpress.R
#
#  Output
#    publications-block.txt   (all works)
#    publications-recent.txt  (the N most recent, set by N_RECENT below)
# ============================================================================

## ---- settings --------------------------------------------------------------
ORCID_ID  <- "0000-0001-6351-4357"
HIGHLIGHT <- "Garnica"    # surname to bold
N_RECENT  <- 5            # how many go in the "recent" file
OUT_DIR   <- Sys.getenv("OUT_DIR", unset = getwd())   # CI sets this to docs/

## ---- packages --------------------------------------------------------------
need <- c("httr", "jsonlite")
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) {
  message("Installing: ", paste(miss, collapse = ", "))
  install.packages(miss, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
})

UA <- user_agent("GarnicaLab-pub-builder/1.0 (mailto:vinicius.garnica@wsu.edu)")

## ---- 1. DOIs from ORCID ----------------------------------------------------
message("Reading ORCID record ", ORCID_ID, " ...")

res <- GET(
  sprintf("https://pub.orcid.org/v3.0/%s/works", ORCID_ID),
  accept_json(), UA, timeout(30)
)
if (http_error(res)) {
  stop("ORCID request failed with status ", status_code(res),
       ". Check the iD and your internet connection.")
}

works <- fromJSON(content(res, "text", encoding = "UTF-8"),
                  simplifyVector = FALSE)$group

dois <- character(0)
no_doi <- character(0)

for (g in works) {
  ids <- g$`external-ids`$`external-id`
  d <- NA_character_
  for (id in ids) {
    if (!is.null(id$`external-id-type`) &&
        tolower(id$`external-id-type`) == "doi") {
      d <- trimws(id$`external-id-value`)
      break
    }
  }
  if (!is.na(d)) {
    dois <- c(dois, d)
  } else {
    ttl <- g$`work-summary`[[1]]$title$title$value
    yr  <- g$`work-summary`[[1]]$`publication-date`$year$value
    no_doi <- c(no_doi, sprintf("%s  %s", if (is.null(yr)) "n.d." else yr, ttl))
  }
}
dois <- unique(dois)
message(sprintf("  %d works with a DOI, %d without", length(dois), length(no_doi)))

## ---- 2. metadata from Crossref --------------------------------------------
fmt_authors <- function(a) {
  if (is.null(a) || length(a) == 0) return("Anonymous")
  nm <- vapply(a, function(p) {
    fam <- if (is.null(p$family)) "" else trimws(p$family)
    giv <- if (is.null(p$given))  "" else trimws(p$given)
    if (!nzchar(fam)) return(NA_character_)
    ini <- unlist(strsplit(gsub("\\.", " ", giv), "\\s+"))
    ini <- ini[nzchar(ini)]
    ini <- paste0(substr(ini, 1, 1), ".", collapse = " ")
    out <- if (nzchar(ini)) paste0(fam, ", ", ini) else fam
    if (grepl(HIGHLIGHT, fam, ignore.case = TRUE)) out <- paste0("<strong>", out, "</strong>")
    out
  }, character(1))
  nm <- nm[!is.na(nm)]
  n <- length(nm)
  if (n == 0) return("Anonymous")
  if (n == 1) return(nm)
  if (n == 2) return(paste0(nm[1], ", and ", nm[2]))
  paste0(paste(nm[-n], collapse = ", "), ", and ", nm[n])
}

cite_one <- function(doi) {
  r <- try(GET(paste0("https://api.crossref.org/works/", utils::URLencode(doi, TRUE)),
               UA, timeout(30)), silent = TRUE)
  if (inherits(r, "try-error") || http_error(r)) return(NULL)
  m <- fromJSON(content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE)$message
  if (is.null(m)) return(NULL)

  title   <- if (length(m$title))          sub("\\.$", "", trimws(m$title[[1]]))  else "Untitled"
  journal <- if (length(m$`container-title`)) trimws(m$`container-title`[[1]])    else ""
  journal <- gsub("\u00ae|\u2122", "", journal)   # strip (R) and TM from journal names
  is_pre  <- identical(m$type, "posted-content") || !nzchar(journal)
  vol     <- if (is.null(m$volume)) "" else m$volume
  pg      <- if (is.null(m$page))   "" else m$page
  yr      <- tryCatch(m$issued$`date-parts`[[1]][[1]], error = function(e) NA)
  if (is.null(yr) || is.na(yr)) yr <- "n.d."

  tail <- ""
  if (nzchar(journal)) {
    tail <- paste0(" <em>", journal, "</em>")
    if (nzchar(vol)) tail <- paste0(tail, " ", vol)
    if (nzchar(pg))  tail <- paste0(tail, ":", pg)
    tail <- paste0(tail, ".")
    if (!nzchar(vol) && !nzchar(pg)) tail <- paste0(tail, " In press.")
  } else if (is_pre) {
    tail <- " <em>Preprint</em>."
  }

  list(
    year = suppressWarnings(as.integer(yr)),
    preprint = is_pre,
    html = sprintf(
      '%s (%s). %s.%s <a href="https://doi.org/%s" target="_blank" rel="noreferrer noopener">doi</a>',
      fmt_authors(m$author), yr, title, tail, doi)
  )
}

recs <- list()
for (i in seq_along(dois)) {
  message(sprintf("  [%d/%d] %s", i, length(dois), dois[i]))
  r <- cite_one(dois[i])
  if (!is.null(r)) recs[[length(recs) + 1]] <- r
  Sys.sleep(0.3)   # be polite to Crossref
}

if (!length(recs)) stop("No citations could be built. Check your connection.")

yrs  <- vapply(recs, function(x) if (is.na(x$year)) 0L else x$year, integer(1))
pre  <- vapply(recs, function(x) isTRUE(x$preprint), logical(1))
recs <- recs[order(-yrs, pre)]

## ---- 3. WordPress block markup --------------------------------------------
wp_list <- function(items) {
  paste(c(
    '<!-- wp:list {"ordered":true} -->',
    '<ol class="wp-block-list">',
    unlist(lapply(items, function(h) c("<!-- wp:list-item -->",
                                       paste0("<li>", h, "</li>"),
                                       "<!-- /wp:list-item -->"))),
    "</ol>",
    "<!-- /wp:list -->"
  ), collapse = "\n")
}

html_all    <- vapply(recs, function(x) x$html, character(1))
peer_only   <- vapply(recs, function(x) !isTRUE(x$preprint), logical(1))
html_recent <- utils::head(html_all[peer_only], N_RECENT)   # preprints stay off the home page

f_all    <- file.path(OUT_DIR, "publications-block.txt")
f_recent <- file.path(OUT_DIR, "publications-recent.txt")
writeLines(wp_list(html_all),    f_all,    useBytes = TRUE)
writeLines(wp_list(html_recent), f_recent, useBytes = TRUE)

message("\nDone.")
message(sprintf("  %-28s %d citations", basename(f_all), length(html_all)))
message(sprintf("  %-28s %d citations", basename(f_recent), length(html_recent)))
message("  saved in: ", OUT_DIR)

## ---- 4. a browsable HTML page for the Pages site --------------------------
li <- paste0("<li>", html_all, "</li>", collapse = "\n")
page <- paste0(
'<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">',
'<meta name="viewport" content="width=device-width, initial-scale=1">',
'<title>Publications &middot; Garnica Lab, WSU</title><style>',
'body{margin:0;background:#fff;color:#3F4548;line-height:1.65;',
'font-family:"Open Sans","Segoe UI",system-ui,Helvetica,Arial,sans-serif}',
'.wrap{max-width:900px;margin:0 auto;padding:44px 24px 70px}',
'h1{font-size:32px;color:#1B1E20;margin:0 0 10px;letter-spacing:-.02em}',
'.rule{width:52px;height:3px;background:#A60F2D;margin:0 0 24px}',
'ol{padding-left:22px}li{margin:0 0 16px}',
'a{color:#A60F2D;text-decoration:none}a:hover{text-decoration:underline}',
'.meta{border-top:1px solid #E3E1DD;margin-top:40px;padding-top:20px;',
'font-size:13px;color:#6E7579}</style></head><body><div class="wrap">',
'<h1>Publications</h1><div class="rule"></div>',
'<p>Generated from <a href="https://orcid.org/', ORCID_ID, '">ORCID</a> and Crossref. ',
'Regenerated monthly.</p><ol>', li, '</ol>',
'<div class="meta"><p>Garnica Lab, WSU Department of Plant Pathology.</p></div>',
'</div></body></html>')
writeLines(page, file.path(OUT_DIR, "publications.html"), useBytes = TRUE)
message("  wrote publications.html")

if (length(no_doi)) {
  message("\nThese ORCID works have no DOI and were skipped. Add them by hand:")
  for (s in sort(no_doi, decreasing = TRUE)) message("  ", s)
}
