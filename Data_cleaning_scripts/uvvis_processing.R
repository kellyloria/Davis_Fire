# UV-Vis processing rewrite from Python notebook to R
# Mirrors the original notebook workflow:
#   1) read all raw spectra CSV files from rawdata/
#   2) baseline-correct using 700-800 nm
#   3) convert absorbance to absorption coefficient (10 cm cuvette = 0.1 m)
#   4) calculate a254, E2:E3, CDOM, S275-295, S350-400, and slope ratio
#   5) write processed summary table
#   6) make example/tutorial plots for selected files


# =========================================================================

options(stringsAsFactors = FALSE) 

# -------------------------
# Date stamp for all output filenames
# -------------------------
lab_date    <- Sys.Date()                          # e.g. 2026-03-16
date_tag    <- format(lab_date, "%Y%m%d")          # compact form: 20260316
# Usage: paste0("my_file_", date_tag, ".csv")  →  "my_file_20260316.csv"

# -------------------------
# User settings
# -------------------------
datapath      <- "/Users/kellyloria/Documents/DRI/UVvis_Reno/"
rawdata_dir   <- file.path(datapath, "rawdata_vegas")
outfile       <- file.path(datapath, paste0("processed_",                date_tag, ".csv"))
path_length_m <- 0.1   # 10 cm cuvette → 0.1 m

shortlist <- c("12102025_vegas_winters_usgs.csv", "12102025_vegas_winters_up.csv")

doclist   <- c(4.10, 2.12)
burnlist  <- c(97, 9)   # kept for parity with notebook; not used in plots below

# -------------------------
# Helper functions
# -------------------------

read_uvvis_csv <- function(file) {
  # Python: pd.read_csv(f, header=1, index_col=0)
  # Row 1 (0-indexed) is the header; skip the instrument metadata on row 0.
  df <- read.csv(file, skip = 1, check.names = FALSE)
  
  if (ncol(df) < 2) {
    stop(sprintf("File %s does not have at least two columns.", basename(file)))
  }
  
  names(df)[1:2] <- c("wavelength", "Abs")
  df$wavelength  <- suppressWarnings(as.numeric(df$wavelength))
  df$Abs         <- suppressWarnings(as.numeric(df$Abs))
  df <- df[!is.na(df$wavelength), c("wavelength", "Abs")]
  df <- df[order(df$wavelength), ]
  
  # Keep only integer wavelengths (mirrors df.loc[np.arange(...)] in Python)
  wl_seq  <- seq(min(df$wavelength, na.rm = TRUE),
                 max(df$wavelength, na.rm = TRUE), by = 1)
  full_df <- merge(data.frame(wavelength = wl_seq), df,
                   by = "wavelength", all.x = TRUE, sort = TRUE)
  full_df
}

# Removed lower bound constraint so behaviour matches curve_fit default
fit_spectral_slope <- function(df, start_nm, end_nm) {
  sub <- df[df$wavelength >= start_nm & df$wavelength <= end_nm,
            c("wavelength", "abs_coeff")]
  sub <- sub[is.finite(sub$abs_coeff) & !is.na(sub$abs_coeff), ]
  sub <- sub[sub$abs_coeff > 0, ]
  
  if (nrow(sub) < 3) return(NA_real_)
  
  lambda0 <- start_nm
  a0 <- sub$abs_coeff[sub$wavelength == lambda0][1]
  if (!is.finite(a0) || is.na(a0) || a0 <= 0) a0 <- sub$abs_coeff[1]
  if (!is.finite(a0) || is.na(a0) || a0 <= 0) return(NA_real_)
  
  # Exponential model: a(λ) = a0 · exp(−S · (λ − λ0))
  # No lower bound on S — matches Python curve_fit behaviour  [FIX 6]
  fit <- try(
    nls(
      abs_coeff ~ a0 * exp(-S * (wavelength - lambda0)),
      data  = sub,
      start = list(S = 0.025)
      # algorithm defaults to Gauss-Newton; no lower bound
    ),
    silent = TRUE
  )
  
  if (inherits(fit, "try-error")) return(NA_real_)
  unname(coef(fit)["S"])
}

process_uvvis_file <- function(file, path_length_m = 0.1) {
  df <- read_uvvis_csv(file)
  
  # Baseline: mean absorbance 700–800 nm (assumed zero signal)
  baseline     <- mean(df$Abs[df$wavelength >= 700 & df$wavelength <= 800],
                       na.rm = TRUE)
  # Napierian absorption coefficient (m^-1)
  df$abs_coeff <- 2.303 * (df$Abs - baseline) / path_length_m
  
  abs_254_coeff <- df$abs_coeff[df$wavelength == 254][1]
  abs_250_coeff <- df$abs_coeff[df$wavelength == 250][1]
  abs_365_coeff <- df$abs_coeff[df$wavelength == 365][1]
  
  # a_254: convert Napierian abs coeff back to decadic absorbance
  #         (divide by 2.303), matching Python: df.loc[254,'abs_coeff']/2.303
  a_254       <- abs_254_coeff / 2.303
  e2_e3       <- abs_250_coeff / abs_365_coeff
  cdom        <- sum(df$abs_coeff[df$wavelength >= 250 & df$wavelength <= 450],
                     na.rm = TRUE)
  
  ss_275_295  <- fit_spectral_slope(df, 275, 295)
  ss_350_400  <- fit_spectral_slope(df, 350, 400)
  slope_ratio <- ss_275_295 / ss_350_400
  
  metrics <- data.frame(
    sample      = tools::file_path_sans_ext(basename(file)),
    a_254       = a_254,
    e2_e3       = e2_e3,
    cdom        = cdom,
    ss_275_295  = ss_275_295,
    ss_350_400  = ss_350_400,
    slope_ratio = slope_ratio,
    stringsAsFactors = FALSE
  )
  
  list(metrics = metrics, spectrum = df, baseline = baseline)
}

# -------------------------
# Batch processing
# -------------------------
allfiles <- list.files(rawdata_dir, pattern = "\\.[cC][sS][vV]$",
                       full.names = TRUE)

if (length(allfiles) == 0) {
  stop(sprintf("No CSV files found in %s", rawdata_dir))
}

results <- lapply(allfiles, process_uvvis_file, path_length_m = path_length_m)
outdf   <- do.call(rbind, lapply(results, `[[`, "metrics"))
rownames(outdf) <- outdf$sample

# -------------------------
# Extract sample_date from filename prefix (mmddyyyy)
# -------------------------
# Expected format: "mmddyyyy_site_name"  e.g. "01142026_vegas_browns_sub"
# NOTE: entry "0312025_vegas_browns_sub" has only 7 leading digits — a known
#       filename typo (missing one digit). These are flagged as NA so they
#       can be reviewed and corrected in the source filenames before re-running.

extract_sample_date <- function(sample_names) {
  # Grab the leading digit block before the first underscore
  prefix <- sub("^(\\d+)_.*$", "\\1", sample_names)
  
  parsed <- as.Date(
    ifelse(
      nchar(prefix) == 8,
      # Standard mmddyyyy → reorder to yyyy-mm-dd for as.Date
      paste0(substr(prefix, 5, 8), "-",   # yyyy
             substr(prefix, 1, 2), "-",   # mm
             substr(prefix, 3, 4)),        # dd
      NA_character_                        # flag malformed prefixes as NA
    ),
    format = "%Y-%m-%d"
  )
  
  # Warn about any entries that could not be parsed
  bad <- sample_names[is.na(parsed)]
  if (length(bad) > 0) {
    warning(sprintf(
      "%d sample name(s) have a malformed date prefix and were set to NA:\n  %s",
      length(bad), paste(bad, collapse = "\n  ")
    ))
  }
  parsed
}

outdf$sample_date <- extract_sample_date(outdf$sample)

# Audit: print rows where date could not be parsed (check/fix source filenames)
na_dates <- outdf[is.na(outdf$sample_date), "sample", drop = FALSE]
if (nrow(na_dates) > 0) {
  message("=== Samples with unparseable date prefixes (check filenames) ===")
  print(na_dates)
}

# -------------------------
# Extract site from sample name (text after "_vegas_")
# -------------------------
# Expected format: "mmddyyyy_vegas_<site>"  e.g. "11232024_vegas_winters_usgs"
#   → site == "winters_usgs"
#                                                "08192025_vegas_ophir"
#   → site == "ophir"
# If "_vegas_" is absent the field is set to NA and a warning is issued,
# so non-standard filenames are caught at runtime rather than silently lost.

extract_site <- function(sample_names) {
  # Return everything after the first occurrence of "_vegas_"
  site <- ifelse(
    grepl("_vegas_", sample_names, fixed = TRUE),
    sub("^.*?_vegas_(.+)$", "\\1", sample_names),
    NA_character_
  )
  
  bad <- sample_names[is.na(site)]
  if (length(bad) > 0) {
    warning(sprintf(
      "%d sample name(s) do not contain '_vegas_' and site was set to NA:\n  %s",
      length(bad), paste(bad, collapse = "\n  ")
    ))
  }
  site
}

outdf$site <- extract_site(outdf$sample)

# Audit: print any rows where site could not be extracted
na_sites <- outdf[is.na(outdf$site), "sample", drop = FALSE]
if (nrow(na_sites) > 0) {
  message("=== Samples with no '_vegas_' token — site set to NA (check filenames) ===")
  print(na_sites)
}

# Quick summary of unique sites detected across all samples
message(sprintf("Sites detected (%d unique): %s",
                length(unique(na.omit(outdf$site))),
                paste(sort(unique(na.omit(outdf$site))), collapse = ", ")))

write.csv(outdf, outfile, row.names = FALSE)
message(sprintf("Processed summary written to: %s", outfile))

# -------------------------
# Site colour palette  (shared by all-spectra and exploratory plots)
# -------------------------
# Derives the ordered set of unique sites from outdf$site so the palette
# is always consistent regardless of how many sites are present.
site_levels  <- sort(unique(na.omit(outdf$site)))   # alphabetical, reproducible
site_palette <- setNames(
  # 6 visually distinct, colour-blind-friendly colours; extend with palette()
  # if more than 6 sites are ever added.
  c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")[
    seq_along(site_levels)],
  site_levels
)
# Helper: look up colour for a site label (returns grey for NA)
site_col <- function(site_vec) {
  cols <- site_palette[site_vec]
  cols[is.na(cols)] <- "grey60"
  unname(cols)
}

# -------------------------
# Plot all corrected spectra coloured by site (notebook cell 2)
# -------------------------
# [FIX 7] y-axis label corrected to 'a absorbance coefficient (m^-1)'
png(filename = file.path(datapath, paste0("all_corrected_spectra_", date_tag, ".png")),
    width = 1000, height = 650)
par(mar = c(5, 5, 2, 10), xpd = FALSE)   # wide right margin for legend
plot(NA,
     xlim = c(180, 520),
     ylim = range(do.call(c, lapply(results,
                                    function(x) x$spectrum$abs_coeff)),
                  na.rm = TRUE),
     xlab = expression(lambda ~ " wavelength (nm)"),
     ylab = expression(a ~ " absorbance coefficient (m"^{-1}*")"))
for (res in results) {
  # Match spectrum back to its site via the sample name stored in metrics
  this_site  <- outdf$site[outdf$sample == res$metrics$sample]
  this_col   <- site_col(this_site)
  lines(res$spectrum$wavelength, res$spectrum$abs_coeff,
        col = this_col, lwd = 1.4)
}
# Legend placed in the right margin
par(xpd = TRUE)
legend(x      = par("usr")[2] * 1.01,   # just outside right axis
       y      = par("usr")[4],
       legend = site_levels,
       col    = site_palette,
       lwd    = 2,
       bty    = "n",
       title  = "Site",
       cex    = 0.85)
par(xpd = FALSE)
dev.off()

# -------------------------
# Example / tutorial plots (notebook cell 4)
# -------------------------
colors    <- c("darkorange", "green")   # index 1 = BROWNSUB, index 2 = OPHIR
suvalist  <- numeric(length(shortlist))
srlist    <- numeric(length(shortlist))

short_res <- lapply(shortlist, function(f) {
  process_uvvis_file(file.path(rawdata_dir, f), path_length_m = path_length_m)
})

# Plot 1: absorption coefficient
png(filename = file.path(datapath, paste0("tutorial_abs_coeff_", date_tag, ".png")),
    width = 900, height = 600)
plot(NA, xlim = c(200, 600), ylim = c(0, 70),
     xlab = "Wavelength (nm)",
     ylab = expression("Absorption coefficient (m"^{-1}*")"))
for (i in seq_along(short_res)) {
  lines(short_res[[i]]$spectrum$wavelength,
        short_res[[i]]$spectrum$abs_coeff,
        col = colors[i], lwd = 2)
}
abline(v = 380, lty = 2)
text(400, 60, "Visible light 380-780 nm ->", adj = 0)
text(240, 60, "Ophir (0% burned)",            col = "green",      adj = 0)
text(240, 53, "Browns subcatchment\n(99% burned)", col = "darkorange", adj = 0)
dev.off()

# Plot 2: specific absorption coefficient + SUVA254
# [FIX 3] suvalist labels now correctly match index: [1]=BROWNSUB, [2]=OPHIR
png(filename = file.path(datapath, paste0("tutorial_specific_abs_coeff_", date_tag, ".png")),
    width = 900, height = 600)
plot(NA, xlim = c(200, 600), ylim = c(0, 10),
     xlab = "Wavelength (nm)",
     ylab = expression("Specific absorption coefficient (L mg-C"^{-1}*" m"^{-1}*")"))
for (i in seq_along(short_res)) {
  spec_abs <- short_res[[i]]$spectrum$abs_coeff / doclist[i]
  lines(short_res[[i]]$spectrum$wavelength, spec_abs, col = colors[i], lwd = 2)
  
  abs_254_coeff <- short_res[[i]]$spectrum$abs_coeff[
    short_res[[i]]$spectrum$wavelength == 254][1]
  tempsuva     <- abs_254_coeff / doclist[i]
  suvalist[i]  <- tempsuva
  
  segments(0, tempsuva, 900, tempsuva, lty = 2, col = colors[i])
  points(254, tempsuva, col = colors[i], pch = 16)
}
# [FIX 3] Labels: index 2 = OPHIR (green), index 1 = BROWNSUB (darkorange)
text(300, 8,
     paste0("Ophir SUVA254 = ", round(suvalist[2], 2), " L mg-C^-1 m^-1"),
     col = "green", adj = 0)
text(300, 6,
     paste0("Browns sub SUVA254 = ", round(suvalist[1], 2), " L mg-C^-1 m^-1"),
     col = "darkorange", adj = 0)
dev.off()

# Plot 3: spectral slopes + fitted segments
png(filename = file.path(datapath, paste0("tutorial_slope_fits_", date_tag, ".png")),
    width = 900, height = 600)
plot(NA, xlim = c(200, 600), ylim = c(-1, 40),
     xlab = "Wavelength (nm)",
     ylab = expression("Absorption coefficient (m"^{-1}*")"))

for (i in seq_along(short_res)) {
  df_s <- short_res[[i]]$spectrum
  lines(df_s$wavelength, df_s$abs_coeff, col = "gray")
  
  s1  <- short_res[[i]]$metrics$ss_275_295
  x1  <- 275:295
  a01 <- df_s$abs_coeff[df_s$wavelength == 275][1]
  y1  <- a01 * exp(-s1 * (x1 - 275))
  lines(x1, y1, col = colors[i], lwd = 3)
  
  s2  <- short_res[[i]]$metrics$ss_350_400
  x2  <- 350:400
  a02 <- df_s$abs_coeff[df_s$wavelength == 350][1]
  y2  <- a02 * exp(-s2 * (x2 - 350))
  lines(x2, y2, col = colors[i], lwd = 3)
  
  srlist[i] <- short_res[[i]]$metrics$slope_ratio
}
text(220, 0,  paste0("Ophir slope ratio = ",     round(srlist[2], 2)),
     col = "green",      adj = 0)
text(300, 20, paste0("Browns sub slope ratio = ", round(srlist[1], 2)),
     col = "darkorange", adj = 0)
dev.off()

# -------------------------
# Exploratory figure coloured by site (notebook cell 5)
# [FIX 8] Use pre-computed outdf$slope_ratio instead of re-dividing columns
# -------------------------
png(filename = file.path(datapath, paste0("exploratory_e2e3_vs_slope_ratio_", date_tag, ".png")),
    width = 900, height = 700)
par(mar = c(5, 5, 2, 10), xpd = FALSE)
plot(outdf$e2_e3, outdf$slope_ratio,
     col  = site_col(outdf$site),
     bg   = site_col(outdf$site),   # filled symbols need bg for pch 21-25
     pch  = 21,
     cex  = 1.4,
     xlab = "E2:E3",
     ylab = "Slope ratio (S275-295 / S350-400)")
par(xpd = TRUE)
legend(x      = par("usr")[2] * 1.01,
       y      = par("usr")[4],
       legend = site_levels,
       pt.bg  = site_palette,
       col    = site_palette,
       pch    = 21,
       pt.cex = 1.4,
       bty    = "n",
       title  = "Site",
       cex    = 0.85)
par(xpd = FALSE)
dev.off()

print(outdf)
