install.packages("tidyverse")
library(ggplot2)
library(scales)

# Analyzing cadences in Bach's 371 chorales (Tidyverse version)
base_url <- "https://raw.githubusercontent.com/craigsapp/bach-371-chorales/master/kern"
cache_dir <- "kern_cache"
dir.create(cache_dir, showWarnings = FALSE)

# Local caching helper
fetch_chorale <- function(i) {
  f <- file.path(cache_dir, sprintf("chor%03d.krn", i))
  if (!file.exists(f)) {
    tryCatch(download.file(sprintf("%s/chor%03d.krn", base_url, i), f, quiet = TRUE),
             error = function(e) if (file.exists(f)) unlink(f))
  }
  if (file.exists(f)) read_lines(f) else NULL
}

# Pitch mappings
LETTERS_SEQ <- c("A", "B", "C", "D", "E", "F", "G")
PC_MAP <- c(A = 9, B = 11, C = 0, D = 2, E = 4, F = 5, G = 7)

maj_sharp <- c("C","G","D","A","E","B","F#","C#"); min_sharp <- c("A","E","B","F#","C#","G#","D#","A#")
maj_flat  <- c("C","F","B-","E-","A-","D-","G-","C-"); min_flat  <- c("A","D","G","C","F","B-","E-","A-")

parse_pitch <- function(tok) {
  sub_tok <- str_split(tok, " ")[[1]][1]
  match <- str_match(sub_tok, "([a-gA-G]+)([#n-]*)")
  if (is.na(match[1, 1])) return(NULL)

  let <- str_to_upper(str_sub(match[1, 2], 1, 1))
  acc <- match[1, 3]
  
  pc <- (PC_MAP[[let]] + str_count(acc, "#") - str_count(acc, "-")) %% 12
  list(letter = let, pc = pc, is_chord = str_detect(tok, " "))
}

deg_from <- function(let, tonic_let) {
  ((match(let, LETTERS_SEQ) - match(tonic_let, LETTERS_SEQ)) %% 7) + 1
}

parse_key_str <- function(s) {
  let <- str_to_upper(str_sub(s, 1, 1))
  acc <- str_sub(s, 2)
  pc <- (PC_MAP[[let]] + str_count(acc, "#") - str_count(acc, "-")) %% 12
  list(letter = let, pc = pc)
}

# Core file parser
parse_chorale <- function(lines, id) {
  res <- tibble(
    id = id, title = NA_character_, tonic = NA_character_, mode = NA_character_,
    key_method = NA_character_, final_sop_degree = NA_integer_,
    sop_matches_tonic_pc = NA_integer_, flag = ""
  )
  phrases <- tibble(id = integer(), degree = integer())
  done <- function() list(summary = res, phrases = phrases)

  if (is.null(lines) || !any(str_detect(lines, "^\\*\\*kern"))) {
    res$flag <- "format_error"; return(done())
  }
  if (any(str_detect(lines, fixed("*^")))) {
    res$flag <- "spine_split"; return(done())
  }

  ttl <- str_subset(lines, "^!!!OTL")
  if (length(ttl) > 0) res$title <- str_replace(ttl[1], "^[^:]*:\\s*", "")

  hdr_i <- which(str_detect(lines, "^\\*\\*"))[1]
  hdr <- str_split(lines[hdr_i], "\t")[[1]]
  k_cols <- which(hdr == "**kern")
  if (length(k_cols) < 4) res$flag <- "non_standard_voices"
  bass_col <- k_cols[1]
  sop_col  <- k_cols[length(k_cols)]

  body <- lines[(hdr_i + 1):length(lines)]
  tokens <- str_split(body, "\t")
  data_toks <- tokens[!str_detect(body, "^[!*=]") & nchar(body) > 0]

  # Find key metadata tokens
  key_tok <- tokens %>% 
    map(~ str_subset(.x, "^\\*[A-Ga-g][#n-]*:$")) %>% 
    compact() %>% 
    map_chr(1, .default = NA_character_) %>% 
    pluck(1, .default = NA_character_)

  key_sig <- tokens %>% 
    map(~ str_subset(.x, "^\\*k\\[[^]]*\\]$")) %>% 
    compact() %>% 
    map_chr(1, .default = NA_character_) %>% 
    pluck(1, .default = NA_character_)

  if (is.na(key_sig)) { res$flag <- "missing_key_sig"; return(done()) }
  key_sig <- str_replace(key_sig, "^\\*k\\[([^]]*)\\]$", "\\1")

  n_sh <- str_count(key_sig, "#")
  n_fl <- str_count(key_sig, "-")
  majt <- if (n_fl > 0) maj_flat[n_fl + 1] else maj_sharp[n_sh + 1]
  mint <- if (n_fl > 0) min_flat[n_fl + 1] else min_sharp[n_sh + 1]

  last_pitch <- function(col) {
    for (i in rev(seq_along(data_toks))) {
      tk <- data_toks[[i]][col]
      if (!is.na(tk)) { p <- parse_pitch(tk); if (!is.null(p)) return(p) }
    }
    NULL
  }

  bp <- last_pitch(bass_col); maj_i <- parse_key_str(majt); min_i <- parse_key_str(mint)
  if (!is.null(bp) && bp$pc == maj_i$pc) {
    t_info <- maj_i; res$mode <- "major"; res$key_method <- "ks+bass"; res$tonic <- majt
  } else if (!is.null(bp) && bp$pc == min_i$pc) {
    t_info <- min_i; res$mode <- "minor"; res$key_method <- "ks+bass"; res$tonic <- mint
  } else if (!is.na(key_tok)) {
    raw_k <- str_replace_all(key_tok, "[\\*:]", "")
    res$mode <- if (str_detect(raw_k, "^[A-G]")) "major" else "minor"
    t_info <- parse_key_str(raw_k); res$tonic <- str_to_upper(str_sub(raw_k, 1, 1))
    res$key_method <- "encoded_fallback"
  } else { res$flag <- "undetermined_key"; return(done()) }

  if (res$key_method == "ks+bass" && !is.na(key_tok)) {
    raw_k <- str_replace_all(key_tok, "[\\*:]", "")
    if (parse_key_str(raw_k)$pc != t_info$pc) res$flag <- str_c(res$flag, ";key_mismatch")
  }

  sp <- last_pitch(sop_col)
  if (is.null(sp)) { res$flag <- "missing_soprano"; return(done()) }
  if (sp$is_chord) res$flag <- str_c(res$flag, ";chord_at_cadence")

  res$final_sop_degree <- deg_from(sp$letter, t_info$letter)
  res$sop_matches_tonic_pc <- as.integer(sp$pc == t_info$pc)


  phrases <- bind_rows(phrases, phrase_rows)
  done()
}

# --- Batch Run ---
message("Parsing 371 chorales...")
parsed <- map(1:371, ~ parse_chorale(fetch_chorale(.x), .x))

chorales <- map_dfr(parsed, "summary")
phrases  <- map_dfr(parsed, "phrases")

write_csv(chorales, "chorale_cadences.csv")
write_csv(phrases, "phrase_endings.csv")

flagged <- chorales %>% 
  filter(nchar(flag) > 0 | key_method == "encoded_fallback")

if (nrow(flagged) > 0) {
  print(flagged %>% select(id, tonic, mode, key_method, final_sop_degree, flag))
}

# --- Data Cleaning & Inference ---
exclude <- c(150)

df <- chorales %>% 
  filter(!id %in% exclude, flag == "", !is.na(final_sop_degree)) %>% 
  mutate(success = as.integer(final_sop_degree == 1))

stats <- df %>% 
  summarize(
    n = n(),
    x = sum(success),
    p_hat = mean(success),
    se = sqrt(p_hat * (1 - p_hat) / n)
  )

cat(sprintf("\nN: %d | Final degree 1 count: %d | p_hat: %.4f | SE: %.4f\n", 
            stats$n, stats$x, stats$p_hat, stats$se))

# Confidence intervals
z_crit <- qnorm(0.975)
cp_ci <- as.numeric(binom.test(stats$x, stats$n)$conf.int)
wald_ci <- stats$p_hat + c(-1, 1) * z_crit * stats$se

denom <- 1 + z_crit^2 / stats$n
center <- (stats$p_hat + z_crit^2 / (2 * stats$n)) / denom
margin <- z_crit * sqrt(stats$p_hat * (1 - stats$p_hat) / stats$n + z_crit^2 / (4 * stats$n^2)) / denom
wilson_ci <- c(center - margin, center + margin)

cat(sprintf("\nClopper-Pearson: [%.4f, %.4f]\nWald:            [%.4f, %.4f]\nWilson:          [%.4f, %.4f]\n",
            cp_ci[1], cp_ci[2], wald_ci[1], wald_ci[2], wilson_ci[1], wilson_ci[2]))

# Hypothesis testing
bt <- binom.test(stats$x, stats$n, p = 0.90)
z_stat <- (stats$p_hat - 0.90) / sqrt(0.9 * 0.1 / stats$n)
cat(sprintf("\nExact Binomial p-val: %.4e | Z-stat p-val: %.4e (z = %.3f)\n", 
            bt$p.value, 2 * pnorm(-abs(z_stat)), z_stat))

# Phrase-level check
ph_summary <- phrases %>% 
  filter(id %in% df$id, !is.na(degree)) %>% 
  summarize(
    n = n(),
    p_hat = mean(degree == 1),
    ci_low = binom.test(sum(degree == 1), n)$conf.int[1],
    ci_high = binom.test(sum(degree == 1), n)$conf.int[2]
  )

cat(sprintf("\nPhrases N: %d | p_hat: %.4f | CI: [%.4f, %.4f]\n", 
            ph_summary$n, ph_summary$p_hat, ph_summary$ci_low, ph_summary$ci_high))


# Combine final and intermediate phrase cadences into one tidy dataset
plot_data <- bind_rows(
  df %>% 
    select(degree = final_sop_degree) %>% 
    mutate(cadence_type = "Final Cadences"),
  
  phrases %>% 
    filter(id %in% df$id, !is.na(degree)) %>% 
    select(degree) %>% 
    mutate(cadence_type = "Intermediate Phrases (Fermatas)")
) %>% 
  filter(!is.na(degree), degree %in% 1:7) %>% 
  mutate(degree = factor(degree, levels = 1:7))

# Plot scale degree percentages side-by-side
ggplot(plot_data, aes(x = degree, fill = cadence_type)) +
  geom_bar(
    aes(y = after_stat(prop), group = cadence_type), 
    position = position_dodge(width = 0.8), 
    width = 0.7
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Final Cadences" = "#1f77b4", "Intermediate Phrases (Fermatas)" = "#ff7f0e")) +
  labs(
    title = "Soprano Cadence Degrees in Bach Chorales",
    subtitle = "Final piece cadences overwhelming favor scale degree 1 compared to interior phrases",
    x = "Scale Degree",
    y = "Percentage of Cadences",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text = element_text(size = 11)
  )

# Save high-res PNG output
ggsave("chorale_cadence_distribution.png", width = 8, height = 5, dpi = 300)
