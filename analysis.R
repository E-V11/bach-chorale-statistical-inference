library(tidyverse)
library(scales)

base_url <- "https://raw.githubusercontent.com/craigsapp/bach-371-chorales/master/kern"
dir.create("kern_cache", showWarnings = FALSE)

LET <- c("A","B","C","D","E","F","G")
PC  <- c(A=9, B=11, C=0, D=2, E=4, F=5, G=7)
MAJ_SH <- c("C","G","D","A","E","B","F#","C#"); MIN_SH <- c("A","E","B","F#","C#","G#","D#","A#")
MAJ_FL <- c("C","F","B-","E-","A-","D-","G-","C-"); MIN_FL <- c("A","D","G","C","F","B-","E-","A-")

fetch <- function(i) {
  f <- sprintf("kern_cache/chor%03d.krn", i)
  if (!file.exists(f))
    try(download.file(sprintf("%s/chor%03d.krn", base_url, i), f, quiet = TRUE), silent = TRUE)
  if (file.exists(f)) read_lines(f) else NULL
}

pitch <- function(tok) {
  if (is.na(tok)) return(NULL)
  m <- str_match(sub(" .*", "", tok), "([a-gA-G])[a-gA-G]*([#n-]*)")
  if (is.na(m[1])) return(NULL)
  l <- str_to_upper(m[2])
  list(letter = l, chord = str_detect(tok, " "),
       pc = (PC[[l]] + str_count(m[3], "#") - str_count(m[3], "-")) %% 12)
}

key_info <- function(s) {
  l <- str_to_upper(str_sub(s, 1, 1))
  list(letter = l, pc = (PC[[l]] + str_count(s, "#") - str_count(s, "-")) %% 12)
}

degree <- function(l, tonic) ((match(l, LET) - match(tonic, LET)) %% 7) + 1

parse_chorale <- function(lines, id) {
  out <- function(flag = "", tonic = NA, mode = NA, method = NA, deg = NA, ph = integer())
    tibble(id = id, tonic = tonic, mode = mode, key_method = method,
           final_degree = deg, flag = flag, phrases = list(ph))

  if (is.null(lines) || !any(str_detect(lines, "^\\*\\*kern"))) return(out("format_error"))
  if (any(str_detect(lines, fixed("*^")))) return(out("spine_split"))

  h <- which(str_detect(lines, "^\\*\\*"))[1]
  cols <- which(str_split(lines[h], "\t")[[1]] == "**kern")
  flag <- if (length(cols) < 4) "non_standard_voices" else ""
  bass <- cols[1]; sop <- cols[length(cols)]

  body <- lines[-seq_len(h)]
  all_toks  <- str_split(body, "\t")
  data_toks <- all_toks[!str_detect(body, "^[!*=]") & nchar(body) > 0]
  flat <- unlist(all_toks)
  ks <- str_subset(flat, "^\\*k\\[[^]]*\\]$")[1]
  kt <- str_subset(flat, "^\\*[A-Ga-g][#n-]*:$")[1]
  if (is.na(ks)) return(out("missing_key_sig"))

  nf <- str_count(ks, "-"); ns <- str_count(ks, "#")
  majt <- if (nf > 0) MAJ_FL[nf + 1] else MAJ_SH[ns + 1]
  mint <- if (nf > 0) MIN_FL[nf + 1] else MIN_SH[ns + 1]

  last_pitch <- function(col) {
    for (k in rev(seq_along(data_toks))) {
      p <- pitch(data_toks[[k]][col]); if (!is.null(p)) return(p)
    }
    NULL
  }

  b <- last_pitch(bass); kmaj <- key_info(majt); kmin <- key_info(mint)
  if (!is.null(b) && b$pc == kmaj$pc) {
    key <- kmaj; tonic <- majt; mode <- "major"; meth <- "ks+bass"
  } else if (!is.null(b) && b$pc == kmin$pc) {
    key <- kmin; tonic <- mint; mode <- "minor"; meth <- "ks+bass"
  } else if (!is.na(kt)) {
    raw <- str_remove_all(kt, "[*:]")
    key <- key_info(raw); tonic <- str_to_upper(str_sub(raw, 1, 1))
    mode <- if (str_detect(raw, "^[A-G]")) "major" else "minor"; meth <- "encoded_fallback"
  } else return(out("undetermined_key"))

  if (meth == "ks+bass" && !is.na(kt) && key_info(str_remove_all(kt, "[*:]"))$pc != key$pc)
    flag <- str_c(flag, ";key_mismatch")

  s <- last_pitch(sop)
  if (is.null(s)) return(out("missing_soprano"))
  if (s$chord) flag <- str_c(flag, ";chord_at_cadence")

  fer <- map_chr(data_toks, \(x) x[sop])
  fer <- fer[!is.na(fer) & str_detect(fer, ";")]
  ph  <- map_int(compact(map(fer, pitch)), \(p) degree(p$letter, key$letter))

  out(flag, tonic, mode, meth, degree(s$letter, key$letter), ph)
}

res <- map_dfr(1:371, \(i) parse_chorale(fetch(i), i))
write_csv(select(res, -phrases), "chorale_cadences.csv")
print(filter(res, flag != "" | key_method == "encoded_fallback") |> select(-phrases))

df <- filter(res, flag == "", !id %in% c(150), !is.na(final_degree))
n <- nrow(df); x <- sum(df$final_degree == 1); p <- x / n
se <- sqrt(p * (1 - p) / n)
ci <- p + c(-1, 1) * qnorm(0.975) * se
z  <- (p - 0.90) / sqrt(0.9 * 0.1 / n)
ph <- unlist(df$phrases)

cat(sprintf("n = %d | X = %d | p_hat = %.4f | SE = %.4f\nWald 95%% CI: [%.4f, %.4f]\nz = %.3f | one-sided p = %.4f\nPhrases: n = %d | p_hat = %.4f\n",
            n, x, p, se, ci[1], ci[2], z, pnorm(z, lower.tail = FALSE), length(ph), mean(ph == 1)))

plot_df <- bind_rows(tibble(degree = df$final_degree, type = "Final cadences"),
                     tibble(degree = ph, type = "Interior phrases (fermatas)")) |>
  filter(degree %in% 1:7) |> mutate(degree = factor(degree, 1:7))

ggplot(plot_df, aes(degree, fill = type)) +
  geom_bar(aes(y = after_stat(prop), group = type), position = position_dodge(0.8), width = 0.7) +
  scale_y_continuous(labels = percent) +
  labs(title = "Soprano Cadence Degrees in Bach Chorales",
       x = "Scale Degree", y = "Percentage of Cadences", fill = NULL) +
  theme_minimal(base_size = 12) + theme(legend.position = "top")

ggsave("chorale_cadence_distribution.png", width = 8, height = 5, dpi = 300)
