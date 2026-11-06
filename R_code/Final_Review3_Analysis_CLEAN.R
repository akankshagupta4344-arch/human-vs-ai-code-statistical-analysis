# ============================================================
# FINAL CLEAN ANALYSIS SCRIPT
# A Statistical Comparison of Human-Written and AI-Generated Code
# Base R only; no packages required.
#
# HOW TO RUN:
# 1. Open this script in RStudio.
# 2. Click Source.
# 3. Select HumanVsAI_CodeDataset.csv inside Code_Dataset.
# 4. Outputs are saved in the parent project folder.
#
# IMPORTANT:
# Results describe this dataset. Human and AI samples come from
# different collection processes, so an isolated causal "AI effect"
# cannot be established from this dataset alone.
# ============================================================

options(stringsAsFactors = FALSE, scipen = 5)

# STEP 1: LOAD DATA AND SET OUTPUT DIRECTORY --------------------

data_path <- file.choose()
data_path <- normalizePath(data_path, winslash = "/", mustWork = TRUE)
input_dir <- dirname(data_path)

# If the selected CSV is in a folder named Code_Dataset, use its
# parent as the project folder. Otherwise save outputs beside the CSV.
if (basename(input_dir) == "Code_Dataset") {
  project_dir <- dirname(input_dir)
} else {
  project_dir <- input_dir
}
setwd(project_dir)

df <- read.csv(data_path, stringsAsFactors = FALSE, check.names = FALSE)

required <- c("problem_id", "Sample_Code", "Generated", "Language", "Source")
if (!all(required %in% names(df))) {
  stop("CSV missing expected columns: ",
       paste(setdiff(required, names(df)), collapse = ", "))
}

df$Generated <- trimws(df$Generated)
df$Language <- trimws(df$Language)
df$problem_id <- trimws(as.character(df$problem_id))

valid_origins <- c("AI", "Human")
valid_languages <- c("C", "C++", "Java", "Python")
before_n <- nrow(df)

df <- df[
  !is.na(df$Sample_Code) &
    !is.na(df$Generated) &
    !is.na(df$Language) &
    df$Generated %in% valid_origins &
    df$Language %in% valid_languages,
]

df$Generated <- factor(df$Generated, levels = c("Human", "AI"))
df$Language <- factor(df$Language, levels = valid_languages)

cat("\n========== DATASET CHECK ==========\n")
cat("Input file:", data_path, "\n")
cat("Project/output folder:", project_dir, "\n")
cat("Rows in original CSV:", before_n, "\n")
cat("Rows retained:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n")
cat("Missing code samples remaining:", sum(is.na(df$Sample_Code)), "\n")
cat("Exact duplicate rows in original CSV:", sum(duplicated(read.csv(data_path))), "\n")
cat("\nCounts by origin:\n")
print(table(df$Generated))
cat("\nCounts by language:\n")
print(table(df$Language))
cat("\nOrigin x language:\n")
print(table(df$Generated, df$Language))

# STEP 2: MEASUREMENT DEFINITIONS -------------------------------

# Count all source lines, excluding empty trailing lines only.
count_lines <- function(code) {
  if (is.na(code) || !nzchar(code)) return(0L)
  lines <- strsplit(gsub("\r", "", code, fixed = TRUE), "\n", fixed = TRUE)[[1]]
  while (length(lines) > 1L && trimws(tail(lines, 1L)) == "") {
    lines <- head(lines, -1L)
  }
  as.integer(length(lines))
}

# Count non-blank lines; used as denominator for normalized density.
count_nonblank_lines <- function(code) {
  if (is.na(code) || !nzchar(code)) return(0L)
  lines <- strsplit(gsub("\r", "", code, fixed = TRUE), "\n", fixed = TRUE)[[1]]
  as.integer(sum(nzchar(trimws(lines))))
}

# Count characters ONLY on nonblank source lines.
# This excludes CR/LF characters and ignores blank/whitespace-only lines
# in BOTH the numerator and denominator of characters-per-nonblank-line.
# Indentation and trailing spaces on nonblank lines remain included.
count_chars_nonblank_lines <- function(code) {
  if (is.na(code) || !nzchar(code)) return(0L)
  lines <- strsplit(gsub("\r", "", code, fixed = TRUE), "\n", fixed = TRUE)[[1]]
  nonblank <- lines[nzchar(trimws(lines))]
  as.integer(sum(nchar(nonblank)))
}

# Approximate standalone comment-line counter:
# Python: standalone lines beginning with #.
# C/C++/Java: standalone // lines and lines beginning /*, including
# subsequent lines while a block comment is open.
# NOT a full parser: inline comments and comment markers inside strings
# are not reliably recognized.
count_comments_heuristic <- function(code, language) {
  if (is.na(code) || !nzchar(code)) return(0L)
  lines <- strsplit(gsub("\r", "", code, fixed = TRUE), "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines)
  total <- 0L
  in_block <- FALSE

  for (line in lines) {
    if (!nzchar(line)) next

    if (language == "Python") {
      if (grepl("^#", line)) total <- total + 1L
      next
    }

    if (in_block) {
      total <- total + 1L
      if (grepl("\\*/", line)) in_block <- FALSE
      next
    }

    if (grepl("^//", line)) {
      total <- total + 1L
      next
    }

    if (grepl("^/\\*", line)) {
      total <- total + 1L
      if (!grepl("\\*/", line)) in_block <- TRUE
    }
  }
  total
}

df$Line_Count <- vapply(df$Sample_Code, count_lines, integer(1))
df$Nonblank_Line_Count <- vapply(df$Sample_Code, count_nonblank_lines, integer(1))
df$Character_Count <- nchar(df$Sample_Code)
df$Characters_In_Nonblank_Lines <- vapply(
  df$Sample_Code, count_chars_nonblank_lines, integer(1)
)
df$Comment_Line_Count <- mapply(
  count_comments_heuristic, df$Sample_Code, as.character(df$Language)
)
df$Has_Comment <- df$Comment_Line_Count > 0L

# Corrected normalized metric:
# numerator sums characters on nonblank lines only; denominator counts
# those same nonblank lines. Blank-line whitespace and CR/LF are excluded.
df$Characters_Per_Nonblank_Line <- with(
  df,
  ifelse(Nonblank_Line_Count > 0,
         Characters_In_Nonblank_Lines / Nonblank_Line_Count,
         NA_real_)
)
df$Comments_Per_100_Nonblank_Lines <- with(
  df,
  ifelse(Nonblank_Line_Count > 0,
         100 * Comment_Line_Count / Nonblank_Line_Count,
         NA_real_)
)

cat("\n========== MEASUREMENT CHECK ==========\n")
cat("Missing line counts:", sum(is.na(df$Line_Count)), "\n")
cat("Missing character counts:", sum(is.na(df$Character_Count)), "\n")
cat("Missing revised comment counts:", sum(is.na(df$Comment_Line_Count)), "\n")
cat("Samples with at least one heuristic comment:", sum(df$Has_Comment), "\n")
cat("Samples with zero heuristic comments:", sum(!df$Has_Comment), "\n")

# STEP 3: DESCRIPTIVE STATISTICS --------------------------------

describe <- function(x) {
  x <- x[is.finite(x)]
  c(
    N = length(x),
    Mean = mean(x),
    Median = median(x),
    SD = sd(x),
    Minimum = min(x),
    Maximum = max(x)
  )
}

metric_names <- c(
  "Line_Count",
  "Character_Count",
  "Characters_Per_Nonblank_Line",
  "Comment_Line_Count",
  "Comments_Per_100_Nonblank_Lines"
)

overall_descriptives <- do.call(rbind, lapply(metric_names, function(metric) {
  do.call(rbind, lapply(levels(df$Generated), function(origin) {
    x <- df[df$Generated == origin, metric]
    data.frame(Measurement = metric, Origin = origin,
               as.list(describe(x)), check.names = FALSE)
  }))
}))
cat("\n========== OVERALL DESCRIPTIVES ==========\n")
print(overall_descriptives, row.names = FALSE, digits = 5)
write.csv(overall_descriptives, "Final_Descriptive_Overall.csv", row.names = FALSE)

language_descriptives <- do.call(rbind, lapply(metric_names, function(metric) {
  do.call(rbind, lapply(levels(df$Language), function(lang) {
    do.call(rbind, lapply(levels(df$Generated), function(origin) {
      x <- df[df$Language == lang & df$Generated == origin, metric]
      data.frame(
        Measurement = metric, Language = lang, Origin = origin,
        N = sum(is.finite(x)),
        Mean = mean(x, na.rm = TRUE),
        Median = median(x, na.rm = TRUE),
        SD = sd(x, na.rm = TRUE)
      )
    }))
  }))
}))
cat("\n========== LANGUAGE-WISE DESCRIPTIVES SAVED ==========\n")
write.csv(language_descriptives, "Final_Descriptive_By_Language.csv", row.names = FALSE)

# STEP 4: EIGHT PRIMARY WELCH T-TESTS ----------------------------

primary_metrics <- c("Line_Count", "Character_Count")
welch_results <- data.frame()

for (lang in levels(df$Language)) {
  sub <- df[df$Language == lang, ]

  for (metric in primary_metrics) {
    ai <- sub[sub$Generated == "AI", metric]
    human <- sub[sub$Generated == "Human", metric]
    tt <- t.test(ai, human, var.equal = FALSE)

    welch_results <- rbind(welch_results, data.frame(
      Language = lang,
      Measurement = metric,
      N_AI = length(ai),
      N_Human = length(human),
      Mean_AI = mean(ai),
      Mean_Human = mean(human),
      Difference_AI_minus_Human = mean(ai) - mean(human),
      CI95_Lower = unname(tt$conf.int[1]),
      CI95_Upper = unname(tt$conf.int[2]),
      T = unname(tt$statistic),
      DF = unname(tt$parameter),
      P_Value = tt$p.value
    ))
  }
}
welch_results$Holm_P <- p.adjust(welch_results$P_Value, method = "holm")
welch_results$Significant_Holm_005 <- welch_results$Holm_P < 0.05
cat("\n========== WELCH TESTS (HOLM CORRECTED) ==========\n")
print(welch_results, row.names = FALSE, digits = 5)
write.csv(welch_results, "Final_Welch_Tests.csv", row.names = FALSE)

# STEP 5: WILCOXON AND CORRECT CLIFF'S DELTA ---------------------
# In R, wilcox.test(x, y)$statistic is already the Mann-Whitney U
# statistic for the first sample (the minimum rank sum is subtracted).
# Therefore DO NOT subtract n_x*(n_x+1)/2 again.
wilcox_results <- data.frame()

for (lang in levels(df$Language)) {
  sub <- df[df$Language == lang, ]

  for (metric in primary_metrics) {
    ai <- sub[sub$Generated == "AI", metric]
    human <- sub[sub$Generated == "Human", metric]
    wt <- wilcox.test(ai, human, exact = FALSE)

    U_ai <- as.numeric(wt$statistic)
    cliffs_delta <- 2 * U_ai / (length(ai) * length(human)) - 1

    wilcox_results <- rbind(wilcox_results, data.frame(
      Language = lang,
      Measurement = metric,
      N_AI = length(ai),
      N_Human = length(human),
      Median_AI = median(ai),
      Median_Human = median(human),
      Mann_Whitney_U_AI = U_ai,
      Cliffs_Delta_AI_vs_Human = cliffs_delta,
      P_Value = wt$p.value
    ))
  }
}
wilcox_results$Holm_P <- p.adjust(wilcox_results$P_Value, method = "holm")
wilcox_results$Significant_Holm_005 <- wilcox_results$Holm_P < 0.05
# Display extreme small p-values as thresholds rather than literal zero.
wilcox_results$P_Value_Display <- ifelse(
  wilcox_results$P_Value < 2.2e-16, "< 2.2e-16",
  format(wilcox_results$P_Value, scientific = TRUE, digits = 4)
)
wilcox_results$Holm_P_Display <- ifelse(
  wilcox_results$Holm_P < 2.2e-16, "< 2.2e-16",
  format(wilcox_results$Holm_P, scientific = TRUE, digits = 4)
)
cat("\n========== WILCOXON + CORRECTED CLIFF'S DELTA ==========\n")
print(wilcox_results, row.names = FALSE, digits = 5)
write.csv(wilcox_results, "Final_Wilcoxon_CliffsDelta.csv", row.names = FALSE)

# Cohen's d, AI minus Human
cohen_results <- data.frame()
for (lang in levels(df$Language)) {
  sub <- df[df$Language == lang, ]
  for (metric in primary_metrics) {
    ai <- sub[sub$Generated == "AI", metric]
    human <- sub[sub$Generated == "Human", metric]
    n1 <- length(ai); n2 <- length(human)
    pooled_sd <- sqrt(((n1 - 1) * var(ai) + (n2 - 1) * var(human)) / (n1 + n2 - 2))
    d <- if (is.finite(pooled_sd) && pooled_sd > 0) (mean(ai) - mean(human)) / pooled_sd else NA_real_
    cohen_results <- rbind(cohen_results, data.frame(
      Language = lang, Measurement = metric, Cohens_d_AI_minus_Human = d
    ))
  }
}
write.csv(cohen_results, "Final_Cohens_d.csv", row.names = FALSE)

# STEP 6: COMMENT PRESENCE AND COMMENT COUNT ---------------------

comment_count_results <- data.frame()
comment_presence_results <- data.frame()

for (lang in levels(df$Language)) {
  sub <- df[df$Language == lang, ]
  ai <- sub$Comment_Line_Count[sub$Generated == "AI"]
  human <- sub$Comment_Line_Count[sub$Generated == "Human"]
  ai_has <- sum(sub$Has_Comment[sub$Generated == "AI"])
  human_has <- sum(sub$Has_Comment[sub$Generated == "Human"])
  n_ai <- sum(sub$Generated == "AI")
  n_human <- sum(sub$Generated == "Human")

  wt <- wilcox.test(ai, human, exact = FALSE)
  comment_count_results <- rbind(comment_count_results, data.frame(
    Language = lang,
    Mean_AI = mean(ai), Mean_Human = mean(human),
    Median_AI = median(ai), Median_Human = median(human),
    P_Value = wt$p.value
  ))

  pt <- prop.test(c(ai_has, human_has), c(n_ai, n_human), correct = FALSE)
  ci <- pt$conf.int
  comment_presence_results <- rbind(comment_presence_results, data.frame(
    Language = lang,
    N_AI = n_ai,
    AI_With_Comment_Pct = 100 * ai_has / n_ai,
    N_Human = n_human,
    Human_With_Comment_Pct = 100 * human_has / n_human,
    Difference_Points_AI_minus_Human = 100 * (ai_has / n_ai - human_has / n_human),
    CI95_Lower_Points = 100 * ci[1],
    CI95_Upper_Points = 100 * ci[2],
    P_Value = pt$p.value
  ))
}
comment_count_results$Holm_P <- p.adjust(comment_count_results$P_Value, method = "holm")
comment_count_results$Significant_Holm_005 <- comment_count_results$Holm_P < 0.05
comment_presence_results$Holm_P <- p.adjust(comment_presence_results$P_Value, method = "holm")
comment_presence_results$Significant_Holm_005 <- comment_presence_results$Holm_P < 0.05

cat("\n========== COMMENT COUNT TESTS ==========\n")
print(comment_count_results, row.names = FALSE, digits = 5)
cat("\n========== COMMENT PRESENCE TESTS ==========\n")
print(comment_presence_results, row.names = FALSE, digits = 5)

write.csv(comment_count_results, "Final_Comment_Count_Tests.csv", row.names = FALSE)
write.csv(comment_presence_results, "Final_Comment_Presence_Tests.csv", row.names = FALSE)

# STEP 6B: NORMALIZED CHARACTERS PER NONBLANK LINE ----------------
# This is the primary sensitivity-corrected density measure. Wilcoxon
# compares distributions; Cliff's delta gives direction and magnitude.
density_results <- data.frame()
for (lang in levels(df$Language)) {
  sub <- df[df$Language == lang, ]
  ai <- sub$Characters_Per_Nonblank_Line[sub$Generated == "AI"]
  human <- sub$Characters_Per_Nonblank_Line[sub$Generated == "Human"]
  ai <- ai[is.finite(ai)]
  human <- human[is.finite(human)]
  wt <- wilcox.test(ai, human, exact = FALSE)
  U_ai <- as.numeric(wt$statistic) # R returns Mann-Whitney U
  delta <- 2 * U_ai / (length(ai) * length(human)) - 1

  density_results <- rbind(density_results, data.frame(
    Language = lang,
    N_AI = length(ai),
    N_Human = length(human),
    Mean_AI = mean(ai),
    Mean_Human = mean(human),
    Median_AI = median(ai),
    Median_Human = median(human),
    Median_Difference_AI_minus_Human = median(ai) - median(human),
    Cliffs_Delta_AI_vs_Human = delta,
    P_Value = wt$p.value
  ))
}
density_results$Holm_P <- p.adjust(density_results$P_Value, method = "holm")
density_results$Significant_Holm_005 <- density_results$Holm_P < 0.05
cat("\n========== CORRECTED CHARACTERS-PER-NONBLANK-LINE TESTS ==========\n")
print(density_results, row.names = FALSE, digits = 5)
write.csv(density_results, "Final_Corrected_Characters_Per_Line_Tests.csv", row.names = FALSE)

# Export a stratified sample for manual comment-counter validation.
# Annotate Manual_Has_Comment and Manual_Comment_Line_Count in a spreadsheet.
set.seed(42)
validation_rows <- unlist(lapply(levels(df$Language), function(lang) {
  unlist(lapply(levels(df$Generated), function(origin) {
    candidates <- which(df$Language == lang & df$Generated == origin)
    sample(candidates, min(10, length(candidates)))
  }))
}))
comment_validation_sample <- data.frame(
  Row_ID = validation_rows,
  problem_id = df$problem_id[validation_rows],
  Language = as.character(df$Language[validation_rows]),
  Generated = as.character(df$Generated[validation_rows]),
  Source = df$Source[validation_rows],
  Detected_Comment_Lines = df$Comment_Line_Count[validation_rows],
  Manual_Has_Comment = NA,
  Manual_Comment_Line_Count = NA,
  Sample_Code = df$Sample_Code[validation_rows]
)
write.csv(comment_validation_sample, "Final_Comment_Validation_Sample.csv", row.names = FALSE)

# STEP 7: MODULE 2 DISTRIBUTION CHECK FOR COMMENT COUNTS ----------
# Poisson has mean approximately equal to variance. Var/mean far
# above 1 suggests overdispersion; this is a diagnostic, not proof
# that a negative-binomial model is the correct model.
comment_distribution <- do.call(rbind, lapply(levels(df$Language), function(lang) {
  do.call(rbind, lapply(levels(df$Generated), function(origin) {
    x <- df$Comment_Line_Count[df$Language == lang & df$Generated == origin]
    m <- mean(x)
    v <- var(x)
    data.frame(
      Language = lang, Origin = origin, N = length(x),
      Mean = m, Variance = v,
      Variance_to_Mean_Ratio = if (m > 0) v / m else NA_real_,
      Observed_Zero_Probability = mean(x == 0),
      Poisson_Expected_Zero_Probability = exp(-m)
    )
  }))
}))
cat("\n========== COMMENT COUNT DISTRIBUTION CHECK ==========\n")
print(comment_distribution, row.names = FALSE, digits = 5)
write.csv(comment_distribution, "Final_Comment_Distribution_Check.csv", row.names = FALSE)

# STEP 8: MODULE 1/4 SPEARMAN CORRELATION ------------------------

overall_spearman <- cor.test(df$Line_Count, df$Character_Count,
                             method = "spearman", exact = FALSE)
cat("\n========== OVERALL SPEARMAN CORRELATION ==========\n")
print(overall_spearman)

spearman_results <- do.call(rbind, lapply(levels(df$Language), function(lang) {
  sub <- df[df$Language == lang, ]
  ct <- cor.test(sub$Line_Count, sub$Character_Count,
                 method = "spearman", exact = FALSE)
  data.frame(Language = lang, N = nrow(sub),
             Spearman_Rho = unname(ct$estimate), P_Value = ct$p.value)
}))
spearman_results$Holm_P <- p.adjust(spearman_results$P_Value, method = "holm")
spearman_results$Significant_Holm_005 <- spearman_results$Holm_P < 0.05
cat("\n========== SPEARMAN BY LANGUAGE ==========\n")
print(spearman_results, row.names = FALSE, digits = 5)
write.csv(spearman_results, "Final_Spearman_Correlations.csv", row.names = FALSE)

# STEP 9: MODULE 4 MULTIPLE REGRESSION ---------------------------
# The outcome is log(character count + 1). The model tests whether
# origin differences vary by language and whether the line-count
# slope differs by origin. Diagnostic plots are saved separately.
df$Generated_Factor <- factor(df$Generated, levels = c("Human", "AI"))
df$Log_Characters <- log1p(df$Character_Count)
df$Log_Lines <- log1p(df$Line_Count)

reg_base <- lm(Log_Characters ~ Log_Lines + Language + Generated_Factor, data = df)
reg_language_origin <- lm(Log_Characters ~ Log_Lines + Language * Generated_Factor, data = df)
reg_full <- lm(Log_Characters ~ Log_Lines * Generated_Factor + Language * Generated_Factor, data = df)

cat("\n========== REGRESSION: ORIGIN x LANGUAGE ==========\n")
reg_language_comparison <- anova(reg_base, reg_language_origin)
print(reg_language_comparison)
cat("\n========== REGRESSION: LINE SLOPE x ORIGIN ==========\n")
reg_slope_comparison <- anova(reg_language_origin, reg_full)
print(reg_slope_comparison)
cat("\n========== FINAL REGRESSION MODEL ==========\n")
print(summary(reg_full))
print(confint(reg_full))

write.csv(as.data.frame(reg_language_comparison),
          "Final_Regression_Language_Origin_Comparison.csv")
write.csv(as.data.frame(reg_slope_comparison),
          "Final_Regression_Slope_Comparison.csv")
reg_coef <- as.data.frame(summary(reg_full)$coefficients)
reg_coef$Term <- rownames(reg_coef)
rownames(reg_coef) <- NULL
write.csv(reg_coef, "Final_Regression_Coefficients.csv", row.names = FALSE)

png("Final_Regression_Diagnostics.png", width = 1600, height = 1200, res = 150)
par(mfrow = c(2, 2))
plot(reg_full)
dev.off()

# STEP 10: MODULE 5 FACTORIAL ANOVA, EACH EFFECT TESTED -----------
# Nested-model comparisons explicitly test language, origin and
# interaction. Outcome metrics are log1p transformed due to skew.
compare_nested <- function(reduced, full, outcome, term) {
  cmp <- anova(reduced, full)
  effect_ss <- cmp$`Sum of Sq`[2]
  data.frame(
    Outcome = outcome,
    Term = term,
    DF_effect = cmp$Df[2],
    DF_error = df.residual(full),
    F_value = cmp$F[2],
    P_value = cmp$`Pr(>F)`[2],
    Partial_Eta_Squared = effect_ss / (effect_ss + deviance(full))
  )
}

anova_results <- data.frame()
for (metric in c("Line_Count", "Character_Count")) {
  df$Log_Response <- log1p(df[[metric]])
  model_language <- lm(Log_Response ~ Language, data = df)
  model_origin <- lm(Log_Response ~ Generated_Factor, data = df)
  model_additive <- lm(Log_Response ~ Language + Generated_Factor, data = df)
  model_interaction <- lm(Log_Response ~ Language * Generated_Factor, data = df)

  anova_results <- rbind(
    anova_results,
    compare_nested(model_origin, model_additive, metric, "Language adjusted for origin"),
    compare_nested(model_language, model_additive, metric, "Origin adjusted for language"),
    compare_nested(model_additive, model_interaction, metric, "Language x origin interaction")
  )
}
anova_results$Holm_P_within_outcome <- NA_real_
for (outcome in unique(anova_results$Outcome)) {
  idx <- which(anova_results$Outcome == outcome)
  anova_results$Holm_P_within_outcome[idx] <- p.adjust(anova_results$P_value[idx], method = "holm")
}
cat("\n========== FACTORIAL ANOVA: ALL EFFECTS ==========\n")
print(anova_results, row.names = FALSE, digits = 5)
write.csv(anova_results, "Final_ANOVA_All_Effects.csv", row.names = FALSE)

# STEP 11: PROBLEM-ID DEPENDENCE DIAGNOSTIC ----------------------
# Describes overlap only; it does not prove observations independent.
id_origin <- aggregate(Generated ~ problem_id, data = df,
                       FUN = function(x) length(unique(x)))
id_language <- aggregate(Generated ~ problem_id + Language, data = df,
                         FUN = function(x) length(unique(x)))
id_summary <- data.frame(
  Problem_IDs_With_One_Origin = sum(id_origin$Generated == 1),
  Problem_IDs_With_Both_Origins = sum(id_origin$Generated == 2),
  Problem_ID_Language_Groups_With_Both_Origins = sum(id_language$Generated == 2)
)
cat("\n========== PROBLEM-ID CHECK ==========\n")
print(id_summary)
write.csv(id_summary, "Final_Problem_ID_Diagnostic.csv", row.names = FALSE)

# STEP 12: CHI-SQUARE COMPOSITION CHECK --------------------------
# This tests the dataset's origin/language distribution. It should
# not be interpreted as evidence of causal authorship effects.
origin_language_table <- table(df$Generated, df$Language)
chi_result <- chisq.test(origin_language_table)
cat("\n========== ORIGIN x LANGUAGE CHI-SQUARE ==========\n")
print(origin_language_table)
print(chi_result)
capture.output(chi_result, file = "Final_Chi_Square_Output.txt")

# STEP 13: PLOTS -------------------------------------------------
png("Final_Line_Count_Boxplots.png", width = 1600, height = 1100, res = 150)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
for (lang in levels(df$Language)) {
  sub <- df[df$Language == lang, ]
  boxplot(Line_Count ~ Generated, data = sub,
          main = paste(lang, "- Line Count"),
          xlab = "Code Origin", ylab = "Number of Lines")
}
dev.off()

png("Final_Character_Count_Boxplots.png", width = 1600, height = 1100, res = 150)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
for (lang in levels(df$Language)) {
  sub <- df[df$Language == lang, ]
  boxplot(Character_Count ~ Generated, data = sub,
          main = paste(lang, "- Character Count"),
          xlab = "Code Origin", ylab = "Number of Characters")
}
dev.off()

png("Final_Corrected_Characters_Per_Line.png", width = 1600, height = 1100, res = 150)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
for (lang in levels(df$Language)) {
  sub <- df[df$Language == lang, ]
  boxplot(Characters_Per_Nonblank_Line ~ Generated, data = sub,
          main = paste(lang, "- Characters per Nonblank Line"),
          xlab = "Code Origin", ylab = "Characters / nonblank line")
}
dev.off()

png("Final_Distribution_Histograms.png", width = 1600, height = 1100, res = 150)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
hist(df$Line_Count[df$Generated == "AI"], breaks = 30,
     main = "AI: Line Count", xlab = "Lines")
hist(df$Line_Count[df$Generated == "Human"], breaks = 30,
     main = "Human: Line Count", xlab = "Lines")
hist(df$Character_Count[df$Generated == "AI"], breaks = 30,
     main = "AI: Character Count", xlab = "Characters")
hist(df$Character_Count[df$Generated == "Human"], breaks = 30,
     main = "Human: Character Count", xlab = "Characters")
dev.off()

# STEP 14: BRIEF RUN SUMMARY ------------------------------------
summary_lines <- c(
  "FINAL PROJECT ANALYSIS SUMMARY",
  paste("Input dataset:", data_path),
  paste("Rows analysed:", nrow(df)),
  paste("AI samples:", sum(df$Generated == "AI")),
  paste("Human samples:", sum(df$Generated == "Human")),
  paste("Overall Spearman rho:", round(unname(overall_spearman$estimate), 4)),
  paste("Welch tests significant after Holm:", sum(welch_results$Significant_Holm_005), "of", nrow(welch_results)),
  paste("Comment presence tests significant after Holm:", sum(comment_presence_results$Significant_Holm_005), "of", nrow(comment_presence_results)),
  "",
  "Interpretation guardrails:",
  "- Findings describe this dataset and do not isolate causal AI authorship effects.",
  "- Characters per nonblank line sums characters only on nonblank lines; CR/LF and blank-line whitespace are excluded.",
  "- Comment measures are heuristic; Final_Comment_Validation_Sample.csv supports manual checking of 80 stratified records.",
  "- Regression diagnostics should be inspected; conventional inference may be sensitive to non-normality/unequal variance.",
  "- Problem-ID overlap checks do not prove independence.",
  "- Do not claim code quality, correctness or authorship from these measurements."
)
writeLines(summary_lines, "Final_Analysis_Summary.txt")

cat("\n========== ANALYSIS COMPLETE ==========\n")
cat("All outputs saved to:\n", project_dir, "\n")
cat("Key files:\n")
print(list.files(project_dir, pattern = "^Final_"))
