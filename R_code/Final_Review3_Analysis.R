
# ============================================================
# FINAL REVIEW 3
# A Statistical Comparison of Human-Written and AI-Generated Code
# ============================================================

# STEP 1: LOAD DATA
setwd("C:/Users/akank/OneDrive/Desktop/pasproj")

data <- read.csv(
  "C:/Users/akank/OneDrive/Desktop/pasproj/Code_Dataset/HumanVsAI_CodeDataset.csv",
  stringsAsFactors = FALSE
)

required <- c("problem_id", "Sample_Code", "Generated", "Language", "Source")
if (!all(required %in% names(data))) {
  stop("The selected CSV does not have the expected columns.")
}

data$Generated <- trimws(data$Generated)
data$Language <- trimws(data$Language)

cat("\n========== DATASET CHECK ==========\n")
cat("Rows:", nrow(data), "\n")
cat("Columns:", ncol(data), "\n")
cat("Missing code samples:", sum(is.na(data$Sample_Code)), "\n")
cat("Exact duplicate rows:", sum(duplicated(data)), "\n")
print(table(data$Generated))
print(table(data$Language))
print(table(data$Generated, data$Language))

# Remove rows without code or required group labels, if any
data <- data[
  !is.na(data$Sample_Code) &
    data$Generated %in% c("AI", "Human") &
    data$Language %in% c("C", "C++", "Java", "Python"),
]

# STEP 2: VALIDATE AND CALCULATE MEASUREMENTS

# Count lines, ignoring empty lines at the very end of a file.
count_lines <- function(code) {
  if (is.na(code) || !nzchar(code)) return(0L)
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  while (length(lines) > 1 && tail(lines, 1) == "") {
    lines <- head(lines, -1)
  }
  as.integer(length(lines))
}

data$Line_Count <- vapply(
  data$Sample_Code, count_lines, integer(1)
)

# Character count includes whitespace and newline characters.
data$Character_Count <- nchar(data$Sample_Code)

# Approximate full-line comment count.
# Python uses #; C, C++, and Java use // or /* at line start.
# This is a heuristic, not a complete programming-language parser.
count_comments <- function(code, language) {
  if (is.na(code) || !nzchar(code)) return(0L)
  
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  
  if (language == "Python") {
    sum(grepl("^#", lines))
  } else {
    sum(grepl("^(//|/\\*)", lines))
  }
}

data$Comment_Line_Count <- mapply(
  count_comments, data$Sample_Code, data$Language
)

cat("\n========== MEASUREMENT CHECK ==========\n")
cat("Line-count missing values:", sum(is.na(data$Line_Count)), "\n")
cat("Character-count missing values:", sum(is.na(data$Character_Count)), "\n")
cat("Comment-count missing values:", sum(is.na(data$Comment_Line_Count)), "\n")
cat("Samples with zero counted comments:",
    sum(data$Comment_Line_Count == 0), "\n")

# STEP 3: DESCRIPTIVE STATISTICS
describe <- function(x) {
  c(
    N = length(x),
    Mean = mean(x),
    Median = median(x),
    SD = sd(x),
    Minimum = min(x),
    Maximum = max(x)
  )
}

cat("\n========== OVERALL DESCRIPTIVE STATISTICS ==========\n")
print(aggregate(
  cbind(Line_Count, Character_Count, Comment_Line_Count) ~ Generated,
  data = data,
  FUN = function(x) mean(x)
))

for (variable in c("Line_Count", "Character_Count",
                   "Comment_Line_Count")) {
  cat("\n---", variable, "by origin ---\n")
  print(do.call(rbind, lapply(split(data[[variable]], data$Generated),
                              describe)))
}

cat("\n========== LANGUAGE-WISE DESCRIPTIVE STATISTICS ==========\n")
for (variable in c("Line_Count", "Character_Count",
                   "Comment_Line_Count")) {
  cat("\n---", variable, "---\n")
  print(aggregate(
    data[[variable]],
    by = list(Language = data$Language, Origin = data$Generated),
    FUN = function(x) c(
      Mean = mean(x), Median = median(x), SD = sd(x)
    )
  ))
}

# STEP 4: EIGHT WELCH T-TESTS AND COHEN'S d
variables <- c("Line_Count", "Character_Count")
languages <- c("C", "C++", "Java", "Python")

test_results <- data.frame()
wilcox_results <- data.frame()
effect_results <- data.frame()

for (lang in languages) {
  subset_data <- data[data$Language == lang, ]
  
  for (variable in variables) {
    ai <- subset_data[subset_data$Generated == "AI", variable]
    human <- subset_data[subset_data$Generated == "Human", variable]
    
    # Welch's independent-samples t-test
    tt <- t.test(ai, human, var.equal = FALSE)
    
    test_results <- rbind(test_results, data.frame(
      Language = lang,
      Measurement = variable,
      AI_Mean = mean(ai),
      Human_Mean = mean(human),
      Difference_AI_minus_Human = mean(ai) - mean(human),
      CI_Lower = tt$conf.int[1],
      CI_Upper = tt$conf.int[2],
      T_Statistic = unname(tt$statistic),
      DF = unname(tt$parameter),
      P_Value = tt$p.value
    ))
    
    # Cohen's d using pooled standard deviation
    pooled_sd <- sqrt(
      ((length(ai) - 1) * var(ai) +
         (length(human) - 1) * var(human)) /
        (length(ai) + length(human) - 2)
    )
    
    d <- if (pooled_sd == 0) NA_real_ else
      (mean(ai) - mean(human)) / pooled_sd
    
    effect_results <- rbind(effect_results, data.frame(
      Language = lang,
      Measurement = variable,
      Cohens_d = d
    ))
    
    # Wilcoxon rank-sum robustness check
    wt <- wilcox.test(ai, human, exact = FALSE)
    
    wilcox_results <- rbind(wilcox_results, data.frame(
      Language = lang,
      Measurement = variable,
      P_Value = wt$p.value
    ))
  }
}

test_results$Holm_P <- p.adjust(test_results$P_Value, method = "holm")
test_results$Significant <- test_results$Holm_P < 0.05

wilcox_results$Holm_P <- p.adjust(wilcox_results$P_Value, method = "holm")
wilcox_results$Significant <- wilcox_results$Holm_P < 0.05

cat("\n========== WELCH T-TESTS ==========\n")
print(test_results, digits = 5, row.names = FALSE)

cat("\n========== COHEN'S D ==========\n")
print(effect_results, digits = 4, row.names = FALSE)

cat("\n========== WILCOXON TESTS ==========\n")
print(wilcox_results, digits = 5, row.names = FALSE)

cat("\nNumber of significant Welch comparisons after Holm correction:",
    sum(test_results$Significant), "out of",
    nrow(test_results), "\n")

# STEP 5: COMMENT COUNT COMPARISON
cat("\n========== COMMENT COUNT COMPARISON ==========\n")

comment_ai <- data$Comment_Line_Count[data$Generated == "AI"]
comment_human <- data$Comment_Line_Count[data$Generated == "Human"]

print(t.test(comment_ai, comment_human, var.equal = FALSE))
print(wilcox.test(comment_ai, comment_human, exact = FALSE))

# Also show comment counts separately by language
print(aggregate(
  Comment_Line_Count ~ Language + Generated,
  data = data,
  FUN = function(x) c(Mean = mean(x), Median = median(x), SD = sd(x))
))

# STEP 6: CHI-SQUARE TEST: ORIGIN AND LANGUAGE
cat("\n========== ORIGIN-LANGUAGE ASSOCIATION ==========\n")
origin_language_table <- table(data$Generated, data$Language)
print(origin_language_table)

chi_result <- chisq.test(origin_language_table)
print(chi_result)

# STEP 7: INVESTIGATE REPEATED PROBLEM IDs
cat("\n========== PROBLEM ID CHECK ==========\n")

id_origin <- aggregate(
  Generated ~ problem_id,
  data = data,
  FUN = function(x) length(unique(x))
)

cat("Number of problem IDs with only one origin:",
    sum(id_origin$Generated == 1), "\n")
cat("Number of problem IDs with both origins:",
    sum(id_origin$Generated == 2), "\n")

id_language <- aggregate(
  Generated ~ problem_id + Language,
  data = data,
  FUN = function(x) length(unique(x))
)

cat("Problem-ID/language groups with both origins:",
    sum(id_language$Generated == 2), "\n")

# STEP 8: LANGUAGE AND ORIGIN TOGETHER
# Log transformation reduces the influence of very large counts.
# Treat this as an exploratory factorial analysis.
cat("\n========== LANGUAGE x ORIGIN ANALYSIS ==========\n")

for (variable in variables) {
  data$Log_Measure <- log1p(data[[variable]])
  
  model <- lm(
    Log_Measure ~ Language * Generated,
    data = data
  )
  
  cat("\n---", variable, "(log1p transformed) ---\n")
  print(anova(model))
}

# STEP 9: SAVE GRAPHS AS PNG FILES

png("Final_Line_Count_Boxplots.png",
    width = 1600, height = 1100, res = 150)

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

for (lang in languages) {
  subset_data <- data[data$Language == lang, ]
  
  boxplot(
    Line_Count ~ Generated,
    data = subset_data,
    main = paste(lang, "- Line Count"),
    xlab = "Code Origin",
    ylab = "Number of Lines"
  )
}

dev.off()


png("Final_Character_Count_Boxplots.png",
    width = 1600, height = 1100, res = 150)

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

for (lang in languages) {
  subset_data <- data[data$Language == lang, ]
  
  boxplot(
    Character_Count ~ Generated,
    data = subset_data,
    main = paste(lang, "- Character Count"),
    xlab = "Code Origin",
    ylab = "Number of Characters"
  )
}

dev.off()

# STEP 10: SAVE RESULT TABLES
write.csv(test_results, "Final_Welch_Tests.csv", row.names = FALSE)
write.csv(effect_results, "Final_Cohens_d.csv", row.names = FALSE)
write.csv(wilcox_results, "Final_Wilcoxon_Tests.csv", row.names = FALSE)

cat("\n========== COMPLETE ==========\n")
cat("Saved Final_Welch_Tests.csv, Final_Cohens_d.csv,",
    "and Final_Wilcoxon_Tests.csv in your R working directory.\n")



# ============================================================
# FINAL REVIEW 3 ADDITION
# MODULE 4: CORRELATION AND REGRESSION
# MODULE 5: TWO-WAY ANOVA
# ============================================================

setwd("C:/Users/akank/OneDrive/Desktop/pasproj")

# Consistent factor references for interpreting coefficients
data$Generated_Factor <- factor(
  data$Generated,
  levels = c("Human", "AI")
)

data$Language <- factor(
  data$Language,
  levels = c("C", "C++", "Java", "Python")
)

# ------------------------------------------------------------
# MODULE 4A: SPEARMAN CORRELATION
# Does code with more lines also tend to have more characters?
# Spearman correlation is useful for skewed/count data.
# ------------------------------------------------------------

cat("\n========== MODULE 4: SPEARMAN CORRELATION ==========\n")

overall_cor <- cor.test(
  data$Line_Count,
  data$Character_Count,
  method = "spearman",
  exact = FALSE
)

print(overall_cor)

correlation_results <- data.frame()

for (lang in levels(data$Language)) {
  sub <- data[data$Language == lang, ]
  
  result <- cor.test(
    sub$Line_Count,
    sub$Character_Count,
    method = "spearman",
    exact = FALSE
  )
  
  correlation_results <- rbind(
    correlation_results,
    data.frame(
      Language = lang,
      N = nrow(sub),
      Spearman_Rho = unname(result$estimate),
      P_Value = result$p.value
    )
  )
}

correlation_results$Holm_P <- p.adjust(
  correlation_results$P_Value,
  method = "holm"
)

correlation_results$Significant <- (
  correlation_results$Holm_P < 0.05
)

print(correlation_results, digits = 5, row.names = FALSE)

write.csv(
  correlation_results,
  "Final_Spearman_Correlations.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# MODULE 4B: MULTIPLE REGRESSION
# Outcome: log(character count + 1)
# Predictors: log(line count + 1), code origin, language
# Interaction: does the line-character relationship differ
# between AI and human samples?
# ------------------------------------------------------------

cat("\n========== MODULE 4: MULTIPLE REGRESSION ==========\n")

regression_model <- lm(
  log1p(Character_Count) ~
    log1p(Line_Count) * Generated_Factor + Language,
  data = data
)

print(summary(regression_model))
print(confint(regression_model))

regression_coefficients <- as.data.frame(
  summary(regression_model)$coefficients
)

regression_coefficients$Term <- rownames(
  regression_coefficients
)

rownames(regression_coefficients) <- NULL

write.csv(
  regression_coefficients,
  "Final_Regression_Coefficients.csv",
  row.names = FALSE
)

# Save regression diagnostic plots
png(
  "Final_Regression_Diagnostics.png",
  width = 1600,
  height = 1200,
  res = 150
)

par(mfrow = c(2, 2))
plot(regression_model)
dev.off()

# ------------------------------------------------------------
# MODULE 5: TWO-WAY FACTORIAL ANOVA
# Factors: programming language and code origin
# Outcomes: log-transformed line and character counts
# drop1 tests each term while accounting for the other terms.
# ------------------------------------------------------------

cat("\n========== MODULE 5: TWO-WAY ANOVA ==========\n")

anova_line_model <- lm(
  log1p(Line_Count) ~ Language * Generated_Factor,
  data = data
)

anova_character_model <- lm(
  log1p(Character_Count) ~ Language * Generated_Factor,
  data = data
)

cat("\n--- Line count: partial F-tests ---\n")
line_anova_terms <- drop1(anova_line_model, test = "F")
print(line_anova_terms)

cat("\n--- Character count: partial F-tests ---\n")
character_anova_terms <- drop1(
  anova_character_model,
  test = "F"
)
print(character_anova_terms)

write.csv(
  as.data.frame(line_anova_terms),
  "Final_ANOVA_Line_Count.csv"
)

write.csv(
  as.data.frame(character_anova_terms),
  "Final_ANOVA_Character_Count.csv"
)

# ------------------------------------------------------------
# MODULE 2 SUPPORT: HISTOGRAMS OF MEASUREMENTS
# Shows distribution shape and possible skewness.
# ------------------------------------------------------------

png(
  "Final_Distribution_Histograms.png",
  width = 1600,
  height = 1100,
  res = 150
)

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

hist(
  data$Line_Count[data$Generated == "AI"],
  breaks = 30,
  main = "AI Code: Line Count",
  xlab = "Number of Lines"
)

hist(
  data$Line_Count[data$Generated == "Human"],
  breaks = 30,
  main = "Human Code: Line Count",
  xlab = "Number of Lines"
)

hist(
  data$Character_Count[data$Generated == "AI"],
  breaks = 30,
  main = "AI Code: Character Count",
  xlab = "Number of Characters"
)

hist(
  data$Character_Count[data$Generated == "Human"],
  breaks = 30,
  main = "Human Code: Character Count",
  xlab = "Number of Characters"
)

dev.off()

cat("\n========== NEW ANALYSES COMPLETE ==========\n")
cat("New outputs saved in:", getwd(), "\n")

print(list.files(
  pattern = "^Final_(Spearman|Regression|ANOVA|Distribution)"
))

# ============================================================
# FINAL REFINEMENT: COMMENT COUNT, REGRESSION AND ANOVA
# ============================================================

setwd("C:/Users/akank/OneDrive/Desktop/pasproj")

data$Generated_Factor <- factor(
  data$Generated, levels = c("Human", "AI")
)
data$Language <- factor(
  data$Language, levels = c("C", "C++", "Java", "Python")
)

# ------------------------------------------------------------
# A. IMPROVED APPROXIMATE COMMENT-LINE COUNT
# Counts standalone // comments, Python # comments, and lines
# inside /* ... */ block comments.
# It excludes preprocessor directives such as #include/#define
# in C/C++. It does not parse strings or inline comments.
# ------------------------------------------------------------

count_comments_v2 <- function(code, language) {
  if (is.na(code) || !nzchar(code)) return(0L)
  
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines)
  
  count <- 0L
  in_block <- FALSE
  
  for (line in lines) {
    if (!nzchar(line)) next
    
    if (language == "Python") {
      if (grepl("^#", line)) count <- count + 1L
      next
    }
    
    if (in_block) {
      count <- count + 1L
      if (grepl("\\*/", line)) in_block <- FALSE
      next
    }
    
    if (grepl("^//", line)) {
      count <- count + 1L
      next
    }
    
    if (grepl("^/\\*", line)) {
      count <- count + 1L
      if (!grepl("\\*/", line)) in_block <- TRUE
    }
  }
  
  count
}

data$Comment_Line_Count_v2 <- mapply(
  count_comments_v2, data$Sample_Code, data$Language
)

comment_summary <- do.call(rbind, lapply(
  levels(data$Language),
  function(lang) {
    do.call(rbind, lapply(
      c("AI", "Human"),
      function(origin) {
        x <- data$Comment_Line_Count_v2[
          data$Language == lang & data$Generated == origin
        ]
        
        data.frame(
          Language = lang,
          Origin = origin,
          N = length(x),
          Mean = mean(x),
          Median = median(x),
          SD = sd(x),
          Zero_Count_Percent = 100 * mean(x == 0)
        )
      }
    ))
  }
))

cat("\n========== REVISED COMMENT SUMMARY ==========\n")
print(comment_summary, row.names = FALSE, digits = 5)

# Compare AI vs Human comment counts within each language
comment_tests <- do.call(rbind, lapply(
  levels(data$Language),
  function(lang) {
    ai <- data$Comment_Line_Count_v2[
      data$Language == lang & data$Generated == "AI"
    ]
    human <- data$Comment_Line_Count_v2[
      data$Language == lang & data$Generated == "Human"
    ]
    
    wt <- wilcox.test(ai, human, exact = FALSE)
    
    data.frame(
      Language = lang,
      AI_Mean = mean(ai),
      Human_Mean = mean(human),
      AI_Median = median(ai),
      Human_Median = median(human),
      P_Value = wt$p.value
    )
  }
))

comment_tests$Holm_P <- p.adjust(comment_tests$P_Value, method = "holm")
comment_tests$Significant <- comment_tests$Holm_P < 0.05

cat("\n========== REVISED COMMENT TESTS ==========\n")
print(comment_tests, row.names = FALSE, digits = 5)

write.csv(comment_summary, "Final_Comment_Summary_v2.csv",
          row.names = FALSE)
write.csv(comment_tests, "Final_Comment_Tests_v2.csv",
          row.names = FALSE)


# ------------------------------------------------------------
# B. IMPROVED TWO-WAY ANOVA
# Explicitly test language, origin and their interaction.
# Use nested-model F-tests so each effect is tested conditional
# on the other included factors.
# ------------------------------------------------------------

compare_models <- function(reduced, full, outcome, term) {
  comparison <- anova(reduced, full)
  
  ss_effect <- comparison$`Sum of Sq`[2]
  df_effect <- comparison$Df[2]
  f_value <- comparison$F[2]
  p_value <- comparison$`Pr(>F)`[2]
  df_error <- df.residual(full)
  ss_error <- deviance(full)
  
  data.frame(
    Outcome = outcome,
    Term = term,
    DF_effect = df_effect,
    DF_error = df_error,
    F_value = f_value,
    P_value = p_value,
    Partial_Eta_Squared = ss_effect / (ss_effect + ss_error)
  )
}

anova_results <- data.frame()

for (variable in c("Line_Count", "Character_Count")) {
  data$Log_Response <- log1p(data[[variable]])
  
  language_only <- lm(Log_Response ~ Language, data = data)
  origin_only <- lm(Log_Response ~ Generated_Factor, data = data)
  
  additive <- lm(
    Log_Response ~ Language + Generated_Factor,
    data = data
  )
  
  full <- lm(
    Log_Response ~ Language * Generated_Factor,
    data = data
  )
  
  anova_results <- rbind(
    anova_results,
    compare_models(
      origin_only, additive, variable,
      "Language adjusted for origin"
    ),
    compare_models(
      language_only, additive, variable,
      "Origin adjusted for language"
    ),
    compare_models(
      additive, full, variable,
      "Language x origin interaction"
    )
  )
}

anova_results$P_Holm_within_outcome <- NA_real_

for (outcome in unique(anova_results$Outcome)) {
  idx <- which(anova_results$Outcome == outcome)
  anova_results$P_Holm_within_outcome[idx] <- p.adjust(
    anova_results$P_value[idx], method = "holm"
  )
}

cat("\n========== COMPLETE FACTORIAL ANOVA ==========\n")
print(anova_results, row.names = FALSE, digits = 5)

write.csv(
  anova_results,
  "Final_ANOVA_All_Effects.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# C. IMPROVED MULTIPLE REGRESSION
# Tests:
# 1. Does the AI-human difference vary by language?
# 2. Does the line-count/character-count relationship vary
#    by code origin?
# Outcome is log(character count + 1).
# ------------------------------------------------------------

log_char <- log1p(data$Character_Count)
log_lines <- log1p(data$Line_Count)

reg_base <- lm(
  log_char ~ log_lines + Language + Generated_Factor,
  data = data
)

reg_language_origin <- lm(
  log_char ~ log_lines + Language * Generated_Factor,
  data = data
)

reg_full <- lm(
  log_char ~ log_lines * Generated_Factor +
    Language * Generated_Factor,
  data = data
)

cat("\n========== REGRESSION COMPARISON 1 ==========\n")
cat("Does origin's effect vary by language?\n")
print(anova(reg_base, reg_language_origin))

cat("\n========== REGRESSION COMPARISON 2 ==========\n")
cat("Does the line-character relationship vary by origin?\n")
print(anova(reg_language_origin, reg_full))

cat("\n========== FINAL REGRESSION MODEL ==========\n")
print(summary(reg_full))
print(confint(reg_full))

regression_comparison <- anova(reg_base, reg_language_origin)
write.csv(
  as.data.frame(regression_comparison),
  "Final_Regression_Language_Origin_Comparison.csv"
)

regression_slope_comparison <- anova(reg_language_origin, reg_full)
write.csv(
  as.data.frame(regression_slope_comparison),
  "Final_Regression_Slope_Comparison.csv"
)

coef_table <- as.data.frame(summary(reg_full)$coefficients)
coef_table$Term <- rownames(coef_table)
rownames(coef_table) <- NULL

write.csv(
  coef_table,
  "Final_Regression_Improved_Coefficients.csv",
  row.names = FALSE
)

cat("\n========== FINAL REFINEMENT COMPLETE ==========\n")
print(list.files(pattern = "^Final_(Comment|ANOVA_All|Regression_)"))


# ============================================================
# NEW ANALYSIS: NORMALIZED CODE STYLE PROFILE
# ============================================================

setwd("C:/Users/akank/OneDrive/Desktop/pasproj")

# Ensure the refined comment count exists
if (!"Comment_Line_Count_v2" %in% names(data)) {
  count_comments_v2 <- function(code, language) {
    if (is.na(code) || !nzchar(code)) return(0L)
    lines <- trimws(strsplit(code, "\n", fixed = TRUE)[[1]])
    count <- 0L
    in_block <- FALSE
    
    for (line in lines) {
      if (!nzchar(line)) next
      
      if (language == "Python") {
        if (grepl("^#", line)) count <- count + 1L
        next
      }
      
      if (in_block) {
        count <- count + 1L
        if (grepl("\\*/", line)) in_block <- FALSE
      } else if (grepl("^//", line)) {
        count <- count + 1L
      } else if (grepl("^/\\*", line)) {
        count <- count + 1L
        if (!grepl("\\*/", line)) in_block <- TRUE
      }
    }
    count
  }
  
  data$Comment_Line_Count_v2 <- mapply(
    count_comments_v2, data$Sample_Code, data$Language
  )
}

# 1. Normalize code size: characters per line
data$Characters_Per_Line <-
  data$Character_Count / pmax(data$Line_Count, 1)

# 2. Separate comment presence from comment quantity
data$Has_Comment <- data$Comment_Line_Count_v2 > 0

# Comment density, per 100 lines
data$Comments_Per_100_Lines <-
  100 * data$Comment_Line_Count_v2 / pmax(data$Line_Count, 1)

# ------------------------------------------------------------
# Descriptive profile by language and origin
# ------------------------------------------------------------

profile <- do.call(rbind, lapply(
  levels(factor(data$Language)),
  function(lang) {
    do.call(rbind, lapply(c("AI", "Human"), function(origin) {
      x <- data[
        data$Language == lang & data$Generated == origin, ]
      
      data.frame(
        Language = lang,
        Origin = origin,
        N = nrow(x),
        Mean_Characters_Per_Line = mean(x$Characters_Per_Line),
        Median_Characters_Per_Line = median(x$Characters_Per_Line),
        Percent_With_Comment = 100 * mean(x$Has_Comment),
        Mean_Comments_Per_100_Lines = mean(x$Comments_Per_100_Lines)
      )
    }))
  }
))

cat("\n========== NORMALIZED CODE PROFILE ==========\n")
print(profile, row.names = FALSE, digits = 4)

write.csv(profile, "Final_Normalized_Code_Profile.csv",
          row.names = FALSE)

# ------------------------------------------------------------
# Test A: characters per line, separately within each language
# Wilcoxon tests plus Cliff's delta effect size
# ------------------------------------------------------------

density_tests <- do.call(rbind, lapply(
  levels(factor(data$Language)),
  function(lang) {
    x <- data[data$Language == lang, ]
    ai <- x$Characters_Per_Line[x$Generated == "AI"]
    hu <- x$Characters_Per_Line[x$Generated == "Human"]
    
    wt <- wilcox.test(ai, hu, exact = FALSE)
    U <- as.numeric(wt$statistic) - length(ai) * (length(ai) + 1) / 2
    cliff_delta <- 2 * U / (length(ai) * length(hu)) - 1
    
    data.frame(
      Language = lang,
      AI_Median = median(ai),
      Human_Median = median(hu),
      Median_Difference = median(ai) - median(hu),
      Cliffs_Delta = cliff_delta,
      P_Value = wt$p.value
    )
  }
))

density_tests$Holm_P <- p.adjust(density_tests$P_Value, method = "holm")
density_tests$Significant <- density_tests$Holm_P < 0.05

cat("\n========== CHARACTERS-PER-LINE TESTS ==========\n")
print(density_tests, row.names = FALSE, digits = 5)

write.csv(density_tests, "Final_Characters_Per_Line_Tests.csv",
          row.names = FALSE)

# ------------------------------------------------------------
# Test B: proportion of code samples containing comments
# Compare AI vs Human within each language
# ------------------------------------------------------------

comment_presence_tests <- do.call(rbind, lapply(
  levels(factor(data$Language)),
  function(lang) {
    x <- data[data$Language == lang, ]
    
    ai_yes <- sum(x$Has_Comment[x$Generated == "AI"])
    ai_n <- sum(x$Generated == "AI")
    hu_yes <- sum(x$Has_Comment[x$Generated == "Human"])
    hu_n <- sum(x$Generated == "Human")
    
    pt <- prop.test(
      x = c(ai_yes, hu_yes),
      n = c(ai_n, hu_n),
      correct = FALSE
    )
    
    data.frame(
      Language = lang,
      AI_With_Comments_Pct = 100 * ai_yes / ai_n,
      Human_With_Comments_Pct = 100 * hu_yes / hu_n,
      Difference_Percentage_Points =
        100 * (ai_yes / ai_n - hu_yes / hu_n),
      CI_Lower_Points = 100 * pt$conf.int[1],
      CI_Upper_Points = 100 * pt$conf.int[2],
      P_Value = pt$p.value
    )
  }
))

comment_presence_tests$Holm_P <- p.adjust(
  comment_presence_tests$P_Value, method = "holm"
)
comment_presence_tests$Significant <-
  comment_presence_tests$Holm_P < 0.05

cat("\n========== COMMENT PRESENCE TESTS ==========\n")
print(comment_presence_tests, row.names = FALSE, digits = 5)

write.csv(
  comment_presence_tests,
  "Final_Comment_Presence_Tests.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# Visualization: normalized density and comment presence
# ------------------------------------------------------------

png("Final_Normalized_Style_Graphs.png",
    width = 1600, height = 1100, res = 150)

par(mfrow = c(2, 2), mar = c(5, 4, 3, 1))

for (lang in levels(factor(data$Language))) {
  x <- data[data$Language == lang, ]
  boxplot(
    Characters_Per_Line ~ Generated,
    data = x,
    main = paste(lang, "- Characters per Line"),
    xlab = "Origin",
    ylab = "Characters per Line"
  )
}

dev.off()

cat("\n========== NORMALIZED PROFILE COMPLETE ==========\n")


