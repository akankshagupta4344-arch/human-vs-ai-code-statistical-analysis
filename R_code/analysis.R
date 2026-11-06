
data <- read.csv(file.choose(), stringsAsFactors = FALSE)

dim(data)
names(data)
head(data)
str(data)

table(data$Generated)
table(data$Language)
table(data$Generated, data$Language)
sum(is.na(data$Sample_Code))
sum(duplicated(data))

data$Line_Count <- sapply(
  strsplit(data$Sample_Code, "\n", fixed = TRUE),
  length
)

data$Character_Count <- nchar(data$Sample_Code)

head(data[, c(
  "Generated", "Language",
  "Line_Count", "Character_Count"
)])

aggregate(
  Line_Count ~ Generated,
  data = data,
  FUN = function(x) c(
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Minimum = min(x, na.rm = TRUE),
    Maximum = max(x, na.rm = TRUE)
  )
)

aggregate(
  Character_Count ~ Generated,
  data = data,
  FUN = function(x) c(
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE)
  )
)

boxplot(
  Line_Count ~ Generated,
  data = data,
  main = "Code Length by Origin",
  xlab = "Code Origin",
  ylab = "Number of Lines",
  col = c("lightblue", "lightpink")
)

hist(
  data$Line_Count[data$Generated == "Human"],
  main = "Distribution of Human-Written Code Length",
  xlab = "Number of Lines",
  breaks = 30
)




# Count lines that appear to start with a comment
data$Comment_Line_Count <- vapply(
  data$Sample_Code,
  function(code) {
    lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
    sum(grepl(
      "^[[:space:]]*(#|//|/\\*)",
      lines
    ))
  },
  integer(1)
)

# Summarize the comment-line count by code origin
aggregate(
  Comment_Line_Count ~ Generated,
  data = data,
  FUN = function(x) c(
    Mean = mean(x),
    Median = median(x),
    SD = sd(x)
  )
)

# Summarize comment-line counts by origin and language
aggregate(
  Comment_Line_Count ~ Generated + Language,
  data = data,
  FUN = function(x) c(
    Mean = mean(x),
    Median = median(x)
  )
)

# Visual comparison
boxplot(
  Comment_Line_Count ~ Generated,
  data = data,
  main = "Comment Lines by Code Origin",
  xlab = "Code Origin",
  ylab = "Approximate Comment-Line Count"
)






# Test for a difference in mean line counts
line_test <- t.test(
  Line_Count ~ Generated,
  data = data
)

line_test

# Test for a difference in mean character counts
character_test <- t.test(
  Character_Count ~ Generated,
  data = data
)

character_test

# Test association between origin and language
language_test <- chisq.test(
  table(data$Generated, data$Language)
)

language_test

data$Preproc_Count <- vapply(data$Sample_Code, function(code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  sum(grepl("^[[:space:]]*#[[:space:]]*(include|define|if|ifdef|ifndef|else|endif|pragma|undef)", lines))
}, integer(1))
aggregate(cbind(Comment_Line_Count, Preproc_Count) ~ Generated + Language, data = data, FUN = mean)


aggregate(
  cbind(Comment_Line_Count, Preproc_Count) ~ Generated + Language,
  data = data,
  FUN = mean
)
