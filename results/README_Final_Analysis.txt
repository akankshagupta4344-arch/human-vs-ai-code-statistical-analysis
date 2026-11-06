FINAL REVIEW 3 — CLEAN SCRIPT INSTRUCTIONS

WHAT THIS SCRIPT FIXES
- Uses a file picker instead of a hard-coded computer-specific CSV path.
- Saves outputs in the project folder (the parent of Code_Dataset).
- Consolidates repeated analyses into one clean, sequential script.
- Corrects the Cliff's delta calculation: base R wilcox.test() returns the Mann–Whitney U form, so the minimum rank-sum term must NOT be subtracted again.
- Adds a better-defined characters-per-nonblank-line measure: it excludes CR/LF characters from the numerator and ignores blank lines in the denominator.
- Adds comment-count distribution diagnostics (variance/mean and observed vs Poisson zero probability) for Module 2.
- Saves updated descriptive tables, tests, effect sizes, regression, factorial ANOVA, diagnostic plots, and a plain-text run summary.
- Labels comment metrics as heuristic rather than exact source-code parsing.

HOW TO RUN
1. Keep the dataset in:
   pasproj\Code_Dataset\HumanVsAI_CodeDataset.csv
2. Put Final_Review3_Analysis_CLEAN.R in:
   pasproj\
3. Open the CLEAN script in RStudio.
4. Click Source.
5. In the file picker, select HumanVsAI_CodeDataset.csv (not the ._HumanVsAI_CodeDataset.csv metadata file).
6. Wait for "ANALYSIS COMPLETE" in the Console.
7. Check the pasproj folder for files beginning Final_.

IMPORTANT INTERPRETATION LIMITS
- This script has been statically cleaned but should be run locally in RStudio against the real CSV before submission.
- Human and AI source code came from different collection processes. The analysis cannot isolate a causal AI authorship effect.
- Only three problem IDs were reported to contain both origins. The check documents overlap; it cannot prove all observations independent.
- Comment counting remains heuristic. A quick manual validation sample is still needed before claiming comment-count accuracy.
- Regression diagnostics should be inspected; non-normal residual tails and non-constant variance mean conventional inference should be presented cautiously.
- The revised characters-per-nonblank-line metric is better defined than the old characters/line ratio, so its results may differ. Update final slides/report from the new CSV, not old review numbers.

FILES THAT ARE REPLACED/UPDATED WHEN RUN
- Final_Welch_Tests.csv
- Final_Wilcoxon_CliffsDelta.csv
- Final_Cohens_d.csv
- Final_Comment_Count_Tests.csv
- Final_Comment_Presence_Tests.csv
- Final_Comment_Distribution_Check.csv
- Final_Spearman_Correlations.csv
- Final_Regression_*.csv / .png
- Final_ANOVA_All_Effects.csv
- Final_Problem_ID_Diagnostic.csv
- Final_Line_Count_Boxplots.png
- Final_Character_Count_Boxplots.png
- Final_Corrected_Characters_Per_Line.png
- Final_Distribution_Histograms.png
- Final_Analysis_Summary.txt

ADDITIONAL QUICK FIXES
- The corrected normalized-density result will differ from the earlier ratio because it now excludes blank-line whitespace consistently. Use the new `Final_Corrected_Characters_Per_Line_Tests.csv` and do not reuse the old characters-per-line p-values as final results.
- `Final_Comment_Validation_Sample.csv` has 80 stratified code samples (10 per language x origin group). Manually label the two `Manual_` columns to estimate whether the heuristic detects comments correctly. The file is a validation aid; the manual labels must actually be completed before reporting precision/recall.
- The full dataset still cannot isolate authorship from source/task differences. State that all results are descriptive associations for this dataset.
