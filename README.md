# Human vs AI-Generated Code: Statistical Analysis

A Probability and Statistics project comparing human-written and AI-generated source code across C, C++, Java and Python.

## Project Overview

This project examines differences in code length, character density and comment characteristics using statistical methods.

## Dataset

- 10,000 code samples
- 5,000 AI-generated samples
- 5,000 human-written samples
- Four programming languages: C, C++, Java and Python

Original dataset:
https://data.mendeley.com/datasets/kjh95n54f8/1

## Statistical Methods

- Descriptive statistics
- Welch's two-sample t-test
- Confidence intervals
- Wilcoxon rank-sum test
- Cohen's d and Cliff's delta
- Chi-square test of independence
- Spearman correlation
- Regression analysis
- Factorial ANOVA

## Key Findings

AI-generated code had higher median characters per nonblank line across all four languages.

AI-generated samples also had a higher proportion of detectable comment presence in every language.

The differences in code length varied by programming language, suggesting that language matters when comparing code characteristics.

## Repository Structure

- R_code: Analysis scripts
- dataset: Dataset citation and source
- reports: Final project reports
- presentations: Review presentations
- results: Statistical outputs and tables
- figures: Graphs and diagnostic plots

## Limitations

The project examines statistical characteristics, not code correctness, quality or definitive authorship. Comment detection is heuristic, and the dataset's balanced composition does not represent real-world prevalence.

