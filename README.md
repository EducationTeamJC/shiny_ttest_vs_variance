# T-Test vs ANOVA: Understanding the Mathematical Relationship

## Overview

This interactive Shiny application demonstrates the mathematical equivalence between the t-test and ANOVA when comparing two groups. It provides a comprehensive visual and interactive exploration of how these two statistical tests are fundamentally the same for the two-group case.

## Key Learning Objectives

- **Mathematical Equivalence**: Understand that t² = F for two-group comparisons
- **Conceptual Differences**: See how t-test focuses on group differences while ANOVA focuses on variance decomposition
- **Visual Understanding**: Explore the relationship through interactive plots and mathematical explanations
- **Practical Application**: Learn when and why you might choose one test over the other

## Features

### 1. Comparing t-test and ANOVA
- **Side-by-side plots** showing the same data from different perspectives
- **T-test view**: Focuses on the difference between group means
- **ANOVA view**: Shows variance decomposition (between-group vs within-group)
- **Consistent scaling** for easy comparison
- **Variance decomposition bar chart** with explanatory text

### 2. t² = F Visualization
- **Interactive plot** showing how test statistics change with effect size
- **Real-time demonstration** of the t² = F relationship
- **Effect size exploration** from 0 to 5
- **Current value highlighting** with red dashed line

### 3. Comparing the Math
- **Detailed mathematical bridge** explaining the relationship
- **Variable explanations** with actual calculated values
- **Formula comparisons** between t-test and ANOVA
- **Shared components** identification

### 4. Comparing the Distributions
- **T-distribution plot** with observed and critical values
- **F-distribution plot** showing the relationship to t²
- **Rejection regions** shaded in red
- **Clear annotations** for all key values

## Interactive Parameters

### Data Parameters
- **Sample Size Group 1**: Number of observations in first group (5-100)
- **Sample Size Group 2**: Number of observations in second group (5-100)
- **Effect Size**: Difference in means between groups (0-10)
- **Standard Deviation**: Common standard deviation for both groups (0.1-5)
- **Alpha Level**: Significance level for hypothesis testing (0.01-0.1)

### Results Summary
Real-time display of:
- T-test results (t-statistic, p-value)
- ANOVA results (F-statistic, p-value)
- Verification that t² = F
- Confirmation of identical p-values

## How to Use

1. **Start the App**: Run `shiny::runApp()` in R with this directory
2. **Adjust Parameters**: Use the sidebar controls to change data characteristics
3. **Explore Tabs**: Navigate through the four main sections
4. **Observe Changes**: Watch how modifications affect all visualizations simultaneously
5. **Read Explanations**: Pay attention to the mathematical bridge and variance decomposition text

## Key Insights

### Mathematical Relationship
- **t² = F**: The t-statistic squared equals the F-statistic
- **Same p-values**: Both tests give identical p-values
- **Shared assumptions**: Both require normality, independence, and equal variances
- **Pooled variance**: Both use the same variance estimate

### Conceptual Differences
- **T-test perspective**: "How different are the group means?"
- **ANOVA perspective**: "How much variance is explained by group membership?"
- **Visual focus**: T-test emphasizes the difference arrow, ANOVA emphasizes variance components

### Practical Implications
- **Statistical power**: Both tests have identical power
- **Effect size**: Both are equally sensitive to the same effects
- **Interpretation**: Choose based on your research question and audience
- **Extension**: ANOVA generalizes to more than two groups

## Technical Details

### R Packages Required
- `shiny`: Interactive web application framework
- `ggplot2`: Data visualization
- `dplyr`: Data manipulation
- `gridExtra`: Plot arrangement
- `grid`: Graphics system
- `patchwork`: Plot composition

### Data Generation
- Uses `rnorm()` with set seed for reproducibility
- Group 1: Mean = 0, Group 2: Mean = effect size
- Both groups share the same standard deviation
- Sample sizes can be different between groups

### Statistical Calculations
- **T-test**: Uses `t.test()` with `var.equal = TRUE`
- **ANOVA**: Uses `aov()` for one-way analysis
- **Variance components**: Manually calculated for educational purposes
- **Critical values**: Calculated using `qt()` and `qf()`

## Educational Value

This app is designed for:
- **Statistics students** learning about hypothesis testing
- **Researchers** wanting to understand test relationships
- **Instructors** teaching statistical concepts
- **Anyone** curious about the mathematical foundations of statistical tests

## Getting Started

1. Ensure you have R and the required packages installed
2. Download or clone this repository
3. Open `app.R` in RStudio
4. Run `shiny::runApp()` or click "Run App"
5. Explore the interactive features and mathematical explanations

## Contributing

Feel free to suggest improvements, report issues, or contribute enhancements to make this educational tool even better!

---

*This app demonstrates that for comparing two groups, the t-test and ANOVA are mathematically equivalent, providing different perspectives on the same underlying statistical relationship.* 