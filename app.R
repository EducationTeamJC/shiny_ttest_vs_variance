library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(patchwork)
library(Cairo)


# Comprehensive high-quality rendering settings


# High-quality rendering settings for crisp, non-blurry plots
# These settings ensure better resolution and quality for all plots

# UI
ui <- fluidPage(
  # Add CSS for high-quality plot rendering
  tags$head(
    tags$style(HTML("
      .shiny-plot-output {
        image-rendering: -webkit-optimize-contrast;
        image-rendering: crisp-edges;
        image-rendering: pixelated;
      }
      .shiny-plot-output img {
        max-width: 100%;
        height: auto;
      }
    "))
  ),

  titlePanel("T-Test vs ANOVA: Understanding the Relationship"),

  sidebarLayout(
    sidebarPanel(
      h4("Data Parameters"),
      numericInput("n1", "Sample Size Group 1:", value = 20, min = 5, max = 100),
      numericInput("n2", "Sample Size Group 2:", value = 20, min = 5, max = 100),
      numericInput("effect_size", "Effect Size (Difference in Means):", value = 1, min = 0, max = 10, step = 0.1),
      numericInput("sd", "Standard Deviation:", value = 1, min = 0.1, max = 5, step = 0.1),
      numericInput("alpha", "Alpha Level:", value = 0.05, min = 0.01, max = 0.1, step = 0.01),

      width = 3
    ),

    mainPanel(
      tabsetPanel(
         tabPanel("Results of t-test and ANOVA",
                  fluidRow(
                    column(6, verbatimTextOutput("results_summary")),
                    column(6, plotOutput("data_visualization", height = "400px", width = "500px"))
                  )
         ),
        tabPanel("Comparing t-test and ANOVA",
                 fluidRow(
                   column(6,
                          plotOutput("ttest_plot", height = "400px", width = "500px"),
                          verbatimTextOutput("ttest_formula")
                   ),
                   column(6,
                          plotOutput("anova_plot", height = "400px", width = "500px"),
                          verbatimTextOutput("anova_formula")
                   )
                 )
        ),

        tabPanel("t² = F visualisation",
                 plotOutput("effect_size_plot", height = "500px", width = "800px")
        ),

        tabPanel("Comparing the distributions",
                 fluidRow(
                   column(6, plotOutput("t_distribution", height = "400px", width = "500px")),
                   column(6, plotOutput("f_distribution", height = "400px", width = "500px"))
                 )
        ),

        tabPanel("Comparing the math",
                 plotOutput("math_bridge", height = "600px", width = "1000px")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Set higher resolution for plots
  options(shiny.plot.dpi = 300)
  options(shiny.useragg = TRUE)

  # Create a high-quality theme for all plots
  high_quality_theme <- function() {
    theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.5)
      )
  }

  # Generate data based on inputs
  data <- reactive({
    set.seed(123) # For reproducibility

    n1 <- input$n1
    n2 <- input$n2
    effect_size <- input$effect_size
    sd_val <- input$sd

    group1 <- rnorm(n1, mean = 0, sd = sd_val)
    group2 <- rnorm(n2, mean = effect_size, sd = sd_val)

    data.frame(
      value = c(group1, group2),
      group = factor(rep(c("Group 1", "Group 2"), c(n1, n2)))
    )
  })

  # Calculate statistics
  stats <- reactive({
    dat <- data()

    # Group means and overall mean
    group_means <- dat %>% group_by(group) %>% summarise(mean = mean(value))
    grand_mean <- mean(dat$value)

    # T-test
    t_result <- t.test(value ~ group, data = dat, var.equal = TRUE)
    t_stat <- t_result$statistic
    t_pvalue <- t_result$p.value

    # ANOVA
    aov_result <- aov(value ~ group, data = dat)
    f_stat <- summary(aov_result)[[1]]$"F value"[1]
    f_pvalue <- summary(aov_result)[[1]]$"Pr(>F)"[1]

    # Variance components
    ss_between <- sum(table(dat$group) * (group_means$mean - grand_mean)^2)
    ss_within <- sum((dat$value - rep(group_means$mean, table(dat$group)))^2)
    ss_total <- ss_between + ss_within

    # Degrees of freedom
    df_between <- 1
    df_within <- nrow(dat) - 2
    df_total <- nrow(dat) - 1

    # Mean squares
    ms_between <- ss_between / df_between
    ms_within <- ss_within / df_within

    list(
      t_result = t_result,
      t_stat = t_stat,
      t_pvalue = t_pvalue,

      aov_result = aov_result,
      f_stat = f_stat,
      f_pvalue = f_pvalue,
      ss_between = ss_between,
      ss_within = ss_within,
      ss_total = ss_total,
      ms_between = ms_between,
      ms_within = ms_within,
      group_means = group_means,
      grand_mean = grand_mean,
      n1 = input$n1,
      n2 = input$n2
    )
  })

  # Results summary
  output$results_summary <- renderPrint({
    s <- stats()
    cat("** T-Test Results:")
    print(s$t_result)
    cat("\n")
    cat("##########################################")
    cat("\n\n")
    # cat("t-statistic:", round(s$t_stat, 4), "\n")
    # cat("p-value:", round(s$t_pvalue, 6), "\n\n")
    #
    cat("** ANOVA Results:\n")
    print(summary(s$aov_result))
    # cat("F-statistic:", round(s$f_stat, 4), "\n")
    # cat("p-value:", round(s$f_pvalue, 6), "\n\n")
  })

  # Simple data visualization
  output$data_visualization <- renderPlot({
    dat <- data()
    s <- stats()

    # Calculate group means
    group_means <- dat %>%
      group_by(group) %>%
      summarise(mean = mean(value))

    ggplot() +
      # Individual points
      geom_jitter(data = dat, aes(x = group, y = value),
                  alpha = 0.6, width = 0.2, color = "gray50", size = 2) +

      # Group means
      geom_point(data = group_means, aes(x = group, y = mean),
                 size = 4, color = "red") +

      # Mean lines
      geom_segment(data = group_means,
                   aes(x = as.numeric(group) - 0.3, xend = as.numeric(group) + 0.3,
                       y = mean, yend = mean),
                   color = "red", linewidth = 2) +

      # Add mean values as text
      geom_text(data = group_means,
                aes(x = group, y = mean + 0.3,
                    label = paste("Mean:", round(mean, 2))),
                color = "red", fontface = "bold", size = 4) +

      labs(title = "Data Visualization",
           subtitle = "Individual observations and group means",
           x = "Group", y = "Value") +
      high_quality_theme()
  })

  # 1. T-test conceptual plot
  output$ttest_plot <- renderPlot({
    dat <- data()
    s <- stats()

    # Calculate standard errors
    se_data <- dat %>%
      group_by(group) %>%
      summarise(
        mean = mean(value),
        se = sd(value) / sqrt(n()),
        sd = sd(value),
        variance = sd^2
      )

    # Calculate consistent y-axis limits for both plots
    y_min <- min(dat$value) - 0.5
    y_max <- max(dat$value) + 0.5

    ggplot() +
      # Individual points
      geom_jitter(data = dat, aes(x = group, y = value),
                  alpha = 0.6, width = 0.2, color = "gray50") +

      # Group means
      geom_point(data = se_data, aes(x = group, y = mean),
                 size = 4, color = "red") +

      # Standard deviation lines
      geom_segment(data = se_data,
                   aes(x = as.numeric(group) - 0.1, xend = as.numeric(group) + 0.1,
                       y = mean - sd, yend = mean - sd),
                   color = "blue", linewidth = 1) +
      geom_segment(data = se_data,
                   aes(x = as.numeric(group) - 0.1, xend = as.numeric(group) + 0.1,
                       y = mean + sd, yend = mean + sd),
                   color = "blue", linewidth = 1) +

      # Difference arrow
      geom_segment(aes(x = 1, xend = 2,
                       y = se_data$mean[1], yend = se_data$mean[2]),
                   arrow = arrow(length = unit(0.3, "cm")),
                   color = "darkgreen", linewidth = 2) +

      # Annotations
      annotate("text", x = 1.5, y = y_max - 0.2,
               label = paste("t =", round(s$t_stat, 3)),
               color = "darkgreen", fontface = "bold") +

      annotate("text", x = 1, y = se_data$mean[1] - se_data$sd[1] - 0.3,
               label = "SD", color = "blue", size = 3) +

      # Set consistent y-axis limits
      ylim(y_min, y_max) +

      labs(title = "T-Test",
           subtitle = "Focus on difference between group means",
           x = "Group", y = "Value") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(face = "italic"))
  })

  # 1. ANOVA conceptual plot
  output$anova_plot <- renderPlot({
    dat <- data()
    s <- stats()

    # Calculate group means and grand mean
    group_means <- dat %>%
      group_by(group) %>%
      summarise(mean = mean(value))

    grand_mean <- mean(dat$value)

    # Create jittered positions for the data points
    set.seed(123) # For reproducible jitter
    dat_jittered <- dat %>%
      group_by(group) %>%
      mutate(
        x_jitter = as.numeric(group) + runif(n(), -0.2, 0.2)
      ) %>%
      ungroup()

    # Create a mapping of group means for each data point
    dat_with_means <- dat_jittered %>%
      left_join(group_means, by = "group")

    # Calculate consistent y-axis limits for both plots
    y_min <- min(dat$value) - 0.5
    y_max <- max(dat$value) + 0.5

    ggplot() +
      # Grand mean line
      geom_hline(yintercept = grand_mean, color = "purple", linewidth = 2, linetype = "dashed") +

      # Individual points
      geom_point(data = dat_jittered, aes(x = x_jitter, y = value),
                 alpha = 0.6, color = "gray50") +

      # Group means
      geom_point(data = group_means, aes(x = as.numeric(group), y = mean),
                 size = 4, color = "red") +

      # Between-group variance (group means to grand mean)
      geom_segment(data = group_means,
                   aes(x = as.numeric(group), xend = as.numeric(group),
                       y = grand_mean, yend = mean),
                   color = "darkgreen", linewidth = 2, arrow = arrow(length = unit(0.2, "cm"))) +

      # Within-group variance (individuals to group means)
      geom_segment(data = dat_with_means,
                   aes(x = x_jitter, xend = x_jitter,
                       y = value, yend = mean),
                   color = "blue", alpha = 0.3) +

      # Annotations
      annotate("text", x = 1.5, y = y_max - 0.2,
               label = paste("F =", round(s$f_stat, 3)),
               color = "darkgreen", fontface = "bold") +

      annotate("text", x = 1.5, y = grand_mean - 0.3,
               label = "Grand Mean", color = "purple", fontface = "bold") +

      annotate("text", x = 1, y = y_min + 0.2,
               label = "Within-group\nvariance", color = "blue", size = 3) +
      annotate("text", x = 2, y = y_max - 0.2,
               label = "Between-group\nvariance", color = "darkgreen", size = 3) +

      # Set x-axis scale to match the numeric values
      scale_x_continuous(breaks = 1:2, labels = c("Group 1", "Group 2")) +

      # Set consistent y-axis limits
      ylim(y_min, y_max) +

      labs(title = "ANOVA",
           subtitle = "Focus on explained variance",
           x = "Group", y = "Value") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(face = "italic"))
  })

  # 1. Variance decomposition
  output$variance_decomp <- renderPlot({
    s <- stats()

    # Create variance decomposition data
    var_data <- data.frame(
      component = c("Between-Group", "Within-Group"),
      variance = c(s$ss_between, s$ss_within),
      percentage = c(s$ss_between / s$ss_total * 100, s$ss_within / s$ss_total * 100)
    )

    # Create the main plot
    p <- ggplot(var_data, aes(x = "", y = variance, fill = component)) +
      geom_bar(stat = "identity", width = 0.5) +
      geom_text(aes(label = paste0(round(percentage, 1), "%")),
                position = position_stack(vjust = 0.5),
                color = "white", fontface = "bold") +
      scale_fill_manual(values = c("Between-Group" = "darkgreen", "Within-Group" = "blue")) +
      labs(title = "Variance Decomposition",
           subtitle = "What makes up the total of variation observed?",
           x = "", y = "Sum of Squares",
           fill = "Component") +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(face = "italic"))

    # Add explanatory text using patchwork
    p +
      plot_annotation(
        caption = paste(
          "Between-Group: Variation explained by group differences (signal)\n",
          "Within-Group: Unexplained variation within groups (noise)\n",
          "F-statistic = MS_between/MS_within =", round(s$f_stat, 3), "\n",
          "This is equivalent to t² =", round(s$t_stat^2, 3), "from the t-test"
        ),
        theme = theme(
          plot.caption = element_text(size = 10, hjust = 0, face = "italic")
        )
      )
  })

  # T-test formula output
  output$ttest_formula <- renderPrint({
    s <- stats()
    cat("T-Test Formula:\n")
    cat("t = (x̄₁ - x̄₂) / √(s²(1/n₁ + 1/n₂))\n\n")
    cat("Where:\n")
    cat("x̄₁ = mean of group 1 =", round(s$group_means$mean[1], 3), "\n")
    cat("x̄₂ = mean of group 2 =", round(s$group_means$mean[2], 3), "\n")
    cat("s² = pooled variance =", round(s$ms_within, 3), "\n")
    cat("n₁ = sample size group 1 =", s$n1, "\n")
    cat("n₂ = sample size group 2 =", s$n2, "\n\n")
    cat("t =", round(s$t_stat, 4), "\n")
  })

  # ANOVA formula output
  output$anova_formula <- renderPrint({
    s <- stats()
    cat("ANOVA Formula:\n")
    cat("F = MS_between / MS_within\n\n")
    cat("Where:\n")
    cat("MS_between = between-group mean square =", round(s$ms_between, 3), "\n")
    cat("MS_within = within-group mean square =", round(s$ms_within, 3), "\n")
    cat("df_between =", 1, "\n")
    cat("df_within =", s$n1 + s$n2 - 2, "\n\n")
    cat("F =", round(s$f_stat, 4), "\n")
  })

  # 2. Effect size exploration
  output$effect_size_plot <- renderPlot({
    # Generate data for different effect sizes
    effect_sizes <- seq(0, 5, by = 0.1)
    n1 <- input$n1
    n2 <- input$n2
    sd_val <- input$sd

    results <- data.frame(
      effect_size = effect_sizes,
      t_stat = numeric(length(effect_sizes)),
      f_stat = numeric(length(effect_sizes))
    )

    for (i in seq_along(effect_sizes)) {
      set.seed(123)
      group1 <- rnorm(n1, mean = 0, sd = sd_val)
      group2 <- rnorm(n2, mean = effect_sizes[i], sd = sd_val)

      # Calculate t-statistic
      pooled_se <- sqrt((var(group1) * (n1-1) + var(group2) * (n2-1)) / (n1 + n2 - 2) * (1/n1 + 1/n2))
      t_stat <- (mean(group2) - mean(group1)) / pooled_se

      results$t_stat[i] <- t_stat
      results$f_stat[i] <- t_stat^2
    }

    ggplot(results, aes(x = effect_size)) +
      geom_line(aes(y = t_stat, color = "t-statistic"), linewidth = 1.5) +
      geom_line(aes(y = f_stat, color = "F-statistic"), linewidth = 1.5) +
      geom_line(aes(y = t_stat^2, color = "t²"), linewidth = 1, linetype = "dashed") +

      # Highlight current effect size
      geom_vline(xintercept = input$effect_size, color = "red", linewidth = 1, linetype = "dashed") +

      scale_color_manual(values = c("t-statistic" = "blue", "F-statistic" = "red", "t²" = "green")) +
      labs(title = "Effect Size vs Test Statistics",
           subtitle = "Demonstrating t² = F relationship",
           x = "Effect Size (Difference in Means)",
           y = "Test Statistic",
           color = "Statistic") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(face = "italic"))
  })

  # 3. Mathematical bridge
  output$math_bridge <- renderPlot({
    s <- stats()

    # Create a custom plot showing the mathematical relationship
    par(mar = c(1, 1, 1, 1))
    plot(1, 1, type = "n", xlim = c(0, 20), ylim = c(-6, 26),
         axes = FALSE, xlab = "", ylab = "")

    # Title
    text(7, 24.5, "Mathematical Relationship: t² = F", cex = 1.5, font = 2)
    distance <- 1.3
    # Step 1: Hypothesis
    text(1, 23.0, "Step 1: Null Hypothesis", cex = 1.2, font = 2, pos = 4)
    text(1, 23 - distance * 1, expression(H[0]: mu[1] == mu[2]), cex = 1, pos = 4, col = "darkblue")
    
    # Step 2: Group differences
    text(1, 23 - distance * 2, "Step 2: Group Differences", cex = 1.2, font = 2, pos = 4)
    text(1, 23 - distance * 3, bquote(bar(Y)[1] - bar(Y)[2] == .(round(s$group_means$mean[1] - s$group_means$mean[2], 3))), cex = 1, pos = 4, col = "blue")
    #text(1, 23 - distance * 4, expression((bar(Y)[1] - bar(Y)[2])^2), cex = 1, pos = 4, col = "red")
    # Extra space before fraction
    text(1, 23 - distance * 4.5, expression(SSB == frac(n[1]*n[2], n[1]+n[2]) * (bar(Y)[1] - bar(Y)[2])^2), cex = 1, pos = 4, col = "red")
    # Extra space after fraction
    #text(1, 23 - distance * 7, bquote(SSB == .(s$n1) * .(s$n2) / (.(s$n1) + .(s$n2)) * .(round((s$group_means$mean[1] - s$group_means$mean[2])^2, 3))), cex = 0.9, pos = 4, col = "red")
    text(1, 23 - distance * 5.5, bquote(SSB == .(round(s$ss_between, 3))), cex = 1, pos = 4, col = "red")
    
    # Step 3: Within-group variance
    text(1,23 - distance * 6.5, "Step 3: Within-Group Variance", cex = 1.2, font = 2, pos = 4)
    text(1, 23 - distance * 7.5, bquote(s^2 * " (pooled variance) = " * .(round(s$ms_within, 2))), cex = 1, pos = 4, col = "blue")    
    # Extra space before fraction
    text(1, 23 - distance * 9, expression(s^2 == frac((n[1]-1)*s[1]^2 + (n[2]-1)*s[2]^2, n[1]+n[2]-2)), cex = 1, pos = 4, col = "blue")
    # Extra space after fraction
    text(1, 23 - distance * 10.5, expression(MSW == frac(SSW, n[1]+n[2]-2)), cex = 1, pos = 4, col = "red")
    # Extra space after fraction
    text(1, 23 - distance * 11.5, expression(MSW == s^2), cex = 1, pos = 4, col = "red")
    text(1, 23 - distance * 13, expression(SSW == sum((Y[1][i] - bar(Y)[1])^2, i==1, n[1]) + sum((Y[2][i] - bar(Y)[2])^2, i==1, n[2])), cex = 1, pos = 4, col = "red")
    text(1, 23 - distance * 14, bquote(SSW == .(round(s$ss_within, 3))), cex = 0.9, pos = 4, col = "red")
    
    
    # Step 4: Test statistics
    text(6, 23, "Step 4: Test Statistics", cex = 1.2, font = 2, pos = 4)
    text(6, 23 - distance * 1, bquote(t == .(round(s$t_stat, 4))), cex = 1, pos = 4, col = "blue")
    text(6, 23 - distance * 2, bquote(F == .(round(s$f_stat, 4))), cex = 1, pos = 4, col = "red")
    text(6, 23 - distance * 3, bquote(t^2 == .(round(s$t_stat^2, 4))), cex = 1, pos = 4, col = "darkgreen", font = 2)
    text(6, 23 - distance * 4, expression(t^2 == F), cex = 1, pos = 4, col = "darkgreen", font = 2)
    
    
    # Mathematical derivation
    text(10, 23.0, "Mathematical Derivation", cex = 1.2, font = 2, pos = 4)
    # Extra space before fraction
    text(10, 20.9, expression(t == frac(bar(Y)[1] - bar(Y)[2], sqrt(s^2*(1/n[1] + 1/n[2])))), cex = 0.9, pos = 4)
    # Extra space after fraction, before next fraction
    text(10, 18.6, expression(t^2 == frac((bar(Y)[1] - bar(Y)[2])^2, s^2*(1/n[1] + 1/n[2]))), cex = 0.9, pos = 4)
    # Extra space after fraction
    text(10, 16.3, expression(F == frac(MSB, MSW)), cex = 0.9, pos = 4)
    # Extra space after fraction
    text(10, 14.0, expression(F == frac(SSB, n[1]+n[2]-2) / s^2), cex = 0.9, pos = 4)
    # Extra space after fraction
    text(10, 11.7, expression(F == frac(n[1]*n[2], n[1]+n[2]) * (bar(Y)[1] - bar(Y)[2])^2 / s^2), cex = 0.9, pos = 4)
    # Extra space after fraction
    text(10, 9.4, expression(F == frac((bar(Y)[1] - bar(Y)[2])^2, s^2*(1/n[1] + 1/n[2]))), cex = 0.9, pos = 4)
    # Extra space after fraction
    text(10, 7.6, expression(F == t^2), cex = 1, pos = 4, col = "darkgreen", font = 2)
    
    
    # Current values
    text(14, 23, "Current Values:", cex = 1.1, font = 2, pos = 4)
    text(14, 23 - distance * 1, bquote(bar(Y)[1] == .(round(s$group_means$mean[1], 3))), cex = 0.9, pos = 4)
    text(14, 23 - distance * 2, bquote(bar(Y)[2] == .(round(s$group_means$mean[2], 3))), cex = 0.9, pos = 4)
    text(14, 23 - distance * 3, bquote(s^2 == .(round(s$ms_within, 3))), cex = 0.9, pos = 4)
    text(14, 23 - distance * 4, "MSW (same as s²)", cex = 0.9, pos = 4, col = "gray50")
    text(14, 23 - distance * 5, bquote(n[1] == .(s$n1) * "," ~ n[2] == .(s$n2)), cex = 0.9, pos = 4)
    
    
    # Comparison table
    text(1, 3.2, "Comparison Table", cex = 1.3, font = 2, pos = 4)
    
    # Table headers
    distance = 1.2
    text(1, 3.2-distance*1, "Component", cex = 1, font = 2, pos = 4)
    text(4, 3.2-distance*1, "t-test", cex = 1, font = 2, pos = 4)
    text(8, 3.2-distance*1, "ANOVA (2 groups)", cex = 1, font = 2, pos = 4)
    
    # Table rows
    text(1, 3.2-distance*2, "Group difference", cex = 1, pos = 4)
    text(4, 3.2-distance*2, expression(bar(Y)[1] - bar(Y)[2]), cex = 1, pos = 4, col = "blue")
    text(8, 3.2-distance*2, "via SSB", cex = 1, pos = 4, col = "red")
    
    text(1, 3.2-distance*3, "Within variance", cex = 1, pos = 4)
    text(4, 3.2-distance*3, expression(s^2), cex = 1, pos = 4, col = "blue")
    text(8, 3.2-distance*3, "MSW", cex = 1, pos = 4, col = "red")
    
    text(1, 3.2-distance*4, "Test statistic", cex = 1, pos = 4)
    text(4, 3.2-distance*4, "t", cex = 1, pos = 4, col = "blue")
    text(8, 3.2-distance*4, expression(F == t^2), cex = 1, pos = 4, col = "red")
    
    text(1, 3.2-distance*5, "Null hypothesis", cex = 1, pos = 4)
    text(4, 3.2-distance*5, expression(mu[1] == mu[2]), cex = 1, pos = 4)
    text(8, 3.2-distance*5, expression(mu[1] == mu[2]), cex = 1, pos = 4)
    
    # Key insight
    text(14, 3.2, "Key Insight:", cex = 1.2, font = 2)
    text(14, 3.2-distance*1, "Both formulas capture the same information:", cex = 1)
    text(14, 3.2-distance*2, "• Group differences (signal)", cex = 1)
    text(14, 3.2-distance*3, "• Within-group variation (noise)", cex = 1)
    text(14, 3.2-distance*4, "• Same underlying assumptions", cex = 1)
    text(14, 3.2-distance*5, "• Identical p-values", cex = 1)

  })

  # 4. T-distribution
  output$t_distribution <- renderPlot({
    s <- stats()

    # Generate t-distribution
    df <- s$n1 + s$n2 - 2
    x <- seq(-4, 4, length.out = 1000)
    y <- dt(x, df)

    # Calculate critical values
    alpha <- input$alpha
    t_critical <- qt(1 - alpha/2, df)

    # Create plot
    df_plot <- data.frame(x = x, y = y)

    ggplot(df_plot, aes(x = x, y = y)) +
      geom_line(color = "blue", linewidth = 1) +

      # Shade rejection regions
      geom_ribbon(data = subset(df_plot, x <= -t_critical),
                  aes(ymax = y), ymin = 0, fill = "red") +
      geom_ribbon(data = subset(df_plot, x >= t_critical),
                  aes(ymax = y), ymin = 0, fill = "red") +

      # Mark observed t-statistic
      geom_vline(xintercept = s$t_stat, color = "darkgreen", linewidth = 2) +
      geom_vline(xintercept = -s$t_stat, color = "darkgreen", linewidth = 2) +

      # Annotations
      annotate("text", x = s$t_stat + 1.1, y = max(y) * 0.8,
               label = paste("t =", round(s$t_stat, 3)),
               color = "darkgreen", fontface = "bold") +

      annotate("text", x = 0, y = 0.05,
               label = paste("Critical t = ±", round(t_critical, 3)),
               color = "red", size = 3) +

      labs(title = "T-Distribution",
           subtitle = paste("df =", df),
           x = "t-statistic", y = "Density") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(face = "italic"))
  })

  # 4. F-distribution
  output$f_distribution <- renderPlot({
    s <- stats()

    # Generate F-distribution
    df1 <- 1  # Between groups df
    df2 <- s$n1 + s$n2 - 2  # Within groups df
    x <- seq(0, 10, length.out = 1000)
    y <- df(x, df1, df2)

    # Calculate critical value
    alpha <- input$alpha
    f_critical <- qf(1 - alpha, df1, df2)

    # Create plot
    df_plot <- data.frame(x = x, y = y)

    ggplot(df_plot, aes(x = x, y = y)) +
      geom_line(color = "red", linewidth = 1) +

      # Shade rejection region
      geom_ribbon(data = subset(df_plot, x >= f_critical),
                  aes(ymax = y), ymin = 0, fill = "red") +
      geom_vline(xintercept = f_critical, color = "red", linewidth = 1, linetype = "dashed") +
      # Mark observed F-statistic
      geom_vline(xintercept = s$f_stat, color = "darkgreen", linewidth = 2) +

      # Mark t²
      geom_vline(xintercept = s$t_stat^2, color = "blue", linewidth = 2, linetype = "dashed") +

      # Annotations
      annotate("text", x = s$f_stat + 1.4, y = 3.25,
               label = paste("F =", round(s$f_stat, 3)),
               color = "darkgreen", fontface = "bold") +

      annotate("text", x = s$t_stat^2 + 1.4, y = 3,
               label = paste("t² =", round(s$t_stat^2, 3)),
               color = "blue", fontface = "bold") +

      annotate("text", x = f_critical + 1.7, y = 3,
               label = paste("Critical F =", round(f_critical, 3)),
               color = "red") +

      labs(title = "F-Distribution",
           subtitle = paste("df1 =", df1, ", df2 =", df2),
           x = "F-statistic", y = "Density") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(face = "italic"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)