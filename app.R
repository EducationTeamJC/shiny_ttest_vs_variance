library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(patchwork)

# UI
ui <- fluidPage(
  titlePanel("T-Test vs ANOVA: Understanding the Relationship"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Data Parameters"),
      numericInput("n1", "Sample Size Group 1:", value = 20, min = 5, max = 100),
      numericInput("n2", "Sample Size Group 2:", value = 20, min = 5, max = 100),
      numericInput("effect_size", "Effect Size (Difference in Means):", value = 1, min = 0, max = 10, step = 0.1),
      numericInput("sd", "Standard Deviation:", value = 1, min = 0.1, max = 5, step = 0.1),
      numericInput("alpha", "Alpha Level:", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
      
      hr(),
      
      h4("Results Summary"),
      verbatimTextOutput("results_summary"),
      
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Comparing t-test and ANOVA", 
                 fluidRow(
                   column(6, plotOutput("ttest_plot", height = "400px")),
                   column(6, plotOutput("anova_plot", height = "400px"))
                 ),
                 fluidRow(
                   column(12, plotOutput("variance_decomp", height = "200px"))
                 )
        ),
        
        tabPanel("t² = F visualisation",
                 plotOutput("effect_size_plot", height = "500px")
        ),
        
        tabPanel("Comparing the math",
                 plotOutput("math_bridge", height = "600px")
        ),
        
        tabPanel("Comparing the distributions",
                 fluidRow(
                   column(6, plotOutput("t_distribution", height = "400px")),
                   column(6, plotOutput("f_distribution", height = "400px"))
                 )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
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
      t_stat = t_stat,
      t_pvalue = t_pvalue,
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
    cat("T-Test Results:\n")
    cat("t-statistic:", round(s$t_stat, 4), "\n")
    cat("p-value:", round(s$t_pvalue, 6), "\n\n")
    
    cat("ANOVA Results:\n")
    cat("F-statistic:", round(s$f_stat, 4), "\n")
    cat("p-value:", round(s$f_pvalue, 6), "\n\n")
    
    cat("Verification:\n")
    cat("t² =", round(s$t_stat^2, 4), "\n")
    cat("F =", round(s$f_stat, 4), "\n")
    cat("t² = F:", round(s$t_stat^2, 4) == round(s$f_stat, 4), "\n")
    cat("p-values equal:", round(s$t_pvalue, 6) == round(s$f_pvalue, 6), "\n")
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
    par(mar = c(0, 0, 0, 0))
    plot(1, 1, type = "n", xlim = c(0, 12), ylim = c(0, 12), 
         axes = FALSE, xlab = "", ylab = "")
    
    # Title
    text(6, 11.5, "Mathematical Relationship: t² = F", cex = 1.5, font = 2)
    
    # T-test section
    text(1, 10.5, "T-Test Formula:", cex = 1.2, font = 2, pos = 4)
    text(1, 10, "t = (x̄₁ - x̄₂) / √(s²(1/n₁ + 1/n₂))", cex = 1, pos = 4)
    
    # T-test variables explanation
    text(1, 9.5, "Variables:", cex = 1.1, font = 2, pos = 4)
    text(1, 9.2, "x̄₁ = mean of group 1", cex = 0.9, pos = 4)
    text(1, 8.9, "x̄₂ = mean of group 2", cex = 0.9, pos = 4)
    text(1, 8.6, "s² = variance", cex = 0.9, pos = 4)
    text(1, 8.3, "n₁ = sample size group 1", cex = 0.9, pos = 4)
    text(1, 8.0, "n₂ = sample size group 2", cex = 0.9, pos = 4)
    
    # T-test actual values
    text(1, 7.5, "Current Values:", cex = 1.1, font = 2, pos = 4)
    text(1, 7.2, paste("x̄₁ =", round(s$group_means$mean[1], 3)), cex = 0.9, pos = 4, col = "blue")
    text(1, 6.9, paste("x̄₂ =", round(s$group_means$mean[2], 3)), cex = 0.9, pos = 4, col = "blue")
    text(1, 6.6, paste("s² =", round(s$ms_within, 3)), cex = 0.9, pos = 4, col = "blue")
    text(1, 6.3, paste("n₁ =", s$n1), cex = 0.9, pos = 4, col = "blue")
    text(1, 6.0, paste("n₂ =", s$n2), cex = 0.9, pos = 4, col = "blue")
    
    # T-test result
    text(1, 5.5, paste("t =", round(s$t_stat, 4)), cex = 1.1, pos = 4, col = "darkblue", font = 2)
    
    # ANOVA section
    text(7, 10.5, "ANOVA Formula:", cex = 1.2, font = 2, pos = 4)
    text(7, 10, "F = MS_between / MS_within", cex = 1, pos = 4)
    
    # ANOVA variables explanation
    text(7, 9.5, "Variables:", cex = 1.1, font = 2, pos = 4)
    text(7, 9.2, "MS_between = between-group mean square", cex = 0.9, pos = 4)
    text(7, 8.9, "MS_within = within-group mean square", cex = 0.9, pos = 4)
    text(7, 8.6, "df_between = between-group degrees of freedom", cex = 0.9, pos = 4)
    text(7, 8.3, "df_within = within-group degrees of freedom", cex = 0.9, pos = 4)
    
    # ANOVA actual values
    text(7, 7.5, "Current Values:", cex = 1.1, font = 2, pos = 4)
    text(7, 7.2, paste("MS_between =", round(s$ms_between, 3)), cex = 0.9, pos = 4, col = "red")
    text(7, 6.9, paste("MS_within =", round(s$ms_within, 3)), cex = 0.9, pos = 4, col = "red")
    text(7, 6.6, paste("df_between =", 1), cex = 0.9, pos = 4, col = "red")
    text(7, 6.3, paste("df_within =", s$n1 + s$n2 - 2), cex = 0.9, pos = 4, col = "red")
    
    # ANOVA result
    text(7, 5.5, paste("F =", round(s$f_stat, 4)), cex = 1.1, pos = 4, col = "darkred", font = 2)
    
    # Connection
    arrows(4, 5.5, 6, 5.5, length = 0.2, lwd = 2, col = "darkgreen")
    text(5, 5.0, "t² = F", cex = 1.2, font = 2, col = "darkgreen")
    
    
    # Shared components
    text(1, 3.5, "Shared Components:", cex = 1.2, font = 2, pos = 4)
    text(1, 3.2, "• Pooled variance estimate (MS_within or s²)", cex = 1, pos = 4)
    text(1, 2.9, "• Degrees of freedom (df_within = n₁ + n₂ - 2)", cex = 1, pos = 4)
    text(1, 2.6, "• Same underlying assumptions", cex = 1, pos = 4)
    text(1, 2.3, "• Identical p-values", cex = 1, pos = 4)
    
    # Key insight
    text(7, 3.5, "Key Insight:", cex = 1.2, font = 2, pos = 4)
    text(7, 3.2, "For 2 groups, t-test and", cex = 1, pos = 4)
    text(7, 2.9, "ANOVA are mathematically equivalent!", cex = 1, pos = 4)
    
    # P-value relationship
    text(6, 1.5, paste("Both tests give identical p-values:", round(s$t_pvalue, 6)), 
         cex = 1, font = 2, col = "purple")
    
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
                  aes(ymax = y), ymin = 0, fill = "red", alpha = 0.3) +
      geom_ribbon(data = subset(df_plot, x >= t_critical), 
                  aes(ymax = y), ymin = 0, fill = "red", alpha = 0.3) +
      
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
                  aes(ymax = y), ymin = 0, fill = "red", alpha = 0.3) +
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
               color = "red", size = 3) +
      
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