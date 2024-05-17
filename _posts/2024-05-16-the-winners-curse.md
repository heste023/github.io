One of the things that I've learned working through Statistical Rethinking (that I was never taught in graduate school!) is how including bad control variables in an analysis can bias results. I've been doing some work trying to better clarify my thoughts on propensity score analysis recently, and I wanted to explore the impact of including irrelevant variables in a propensity score analysis. 

I always like to start simple, so I built a synthetic dataset from an area of research that I'm familiar with and began working through a simple regression problem before moving on to the more sophisticated technique of a propensity score analysis. The idea was to get comfortable with my understanding using the simple regression analysis, then move on to the propensity score work with a basic understanding in hand. But along the way, I ran up on something that I knew, but that's never showed itself to me as plainly.  

One of the issues with inference around statistical significance is the problem of "publication bias."  The idea behind publication bias is that because so much is made about statistical significance, there's a bias in academic research towards studies that show statistically significant results. Consider a toy example: Imagine that there are several universities studying the effectiveness of a particular teaching method on student engagement.  Each study uses different methods or variations and compares them to traditional lecture-based teaching. 

Study 1: Finds that using interactive multimedia increases student engagement significantly (p < 0.05).

Study 2: Finds that peer teaching slightly improves engagement, but the results are not statistically significant (p > 0.05).

Study 3: Finds that flipped classrooms significantly increase student engagement (p < 0.01).

Study 4: Finds no significant difference in engagement when using online discussion forums (p > 0.05).

Because Study 1 and Study 3 have results that are statistically significant, they are more likely to be published in peer-reviewed academic journals. Because the studies are published, they're discussed widely and perhaps even implemented as methods to improve student engagement. The other methods, on the other hand, get ignored, even though effect sizes for each intervention can be meaningful or the results could have happened by chance. 

This effect has been widely documented across numerous academic disciplines ([here's](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=43eb85dd4af8c64cf6bacba73c39b1027606bcdf)  an example from the political science literature). The net effect is that there distortions in the takeaways from bodies of research. The chain of events runs like this: 

- Authors are less likely to submit studies with non-significant results, because journals and reviewers tend to reject studies that lack statistically-significant results
- The entire body of research becomes skewed towards positive findings
- This leads to an overestimation of the effectiveness of the studied interventions or phenomena. 
 
This skewed perspective then influences subsequent research, policy decisions, and practical applications, perpetuating a cycle where only certain types of results are valued and disseminated, further distorting any understanding of true effects and contributing to inefficiencies or even harm in applied settings. 

And in an odd way, trying to understand how bad control variables work in a propensity score analysis led me to right to it. 

Here's how I got there. In the past, in my work in higher education, I've worked with non-academic units that wanted to understand how living on campus effects a student's performance. Effects are hard to disentangle in this type of research because there are so many factors that can impact a student's ability to succeed in college, but in general, I would argue that the causal structure of living on campus (if there is a true effect) looks like this:

```
library(dagitty)

dag_3 <- dagitty("dag {
  AbilityOrWorkEthic [Unobserved]
  Income [Unobserved]
  OnCampus -> Performance
  Pell -> OnCampus
  Age -> OnCampus -> Performance
  Age -> Performance
  FirstGen -> SATScore -> Performance
  FirstGen -> Performance 
  FirstGen -> OnCampus
  SATScore -> Scholarship -> OnCampus -> Performance
  SATScore -> Performance 
  HSGPA -> Performance 
  HSGPA -> Scholarship -> OnCampus -> Performance
  AbilityOrWorkEthic -> SATScore -> Performance
  AbilityOrWorkEthic -> HSGPA -> Performance
  Income -> SATScore -> Performance
  Income -> FirstGen -> Performance
  Income -> Pell -> Performance
  Scholarship -> Performance
  Female -> STEM -> Performance
  Female -> Performance
  SATScore -> STEM -> Performance
  UnderrepMinority -> STEM -> Performance
} ")
plot(dag_3)
```

Using the [daggity](https://www.dagitty.net/) package, we can plot this structure and get the variables that we need to include in our regression that, according to the DAG, will allow us to estimate a causal effect without confounding: 

```
> adjustmentSets(dag_3, exposure = 'OnCampus', outcome = 'Performance')
{ Age, FirstGen, Pell, Scholarship }
```

So we know that at a minimum, we will need to include "Age," "FirstGen," "Pell," and "Scholarship" in our model.

The next step was to simulate a dataset that mirrors the structure of dag_3. One of the things that I was interested in was if I could recover a zero effect - in other words, what would it look like in the model if living on campus (OnCampus) had no effect on performance? And how does that look when including variables beyond the minimal adjustment set of the causal structure? 

So I started with by simulating a synthetic dataset that mirrored the structure of the DAG, like this: 

```
library(broom)
library(dplyr)
library(ggplot2)
library(rethinking)

N <- 1000
Age_sim <- rnorm(N)
Ability_sim <- rnorm(N)
Income_sim <- rnorm(N)
Female_sim <- sample(1:2, size=N, replace=TRUE)
UnderrepMinority_sim <- rbinom(N, 1, 0.2) + 1
FirstGen_sim <- rbinom(N, 1, plogis(-0.5 * Income_sim)) + 1
SATScore_sim <- Ability_sim + 0.3 * Income_sim - 0.5 * FirstGen_sim + rnorm(N)
HSGPA_sim <- Ability_sim + rnorm(N)

STEM_prob <- plogis(0.1 * SATScore_sim + 0.1 * HSGPA_sim + 0.05 * (Female_sim - 1) + 0.05 * (UnderrepMinority_sim - 1))
STEM_sim <- rbinom(N, 1, STEM_prob) + 1

Scholarship_sim <- ifelse(SATScore_sim + 0.5 * HSGPA_sim > quantile(SATScore_sim + 0.5 * HSGPA_sim, 0.75), 1, 0) + 1
Pell_prob <- plogis(-0.5 * Income_sim)
Pell_sim <- rbinom(N, 1, Pell_prob) + 1

composite_score <- -0.2 * Age_sim + 0.5 * Scholarship_sim + 0.3 * Pell_sim + 0.4 * FirstGen_sim
composite_prob <- plogis(composite_score)
OnCampus_sim <- rbinom(N, 1, composite_prob)

Performance_sim <- rnorm(N, Age_sim + FirstGen_sim + SATScore_sim + HSGPA_sim + Scholarship_sim + 0 * OnCampus_sim + Female_sim + STEM_sim + Pell_sim)

df_std <- data.frame(
    A = standardize(Age_sim),
    I = standardize(Income_sim),
    F = standardize(FirstGen_sim),
    S = standardize(SATScore_sim),
    H = standardize(HSGPA_sim),
    Sch = standardize(Scholarship_sim),
    O = standardize(OnCampus_sim),
    P = standardize(Performance_sim),
    Fe = standardize(Female_sim),
    STEM = standardize(STEM_sim),
    URM = standardize(UnderrepMinority_sim),
    Pell = standardize(Pell_sim)
  )
```

Importantly, OnCampus_sim has a "0" effect on Performance_sim; this was the effect that I was trying to recover. 

Next, I built two separate regression models, an "optimal" model that only included the minimal adjustment set, and a "full" model that included all of the additional measured variables in the synthetic dataset. Here's the code for that: 

```
linear_reg_optimal <- lm(P ~ O + A + Sch + F + Pell, data = df_std)
  linear_reg_full <- lm(P ~ O + A + F + S + H + Sch + Fe + STEM + URM + Pell, data = df_std)
```

Because I simulated the dataset, there was some random variation for each run. On some runs, the models would recover the "0" effect of OnCampus that I was looking for. On other runs, though, the results would occasionally be significant and positive, or significant and negative, mostly due to random variation in the data generating process. 

To try and better understand that variation, I converted the code that I had into a function that would simulate the data generating process multiple times, run the regressions, log the coefficient value for OnCampus (and whether it was significant), and graph the results as a density distribution. Here's that code: 

```
library(broom)
library(dplyr)
library(ggplot2)
library(rethinking)

# Set the number of iterations and sample size
num_iterations <- 5000
N <- 5000

# Create an empty dataframe to store the results
results_df <- data.frame(
  iteration = integer(),
  model = character(),
  estimate = numeric(),
  p.value = numeric(),
  stringsAsFactors = FALSE
)

# Run the simulation and capture the results
for (i in 1:num_iterations) {
  # Data simulation
  Age_sim <- rnorm(N)
  Ability_sim <- rnorm(N)
  Income_sim <- rnorm(N)
  Female_sim <- sample(1:2, size = N, replace = TRUE)
  UnderrepMinority_sim <- rbinom(N, 1, 0.2) + 1
  FirstGen_sim <- rbinom(N, 1, plogis(-0.5 * Income_sim)) + 1
  SATScore_sim <- Ability_sim + 0.3 * Income_sim - 0.5 * FirstGen_sim + rnorm(N)
  HSGPA_sim <- Ability_sim + rnorm(N)
  STEM_prob <- plogis(0.1 * SATScore_sim + 0.1 * HSGPA_sim + 0.05 * (Female_sim - 1) + 0.05 * (UnderrepMinority_sim - 1))
  STEM_sim <- rbinom(N, 1, STEM_prob) + 1
  Scholarship_sim <- (ifelse(SATScore_sim + 0.5 * HSGPA_sim > quantile(SATScore_sim + 0.5 * HSGPA_sim, 0.75), 1, 0) + 1)
  Pell_prob <- plogis(-0.5 * Income_sim)
  Pell_sim <- rbinom(N, 1, Pell_prob) + 1
  composite_score <- -0.2 * Age_sim + 0.5 * Scholarship_sim + 0.3 * Pell_sim + 0.4 * FirstGen_sim
  composite_prob <- plogis(composite_score)
  OnCampus_sim <- rbinom(N, 1, composite_prob)
  Performance_sim <- rnorm(N, Age_sim + FirstGen_sim + SATScore_sim + HSGPA_sim + Scholarship_sim + 0 * OnCampus_sim + Female_sim + STEM_sim + Pell_sim)
  
  df_std <- data.frame(
    A = standardize(Age_sim),
    I = standardize(Income_sim),
    F = standardize(FirstGen_sim),
    S = standardize(SATScore_sim),
    H = standardize(HSGPA_sim),
    Sch = standardize(Scholarship_sim),
    O = standardize(OnCampus_sim),
    P = standardize(Performance_sim),
    Fe = standardize(Female_sim),
    STEM = standardize(STEM_sim),
    URM = standardize(UnderrepMinority_sim),
    Pell = standardize(Pell_sim)
  )
  
  # Regression on the unmatched dataset
  linear_reg_optimal <- lm(P ~ O + A + Sch + F + Pell, data = df_std)
  linear_reg_full <- lm(P ~ O + A + F + S + H + Sch + Fe + STEM + URM + Pell, data = df_std)
  
  # Extract the OnCampus coefficient and p-value from the optimal model
  optimal_results <- tidy(linear_reg_optimal) %>% filter(term == "O")
  
  # Extract the OnCampus coefficient and p-value from the full model
  full_results <- tidy(linear_reg_full) %>% filter(term == "O")
  
  # Append the results to the dataframe
  results_df <- bind_rows(
    results_df,
    data.frame(
      iteration = i,
      model = "Optimal",
      estimate = optimal_results$estimate,
      p.value = optimal_results$p.value
    ),
    data.frame(
      iteration = i,
      model = "Full",
      estimate = full_results$estimate,
      p.value = full_results$p.value
    )
  )
}

# Add a new column to indicate significance
results_df$significant <- ifelse(results_df$p.value < 0.05, "Significant", "Non-significant")

# Create a summary dataframe with counts of significant and non-significant results
results_summary <- results_df %>%
  group_by(model, significant) %>%
  summarise(count = n(), .groups = "drop")

# Define the x and y positions for the annotations
annotation_positions <- data.frame(
  model = c("Full", "Full", "Optimal", "Optimal"),
  significant = c("Non-significant", "Significant", "Non-significant", "Significant"),
  x = c(-0.005, -0.005, -0.005, -0.005),
  y = c(100, 70, 100, 70)
)

# Merge the annotation positions with the results_summary dataframe
results_summary <- merge(results_summary, annotation_positions, by = c("model", "significant"))

# Plot the OnCampus coefficient estimates with significance color-coding and count annotations
ggplot(results_df, aes(x = estimate, fill = significant), height = 6) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "OnCampus Coefficient Estimate", y = "Density of Estimates") +
  scale_fill_manual(name = "Significance",
                    values = c("Non-significant" = "blue", "Significant" = "gray")) + 
  theme_minimal() +
  facet_wrap(~ model) +
  geom_text(data = results_summary, aes(x = x, y = y, label = paste0(significant, ": ", count)),
            size = 5, fontface = "bold") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
```

And here's where it gets interesting! Around 5% of the time, because of random variation in the data simulating process, in both the Optimal and Full models, the coefficient value for OnCampus was positive or negative _and significant_. 

But because those results are positive or negative and significant, they are the most likely results to be published! The true effect of OnCampus on Performance is zero - I wrote it into the code - but in a real-world setting, a study that recovers the true value in a research setting is far less likely to be published than studies recovering the true effect. 

In other words, the results that were simply noise are most likely to be published, discussed, and to effect policy. This is a big deal! 

The Bayesian approach to this problem performed similarly for correctly recovering the coefficient value for OnCampus. Here's some code for it: 

```
library(rethinking)
library(ggplot2)
library(dplyr)
library(plotly)

simulate_and_model <- function(runs) {
  results <- vector("list", runs)  # Pre-allocate a list to store results
  
  for (i in 1:runs) {
    # Simulate dataset
    N <- 1000
    Age_sim <- rnorm(N)
    Ability_sim <- rnorm(N)
    Income_sim <- rnorm(N)
    Female_sim <- sample(1:2, size=N, replace=TRUE)
    UnderrepMinority_sim <- rbinom(N, 1, 0.2) + 1
    FirstGen_sim <- rbinom(N, 1, plogis(-0.5 * Income_sim)) + 1
    SATScore_sim <- Ability_sim + 0.3 * Income_sim - 0.5 * FirstGen_sim + rnorm(N)
    HSGPA_sim <- Ability_sim + rnorm(N)
    
    STEM_prob <- plogis(0.1 * SATScore_sim + 0.1 * HSGPA_sim + 0.05 * (Female_sim - 1) + 0.05 * (UnderrepMinority_sim - 1))
    STEM_sim <- rbinom(N, 1, STEM_prob) + 1
    
    Scholarship_sim <- ifelse(SATScore_sim + 0.5 * HSGPA_sim > quantile(SATScore_sim + 0.5 * HSGPA_sim, 0.75), 1, 0) + 1
    Pell_prob <- plogis(-0.5 * Income_sim)
    Pell_sim <- rbinom(N, 1, Pell_prob) + 1
    
    composite_score <- -0.2 * Age_sim + 0.5 * Scholarship_sim + 0.3 * Pell_sim + 0.4 * FirstGen_sim
    composite_prob <- plogis(composite_score)
    OnCampus_sim <- rbinom(N, 1, composite_prob)
    
    Performance_sim <- rnorm(N, Age_sim + FirstGen_sim + SATScore_sim + HSGPA_sim + Scholarship_sim + 0 * OnCampus_sim + Female_sim + STEM_sim + Pell_sim)
    
    dat_sim <- list(
      A = standardize(Age_sim),
      I = standardize(Income_sim),
      F = standardize(FirstGen_sim),
      S = standardize(SATScore_sim),
      H = standardize(HSGPA_sim),
      Sch = standardize(Scholarship_sim),
      O = standardize(OnCampus_sim),
      P = standardize(Performance_sim),
      Fe = standardize(Female_sim),
      STEM = standardize(STEM_sim),
      URM = standardize(UnderrepMinority_sim),
      Pell = standardize(Pell_sim)
    )
    
    # Model fitting
    m_adjusted <- quap(
      alist(
        P ~ dnorm(mu, sigma),
        mu <- a + bO * O + bA * A + bSch * Sch + bPell * Pell + bfstgen * F,
        a ~ dnorm(0, 1),        
        bO ~ dnorm(0.5, 0.5),    
        bA ~ dnorm(0, 1),
        bSch ~ dnorm(0.5, 0.5),
        bPell ~ dnorm(-0.5, 1),
        bfstgen ~ dnorm(-0.5, 1),
        sigma ~ dexp(1)         
      ),
      data = dat_sim
    )
    
    # Extracting and storing the OnCampus coefficient
    model_precis <- precis(m_adjusted)
    if ("bO" %in% rownames(model_precis)) {
      bO_coef <- model_precis["bO", , drop = FALSE]
      results[[i]] <- bO_coef
    } else {
      results[[i]] <- data.frame(Parameter = "bO", Estimate = NA, `2.5%` = NA, `97.5%` = NA)
    }
  }
  
  # Convert results list to data frame
  results_df <- do.call(rbind, results)
  rownames(results_df) <- NULL  # Clean up row names for neatness
  return(results_df)
}

# Running the function
results_df <- simulate_and_model(100)  
head(results_df)

# Create an empty data frame to store the data for all rows
data_all <- data.frame(x = numeric(), density = numeric(), row = integer(), includes_zero = logical())

# Loop through each row of results_df
for (i in 1:nrow(results_df)) {
  # Select the current row
  row <- results_df[i, ]
  
  # Generate a sequence of x-values for the plot
  x <- seq(from = row$`5.5%`, to = row$`94.5%`, length.out = 100)
  
  # Calculate the density values using dnorm()
  density <- dnorm(x, mean = row$mean, sd = row$sd)
  
  # Check if the distribution includes zero
  includes_zero <- ifelse(row$`5.5%` <= 0 & row$`94.5%` >= 0, TRUE, FALSE)
  
  # Create a data frame with x, density, row, and includes_zero values
  data <- data.frame(x = x, density = density, row = i, includes_zero = includes_zero)
  
  # Append the data to data_all
  data_all <- rbind(data_all, data)
}

# Use aggregate to group by 'row' and evaluate the logical condition once per group
unique_rows <- aggregate(includes_zero ~ row, data = data_all, FUN = any)

# Count true and false values
num_includes_zero <- sum(unique_rows$includes_zero)
num_excludes_zero <- nrow(unique_rows) - num_includes_zero

# Create the plot using ggplot() with separate annotations
plot <- ggplot(data_all, aes(x = x, y = density, group = row, color = includes_zero)) +
  geom_line(size = 0.7, alpha = 0.7) +
  scale_color_manual(values = c("steelblue", "red"), 
                     labels = c("Excludes 0", "Includes 0"),
                     name = "") +
  labs(title = "Distributions for All Rows",
       x = "Value",
       y = "Density") +
  theme_minimal(base_size = 14) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "bottom") +
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
           label = paste("Number of distributions that include zero:", num_includes_zero),
           size = 5) +
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 2,
           label = paste("Number of distributions that exclude zero:", num_excludes_zero),
           size = 5)

plot
```

But the Bayesian approach is different in one crucial way - Bayesians don't care about statistical significance, so there's less opportunity for publication bias to impact the results! The posterior distribution is the only estimator for Bayesian analyses, and posterior distributions embody the universe of possible values for the data, given the assumptions of the model. The true coefficient value - 0 in this case - is just as likely to be published, discussed, and to effect policy as the results that capture the random noise in the data generating process. 

So while both the Frequentist and Bayesian approaches perform similarly in recovering the true effect, the Bayesian approach is less likely to contribute to publication bias. There are a numerous reasons that the Bayesian approach to statistical analysis is superior, and this is perhaps another one. 
