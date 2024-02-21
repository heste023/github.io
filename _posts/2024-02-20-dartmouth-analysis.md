## Are Standardized Test Scores Making a Comeback?

I stumbled on this report on hackernews a week or two ago:
<https://home.dartmouth.edu/sites/home/files/2024-02/sat-undergrad-admissions.pdf>.
The report makes the claim that the use of standardized test scores “is
an essential method by which Admissions can identify applicants who will
succeed at Dartmouth.” Knowing a bit about this area of higher education
research, I was pulled in pretty quickly. Most of the recent research
considers high school GPA or personal factors like the ability to
overcome obstacles a better predictor of college student success. So
this is a pretty big claim!

The original thought in the report is nifty; the authors suggest that
students who have higher standardized test scores relative to their
local educational environment should submit their scores - even under a
test-optional application policy - because a higher relative
standardized test score could then be used by Admissions as a way to
identify them as someone who is likely to succeed. Here’s a line from
the report: “many high-achieving less-advantaged applicants choose not
to submit scores even when doing so would allow Admissions to identify
them as students likely to succeed at Dartmouth and in turn benefit
their application.”

This makes sense! If you’re an applicant to an Ivy League school, you
want to stand out, and if you’re a student who comes from a
disadvantaged background, getting a pretty-high score is a way that you
can show you’re capable. In other words, your standardized test scores,
if respectable on a global scale, are a good indicator of you being
exceptional if your relative score is an outlier compared to other
students in your high school. The idea reminded me of the research on
happiness and income - a certain level of income is needed for
happiness, but relative income has a larger effect on happiness. Again,
this makes sense.

When I looked at the evidence provided for the conclusion, though, I was
disappointed. Why?

Here are the conclusions and why I remain skeptical.

Conclusion One: SAT (and ACT) scores are highly predictive of academic
achievement at Dartmouth.

As evidence, the authors show a linear relationship between SAT scores
and first-year GPA (in a visual) combined with a regression using
R-squared as a measure of fit to claim that, “consideration of SAT
scores allows Admissions to identify applicants who will thrive
academically at Dartmouth better than does the use of high-school GPA
alone.” No surprise here. In general, having another measurement almost
always improves predictive accuracy, both statistically and practically.
So conceptually, I agree - the problem for this analysis, though, is
that you can’t say anything about predictive power using R-squared.

R-squared is a measure of correlation, not accuracy, and can be anywhere
between 0 and 1 just by changing the range of a predictor. There are
many other metrics that would be better here, but just for fun, let’s
consider the most common metric used for prediction in regression,
mean-squared-error, in a simulation alongside R-squared.
Mean-squared-error (MSE) measures the amount of error between the
observed and predicted values from the regression. Here’s a little
simulation (NOTE: the first part below is copied from Clay Ford’s blog
[post](https://library.virginia.edu/data/articles/is-r-squared-useless)
where I first learned about the qualities of R-squared):

    x <- seq(1,10,length.out = 100)
    set.seed(1)
    y <- 2 + 1.2*x + rnorm(100,0,sd = 0.9)
    mod1 <- lm(y ~ x)
    summary(mod1)$r.squared

    ## [1] 0.9383379

    sum((fitted(mod1) - y)^2)/100  # MSE

    ## [1] 0.6468052

    x <- seq(1,2,length.out = 100)       
    set.seed(1)
    y <- 2 + 1.2*x + rnorm(100,0,sd = 0.9)
    mod2 <- lm(y ~ x)
    summary(mod2)$r.squared

    ## [1] 0.1502448

    sum((fitted(mod2) - y)^2)/100  # MSE

    ## [1] 0.6468052

In this case, the MSE for both models are the same, but the R-squared
values are different. In other words, the in-sample prediction error is
the exact same, so inferring anything about the predictive ability using
R-squared is an illusion.

Here’s another simulation that likely flies closer to the data in the
report. Suppose, for example, that the range of variation in the SAT
predictor is greater than the range of variation in applicant
high-school GPA, like this:

    x <- seq(1200, 1600, length.out = 100)
    set.seed(1)
    y <- 2 + 1.2*x + rnorm(100, 0, sd = 0.2)
    mod1 <- lm(y ~ x)
    summary(mod1)$r.squared

    ## [1] 0.9999984

    sum((fitted(mod1) - y)^2)/100 

    ## [1] 0.031941

    x <- seq(3.0, 4.5, length.out = 100)
    set.seed(1)
    y <- 2 + 1.2*x + rnorm(100, 0, sd = 0.2)
    mod2 <- lm(y ~ x)
    summary(mod2)$r.squared

    ## [1] 0.8951616

    sum((fitted(mod2) - y)^2)/100 

    ## [1] 0.031941

Here, R-squared is worse on the second model, only because there is less
variation in the predictor. Again, the in-sample MSE is exactly the
same! But that’s not all - it can also be that models with a lower
R-squared value are *better* at prediction. For example, here’s a
simulation where there’s a bit more noise on one predictor:

    x1 <- seq(1200, 1600, length.out = 100)
    set.seed(1)
    y1 <- 2 + 1.2 * x1 + rnorm(100, 0, sd = 1)  # Increased noise
    mod1 <- lm(y1 ~ x1)
    summary(mod1)$r.squared

    ## [1] 0.9999592

    sum((fitted(mod1) - y1)^2) / length(y1)

    ## [1] 0.7985249

    x2 <- seq(3.5, 5, length.out = 100)  # Narrower range of x
    set.seed(1)
    y2 <- 2 + 1.2 * x2 + rnorm(100, 0, sd = 0.5)  # Reduced noise
    mod2 <- lm(y2 ~ x2)
    summary(mod2)$r.squared

    ## [1] 0.5737059

    sum((fitted(mod2) - y2)^2) / length(y2)

    ## [1] 0.1996312

So this line from the report, quoted below, is *way* off:

“In column 1, SAT by itself explains about 22% of the variation in
first-year GPA. High school GPA by itself explains 9% of the variation
(column 2). The explained variation in first-year GPA rises to 25% when
we include both high school GPA and SAT scores as predictors (column 3).
In other words, *the marginal contribution of high school GPA above SAT
is only 3%.*”

This is a flippant use of R-squared - R-squared never measures marginal
contribution added.

So what should they have done? In machine learning, which is ahead of
the economics field in it’s understanding of what makes a good
prediction, out-of-sample error - how well a model predicts on unseen
data - is the standard way to gauge a model’s predictive capabilities.
There are a few different ways to get this value manually or approximate
it, and R-squared is not one of those methods. To add, there are models
that have built-in qualities to measure which variables actually have
more predictive power - decision-tree based models, for example.

In short - I think the intuition behind the argument is probably right!
Standardized test scores are likely highly predictive of first-year
success at Dartmouth. You just can’t make the claim that it’s more
predictive than high-school GPA with this approach.

Conclusion Two: SAT is a strong predictor of academic success at
Dartmouth for all subgroups.

The author’s first conclusion is weakly supported by the analysis - what
about the evidence provided for this claim? Again, this section
references the tables with regression, using R-squared to make claims
about prediction abilities. It’s fine, but weak evidence.

Conclusion Three: A test-optional policy is likely a barrier to
Dartmouth identifying less-advantaged students who would succeed at
Dartmouth.

Here’s where it gets interesting! I like this claim - as someone who’s
worked in higher education, it feels like it should be true. Again, it’s
like the relationship between happiness and income - what matters for
happiness is that you’re not starving and you’re better-off than your
neighbor. So how convincing is the evidence the authors present?

The third claim has three parts that work together to suggest a
test-optional policy as a barrier to identifying less-advantaged
students who would likely be successful:

Part one - “First, in the absence of having a common test score metric
across students, Admissions is left having to place more weight on other
factors that have been shown to be biased toward higher-income
students.” The authors cite a paper by Chetty, Deming, and Friedman
here, and I haven’t read the referenced paper, so let’s just assume that
it presents an air-tight argument that other factors are more biased
towards higher-income students. Great! This is some mild-evidence and we
should use it to update our prior just a bit.

Part two - “Second, lower-income students and international students are
more likely to be from high schools where Admissions has less
information to interpret the transcript.” The authors don’t provide any
hard evidence for this claim, but it makes sense! I can’t think of a
reason why I would disagree - admissions counselors have relationships
with individuals and institutions, and it’s hard to make a judgement
when you don’t know them.

Part three - “Third, under test-optional policies, some less-advantaged
students withhold test scores even in cases where providing the test
score would be a significant positive signal to Admissions.” Dartmouth
admissions is “within-context,” so relative performance is a strong
indicator of a student’s ability to succeed, but applicants don’t
understand this. So they choose to withhold scores, when submitting
scores would help them.

The evidence for this claim is multi-part. First, the authors suggest
that “all groups \[of students\] submit scores at roughly the same
rate.” “Estimates are derived by dividing the number of scores for the
test-optional cohorts by the number of scores for the test-required
cohorts, separately by bin. 46% of test-optional applicants opted not to
submit a score.”

This was not what I expected as an estimate, and didn’t like it
(initially) for two different reasons. First, I think of estimates as
parameters, and parameters (should) have some reporting of uncertainty
about them. Second, since it only includes students who reported scores
it felt to me that it left out important information on the 46% of
applicants who *did not* submit a score. There’s a strong assumption
lurking in the background here that submitters and non-submitters are
really the same kind of student, but I think it is important to be
skeptical on this assumption. But the authors have to provide some
metric to use, and after running a little simulation I came to think
that maybe this worked fine to make the point that all groups of
students submit scores at about the same rate.

This point feeds into the next sub-argument for part three - that
students who are less-advantaged (and score well-enough) would increase
their chances of admission if they submit their scores. Again, I agree
with this! And importantly, the authors provide some good evidence for
this in a visual - the visual shows the likelihood of admission for
applicants who submitted scores (Figure 5) and clearly, students who are
less-advantaged but submit scores are more likely to be admitted.

The point is reiterated in a figure that uses a subsample of students
who “initially submitted scores but then asked Admissions not to
consider them in the admissions decision.” In the figure, students with
a score of 1450-1490 from less-advantaged background, “increased their
admission probability by a factor of 3.7x (from .02 to .074) by
revealing their score.” This is good evidence! For students who are
less-advantaged, submitted a score, and had scores that were pretty
good, their likelihood of acceptance increased.

But in the same visual, and *not* noted, is the fact that for scores
below 1400, students who don’t submit scores have a higher rate of
acceptance by (roughly) a factor of 5 (from 0.01 to 0.05). Which leads
us back to the key assumption lurking in the background - what do
(hypothetical) scores look like for students who *do not* submit their
score? And can we estimate this?

The authors approach to estimating the number of students was to use the
sample of students who submitted a score and then requested not to use
it as a subsample to “scale up to the full set of applicants.” Using
this approach (I’ll save space on the details), they conclude that there
are around 1,000 less-advantaged students that should be submitting a
score with their application. They use some strong assumptions to get to
this point, and admit as much.

The problem, though, is that this assumption holds *meaningful* sway
over the estimate. For example, let’s assume that the non-submitters all
have SAT scores, and that the distribution of their scores mirrors that
of the enrolled population (which wouldn’t likely be true). Under this
assumption, the estimate is correct - we could expect around 1,000
students who are less-advantaged to have good enough scores that it
would be in their best interest to submit those scores. But what if they
don’t? What if, for example, the distribution of non-submitted SAT
scores was approximately normally distributed around 1360? In this case,
there’s only around 180 students. This is still important information,
but the story changes fairly drastically depending on the assumptions
that are made. In this simulation (below this paragraph), there would be
around 180 students who would increase their chances of admission; but
there would be around 3500 who would *decrease* their chances of
admission. By the evidence that the authors present (in Figure 6) - if
less advantaged students have high-enough scores, they should submit
scores to signal their ability to succeed; but if their scores are not
high-enough, it is to their disadvantage to submit a score. What would
have been great here (and maybe there’s a full report where this
information lives) is some good evidence that non-submitters *have
scores that are similar* to submitters and (especially) those admitted
to Dartmouth.

    library(ggplot2)

    n <- 4000  # Number of scores

    # Standard deviations
    sd_ebrw <- 40
    sd_math <- 40

    # Function to generate bounded normal distribution scores
    generate_score <- function(mean, sd, n, lower = 200, upper = 800) {
      scores <- rnorm(n, mean, sd)
      scores <- pmin(pmax(scores, lower), upper)  # Bound scores within limits
      return(scores)
    }

    # Function to perform a single simulation and return the count of 
    # scores in the 1450-1490 range
    simulate_and_count <- function(mean_ebrw, mean_math) {
      # Generate scores for each section
      ebrw_scores <- generate_score(mean_ebrw, sd_ebrw, n)
      math_scores <- generate_score(mean_math, sd_math, n)
      
      # Calculate total SAT scores
      total_scores <- ebrw_scores + math_scores
      total_scores <- pmin(pmax(total_scores, 1200), 1600)
      
      # Count the number of scores within the 1450-1490 range
      sum(total_scores >= 1450 & total_scores <= 1490)
    }

    # Adjusted function to run simulations with given mean values and plot density
    run_simulations_with_means_and_plot_density <- function(
        num_simulations, mean_ebrw_list, mean_math_list) {
      if (length(mean_ebrw_list) != length(mean_math_list)) {
        stop("mean_ebrw_list and mean_math_list must have the same length")
      }
      
      results <- data.frame(
        Counts_In_Range = numeric(0), 
        Mean_EBRW = numeric(0), 
        Mean_Math = numeric(0)
        )
      
      for (i in seq_along(mean_ebrw_list)) {
        mean_ebrw <- mean_ebrw_list[i]
        mean_math <- mean_math_list[i]
        
        counts_in_range <- numeric(num_simulations)
        
        for (j in 1:num_simulations) {
          counts_in_range[j] <- simulate_and_count(mean_ebrw, mean_math)
        }
        
        results <- rbind(results, data.frame(
          Counts_In_Range = counts_in_range, 
          Mean_EBRW = rep(mean_ebrw, num_simulations), 
          Mean_Math = rep(mean_math, num_simulations)))
      }
      
      # Calculate mean of Counts_In_Range for each Mean_EBRW and Mean_Math pair 
      means <- aggregate(
        Counts_In_Range ~ Mean_EBRW + Mean_Math, data = results, mean
        )
      means$Label <- paste("Mean:", round(means$Counts_In_Range, 2))
      
      # Plot with adjustments
      p <- ggplot(
        results, aes(x = Counts_In_Range, fill = paste(Mean_EBRW, Mean_Math))
        ) +
        geom_density(alpha = 0.6) +
        geom_text(data = means, aes(
          label = Label, 
          y = 0, 
          x = Counts_In_Range
          ), 
          size = 3, 
          hjust = "inward", 
          vjust = -0.5
          ) +
        scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink")) +
        theme_minimal() +
        labs(
          title = "Density of Counts within 1450-1490 Range for Different Assumptions",
          x = "Count of Scores in 1450-1490 Range",
          y = "Density",
          fill = "Mean Scores"
        )
      
      # Dynamically adjust x-limits to include all densities
      x_min <- min(means$Counts_In_Range) - 5
      x_max <- max(means$Counts_In_Range) + 5
      p <- p + xlim(x_min, x_max)
      
      print(p)
    }

    # Example usage with the provided mean values
    mean_ebrw_list <- c(733, 690, 670)
    mean_math_list <- c(750, 710, 690)
    run_simulations_with_means_and_plot_density(1000, mean_ebrw_list, mean_math_list)

    ## Warning: Removed 788 rows containing non-finite values (`stat_density()`).

![](Dartmouth-analysis_files/figure-markdown_strict/unnamed-chunk-4-1.png)

Conclusion Four: Test-optional policies do not necessarily increase the
proportion of less-advantaged students in the applicant pool.

This, much like Conclusion Three, seems pretty straightforward, and I
like that there’s some uncertainty narrated into the conclusion. The
claims here are meager: that test-optional policies aren’t guaranteed -
and in this case, don’t seem to have led - to increased numbers of
less-advantaged applicants.

So overall, this report is a mixed bag.

On one hand, there’s some good evidence that SAT scores should be in the
admissions mix, especially for students who do well-enough on the SAT
and are also disadvantaged in some way compared to other applicants. I
also think qualitative data is extremely useful in this context; for
example, the authors mention that they conducted interviews with the
Admissions staff at Dartmouth, and these informed the report
conclusions. It might also have been informative to gather qualitative
data on conversations with students who didn’t submit scores to find out
more on the reason they chose not to submit scores.

But in my opinion there’s also a touch of analytics slight-of-hand in
the report; it’s the kind of authoritative-sounding analysis found in
many economics-styled works that project confidence but actually present
weak evidence. In some ways, this moves the prior on the importance of
standardized test scores for admissions at Dartmouth; but in many other
ways, the evidence is weak, too heavily based on unproven assumptions,
or just irresponsible.

Which brings me to my concern with this report. There are leaders in
state colleges and universities across the US that want their
institutions to be like the Ivies. But the patterns for admissions at
Dartmouth don’t hold outside of Dartmouth (for example, see this
article, which suggests that more selective colleges more often receive
test scores:
<https://www.insidehighered.com/admissions/article/2021/09/13/study-reveals-which-applicants-didnt-submit-test-scores>).
Nonetheless - since I first saw this report a few weeks ago, I’ve seen
it referenced in two different places (a newsletter that I subscribe to
and an article from a local news outlet). University leaders (in my
experience) are almost always eager to raise the prestige-profile of
their institution, and if they use a similar analysis as a starting
point, they may come to similar weakly-informed conclusions about the
need for a test-required admissions policy. But we should avoid
designing policy based on mixed or weak evidence if we can help it.
