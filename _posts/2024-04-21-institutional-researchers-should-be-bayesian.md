Over the past few years, I've moved further towards adopting Bayesian analysis methods, and I'm convinced that more people should. In my career, I've worked in tech (currently) and in higher education, and although advanced statistical analysis is rare in tech (surprisingly!), it's common in higher education. 

There are two broad categories of professionals who work at colleges and universities - staff and faculty. Faculty teach and hand out grades. Staff are the other personnel who run the college, from lawncare and maintenance to accountants and presidents. Most faculty are highly trained, the majority having terminal degrees in their field. On the staff side, though, the training varies widely. Many staff are trained on the job, but there are also pockets of professionals who are highly credentialed, many who have similar pedigrees to the faculty members that they work alongside. 

One group of the more highly trained/specialized staff are those who work in Institutional Research (IR). Institutional researchers at colleges and universities are responsible for a few different things, but broadly IR work can be thought of as either reporting - sending enrollment numbers to the Federal Government, for example - or data management and analysis. The data management and analysis role of IR is a blend of analytics and research. One one hand, it's a lot of work to organize a school's information and keep it tidy; and on the other, IR is expected to do research comparable to many of school's faculty members. In contrast to faculty research, though, most IR studies aren't peer reviewed beyond the department or perhaps another expert at the institution. So it's similar in some ways - the methods used and the formal academic tone, for example - but different in that it's not primarily for others in the discipline. It's used to make decisions about how the institution should be run. 

So IR work is a rare profession where advanced statistics is actually used to regularly make decisions about the way an organization should be run. Unfortunately, though, most IR professionals are not trained in the type of statistics that's best suited for decision analysis. 

As a graduate student, I was formally trained in the Frequentist style of statistical analysis. Most people are. Frequentist statistics is all the things about p-values, confidence intervals, and hypothesis testing. Most people also find frequentist statistics counterintuitive. What does it mean to think about something as a long term frequency of hypothetical repetitions of the same process? Consider a p-value; a p-value means that if we were to repeat an experiment many times (both sampling and the analysis), assuming the null hypothesis is true, the proportion of times we would observe data as extreme or more extreme than the current data is equal to the p-value. Even interpreting this correctly is really hard - how do you use this to make decisions? When you're making decisions, it's nice to have a sense of the uncertainty. Working with frequentist methods makes this challenging. 

In contrast, the Bayesian approach to statistics uses Bayes' Theorem alongside probability theory as a way to represent plausibility. Most people are familiar with the process of having an expectation about some event or outcome and having their expectations change as more information about that event becomes available. In a nutshell, this is the Bayesian way of doing analyses. By assigning probabilities to outcomes, Bayesian analysis provides a more intuitive way to quantify and communicate the strength of evidence for this type of process. For decision analysis, this is a distinct advantage.

Here's a concrete example applicable to IR. Numerous studies have suggested that living on-campus corresponds to increased student success for first-year students. One of the most common outcomes that researchers use as a proxy for student success is retention to the second year. Let's say that we've used a Bayesian logistic regression analysis to conclude that living on-campus is, on average, associated with an increase of 0.56 in the log-odds of retention compared to not being on-campus. Here's some code that will give you that estimate: 

```
library(rethinking)

set.seed(1)

# Sample size
n <- 1000

# Generate data
Gender <- sample(c(0, 1), n, replace = TRUE, prob = c(0.5, 0.5))
HSGPA <- rnorm(n, mean = 3.0, sd = 0.5)
OnCampus <- c(rep(1, 500), rep(0, 500)) # 500 on-campus and 500 off-campus

# Generate Retained based on HSGPA and OnCampus
RetainedProb <- plogis(-2 + 1.25 * HSGPA + 0.75 * OnCampus)
Retained <- rbinom(n, 1, RetainedProb)

# Create the synthetic dataset
data <- data.frame(Gender, HSGPA, OnCampus, Retained)

# Print the first few rows of the dataset to verify
head(data)
precis(data)

# Fit the Bayesian logistic regression model
model <- ulam(
  alist(
    Retained ~ dbinom(1, p),
    logit(p) <- a + b_Gender * Gender + b_HSGPA * HSGPA + b_OnCampus * OnCampus,
    a ~ dnorm(0, 10),
    b_Gender ~ dnorm(0, 10),
    b_HSGPA ~ dnorm(0, 10),
    b_OnCampus ~ dnorm(0, 10)
  ),
  data = data,
  chains = 4,
  cores = 4
)

# Check the summary of the model
precis(model)
```

Unfortunately for us, though, administrators don't care about beta coefficients for the log-odds of retention. They care about other things - increases in income, or prestige, for example. How can we translate this estimate into something meaningful to be used by decision makers? 

Let's assume that our college's leadership is considering a change in policy to require first-year students to live on-campus, except for those living in close geographical proximity to the college. This is expected to increase the number of students living on-campus from 500 to 700 students out of the 1000 first year students. There are costs to this - maintenance, utilities, and staffing, for example. But there's also revenue; keeping existing students is cheaper than acquiring new students, and more engaged students are more likely to contribute financially as alumni. Since living on-campus is expected to increase the likelihood of retention, we should expect an increase in future revenue. How can we translate our model into an estimate of revenue? 

It would be nice to have a range of values to present to our colleges leadership on the possible increase in revenue that we should expect. A simplified approach for this is to use our posterior distribution to estimate the difference in the number of students retained, and multiply this estimate by the income for each additional student. With a Bayesian estimate, this is straightforward. The procedure is: 

1 - Sample parameters (the coefficients) from the posterior distribution.
2 - Use the linear model (with mean values for the additional parameters) to predict whether those samples will be retained. 
3 - Calculate the number of students retained for both scenarios (500 v 700 students living on campus). 
4 - Calculate the revenue increase for the difference in the number of students retained. 

Here's some code that does that: 

```
# Additional incremental revenue 

# Extract the posterior samples
posterior_samples <- extract.samples(model)

# Calculate the mode of Gender and mean of HSGPA
mode_Gender <- as.numeric(names(which.max(table(data$Gender))))
mean_HSGPA <- mean(data$HSGPA)

# Calculate the linear predictor for students living on campus
linear_pred_on_campus <- posterior_samples$a +
  posterior_samples$b_Gender * mode_Gender +
  posterior_samples$b_HSGPA * mean_HSGPA +
  posterior_samples$b_OnCampus * 1

# Calculate the predicted retention probability for students living on campus
prob_on_campus <- plogis(linear_pred_on_campus)

# Calculate the expected number of students retained for both scenarios
retained_500_on_campus <- 500 * prob_on_campus
retained_700_on_campus <- 700 * prob_on_campus

# Calculate the difference in the number of students retained
diff_retained <- retained_700_on_campus - retained_500_on_campus

# Calculate the revenue increase
# Assume, just for simplicity, that each retained student adds $1000 in revenue
revenue_increase <- diff_retained * 1000

# Visualize the posterior distribution of the revenue increase
plot(density(revenue_increase), main = "Posterior Distribution of Incremental Revenue Increase",
     xlab = "Revenue Increase (USD)", ylab = "Density")
abline(v = quantile(revenue_increase, c(0.025, 0.975)), col = "red", lty = 2)
legend("topright", "95% Credible Interval", col = "red", lty = 2)
```

So we end up with a distribution of the possible increases in revenue from more students living on campus. Assuming that our model is correct, there is a 95% probability that the revenue increase from having 700 students living on campus lies between $178,479 and $188,727. If our administrators accurately understand the costs, they can then make a more informed decision about whether this is a good policy decision for the school based on the range of values.

The flexibility of the Bayesian approach is what makes this possible. How would we do something similar using Frequentist methods? While the code and results are similar (see below) the interpretation is drastically different: the interval from $178,445 to $188,890 means that if we were to repeat this procedure many times, 95% of the bootstrapped intervals would contain the true increase in revenue. But importantly - 95% confidence here *is about the procedure, not the probability of a specific value lying within an interval.* In practice, analyses are hardly ever repeated. And with only one interval, you can never know for sure that your interval contains the true value or not. 

```
library(dplyr)

set.seed(1)

# Sample size
n <- 1000

# Generate data
Gender <- sample(c(0, 1), n, replace = TRUE, prob = c(0.5, 0.5))
HSGPA <- rnorm(n, mean = 3.0, sd = 0.5)
OnCampus <- c(rep(1, 500), rep(0, 500)) # 500 on-campus and 500 off-campus

# Generate Retained based on HSGPA and OnCampus
RetainedProb <- plogis(-2 + 1.25 * HSGPA + 0.75 * OnCampus)
Retained <- rbinom(n, 1, RetainedProb)

# Create the synthetic dataset
data <- data.frame(Gender, HSGPA, OnCampus, Retained)

# Print the first few rows of the dataset to verify
head(data)

# Fit the Frequentist logistic regression model
model <- glm(Retained ~ Gender + HSGPA + OnCampus, data = data, family = binomial(link = "logit"))

# Check the summary of the model
summary(model)

# Additional incremental revenue
# Extract the model coefficients
coef_Gender <- coef(model)["Gender"]
coef_HSGPA <- coef(model)["HSGPA"]
coef_OnCampus <- coef(model)["OnCampus"]
intercept <- coef(model)["(Intercept)"]

# Calculate the mode of Gender and mean of HSGPA
mode_Gender <- as.numeric(names(which.max(table(data$Gender))))
mean_HSGPA <- mean(data$HSGPA)

# Calculate the linear predictor for students living on campus
linear_pred_on_campus <- intercept +
  coef_Gender * mode_Gender +
  coef_HSGPA * mean_HSGPA +
  coef_OnCampus * 1

# Calculate the predicted retention probability for students living on campus
prob_on_campus <- plogis(linear_pred_on_campus)

# Calculate the expected number of students retained for both scenarios
retained_500_on_campus <- 500 * prob_on_campus
retained_700_on_campus <- 700 * prob_on_campus

# Calculate the difference in the number of students retained
diff_retained <- retained_700_on_campus - retained_500_on_campus

# Calculate the revenue increase
# Assume, just for simplicity, that each retained student adds $1000 in revenue
revenue_increase <- diff_retained * 1000

# Print the revenue increase
cat("Estimated revenue increase: $", round(revenue_increase, 2), "\n", sep = "")

# Bootstrap confidence interval
B <- 1000 # Number of bootstrap samples

bootstrap_revenue_increase <- replicate(B, {
  # Resample the data with replacement
  bootstrap_data <- data[sample(nrow(data), replace = TRUE), ]
  
  # Fit the logistic regression model on the bootstrap sample
  bootstrap_model <- glm(Retained ~ Gender + HSGPA + OnCampus, data = bootstrap_data, family = binomial(link = "logit"))
  
  # Extract the model coefficients
  coef_Gender <- coef(bootstrap_model)["Gender"]
  coef_HSGPA <- coef(bootstrap_model)["HSGPA"]
  coef_OnCampus <- coef(bootstrap_model)["OnCampus"]
  intercept <- coef(bootstrap_model)["(Intercept)"]
  
  # Calculate the predicted retention probability for students living on campus
  linear_pred_on_campus <- intercept +
    coef_Gender * mode_Gender +
    coef_HSGPA * mean_HSGPA +
    coef_OnCampus * 1
  prob_on_campus <- plogis(linear_pred_on_campus)
  
  # Calculate the difference in the number of students retained and revenue increase
  retained_500_on_campus <- 500 * prob_on_campus
  retained_700_on_campus <- 700 * prob_on_campus
  diff_retained <- retained_700_on_campus - retained_500_on_campus
  revenue_increase <- diff_retained * 1000
  
  revenue_increase
})

# Calculate the 95% confidence interval
conf_interval <- quantile(bootstrap_revenue_increase, c(0.025, 0.975))

# Print the confidence interval
cat("95% Confidence Interval: [$", round(conf_interval[1], 2), ", $", round(conf_interval[2], 2), "]\n", sep = "")
```

Most notably, people tend to *think* of Frequentist intervals in the Bayesian way - that there is a 95% probability that the true value falls within the interval. For decision analysis this a big risk! The correct way to interpret a Frequentist confidence interval is to not interpret it at all - it's simply the result of a procedure that will contain the true value in an infinite number of hypothetical repetitions. If you need something to use so that you can make decisions using with a range of values, only the Bayesian approach properly accounts for uncertainty in the data and the precision of the estimate. 

There are numerous other decision analyses in IR/higher education where a range of possible values is beneficial - enrollment forecasting, estimating the effects of an educational intervention, faculty retention over time, or optimizing financial aid strategies, for example. In any of these, the Bayesian approach is the approach that best informs decision makers in a logical and straightforward way. And since IR work is so important for making decisions, more IR work should be Bayesian. 









