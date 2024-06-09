One of the most analyzed problems in higher education is how to keep students enrolled until they graduate. During my time in higher education, I worked for three different institutions, and each one cared deeply about it. There are genuine and strategic reasons for this. The genuine reason is that most of the people who lead and work at colleges and universities actually care about student success. The strategic reason is that graduating enrollees at a high rate helps raise the institution's prestige profile with rankings institutions, so there's a strong incentive for administrators to care about this number. 

Most institutions also have room for growth in graduating students. The official number is a bit higher, but roughly half of students who begin college will have a degree after six years. For two-year colleges, the number is less than half. So most institutions care deeply about monitoring how likely students are to graduate, and there are compelling reasons for this to stay important for a long time. 

Despite having a substantial research literature, most of the research on college student retention is a [causal salad](https://elevanth.org/blog/2021/06/15/regression-fire-and-dangerous-things-1-3/) - ram everything into a regression, cross your fingers that something meaningful comes out. But it doesn't have to be this way. This post is my attempt to demonstrate how to work through an analysis of college student success/retention in a way that's principled. Too much of the research in this area is lacking in logical structure, and my own work hasn't contributed positively to the methods used to understand this problem in a meaningful way. 

For student success/retention, there are two related but distinct goals:
1 - Understand the factors that cause changes in retention. This is about if and how specific factors impact a student's likelihood of sticking around and finishing college. For this question, causal tools are required. Most of the academic research is of this type.
2 - Being able to predict who will be retained. This is about being able to anticipate which students will and won't stick around until graduation. For this question, causal tools are not necessarily required, so there's a bit more flexibility, but there are other things to look out for. 

For this exercise, let's say that we are interested in learning how student involvement impacts a student's likelihood of staying at the institution. To work in a principled way, we first need to situate this question within the existing scientific literature. Next, we need to represent our expectations, based on the existing science, in a scientific model. 

There are a number of theories that explain why student's leave college without obtaining a degree. Each one is nuanced in it's own way, but most center on the idea of involvement and connection to the institution. If a student is well-integrated academically and socially, they are expected to be more likely to be academically successful and obtain a degree. The less connected they are, whether academically or socially, the more likely they are to drop out. 

Let's say that we have a measure of student success, first-semester GPA, and also a measure reflecting a student's first-semester involvement, and we'd like to estimate the relationship between the two. 

	First Semester GPA = f(Involvement, U)

U in this case stands for "unobserved," acknowledging that first-semester GPA is impacted not just by a student's academic and social involvement, but also other factors that we do not have measured. We could also write it like this: 

	First Semester GPA = β * Involvement + U

Translated: first-semester GPA is a proportion of Involvement plus the influence of other, unobserved causes. 

We will also want to define a DAG for our analysis that makes the causal structure explicit. We can use the ggdag package in R for this: 

```
library(tidyverse)
library(dagitty)
library(rethinking)
library(ggdag)  

# Scientific model in DAG notation
dag_1 <- dagify(
  y ~ x,
  exposure = "x",
  outcome = "y",
  labels = c(y = "First Semester GPA", x = "Involvement"),
  coords = list(
    x = c(x = 1, y = 3),
    y = c(x = 2, y = 2)
  )
)

ggdag_status(
  dag_1,
  use_labels = "label", text = FALSE
  ) +
  guides(fill = "none", color = "none") +  # Disable the legend
  theme_dag()
  
```

Before we fit anything to any actual data from our institution, we want to make sure that that we are able to *validate all of our assumptions with synthetic data*. So we need a function to simulate First Semester GPA. Here's a simple function in R that will give us some synthetic students with measures for first-semester GPA, FGPA, and Involvement: 

```
# R function to simulate first semester GPA
sim_first_semester_GPA <- function(Involvement, b, sd){
  U <- rnorm(length(Involvement), 0, sd)
  FGPA <- b * Involvement + U
  return(FGPA)
}

# Generate Involvement values between 1 and 10
Involvement <- runif(100, 1, 10)

```

Now that we have a function to simulate first-semester GPA and involvement, we need some code to create our fake students. Here's some code that does that, plotting a student's first-semester GPA compared to their involvement:

```
# Simulate first-semester GPA
FGPA <- sim_first_semester_GPA(Involvement, b = 0.5, sd = 5)

# Plot the results
data <- data.frame(Involvement, FGPA)
ggplot(data, aes(x = Involvement, y = FGPA)) +
  geom_point() +
  labs(
    x = "Student's Measured 'Involvement'",
    y = "First-semester GPA"
  ) +
  ylim(0, 4) +
  theme_minimal()
```

Now we need to start working on our statistical model, where we develop an estimator for how the average GPA changes with a student's level of involvement. Our first task is to specify our priors, which answer the question, "When there are no observations, what does the model believe?" The goal is to constrain the estimation to a range of what's scientifically realistic. For example, GPA will never be less than 0, or more than 4. We will need to tune our priors so that our values start within a range that fits our expectations about reality. 

Here's what we know, scientifically:
- When Involvement is 0, first-semester GPA doesn't have to be zero. A student can be under-involved, but also do well in courses.   
- First-semester GPA increases (on average) with involvement but it's difficult to say how much. 
- It's difficult to guess how much variation there is in first-semester GPA, but we know that it can only be positive.

Here's the way that we could represent what we know scientifically in a statistical model: 

	FirstSemesterGPA_i ~ Normal(μ_i, σ)
	μ_i = α + β * Involvement_i
	α ~ Normal(0, 1)
	β ~ Uniform(0, 1)
	σ ~ Uniform(0, 5)

Since our model is a line (μ_i = α + β * Involvement_i) and we are estimating a posterior distribution, we can sample from our priors here to understand the range of possibilities, like this: 

```
data <- data.frame(
  intercept = rnorm(50, 0, 1),
  slope = runif(50, 0, 1)
)

p <- ggplot(data, aes(x = 0, y = intercept)) +
  xlim(0, 10) +
  ylim(0, 5) +
  theme_minimal()

for (i in 1:nrow(data)) {
  p <- p + geom_abline(intercept = data$intercept[i], slope = data$slope[i])
}

print(p)
```

These are all over the place! Some of the lines have a pretty serious slope; others are almost flat. This is fine for now, but in a higher-stakes setting, we would want to finely tune our priors a bit more. Again, the goal is to narrow the range of values that our model thinks possible before it sees any actual data. 

Next, we need to try and *recover the known values of our simulation*. There are thousands of open-source packages that we could use in R and Python for building a statistical model (and this is probably the most common way to do research today). One of the only ways to validate that the statistical model is working as expected is to create datasets where the values are known and recover those values with the model. 

For this exercise, I'll use ```quap``` from the rethinking package in R to try and recover the values within the simulation; we want to verify that our beta value, \beta$ , is around 0.5 and sigma value, $\sigma$, is around 5 . Here's the code for that: 

```
m <- quap(
  alist(
    FGPA ~ dnorm(mu, sigma),
    mu <- a + b*Involvement,
    a ~ dnorm(0, 1),
    b ~ dunif(0, 1), 
    sigma ~ dunif(0, 5)
  ),
  data = list(FGAPA = FGPA, Involvement = Involvement)
)

> precis(m)
      mean   sd  5.5% 94.5%
a     0.17 0.77 -1.06  1.40
b     0.45 0.14  0.23  0.67
sigma 4.67 0.33  4.15  5.20

```

We're pretty close! In a more rigorous setting, we would want to vary the values in our synthetic data for beta and sigma and try to recover those values as well, but this will work for now. 

Now we need to take a look at some predictions with the model. The best approach here is the sample a few lines from our synthetic posterior (m) and plot them along with our synthetic people, but the most straightforward starting point is to pick a summary value and see what the predictions look like using those summary values. There's a convenience function in the ```quap```package that we can leverage for sampling from our synthetic posterior, ```extract.samples()```, but in this case, we can just use the mean values, like this: 

```
library(txtplot)

post <- precis(m)
mean_a <- post$mean[1]
mean_b <- post$mean[2]
predicted_FGPA <- mean_a + mean_b * Involvement
plot_data <- data.frame(Involvement, FGPA, predicted_FGPA)
plot_data <- plot_data[order(plot_data$Involvement),]
# txtplot(plot_data$Involvement, plot_data$FGPA)
txtplot(plot_data$Involvement, plot_data$predicted_FGPA)
 
> txtplot(plot_data$Involvement, plot_data$predicted_FGPA)
  +-----------+------------------+------------------+------------------+-----------------+---+
  |                                                                                      *   |
  |                                                                                 ***      |
  |                                                                            ****          |
4 +                                                                        ** *              +
  |                                                                    ****                  |
  |                                                                * **                      |
  |                                                            * **                          |
  |                                                       ****                               |
3 +                                                     *                                    +
  |                                               * **                                       |
  |                                           ** *                                           |
  |                                       ****                                               |
  |                                  ****                                                    |
2 +                              *** *                                                       +
  |                           * *                                                            |
  |                      ****                                                                |
  |                  ***                                                                     |
  |              *  *                                                                        |
1 +          ****                                                                            +
  |       ***                                                                                |
  |   **                                                                                     |
  +-----------+------------------+------------------+------------------+-----------------+---+
              2                  4                  6                  8                10    

``` 

This is a good start - but it's just a start! ***Before working with any actual data**, we need to repeat this process with different values and variations, fitting models and creating plots to make sure that we understand where our model fits our data well and where it doesn't.* On this toy example alone, there's a lot we can do to improve our model and understanding - and it only includes one predictor! This process is more challenging with more predictors, and it gets more important as the complexity increases. And again, this is all before we see any actual data. The goal is to fine tune our expectations and understand our model with known values before we apply it to any unknowns. 

The overall process followed here demonstrates the basics of [drawing the owl](https://x.com/rlmcelreath/status/1600123416601714689), in a way that's principled. If everyone were to use a similar workflow for studying college student success, we'd be a lot further along in the science - and that just might mean progress on graduating more students. 
