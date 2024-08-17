When I was working in higher education, I've been on both the academic and "student life" wings of the university. The student life side of a university is a big chunk of the administrative apparatus, including support services (tutoring services, the health center, counseling center, disability resources, etc.) and the more villainized parts of the modern university (the recreation center, career center, campus life, etc.). Because most of these services are non-classroom services, it's important for them to be able to show how they contribute to student learning and success. 

I believe that most of the non-academic institutions within a university provide real value to students. There are exceptions, but overall, the impression that I got from working with these departments - even the heavily villainized recreation centers with climbing walls and lazy rivers - is that they help student success both at the institution and beyond. Anecdotally, I've seen it first-hand; students getting support they need from tutoring or disability resources, for example. But I also have what I believe is good social-scientific evidence for my thinking. 

I enjoy doing research that makes causal claims, but I missed an opportunity that I often wish I should have pursued. This post is a simulation of that research, how I would do it if I could do it again, and also explains why I believe that support services at a university are actually beneficial for students pursuing a college degree. Here's the story: 

In 2018, I worked for a university impacted by a hurricane. When the university returned to campus, there were a few buildings that had taken on damage, including a few large residence halls. The damage to a few of them was so bad that we couldn't allow students to return to them; instead, we worked with apartment rentals around the community to relocate students to similar living environments around town. This community, though, like any college town, didn't have widespread apartment complex vacancies, so we ended up placing students randomly around town. Some students ended up living just off-campus. Other students ended up living in apartment complexes in different parts of town, some up to 15/20 minutes away depending on traffic. 

One of the more common ways to design a study for causal inference is to use a research technique called "instrumental variables." An instrumental variables analysis uses a variable that is related to an exposure variable, not related to an outcome, and related to the outcome only through the exposure variable as an "instrument" to estimate causal effects. One of the more famous studies that uses instrumental variables, for example, uses a student's quarter of birth as an instrument for years of schooling to estimate returns to education. Because of differences in school entry and dropout laws, people born in different quarters of the year could leave school at slightly different education levels, allowing the authors to measure the impact of that small difference in schooling on earnings in later periods of life. Assuming that you've found a reasonable instrument, it's possible to mimic a natural experiment and obtain what is (arguably) a true causal effect. 

One of the more common instrumental variables is distance to some service. Let's say, for example, that we want to know the impact of some service on a student's first year performance. It's usually impossible or unethical to randomize access to the service, but distance (arguably) provides a way of randomizing access; the further that a student is away from some service, the less likely they are to use it. So distance is related to the service. But there's no clear way to connect distance the outcome itself, first year performance. There are many things that impact a student's likelihood of success, but distance is simply a measure of the length or amount of space between two points. And there's no obvious reason why how far apart things are in physical space should have a direct causal impact on first-year GPA. Distance does, however, have a causal relationship to on-campus services like tutoring services or the health center. And those things should relate to student success. 

So with mother nature's insistence, we randomly choose a few hundred students and scattered them around town, making it more difficult for some of them to access the student life services on campus. And considering the placement around town was random, it seemed possible to use our situation to estimate the impact that those services had on student success. There was one problem though - increasing the distance between a student and campus could also decrease the likelihood of that student going to class, which would also decrease the chances of success. But if we could control for how often a student went to class, we could satisfy the requirements of an instrumental variable, isolating the effect of services on a student's performance using distance as an instrument. 

Here's an example. There are lots of things that relate to student success. Here's a possible DAG using variables that are widely available to higher education researchers.

```
library(tidyverse)
library(dagitty)
library(rethinking)

dag_1 <- dagitty("dag {
  AbilityOrWorkEthic [Unobserved]
  Income [Unobserved]
  Services -> Performance
  Pell -> Performance
  Income -> Pell -> Performance
  Income -> SATScore -> Performance 
  FirstGen -> Performance 
  HSGPA -> Performance
  AbilityOrWorkEthic -> HSGPA -> Performance 
  AbilityOrWorkEthic -> SATScore -> Performance 
  Female -> STEM -> Performance
  Female -> Performance
  SATScore -> STEM -> Performance
  UnderrepMinority -> STEM -> Performance
  Distance -> Services
  Distance -> GoToClass -> Performance
  }"
)
plot(dag_1)
```

I'm not plotting the full DAG here, but in words: distance impacts both going to class and the likelihood of using on campus services, and both impact performance. 

Having moved on from the university, I don't have access to the data anymore, but the structure of the analysis can be simulated with only a few lines of code. At the time, I used a more traditional econometric approach, but if I were to have the opportunity to do a similar analysis today, I would use a Bayesian multivariate normal model, like this: 

```
# Number of samples
n <- 500

# Simulate variables
U_sim <- rnorm(n)  # Unobserved variables; ability or work ethic, for example
D_sim <- sample(1:4, n, replace = TRUE)  # Distance
G_sim <- rnorm(n, U_sim + D_sim)         # GoToClass 
S_sim <- rnorm(n, U_sim + D_sim)         # Services 
P_sim <- rnorm(n, U_sim + S_sim + G_sim)

# Standardize the simulated data
dat_sim <- list(
  D = standardize(D_sim),
  S = standardize(S_sim),
  P = standardize(P_sim),
  G = standardize(G_sim) 
)

# Define the model
m1 <- ulam(
  alist(
    c(S, P) ~ multi_normal(c(muS, muP), Rho, Sigma),
    muS <- aS + bDS * D, 
    muP <- aP + bSP * S + bGP * G,  # Include GoToClass 
    c(aP, aS) ~ normal(0, 0.5),
    c(bSP, bDS, bGP) ~ normal(0, 0.5),  
    Rho ~ lkj_corr(2),
    Sigma ~ exponential(1)
  ),
  data = dat_sim, 
  chains = 4, 
  cores = 4
)

> precis(m1, depth = 3)
         mean   sd  5.5% 94.5% rhat ess_bulk
aS       0.00 0.03 -0.06  0.05 1.00  1379.57
aP       0.00 0.02 -0.02  0.02 1.00  1527.50
bGP      0.58 0.02  0.55  0.61 1.00  1260.21
bDS      0.64 0.03  0.58  0.69 1.00  1399.27
bSP      0.30 0.03  0.25  0.35 1.01   953.47
Rho[1,1] 1.00 0.00  1.00  1.00   NA       NA
Rho[2,1] 0.60 0.05  0.53  0.67 1.01   909.72
Rho[1,2] 0.60 0.05  0.53  0.67 1.01   909.72
Rho[2,2] 1.00 0.00  1.00  1.00   NA       NA
Sigma[1] 0.77 0.02  0.73  0.81 1.00  1687.58
Sigma[2] 0.34 0.02  0.32  0.37 1.00  1128.77

```

In this simulation, bSP, the mean coefficient value for the impact of services on performance, is 0.30 with an 89% interval of 0.25 to 0.35. For this simulation, that's what we want to see. We also want to see that the correlation (Rho\[1,2] between services, S, and performance, P, is positive; that they tend to move together. What would the estimate look like without using the instrument? 

```
m2 <- ulam(
  alist(
    P ~ dnorm(mu, sigma), 
    mu <- aP + bSP*S, 
    aP ~ dnorm(0, 0.5), 
    bSP ~ dnorm(0, 0.5), 
    sigma ~ dexp(1)
  ), 
  data = dat_sim, 
  chains = 4, 
  cores = 4
)
> precis(m2)
      mean   sd  5.5% 94.5% rhat ess_bulk
aP    0.00 0.02 -0.03  0.03    1  1918.86
bSP   0.89 0.02  0.86  0.92    1  1762.25
sigma 0.46 0.01  0.44  0.48    1  1912.29

```
Here bSP is clearly biased upwards, nearly tripled compared to the multivariate normal instrumental variables model. But what if the true effect of was actually zero? Does our instrument help us out? Here's three different looks with the data simulated so that services has no impact on performance: 

```
# Number of samples
n <- 500

# Simulate variables
U_sim <- rnorm(n)  # Unobserved variables; ability or work ethic, for example
D_sim <- sample(1:4, n, replace = TRUE)  # Distance
G_sim <- rnorm(n, U_sim + D_sim)         # GoToClass 
S_sim <- rnorm(n, U_sim + D_sim)         # Services 
P_sim <- rnorm(n, U_sim + 0*S_sim + G_sim)

# Standardize the simulated data
dat_sim <- list(
  D = standardize(D_sim),
  S = standardize(S_sim),
  P = standardize(P_sim),
  G = standardize(G_sim) 
)

m3 <- ulam(
  alist(
    P ~ dnorm(mu, sigma), 
    mu <- aP + bSP*S, 
    aP ~ dnorm(0, 0.5), 
    bSP ~ dnorm(0, 0.5), 
    sigma ~ dexp(1)
  ), 
  data = dat_sim, 
  chains = 4, 
  cores = 4
)

m4 <- ulam(
  alist(
    P ~ dnorm(mu, sigma), 
    mu <- aP + bSP*S + bGP*G, 
    aP ~ dnorm(0, 0.5), 
    bSP ~ dnorm(0, 0.5), 
    bGP ~ dnorm(0, 0.5), 
    sigma ~ dexp(1)
  ), 
  data = dat_sim, 
  chains = 4, 
  cores = 4
)

m5 <- ulam(
  alist(
    c(S, P) ~ multi_normal(c(muS, muP), Rho, Sigma),
    muS <- aS + bDS * D, 
    muP <- aP + bSP * S + bGP * G,  
    c(aP, aS) ~ normal(0, 0.5),
    c(bSP, bDS, bGP) ~ normal(0, 0.5),  
    Rho ~ lkj_corr(2),
    Sigma ~ exponential(1)
  ),
  data = dat_sim, 
  chains = 4, 
  cores = 4
)

> precis(m3)
      mean   sd  5.5% 94.5% rhat ess_bulk
aP    0.00 0.03 -0.06  0.05    1  2006.98
bSP   0.66 0.03  0.61  0.71    1  1994.72
sigma 0.75 0.02  0.72  0.79    1  1578.51
> precis(m4)
      mean   sd  5.5% 94.5% rhat ess_bulk
aP    0.00 0.02 -0.03  0.03    1  1476.16
bSP   0.08 0.03  0.04  0.13    1  1020.88
bGP   0.83 0.03  0.78  0.87    1  1001.88
sigma 0.46 0.01  0.44  0.49    1  1625.44
> precis(m5, depth = 3)
         mean   sd  5.5% 94.5% rhat ess_bulk
aS       0.00 0.04 -0.06  0.06 1.01  1245.10
aP       0.00 0.05 -0.08  0.07 1.20  1199.19
bGP      0.92 0.03  0.87  0.97 1.14    19.63
bDS      0.44 0.30 -0.09  0.67 1.53     7.29
bSP      0.37 1.10 -0.33  2.48 1.53     7.30
Rho[1,1] 1.00 0.00  1.00  1.00   NA       NA
Rho[2,1] 0.23 0.70 -0.99  0.70 1.53     7.29
Rho[1,2] 0.23 0.70 -0.99  0.70 1.53     7.29
Rho[2,2] 1.00 0.00  1.00  1.00   NA       NA
Sigma[1] 0.85 0.12  0.75  1.08 1.53     7.31
Sigma[2] 0.98 0.76  0.51  2.49 1.53     7.33
```

So we can see, if the true effect of bSP is 0, the only way to recover the true effect is through the instrument. Adding a control variable for going to class helps, but it isn't correct until the instrument is added. 

I moved on from higher education not too long afterwards, so I didn't have time to finish and publish our study. But our analysis was very similar to this simulation, showing that the impact of services on student performance was meaningful. These days, it's a common criticism of higher education that there's administrative bloat, and usually the student services part of the university receives the most pointed bit of the criticism. Is it merited? Possibly! There is point where the added costs of additional services is greater than the value that it adds helping students graduate. But before the costs and benefits can be weighed, you have to understand the impact of the services on success, and that requires sound research and estimation methods, like instrumental variables. 
