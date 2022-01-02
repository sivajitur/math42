
## Parameter Setup
n_sim <- 1:500
n_years <- 2021:2031

ND_simulations <- data.frame(0, length(n_sim), length(n_years))
ND_2020 <- 724645
ND_mu <- 1.0474
ND_sd <- 0.0648
p_ND_cat <- 1/14.5
eff_ND_cat <- 1.098


HR_simulations <- data.frame(0, length(n_sim), length(n_years))
HR_2020 <- 4339671
HR_mu <- 1.0565
HR_sd <- 0.0391
p_HR_cat <- 7/44
eff_HR_cat <-  1.308

PR_simulations <- data.frame(0, length(n_sim), length(n_years))
PR_2020 <- 845901
PR_mu <- 1.1629
PR_sd <- 0.5583
p_PR_cat <- 7/44
eff_PR_cat <- 1.308

NI_simulations <- data.frame(data = 0, nrow = length(n_sim), ncol = length(n_years))
NI_2020 <- 345470
NI_mu <- 1.0360
NI_sd <- 0.1300


OF_simulations <- data.frame(0, length(n_sim), length(n_years))
OF_2020 <- 345470
OF_mu <- 1.0355
OF_sd <- 0.0670
```


## Actual Cool Code - run a stochastic simulation

set.seed(250982309)
for (i in n_sim) {
  if (runif(1) < p_ND_cat) {ND_simulations[i, 1] <- eff_ND_cat * ND_2020}
  else {ND_simulations[i, 1] <- rnorm(1, mean = ND_mu, sd = ND_sd) * ND_2020}
  
  if (runif(1) < p_HR_cat) {HR_simulations[i, 1] <- eff_HR_cat * HR_2020}
  else {HR_simulations[i, 1] <- rnorm(1, mean = HR_mu, sd = HR_sd) * HR_2020}
  
  if (runif(1) < p_PR_cat) {PR_simulations[i, 1] <- eff_PR_cat * PR_2020}
  else {PR_simulations[i, 1] <- rnorm(1, mean = PR_mu, sd = PR_sd) * PR_2020}
  
  NI_simulations[i, 1] <- rnorm(1, mean = NI_mu, sd = NI_sd) * NI_2020
  OF_simulations[i, 1] <- rnorm(1, mean = OF_mu, sd = OF_sd) * OF_2020
  
  
  for (j in 2:11) {
    if (runif(1) < p_ND_cat) {ND_simulations[i, j] <- eff_ND_cat * ND_simulations[i,j-1]}
    else {ND_simulations[i, j] <- rnorm(1, mean = ND_mu, sd = ND_sd) * ND_simulations[i,j-1]}
    
    if (runif(1) < p_HR_cat)  {HR_simulations[i, j] <- eff_HR_cat * HR_simulations[i,j-1]}
    else {HR_simulations[i, j] <- rnorm(1, mean = HR_mu, sd = HR_sd) * HR_simulations[i,j-1]}
    
    if (runif(1) < p_PR_cat) {PR_simulations[i, j] <- eff_PR_cat * PR_simulations[i,j-1]}
    else {PR_simulations[i, j] <- rnorm(1, mean = PR_mu, sd = PR_sd) * PR_simulations[i,j-1]}
    
    NI_simulations[i, j] <- rnorm(1, mean = NI_mu, sd = NI_sd) * NI_simulations[i,j-1]
    OF_simulations[i, j] <- rnorm(1, mean = OF_mu, sd = OF_sd) * OF_simulations[i,j-1]
  }
  
}


## Creates a new Dataframe
ND_vals <- unname(sapply(ND_simulations, mean, 2))
names(ND_vals) <- 2021:2031

HR_vals <- unname(sapply(HR_simulations, mean, 2))
names(HR_vals) <- 2021:2031

PR_vals <- unname(sapply(PR_simulations, mean, 2))
names(PR_vals) <- 2021:2031

NI_vals <- unname(sapply(NI_simulations, mean, 2))
names(NI_vals) <- 2021:2031

OF_vals <- unname(sapply(OF_simulations, mean, 2))
names(OF_vals) <- 2021:2031

total_amount = ND_vals + HR_vals + PR_vals + NI_vals + OF_vals
names(total_amount) <- 2021:2031

expenditure.data = data.frame(ND_vals, HR_vals, PR_vals, NI_vals, OF_vals, total_amount)
print(expenditure.data)

write.csv(expenditure.data, "expenditures_stochastic.csv", row.names = TRUE)
