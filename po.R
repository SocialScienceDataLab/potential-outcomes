## 1. set a seed
set.seed(190910)

## 2. generate N
N <- 1000L

## 3. generate Y_0
Y_0 <- runif(N, min = 20, max = 80)

## 4. generate tau with differential treatment effects
tau <- 0.2 * rnorm(N, mean = Y_0, sd = 10)
tau <- ifelse(tau < 0, 0, ifelse(tau > 20, 20, tau)) # truncate

## 5. scatter plot
plot(Y_0,
     tau,
     pch = 19,
     col = "gray",
     xlab = expression(Y[0]),
     ylab = expression(tau),
     main = "Differential Treatment Effects",
     ylim = c(0, 20))

## 6. generate Y_1
Y_1 <- Y_0 + tau

## 7. generate D (randomization scenario)
D <- rbinom(N, size = 1L, prob = 0.5)

## 8. show that the potential outcomes are independent of treatment 
t.test(Y_0 ~ D)
t.test(Y_1 ~ D)

## 9. generate Y_obs
Y_obs <- ifelse(D == 1, Y_1, Y_0)

## 10. estimate ATE via regression
summary(lm(Y_obs ~ D))

## 11. generate D_sel (selection scenario)
prob_sel <- Y_0 * 0.01
D_sel <- rbinom(N, size = 1L, prob = prob_sel)

## 12. show that the potential outcomes are not independent of treatment 
t.test(Y_0 ~ D_sel)
t.test(Y_1 ~ D_sel)

## 13. generate Y_obs_sel
Y_obs_sel <- ifelse(D_sel == 1, Y_1, Y_0)

## 14. estimate ATE via regression
summary(lm(Y_obs_sel ~ D_sel))

## 15. specify the magnitude of selection bias and differential treatment effect bias

# Here, I generate the five quantities reported on slides 13 and 18:
# I generate each quantity twice:

# First, for the scenario under randomization
ey11 <- mean(Y_1[D == 1]) # E[Y_i(1) | D_i = 1]
ey01 <- mean(Y_0[D == 1]) # E[Y_i(0) | D_i = 1]
ey10 <- mean(Y_1[D == 0]) # E[Y_i(1) | D_i = 0]
ey00 <- mean(Y_0[D == 0]) # E[Y_i(0) | D_i = 0]
ate <- ey11 - ey00        # E[tau]
att <- mean(tau[D == 1])  # E[tau_i | D_i = 1]
atu <- mean(tau[D == 0])  # E[tau_i | D_i = 0]
pi <- mean(D)             # pi
sbias <- ey01 - ey00            # selection bias
dbias <- (1 - pi) * (att - atu) # differential treatment effect bias

# Resuls
mean(tau) # True ATE
ate       # Estimated ATE
sbias     # Estimated selection bias
dbias     # Estimated differntial treatment effect bias

# Second, for the scenatio under self-selection
ey11_sel <- mean(Y_1[D_sel == 1])               # E[Y_i(1) | D_i = 1]
ey01_sel <- mean(Y_0[D_sel == 1])               # E[Y_i(0) | D_i = 1]
ey10_sel <- mean(Y_1[D_sel == 0])               # E[Y_i(1) | D_i = 0]
ey00_sel <- mean(Y_0[D_sel == 0])               # E[Y_i(0) | D_i = 0]
ate_sel <- ey11_sel - ey00_sel                  # E[tau]
att_sel <- mean(tau[D_sel == 1])                # E[tau_i | D_i = 1]
atu_sel <- mean(tau[D_sel == 0])                # E[tau_i | D_i = 0]
pi_sel <- mean(D_sel)                           # pi
sbias_sel <- ey01_sel - ey00_sel                # selection bias
dbias_sel <- (1 - pi_sel) * (att_sel - atu_sel) # differential treatment effect bias

# Resuls
mean(tau) # True ATE
ate_sel   # Estimated ATE
sbias_sel # Estimated selection bias
dbias_sel # Estimated differntial treatment effect bias