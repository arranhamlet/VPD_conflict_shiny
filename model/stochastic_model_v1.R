# Initial compartment values ----------------------------------------------

#Compartments
initial(S[, , ]) <- S0[i, j, k]
initial(E[, , ]) <- 0
initial(I[, , ]) <- I0[i, j, k]
initial(R[, , ]) <- Rpop0[i, j, k]
initial(Is[, , ]) <- 0
initial(Rc[, , ]) <- 0

#Additional outputs
initial(total_pop) <- sum(S0) + sum(I0) + sum(Rpop0)

# Compartments ------------------------------------------------------------

# Update compartments
update(S[, , ]) <- max(S[i, j, k] + waning_R[i, j, k] + waning_Rc[i, j, k] + aging_into_S[i, j, k] - aging_out_of_S[i, j, k] - lambda_S[i, j, k] - S_death[i, j, k] + moving_risk_to_S[i, j, k] - moving_risk_from_S[i, j, k] + migration_S[i, j, k] * pos_neg_migration + vaccinating_into_S[i, j, k] - vaccinating_out_of_S[i, j, k] + waning_to_S_long[i, j, k] + waning_to_S_unvaccinated[i, j, k] - waning_from_S_short[i, j, k] - waning_from_S_long[i, j, k] - t_seeded[i, j, k], 0)

#
update(E[, , ]) <- max(E[i, j, k] + lambda_S[i, j, k] - incubated[i, j, k] + aging_into_E[i, j, k] - aging_out_of_E[i, j, k] - E_death[i, j, k] + moving_risk_to_E[i, j, k] - moving_risk_from_E[i, j, k] + migration_E[i, j, k] * pos_neg_migration + vaccinating_into_E[i, j, k] - vaccinating_out_of_E[i, j, k] + waning_to_E_long[i, j, k] + waning_to_E_unvaccinated[i, j, k] - waning_from_E_short[i, j, k] - waning_from_E_long[i, j, k], 0)
# 
update(I[, , ]) <- max(I[i, j, k] + into_I[i, j, k] + aging_into_I[i, j, k] - aging_out_of_I[i, j, k] - recovered_I_to_R[i, j, k] - I_death[i, j, k] + t_seeded[i, j, k] + moving_risk_to_I[i, j, k] - moving_risk_from_I[i, j, k] + migration_I[i, j, k] * pos_neg_migration + vaccinating_into_I[i, j, k] - vaccinating_out_of_I[i, j, k] + waning_to_I_long[i, j, k] + waning_to_I_unvaccinated[i, j, k] - waning_from_I_short[i, j, k] - waning_from_I_long[i, j, k], 0)
# 
update(R[, , ]) <- max(R[i, j, k] + recovered_I_to_R[i, j, k] + recovered_Is_to_R[i, j, k] - waning_R[i, j, k] + aging_into_R[i, j, k] - aging_out_of_R[i, j, k] - R_death[i, j, k] + moving_risk_to_R[i, j, k] - moving_risk_from_R[i, j, k] + migration_R[i, j, k] * pos_neg_migration + vaccinating_into_R[i, j, k] - vaccinating_out_of_R[i, j, k] + waning_to_R_long[i, j, k] + waning_to_R_unvaccinated[i, j, k] - waning_from_R_short[i, j, k] - waning_from_R_long[i, j, k], 0)
# 
update(Is[, , ]) <- max(Is[i, j, k] + into_Is[i, j, k] - recovered_from_Is[i, j, k] + aging_into_Is[i, j, k] - aging_out_of_Is[i, j, k] - Is_death[i, j, k] + moving_risk_to_Is[i, j, k] - moving_risk_from_Is[i, j, k] + migration_Is[i, j, k] * pos_neg_migration + vaccinating_into_Is[i, j, k] - vaccinating_out_of_Is[i, j, k] + waning_to_Is_long[i, j, k] + waning_to_Is_unvaccinated[i, j, k] - waning_from_Is_short[i, j, k] - waning_from_Is_long[i, j, k], 0)
# 
update(Rc[, , ]) <- max(Rc[i, j, k] + recovered_Is_to_Rc[i, j, k] - waning_Rc[i, j, k] + aging_into_Rc[i, j, k] - aging_out_of_Rc[i, j, k] - Rc_death[i, j, k] + moving_risk_to_Rc[i, j, k] - moving_risk_from_Rc[i, j, k] + migration_Rc[i, j, k] * pos_neg_migration + vaccinating_into_Rc[i, j, k] - vaccinating_out_of_Rc[i, j, k] + waning_to_Rc_long[i, j, k] + waning_to_Rc_unvaccinated[i, j, k] - waning_from_Rc_short[i, j, k] - waning_from_Rc_long[i, j, k], 0)

#Additional outputs
update(total_pop) <- N

# Entering and exiting compartments ---------------------------------------

#Death
S_death[, , ] <- if(S[i, j, k] <= 0) 0 else Binomial(S[i, j, k], max(min(background_death[i, k], 1), 0))
E_death[, , ] <- if(E[i, j, k] <= 0) 0 else Binomial(E[i, j, k], max(min(background_death[i, k], 1), 0))
I_death[, , ] <- if(I[i, j, k] <= 0) 0 else Binomial(I[i, j, k], max(min(background_death[i, k], 1), 0) + max(cfr_normal[i], 0))
R_death[, , ] <- if(R[i, j, k] <= 0) 0 else Binomial(R[i, j, k], max(min(background_death[i, k], 1), 0))
Is_death[, , ] <- if(Is[i, j, k] <= 0) 0 else Binomial(Is[i, j, k], max(min(background_death[i, k], 1), 0) + max(cfr_severe[i], 0))
Rc_death[, , ] <- if(Rc[i, j, k] <= 0) 0 else Binomial(Rc[i, j, k], max(min(background_death[i, k], 1), 0))

#S sampling
lambda_S[, , ] <- if(S[i, j, k] <= 0) 0 else Binomial(S_after_waning[i, j, k], max(min(lambda[i, j, k], 1), 0))
waning_R[, , ] <- if(R[i, j, k] <= 0) 0 else Binomial(R[i, j, k], max(min(natural_immunity_waning, 1), 0))
waning_Rc[, , ] <- if(Rc[i, j, k] <= 0) 0 else Binomial(Rc[i, j, k], max(min(natural_immunity_waning, 1), 0))

#E sampling
incubated[, , ] <- if(E[i, j, k] <= 0) 0 else Binomial(E[i, j, k], max(min(incubation_rate, 1), 0))

update(new_case[, , ]) <- incubated[i, j, k] + t_seeded[i, j, k]
initial(new_case[, , ]) <- I0[i, j, k]
dim(new_case) <- c(n_age, n_vacc, n_risk)

#I sampling
into_I[, , ] <- if(incubated[i, j, k] <= 0) 0 else Binomial(incubated[i, j, k], max(min(1 - prop_severe[i, j, k], 1), 0))
into_Is[, , ] <- max(incubated[i, j, k] - into_I[i, j, k], 0)
recovered_I_to_R[, , ] <- if(I[i, j, k] <= 0) 0 else Binomial(I[i, j, k], max(min(recovery_rate, 1), 0))

#Is sampling
recovered_from_Is[, , ] <- if(Is[i, j, k] <= 0) 0 else Binomial(Is[i, j, k], max(min(severe_recovery_rate, 1), 0))
recovered_Is_to_R[, , ] <- if(recovered_from_Is[i, j, k] <= 0) 0 else Binomial(recovered_from_Is[i, j, k], max(min(1 - prop_complications[i], 1), 0))
recovered_Is_to_Rc[, , ] <- max(recovered_from_Is[i, j, k] - recovered_Is_to_R[i, j, k], 0)

# dim(total_S_in) <- c(n_age, n_vacc, n_risk)
# dim(total_S_out) <- c(n_age, n_vacc, n_risk)
# dim(S_left) <- c(n_age, n_vacc, n_risk)
# 
# dim(total_E_in) <- c(n_age, n_vacc, n_risk)
# dim(total_E_out) <- c(n_age, n_vacc, n_risk)
# dim(E_left) <- c(n_age, n_vacc, n_risk)
# 
# dim(total_I_in) <- c(n_age, n_vacc, n_risk)
# dim(total_I_out) <- c(n_age, n_vacc, n_risk)
# dim(I_left) <- c(n_age, n_vacc, n_risk)
# 
# dim(total_Is_in) <- c(n_age, n_vacc, n_risk)
# dim(total_Is_out) <- c(n_age, n_vacc, n_risk)
# dim(Is_left) <- c(n_age, n_vacc, n_risk)
# 
# dim(total_R_in) <- c(n_age, n_vacc, n_risk)
# dim(total_R_out) <- c(n_age, n_vacc, n_risk)
# dim(R_left) <- c(n_age, n_vacc, n_risk)
# 
# dim(total_Rc_in) <- c(n_age, n_vacc, n_risk)
# dim(total_Rc_out) <- c(n_age, n_vacc, n_risk)
# dim(Rc_left) <- c(n_age, n_vacc, n_risk)

# STEP 1: AGING - Apply aging
# For S compartment
aging_into_S[1, 1, ] <- Births[k]
aging_into_S[2:n_age, , ] <- if(S[i - 1, j, k] <= 0) 0 else Binomial(S[i - 1, j, k], max(min(aging_rate[i-1], 1), 0))
aging_out_of_S[1:(n_age - 1), , ] <- if(S[i, j, k] <= 0) 0 else Binomial(S[i, j, k], max(min(aging_rate[i], 1), 0))
S_after_aging[, , ] <- S[i, j, k] + aging_into_S[i, j, k] - aging_out_of_S[i, j, k]

# For E compartment
aging_into_E[2:n_age, , ] <- if(E[i - 1, j, k] <= 0) 0 else Binomial(E[i - 1, j, k], max(min(aging_rate[i-1], 1), 0))
aging_out_of_E[1:(n_age - 1), , ] <- if(E[i, j, k] <= 0) 0 else Binomial(E[i, j, k], max(min(aging_rate[i], 1), 0))
E_after_aging[, , ] <- E[i, j, k] + aging_into_E[i, j, k] - aging_out_of_E[i, j, k]

# For I compartment
aging_into_I[2:n_age, , ] <- if(I[i - 1, j, k] <= 0) 0 else Binomial(I[i - 1, j, k], max(min(aging_rate[i-1], 1), 0))
aging_out_of_I[1:(n_age - 1), , ] <- if(I[i, j, k] <= 0) 0 else Binomial(I[i, j, k], max(min(aging_rate[i], 1), 0))
I_after_aging[, , ] <- I[i, j, k] + aging_into_I[i, j, k] - aging_out_of_I[i, j, k]

# For R compartment
aging_into_R[2:n_age, , ] <- if(R[i - 1, j, k] <= 0) 0 else Binomial(R[i - 1, j, k], max(min(aging_rate[i-1], 1), 0))
aging_out_of_R[1:(n_age - 1), , ] <- if(R[i, j, k] <= 0) 0 else Binomial(R[i, j, k], max(min(aging_rate[i], 1), 0))
R_after_aging[, , ] <- R[i, j, k] + aging_into_R[i, j, k] - aging_out_of_R[i, j, k]

# For Is compartment
aging_into_Is[2:n_age, , ] <- if(Is[i - 1, j, k] <= 0) 0 else Binomial(Is[i - 1, j, k], max(min(aging_rate[i-1], 1), 0))
aging_out_of_Is[1:(n_age - 1), , ] <- if(Is[i, j, k] <= 0) 0 else Binomial(Is[i, j, k], max(min(aging_rate[i], 1), 0))
Is_after_aging[, , ] <- Is[i, j, k] + aging_into_Is[i, j, k] - aging_out_of_Is[i, j, k]

# For Rc compartment
aging_into_Rc[2:n_age, , ] <- if(Rc[i - 1, j, k] <= 0) 0 else Binomial(Rc[i - 1, j, k], max(min(aging_rate[i-1], 1), 0))
aging_out_of_Rc[1:(n_age - 1), , ] <- if(Rc[i, j, k] <= 0) 0 else Binomial(Rc[i, j, k], max(min(aging_rate[i], 1), 0))
Rc_after_aging[, , ] <- Rc[i, j, k] + aging_into_Rc[i, j, k] - aging_out_of_Rc[i, j, k]

# #Total in and out
# total_S_in[, , ] <- if(pos_neg_migration == 1) waning_R[i, j, k] + waning_Rc[i, j, k] + migration_S[i, j, k] * pos_neg_migration else waning_R[i, j, k] + waning_Rc[i, j, k]
# total_S_out[, , ] <- if(pos_neg_migration == 1) lambda_S[i, j, k] + S_death[i, j, k] + t_seeded[i, j, k] else lambda_S[i, j, k] + S_death[i, j, k] + migration_S[i, j, k] + t_seeded[i, j, k]
# S_left[, , ] <- S_after_aging[i, j, k] + total_S_in[i, j, k] - total_S_out[i, j, k]
# 
# total_E_in[, , ] <- if(pos_neg_migration == 1) lambda_S[i, j, k] + migration_E[i, j, k] else lambda_S[i, j, k]
# total_E_out[, , ] <- if(pos_neg_migration == 1) incubated[i, j, k] + E_death[i, j, k] else incubated[i, j, k] + E_death[i, j, k] + migration_E[i, j, k] 
# E_left[, , ] <- E_after_aging[i, j, k] + total_E_in[i, j, k] - total_E_out[i, j, k]
# 
# total_I_in[, , ] <- if(pos_neg_migration == 1) into_I[i, j, k] + t_seeded[i, j, k] + migration_I[i, j, k] else into_I[i, j, k] + t_seeded[i, j, k]
# total_I_out[, , ] <- if(pos_neg_migration == 1) recovered_I_to_R[i, j, k] + I_death[i, j, k] else recovered_I_to_R[i, j, k] + I_death[i, j, k] + migration_I[i, j, k] 
# I_left[, , ] <- I_after_aging[i, j, k] + total_I_in[i, j, k] - total_I_out[i, j, k]
# 
# total_Is_in[, , ] <- if(pos_neg_migration == 1) into_Is[i, j, k] + migration_Is[i, j, k] else into_Is[i, j, k]
# total_Is_out[, , ] <- if(pos_neg_migration == 1) recovered_from_Is[i, j, k] + Is_death[i, j, k] else recovered_from_Is[i, j, k] + Is_death[i, j, k] + migration_Is[i, j, k] 
# Is_left[, , ] <- Is_after_aging[i, j, k] + total_Is_in[i, j, k] - total_Is_out[i, j, k]
# 
# total_R_in[, , ] <- if(pos_neg_migration == 1) recovered_I_to_R[i, j, k] + recovered_Is_to_R[i, j, k] + migration_R[i, j, k] else recovered_I_to_R[i, j, k] + recovered_Is_to_R[i, j, k]
# total_R_out[, , ] <- if(pos_neg_migration == 1) R_death[i, j, k] + waning_R[i, j, k] else R_death[i, j, k] + waning_R[i, j, k] + migration_R[i, j, k] 
# R_left[, , ] <- R_after_aging[i, j, k] + total_R_in[i, j, k] - total_R_out[i, j, k]
# 
# total_Rc_in[, , ] <- if(pos_neg_migration == 1) recovered_Is_to_Rc[i, j, k] + migration_Rc[i, j, k] else recovered_Is_to_Rc[i, j, k]
# total_Rc_out[, , ] <- if(pos_neg_migration == 1) Rc_death[i, j, k] + waning_Rc[i, j, k] else Rc_death[i, j, k] + waning_Rc[i, j, k] + migration_Rc[i, j, k] 
# Rc_left[, , ] <- Rc_after_aging[i, j, k] + total_Rc_in[i, j, k] - total_Rc_out[i, j, k]

# STEP 2: VACCINATION - Apply vaccination to the results after aging
# For S compartment
vaccinating_out_of_S[, , ] <- if(n_vacc == 1 || j >= n_vacc - 1 || aging_into_S[i, j, k] <= 0 || vaccination_prop[i, j, k] <= 0) 0 else Binomial(aging_into_S[i, j, k], max(min(vaccination_prop[i, j, k], 1), 0))
vaccinating_into_S[, , ] <- if(j == 3) vaccinating_out_of_S[i, 1, k] else if(j > 3 && j %% 2 == 1) vaccinating_out_of_S[i, j - 2, k] + vaccinating_out_of_S[i, j - 3, k] else 0
S_after_vaccination[, , ] <- S_after_aging[i, j, k] + vaccinating_into_S[i, j, k] - vaccinating_out_of_S[i, j, k]

# For E compartment
vaccinating_out_of_E[, , ] <- if(1 == 1) 0 else if(n_vacc == 1 || j > n_vacc - 2 || aging_into_E[i, j, k] <= 0 || vaccination_prop[i, j, k] <= 0) 0 else Binomial(aging_into_E[i, j, k], max(min(vaccination_prop[i, j, k], 1), 0))
vaccinating_into_E[, , ] <- if(1 == 1) 0 else if(j == 3) vaccinating_out_of_E[i, 1, k] else if(j >= 5 && j %% 2 == 1) vaccinating_out_of_E[i, j - 2, k] + vaccinating_out_of_E[i, j - 3, k] else 0
E_after_vaccination[, , ] <- E_after_aging[i, j, k] + vaccinating_into_E[i, j, k] - vaccinating_out_of_E[i, j, k]

# For I compartment
vaccinating_out_of_I[, , ] <- if(1 == 1) 0 else if(n_vacc == 1 || j > n_vacc - 2 || aging_into_I[i, j, k] <= 0 || vaccination_prop[i, j, k] <= 0) 0 else Binomial(aging_into_I[i, j, k], max(min(vaccination_prop[i, j, k], 1), 0))
vaccinating_into_I[, , ] <- if(1 == 1) 0 else if(j == 3) vaccinating_out_of_I[i, 1, k] else if(j >= 5 && j %% 2 == 1) vaccinating_out_of_I[i, j - 2, k] + vaccinating_out_of_I[i, j - 3, k] else 0
I_after_vaccination[, , ] <- I_after_aging[i, j, k] + vaccinating_into_I[i, j, k] - vaccinating_out_of_I[i, j, k]

# For R compartment
vaccinating_out_of_R[, , ] <- if(1 == 1) 0 else if(n_vacc == 1 || j > n_vacc - 2 || aging_into_R[i, j, k] <= 0 || vaccination_prop[i, j, k] <= 0) 0 else Binomial(aging_into_R[i, j, k], max(min(vaccination_prop[i, j, k], 1), 0))
vaccinating_into_R[, , ] <- if(1 == 1) 0 else if(j == 3) vaccinating_out_of_R[i, 1, k] else if(j >= 5 && j %% 2 == 1) vaccinating_out_of_R[i, j - 2, k] + vaccinating_out_of_R[i, j - 3, k] else 0
R_after_vaccination[, , ] <- R_after_aging[i, j, k] + vaccinating_into_R[i, j, k] - vaccinating_out_of_R[i, j, k]

# For Is compartment
vaccinating_out_of_Is[, , ] <- if(1 == 1) 0 else if(n_vacc == 1 || j > n_vacc - 2 || aging_into_Is[i, j, k] <= 0 || vaccination_prop[i, j, k] <= 0) 0 else Binomial(aging_into_Is[i, j, k], max(min(vaccination_prop[i, j, k], 1), 0))
vaccinating_into_Is[, , ] <- if(1 == 1) 0 else if(j == 3) vaccinating_out_of_Is[i, 1, k] else if(j >= 5 && j %% 2 == 1) vaccinating_out_of_Is[i, j - 2, k] + vaccinating_out_of_Is[i, j - 3, k] else 0
Is_after_vaccination[, , ] <- Is_after_aging[i, j, k] + vaccinating_into_Is[i, j, k] - vaccinating_out_of_Is[i, j, k]

# For Rc compartment
vaccinating_out_of_Rc[, , ] <- if(1 == 1) 0 else if(n_vacc == 1 || j > n_vacc - 2 || aging_into_Rc[i, j, k] <= 0 || vaccination_prop[i, j, k] <= 0) 0 else Binomial(aging_into_Rc[i, j, k], max(min(vaccination_prop[i, j, k], 1), 0))
vaccinating_into_Rc[, , ] <- if(1 == 1) 0 else if(j == 3) vaccinating_out_of_Rc[i, 1, k] else if(j >= 5 && j %% 2 == 1) vaccinating_out_of_Rc[i, j - 2, k] + vaccinating_out_of_Rc[i, j - 3, k] else 0
Rc_after_vaccination[, , ] <- Rc_after_aging[i, j, k] + vaccinating_into_Rc[i, j, k] - vaccinating_out_of_Rc[i, j, k]

# STEP 3: WANING - Apply waning to the results after vaccination
# Waning for S
waning_from_S_short[, , ] <- if(j %% 2 == 1 && j > 1 && S_after_vaccination[i, j, k] > 0) Binomial(S_after_vaccination[i, j, k], max(min(short_term_waning[j], 1), 0)) else 0
waning_to_S_long[, 1:(n_vacc - 1), ] <- if(j %% 2 == 0 && j > 1) waning_from_S_short[i, j + 1, k] else 0
waning_from_S_long[, , ] <- if(j %% 2 == 0 && j > 1 && S_after_vaccination[i, j, k] > 0) Binomial(S_after_vaccination[i, j, k], max(min(long_term_waning[j], 1), 0)) else 0
waning_to_S_unvaccinated[, , ] <- if(j == 1) sum(waning_from_S_long[i, 2:n_vacc, k]) else 0
S_after_waning[, , ] <- S_after_vaccination[i, j, k] + waning_to_S_long[i, j, k] + waning_to_S_unvaccinated[i, j, k] - waning_from_S_short[i, j, k] - waning_from_S_long[i, j, k]

# Waning for E
waning_from_E_short[, , ] <- if(j %% 2 == 1 && j > 1 && E_after_vaccination[i, j, k] > 0) Binomial(E_after_vaccination[i, j, k], max(min(short_term_waning[j], 1), 0)) else 0
waning_to_E_long[, 1:(n_vacc - 1), ] <- if(j %% 2 == 0 && j > 1) waning_from_E_short[i, j + 1, k] else 0
waning_from_E_long[, , ] <- if(j %% 2 == 0 && j > 1 && E_after_vaccination[i, j, k] > 0) Binomial(E_after_vaccination[i, j, k], max(min(long_term_waning[j], 1), 0)) else 0
waning_to_E_unvaccinated[, , ] <- if(j == 1) sum(waning_from_E_long[i, 2:n_vacc, k]) else 0
E_after_waning[, , ] <- E_after_vaccination[i, j, k] + waning_to_E_long[i, j, k] + waning_to_E_unvaccinated[i, j, k] - waning_from_E_short[i, j, k] - waning_from_E_long[i, j, k]

# Waning for I
waning_from_I_short[, , ] <- if(j %% 2 == 1 && j > 1 && I_after_vaccination[i, j, k] > 0) Binomial(I_after_vaccination[i, j, k], max(min(short_term_waning[j], 1), 0)) else 0
waning_to_I_long[, 1:(n_vacc - 1), ] <- if(j %% 2 == 0 && j > 1) waning_from_I_short[i, j + 1, k] else 0
waning_from_I_long[, , ] <- if(j %% 2 == 0 && j > 1 && I_after_vaccination[i, j, k] > 0) Binomial(I_after_vaccination[i, j, k], max(min(long_term_waning[j], 1), 0)) else 0
waning_to_I_unvaccinated[, , ] <- if(j == 1) sum(waning_from_I_long[i, 2:n_vacc, k]) else 0
I_after_waning[, , ] <- I_after_vaccination[i, j, k] + waning_to_I_long[i, j, k] + waning_to_I_unvaccinated[i, j, k] - waning_from_I_short[i, j, k] - waning_from_I_long[i, j, k]

# Waning for R
waning_from_R_short[, , ] <- if(j %% 2 == 1 && j > 1 && R_after_vaccination[i, j, k] > 0) Binomial(R_after_vaccination[i, j, k], max(min(short_term_waning[j], 1), 0)) else 0
waning_to_R_long[, 1:(n_vacc - 1), ] <- if(j %% 2 == 0 && j > 1) waning_from_R_short[i, j + 1, k] else 0
waning_from_R_long[, , ] <- if(j %% 2 == 0 && j > 1 && R_after_vaccination[i, j, k] > 0) Binomial(R_after_vaccination[i, j, k], max(min(long_term_waning[j], 1), 0)) else 0
waning_to_R_unvaccinated[, , ] <- if(j == 1) sum(waning_from_R_long[i, 2:n_vacc, k]) else 0
R_after_waning[, , ] <- R_after_vaccination[i, j, k] + waning_to_R_long[i, j, k] + waning_to_R_unvaccinated[i, j, k] - waning_from_R_short[i, j, k] - waning_from_R_long[i, j, k]

# Waning for Is
waning_from_Is_short[, , ] <- if(j %% 2 == 1 && j > 1 && Is_after_vaccination[i, j, k] > 0) Binomial(Is_after_vaccination[i, j, k], max(min(short_term_waning[j], 1), 0)) else 0
waning_to_Is_long[, 1:(n_vacc - 1), ] <- if(j %% 2 == 0 && j > 1) waning_from_Is_short[i, j + 1, k] else 0
waning_from_Is_long[, , ] <- if(j %% 2 == 0 && j > 1 && Is_after_vaccination[i, j, k] > 0) Binomial(Is_after_vaccination[i, j, k], max(min(long_term_waning[j], 1), 0)) else 0
waning_to_Is_unvaccinated[, , ] <- if(j == 1) sum(waning_from_Is_long[i, 2:n_vacc, k]) else 0
Is_after_waning[, , ] <- Is_after_vaccination[i, j, k] + waning_to_Is_long[i, j, k] + waning_to_Is_unvaccinated[i, j, k] - waning_from_Is_short[i, j, k] - waning_from_Is_long[i, j, k]

# Waning for Rc
waning_from_Rc_short[, , ] <- if(j %% 2 == 1 && j > 1 && Rc_after_vaccination[i, j, k] > 0) Binomial(Rc_after_vaccination[i, j, k], max(min(short_term_waning[j], 1), 0)) else 0
waning_to_Rc_long[, 1:(n_vacc - 1), ] <- if(j %% 2 == 0 && j > 1) waning_from_Rc_short[i, j + 1, k] else 0
waning_from_Rc_long[, , ] <- if(j %% 2 == 0 && j > 1 && Rc_after_vaccination[i, j, k] > 0) Binomial(Rc_after_vaccination[i, j, k], max(min(long_term_waning[j], 1), 0)) else 0
waning_to_Rc_unvaccinated[, , ] <- if(j == 1) sum(waning_from_Rc_long[i, 2:n_vacc, k]) else 0
Rc_after_waning[, , ] <- Rc_after_vaccination[i, j, k] + waning_to_Rc_long[i, j, k] + waning_to_Rc_unvaccinated[i, j, k] - waning_from_Rc_short[i, j, k] - waning_from_Rc_long[i, j, k]

# STEP 4: RISK - Apply risk to the results after waning
# Moving FROM each compartment - now using the population after previous transitions
moving_risk_from_S[, , ] <- if(S_after_waning[i, j, k] <= 0) 0 else Binomial(S_after_waning[i, j, k], max(min(moving_risk_prop[i, j, k], 1), 0))
moving_risk_from_E[, , ] <- if(E_after_waning[i, j, k] <= 0) 0 else Binomial(E_after_waning[i, j, k], max(min(moving_risk_prop[i, j, k], 1), 0))
moving_risk_from_I[, , ] <- if(I_after_waning[i, j, k] <= 0) 0 else Binomial(I_after_waning[i, j, k], max(min(moving_risk_prop[i, j, k], 1), 0))
moving_risk_from_R[, , ] <- if(R_after_waning[i, j, k] <= 0) 0 else Binomial(R_after_waning[i, j, k], max(min(moving_risk_prop[i, j, k], 1), 0))
moving_risk_from_Is[, , ] <- if(Is_after_waning[i, j, k] <= 0) 0 else Binomial(Is_after_waning[i, j, k], max(min(moving_risk_prop[i, j, k], 1), 0))
moving_risk_from_Rc[, , ] <- if(Rc_after_waning[i, j, k] <= 0) 0 else Binomial(Rc_after_waning[i, j, k], max(min(moving_risk_prop[i, j, k], 1), 0))

# Moving INTO each compartment with specified distribution
moving_risk_to_S[, , ] <- if(sum(moving_risk_distribution[i, j, ]) <= 0) moving_risk_from_S[i, j, k] else sum(moving_risk_from_S[i, j, ]) * moving_risk_distribution[i, j, k]/sum(moving_risk_distribution[i, j, ])
moving_risk_to_E[, , ] <- if(sum(moving_risk_distribution[i, j,]) <= 0) moving_risk_from_E[i, j, k] else sum(moving_risk_from_E[i, j, ]) * moving_risk_distribution[i, j, k]/ sum(moving_risk_distribution[i, j, ])
moving_risk_to_I[, , ] <- if(sum(moving_risk_distribution[i, j,]) <= 0) moving_risk_from_I[i, j, k] else sum(moving_risk_from_I[i, j, ]) * moving_risk_distribution[i, j, k]/ sum(moving_risk_distribution[i, j, ])
moving_risk_to_R[, , ] <- if(sum(moving_risk_distribution[i, j,]) <= 0) moving_risk_from_R[i, j, k] else sum(moving_risk_from_R[i, j, ]) * moving_risk_distribution[i, j, k]/ sum(moving_risk_distribution[i, j, ])
moving_risk_to_Is[, , ] <- if(sum(moving_risk_distribution[i, j,]) <= 0) moving_risk_from_Is[i, j, k] else sum(moving_risk_from_Is[i, j, ]) * moving_risk_distribution[i, j, k]/ sum(moving_risk_distribution[i, j, ])
moving_risk_to_Rc[, , ] <- if(sum(moving_risk_distribution[i, j,]) <= 0) moving_risk_from_Rc[i, j, k] else sum(moving_risk_from_Rc[i, j, ]) * moving_risk_distribution[i, j, k]/ sum(moving_risk_distribution[i, j, ])

# Parameters to control movement between risk groups
migration <- interpolate(tt_migration, migration_in_number, "constant") 
migration_distribution <- interpolate(tt_migration, migration_distribution_values, "constant")

#Positive or negative flow
pos_neg_migration <- if(sum(migration) < 0) -1 else 1
migration_adjusted[, , ] <- migration[i, j, k] * pos_neg_migration

migration_occuring_S[, , ] <- if(migration_distribution[1] <= 0 || sum(S) <= 0) 0 else Binomial(sum(migration_adjusted), S[i, j, k]/sum(S) * sum(migration_distribution[1])/sum(migration_distribution))
migration_S[, , ] <- if(migration_distribution[1] <= 0) 0 else Binomial(migration_occuring_S[i, j, k], migration_distribution[1])/sum(migration_distribution)
dim(migration_occuring_S) <- c(n_age, n_vacc, n_risk)

migration_occuring_E[, , ] <- if(migration_distribution[2] <= 0 || sum(E) <= 0) 0 else Binomial(sum(migration_adjusted), E[i, j, k]/sum(E) * sum(migration_distribution[2])/sum(migration_distribution))
migration_E[, , ] <- if(migration_distribution[2] <= 0) 0 else Binomial(migration_occuring_E[i, j, k], migration_distribution[2])/sum(migration_distribution)
dim(migration_occuring_E) <- c(n_age, n_vacc, n_risk)

migration_occuring_I[, , ] <- if(migration_distribution[3] <= 0 || sum(I) <= 0) 0 else Binomial(sum(migration_adjusted), I[i, j, k]/sum(I) * sum(migration_distribution[3])/sum(migration_distribution))
migration_I[, , ] <- if(migration_distribution[3] <= 0) 0 else Binomial(migration_occuring_I[i, j, k], migration_distribution[3])/sum(migration_distribution)
dim(migration_occuring_I) <- c(n_age, n_vacc, n_risk)

migration_occuring_R[, , ] <- if(migration_distribution[4] <= 0 || sum(R) <= 0) 0 else Binomial(sum(migration_adjusted), R[i, j, k]/sum(R) * sum(migration_distribution[4])/sum(migration_distribution))
migration_R[, , ] <- if(migration_distribution[4] <= 0) 0 else Binomial(migration_occuring_R[i, j, k], migration_distribution[4])/sum(migration_distribution)
dim(migration_occuring_R) <- c(n_age, n_vacc, n_risk)

migration_occuring_Is[, , ] <- if(migration_distribution[5] <= 0 || sum(Is) <= 0) 0 else Binomial(sum(migration_adjusted), Is[i, j, k]/sum(Is) * sum(migration_distribution[5])/sum(migration_distribution))
migration_Is[, , ] <- if(migration_distribution[5] <= 0) 0 else Binomial(migration_occuring_Is[i, j, k], migration_distribution[5])/sum(migration_distribution)
dim(migration_occuring_Is) <- c(n_age, n_vacc, n_risk)

migration_occuring_Rc[, , ] <- if(migration_distribution[6] <= 0 || sum(Rc) <= 0) 0 else Binomial(sum(migration_adjusted), Rc[i, j, k]/sum(Rc) * sum(migration_distribution[6])/sum(migration_distribution))
migration_Rc[, , ] <- if(migration_distribution[6] <= 0) 0 else Binomial(migration_occuring_Rc[i, j, k], migration_distribution[6])/sum(migration_distribution)
dim(migration_occuring_Rc) <- c(n_age, n_vacc, n_risk)

# User parameter values --------------------------------------------------------

#Compartment dimensions
#Number of age compartments
n_age <- parameter(1)
#Number of vaccination compartments
n_vacc <- parameter(1)
#Number of risk population compartments
n_risk <- parameter(1)

#Initial total population
S0 <- parameter()
I0 <- parameter()
Rpop0 <- parameter()

#Disease specific parameters
#Incubation rate
incubation_rate <- parameter()
#Recovery rate
recovery_rate <- parameter()
#Disease specific mortality
cfr_normal <- parameter()
#Waning antibody rate
natural_immunity_waning <- parameter()
#Proportion of cases that are severe
prop_severe <- parameter()
#Severe case recovery rate
severe_recovery_rate <- parameter()
#Severe death rate
cfr_severe <- parameter()
#Proportion of cases that have complications
prop_complications <- parameter()

#Transmission parameters
R0 <- parameter()
#Changing R0
#Define the times when R0 changes
tt_R0 <- parameter()
#Set the number of times R0 changes
no_R0_changes <- parameter()
#Contact matrix
contact_matrix <- parameter()
#Seeding parameters
seeded <- parameter()
#Changing seeded
#Define the times when seeded changes
tt_seeded <- parameter()
#Set the number of times seeded changes
no_seeded_changes <- parameter()

#Death parameters
initial_background_death <- parameter()
#Crude death rate
crude_death <- parameter()
#Number of changes to death rate
no_death_changes <- parameter()
#Times of changes to death rate
tt_death_changes <- parameter()

#Birth rate parameters
#Add in switch to decouple births and deaths when wanted
simp_birth_death <- parameter(1)
#Add in changing birth and deaths
crude_birth <- parameter()
#Number of changes to birth rate
no_birth_changes <- parameter()
#Times of changes to birth rate
tt_birth_changes <- parameter()
#Reproductive lower limit
repro_low <- parameter()
#Reproductive upper limit
repro_high <- parameter()
#Maternal antibody protection
age_maternal_protection_ends <- parameter()
#Maternal antibody protection from vaccination
protection_weight_vacc <- parameter()
#Maternal antibody protection from natural infection
protection_weight_rec <- parameter()

#Aging parameters
#Aging rate
aging_rate <- parameter()

#Vaccination parameters
#Set up times for changing vaccination coverage
tt_vaccination_coverage <- parameter()
no_vacc_changes <- parameter()
#Set up vaccination coverage changing over time
vaccination_coverage <- parameter()
#Beta modifier for age and vaccination
age_vaccination_beta_modifier <- parameter()
# Time parameters for risk movement
tt_moving_risk <- parameter()
no_moving_risk_changes <- parameter()
moving_risk_values <- parameter()
moving_risk_distribution_values <- parameter()

#Migration parameters
no_migration_changes <- parameter()
tt_migration <- parameter()
migration_in_number <- parameter()
migration_distribution_values <- parameter()

#Modifiers for fitting
fertility_modifier = parameter()
death_modifier = parameter()

# Calculated parameters ---------------------------------------------------

#Calculate transmission parameters
infectious_period[, , ] <- if((severe_recovery_rate + cfr_severe[i] + background_death[i, k]) <= 0 || (recovery_rate + cfr_normal[i] + background_death[i, k]) <= 0) 0 else (1 - prop_severe[i, j, k]) / (recovery_rate + cfr_normal[i] + background_death[i, k]) + prop_severe[i, j, k] / (severe_recovery_rate + cfr_severe[i] + background_death[i, k])

#Interpolate R0
t_R0 <- interpolate(tt_R0, R0, "constant")
#Calculate beta from the R0 and infectious period
beta[, , ] <- if(infectious_period[i, j, k] <= 0) 0 else t_R0 / infectious_period[i, j, k]

#Update with vaccination and age mediation
beta_updated[, , ] <- if(i <= age_maternal_protection_ends) beta[i, j, k] * (1 - age_vaccination_beta_modifier[i, j, k]) * (1 - (protection_weight_vacc * prop_maternal_vaccinated[k] + protection_weight_rec * prop_maternal_natural[k])) else (1 - age_vaccination_beta_modifier[i, j, k]) * beta[i, j, k]

# Step 1: Infectious contribution by age
inf_weighted[, , ] <- beta_updated[i, j, k] * (I[i, j, k] + Is[i, j, k])
dim(inf_weighted) <- c(n_age, n_vacc, n_risk)

infectious_source[] <- sum(inf_weighted[i, , ])
dim(infectious_source) <- n_age

# Step 2: Age-specific contact-weighted force of infection
lambda_contact[, ] <- contact_matrix[i, j] * infectious_source[j]
dim(lambda_contact) <- c(n_age, n_age)

lambda_raw[] <- if(Npop_age[i] <= 0) 0 else sum(lambda_contact[i, ]) / Npop_age[i]
dim(lambda_raw) <- n_age

lambda[, , ] <- if(N <= 0) 0 else max(0, lambda_raw[i]) * (1 - age_vaccination_beta_modifier[i, j, k])

# Calculate next-generation matrix elements
ngm_unfolded[, , , ] <- S[i, k, l] * beta_updated[i, k, l] * infectious_period[i, k, l] * contact_matrix[i, j]
dim(ngm_unfolded) <- c(n_age, n_age, n_vacc, n_risk)

ngm[] <- if(Npop_age[i] <= 0) 0 else sum(ngm_unfolded[i, , , ])/Npop_age[i]
dim(ngm) <- c(n_age)

# Step 2: Collapse across i and j
update(Reff) <- if(n_age <= 0) 0 else sum(ngm)/n_age
initial(Reff) <- R0[1]

update(Reff_age[]) <- ngm[i]
initial(Reff_age[]) <- 0
dim(Reff_age) <- n_age

#Seeding
t_seeded <- interpolate(tt_seeded, seeded, "constant")

#Calculate populations
N <- sum(S) + sum(E) + sum(I) + sum(R) + sum(Is) + sum(Rc)
Npop_age_risk[, ] <- sum(S[i, , j]) + sum(E[i, , j]) + sum(I[i, , j]) + sum(R[i, , j]) + sum(Is[i, , j]) + sum(Rc[i, , j])
Npop_age[] <- sum(S[i, , ]) + sum(E[i, , ]) + sum(I[i, , ]) + sum(R[i, , ]) + sum(Is[i, , ]) + sum(Rc[i, , ])
dim(Npop_age) <- n_age

#Calculate death rates
Npop_background_death[, ] <- if(Npop_age_risk[i, j] <= 0) 0 else Binomial(Npop_age_risk[i, j], max(min(background_death[i, j], 1), 0))
#Interpolate changes in death rate
death_int <- interpolate(tt_death_changes, crude_death, "constant")
# life_expectancy <- parameter()

#Select background death rate to use
background_death[, ]<- if(simp_birth_death == 1) max(min(initial_background_death[i, j] * death_modifier, 1), 0) else max(min(death_int[i, j] * death_modifier, 1), 0)

#Calculate birth rates
#Reproductive population
reproductive_population[] <- sum(S[repro_low:repro_high, , i]) +
  sum(E[repro_low:repro_high, , i]) +
  sum(I[repro_low:repro_high, , i]) +
  sum(R[repro_low:repro_high, , i]) +
  sum(Is[repro_low:repro_high, , i]) +
  sum(Rc[repro_low:repro_high, , i])

#Calculate birth rate
birth_rate[] <- if(reproductive_population[i] <= 0) 0 else sum(Npop_background_death[, i])/reproductive_population[i]
#Interpolate changes in birth rate
birth_int <- interpolate(tt_birth_changes, crude_birth, "constant")
#Calculate the number of births
Births[] <-  if(reproductive_population[i] <= 0) 0 else if(simp_birth_death == 1) Binomial(reproductive_population[i], max(min(fertility_modifier * birth_rate[i]/2, 1), 0)) else Binomial(reproductive_population[i], max(min(fertility_modifier * birth_int[i]/2, 1), 0))

# Mothers who confer vaccine derived maternal antibodies
vaccinated_mums[] <- if(n_vacc <= 1) 0 else sum(S[repro_low:repro_high, 2:n_vacc, i]) + sum(E[repro_low:repro_high, 2:n_vacc, i]) + sum(I[repro_low:repro_high, 2:n_vacc, i]) + sum(R[repro_low:repro_high, 2:n_vacc, i]) + sum(Is[repro_low:repro_high, 2:n_vacc, i]) + sum(Rc[repro_low:repro_high, 2:n_vacc, i])

# Mothers who confer natural derived maternal antibodies
antibody_mums[] <- sum(I[repro_low:repro_high, , i]) + sum(R[repro_low:repro_high, , i]) + sum(Is[repro_low:repro_high, , i]) + sum(Rc[repro_low:repro_high, , i])

# Maternal
prop_maternal_vaccinated[] <- if(reproductive_population[i] <= 0) 0 else vaccinated_mums[i]/reproductive_population[i]
prop_maternal_natural[] <- if(reproductive_population[i] <= 0) 0 else antibody_mums[i]/reproductive_population[i]

#Interpolate vaccination coverage
vaccination_prop <- interpolate(tt_vaccination_coverage, vaccination_coverage, "constant")

# Parameters to control movement between risk groups
moving_risk_prop <- interpolate(tt_moving_risk, moving_risk_values, "constant")
moving_risk_distribution <- interpolate(tt_moving_risk, moving_risk_distribution_values, "constant")  # Proportion going to each risk group


# Dimensions --------------------------------------------------------------

dim(S) <- c(n_age, n_vacc, n_risk)
dim(E) <- c(n_age, n_vacc, n_risk)
dim(I) <- c(n_age, n_vacc, n_risk)
dim(R) <- c(n_age, n_vacc, n_risk)
dim(Is) <- c(n_age, n_vacc, n_risk)
dim(Rc) <- c(n_age, n_vacc, n_risk)
dim(S0) <- c(n_age, n_vacc, n_risk)
dim(I0) <- c(n_age, n_vacc, n_risk)
dim(Rpop0) <- c(n_age, n_vacc, n_risk)

# Vaccination waning rate
dim(prop_complications) <- c(n_age)
dim(beta_updated) <- c(n_age, n_vacc, n_risk)
dim(age_vaccination_beta_modifier) <- c(n_age, n_vacc, n_risk)
dim(prop_severe) <- c(n_age, n_vacc, n_risk)
dim(cfr_normal) <- c(n_age)
dim(cfr_severe) <- c(n_age)
dim(beta) <- c(n_age, n_vacc, n_risk)
dim(infectious_period) <- c(n_age, n_vacc, n_risk)
dim(lambda) <- c(n_age, n_vacc, n_risk)
dim(tt_R0) <- no_R0_changes
dim(R0) <- no_R0_changes
# dim(Reff_contrib) <- c(n_age, n_vacc, n_risk)
dim(contact_matrix) <- c(n_age, n_age)
dim(lambda_S) <- c(n_age, n_vacc, n_risk)
dim(waning_R) <- c(n_age, n_vacc, n_risk)
dim(waning_Rc) <- c(n_age, n_vacc, n_risk)
dim(incubated) <- c(n_age, n_vacc, n_risk)
dim(tt_seeded) <- no_seeded_changes
dim(seeded) <- c(n_age, n_vacc, n_risk, no_seeded_changes)
dim(t_seeded) <- c(n_age, n_vacc, n_risk)

dim(into_I) <- c(n_age, n_vacc, n_risk)
dim(into_Is) <- c(n_age, n_vacc, n_risk)
dim(recovered_I_to_R) <- c(n_age, n_vacc, n_risk)
dim(recovered_from_Is) <- c(n_age, n_vacc, n_risk)
dim(recovered_Is_to_R) <- c(n_age, n_vacc, n_risk)
dim(recovered_Is_to_Rc) <- c(n_age, n_vacc, n_risk)

dim(aging_rate) <- n_age
dim(aging_into_S) <- c(n_age, n_vacc, n_risk)
dim(aging_out_of_S) <- c(n_age, n_vacc, n_risk)
dim(aging_into_E) <- c(n_age, n_vacc, n_risk)
dim(aging_out_of_E) <- c(n_age, n_vacc, n_risk)
dim(aging_into_I) <- c(n_age, n_vacc, n_risk)
dim(aging_out_of_I) <- c(n_age, n_vacc, n_risk)
dim(aging_into_R) <- c(n_age, n_vacc, n_risk)
dim(aging_out_of_R) <- c(n_age, n_vacc, n_risk)
dim(aging_into_Is) <- c(n_age, n_vacc, n_risk)
dim(aging_out_of_Is) <- c(n_age, n_vacc, n_risk)
dim(aging_into_Rc) <- c(n_age, n_vacc, n_risk)
dim(aging_out_of_Rc) <- c(n_age, n_vacc, n_risk)
dim(Npop_age_risk) <- c(n_age, n_risk)

dim(prop_maternal_vaccinated) <- n_risk
dim(prop_maternal_natural) <- n_risk
dim(vaccinated_mums) <- n_risk
dim(antibody_mums) <- n_risk

dim(tt_vaccination_coverage) <- no_vacc_changes
dim(vaccination_coverage) <- c(n_age, n_vacc, n_risk, no_vacc_changes)
dim(vaccination_prop) <- c(n_age, n_vacc, n_risk)

# --------------- DIMENSIONS ---------------

dim(waning_from_S_short) <- c(n_age, n_vacc, n_risk)
dim(waning_to_S_long) <- c(n_age, n_vacc, n_risk)
dim(waning_from_S_long) <- c(n_age, n_vacc, n_risk)
dim(waning_to_S_unvaccinated) <- c(n_age, n_vacc, n_risk)

dim(waning_from_E_short) <- c(n_age, n_vacc, n_risk)
dim(waning_to_E_long) <- c(n_age, n_vacc, n_risk)
dim(waning_from_E_long) <- c(n_age, n_vacc, n_risk)
dim(waning_to_E_unvaccinated) <- c(n_age, n_vacc, n_risk)

dim(waning_from_I_short) <- c(n_age, n_vacc, n_risk)
dim(waning_to_I_long) <- c(n_age, n_vacc, n_risk)
dim(waning_from_I_long) <- c(n_age, n_vacc, n_risk)
dim(waning_to_I_unvaccinated) <- c(n_age, n_vacc, n_risk)

dim(waning_from_R_short) <- c(n_age, n_vacc, n_risk)
dim(waning_to_R_long) <- c(n_age, n_vacc, n_risk)
dim(waning_from_R_long) <- c(n_age, n_vacc, n_risk)
dim(waning_to_R_unvaccinated) <- c(n_age, n_vacc, n_risk)

dim(waning_from_Is_short) <- c(n_age, n_vacc, n_risk)
dim(waning_to_Is_long) <- c(n_age, n_vacc, n_risk)
dim(waning_from_Is_long) <- c(n_age, n_vacc, n_risk)
dim(waning_to_Is_unvaccinated) <- c(n_age, n_vacc, n_risk)

dim(waning_from_Rc_short) <- c(n_age, n_vacc, n_risk)
dim(waning_to_Rc_long) <- c(n_age, n_vacc, n_risk)
dim(waning_from_Rc_long) <- c(n_age, n_vacc, n_risk)
dim(waning_to_Rc_unvaccinated) <- c(n_age, n_vacc, n_risk)
# 
# # --------------- PARAMETERS ---------------
short_term_waning <- parameter()
long_term_waning <- parameter()
dim(short_term_waning) <- c(n_vacc)
dim(long_term_waning) <- c(n_vacc)


# --------------- VACCINATION DIMENSIONS ---------------

dim(vaccinating_into_S) <- c(n_age, n_vacc, n_risk)
dim(vaccinating_out_of_S) <- c(n_age, n_vacc, n_risk)

dim(vaccinating_into_E) <- c(n_age, n_vacc, n_risk)
dim(vaccinating_out_of_E) <- c(n_age, n_vacc, n_risk)

dim(vaccinating_into_I) <- c(n_age, n_vacc, n_risk)
dim(vaccinating_out_of_I) <- c(n_age, n_vacc, n_risk)

dim(vaccinating_into_R) <- c(n_age, n_vacc, n_risk)
dim(vaccinating_out_of_R) <- c(n_age, n_vacc, n_risk)

dim(vaccinating_into_Is) <- c(n_age, n_vacc, n_risk)
dim(vaccinating_out_of_Is) <- c(n_age, n_vacc, n_risk)

dim(vaccinating_into_Rc) <- c(n_age, n_vacc, n_risk)
dim(vaccinating_out_of_Rc) <- c(n_age, n_vacc, n_risk)

dim(Births) <- n_risk
dim(reproductive_population) <- n_risk
dim(birth_rate) <- n_risk
dim(tt_birth_changes) <- no_birth_changes
dim(tt_death_changes) <- no_death_changes

dim(background_death) <- c(n_age, n_risk)
dim(Npop_background_death) <- c(n_age, n_risk)
dim(initial_background_death) <- c(n_age, n_risk)
dim(crude_birth) <- c(n_risk, no_birth_changes)
dim(crude_death) <- c(n_age, n_risk, no_death_changes)
dim(birth_int) <- n_risk
dim(death_int) <- c(n_age, n_risk)
dim(S_death) <- c(n_age, n_vacc, n_risk)
dim(E_death) <- c(n_age, n_vacc, n_risk)
dim(I_death) <- c(n_age, n_vacc, n_risk)
dim(R_death) <- c(n_age, n_vacc, n_risk)
dim(Is_death) <- c(n_age, n_vacc, n_risk)
dim(Rc_death) <- c(n_age, n_vacc, n_risk)

# Dimensions for risk movement
dim(moving_risk_from_S) <- c(n_age, n_vacc, n_risk)
dim(moving_risk_to_S) <- c(n_age, n_vacc, n_risk)
dim(moving_risk_from_E) <- c(n_age, n_vacc, n_risk)
dim(moving_risk_to_E) <- c(n_age, n_vacc, n_risk)
dim(moving_risk_from_I) <- c(n_age, n_vacc, n_risk)
dim(moving_risk_to_I) <- c(n_age, n_vacc, n_risk)
dim(moving_risk_from_R) <- c(n_age, n_vacc, n_risk)
dim(moving_risk_to_R) <- c(n_age, n_vacc, n_risk)
dim(moving_risk_from_Is) <- c(n_age, n_vacc, n_risk)
dim(moving_risk_to_Is) <- c(n_age, n_vacc, n_risk)
dim(moving_risk_from_Rc) <- c(n_age, n_vacc, n_risk)
dim(moving_risk_to_Rc) <- c(n_age, n_vacc, n_risk)
dim(tt_moving_risk) <- no_moving_risk_changes
dim(moving_risk_values) <-  c(n_age, n_vacc, n_risk, no_moving_risk_changes)
dim(moving_risk_distribution_values) <- c(n_age, n_vacc, n_risk, no_moving_risk_changes)
dim(moving_risk_prop) <- c(n_age, n_vacc, n_risk)
dim(moving_risk_distribution) <- c(n_age, n_vacc, n_risk)

#Dimensions for migration
dim(tt_migration) <- c(no_migration_changes)
dim(migration_distribution) <- c(6)
dim(migration_in_number) <- c(n_age, n_vacc, n_risk, no_migration_changes)
dim(migration_distribution_values) <- c(6, no_migration_changes)
dim(migration) <- c(n_age, n_vacc, n_risk)
dim(migration_adjusted) <- c(n_age, n_vacc, n_risk)
dim(migration_S) <- c(n_age, n_vacc, n_risk)
dim(migration_E) <- c(n_age, n_vacc, n_risk)
dim(migration_I) <- c(n_age, n_vacc, n_risk)
dim(migration_R) <- c(n_age, n_vacc, n_risk)
dim(migration_Is) <- c(n_age, n_vacc, n_risk)
dim(migration_Rc) <- c(n_age, n_vacc, n_risk)

# Dimensions for age transition output variables
dim(S_after_aging) <- c(n_age, n_vacc, n_risk)
dim(E_after_aging) <- c(n_age, n_vacc, n_risk)
dim(I_after_aging) <- c(n_age, n_vacc, n_risk)
dim(R_after_aging) <- c(n_age, n_vacc, n_risk) 
dim(Is_after_aging) <- c(n_age, n_vacc, n_risk)
dim(Rc_after_aging) <- c(n_age, n_vacc, n_risk)

# Dimensions for vaccination transition output variables
dim(S_after_vaccination) <- c(n_age, n_vacc, n_risk)
dim(E_after_vaccination) <- c(n_age, n_vacc, n_risk)
dim(I_after_vaccination) <- c(n_age, n_vacc, n_risk)
dim(R_after_vaccination) <- c(n_age, n_vacc, n_risk)
dim(Is_after_vaccination) <- c(n_age, n_vacc, n_risk)
dim(Rc_after_vaccination) <- c(n_age, n_vacc, n_risk)

# Dimensions for waning transition output variables
dim(S_after_waning) <- c(n_age, n_vacc, n_risk)
dim(E_after_waning) <- c(n_age, n_vacc, n_risk)
dim(I_after_waning) <- c(n_age, n_vacc, n_risk)
dim(R_after_waning) <- c(n_age, n_vacc, n_risk)
dim(Is_after_waning) <- c(n_age, n_vacc, n_risk)
dim(Rc_after_waning) <- c(n_age, n_vacc, n_risk)