
mu <- 5
sigma <-  10

x <- -40


# mixture components. These are independent of any sample values
# mix_comp_1
# mix_comp_2

# Each point has a membership weight for each component
# mem_weight_1:
# mem_weight_2:

# mu_1, sd_1
# mu_2, sd_2


ls_mu <- list(`1` = 5,
              `2` = 20)
ls_sd <- list(`1` = 3,
              `2` = 3)
ls_alpha <- list(`1` = 0.5,
                 `2` = 0.5)


xs <- c(1, 2, 3, 4, 5, 6, 3, 10, 33)

wh_x <- 1


# Update membership weights for each sample for each component
# Return:
e_step <- function(xs,
                   ls_mu,
                   ls_sd,
                   ls_alpha) {

  ls_weight_mem_dfs <- list()

  for (i in seq_along(xs)) {

    wh_x <- xs[i]

    # Compute the membership weight for
    vec_unnorm_mem_weights <- c()
    for (wh_comp in names(ls_mu)) {
      unnorm_mem_weight <- ls_alpha[[wh_comp]] * dnorm(wh_x, mean = ls_mu[[wh_comp]], sd = ls_sd[[wh_comp]])
      vec_unnorm_mem_weights <- c(vec_unnorm_mem_weights,
                                  unnorm_mem_weight)
    }
    # Normalize the membership weights
    vec_mem_weights <- vec_unnorm_mem_weights / sum(vec_unnorm_mem_weights)
    ls_weight_mem_dfs[[i]] <- data.frame(t(c(wh_x, vec_mem_weights)))
  }

  #  xs            1            2
  #  1  1.000000e+00 4.742665e-09
  #  2  1.000000e+00 2.510999e-08
  #  3  9.999999e-01 1.329446e-07
  df_all_mem_weights <- dplyr::bind_rows(ls_weight_mem_dfs)
  colnames(df_all_mem_weights) <- c("xs", names(ls_mu))

  df_all_mem_weights
}


m_step <- function(df_all_mem_weights) {

 vec_colnames <- colnames(df_all_mem_weights)
 stopifnot(vec_colnames[1] == "xs")

 # Get the component names, e.g., c("1", "2")
 component_names <- vec_colnames[2:length(vec_colnames)]

 df_mem_weights_only <- df_all_mem_weights[, 2:ncol(df_all_mem_weights)]

 # Get the named numeric vector of the sum of all membership weights
 vec_unnorm_alphas <- colSums(df_mem_weights_only)
 vec_alphas <- vec_unnorm_alphas / sum(vec_unnorm_alphas)

 updated_ls_alpha <- as.list(vec_alphas)

 # Update means
 vec_mus <- c()
 for (wh_component in component_names) {


   df_xs_mem_weights <- df_all_mem_weights %>%
     dplyr::select(c("xs", wh_component)) %>%
     dplyr::mutate(x_times_mem_weight =  xs * !!rlang::sym(wh_component))

   total_mem_weight <- df_xs_mem_weights %>%
     dplyr::pull(!!rlang::sym(wh_component)) %>%
     sum()

   sum_x_times_mem_weight <- df_xs_mem_weights %>%
     dplyr::select(x_times_mem_weight) %>%
     sum()

   updated_mu <- sum_x_times_mem_weight / total_mem_weight

   vec_mus <- c(vec_mus,
                updated_mu)
 }
 names(vec_mus) <- component_names
 ls_updated_mus <- as.list(vec_mus)

 # Update  sdevs
 wh_component <- "2"
 vec_stds <- c()
 for (wh_component in component_names) {

   comp_mu <- ls_updated_mus[[wh_component]]

   df_xs_vars <- df_all_mem_weights %>%
     dplyr::select(c("xs", wh_component)) %>%
     dplyr::mutate(mem_weight_var =  !!rlang::sym(wh_component) * (x - comp_mu)**2)

   total_mem_weight <- df_xs_vars %>%
     dplyr::pull(!!rlang::sym(wh_component)) %>%
     sum()

   sum_vars <- df_xs_vars %>%
     dplyr::select(mem_weight_var) %>%
     sum()

   updated_var <- sum_vars / total_mem_weight

   vec_stds <- c(vec_stds, sqrt(updated_var))
 }
 names(vec_stds) <- component_names
 ls_updated_stds <- as.list(vec_stds)

 list(ls_alpha = updated_ls_alpha,
      ls_mu = ls_updated_mus,
      ls_std = ls_updated_stds)
}


prev_ll <- 0

true_mu <- list(`1` = 100,
                `2` = 50)
true_sigma <- list(`1` = 15,
                   `2` = 10)
true_alpha <- list(`1` = 0.2,
                   `2` = 0.80)

n_obs <- 10000


draw_component <- function(ls_alphas) {

  r <- runif(1, min = 0, max = sum(unlist(ls_alphas)))

  vec_components <- names(ls_alphas)
  cum_sum <- 0
  for (wh_comp in vec_components) {
    cum_sum <- cum_sum + ls_alphas[[wh_comp]]
    if (r <= cum_sum) {
      return(wh_comp)
    }
  }
  stop("error: could not draw random component")
}


ls_dfs <- list()
for (i in 1:n_obs) {
  # Draw the true component
  actual_component <- draw_component(true_alpha)

  # Get a random draw
  x <- rnorm(1,
             mean = true_mu[[actual_component]],
             sd = true_sigma[[actual_component]])

  ls_dfs[[i]] <- data.frame(x = x,
                      component = actual_component,
                      stringsAsFactors = FALSE)
}
df_xs <- dplyr::bind_rows(ls_dfs)


# Get all the xs. Sort from highest to lowest, and get the initial
# means and standard deviations
xs <-  df_xs$x

half_n <- round(length(xs) / 2)

sorted_xs <- sort(xs)
first_half_xs <- sorted_xs[1:half_n]
second_half_xs  <- sorted_xs[(half_n+1):length(sorted_xs)]

ls_mu <- list("1" = mean(first_half_xs),
              "2" = mean(second_half_xs))
ls_sigma <- list("1" = sd(first_half_xs),
                 "2" = sd(second_half_xs))
ls_alpha <- list("1" = 0.5,
                 "2" = 0.5)



prev_ll <- -1


for (iter in 1:100) {

  print(iter)

  # > df_updated_membership_weights
  # xs            1            2
  #  1 1.000000e+00 4.742665e-09
  #  2 1.000000e+00 2.510999e-08
  #  3 9.999999e-01 1.329446e-07
  #  4 9.999993e-01 7.038734e-07
  #  5 9.999963e-01 3.726639e-06
  df_updated_membership_weights <- e_step(xs,
                                          ls_mu,
                                          ls_sd,
                                          ls_alpha)

  # Get updated parameters
  ls_updated_params <- m_step(df_updated_membership_weights)

  # Get the log likelihood
  sum_ll <- 0
  for (x in xs) {
    sum_weighted_likelihood <- 0
    for (wh_component in component_names) {

      alpha <- ls_updated_params$ls_alpha[[wh_component]]
      mu <- ls_updated_params$ls_mu[[wh_component]]
      std <- ls_updated_params$ls_std[[wh_component]]

      weighted_likelihood <- alpha *dnorm(x, mean = mu, sd = std)
      sum_weighted_likelihood <- sum_weighted_likelihood + weighted_likelihood
    }
    log_likelihood <- log(sum_weighted_likelihood)
    sum_ll <- sum_ll + log_likelihood
  }

  delta_ll <- abs(sum_ll - prev_ll)
  rel_change <- delta_ll / abs(prev_ll)
  print(sprintf("rel_change: %.6f", rel_change))
  if (rel_change < 0.000001) {
    break
  }

  # Update the prev_ll
  prev_ll <- sum_ll

  ls_mu <- ls_updated_params$ls_mu
  ls_sd <- ls_updated_params$ls_std
  ls_alpha <- ls_updated_params$ls_alpha

}

hist(xs,
     breaks = -50:200)

xs_plt <- 1:20000 / 100

ys_1 <- length(xs) * ls_alpha[["1"]] * dnorm(xs_plt, mean = ls_mu[["1"]], sd = ls_sd[["1"]])
ys_2 <- length(xs) * ls_alpha[["2"]] * dnorm(xs_plt, mean = ls_mu[["2"]], sd = ls_sd[["2"]])


plot(xs_plt, ys_1)
plot(xs_plt, ys_2)
hist(xs,
     breaks = -50:200)





