## Bettega Paul
## 2021_06_07 ==> 2021_06_07
## Function to compute p.value for ghost treatment

sample_subject_period <- function(x, n_period) {
  sampled_period <- sample(x, 2 * n_period)
  group_sampler <- sample(rep(c(TRUE, FALSE), n_period))
  list('period' = sampled_period, 'sampler' = group_sampler)
}


ghost_treatment_ind_random <- function(subjects_play, n_period) {
  sampled <- lapply(subjects_play, sample_subject_period, n_period)
  play <- unlist(lapply(sampled, `[[`, 1))
  sampler <- unlist(lapply(sampled, `[[`, 2))
  
  t.test(play[sampler], play[!sampler])$p.value
}

ghost_treatment_par_random <- function(subjects_play, n_period) {
  max_period <- length(subjects_play[[1]])
  treatment <- sample(c(rep('a', n_period), rep('b', n_period), 
                        rep('exclude', max_period - 2 * n_period)))
  ta <- unlist(lapply(subjects_play, `[`, treatment == 'a'))
  tb <- unlist(lapply(subjects_play, `[`, treatment == 'b'))
  
  t.test(ta, tb)$p.value
}

ghost_treatment_par_consec <- function(subjects_play, n_period) {
  max_period <- length(subjects_play[[1]])
  valid_tt <- n_period:(max_period - n_period)
  treatment_date <- ifelse(length(valid_tt) == 1, valid_tt,
                           sample(valid_tt, 1))
  treatment <- c(rep('exclude', treatment_date - n_period),
                 rep('a', n_period),
                 rep('b', n_period),
                 rep('exclude', max_period - n_period - treatment_date))
  
  ta <- unlist(lapply(subjects_play, `[`, treatment == 'a'))
  tb <- unlist(lapply(subjects_play, `[`, treatment == 'b'))
  
  t.test(ta, tb)$p.value
}

ghost_treatment_par_orderer <- function(subjects_play, n_period) {
  max_period <- length(subjects_play[[1]])
  valid_tt <- n_period:(max_period - n_period)
  treatment_date <- ifelse(length(valid_tt) == 1, valid_tt,
                           sample(valid_tt, 1))
  treatment <- c(sample(c(rep('exclude', treatment_date - n_period),
                 rep('a', n_period))),
                 sample(c(rep('b', n_period),
                 rep('exclude', max_period - n_period - treatment_date))))
  
  ta <- unlist(lapply(subjects_play, `[`, treatment == 'a'))
  tb <- unlist(lapply(subjects_play, `[`, treatment == 'b'))
  
  t.test(ta, tb)$p.value
}

ghost_treatment_beetween <- function(subjects_play_list, nb_sub_by_t, nb_choice) {
  max_choice <- length(subjects_play_list[[1]])
  stopifnot(nb_choice <= max_choice)
  stopifnot(length(subjects_play_list) >= 2 * nb_sub_by_t)
  
  choi_sampler <- sample(c(rep(TRUE, nb_choice), rep(FALSE, max_choice - nb_choice)))
  play <- lapply(subjects_play_list, `[`, choi_sampler)
  
  sub_sampler <- sample(c(rep(c('a', 'b'), nb_sub_by_t), 
                          rep('exclude', 
                              length(subjects_play_list) - 2 * nb_sub_by_t)))
  
  play_by_treat <- split(play, sub_sampler)
  t.test(unlist(play_by_treat['a']), unlist(play_by_treat['b']))$p.value
}

ghost_treatment_diffdiff <- function(subjects_play_list, nb_sub_by_t, n_period) {
  max_choice <- min(unlist(lapply(subjects_play_list, length)))
  
  stopifnot(n_period * 2 <= max_choice)
  stopifnot(length(subjects_play_list) >= 2 * nb_sub_by_t)
  
  sub_play <- sample(subjects_play_list, 2 * nb_sub_by_t)
  
  valid_tt <- n_period:(max_choice - n_period)
  treatment_date <- ifelse(length(valid_tt) == 1, valid_tt,
                           sample(valid_tt, 1))
  choi_sampler <- c(rep('exclude', treatment_date - n_period),
                    rep('a', n_period),
                    rep('b', n_period),
                    rep('exclude', max_choice - n_period - treatment_date))
  
  diff_mean <- lapply(sub_play, function(x) {
    mean(x[choi_sampler == 'a']) - mean(x[choi_sampler == 'b'])
  })
  
  ind_treat <- split(diff_mean, sample(rep(c('A', 'B'), each = nb_sub_by_t)))
  
  t.test(unlist(ind_treat['A']), unlist(ind_treat['B']))$p.value
}
