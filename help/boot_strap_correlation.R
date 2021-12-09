## Bettega Paul
## 2021_07_09 ==> 2021_07_09
## Function to compute correlation

sample_subject_period <- function(x, n_period) {
  sampled_period <- sample(x, 2 * n_period)
  group_sampler <- sample(rep(c(TRUE, FALSE), n_period))
  list('period' = sampled_period, 'sampler' = group_sampler)
}


cor_ind_random <- function(subjects_play, n_period) {
  sampled <- lapply(subjects_play, sample_subject_period, n_period)
  play <- unlist(lapply(sampled, `[[`, 1))
  sampler <- unlist(lapply(sampled, `[[`, 2))
  
  cor(play[sampler], play[!sampler])
}

cor_par_random <- function(subjects_play, n_period) {
  max_period <- length(subjects_play[[1]])
  treatment <- sample(c(rep('a', n_period), rep('b', n_period), 
                        rep('exclude', max_period - 2 * n_period)))
  ta <- unlist(lapply(subjects_play, `[`, treatment == 'a'))
  tb <- unlist(lapply(subjects_play, `[`, treatment == 'b'))
  
  cor(ta, tb)
}

cor_par_consec <- function(subjects_play, n_period) {
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
  
  cor(ta, tb)
}

cor_par_orderer <- function(subjects_play, n_period) {
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
  
  cor(ta, tb)
}