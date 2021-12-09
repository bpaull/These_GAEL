## Bettega Paul
## Function file for tempting laboratory 2
## 2021_09_21 => 2021_11_04

## Divers ====
#' Print glue string
#' Print a glue string by passing it to print(glue::glue(.)).
#' The value is print if the global var .p_lvl is higher egal than print level.
#' The function create the .p_val if doesn't exist. 
#'
#' @param x A glue string
#' @param print_level integer to compare to .p_lvl to test if print is done.
#'
#' @return
#' @export
#'
#' @examples
gprint <- function(x, print_level = 0)
{
  pf <- parent.frame()
  if (!exists(".p_lvl")) {
    message("No variable .p_lvl find one with value Inf is create")
    pf$.p_lvl <- Inf
  }
  if (print_level <= pf$.p_lvl) {
    print(glue::glue(x, .envir = pf))
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

#' Test if a vector is sorted
#'
#' @param x a vector
#'
#' @return logical. TRUE if x is sorted FALSE if not
#' @export
#'
#' @examples is_sorted(letters)
is_sorted <- function(x) 
{
  all(x == sort(x))
}


#' Vectorizes application of min(max(...))
#'
#' @param X vector
#' @param low lower value return
#' @param high highter value return
#'
#' @return
#' @export
#'
#' @examples
limit <- function(X, low, high) {
  sapply(X, function(x) min(max(low, x), high))
}

#' Unliste Map
#' 
#' unlist of Map result. unlist(Map(f, ...))
#'
#' @param f function for Map()
#' @param ... extra parameter pass to Map()
#' @param recursive logical. Should unlisting be applied to list components
#' @param use.names logical. Should names be preserved?
#'
#' @return
#' @export
#'
#' @examples
sMap <- function(f, ..., recursive = FALSE, use.names = FALSE)
{
  unlist(Map(f, ...), recursive = recursive, use.names = use.names)
}

electe_major <- function(x, threshold = 0)
{
  n <- length(x)
  obs <- table(x)/n
  obs <- obs[obs > threshold]
  if (length(obs) == 0) return(NA)
  if (length(which(obs == max(obs))) > 1) return(NA)
  names(obs)[which.max(obs)]
}

classif <- function(obs, pred) {
  if (is.na(obs) | is.na(pred)) return(NA)
  if (obs & pred) return('TP')
  if (!obs & !pred) return('TN')
  if (obs & !pred) return('FN')
  if (!obs & pred) return('FP')
  stop("error in classif")
}

accuracy <- function(x) {
  mean((x == 'TP' | x == 'TN'), na.rm = TRUE)
}

precision <- function(x) {
  x <- x[!is.na(x)]
  sum(x == 'TP')/sum((x == 'TP' | x == 'FP'))
}

recall <- function(x) {
  x <- x[!is.na(x)]
  sum(x == 'TP')/sum((x == 'TP' | x == 'FN'))
}

F1_score <- function(x) {
  p <- precision(x)
  r <- recall(x)
  
  2 * p * r / (p + r)
}

mse <- function(pred, obs, na.rm = TRUE) 
  mean((pred - obs)^2, na.rm = na.rm)

corr_sign <- function(pred, obs, na.rm = TRUE) 
  mean(sign(pred) == sign(obs), na.rm = na.rm)

## General Menu ====

encode_menu <- function(menu){
  code_list <- str_extract_all(menu, "\\d{1,2}(?=\\))")
  unlist(lapply(code_list, function(x) paste0("{", paste(x, collapse = ", "), "}")))
}

menu_cardinal <- function(coded_menu) {
  str_count(coded_menu, "\\d{1,2}")
}

reducte_cId <- function(x) {
  values <- sort(as.numeric(unique(x)))
  y <- structure(seq_along(values), names = values)
  
  y[x]
}

#' Return element in a menu
#'
#' @param menu a menu
#'
#' @return vector of element
#' @export
#'
#' @examples
menu__element <- function(menu)
{
  unlist(str_extract_all(menu, "[1-9]{1,2}"))
}

#' Prefered element
#' 
#' Return the prefered element contain in menu based on estimation in s1_estim.
#'
#' @param subject_id character. Id matching column in s1_estim.
#' @param menu character. A menu.
#' @param s1_estim matrix. Element estimation with element in rows and subjects
#' in column. 
#'
#' @return an element of menu
#' @export
#'
#' @examples
pref_element <- function(subject_id, menu, s1_estim = s1_menu_val) 
{
  menu_element <- menu__element(menu)
  if (length(menu_element) == 1) return(menu_element)
  res <- names(which.max(s1_estim[menu_element, subject_id]))
  if (is.null(res)) return(NA)
  res
}

#' Value of prefered element
#' 
#' Return the value of the prefered element in a menu based on estimation in 
#' s1_estim
#'
#' @param subject_id character. Id matching column in s1_estim.
#' @param menu character. A menu.
#' @param s1_estim matrix. Element estimation with element in rows and subjects
#' in column. 
#'
#' @return numeric value
#' @export
#'
#' @examples
pref_element_value <- function(subject_id, menu, s1_estim = s1_menu_val) 
{
  p_elem <- pref_element(subject_id, menu, s1_estim)
  if (is.na(p_elem)) return(NA)
  s1_estim[p_elem, subject_id]
}



contains.menu <- function(menu1, menu2)
{
  el1 <- menu__element(menu1)
  el2 <- menu__element(menu2)
  
  setequal(intersect(el1, el2), el2)
}

commons_element <- function(menu1, menu2)
{
  intersect(menu__element(menu1), menu__element(menu2))
}

size__type <- function(s1, s2) {
  if (s1 == 1 & s2 == 1) return("1 vs 1")
  if ((s1 == 2 & s2 == 1) | (s1 == 1 & s2 == 2)) return("2 vs 1")
  if (s1 == 2 & s2 == 2) return("2 vs 2")
  if ((s1 == 3 & s2 == 2) | (s1 == 2 & s2 == 3)) return("3 vs 2")
  if ((s1 == 4 & s2 == 2) | (s1 == 2 & s2 == 4)) return("4 vs 2")
  if ((s1 == 5 & s2 == 2) | (s1 == 2 & s2 == 5)) return("5 vs 2")
  if ((s1 == 5 & s2 == 4) | (s1 == 4 & s2 == 5)) return("5 vs 3")
  if ((s1 == 6 & s2 == 1) | (s1 == 1 & s2 == 6)) return("6 vs 1")
  if ((s1 == 6 & s2 == 2) | (s1 == 2 & s2 == 6)) return("6 vs 2")
  if ((s1 == 6 & s2 == 4) | (s1 == 4 & s2 == 6)) return("6 vs 4")
  if ((s1 == 6 & s2 == 5) | (s1 == 5 & s2 == 6)) return("6 vs 5")
  
  NA
}

menus__binMatrice <- function(Menus, prefix = '', suffixe = '') {
  unique_value <- unique(menu__element(Menus))
  unique_value <- as.character(sort(as.numeric(unique_value))) # Specific sort
  
  res <- matrix(0, nrow = length(Menus), ncol = length(unique_value),
                dimnames = list(names(Menus), 
                                unique_value))
  
  for(i in seq_along(Menus)) {
    res[i, menu__element(Menus[i])] <- 1
  }
  
  colnames(res) <- paste0(prefix, unique_value, suffixe)
  res
}

## Size 1 menu relative value ====

#' Parse equation from Menu choice Dataframe
#' 
#' Create a structure if expression from the menu_df dataframe comparaison of 
#' size 1 menu. This function return expression that can be evaluate to fill 
#' a structure to compute the value of size 1 menu
#'
#' @param menu_df a dataframe of menu comparaison
#' @param subject a subject matching a value in menu_df$subject_id 
#'
#' @return An expresion structure to filling struc_name
#' @export
#'
#' @examples
parse_equation <- function(menu_df, subject, struc_name = "it_value") {
  lot_names <- c("2", "4", "6", "8", "16","32")
  
  sub_1s <- menu_df %>% # retrieve size 1 menu comparaison
    filter(subject_id == subject, choice_id > 10,
           ml_size == 1, mr_size == 1)
  
  sub_eq <- c()
  for(i in seq_len(nrow(sub_1s))) {
    left <- str_extract(sub_1s$menu_l[i], "[0-9]{1,2}")
    right <- str_extract(sub_1s$menu_r[i], "[0-9]{1,2}")
    wtc <- sub_1s$wtc[i]
    
    l_eq <- glue::glue("{struc_name}[['{left}']] <- c({struc_name}[['{left}']], 
                       value['{right}', i] + {wtc})")
    r_eq <- glue::glue("{struc_name}[['{right}']] <- c({struc_name}[['{right}']], 
                       value['{left}', i] - {wtc})")
    
    sub_eq <- c(sub_eq, l_eq, r_eq)
  }
  
  parse(text = sub_eq)
}

parse_equation_temp <- function(menu_df, subject, struc_name = "it_value") {
  lot_names <- c("2", "4", "6", "8", "16","32")
  
  sub_menu <- menu_df %>% # retrieve size 1 menu comparaison
    filter(subject_id == subject, choice_id > 10)
  
  sub_eq <- c()
  for(i in seq_len(nrow(sub_menu))) {
    pref <- sub_menu$l_pref_elem[i]
    temp <- setdiff(
      str_extract_all(sub_menu$l_menu[i], "[0-9]{1,2}", simplify = TRUE),
      pref)
    t_wtc <- sub_menu$t_wtc[i]
    o_wtc <- sub_menu$o_wtc[i]
    d_w <- o_wtc - t_wtc
    
    l_eq <- glue::glue("{struc_name}[['{pref}']] <- c({struc_name}[['{pref}']], 
                       value['{temp}', i] + {d_w})")
    r_eq <- glue::glue("{struc_name}[['{temp}']] <- c({struc_name}[['{temp}']], 
                       value['{pref}', i] - {d_w})")
    
    sub_eq <- c(sub_eq, l_eq, r_eq)
  }
  
  parse(text = sub_eq)
}

#' Fill a structure with expression 
#'
#' @param sub_eq structure expression as return by \link{parse_equation}
#' @param ref_lot element of lot_names use as starting point
#' @param nb_it number of iteration to do on expression
#' @param lot_names element to compute value
#'
#' @return
#' @export
#'
#' @examples
fill_structure <- function(sub_eq, ref_lot = '2', nb_it = 500, 
                           lot_names = c("2", "4", "6", "8", "16","32")) {
  if(!(ref_lot %in% lot_names)) 
    stop(glue::glue("reference value : {ref_lot} not in the specified value"))
  
  value <- matrix(NA_real_, 
                  nrow = length(lot_names), ncol = nb_it, 
                  dimnames = list(lot_names, seq_len(nb_it) - 1))
  value[ref_lot, 1] <- 0
  
  
  for(i in seq_len(nb_it - 1)) {
    it_value <- list(NULL, NULL, NULL, NULL, NULL, NULL)
    names(it_value) <- lot_names
    
    eval(sub_eq) # remplit it_value
    
    value[, i + 1] <- unlist(lapply(it_value, mean, na.rm = TRUE))
  }
  
  value
}


#' Compute relative value of size 1 menu
#'
#' @param menu_df a data frame of menu comparison.
#' @param nb_it number of iteration to run on equation.
#' @param drop_it number of iteration to drop before compute mean.
#' @param lot_names names of elements in menus.
#' @param ref_lot element of lot_names who's initial value is set to 0.
#' @param norm_lot element of lot_names who's relative value is set to 0.
#'
#' @return numerical matrix where row is elements of menus and column is 
#' subjects.
#' @export
#'
#' @examples
s1_value <- function(menu_df, 
                     nb_it = 500, drop_it = 450,
                     lot_names = c("2", "4", "6", "8", "16","32"), 
                     ref_lot = "2", norm_lot = "2") {
  sub_id <- unique(menu_df$subject_id)
  
  u <- array(NA_real_, 
             dim = c(length(lot_names), nb_it, length(sub_id)),
             dimnames = list(lot_names, seq_len(nb_it) - 1, sub_id))
  ## Iteratly compute value for the different lottery using size 1 menu
  for(t_sub in sub_id) {
    s_eq <- parse_equation(menu_df, t_sub, struc_name = "it_value")
    
    u[, ,t_sub] <- fill_structure(s_eq, ref_lot = ref_lot)
  }
  
  ## Use last n iteration to compute mean value
  n <- nb_it - drop_it
  lot_u <- apply(u[, (nb_it - n):nb_it, ], c(1, 3), mean, na.rm = TRUE)
  t(t(lot_u) - lot_u[norm_lot, ])
}



temp_value <- function(menu_df, 
                     nb_it = 500, drop_it = 450,
                     lot_names = c("2", "4", "6", "8", "16","32"), 
                     ref_lot = "2", norm_lot = "2") {
  
  sub_id <- unique(menu_df$subject_id)
  
  u <- array(NA_real_, 
             dim = c(length(lot_names), nb_it, length(sub_id)),
             dimnames = list(lot_names, seq_len(nb_it) - 1, sub_id))
  ## Iteratly compute value for the different lottery using size 1 menu
  for(t_sub in sub_id) {
    s_eq <- parse_equation_temp(menu_df, t_sub, struc_name = "it_value")
    
    u[, ,t_sub] <- fill_structure(s_eq, ref_lot = ref_lot)
  }
  
  ## Use last n iteration to compute mean value
  n <- nb_it - drop_it
  lot_u <- apply(u[, (nb_it - n):nb_it, ], c(1, 3), mean, na.rm = TRUE)
  t(t(lot_u) - lot_u[norm_lot, ])
}

## Preference Test ====

#' Test validity of preference
#'
#' @param x vector of value as 
#'
#' @return
#' @export
#'
#' @examples
valid_pref <- function(x, print_level = 3) 
{
  .p_lvl <- 0
  x <- x[!is.na(x)]
  n <- length(x)
  
  max_ind <- which.max(x)
  
  left <- x[1:max_ind]
  right <- rev(x[max_ind:n])
  
  if (!(is_sorted(left) & is_sorted(right)))
    gprint("=======================================
    X Not sorted :
           {paste(x, collapse = ', ')}", print_level)
  
  if (!is_sorted(left))
    gprint("left not sorted :
           \t left -- {paste(left, collapse = ', ')}", print_level)
  if (!is_sorted(right))
    gprint("right not sorted :
           \t right -- {paste(right, collapse = ', ')}", print_level)
  
  is_sorted(left) & is_sorted(right)
}

is_full_estimate <- function(sub_id, m1, m2, estimate) 
{
  items <- union(menu__element(m1), menu__element(m2))
  
  !any(is.na(estimate[items, sub_id]))
}

## Temptation models ====

GP_wtc <- function(left, right, subject_id, u_est, v_est)
{
  uv_est <- as.matrix(u_est[, subject_id] + v_est[, subject_id])
  colnames(uv_est) <- subject_id
  
  l_u <- pref_element_value(subject_id, left,  uv_est)
  r_u <- pref_element_value(subject_id, right, uv_est)
  l_v <- pref_element_value(subject_id, left,  v_est)
  r_v <- pref_element_value(subject_id, right, v_est)
  
  if (menu_cardinal(left) == 1) {
    l_u <- pref_element_value(subject_id, left,  u_est)
    l_v <- 0
  }
  
  if (menu_cardinal(right) == 1) {
    r_u <- pref_element_value(subject_id, right,  u_est)
    r_v <- 0
  }
  
  l_u - l_v - (r_u - r_v)
}

ait_wtc <- function(left, right, subject_id, u_est, v_est)
{
  uv_est <- as.matrix(u_est[, subject_id] + v_est[, subject_id])
  colnames(uv_est) <- subject_id
  
  l_u <- pref_element_value(subject_id, left,  uv_est)
  r_u <- pref_element_value(subject_id, right, uv_est)
  
  l_v <- sum(v_est[menu__element(left),  subject_id], na.rm = TRUE)
  r_v <- sum(v_est[menu__element(right), subject_id], na.rm = TRUE)
  
  l_u - l_v - (r_u - r_v)
}


tait_wtc <- function(left, right, subject_id, u_est, v_est)
{
  uv_est <- as.matrix(u_est[, subject_id] + v_est[, subject_id])
  colnames(uv_est) <- subject_id
  
  l_u <- pref_element_value(subject_id, left,  uv_est)
  r_u <- pref_element_value(subject_id, right, uv_est)
  
  l_v <- sum(limit(v_est[menu__element(left),  subject_id], 0, Inf), na.rm = TRUE)
  r_v <- sum(limit(v_est[menu__element(right), subject_id], 0, Inf), na.rm = TRUE)
  
  l_u - l_v - (r_u - r_v)
}


bootstrap_lm.fit <- function(formula, data, ..., sample_size, nstrap, 
                             replace = FALSE)
{
  replicate(nstrap, {
    lm(m_mod, 
       reg_data[sample(nrow(reg_data), sample_size, replace = replace), ]
       )$coefficients
  })
}

bootstrap_lm.predict <- function(b_lm_coef, new_data)
{
  mean_coef <- rowMeans(b_lm_coef, na.rm = TRUE)
  
  intercep <- mean_coef["(Intercept)"]
  if (is.na(intercep)) intercep <- 0
  coef <- mean_coef[names(mean_coef) != "(Intercept)"]
  
  pred_data <- as.matrix(new_data[, names(coef)])
  
  pred_data %*% coef + intercep
}



