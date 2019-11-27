

step_group_mean_new <- 
  function(
    terms, 
    role, 
    trained, 
    outcome, 
    mapping,
    alpha,
    # prefix, 
    skip, 
    id 
  ) {
    
    step(
      subclass = "group_mean",
      terms    = terms,
      role     = role,
      trained  = trained,
      outcome  = outcome, 
      mapping  = mapping,
      alpha    = alpha,
      skip     = skip,
      id       = id
    )
    
  }


# ---------


step_group_mean <- 
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    outcome = NULL,
    mapping = NULL,
    alpha   = NULL,
    skip    = FALSE,
    id      = rand_id("group_mean")
  ){
    
    # if (missing(outcome)) {
    #   stop('argument "outcome" is missing, with no default', call. = FALSE)
    # }
    
    add_step(
      recipe,
      step_group_mean_new(
        terms   = ellipse_check(...),
        role    = role,
        trained = trained,
        outcome = outcome,
        mapping = mapping,
        alpha   = alpha,
        # prefix = prefix,
        skip    = skip,
        id      = id
      )
    )
    
  }

# ---------

prep.step_group_mean <- 
  function(
    x, 
    training, 
    info = NULL, 
    ...
  ) {
    
    col_names <- terms_select(x$terms, info = info)
    y_name <- terms_select(x$outcome, info = info)
    alpha_smooth <- if (is.null(x$alpha)) 0 else x$alpha
    y_name_final <- paste(paste0(col_names, collapse = "_"), y_name, sep = "_")
    
    global_mean <-  
      mean(training[[y_name]], na.rm = T)
    
    res <- 
      training %>% 
      dplyr::group_by(!!!syms(col_names)) %>% 
      dplyr::summarise(
        local_mean = mean(!!sym(y_name), na.rm = TRUE),
        n = dplyr::n()
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        
        sm = 
          (local_mean * n + global_mean * alpha_smooth) / (n + alpha_smooth)
        
      ) %>% 
      dplyr::select(-c(local_mean, n))  %>% 
      tibble::add_row(sm = global_mean) %>% # for replacement NA
      dplyr::rename_at(dplyr::vars(sm), ~paste(y_name_final, ., sep = "_"))
    
    print(res)
    
    step_group_mean_new(
      terms = x$terms,
      role = x$role,
      trained = TRUE,
      outcome = x$outcome,
      mapping = res,
      alpha   = x$alpha,
      skip = x$skip,
      id = x$id
    )
    
  }



bake.step_group_mean <- 
  function(
    object, 
    new_data,
    ...
  ) {
    
    
    
    col_by <- colnames(object$mapping)
    col_by <- col_by[-length(col_by)]
    
    new_data <- 
      new_data %>% 
      dplyr::left_join(object$mapping, by = col_by)
    
    new_data
    
  }



print.step_group_mean <-
  function(
    x, 
    width = max(20, options()$width - 30),
    ...
  ) {
    cat("Creating MEAN variables for ", sep = "")
    printer(names(x$mapping[-length(x$mapping)]), x$terms, x$trained, width = width)
    invisible(x)
  }


tidy.step_group_mean <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$columns)
  } else {
    res <- tibble(terms = sel2char(x$terms))
  }
  res
}





# mtcars_tr <- mtcars[1:20, ] 
# mtcars_ts <- mtcars[21:32, ]
# 
# rr <- mtcars_tr %>% 
#   recipe() %>% 
#   step_group_mean(cyl, outcome = vars(mpg), alpha = 1) %>% 
#   prep()
# 
# rr %>% juice %>% distinct(cyl, cyl_mpg_sm)
# 
# bake(rr, mtcars_ts) %>% distinct(cyl, cyl_mpg_sm)
# 
# mtcars_tr %>% 
#   group_by(cyl) %>% 
#   summarise(
#     mlocal = mean(mpg, na.rm = T),
#     n = n()
#   ) %>% 
#   mutate(
#     global_mean =  mean(mtcars_tr$mpg, na.rm = TRUE),
#     sm_mean = ( mlocal * n +global_mean * 1) / (n + 1)
#   )
# 
# 
# tidy(rr)
# print(rr)