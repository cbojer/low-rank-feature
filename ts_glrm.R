ts_glrm <- function(dt, k = 20) {
  
  wide_dt <- dt %>% to_wide()
  
  h2o_ds <-  wide_dt %>% as.h2o()
  
  glrm <- h2o.glrm(
    training_frame = h2o_ds,
    k = k, 
    loss = "Quadratic",
    regularization_x = "None", 
    regularization_y = "None", 
    transform = "STANDARDIZE", 
    max_iterations = 2000,
    seed = 123,
    impute_original = TRUE,
    ignore_const_cols = FALSE
  )
  
  reconstructed <- h2o.reconstruct(glrm, h2o_ds, reverse_transform = TRUE) %>% as.data.table()
  
  
  return(
    structure(
      list(
        model = glrm,
        rec = reconstructed,
        err = wide_dt - reconstructed
      ),
      class = "ts_glrm"
    )
  )
}
