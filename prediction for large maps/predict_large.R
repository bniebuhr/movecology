#' Predict for large datasets
#' 
#' Predicts fits from models to large datasets by subdividing (slicing) 
#' them, running the prediction
#' for each part, then combining the predictions again.
#' 
#' @param model result from a model fit, as the `object` parameter from a traditional `predict` 
#' function.
#' @param newdata new data to which the model fit will predict 
#' @param n.parts number of parts your dataset should be split. If you have issues of memory,
#' trying increasing the number of parts.
#' @param remove.sub.memory logical. should the subsets from `newdata` be removed right after
#' being used?
#' @verbose logical. Should the script print what part is running?
#' ... additional arguments passed to the `predict` function.
#' 
#' @return prediction of the `model` for the `newdata`.
predict_large <- function(model, newdata, n.parts = 2, remove.sub.memory = T,
                          verbose = F, ...) {
  
  # total length of the new datset
  l <- nrow(newdata)
  # best way to divide it
  start <- floor(seq(1, l, by = l/n.parts))
  end <- c(start[-1]-1, l)
  
  # use foreach?
  pred <- list()
  for(i in 1:n.parts) {
    
    if(verbose) {
      print(paste0("Starting to predict part ", i, "/", n.parts, "."))
      print(paste0("Start: line ", start[i], "; end: line", end[i], "."))
    }
    sub_data <- newdata[start[i]:end[i],]
    sub_pred <- predict(model, sub_data, ...)
    pred[[i]] <- sub_pred
    
    if(remove.sub.memory == T) rm(sub_data, sub_pred)
  }
  
  unlist(pred)
}

  