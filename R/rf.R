#' This is a wrap function of randomForest
#'
#' @param data a data frame containg grouping information
#' @param ntree the number of trees, default value is 500
#' @param ... other paras
#'
#' @return
#' @export
#'
#' @examples
#' data <- iris
#' colnames(data)[5] <- "group"
#' ggrf(data)
ggrf <- function(data, ntree = 500, ...){
  stopifnot("group" %in% colnames(data))
  if (!is.factor(data$group)) {
    data$group <- as.factor(data$group)
  }
  RF <- randomForest::randomForest(group ~ ., data = data,
                                   importance = TRUE,
                                   proximity = TRUE,
                                   ntree = ntree)
  return(RF)
}

#' Extract the importance information of randomForest result
#'
#' @param rf a randomForest result
#' @param type 1 or 2, default value is 2 for Gini index
#' @param simply rownames to col
#' @param sort order Gini index
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' data <- iris
#' colnames(data)[5] <- "group"
#' ggrf(data) |>  rf_importance()
rf_importance <- function(rf, type = 2, simply = TRUE, sort = TRUE){
  if (!inherits(rf, "randomForest"))
    stop(deparse(substitute(rf)), " must be a randomForest object")
  res <- round(randomForest::importance(rf, type = type), 2)  |> as.data.frame()
  if (simply) {
    res <- rownames_to_col(res)
    rownames(res) <- NULL
  }
  if (sort) {
    res <- res[order(res$MeanDecreaseGini, decreasing = TRUE), ]
  }
  return(res)
}

#' Plot the result of rf_importance function
#'
#' @param data the result of rf_importance function, a data frame
#'
#' @return a ggplot2 object
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' data <- iris
#' colnames(data)[5] <- "group"
#' ggrf(data) |>  rf_importance()  |> groutable::dt2()
#' ggrf(data) |>  rf_importance() |> rf_importance_plot()
rf_importance_plot <- function(data){
  data$sample <- factor(data$sample, levels = data$sample)
  ggplot(data = data, aes(x = sample, y = MeanDecreaseGini)) +
    geom_col(color = "white", size = 0.5, fill = "#3C5488") +
    ggtheme::theme_pub() +
    coord_flip() +
    xlab(NULL) + ylab("MeanDecreaseGini value")  +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_text(face = "bold"))
}

rownames_to_col <- function(data){
  data <- data.frame(sample = rownames(data), data)
}

