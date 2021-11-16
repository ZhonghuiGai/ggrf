#' unsupervised randomForest
#'
#' @param data data frame
#' @param pc Which PC to present, one of 12, 13, 23, the default value is 12
#' @param level CI default value is 0.68
#'
#' @return a ggplot2 object
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' data <- iris
#' colnames(data)[5] <- "group"
#' ggrf_uns_plot(data)
ggrf_uns_plot <- function(data, pc = 12, level = 0.68){
  res <- ggrf_unsupervise(data)
  if (pc == 12) {
    p <- ggplot(data = res, aes(x = Dim.1, y = Dim.2, color = group)) +
      xlab("Dim 1") + ylab("Dim 2")
  }else if (pc == 13) {
    p <- ggplot(data = res, aes(x = Dim.1, y = Dim.3, color = group)) +
      xlab("Dim 1") + ylab("Dim 3")
  }else if (pc == 23) {
    p <- ggplot(data = res, aes(x = Dim.2, y = Dim.3, color = group)) +
      xlab("Dim 2") + ylab("Dim 3")
  }
  p <- p + geom_point(aes(color = group, shape = group), size = 1.5, alpha = 1) +
    stat_ellipse(level = level,  linetype = 3,
                 geom = "polygon", alpha = 0.02,
                 aes(fill = group), show.legend = FALSE)
  p <- p + theme(panel.grid = element_line(color = 'gray90', size = 0.1),
                 panel.background = element_rect(color = 'white',
                                                 fill = 'transparent', size = 1),
                 axis.text = element_text(size = 12, face = "bold", color = "black"),
                 axis.text.x = element_text(colour = "black", size = 12, face = "bold"),
                 axis.title = element_text(size = 12, face = "bold"),
                 legend.text = element_text(size = 10, face = "bold"),
                 legend.title = element_blank(),
                 legend.position = "right",
                 panel.border = element_rect(colour = "black", fill = "transparent"),
                 legend.background = element_rect(fill = "transparent"),
                 legend.key = element_rect(fill = "transparent"))
  return(p)
}

#' unsupervised randomForest
#'
#' @param data data frame
#'
#' @return a data frame
#'
#' @examples
#' data <- iris
#' colnames(data)[5] <- "group"
#' ggrf_unsupervise(data)
ggrf_unsupervise <- function (data) {
  stopifnot("group" %in% colnames(data))
  if (!is.factor(data$group)) {
    data$group <- as.factor(data$group)
  }
  rf <- randomForest::randomForest(data[, !(colnames(data) %in% "group")])
  rf.mds <- stats::cmdscale(1 - rf$proximity, eig = TRUE, k = 3)
  colnames(rf.mds$points) <- paste("Dim", 1:3)
  res <- data.frame(group = data[, "group"], rf.mds$points)
  class(res) <- c("data.frame", "rf_unsupervise")
  return(res)
}
