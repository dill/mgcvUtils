#' Visualize NCV neighbourhoods
#'
#' Create a little plot showing what the neighbourhoods look like. The
#' resulting plot has number of rows equal to the number of folds and number of
#' columns equal to the number of data. Each tile is coloured according to
#' whether it's in the set of dropped indices, prediction points or both.
#'
#' @param nei a `list` of elements as required by `mgcv::gam`
#' @return `ggplot2` plot
#' @author David L Miller
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select "%>%" case_match
#' @importFrom ggplot2 aes ggplot geom_tile theme_minimal coord_cartesian labs scale_fill_manual
#' @importFrom rlang .data
#' @export
vis_nei <- function(nei){

# reminder from the nei documentation section of ?gam

# 'a' is the vector of indices to be dropped for each neighbourhood and 'ma'
# gives the end of each neighbourhood. So 'nei$a[(nei$ma[j-1]+1):nei$ma[j]]'
# gives the points dropped for the neighbourhood 'j'. 'd' is the vector of
# indices of points to predict, with corresponding endpoints 'md'. So
# 'nei$d[(nei$md[j-1]+1):nei$md[j]]' indexes the points to predict for
# neighbourhood j.

  # build a matrix to hold everything
  mm <- matrix(NA, nrow=length(nei$ma), ncol=max(c(nei$a, nei$d)))
  # we'll encode this with 1 if it's "in" and -1 if it's predicted
  # do 1
  this_out <- nei$a[1:nei$ma[1]]
  this_pred <- nei$d[1:nei$md[1]]
  mm[1, this_pred] <- 1
  mm[1, this_out] <- -1
  mm[1, intersect(this_pred, this_out)] <- 0

  # do the rest
  for(j in 2:length(nei$ma)){
    this_out <- nei$a[(nei$ma[j-1]+1):nei$ma[j]]
    this_pred <- nei$d[(nei$md[j-1]+1):nei$md[j]]
    mm[j, this_pred] <- 1
    mm[j, this_out] <- -1
    mm[j, intersect(this_pred, this_out)] <- 0
  }

  # transform for plot
  mm <- as.data.frame(mm) %>%
    mutate(fold = 1:nrow(mm)) %>%
    pivot_longer(cols=-.data$fold, names_to="datum") %>%
    mutate(datum = as.integer(sub("V", "", .data$datum))) %>%
    mutate(value2 = case_match(.data$value,
                              -1 ~ "Dropped",
                              0  ~ "Both",
                              1  ~ "Predicted")) %>%
    mutate(value = as.factor(.data$value2)) %>%
    select(-"value2")

  ggplot(mm) +
    geom_tile(aes(x=.data$datum, y=.data$fold, fill=.data$value), na.rm=TRUE) +
    scale_fill_manual(values=c("Dropped" = "#e41a1c",
                               "Both" = "#4daf4a",
                               "Predicted" = "#377eb8"),
                      na.value=NA) +
    coord_cartesian(expand=FALSE) +
    labs(y="Fold", x="Datum", fill="Status") +
    theme_minimal()
}
