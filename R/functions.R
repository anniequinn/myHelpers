quibble <- function(x, p = c(0, 0.25, 0.5, 0.75, 1.0), fns = FALSE) {

  output <- tibble("{{ x }}" := quantile(x, p),
                   "p{{ x }}" := p)

  if(p == c(0, 0.25, 0.5, 0.75, 1.0) && fns == TRUE) {

    output <-
      output %>%
      mutate(stat = c("min", "lower", "middle", "upper", "max"))

  }

  return(output)

}

filenameTimestamp <- function(prefix, extension, stamp = "both", sep = "_") {

  if(stamp != "both") { dv <- format(Sys.time(), "%Y%m%d") }
  if(stamp == "both") { dv <- format(Sys.time(), "%Y%m%d-%H%M%S") }

  paste0(prefix, sep, dv, extension)

}

rmCols <- function(dataframe) { dataframe %>% Filter(function(x) !all(is.na(x)), input) }

rmNullList <- function(list) { list %>% Filter(Negate(is.null), input) }

rmEmptyList <- function(list) { list %>% Filter(function(x) dim(x)[1] > 0, input) }

melt2 <- # Helper function to use pivot_longer in a way similar to reshape2::melt
  function(dataframe, measure.vars, variable.name = "variable", value.name = "value") {
    dataframe %>%
      pivot_longer(cols = all_of(measure.vars), names_to = variable.name, values_to = value.name)
  }
