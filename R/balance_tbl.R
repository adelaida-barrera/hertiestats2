
#' Create a balance table usually needed for the course Statistics II at the Hertie School 
#'
#' Creates a well-formatted table containing the mean values of a set of covariates for treatment and control groups. 
#' 
#'
#' @param data a dataframe containing only a binary treatment variable and a series of covariates to be included in the table 
#' @param treatment the treatment variable in the provided dataframe
#'
#' @return table
#' @export

balance_tbl <- function (data, treatment) {
  library(tidyverse)
  library(knitr)
  library(kableExtra)
  balance_tbl <- function(data, treatment){data %>%
      dplyr::group_by(treat) %>%
      dplyr::summarize_all(funs(mean(., na.rm = T))) %>% # summarize the mean of every variable
      t() %>% # transpose data
      as.data.frame() %>% # after transposing, convert from matrix to df
      add_rownames("variable") %>% # rownames to explicit column
      dplyr::rename(control = V1, treat = V2) %>% # rename columns that are created by as.data.frame
      dplyr::filter(variable != "treat" & variable != "data_id") 
  }


