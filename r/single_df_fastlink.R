#' Perform fastLink Matching with Posteriors with Single Data Set
#'
#' @description Function that "tricks" fastLink into performing matches
#'              within a single data set (within-set matching) as if it were
#'              performing between-set matching. This function returns
#'              posterior probabilities and does not permit deduplication.
#'              This is achieved by appending a blank row to the "second"
#'              data frame, tricking fastLink into believing the two are
#'              separate data frames.
#'              
#'   Dependencies: dplyr, fastLink
#'   
#'   Built: R (4.4.1); dplyr (1.1.4)
#'
#' @param df_A Dataframe for within-set fuzzy matching with fastLink
#' @param str_vars A list/vector of string variable names for string-distance 
#'                 matching. Default is Jaro-Winkler, per fastLink's
#'                 default
#' @param num_vars A list/vector of numeric variables names for numeric
#'                 distance processing.
#' @param ... Additional arguments for the wrapped fastLink::fastLink()
#'            function. Optional.
#'
#' @return fastLink object generated through fuzzy-matching within a single
#'         data set
#' @export
#'

single_df_fastLink <- function(df_A, str_vars, num_vars, ...){
  
  # if ( ... ) is not empty, use as additional arguments
  if (length(list(...)) > 0) {
    out <- fastLink::fastLink(
      dfA = df_A, 
      dfB = rbind(df_A,
                  as.data.frame(
                    matrix(
                      NA,
                      nrow=1,
                      ncol=ncol(df_A), 
                      dimnames = list(list(),colnames(df_A)))
                    )
                  ),
      varnames = c(str_vars,num_vars),
      stringdist.match = str_vars,
      numeric.match = num_vars,
      dedupe.matches = FALSE,
      ...
    )
  }
  
  # Otherwise, omit any possible additional parameters to avoid possibly 
  # raising errors
  if (!length(list(...)) > 0){
    out <- fastLink::fastLink(
      dfA = df_A, 
      dfB = rbind(df_A,
                  as.data.frame(
                    matrix(
                      NA,
                      nrow=1,
                      ncol=ncol(df_A), 
                      dimnames = list(list(),colnames(df_A)))
                  )
      ),
      varnames = c(str_vars,num_vars),
      stringdist.match = str_vars,
      numeric.match = num_vars,
      dedupe.matches = FALSE,
    )
  }
  
  # Return output
  out
  
}