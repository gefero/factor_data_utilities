#' Weigthed sample index
#' Extracts sampling indexes from data.frame -only one sample-
#' @param df a data frame, data frame extension -e.g. a tibble-, or a lazy data frame -e.g. from dbplyr or dtplyr-. See Methods, below, for more details.
#' @param wei a vector of probability weights for obtaining the elements of the vector being sampled.
#' @return a sampled -using weights- data frame, tibble, etc.
#' @examples 
#' df_sampled <- sample_wei(df=df, probs=df$wei);
#' @export
sample_wei <- function(df, wei){
        n <- nrow(df)
        index <- sample(1:n, prob=wei, replace=TRUE)
        df_boot <- df[index, ]
        return(df_boot)
}

#' Boostrap sampling -using weights-
#'
#' A bootstrap sample is a sample that is the same size as the original data set that is made using replacement. This results in analysis samples that have multiple replicates of some of the original rows of the data. The assessment set is defined as the rows of the original data that were not included in the bootstrap sample. This is often referred to as the "out-of-bag" -OOB- sample.
#' @param df a data frame.
#' @param wei a vector of probability weights for obtaining the elements of the vector being sampled.
#' @param reps  yhe number of bootstrap samples.
#' @return An tibble with classes bootstraps, rset, tbl_df, tbl, and data.frame. The results include a column for the data split objects and a column called id that has a character string with the resample identifier.
#' @examples 
#' df_boots <- bootstrap_wei(df=df, wei=df$wei, reps=5000);
#' @export

bootstrap_wei <- function(df,
                          wei, 
                          reps=1000){
        
        samples <- list()
        
        for (r in 1:reps){
                s <- sample_wei(df, wei)
                samples[[r]] <- s
        }
        
        samples <- enframe(samples, name='boot_id', value='splits')
        return(samples)
}