# synthesizing clinical trials data

#' generate synthesis dataframe
#' 
#' \code{synth_df()} generates the tibble that relates original and synthetic new persons.
#' This synthesis dataframe should be kept secret to protect privacy of the original persons;
#' it is the base for all synthetic dataframes/tibbles that are generated.
#'
#' @param USUBJID character vector of person identifiers
#' @param n_new  number of new persons to generate, defaults to 10
#' @param width  number of persons from original trial to use for new person synthesis, defaults to 3
#' @param maxweight maximum allowed weight for one person for synthesis of new person, defaults to 2/3
#' @importFrom stats runif
#' @importFrom utils data
#' @importFrom tibble tibble
#'
#' @return The synthesis tibble relates new synthetic persons to source persons and 
#'   specifies the weight of each source person's contribution.
#' @export
#'
#' @examples synth_df(USUBJID = letters)
synth_df <- function(USUBJID, n_new=10, width=3, maxweight=2/3) {
  # determine sources for each synthetic person
  rand_srcs <- function() {
    vapply(seq.int(n_new), function(X) sample(USUBJID, size= width, replace=FALSE), character(width))
  }
  # per synthetic person and source specify weights such that each source at most has maxweight influence
  rand_weights <- function() {
    rw <- matrix(nrow = 0, ncol = width)
    while (nrow(rw) < n_new) {
      xx <- suppressWarnings(matrix(stats::runif(1.3*n_new*width), ncol = width))
      xx <- xx/rowSums(xx)
      rw <- rbind(rw, xx[apply(xx, 1, function(X) max(X) <= maxweight),])
    }
    rw <- rw[seq.int(n_new),]
    sel <- apply(rw, 1, function(X) seq.int(width)==sample.int(width, 1, prob = X))
    data.frame(weight=as.vector(t(rw)), selected=as.vector(sel))
  }
  
  tibble::tibble(synid=rep(seq.int(n_new), each=width),
             USUBJID=as.vector(rand_srcs()),
             rand_weights()
  )
}

#' domain identifier variables
#' 
#' Return identifier variables of a domain.
#'
#' @param df dataframe of one SDTM domain
#'
#' @return character vector of identifier variable names
#' @export
#'
#' @examples domain_ids(CRC305ABC_DM)
domain_ids <- function(df) {
  c('USUBJID', grep('NUM$|TESTCD$|TERM$', names(df), value = TRUE))
}

#' synthesize a SDTM dataframe
#' 
#' Create a synthetic dataframe from syndf and a source SDTM dataframe.
#'
#' @param syndf synthesis dataframe
#' @param df original SDTM dataframe 
#' @param cat_fuzz fuzz factor for noise on categorical variables, defaults to 1
#' @importFrom magrittr %>%
#' @importFrom stats weighted.mean
#' @importFrom tidyr nest unnest
#'
#' @return synthesized SDTM dataframe
#' @export
#'
#' @examples synthesize(synth_df(CRC305ABC_DM$USUBJID), CRC305ABC_DM)
synthesize <- function(syndf, df, cat_fuzz = 1) {
  n_sources <- nrow(syndf) / dplyr::n_distinct(syndf$synid)

  # remove standard sequence variables and SUBJID if existent
  df <- df[, !grepl(paste0('^',df$DOMAIN[1], 'SEQ','$|^SUBJID$'), names(df))]

  # join with syndf
  xx <- dplyr::inner_join(syndf,
                         dplyr::mutate_at(df, dplyr::vars(dplyr::ends_with('DTC')), lubridate::ymd_hm, truncated=4),
                         by = 'USUBJID'
                         ) %>% 
    dplyr::select(-USUBJID)
  # collect identifiers
  old_ids <- setdiff(domain_ids(df), 'USUBJID')
  new_ids <- c('synid', old_ids)
  # split variables into categorical and continuous
  nam <- setdiff(names(xx), names(syndf))
  cat_vars <- nam[vapply(xx[,nam], function(X) is.character(X)|is.factor(X)|is.logical(X), logical(1))|
                    endsWith(nam, 'CD')
                  ]
  num_vars <- nam[vapply(xx[,nam], function(X) is.numeric(X)|inherits(X, 'POSIXct'), logical(1)) &
                  !endsWith(nam, 'CD')
                  ]
  
  num_df <- xx %>%
    dplyr::select(c(new_ids, 'weight', num_vars)) %>% 
    dplyr::group_by_at(new_ids) %>% 
    dplyr::summarise_if(function(X) {is.numeric(X)|inherits(X, 'POSIXct')} ,
                 function(X) weighted.mean(X, prob=weight, na.rm = TRUE)) %>% 
    dplyr::select(-weight)
  
  
  cat_df <- if (cat_fuzz == 0) {
    xx %>% 
      dplyr::filter(selected) %>% 
      dplyr::select(new_ids, cat_vars)
  } else {
    xx %>% 
    dplyr::select(c(new_ids, 'weight', 'selected', cat_vars)) %>% 
    dplyr::mutate(cat_weights=cat_fuzz*weight + (1-cat_fuzz)*selected) %>% 
    dplyr::group_by_at(new_ids) %>% 
    tidyr::nest() %>% 
    dplyr::mutate(selx=lapply(data,
                              function(X) dplyr::bind_rows(
                                apply(X[,setdiff(cat_vars, new_ids)], 2,
                                      function(XX) sample(XX, size=1, prob=X$cat_weights)
                                )
                              )
                      )
    ) %>% 
    dplyr::select(-data) %>% 
    tidyr::unnest(cols = c(selx))
  }

    
  dplyr::right_join(num_df, cat_df, by=new_ids)
}

#' synthesize events dataframe
#'
#' @param syndf synthesis dataframe
#' @param df original SDTM dataframe 
#' @param cat_fuzz fuzz factor for noise on categorical variables, defaults to 1
#'
#' @return synthesized SDTM events dataframe
#'
#' @examples
#' \dontrun{synthesize_E(synth_df(CRC305ABC_DM$USUBJID), CRC305ABC_AE)}
synthesize_E <- function(syndf, df, cat_fuzz = 1) {
  # join with syndf
  xx <- dplyr::inner_join(syndf,
                          dplyr::mutate(df, dplyr::across(dplyr::ends_with('DTC'), lubridate::ymd_hm, truncated=4)),
                          by = 'USUBJID'
        ) %>% dplyr::select(-USUBJID)
  weight <- cat_fuzz*xx$weight + (1-cat_fuzz)*xx$selected
  ii <- runif(nrow(xx)) <= weight
  xx[ii,]
}
