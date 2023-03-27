#' categorical variables
#'
#' Determine all categorical variables of a dataframe and return a vector of their names.
#'
#' @param df dataframe to be evaluated 
#'
#' @return logical
#' @export
#'
#' @examples categorical_vars(CRC305ABC_DM)
categorical_vars <- function(df) {
  nam <- names(df)
  nam[(vapply(df, function(X) is.character(X)|is.factor(X)|is.logical(X), logical(1)) 
       & 
      !endsWith(nam, 'DTC')
      ) |
    endsWith(nam, 'CD')
  ]
}

#' maximum entropy
#' 
#' Compute maximum shannon entropy \code{log(N, base=2)} of a number or of a dataframe or matrix. 
#' In the latter case the number of rows is used.
#'
#' @param N a number, a dataframe, or a matrix
#'
#' @return log(N, base=2)
#' @export
#'
#' @examples Hmax(CRC305ABC_DM)
Hmax <- function(N) {
  if (is.vector(N)) {
    if (length(N)>1) {
      N <- N[1]
      warning('syntrial::Hmax : Only first element of numeric vector will be used')
    }
  } else {
    if (is.matrix(N) || is.data.frame(N))
      N <- nrow(N) else
        stop('Hmax can only be computed for a number, a matrix, or a dataframe')
  }
  log(N, 2)
}

#' joint Shannon entropy
#'
#' @param .data dataframe within which entropy is computed
#' @param ... variables of the dataframe. Can be unquoted comma separated variables
#'   or a character vector of variable names \code{all_of(vars)}
#'
#' @return numeric value of joint Shannon entropy
#' @keywords internal
#' @examples
#' \dontrun{Hvars(CRC305ABC_DM, SEX, RACE)
#' 
#' vars <- c('SEX', 'RACE')
#' Hvars(CRC305ABC_DM, all_of(vars))}
Hvars <- function(.data, ...) {
  if (...length()==0) log(nrow(.data), 2) else philentropy::H(dplyr::count(dplyr::select(.data, ...), dplyr::across(), name='nn_xyz_')$nn_xyz_/nrow(.data))
}

#' Shannon entropy of subsets of dataframe variables
#' 
#' For all subsets of dataframe variables of a certain cardinality compute the 
#' Shannon entropy.
#'
#' @param .data dataframe
#' @param k integer indicating the cardinality of the subsets of \code{.data}'s variables
#'
#' @return Numeric vector of Shannon entropy of variable subset.
#'   Return value names are composed by pasting together variable names using "~" as separator.
#' 
#' @keywords internal
#' 
#' @examples
#' \dontrun{Hk(CRC305ABC_DM, 1)}
Hk <- function(.data, k) {
  varcombs <- utils::combn(x=names(.data), m=k)
  colnames(varcombs) <- apply(X=varcombs, MARGIN=2, FUN=paste, collapse='~')
  apply(X=varcombs, MARGIN=2, FUN=Hvars, .data=.data)
}

#' Shannon entropy structure details of a dataframe
#'
#' \code{Hstruc} computes the Shannon entropy of each dataframe's variable and
#' checks for \itemize{
#'  \item constant,
#'  \item record identifying, or
#'  \item 1:1 equivalent
#'  }
#' variables.  
#'  
#' @param .data dataframe
#'
#' @return A list
#' @export
#'
#' @examples
#' Hstruc(CRC305ABC_DM)
Hstruc <- function(.data) {
  Hmax0 <- Hmax(nrow(.data))-(eps <- sqrt(.Machine$double.eps))
  n <- ncol(.data)
  
  # simplify input dataframe
  H1 <- xx <- sort(Hk(.data, 1))
  constants <- names(xx)[xx==0]
  fullspecs <- names(xx)[xx>Hmax0]
  xx <- xx[setdiff(names(xx), c(constants, fullspecs))]
  yy <- seq_along(xx)[xx[-1]-xx[-length(xx)]<eps]
  xk <- xx[yy] == mapply(Hvars, 
                         xx_drop <- names(xx)[yy],
                         xx_keep <- names(xx)[yy+1],
                         MoreArgs=list(.data=.data)
                         )
  replaces <- structure(xx_keep[xk], names=xx_drop[xk])
  always <- base::intersect(names(.data),
                            c('USUBJID', 'VISITNUM', grep('TPTNUM$|TESTCD$', names(.data), value = TRUE)))
  
  .data[,base::setdiff(c(constants, fullspecs, names(replaces)), always)] <- NULL
  
  # build up list of identifying variables
  nams <- names(.data)
  start_ids <- c(always, grep('TPT$', nams, value = TRUE))
  
  if ((n_ids <- length(start_ids)) > 0) Hm <- start_ids else {
    n_ids <- 1
    Hm <- names(which.max(xx[nams]))
  }
  for (k in seq.int(n_ids+1, ncol(.data), by=1)) {
    nams <- setdiff(nams, Hm)
    xx <- sort(sapply(nams, Hvars, .data=.data, Hm))
    Hm[k] <- names(m <- which.max(xx))
    if (identical(all.equal(Hmax, unname(xx[m])), TRUE)) break
  }
  
  # reduce list of identifying variables
  for (i in base::setdiff(Hm, always)) {
    if (Hvars(.data, base::setdiff(Hm, i))>Hmax0) Hm <- base::setdiff(Hm, i)
  }
  
  return(list(max=unname(xx[m]),
              Hmax=Hmax0,
              ids=Hm,
              constants=constants,
              fullspecs=fullspecs,
              replaces=replaces,
              H1=H1
              ))
}
