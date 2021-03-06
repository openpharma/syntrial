#' lab data from clinical trials CRC305ABC
#' 
#' An anonymized dataset from 123 patients in SDTM format (\url{https://en.wikipedia.org/wiki/SDTM}).
#' 
#' @format A dataframe with 55660 rows and 31 variables:
#'   \describe{
#'     \item{STUDYID}{Study Identifier}
#'     \item{DOMAIN}{Domain Abbreviation}
#'     \item{USUBJID}{Unique Subject Identifier}
#'     \item{LBSEQ}{Sequence Number}
#'     \item{LBREFID}{Specimen ID}
#'     \item{LBTESTCD}{Lab Test or Examination Short Name}
#'     \item{LBTEST}{Lab Test or Examination Name}
#'     \item{LBCAT}{Category for Lab Test}
#'     \item{LBORRES}{Result or Finding in Original Units}
#'     \item{LBORRESU}{Original Units}
#'     \item{LBORNRLO}{Reference Range Lower Limit in Orig Unit}
#'     \item{LBORNRHI}{Reference Range Upper Limit in Orig Unit}
#'     \item{LBSTRESC}{Character Result/Finding in Std Format}
#'     \item{LBSTRESN}{Numeric Result/Finding in Standard Units}
#'     \item{LBSTRESU}{Standard Units}
#'     \item{LBSTNRLO}{Reference Range Lower Limit-Std Units}
#'     \item{LBSTNRHI}{Reference Range Upper Limit-Std Units}
#'     \item{LBNRIND}{Reference Range Indicator}
#'     \item{LBSTAT}{Completion Status}
#'     \item{LBREASND}{Reason Test Not Done}
#'     \item{LBNAM}{Vendor Name}
#'     \item{LBSPEC}{Speciment Type}
#'     \item{LBSPCCND}{Specimen Condition}
#'     \item{LBBLFL}{Baseline Flag}
#'     \item{LBFAST}{Fasting Status}
#'     \item{VISITNUM}{Visit Number}
#'     \item{VISIT}{Visit Name}
#'     \item{LBDTC}{Date/Time of Specimen Collection}
#'     \item{LBDY}{Study Day of Specimen Collection}
#'     \item{LBTPT}{Planned Time Point Name}
#'     \item{LBTPTNUM}{Planned Time Point Number}
#'     }
#' 
#' @source \url{https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/QPHMKX}
#'
#' \preformatted{
#' CRC305ABC_LB <- readr::read_tsv('https://dataverse.harvard.edu/api/access/datafile/3462715?gbrecs=false',
#'                                 col_types = 'cccicccccccccdcddcccccccciccici',
#'                                 locale = readr::locale(encoding = 'latin1')
#'                                 )
#' }
'CRC305ABC_LB'