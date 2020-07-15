#' demographic data from clinical trials CRC305ABC
#' 
#' An anonymized dataset of 123 patients in SDTM format (\url{https://en.wikipedia.org/wiki/SDTM}).
#' 
#' @format A dataframe with 123 rows and 19 variables:
#'   \describe{
#'     \item{STUDYID}{Study Identifier}
#'     \item{DOMAIN}{Domain Abbreviation}
#'     \item{USUBJID}{Unique Subject Identifier}
#'     \item{SUBJID}{Subject Identifier for the Study}
#'     \item{RFSTDTC}{Subject Reference Start Date/Time}
#'     \item{RFENDTC}{Subject Reference End Date/Time}
#'     \item{RFXSTDTC}{Date/Time of First Study Treatment}
#'     \item{RFXENDTC}{Date/Time of Last Study Treatment}
#'     \item{RFICTC}{Date/Time of Informed Consent}
#'     \item{RFPENDTC}{Date/Time of End of Participation}
#'     \item{SITEID}{Study Site Identifier}
#'     \item{AGE}{Age}
#'     \item{AGEU}{Age Units}
#'     \item{SEX}{Sex}
#'     \item{RACE}{Race}
#'     \item{ETHNIC}{Ethnicity}
#'     \item{ARMCD}{Planned Arm Code}
#'     \item{ARM}{Description of Planned Arm}
#'     \item{COUNTRY}{Country}
#'     }
#' 
#' @source \url{https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/QPHMKX}
#'
#' \preformatted{
#' CRC305ABC_DM <- readr::read_tsv('https://dataverse.harvard.edu/api/access/datafile/3462712?gbrecs=false',
#'                                  col_types = 'ccccccccccciccccccc',
#'                                  locale = readr::locale(encoding = 'latin1')
#'                                )
#' }
'CRC305ABC_DM'