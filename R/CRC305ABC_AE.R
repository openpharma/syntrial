#' adverse event data from clinical trials CRC305ABC
#' 
#' An anonymized dataset from 123 patients in SDTM format (\url{https://en.wikipedia.org/wiki/SDTM}).
#' 
#' @format A dataframe with 552 rows and 32 variables:
#'   \describe{
#'     \item{STUDYID}{Study Identifier}
#'     \item{DOMAIN}{Domain Abbreviation}
#'     \item{USUBJID}{Unique Subject Identifier}
#'     \item{AESEQ}{Sequence Number}
#'     \item{AESPID}{Sponsor-Defined Identifier}
#'     \item{AETERM}{Reported Term for the Adverse Event}
#'     \item{AEMODIFY}{Modified Reported Term}
#'     \item{AELLT}{Lowest Level Term}
#'     \item{AELLTCD}{Lowest Level Term Code}
#'     \item{AEDECOD}{Dictionary-Derived Term}
#'     \item{AEPTCD}{Preferred Term Code}
#'     \item{AEHLT}{High Level Term}
#'     \item{AEHLTCD}{High Level Term Code}
#'     \item{AEHLGT}{High Level Group Term}
#'     \item{AEHLGTCD}{High Level Group Term Code}
#'     \item{AECAT}{Category for Adverse Event}
#'     \item{AEPRESP}{Pre-Specified Adverse Event}
#'     \item{AESOC}{Primary System Organ Class}
#'     \item{AESOCCD}{Primary System Organ Class Code}
#'     \item{AELOC}{Location of Event}
#'     \item{AESEV}{Severity/Intensity}
#'     \item{AESER}{Serious Event}
#'     \item{AEACN}{Action Taken with Study Treatment}
#'     \item{AEREL}{Causality}
#'     \item{AEPATT}{Pattern of Adverse Event}
#'     \item{AEOUT}{Outcome of Adverse Event}
#'     \item{AESTDTC}{Start Date/Time of Adverse Event}
#'     \item{AEENDTC}{End Date/Time of Adverse Event}
#'     \item{AESTDY}{Study Day of Start of Adverse Event}
#'     \item{AEENDY}{Study Day of End of Adverse Event}
#'     \item{AEDUR}{Duration of Adverse Event}
#'     \item{AESIZE}{Measure of Adverse Event}
#'     }
#' 
#' @source \url{https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/QPHMKX}
#'
#' \preformatted{
#' CRC305ABC_AE <- readr::read_tsv('https://dataverse.harvard.edu/api/access/datafile/3462713?gbrecs=false',
#'                                 col_types = 'cccicccciciciciccciccccccccciici',
#'                                 locale = readr::locale(encoding = 'latin1')
#'                                )
#' }
'CRC305ABC_AE'
