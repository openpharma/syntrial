test_that("find categorical variables", {
  expect_equal(categorical_vars(CRC305ABC_DM),
               c("STUDYID", "DOMAIN", "USUBJID", "SUBJID", "SITEID", "AGEU", "SEX", "RACE", "ETHNIC", "ARMCD", "ARM", "COUNTRY")
               )
})

test_that('stop 3-d arrays in Hmax', {
  expect_error(Hmax(array(data = 1, dim=c(1:3))), 
               'Hmax can only be computed for a number, a matrix, or a dataframe')
})

test_that('Hmax 65536', {
  expect_equal(Hmax(65536), 16)
})

test_that('Hvars of dataframe', {
  expect_equal(Hvars(CRC305ABC_VS), log(nrow(CRC305ABC_VS), 2))
})
