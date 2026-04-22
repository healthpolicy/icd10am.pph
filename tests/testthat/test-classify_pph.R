dfbase <- icd10am.utils::dfbase
dfdiag <- icd10am.utils::dfdiag
dfproc <- icd10am.utils::dfproc
dfwide <- icd10am.utils::dfwide

test_that("classify_pph returns expected columns in long format", {
  result <- classify_pph(
    df         = dfbase,
    id         = recordID,
    format     = "long",
    df_diag    = dfdiag,
    df_proc    = dfproc,
    diag       = "diag",
    diagno     = "diagno",
    proc       = "proc",
    age_months = "agemonths",
    age_years  = "ageyears"
  )

  expect_equal(nrow(result), nrow(dfbase))
  expect_true("PPHnum"  %in% names(result))
  expect_true("PPH1"    %in% names(result))
  expect_true("PPHcat1" %in% names(result))
  expect_true(all(result$PPHnum >= 0))
  expect_true(any(result$PPHnum > 0))
})

test_that("classify_pph wide format produces same results as long format on shared data", {
  # Derive all long-format inputs from dfwide so both paths use identical data
  df_episodes <- dfwide[, c("recordID", "ageyears", "agemonths")]

  diag_cols <- grep("^diag[0-9]+$", names(dfwide), value = TRUE)
  df_diag_long <- tidyr::pivot_longer(
    dfwide[, c("recordID", diag_cols)],
    cols = dplyr::all_of(diag_cols),
    names_to  = "diagno",
    values_to = "diag"
  ) |>
    dplyr::mutate(diagno = as.integer(gsub("diag", "", diagno))) |>
    dplyr::filter(!is.na(diag), diag != "")

  proc_cols <- grep("^proc[0-9]+$", names(dfwide), value = TRUE)
  df_proc_long <- tidyr::pivot_longer(
    dfwide[, c("recordID", proc_cols)],
    cols = dplyr::all_of(proc_cols),
    names_to  = "procno",
    values_to = "proc"
  ) |>
    dplyr::filter(!is.na(proc), proc != "")

  result_long <- classify_pph(
    df         = df_episodes,
    id         = recordID,
    format     = "long",
    df_diag    = df_diag_long,
    df_proc    = df_proc_long,
    diag       = "diag",
    diagno     = "diagno",
    proc       = "proc",
    age_months = "agemonths",
    age_years  = "ageyears"
  )

  result_wide <- classify_pph(
    df          = dfwide,
    id          = recordID,
    format      = "wide",
    diag_prefix = "diag",
    proc_prefix = "proc",
    age_months  = "agemonths",
    age_years   = "ageyears"
  )

  expect_equal(nrow(result_long), nrow(result_wide))

  pph1_long <- result_long[order(result_long$recordID), "PPH1"][[1]]
  pph1_wide <- result_wide[order(result_wide$recordID), "PPH1"][[1]]
  expect_equal(pph1_long, pph1_wide)

  num_long <- result_long[order(result_long$recordID), "PPHnum"][[1]]
  num_wide <- result_wide[order(result_wide$recordID), "PPHnum"][[1]]
  expect_equal(num_long, num_wide)
})

test_that("classify_pph gives PPHnum = 0 for episodes with no matching diagnoses", {
  empty_diag <- data.frame(
    recordID = dfbase$recordID[1],
    diag     = "ZZZZZ",
    diagno   = 1L
  )
  empty_proc <- data.frame(
    recordID = dfbase$recordID[1],
    proc     = "0000000",
    procno   = 1L
  )

  result <- classify_pph(
    df         = dfbase[1, ],
    id         = recordID,
    format     = "long",
    df_diag    = empty_diag,
    df_proc    = empty_proc,
    diag       = "diag",
    diagno     = "diagno",
    proc       = "proc",
    age_months = "agemonths",
    age_years  = "ageyears"
  )

  expect_equal(result$PPHnum, 0L)
  expect_equal(result$PPH1,    "")
  expect_equal(result$PPHcat1, "")
})

test_that("classify_pph returns empty strings (not NA) for non-PPH episodes", {
  result <- classify_pph(
    df         = dfbase,
    id         = recordID,
    format     = "long",
    df_diag    = dfdiag,
    df_proc    = dfproc,
    diag       = "diag",
    diagno     = "diagno",
    proc       = "proc",
    age_months = "agemonths",
    age_years  = "ageyears"
  )

  expect_false(any(is.na(result$PPH1)))
  expect_false(any(is.na(result$PPHcat1)))
  expect_false(any(is.na(result$PPHnum)))
})
