#' Classify Potentially Preventable Hospitalisations (PPH)
#'
#' @description
#' Identifies Potentially Preventable Hospitalisations (PPH) in admitted
#' patient episodes coded with ICD-10-AM, using the Australian Institute of
#' Health and Welfare (AIHW) PPH classification.
#'
#' Both wide-format and long-format inputs are supported via the `format`
#' argument.
#'
#' @param df A data frame with one row per episode.
#' @param id The episode identifier column (unquoted name).
#' @param format Input format: `"long"` (default) or `"wide"`.
#' @param df_diag Long-format diagnosis data frame. Required when
#'   `format = "long"`.
#' @param df_proc Long-format procedure data frame. Required when
#'   `format = "long"`.
#' @param diag Name of the diagnosis code column in `df_diag` (quoted string).
#'   Default: `"diag"`. Used only when `format = "long"`.
#' @param diagno Name of the diagnosis sequence number column in `df_diag`
#'   (quoted string). Default: `"diagno"`. Used only when `format = "long"`.
#' @param proc Name of the procedure code column in `df_proc` (quoted string).
#'   Default: `"proc"`. Used only when `format = "long"`.
#' @param diag_prefix Prefix shared by all diagnosis columns in `df` (quoted
#'   string). Default: `"diag"`. Used only when `format = "wide"`.
#' @param proc_prefix Prefix shared by all procedure columns in `df` (quoted
#'   string). Default: `"proc"`. Used only when `format = "wide"`.
#' @param age_months Name of the age-in-months column in `df` (quoted string).
#'   Default: `"agemonths"`.
#' @param age_years Name of the age-in-years column in `df` (quoted string).
#'   Default: `"ageyears"`.
#'
#' @return The input `df` with additional columns: `PPHnum` (count of PPH
#'   categories identified), and paired `PPH1`/`PPHcat1`, `PPH2`/`PPHcat2`,
#'   ... columns for each identified PPH. Episodes with no PPH assigned have
#'   `PPHnum = 0` and `PPH1 = PPHcat1 = ""`.
#'
#' @references
#' Australian Institute of Health and Welfare (2021). *Potentially preventable
#' hospitalisations in Australia by small geographic areas 2017--18*. Cat. no.
#' HSE 236. Canberra: AIHW.
#'
#' National Healthcare Agreement: PI 18 -- Selected potentially preventable
#' hospitalisations (2018). Australian Institute of Health and Welfare.
#' <https://meteor.aihw.gov.au/content/index.phtml/itemId=658499>
#'
#' @export
#'
#' @importFrom dplyr all_of any_of arrange bind_rows case_when coalesce
#'   distinct filter group_by if_else left_join mutate n rename select
#'   summarise
#' @importFrom tidyr separate_wider_delim
#' @importFrom rlang .data abort as_name ensym sym
#' @importFrom stringr str_replace_na
#' @importFrom stats setNames
#' @importFrom icd10am.utils .to_long
#'
#' @examples
#' dfbase <- icd10am.utils::dfbase
#' dfdiag <- icd10am.utils::dfdiag
#' dfproc <- icd10am.utils::dfproc
#'
#' # Long format
#' classify_pph(
#'   df         = dfbase,
#'   id         = recordID,
#'   format     = "long",
#'   df_diag    = dfdiag,
#'   df_proc    = dfproc,
#'   diag       = "diag",
#'   diagno     = "diagno",
#'   proc       = "proc",
#'   age_months = "agemonths",
#'   age_years  = "ageyears"
#' )
#'
#' # Wide format
#' dfwide <- icd10am.utils::dfwide
#' classify_pph(
#'   df          = dfwide,
#'   id          = recordID,
#'   format      = "wide",
#'   diag_prefix = "diag",
#'   proc_prefix = "proc",
#'   age_months  = "agemonths",
#'   age_years   = "ageyears"
#' )
classify_pph <- function(
    df,
    id,
    format      = c("long", "wide"),
    df_diag     = NULL,
    df_proc     = NULL,
    diag        = "diag",
    diagno      = "diagno",
    proc        = "proc",
    diag_prefix = "diag",
    proc_prefix = "proc",
    age_months  = "agemonths",
    age_years   = "ageyears"
) {
  format <- match.arg(format)
  id_str <- rlang::as_name(rlang::ensym(id))

  # --- Normalise to long format ---
  if (format == "wide") {
    converted <- .to_long(df, id = !!rlang::sym(id_str),
                          diag_prefix = diag_prefix, proc_prefix = proc_prefix)
    df_diag <- converted$df_diag
    df_proc <- converted$df_proc
    diag   <- "diag"
    diagno <- "diagno"
    proc   <- "proc"
  }

  if (is.null(df_diag)) rlang::abort("`df_diag` must be supplied when `format = 'long'`.")
  if (is.null(df_proc)) rlang::abort("`df_proc` must be supplied when `format = 'long'`.")

  # --- Step 1: Standardise inputs and create age criteria ---
  df_age <- df |>
    dplyr::select(
      episode_id = dplyr::all_of(id_str),
      age_months = dplyr::all_of(age_months),
      age_years  = dplyr::all_of(age_years)
    ) |>
    dplyr::mutate(
      GT3years = .data$age_years > 3,
      GT1month = dplyr::case_when(
        .data$age_years  > 0 ~ TRUE,
        .data$age_months > 1 ~ TRUE,
        TRUE                 ~ FALSE
      )
    )

  df_diag_std <- df_diag |>
    dplyr::rename(
      episode_id = dplyr::all_of(id_str),
      diag       = dplyr::all_of(diag),
      diagno     = dplyr::all_of(diagno)
    )

  df_proc_std <- df_proc |>
    dplyr::rename(
      episode_id = dplyr::all_of(id_str),
      proc       = dplyr::all_of(proc)
    )

  # --- Step 2: Principal-diagnosis PPH match ---
  df_pdiag <- df_diag_std |>
    dplyr::filter(.data$diagno == 1) |>
    dplyr::left_join(PPH_pdiag, by = c("diagno", "diag")) |>
    dplyr::filter(!is.na(.data$PPH))

  # --- Step 3: J20 disambiguation (COPD vs Bronchiectasis) ---
  # J20 (acute bronchitis) is only a PPH if accompanied by a COPD or
  # Bronchiectasis additional diagnosis; otherwise excluded.
  df_j20_addl <- df_diag_std |>
    dplyr::filter(.data$diagno != 1) |>
    dplyr::mutate(
      J20_COPD_add = .data$diag %in% diag_add_COPD,
      J20_Bron_add = .data$diag %in% diag_add_Bron
    ) |>
    dplyr::filter(.data$J20_COPD_add | .data$J20_Bron_add) |>
    dplyr::group_by(.data$episode_id) |>
    dplyr::summarise(
      J20_COPD_add = any(.data$J20_COPD_add),
      J20_Bron_add = any(.data$J20_Bron_add),
      .groups = "drop"
    )

  df_pdiag2 <- df_pdiag |>
    dplyr::filter(.data$diag_PPH == "J20") |>
    dplyr::left_join(df_j20_addl, by = "episode_id") |>
    dplyr::mutate(
      PPH = dplyr::case_when(
        .data$J20_COPD_add ~ "COPD",
        .data$J20_Bron_add ~ "Bronchiectasis",
        TRUE               ~ "Exclude"
      )
    ) |>
    dplyr::filter(.data$PPH != "Exclude") |>
    dplyr::bind_rows(
      df_pdiag |> dplyr::filter(.data$diag_PPH != "J20")
    )

  # --- Step 4: Any-diagnosis PPH match ---
  df_alldiag <- df_diag_std |>
    dplyr::left_join(PPH_alldiag, by = "diag") |>
    dplyr::filter(!is.na(.data$PPH)) |>
    dplyr::bind_rows(df_pdiag2)

  # --- Step 5: Age exclusions ---
  df_aged <- df_alldiag |>
    dplyr::left_join(df_age, by = "episode_id") |>
    dplyr::mutate(
      PPH = dplyr::case_when(
        .data$PPH == "Pneumonia and influenza (vaccine-preventable)" & !.data$GT1month ~ "Exclude",
        .data$PPH == "Pneumonia (not vaccine-preventable)"           & !.data$GT1month ~ "Exclude",
        .data$PPH == "Asthma"                                        & !.data$GT3years ~ "Exclude",
        TRUE ~ .data$PPH
      )
    ) |>
    dplyr::filter(.data$PPH != "Exclude")

  # --- Step 6: Cardiac procedure exclusions ---
  # Exclude selected cardiac procedures for CCF, Angina, Hypertension.
  cardiac_exc <- df_proc_std |>
    dplyr::filter(.data$proc %in% proc_exc_1) |>
    dplyr::distinct(.data$episode_id) |>
    dplyr::mutate(proc_exc = TRUE)

  df_cardiac <- df_aged |>
    dplyr::filter(.data$PPH %in% c("Congestive cardiac failure", "Angina", "Hypertension")) |>
    dplyr::left_join(cardiac_exc, by = "episode_id") |>
    dplyr::mutate(proc_exc = !is.na(.data$proc_exc)) |>
    dplyr::filter(!.data$proc_exc) |>
    dplyr::select(-"proc_exc") |>
    dplyr::bind_rows(
      df_aged |> dplyr::filter(!.data$PPH %in% c("Congestive cardiac failure", "Angina", "Hypertension"))
    )

  # --- Step 7: Cellulitis procedure exclusions (two sub-rules) ---
  cellulitis_exc2 <- df_proc_std |>
    dplyr::filter(.data$proc %in% proc_exc_2) |>
    dplyr::distinct(.data$episode_id) |>
    dplyr::mutate(proc_exc = TRUE)

  cellulitis_exc3 <- df_proc_std |>
    dplyr::filter(.data$proc %in% proc_exc_3) |>
    dplyr::group_by(.data$episode_id) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(.data$n == 1) |>
    dplyr::distinct(.data$episode_id) |>
    dplyr::mutate(proc_exc = TRUE)

  df_final_diag <- df_cardiac |>
    dplyr::filter(.data$PPH == "Cellulitis") |>
    dplyr::left_join(cellulitis_exc2, by = "episode_id") |>
    dplyr::mutate(proc_exc = !is.na(.data$proc_exc)) |>
    dplyr::filter(!.data$proc_exc) |>
    dplyr::select(-"proc_exc") |>
    dplyr::left_join(cellulitis_exc3, by = "episode_id") |>
    dplyr::mutate(proc_exc = !is.na(.data$proc_exc)) |>
    dplyr::filter(!.data$proc_exc) |>
    dplyr::select(-"proc_exc") |>
    dplyr::bind_rows(
      df_cardiac |> dplyr::filter(.data$PPH != "Cellulitis")
    )

  # --- Steps 8-9: Category lookup and aggregation ---
  df_summary <- df_final_diag |>
    dplyr::select("episode_id", "PPH") |>
    dplyr::distinct() |>
    dplyr::left_join(PPHcodes, by = "PPH") |>
    dplyr::arrange(.data$episode_id, .data$PPHcat, .data$PPH) |>
    dplyr::group_by(.data$episode_id) |>
    dplyr::summarise(
      PPH    = paste0(.data$PPH,    collapse = ";"),
      PPHcat = paste0(.data$PPHcat, collapse = ";"),
      PPHnum = dplyr::n(),
      .groups = "drop"
    )

  df_out <- df |>
    dplyr::left_join(df_summary, by = stats::setNames("episode_id", id_str)) |>
    dplyr::mutate(
      PPH    = stringr::str_replace_na(.data$PPH,    ""),
      PPHcat = stringr::str_replace_na(.data$PPHcat, ""),
      PPHnum = dplyr::coalesce(.data$PPHnum, 0L)
    )

  # --- Step 10: Split concatenated strings into numbered columns ---
  max_pph <- max(df_out$PPHnum)

  if (max_pph == 0L) {
    return(
      df_out |>
        dplyr::mutate(PPH1 = "", PPHcat1 = "") |>
        dplyr::select(-"PPH", -"PPHcat")
    )
  }

  into_pph <- paste0("PPH",    seq_len(max_pph))
  into_cat <- paste0("PPHcat", seq_len(max_pph))

  df_out |>
    tidyr::separate_wider_delim(
      "PPH",    delim = ";", names = into_pph, too_few = "align_start"
    ) |>
    tidyr::separate_wider_delim(
      "PPHcat", delim = ";", names = into_cat, too_few = "align_start"
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(c(into_pph, into_cat)), ~ stringr::str_replace_na(.x, ""))
    )
}
