# data-raw/build_data.R
# Build internal lookup data for icd10am.pph.
# Run this script interactively from the package root to regenerate R/sysdata.rda.
#
# Source files (update via export_source_data.R when upstream data changes):
#   pph_principal_diagnosis.xlsx      — principal diagnosis PPH codes (573 rows)
#   pph_any_diagnosis.xlsx            — any-diagnosis PPH codes (66 rows)
#   pph_categories.xlsx               — PPH category labels (22 rows)
#   pph_j20_additional_diagnoses.xlsx — J20 COPD/Bronchiectasis qualifying codes
#   pph_procedure_exclusions.xlsx     — cardiac and cellulitis procedure exclusions

library(readxl)

PPH_pdiag <- as.data.frame(
  readxl::read_excel("data-raw/pph_principal_diagnosis.xlsx",
                     sheet = "PPH_pdiag",
                     col_types = c("text", "text", "numeric", "text"))
)
PPH_pdiag$diagno <- as.integer(PPH_pdiag$diagno)

PPH_alldiag <- as.data.frame(
  readxl::read_excel("data-raw/pph_any_diagnosis.xlsx",
                     sheet = "PPH_alldiag",
                     col_types = c("text", "text", "text"))
)

PPHcodes <- as.data.frame(
  readxl::read_excel("data-raw/pph_categories.xlsx",
                     sheet = "PPHcodes",
                     col_types = c("text", "text"))
)

diag_add_COPD <- readxl::read_excel("data-raw/pph_j20_additional_diagnoses.xlsx",
                                     sheet = "diag_add_COPD",
                                     col_types = "text")$diag

diag_add_Bron <- readxl::read_excel("data-raw/pph_j20_additional_diagnoses.xlsx",
                                     sheet = "diag_add_Bron",
                                     col_types = "text")$diag

proc_exc_1 <- readxl::read_excel("data-raw/pph_procedure_exclusions.xlsx",
                                  sheet = "proc_exc_1",
                                  col_types = "text")$proc

proc_exc_2 <- readxl::read_excel("data-raw/pph_procedure_exclusions.xlsx",
                                  sheet = "proc_exc_2",
                                  col_types = "text")$proc

proc_exc_3 <- readxl::read_excel("data-raw/pph_procedure_exclusions.xlsx",
                                  sheet = "proc_exc_3",
                                  col_types = "text")$proc

usethis::use_data(
  PPH_pdiag, PPH_alldiag, PPHcodes,
  diag_add_COPD, diag_add_Bron,
  proc_exc_1, proc_exc_2, proc_exc_3,
  internal = TRUE, overwrite = TRUE
)
