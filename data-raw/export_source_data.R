# data-raw/export_source_data.R
# One-time script: exports PPH source data from hpa.classifications .rda files
# into Excel files in this data-raw/ directory.
#
# Run this script when hpa.classifications source data is updated.
# After running, commit the updated Excel files and re-run build_data.R.
#
# Requires hpa.classifications to be available at ../hpa.classifications

library(openxlsx)

hpa_data <- "/Users/jimpearse/Rprojects/hpa.classifications/data"

load(file.path(hpa_data, "PPH_pdiag.rda"))
load(file.path(hpa_data, "PPH_alldiag.rda"))
load(file.path(hpa_data, "PPHcodes.rda"))
load(file.path(hpa_data, "diag_add_COPD.rda"))
load(file.path(hpa_data, "diag_add_Bron.rda"))
load(file.path(hpa_data, "proc_exc_1.rda"))
load(file.path(hpa_data, "proc_exc_2.rda"))
load(file.path(hpa_data, "proc_exc_3.rda"))

# Principal diagnosis PPH lookup
wb <- createWorkbook()
addWorksheet(wb, "PPH_pdiag")
writeData(wb, "PPH_pdiag", PPH_pdiag)
saveWorkbook(wb, "data-raw/pph_principal_diagnosis.xlsx", overwrite = TRUE)

# Any-diagnosis PPH lookup
wb <- createWorkbook()
addWorksheet(wb, "PPH_alldiag")
writeData(wb, "PPH_alldiag", PPH_alldiag)
saveWorkbook(wb, "data-raw/pph_any_diagnosis.xlsx", overwrite = TRUE)

# PPH category labels
wb <- createWorkbook()
addWorksheet(wb, "PPHcodes")
writeData(wb, "PPHcodes", PPHcodes)
saveWorkbook(wb, "data-raw/pph_categories.xlsx", overwrite = TRUE)

# J20 additional diagnosis codes (COPD and Bronchiectasis)
wb <- createWorkbook()
addWorksheet(wb, "diag_add_COPD")
writeData(wb, "diag_add_COPD", data.frame(diag = diag_add_COPD))
addWorksheet(wb, "diag_add_Bron")
writeData(wb, "diag_add_Bron", data.frame(diag = diag_add_Bron))
saveWorkbook(wb, "data-raw/pph_j20_additional_diagnoses.xlsx", overwrite = TRUE)

# Procedure exclusion codes (cardiac, cellulitis x2)
wb <- createWorkbook()
addWorksheet(wb, "proc_exc_1")
writeData(wb, "proc_exc_1", data.frame(proc = proc_exc_1))
addWorksheet(wb, "proc_exc_2")
writeData(wb, "proc_exc_2", data.frame(proc = proc_exc_2))
addWorksheet(wb, "proc_exc_3")
writeData(wb, "proc_exc_3", data.frame(proc = as.character(proc_exc_3)))
saveWorkbook(wb, "data-raw/pph_procedure_exclusions.xlsx", overwrite = TRUE)

message("Source data exported to data-raw/")
