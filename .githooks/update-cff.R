# Sync version and date-released in CITATION.cff with DESCRIPTION.
# Base R only; edits just those two lines so hand-written fields are preserved.
version <- read.dcf("DESCRIPTION", fields = "Version")[[1]]
lines <- readLines("CITATION.cff")
lines <- sub("^version:.*$", paste0("version: ", version), lines)
lines <- sub("^date-released:.*$", paste0("date-released: ", Sys.Date()), lines)
writeLines(lines, "CITATION.cff")
