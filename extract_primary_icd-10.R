# Extract first ICD-10 diagnosis code in a comma-separated string of diags------

data2 <- data1 %>%
  mutate(
    # take everything before the first comma, remove the periods
    diag_isolate = gsub("\\.","",sub("\\,.*","",diagnosis_code)),    
    # look for the first instance that this string matches the diag code pattern
    diag1 = str_extract(diag_isolate, "([A-Z]{1}[0-9]{2,})")
    )