sal_class <- function(x) {
  x %>%
    mutate(SalinityCode = case_when(SalinityCode == "high >18 ppt" ~ "High",
                                    SalinityCode == "medium 0.5-18 ppt" ~ "Medium",
                                    SalinityCode == "low <0.5 ppt" ~ "Low", 
                                    SalinityCode == "unknown" | SalinityCode == "no" ~ NA, 
                                    T ~ SalinityCode),
           SalinityTolerance = case_when(SalinityTolerance == "" ~ NA,
                                         SalinityTolerance == "None" ~ NA,
                                         T ~ SalinityTolerance)) %>%
    mutate(sal_code = if_else(is.na(SalinityTolerance) & !is.na(SalinityCode), SalinityCode, SalinityTolerance))
}