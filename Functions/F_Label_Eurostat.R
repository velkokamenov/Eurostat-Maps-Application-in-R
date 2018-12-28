# Develop a function for the purposes of labeling geo locations as well as units of values

# N.B.!!! - the data frames D_Unit and D_Geo shall be made available outside of the function
# Please note that these are eurostat dictionaries

label_eurostat_my = function(EurostatDF,D_Unit,D_Geo){
  
  # Load libraries needed for the function to work
  library(dplyr)
  
  # Perform labeling using left joins and the dfs defined outside the function
  EurostatDF_Labeled = EurostatDF %>%
    left_join(D_Unit, by = "unit") %>%
    left_join(D_Geo, by = "geo") %>%
    select(-unit,-geo) %>%
    plyr::rename(c("unit_name" = "unit",
                   "geo_name" = "geo")) %>%
    select(unit,geo,time,values)
  
  return(EurostatDF_Labeled)
  
}
