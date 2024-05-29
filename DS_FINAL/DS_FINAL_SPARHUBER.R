library(tidyverse)

hfi2008_2016 <- read_csv("DS_FINAL/HFI/hfi2008_2016.csv")
# maybe religious freedom and labour market regulation?

str(hfi2008_2016)

hfi2008_2016 |> 
  group_by(countries) |> 
  summarise(across(everything(), ~ sum(is.na(.x)))) |> 
  rowwise() |> 
  mutate(total_missing = sum(c_across(where(is.numeric))),
         missing_perc = round(total_missing/(123*8)*100, 2)) |> 
  relocate(total_missing, .after = countries) |> 
  relocate(missing_perc, .after = total_missing) |> 
  View()

# missing at random is not a given in this dataset
