library(tidyverse)

tab_unstable <- read_tsv("AllBlocks_resultsnew.txt")
tab_stable <- read_tsv("AllBlocks_resultsbayes.txt")

merckValues_unstable <- tab_unstable %>%
	select(cell_line,drugA,drugB,kappa,IAE)

merckValues_stable <- tab_stable %>%
	select(cell_line,drugA,drugB,kappa,IAE)


usethis::use_data(merckValues_unstable, overwrite = TRUE)
usethis::use_data(merckValues_stable, overwrite = TRUE)
