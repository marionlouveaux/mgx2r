usethis::use_build_ignore("devstuff_history.R")
usethis::use_pipe()
usethis::use_gpl3_license(name = "Marion Louveaux")
usethis::use_test("package")

usethis::use_package("colorRamps")
usethis::use_package("dplyr")
usethis::use_package("glue")
usethis::use_package("plotly")
usethis::use_package("purrr")
usethis::use_package("readr")
usethis::use_package("rgl")
usethis::use_package("tibble")
usethis::use_package("tidyr")
usethis::use_package("snow")


usethis::use_package("knitr", type = "Suggests")
usethis::use_package("plotly", type = "Suggests")
usethis::use_package("rmarkdown", type = "Suggests")
usethis::use_package("RColorBrewer", type = "Suggests")
# usethis::use_package("cellviz3d", type = "Suggests")

usethis::use_readme_rmd()
usethis::use_news_md()
usethis::use_code_of_conduct()

# Vignette ----
devtools::build_vignettes()

# GitHub ----
usethis::browse_github_pat()
usethis::edit_r_environ()
# Be sure to have a ssh key stored in Github or use 'protocol="https"'
# https://github.com/settings/keys
usethis::use_github()
usethis::use_github_links()

