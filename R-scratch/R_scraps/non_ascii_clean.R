rsa_simulated <- read.csv("data-raw/rsa_simulated_subset.csv")
View(rsa_simulated)

unique(rsa_simulated$E63_Plan_Exhaust_TANF_Desc)

handle_non_ascii <- function(x) {
  cleaned_var <- stringi::stri_trans_general(x, "latin-ascii")
  cleaned_var
}

try <- handle_non_ascii(rsa_simulated$E63_Plan_Exhaust_TANF_Desc)
unique(try)

rsa_simulated$E63_Plan_Exhaust_TANF_Desc <- handle_non_ascii(
  rsa_simulated$E63_Plan_Exhaust_TANF_Desc)

unique(rsa_simulated$E63_Plan_Exhaust_TANF_Desc)

data.table::fwrite(rsa_simulated,
                   "data-raw/rsa_simulated_subset_fixed.csv")

rsa_simulated_fixed <- read.csv("data-raw/rsa_simulated_subset_fixed.csv")

unique(rsa_simulated_fixed$E63_Plan_Exhaust_TANF_Desc)
