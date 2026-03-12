library(dplyr)
library(mcr)

skml_bias <- read.csv('data/skml_bias_table.csv')

# Get columns with p-values of linearity test
lin_test_cols <- grep("Lin_test", names(skml_bias), value = T)

# Adjust according to fdr method
p_vals_adj <- lapply(skml_bias[, lin_test_cols], p.adjust, method = "fdr")
  
# Count number of significant test results
n_signif <- sapply(p_vals_adj, function(x) sum(x < 0.05, na.rm = T))
signif_tests <- n_signif[n_signif > 0]
signif_tests <- signif_tests[order(signif_tests, decreasing = T)]
names(signif_tests) <- gsub("Lin_test_p_", "", names(signif_tests))
data.frame(H0_verworpen = signif_tests)
# LD is main test that doesnt adhere to linearity
skml_raw <- read.csv("data/skml_merged.csv")

# Example 
non_lin_LDs <- subset(skml_bias, Lin_test_p_LD < 0.01)
head(non_lin_LDs[, c('ptp', 'ctr')])
LD_example <- skml_raw %>% filter(ptp == 9 & ctr == 6 & Bepaling == "LD")
PB.reg <- mcr::mcreg(LD_example$ConsensusWaarde, LD_example$Resultaat,
                     method.reg = "PBequi", method.ci = "analytical")
png(file="data/LD_9_6.png", units = "in", width = 6, height= 5, 
    res = 300)
plot(PB.reg)
dev.off()
