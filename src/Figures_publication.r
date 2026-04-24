### script figures publication
library("ggplot2")
library("ggpubr")
library("cowplot")

# Kniter the files
files <- c("markdown/examples_egfr/egfr_gp_discordance_bias.Rmd",
           "markdown/examples_friedewald/conventional_friedewald_gp_discordance_bias.Rmd",
           "markdown/examples_friedewald/friedewald_gp_discordance_bias.Rmd",
           "markdown/examples_CoLab/colab_ed_high_discordance_bias.Rmd",
           "markdown/examples_CoLab/colab_ed_high_confusion_matrices_discordance_bias.Rmd")
lapply(files, rmarkdown::render)




# EGFR CKDEPI ----------------------------------------------------------------------
# A: CONT RMSE, B: ORD PREC DISC, C: ORD RMSE, D: Dicht PREC DISC
CDKEPI_C_RMSE <- readRDS("out/egfr/simulations_discordance/egfr_discordance_gp_histogram_continuous_rmse.rds")
CDKEPI_O_PD <- readRDS("out/egfr/simulations_discordance/egfr_discordance_gp_histogram_ordinal_percentage_discordant.rds")
CDKEPI_O_micro_RMSE <- readRDS("out/egfr/simulations_discordance/egfr_discordance_gp_histogram_ordinal_micro_rmse.rds")
CDKEPI_D_PD <- readRDS("out/egfr/simulations_discordance/egfr_discordance_gp_histogram_categorical_percentage_discordant.rds")

EGFR_plot <- ggarrange(CDKEPI_C_RMSE  + 
                         xlab("Root mean squared error (continuous)") + 
                         ylim(0, 30) + 
                         xlim(0,10), 
                       CDKEPI_O_PD  + 
                         xlab("Percentage discordant (ordinal)") + 
                         ylim(0,50) + 
                         xlim(0,20) , 
                       CDKEPI_O_micro_RMSE  + 
                         xlab("Root mean squared error (ordinal)") +
                         ylim(0, 30) +
                         xlim(0,1.0),
                       CDKEPI_D_PD  + 
                         xlab("Percentage discordant (dichotomous)") + 
                         ylim(0,50) + 
                         xlim(0,20),
                       ncol = 2, nrow = 2,labels = c("A", "B", "C", "D"))

tiff("figures publication/EGFR_Figure.tiff",
     width = 176, 
     height = 176,
     units = "mm",
     res = 1200,
     compression = "lzw")
EGFR_plot
dev.off() 

# Friedewald ---------------------------------------------------------------------------------------
# A: CONT RMSE, B: ORD PREC DISC, C: ORD RMSE, D: Dicht PREC DISC
# conventional
Friedewald_C_RMSE <- readRDS("out/friedewald/simulations_discordance/conventional_friedewald_discordance_gp_histogram_continuous_rmse.rds")
Friedewald_O_PD <- readRDS("out/friedewald/simulations_discordance/conventional_friedewald_discordance_gp_histogram_ordinal_percentage_discordant.rds")
Friedewald_O_micro_RMSE <- readRDS("out/friedewald/simulations_discordance/conventional_friedewald_discordance_gp_histogram_ordinal_micro_rmse.rds")
Friedewald_D_PD <- readRDS("out/friedewald/simulations_discordance/conventional_friedewald_discordance_gp_histogram_categorical_percentage_discordant.rds")

Friedewald_conventional_plot <- ggarrange(Friedewald_C_RMSE  + 
                                            xlab("RMSE (continuous)") + 
                                            ylim(0, 50) + 
                                            xlim(0,15), 
                                          Friedewald_O_PD  + 
                                            xlab("Percentage discordant (ordinal)") +
                                            ylim(0, 30) + 
                                            xlim(0,20), 
                                          Friedewald_O_micro_RMSE  + 
                                            xlab("RMSE (ordinal)") +
                                            ylim(0, 50) + 
                                            xlim(0,1.5),
                                          Friedewald_D_PD  + 
                                            xlab("Percentage discordant (dichotomous)") +
                                            ylim(0, 30) + 
                                            xlim(0,20),
                                          ncol = 2, nrow = 2,labels = c("A", "B", "C", "D"))


tiff("figures publication/Friedewald_conventional_Figure.tiff",
     width = 176, 
     height = 176,
     units = "mm",
     res = 1200,
     compression = "lzw")
Friedewald_conventional_plot
dev.off() 


# SI
Friedewald_SI_C_RMSE <- readRDS("out/friedewald/simulations_discordance/friedewald_discordance_gp_histogram_continuous_rmse.rds")
Friedewald_SI_O_PD <- readRDS("out/friedewald/simulations_discordance/friedewald_discordance_gp_histogram_ordinal_percentage_discordant.rds")
Friedewald_SI_O_micro_RMSE <- readRDS("out/friedewald/simulations_discordance/friedewald_discordance_gp_histogram_ordinal_micro_rmse.rds")
Friedewald_SI_D_PD <- readRDS("out/friedewald/simulations_discordance/friedewald_discordance_gp_histogram_categorical_percentage_discordant.rds")


Friedewald_SI_plot <- ggarrange(Friedewald_SI_C_RMSE  + 
                                  xlab("RMSE (continuous)") + 
                                  ylim(0, 50) + 
                                  xlim(0,1) , 
                                Friedewald_SI_O_PD  + 
                                  xlab("Percentage discordant (ordinal)") +
                                  ylim(0, 30) + 
                                  xlim(0,20), 
                                Friedewald_SI_O_micro_RMSE  + 
                                  xlab("RMSE (ordinal)") +
                                  ylim(0, 50) + 
                                  xlim(0,1) ,
                                Friedewald_SI_D_PD  + 
                                  xlab("Percentage discordant (dichotomous)") +
                                  ylim(0, 30) + 
                                  xlim(0,20),
                                ncol = 2, nrow = 2,labels = c("A", "B", "C", "D"))

tiff("figures publication/Friedewald_SI_Figure.tiff",
     width = 176, 
     height = 176,
     units = "mm",
     res = 1200,
     compression = "lzw")
Friedewald_SI_plot
dev.off() 

# CoLab score ----------------------------------------------------------------------------------
# A: CONT RMSE, B: ORD PREC DISC, C: ORD RMSE, D: Dicht PREC DISC, E: SENS, F:SPEC
colab_C_RMSE <- readRDS("out/CoLab/simulations_discordance/colab_discordance_ed_high_histogram_continuous_rmse.rds")
colab_O_PD <- readRDS("out/CoLab/simulations_discordance/colab_discordance_ed_high_histogram_ordinal_percentage_discordant.rds")
colab_O_micro_RMSE <- readRDS("out/CoLab/simulations_discordance/colab_discordance_ed_high_histogram_ordinal_micro_rmse.rds")
colab_D_PD <- readRDS("out/CoLab/simulations_discordance/colab_discordance_ed_high_histogram_categorical_percentage_discordant.rds")
colab_sens <- readRDS("out/CoLab/simulations_confusion_matrix/colab_confusion_matrix_ed_high_histogram_sensitivity.rds")
colab_spec <- readRDS("out/CoLab/simulations_confusion_matrix/colab_confusion_matrix_ed_high_histogram_specificity.rds")


CoLab_plot <- ggarrange(colab_C_RMSE  + 
                          xlab("RMSE (continuous)") +
                          ylim(0,10) +
                          xlim(0,1), 
                        colab_O_PD  + 
                          xlab("Percentage discordant (ordinal)") +
                          ylim(0,10) + 
                          xlim(0,50), 
                        colab_O_micro_RMSE  + 
                          xlab("RMSE (ordinal)") +
                          ylim(0,10) +
                          xlim(0,1),
                        colab_D_PD  + 
                          xlab("Percentage discordant (dichotomous)") +
                          ylim(0,10) + 
                          xlim(0,5),
                        colab_sens + 
                          ggtitle("sensitivity") +
                          ylim(0,35) +
                          xlim(0,1),
                        colab_spec + 
                          ggtitle("specificity") +
                          ylim(0,35) +
                          xlim(0,1),
                        ncol = 2, nrow = 3,labels = c("A", "B", "C", "D", "E", "F"))


# cowplot alternative tot allign
p1 <- colab_C_RMSE  + 
  xlab("RMSE (continuous)") +
  ylim(0,8) +
  xlim(0,1) 
p2 <- colab_O_PD  + 
  xlab("Percentage discordant (ordinal)") +
  ylim(0,8) + 
  xlim(0,50)
p3 <- colab_O_micro_RMSE  + 
  xlab("RMSE (ordinal)") +
  ylim(0,8) +
  xlim(0,1)
p4 <- colab_D_PD  + 
  xlab("Percentage discordant (dichotomous)") +
  ylim(0,8) + 
  xlim(0,5)
p5 <- colab_sens + 
  xlab("sensitivity") +
  ylim(0,50) +
  xlim(0,1)
p6 <- colab_spec + 
  xlab("specificity") +
  ylim(0,50) +
  xlim(0,1)

aligned <- align_plots(p1, p2, p3, p4, p5, p6, 
                       align = "hv",   
                       axis = "tblr")  
CoLab_plot_cow <- plot_grid(plotlist = aligned, 
                            ncol = 2,
                            labels = c('A', 'B', 'C', 'D', 'E', 'F'))
CoLab_plot_cow

tiff("figures publication/Colab_Figure.tiff",
     width = 176, 
     height = 176,
     units = "mm",
     res = 1200,
     compression = "lzw")
print(CoLab_plot_cow)
dev.off() 





EGFR_plot
Friedewald_conventional_plot
Friedewald_SI_plot
CoLab_plot_cow
