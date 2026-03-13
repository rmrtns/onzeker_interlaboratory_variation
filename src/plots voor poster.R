
library(ggplot2)
library(patchwork)

# Discordance data plots
disc_dat <- readRDS('out/colab/simulations_discordance/colab_discordance_ed_histogram_dichotomous_percentage_discordant.rds')

ggplot(disc_dat$data, aes(x = dichotomous_percentage_discordant, 
                          fill = "color", color = "color")) + 
  geom_histogram(bins = 55) + 
  theme_classic() + xlim(0, 10) + 
  scale_fill_manual(values = c(rgb(153, 202, 234, maxColorValue = 255))) + 
  scale_color_manual(values = "white") + 
  xlab("Percentage discordante paren") + ylab("Aantal laboratoria") + 
  theme(legend.position = "none")
ggsave("disc_paren_poster.png", width = 4, height = 3)


# Sens, spec, data plots
sens_dat <- readRDS('out/colab/simulations_confusion_matrix/colab_confusion_matrix_ed_histogram_sensitivity.rds')
spec_dat <- readRDS('out/colab/simulations_confusion_matrix/colab_confusion_matrix_ed_histogram_specificity.rds')

p_sens <- ggplot(sens_dat$data, aes(x = round(sensitivity, 3), 
                                    fill = "color", color = "color")) + 
  geom_histogram(bins = 50) + 
  theme_classic() + xlim(0, 1) + 
  geom_vline(xintercept = 0.665, linetype = "dashed") + 
  scale_fill_manual(values = c(rgb(153, 202, 234, maxColorValue = 255))) + 
  scale_color_manual(values = "white") + 
  xlab("Sensitiviteit") + ylab("Aantal laboratoria") + 
  theme(legend.position = "none")

p_sens
p_spec <- ggplot(spec_dat$data, aes(x = round(specificity, 3), 
                                    fill = "color", color = "color")) + 
  geom_histogram(bins = 50) + 
  theme_classic() + xlim(0, 1) + 
  geom_vline(xintercept = 0.942, linetype = "dashed") + 
  scale_fill_manual(values = c(rgb(153, 202, 234, maxColorValue = 255))) + 
  scale_color_manual(values = "white") + 
  xlab("Specificiteit") + ylab("Aantal laboratoria") + 
  theme(legend.position = "none")

p_spec

p_sens + p_spec + plot_annotation(tag_levels = 'A')
ggsave("sens_spec_paren_poster.png", width = 4, height = 3)
