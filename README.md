
# Interlaboratory variation in prediction model performance

This repository contains the code for a simulation method to evaluate the impact of bias in laboratory tests (or other continuous variables) on the clinical performance of prediction models.

The code accompanies the journal article:
[to be filled in]

which has been published in: [to be filled in].

## Repository structure:
The structure of this repository resembles that of the RStudio Project that was used to perform the simulations described in the article. The data used in the article are only available upon reasonable request. However, using the same project structure, the code may be used to evaluate alternative models or alternative datasets or both. 

data/<br>
This folder should hold the data files used in the simulations (i.e., reference population and estimates of bias in individual laboratory tests). 

markdown/<br>
The main folder contains the R Markdown templates ('template_discordance.Rmd', 'template_confusion_matrices.Rmd', 'template_crossover.Rmd') that are used to call the simulation functions and report their results. These files serve as templates to conduct the experiments and should, therefore, not be modified.<br>
The subfolders contain exemplary and modifiable parameterized R Markdown files to perform similar analyses as described in the article. The reports based on these R Markdown files will be saved in the subfolders as well.<br>
All relative paths defined in the R Markdown files are relative to the project directory.

out/<br>
The output of the simulations will be saved in this folder.<br>
Each prediction model has its own subfolder.<br>
The structure of its subfolders facilitates the interaction between the R Markdown files.<br>

renv/<br>
This folder contains files that are required to recreate the project library that was used for the analyses described in the article.

src/<br>
This folder contains all the source code from the project.

## Installation:
- Clone or download the entire repository.
- Recreate the R environment using renv (https://rstudio.github.io/renv/articles/collaborating.html): Briefly, use renv::restore() to restore the project library, after renv has been installed automatically.

## How to use:
The next section provides a detailed guide to use the simulation code.

- Save the data file containing the reference population that is used in the simulations.<br>
The data should be saved as a .csv file in the 'data/' folder.<br>
It is recommended that the file with the reference population only contains the variables used by the prediction model that is evaluated. In addition, a patient identifier or order identifier is required for identification of rows in the data file. If the impact on clinical performance characteristics is assessed as well, the data file should also contain the reference outcome.<br>
Please note that some matrix-based prediction models require the column order in the dataset to be in the exact same order as in the dataset that was used for model training. For this purpose, variables may be sorted in the raw dataset or the prediction model function.

- Define which variables in the reference population data are subject to bias.<br>
Sensitivity to bias can be defined as boolean data in a 'variables.R' file in the 'src/' folder. Examples are available in the 'src/' folder ('variables_colab.R').<br>

- Define the prediction model that is to be evaluated.<br>
The prediction model should be saved in a .R file in the 'src/' folder. <br>
The simulation method accepts additional function arguments, if required by the prediction model, as additional parameters in the R Markdown files. All additional function arguments are listed in the 'dots_arguments' variable and can be extracted by the prediction model function using list subsetting. Examples are available in the 'src/' folder ('predict_colab.R'). 

- Select and save the bias data.<br>
The bias data should be save as a .csv file in the 'data/'folder.<br>
Sources of  bias data may be external quality assessment (EQA) data or sample exchange between laboratories.<br>
The data file should contain columns for analyte, laboratory, cluster, intercept and slope. It is not necessary to exclude analytes that are not part of the prediction formula. A laboratory may participate with multiple analyzers in an EQA program. These analyzers are indicated in the column clusters. The simulation method selects the first cluster for analysis.<br>
Code to generate bias from excel files from the EQA provider SKML is present in the 'src/' folder.<br> 
Target values for the calculation of bias are assigned hierarchically by the laboratory that produced the results of the reference population, the measurement procedure applied in the reference population, reference methods, expert laboratories or consensus method group averages.<br>
If the laboratory that produced the results of the reference population participates in an EQA round, results may be recalculated relative to this laboratory in the simulation program.<br>

- Estimate the bias-induced discordance measures.<br>
Follow the approach outlined in 'colab_ed_high_discordance_bias.Rmd' in the 'markdown/examples_colab/' subfolder.<br>
The simulation parameters may modified as indicated.<br>
The variable names in the reference population data should be matched to the variable names in the bias data.<br>
Bootstrap intervals may be calculated for the results of individual laboratories.<br>

- Estimate the impact on clinical performance characteristics (optional).<br>
Follow the approach outlined in 'colab_ed_high_confusion_matrices_discordance_bias.Rmd' in the 'markdown/examples_colab/' subfolder.<br>

- Estimate the impact on reclassification within the confusion matrix (optional).<br>
Follow the approach outlined in 'colab_ed_high_crossover_discordance_bias.Rmd' in the 'markdown/examples_colab/' subfolder.<br>

## Disclaimer:
The code in this repository is intended for research use only.<br>
The simulation method was developed in R (version 4.5.2) with RStudio (version 2025.05.0) in an Ubuntu 24.04 development environment using WSL on a 64-bit Windows 11 machine. Running the code under other operating systems, or versions of the software and packages used, may require code modifications or installation of additional system dependencies.<br>
Computational time may vary depending on the complexity of the model tested, applied simulation parameters and computational resources.
