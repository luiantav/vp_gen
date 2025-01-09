### README

**Description**
This repo is associated with the preprint:  "Increased generalisation in trait anxiety is driven by value transfer, not reduced perceptual discrimination"
It contains data and scripts to reproduce the results in the main text & Supplementary Material. 

**Instructions**
The behavioural analyses scripts are organised in a single r-markdown notebook, found in the `analysis` folder. 
Modeling scipts and analyses of modeling outputs are organised in separate r-markdown notebooks that are named accordingly in the `modeling` folder. The computational environment can be reproduced using the renv package for R environments.
All data needed for behavioural analyses are provided in `data` folder. Raw model output files are collected in `outputs`. 

1. Clone git repo 
2. In RStudio, open analysis/manuscript_analyses.Rmd
3. Run. The script should load the renv.lock file and use it to download and install all necessary packages.

**Requirements**
* R version 4.2.1 (2022-06-23)
* RStudio (or other r-markdown editor)

Scripts tested on
- macOS Monterey 12.7.6; macOS Catalina 10.15.7
- Ubuntu 20.04

Expected runtime
<5 min for behavioural and modelling output analysis. Raw files of model fitting, parameter and model recovery are provided. 

**License**
Code: GNU GPLv3 (see LICENSE.md)

