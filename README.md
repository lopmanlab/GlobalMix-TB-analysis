# GlobalMix-TB-analysis
**Contributors** <br/>
*Kristin Nelson <sup>1</sup>, Machi Shiiba <sup>1</sup>, Rajan Srinivsasan <sup>2</sup>, Tyler Brown <sup>3</sup>, Leonardo Martinez <sup>4</sup>, Ben Lopman <sup>1</sup>*

*<sup>1</sup> Department of Epidemiology, Emory University Rollins School of Public Health, Atlanta, GA, USA* <br/>
*<sup>2</sup>* <br/>
*<sup>3</sup>* <br/>
*<sup>4</sup>* <br/>

*Correspondence to Kristin Nelson (knbratt@emory.edu)*

# Description of study
**Aim**  
To evaluate patterns and locations of contact relevant to Mtb transmission using data from GlobalMix study.

The protocol that contains the detailed data collection procedure in GlobalMix study is explained in Aguolu et al (2024).<sup>1</sup>

# Description of repository
This repository contains data, scripts, and codebook.
Folders are arranged as follows.
1. Guatemala
2. India
3. Mozambique
4. Pakistan
5. Other
6. Scripts
7. Codebook

Each country folder contains 'participants', 'contact diary', and 'casual contact' datasets, which contain information about the study participants, their reported close individual contacts, and their reported casual contacts respectively. The Other folder contains the clean datasets from outside of GlobalMix study used for the analysis.
The scripts folder has scripts used for the analysis of the "Characterizing social behavior relevant for tuberculosis transmission in four low- and middle-income countries". The Codebook folder contains codebooks explaining the variables in the datasets.

The participant, contact diary, and casual contact datasets are named as follows;
- country-code_participant_data_aim1.RDS
- country-code_contact_data_aim1.RDS
- country-code_locations_visited_data_aim1.RDS

Country codes are **gt** (Guatemala), **ind** (India), **moz** (Mozambique), **pak** (Pakistan).

The datasets from previous studies used for this analysis are as follows;
1. Oxford Covid-19 Government Response Tracker <sup>2</sup>

# System requirements
- The code is written in R version 4.4.1 using RStudio version 2023.06.1. <br/>
- The following packages are used to run the code: [dplyr, ggplot2, tidyr, scales, srvyr, survey, shadowtext, ggpubr, gridExtra, ggpattern, lubridate]. <br/>
- The code has been developed and tested on Windows 11. The code should be compatible with Windows and Mac operating systems. <br/>
- No non-standard hardware is required to run the code. 

# Installation guide
**Installing the latest version of R**
1. Go to the Comprehensive R Archive Network: https://cran.r-project.org/
2. Download the version for your operating system (e.g. Click Download R for Windows/macOS)
3. Follow the instructions provided

This should take about 3 minutes.

**Installing RStudio**
1. Go to the download RStudio website: https://posit.co/downloads/
2. Download RStudio for your operating system.
3. Follow the instructions provided

This should take about 2 minutes.

**Installing the R packages**
After installing R and RStudio, you can install packages using the following code.
```
install.packages(c("dplyr", "ggpattern", "ggplot2", "ggpubr", "gridExtra", "lubridate", "scales", "shadowtext,"srvyr", "survey", "tidyr"))
```
This should take about 5 minutes.

# Instructions for running the scripts
1. Load the packages in the "package_and_figures" file.
2. Run the code in the "(countrycode)_tb_main_analysis" file.
3. If needed, code for creating multipanel summary figures is in the "package_and_figures" file, which can be used after running all the country's main analysis code.
Each file should take about 20 seconds to run all the code. All the outputs corresponding to the Figure numbers and table numbers are available in main text and supplemental materials.

## Reference
1. Aguolu OG, Kiti MC, Nelson K, et al. Comprehensive profiling of social mixing patterns in resource poor countries: A mixed methods research protocol. PLOS ONE. 2024;19(6):e0301638. doi:10.1371/journal.pone.0301638
2. Hale T, Angrist N, Goldszmidt R, et al. A global panel database of pandemic policies (Oxford COVID-19 Government Response Tracker). Nat Hum Behav. 2021;5(4):529-538. doi:10.1038/s41562-021-01079-8
