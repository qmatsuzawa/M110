## Read Me

In this repository, you can find all the Stata code and R code to facilitate the replication. The master R file “Run_All.R” will run all the cleaning and estimation. All do-file and R scripts include a comment at the top describing what they do.

This repository has a few subdirectories:

  - **Code:** This is where all the R and Stata codes are stored 
  - **Data:** This is where all datasets (both raw and cleaned ones) are/should be stored
  - **Estimate:** This is where all temporary and final estimates are stored. The subfolders _SC_Main_ and _Robustness_ include temporary outputs. The subfolder Tables includes CSV files of all output tables.
  - **Figures:** This is where all figures are saved
  - **fentanyl_oregon_replication_dataverse:** This is where Zoorob et al. (2023)’s replication package is saved


## Important Note Regarding Raw Data

**Mortality Data:** In our paper, we use the restricted NVSS mortality data. If you do not have access to the restricted version, please download and save the public-use version. Please follow these steps:
  
  - Download Spencer (2023) data and save it into Data/Raw/ as Spencer_v2.txt: https://wonder.cdc.gov/controller/saved/D176/D373F989
  - Download Joshi et al. (2023) data and save it into Data/Raw/ as Jama_v2.txt: https://wonder.cdc.gov/controller/saved/D176/D373F990

**UCR Data:** We also use the publicly available UCR data from Kaplan (2024). Because of the file size, the raw data is not provided. However, you can access it here: [https://www.openicpsr.org/openicpsr/project/102263/version/V16/view?path=/openicpsr/102263/fcr:versions/V16/arrests_parquet_1974_2024_month.zip&type=file](https://www.openicpsr.org/openicpsr/project/102263/version/V16/view?path=/openicpsr/102263/fcr:versions/V16/arrests_parquet_1974_2024_month.zip&type=file). Please download and unzip 2018 to 2024 into the subfolder _Raw/UCR/arrests_parquet_1974_2024_month/_
