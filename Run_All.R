####### Define Function to run stata code  #######
run_stata <- function(dofile) {
  ## Set your stata path below
  stata_path <- "/Applications/Stata/StataMP.app/Contents/MacOS/StataMP" 
  dofile_path <- "Code/Stata/"
  system(paste0('"', stata_path, '" -b do "', 
                dofile_path,
                dofile, '"'))
  
  # move the log file
  logname <- sub(".do", ".log", dofile)
  logname <- sub("Analysis/", "", logname)
  logname <- sub("Analysis/", "", logname)
  file.rename(logname, file.path("Log/", logname))
}


####### Create Directory to Store Estimates + Figures #######
subdir = c("Log",
           "Estimate/SC_Main/Crime",
           "Estimate/SC_Main/ReplicationExtention",
           "Estimate/SC_Main/Robustness",
           "Estimate/Zoorob",
           "Estimate/Table",
           "Figures/Crime",
           "Figures/NFIL",
           "Figures/Descriptive",
           "Figures/Replication",
           "Figures/Robustness"
)

for (f in subdir) {
  if (!dir.exists(f)) {
    dir.create(f, recursive = TRUE)
  }
}


######### Cleaning ############

#### NVSS Data
restricteddata=F ## Set this equal to T if using restricted use

if (restricteddata ==T) {
  run_stata("Cleaning/read_NCHS_restricted.do")
  run_stata("Cleaning/aggregate_overdoses.do")
  source("Code/R/Cleaning/Final_Clean_NVSS.R")
} else{
  source("Code/R/Cleaning/Clean_Public_NVSS.R")
}

#### UCR Data
source("Code/R/Cleaning/Clean_UCR_Step1.R")
source("Code/R/Cleaning/Final_Clean_UCR.R")



######## Run Estimates -- Main #########
### Figure 1: SC Arrest
run_stata("Analysis/Run_Synth_Arrest_UCR.do")

### Figure 2, App Tables 1-2,4-5: Main SC Estimate
run_stata("Analysis/Run_Synth_ReplicationExtension.do")


### Table 1 & Appendix Table 9: Sensitivity to Fentanyl Control
run_stata("Analysis/Run_Synth_Fentanyl_Control.do")
run_stata("Analysis/Run_Synth_Fentanyl_Control_Part2.do")
source("Code/R/Analysis/Zoorob_Synth_Replication.R")




######## Run Estimates -- Appendix #########

### Appendix Figure 4: SC dropping one state
run_stata("Analysis/Run_Synth_RobustnessDropOneState.do")

### Appendix Figure 8: Simulating <9 Deaths
run_stata("Analysis/Run_Synth_Placebos_Imputation.do")

### Appendix Table 3: Adding Confidence Intervals
source("Code/R/Analysis/SCPI.R")


### Appendix Table 5: Robustness to other synthetic method
source("Code/R/Analysis/SynthDD.R")
source("Code/R/Analysis/DD.R")
source("Code/R/Analysis/AugSynth.R")
source("Code/R/Analysis/GSynth.R")
source("Code/R/Analysis/MultipleOutcome.R")



#### Appendix Table 6: Run SC w/o COVID-19
run_stata("Analysis/Run_Synth_ExcludeCOVID.do")

#### Appendix Table 7: Run SC for West Coast 
run_stata("Analysis/Run_Synth_RobustnessWestCoast.do")

### Appendix Table 8: SC Estimate NFIL
run_stata("Analysis/Run_Synth_NFIL_BiAnnual.do")



 
############ Create Tables & Figures ############

### Figure 1: SC Arrest
source("Code/R/CreateTablesFigures/SynthFinalize_Crime.R")

### Figure 2, App Tables 1-2,4-5: Main SC Estimate
source("Code/R/CreateTablesFigures/SC_ReplicationExtension_Finalize.R")

### Figures 3 & 4, Appendix Fig 5-7
source("Code/R/CreateTablesFigures/StrucBreak.R")

### Tables 2: Fentanyl Control
source("Code/R/CreateTablesFigures/Table_ZoorobReplication.R")

### Appendix Figures 1-2: Time Trend
source("Code/R/CreateTablesFigures/MortalityDefinitions.R")

### Appendix Figure 4 & 8:  Simulating <9 Deaths & SC dropping one state
source("Code/R/CreateTablesFigures/CreateRobustness_Figures.R")


### Appendix Tables 3: Adding Confidence Intervals
source("Code/R/CreateTablesFigures/Synth_Replication_TTest.R")

### Appendix Table 5: Robustness to other synthetic method
source("Code/R/CreateTablesFigures/SupplementalTables_OtherMethods.R")

#### Appendix Table 6: Run SC w/o COVID-19
source("Code/R/CreateTablesFigures/Create_COVIDRobustness.R")

#### Appendix Table 7: Run SC for West Coast 
source("Code/R/CreateTablesFigures/SynthFinalize_PlaceboWest.R")

### Appendix Table 8: SC Estimate NFIL
source("Code/R/CreateTablesFigures/Create_NFILS_Table.R")

