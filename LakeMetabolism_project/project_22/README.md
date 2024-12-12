Analysis of sondes data 2022

1. Description
This is a data-analysis project of the sondes data obtained from the
Stickleback-experiments of Blake's group in Greenland, 2022.
The aim of this project is to get insight on how Sticklebacks affect
lake metabolism.

2. Overview of folders
- code:
	- "make_data_2022": script to process and merge all the sondes
		time series of the ponds
	- "analysis_metabolism_...": metabolism analysis of the different ponds
	- "make_figure_metabolism_22": creating preliminary results figures for the metabolism analysis
	- "code.Rproj": the R project associated with this directory
	- "make_sonde_profiles_2022": making the cut and merged profile dataset of 2022
	- "metadata_prep_2022": preparing weather data for metabolism analysis 2022
	- "SensAnalysisZmix_ERL85": Code for sensitivity analysis for mixed layer depth of ERL85, 2022
- data:
	- "data_sondes_IntermProcessingSteps": intermediate processing steps of sonde data
	- "metadata_metabolism_analysis": metadata regarding wind and irradiation:
		- Source: https://dataverse.geus.dk/dataset.xhtml?persistentId=doi:10.22008/FK2/IW73UU. Dataset name: QAS_L_hour.csv
	- "processed_data_sondes": processed sonde data to be used for further analysis
	- "raw_data_sondes": raw sonde data
	- "sondes_profiling_cut": merged and cut dataset from all profiling done in Greenland in 2022
	- "sondes_profiling_raw": individual raw profiling datasets
	- "ponds_sonde_key": information on the sonde and treatment for the different ponds

- outliers_sondes: folder containing diagnostic graphs from outlier processing
- results_metabolism: All results from the metabolism analysis
- results_profiling: Results from plotting the cut and merged profile dataset of 2022
- results_SensAnalysis: Results from Sensitivity analysis with regards to mixed layer depth for some ponds

3. Executing the project:
	1. Install JAGS: https://sourceforge.net/projects/mcmc-jags/files/
		This is required to run the Bayesian models for lake metabolism.
	2. To execute the code, open the R-project file (.Rproj) under code/; Open the script
		you want to execute
	3. Install missing packages
	4. Run the code (no change of filepaths required since
		we work with an R projects and relative file paths; Rprojects anchor the
		working directory to where they are situated, in our case under code/;
		the file paths are relative to that Rproject)

4. Contact information:
	Don't hesitate to contact me if questions pop up.
	- Emanuel Mauch
	- Private: emanuelmauch@gmail.com
