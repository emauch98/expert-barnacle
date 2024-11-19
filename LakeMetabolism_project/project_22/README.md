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
	- "make_figure_metabolism_2022": creating the results figures for the metabolism analysis
	- "code.Rproj": the R project associated with this directory
	- "profile_plots_2022": plots of temperature profiles to estimate actively mixed layer depth needed for the metabolism analysis
	- "analysis_choro_rfu" is not relevant for this project
- data:
	- raw and processed data from the sondes, as well as intermetiate processing steps
	- metadata regarding wind and irradiation:
		- Source wind data: https://www.dmi.dk/lokationarkiv/show/GL/3421711/Narsarsuaq/#arkiv
		- Source irradiation data: https://dataverse.geus.dk/dataset.xhtml?persistentId=doi:10.22008/FK2/IW73UU
	- "sonde_profiling" contains vertical profiles of the pond
	- "ponds_sonde_key": information on the sonde and treatment for the different ponds
- outliers_sondes: folder containing diagnostic graphs from outlier processing
- results_metabolism: All results from the metabolism analysis

3. Executing the project:
	1. Install JAGS: https://sourceforge.net/projects/mcmc-jags/files/
		This is required to run the Bayesian model for lake metabolism.
	2. To execute the code, open the R-project under code/...; Open the script
		you want to execute and run the code (no change of filepaths required since
		we work with an Rproject and relative file paths; Rprojects anchor the
		working directory to where they are situated, in our case under code/;
		the file paths are relative to that Rproject)

4. Contact information:
	Don't hesitate to contact me if questions pop up.
	- Emanuel Mauch
	- Private: emanuelmauch@gmail.com


