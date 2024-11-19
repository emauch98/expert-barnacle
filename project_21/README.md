Analysis of sondes data 2021

1. Description
This is a data-analysis project of the sondes data obtained from the
Stickleback-experiments of Blake's group in Greenland, 2021.
The aim of this project is to get insight on how Sticklebacks affect
lake metabolism.

2. Overview of folders
- code:
	- "make_data_2021": script to process and merge all the sondes
		time series of the ponds
	- "analysis_metabolism_...": metabolism analysis of the different ponds
	- "make_figure_metabolism": creating the results figures for the metabolism analysis
	- "code.Rproj": the R project associated with this directory
	- "profile_plots_2021": plots of temperature profiles of the ponds
	- "analysis_Cloro_mgL"is not relevant for this project
- data:
	- raw and processed data from the sondes, as well as intermetiate processing steps
	- metadata regarding wind and irradiation:
		- Source wind data: https://www.dmi.dk/lokationarkiv/show/GL/3421711/Narsarsuaq/#arkiv
		- Source irradiation data: https://dataverse.geus.dk/dataset.xhtml?persistentId=doi:10.22008/FK2/IW73UU.
	- "sondes_profiling" raw data of depth profiles of the ponds
	- "sondes_key_2021": information on the sonde and treatment for the different ponds
- outliers_sondes: folder containing diagnostic graphs from outlier processing
- results_ChlorophyllAnalysis: Not relevant for the metabolism project
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


