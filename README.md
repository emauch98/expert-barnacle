This is the repository for a data analysis project from Blake's group regarding Stickleback experiments near Narsarsuaq, Greenland.
In 2019, 6 ponds previously fishless had been introduced with Sticklebacks, while 6 additional ponds served as fishless controls.
From 2021 onwards, the 12 ponds had been monitored with EXO2 sondes for several days every summer.
The goal of this project is to assess the effect of Sticklebacks on ecosystem metabolism.

The project is divided into 2021, 2022, 2023, and 2024. Each sub-project is roughly divided into code, data, and results.
Additionally, the Synthesis_21_22_23 folder contains overall results figures from 3 years of metabolism analysis.
The code is written in such a way that it should run without changing it (relative file paths, Rprojects),
given the required packages are installed. The easiest way to run the code is by opening the respective .Rproj file
within a code folder and subsequently open the desired R script within the RStudio interface.

The R package to estimate lake metabolism is called LakeMetabolizer, and a Bayesian model
was chosen for that purpose (see https://doi.org/10.1080/IW-6.4.883). Unique to the Bayesian model,
one must additionally install the Gibbs sampler JAGS before running any metabolism script (https://sourceforge.net/projects/mcmc-jags/files/).

Currently, the future of this project is uncertain, as we do not have the necessary data to estimate mixed layer depth,
which is an essential input for the metabolism model. As of now, we somewhat arbitrarily chose a constant mixed layer over time for each pond.



