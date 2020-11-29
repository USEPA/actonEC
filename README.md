# actonEC
Citation for paper in review (9/2020):

Waldo, S., J.J. Beaulieu, W. Barnett, A. D. Balz, M.J. Vanni, T. Williamson, and J.T. Walker. Submitted. Temporal patterns and biophysical controls on methane emissions from a small eutrophic reservoir: insights from two years of eddy covariance monitoring. Submitted to: Biogeosciences. 

This repository contains all the material needed to reproduce the raw data processing, including the Artificial Neural Network (ANN) gap-filling, presented in Waldo et al (submitted). Scripts should be run in the order detailed in masterScript.R, after editing your "projectWD" path to match your project directory. See masterLibrary.R for the version of R and associated packages used for the manuscript. To reduce the size of this repository and make it easier to clone, the "BestANNsResampleNN.RData" files (where NN is a number from 01 to 20) are not saved in the repo, but are available on Zenodo.  

Be aware that the ANN calculations can take a long time to run. 

EPA Disclaimer: The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.
