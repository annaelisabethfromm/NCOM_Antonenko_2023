# NCOM_Antonenko_2023
## Microstructural and functional plasticity following repeated brain stimulation during cognitive training in older adults
Author List:
Daria Antonenko, Anna Elisabeth Fromm, Friederike Thams, Ulrike Grittner, Marcus Meinzer, Agnes Flöel

### Data and Code Availability 

The data of this study are available upon reasonable request from the corresponding author. The data are not publicly available due to potential identifying information that could compromise participant privacy. Source data are provided with the respository (SourceData.xlsx). 

All analyses were performed using the available toolboxes: R version 4.1.2 (http://www.rproject.org/), MATLAB R2019a (https://www.mathworks.com), CONN 19.b (https://web.conn-toolbox.org), FreeSurfer Version 6.0.0 for Segmentation (Version 7.2.0. for TRACULA; http://freesurfer.net) and FSL 6.0.0 (https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/). Costumized codes are available within the respository. 


### Statistical Analyses and Source Data 
For transparency and in order to reconstruct/ replicate our results, we upload the R script (Analysis.R) to analyse the data we used within our manuscript (SourceData.xlsx).
The data file contains different named excel sheets. The first sheet contains all data we used for statistical analyses und for visualization. In addition, we created separate files for each figure/ table which only contains necessary information for that representation. 

To run the R script, you need to: 
- install R (http://www.rproject.org/)
- install the required packages to run the analysis (can be found in the first lines of the R script)
- dowload the data and set the working directory to your own directory 

### MRI Analyses 
1 - System requirements for each analysis are mentioned in the respective word file for Freesurfer, FSL, and CONN toolbox. No non-standard hardware is required. 

2 - Installation instructions are available on the respective homepages: 
FSL: https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/FslInstallation
Freesurfer: https://surfer.nmr.mgh.harvard.edu/fswiki/ReleaseNotes 
CONN: https://web.conn-toolbox.org

3 - We provide the bash codes and a short introduction for  
- grey matter analyses (GreyMatter)
- white matter analyses (WhiteMatter)
- resting state analyses (RestingState)
