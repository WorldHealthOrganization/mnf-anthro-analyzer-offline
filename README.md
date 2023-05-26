# WHO Anthro Survey Analyser (Microsoft Windows Application)

Download Link: https://github.com/WorldHealthOrganization/mnf-anthro-analyzer-offline/archive/refs/tags/v1.0.3.zip

Online Version: https://worldhealthorg.shinyapps.io/anthro/

## About
The Anthro Survey Analyser is a tool developed by the Department of Nutrition and Food Safety of the World Health Organization (WHO),that allows the user to perform comprehensive analysis of anthropometric survey data for children under 5 years of age based on weight and height. The analyses is based on the WHO Child Growth Standards.[^1] This version of the tool provides results for four of the anthropometric indexes: 
- height-for-age
- weight-for-age
- weight-for-height
- body-mass-index-for-age.

This tool is designed to build country capacity on data analysis and reporting on child malnutrition outcomes. It aims to enhance good practice in collecting survey data, analysing and reporting results.
Users should read the manual (Quick Guide) before entering their data, as it contains a number of directions on data preparation for assuring analysis accuracy.

## What are the outputs of the Anthro Survey Analyser?
- **Z-score** file based on the WHO Child Growth Standards: individual data including calculated z-scores and corresponding flags according to the WHO flagging system for identifying implausible values;
- **Prevalence** file according to the WHO recommended standard analysis: includes prevalence estimates with corresponding standard errors and confidence intervals, and z-score summary statistics (mean and standard deviation) with all cut-offs describing the full index distribution (-3, -2, -1, +1, +2, +3). All results are provided at overall and disaggregated levels for all available stratification variables (age, sex, type of residence, geographical regions, wealth quintiles, mother education and one additional factor the user is interested in and for which data are available);
- **Summary survey report template** in Word format. It is a summary report template laying out guidance on minimum required details to follow good practice in reporting, according to existing guidance. The report also includes main findings (graphics and tables) depicting prevalence estimates and z-score distributions according to stratifications by different group variables for the five main indicators, namely stunting, wasting, severe wasting, overweight and underweight, as well as data quality assessment statistics and displays. Note, this is a summary report with key findings and data quality assessment; it can provide useful inputs to the survey full report.
- **Graphics and figures**: all graphical visualizations included in the application was developed in gray-scale colours to allow for black and white printing. They can be downloaded whenever they are displayed.

## Set-Up
Warning: you must have a browser installed on your device to run the offline application. The recommended browsersas provided by RStudio are Google Chrome, Mozilla Firefox, Safari, and Internet Explorer.

### Instructions for running offline Anthro Windows app
- Navigate to the zip folder containing the offline Anthro Windows app
- Unzip the folder to your desired location
- Open the unzipped folder containing the offline Anthro Windows app
- Run the `Start_Anthro.bat` file by double-clicking it
Congratulations! The offline Anthro Windows app should now be running on your default browser.
For any queries or comments, please contact the Anthro Tool team at **anthro2005@who.int**.

## Acknowledgments
The WHO Anthro Survey Analyser was built up from the WHO R macro developed by Elaine Borghi, from the Growth Monitoring and Assessment Unit, Department of Nutrition for Health and Development, WHO, Geneva. Monika Blössner and Elaine Borghi worked on the conceptualization, design and content of the application. The tool builds on the concepts from the Anthro Software, Nutrition Survey module. The first prototype of the online application based on R and Shiny R package was developed by the consultant firm Epidemos (Jonathan Polonsky as focal point), followed by important enhancements and data validation checks by the consultant Dirk Schumacher.
The development of the WHO Anthro Survey Analyser was fully funded by the Bill and Melinda Gates Foundation.

## References
[^1]: WHO Multicentre Growth Reference Study Group. WHO child growth standards. Length, height for-age, weight-for-age, weight-for-length and body mass index-for age. Methods and development. Geneva: World Health Organization; 2006. Available at http://www.who.int/childgrowth/standards/Technical_report.pdf (Accessed 07 December 2017).
