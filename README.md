# UKMOD_and_mortality
Code for working with [UKMOD](https://www.microsimulation.ac.uk/ukmod/) output files, including how to estimate population-wide mortality impacts of income changes. 

# Background
This code provides an example of how output files from the tax-benefit microsimulation model [UKMOD](https://www.microsimulation.ac.uk/ukmod/) can be processed and analysed in R. This approach has been used to estimate the mortality impacts of various changes to household incomes in Scotland. See [Richardson et al. 2025](https://academic.oup.com/eurpub/article/35/2/242/8009070?login=true) and [Richardson et al. 2023](https://bmjpublichealth.bmj.com/content/1/1/e000097) for examples of this approach. The approach originated as part of NHS Health's Scotland's Informing Interventions to reduce health Inequalities (Triple I) work: read more in [Richardson et al. 2020](https://pubmed.ncbi.nlm.nih.gov/32113518/) and on the [Public Health Scotland website](https://publichealthscotland.scot/resources-and-tools/health-strategy-and-outcomes/informing-interventions-to-reduce-health-inequalities-triple-i/overview-of-triple-i/). The code is applicable to Scotland, but the approach could be applied to other contexts.  

# Requirements
* EUROMOD and UKMOD access (see [UKMOD](https://www.microsimulation.ac.uk/ukmod/) pages)
* Authorisation to work with Family Resources Survey data, available from [UK Data Service](https://ukdataservice.ac.uk/)
* Access to NRS mortality microdata (deaths by SIMD decile and single year of age) is useful for updating the model coefficients. The script currently contains coefficients based on populations from 2022 and deaths from 2023. 

# Processing steps
1. Run your systems in UKMOD.
2. Save summary results from the Statistics Presenter for all systems in one file (multiple floppy disc icon) to this project's 'results' folder.
3. Copy UKMOD output files to a 'ukmod output' folder within this project.
4. Script processing_pop_deaths.R: Process death rates (if microdata are available) and population data (open data), to produce standardised death rates and population totals for the required groupings.
5. Script ukmod_processing.R:
6. Process summary results for easier comparisons (fiscal, poverty, income inequality, household incomes, poverty cut-offs) using get_ukmod_data().
7. Extract SIMD deciles for the FRS households from the FRS data from UK Data Service
8. Equivalise disposable incomes (before housing costs) for each household.
9. Assign households to income quintiles and deciles (using FRS weights).
10. Produce summary tables of income distribution, by income quantiles and SIMD quantiles.
11. Estimate the cross-sectional relationship between household income and standardised mortality rates (using data for SIMD deciles). This step is required in the absence of empirical quantification of the longitudinal relationship between income and mortality, which would be more appropriate. It should also be noted that the model currently doesn't include any covariates that could be important (e.g., education, employment, etc) so represents an overall effect.
12. Use the income-mortality relationship to estimate death rates and deaths by age group, sex, and SIMD decile for each scenario.
13. Calculate life expectancy for each scenario
14. Calculate premature mortality rate for each scenario
15. Calculate inequalities: absolute gap, relative gap, slope index of inequality and relative index of inequality (N.B. for life expectancy only in this code)
    
