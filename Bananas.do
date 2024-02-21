

*DO File For "The hidden cost of bananas: The effects of pesticides on newborns' health"
*Published in the Journal of the Association of Environmental and Resource Economists
*Joan Calzada (calzada@ub.edu), Meritxell Gisbert (meritxell.gisbert@uab.cat), & Bernard Moscoso (hmoscoso@espol.edu.ec)
*March 2023
ssc install ftools
ssc install reghdfe
ssc install estout 
********************************************************************************************************************************************************************************************************************************************************
*Table 1: Effects of pesticides on newborns' health – A comparative review
*This table reviews the literature examining the effects of pesticides on newborns’ health
********************************************************************************************************************************************************************************************************************************************************

clear all
set more off
set matsize 11000

*Define cd and load data:

cd <define location file>

********************************************************************************************************************************************************************************************************************************************************
*Table 2 – Descriptive Statistics: Means (standard deviations) of selected variables
********************************************************************************************************************************************************************************************************************************************************
clear all

use bananas1.dta, clear
global vars weight gweek lbw preterm age dsex1 deduc1 dethnic6 tlabor1 dmarital1 dmarital2 dmarital3 dprivate1 labors nchild visits dnbl1
eststo: mean $vars if esample==1
eststo: mean $vars if esample==1 & bx==0
eststo: mean $vars if esample==1 & bx==1
eststo: mean $vars if esample==1 & pxp==0
eststo: mean $vars if esample==1 & pxp==1
esttab using table2.csv, replace

discard



********************************************************************************************************************************************************************************************************************************************************
********************************************************************************************************************************************************************************************************************************************************
*ESTIMATIONS
********************************************************************************************************************************************************************************************************************************************************
********************************************************************************************************************************************************************************************************************************************************
*Note: See the notes in the tables to know more about the specification of the regressions and the bootstrapping.
*Estimations consider bootstrapped s.e. with 1000 reps. 
*Since bootstrapping implies a lot of time for replication, we write the regressions below without this command.
*An example of how to run the regressions with bootstrappimg is the following, based on the first estimation of Table 3:
*bootstrap, rep(1000): reghdfe weight bx age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
*Please, consider that each bootstrap repetition computes different standard errors and therefore the significance levels may vary.
********************************************************************************************************************************************************************************************************************************************************


********************************************************************************************************************************************************************************************************************************************************
*Table 3 - Effects of the seasonal intensification of fumigations on birthweight
********************************************************************************************************************************************************************************************************************************************************

discard
clear all

use bananas1.dta, clear
eststo: reghdfe weight bx age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight bx##pxp age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight bx##pxt1 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight bx##pxt2 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight bx##pxt3 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight bx##pxt1 bx##pxt2 bx##pxt3 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
esttab using table3.rtf, keep(bx 1.bx 1.pxp 1.bx#1.pxp 1.pxt1 1.pxt2 1.pxt3 1.bx#1.pxt1 1.bx#1.pxt2 1.bx#1.pxt3) se r2 pr2 scalars("N_clust Number of grids") starlevels(* .1 ** .05 *** .01) replace

discard
clear all

********************************************************************************************************************************************************************************************************************************************************
*Table 4 - Effects of the seasonal intensification of fumigations on gestation weeks, preterm and low birth weight
********************************************************************************************************************************************************************************************************************************************************

discard
clear all

use bananas1.dta, clear
eststo: reghdfe gweek bx##pxp age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe gweek bx##pxt1 bx##pxt2 bx##pxt3 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
eststo: logit preterm bx##pxp age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid, or robust cluster(grid) nolog
eststo: logit preterm bx##pxt1 bx##pxt2 bx##pxt3 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid, or robust cluster(grid) nolog
eststo: logit lbw bx##pxp age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid, or robust cluster(grid) nolog
eststo: logit lbw bx##pxt1 bx##pxt2 bx##pxt3 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid, or robust cluster(grid) nolog
esttab using table4.rtf, eform(0 0 1 1 1 1) keep(1.bx 1.pxp 1.bx#1.pxp 1.pxt1 1.pxt2 1.pxt3 1.bx#1.pxt1 1.bx#1.pxt2 1.bx#1.pxt3) se r2 pr2 scalars("N_clust Number of grids") starlevels(* .1 ** .05 *** .01) replace

discard
clear all

********************************************************************************************************************************************************************************************************************************************************
*Table 5 - Effects of the seasonal intensification of fumigations on birth weight and gestation weeks – Maternal fixed effects
********************************************************************************************************************************************************************************************************************************************************
*Note: The number of clusters reflects the number of mothers.

discard
clear all

use bananas2.dta, clear
eststo: reghdfe weight bx birth_interval1 birth_interval2 birth_interval3 age educ private labors nchild visits nbl sex tlabor i.marital, absorb(cohort mfeid##grid) vce(cluster mfeid grid)
eststo: reghdfe weight bx##pxp birth_interval1 birth_interval2 birth_interval3 age educ private labors nchild visits nbl sex tlabor i.marital, absorb(cohort mfeid##grid) vce(cluster mfeid grid)
eststo: reghdfe weight bx##pxt1 bx##pxt2 bx##pxt3 birth_interval1 birth_interval2 birth_interval3 age educ private labors nchild visits nbl sex tlabor i.marital, absorb(cohort mfeid##grid) vce(cluster mfeid grid)
eststo: reghdfe weight bx##pxt1 bx##pxt2 bx##pxt3 birth_interval1 birth_interval2 birth_interval3 age educ private labors nchild visits nbl sex tlabor i.marital if stay>0, absorb(cohort mfeid##grid) vce(cluster mfeid grid)
eststo: reghdfe gweek bx birth_interval1 birth_interval2 birth_interval3 age educ private labors nchild visits nbl sex tlabor i.marital, absorb(cohort mfeid##grid) vce(cluster mfeid grid)
eststo: reghdfe gweek bx##pxp birth_interval1 birth_interval2 birth_interval3 age educ private labors nchild visits nbl sex tlabor i.marital, absorb(cohort mfeid##grid) vce(cluster mfeid grid)
eststo: reghdfe gweek bx##pxt1 bx##pxt2 bx##pxt3 birth_interval1 birth_interval2 birth_interval3 age educ private labors nchild visits nbl sex tlabor i.marital, absorb(cohort mfeid##grid) vce(cluster mfeid grid)
eststo: reghdfe gweek bx##pxt1 bx##pxt2 bx##pxt3 birth_interval1 birth_interval2 birth_interval3 age educ private labors nchild visits nbl sex tlabor i.marital if stay>0, absorb(cohort mfeid##grid) vce(cluster mfeid grid)
esttab using table5.rtf, keep(bx 1.bx 1.pxp 1.bx#1.pxp 1.pxt1 1.pxt2 1.pxt3 1.bx#1.pxt1 1.bx#1.pxt2 1.bx#1.pxt3) se r2 pr2 scalars("N_clust Number of mothers") starlevels(* .1 ** .05 *** .01) replace

discard
clear all

********************************************************************************************************************************************************************************************************************************************************
*Table 6 - Effects of the seasonal intensification of fumigations on maternal characteristics: main estimation approach
********************************************************************************************************************************************************************************************************************************************************

discard
clear all

use bananas1.dta, clear
eststo: reghdfe labors bx##pxp age educ private nchild visits nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe nchild bx##pxp age educ private labors visits nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe nbl bx##pxp age educ private labors nchild visits sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe visits bx##pxp age educ private labors nchild nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
eststo: logit educ bx##pxp age private labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid, or robust cluster(grid) nolog
eststo: logit private bx##pxp age educ labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid, or robust cluster(grid) nolog
eststo: logit tlabor bx##pxp age educ private labors nchild visits nbl sex i.marital i.ethnic i.cohort i.grid, or robust cluster(grid) nolog
esttab using table6.rtf, eform(0 0 0 0 1 1 1) keep(1.bx 1.pxp 1.bx#1.pxp) se r2 pr2 scalars("N_clust Number of grids") starlevels(* .1 ** .05 *** .01) replace

discard
clear all

********************************************************************************************************************************************************************************************************************************************************
********************************************************************************************************************************************************************************************************************************************************
*ONLINE APPENDIX
********************************************************************************************************************************************************************************************************************************************************
********************************************************************************************************************************************************************************************************************************************************
********************************************************************************************************************************************************************************************************************************************************
*Table A1 - Effects of the seasonal intensification of fumigations on birthweight
********************************************************************************************************************************************************************************************************************************************************

discard
clear all

use bananas1.dta, clear
eststo: reghdfe weight bx##pxp rxp5002 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight bx##pxp rxp5003 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight bx##pxp rxp7502 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight bx##pxp rxp7503 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
esttab using tableA1.rtf, keep(1.bx 1.pxp 1.bx#1.pxp rxp5002 rxp5003 rxp7502 rxp7503) se r2 scalars("N_clust Number of grids") starlevels(* .1 ** .05 *** .01) replace

discard
clear all

********************************************************************************************************************************************************************************************************************************************************
*Table A2 - Effects of the seasonal intensification of fumigations on birthweight and low birthweight: gender
********************************************************************************************************************************************************************************************************************************************************

discard
clear all

use bananas1.dta, clear
eststo: reghdfe weight bx##pxp age educ private labors nchild visits nbl tlabor i.marital i.ethnic if sex==1, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight bx##pxt1 bx##pxt2 bx##pxt3 age educ private labors nchild visits nbl tlabor i.marital i.ethnic if sex==1, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight bx##pxp age educ private labors nchild visits nbl tlabor i.marital i.ethnic if sex==0, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight bx##pxt1 bx##pxt2 bx##pxt3 age educ private labors nchild visits nbl tlabor i.marital i.ethnic if sex==0, absorb(cohort grid) vce(cluster grid)
eststo: logit lbw bx##pxp age educ private labors nchild visits nbl tlabor i.marital i.ethnic i.cohort i.grid if sex==1, or robust cluster(grid) nolog
eststo: logit lbw bx##pxt1 bx##pxt2 bx##pxt3 age educ private labors nchild visits nbl tlabor i.marital i.ethnic i.cohort i.grid if sex==1, or robust cluster(grid) nolog
eststo: logit lbw bx##pxp age educ private labors nchild visits nbl tlabor i.marital i.ethnic i.cohort i.grid if sex==0, or robust cluster(grid) nolog
eststo: logit lbw bx##pxt1 bx##pxt2 bx##pxt3 age educ private labors nchild visits nbl tlabor i.marital i.ethnic i.cohort i.grid if sex==0, or robust cluster(grid) nolog
esttab using tableA2.rtf, eform(0 0 0 0 1 1 1 1) keep(1.bx 1.pxp 1.bx#1.pxp 1.pxt1 1.pxt2 1.pxt3 1.bx#1.pxt1 1.bx#1.pxt2 1.bx#1.pxt3) se r2 pr2 scalars("N_clust Number of grids") starlevels(* .1 ** .05 *** .01) replace

discard
clear all

********************************************************************************************************************************************************************************************************************************************************
*Table A3 - Effects of the seasonal intensification of fumigations on birth weight: education
********************************************************************************************************************************************************************************************************************************************************

discard
clear all

use bananas1.dta, clear
eststo: reghdfe weight bx##pxp age private labors nchild visits nbl sex tlabor i.marital i.ethnic if educ==0, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight bx##pxt1 bx##pxt2 bx##pxt3 age private labors nchild visits nbl sex tlabor i.marital i.ethnic if educ==0, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight bx##pxp age private labors nchild visits nbl sex tlabor i.marital i.ethnic if educ==1, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight bx##pxt1 bx##pxt2 bx##pxt3 age private labors nchild visits nbl sex tlabor i.marital i.ethnic if educ==1, absorb(cohort grid) vce(cluster grid)
eststo: logit lbw bx##pxp age private labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid if educ==0, or robust cluster(grid) nolog
eststo: logit lbw bx##pxt1 bx##pxt2 bx##pxt3 age private labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid if educ==0, or robust cluster(grid) nolog
eststo: logit lbw bx##pxp age private labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid if educ==1, or robust cluster(grid) nolog
eststo: logit lbw bx##pxt1 bx##pxt2 bx##pxt3 age private labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid if educ==1, or robust cluster(grid) nolog

esttab using tableA3.rtf, eform(0 0 0 0 1 1 1 1) keep(1.bx 1.pxp 1.bx#1.pxp 1.pxt1 1.pxt2 1.pxt3 1.bx#1.pxt1 1.bx#1.pxt2 1.bx#1.pxt3) se r2 pr2 scalars("N_clust Number of grids") starlevels(* .1 ** .05 *** .01) replace

discard
clear all

********************************************************************************************************************************************************************************************************************************************************
*Table A4 – Probability of being gestated during periods of intensive fumigations for younger siblings
********************************************************************************************************************************************************************************************************************************************************

discard
clear all

use bananas2.dta, clear
eststo: reghdfe pxp bx##second birth_interval1 birth_interval2 birth_interval3 age educ private labors nchild visits nbl sex tlabor i.marital, absorb(cohort mfeid##grid) vce(cluster mfeid)
eststo: reghdfe pxt1 bx##second birth_interval1 birth_interval2 birth_interval3 age educ private labors nchild visits nbl sex tlabor i.marital, absorb(cohort mfeid##grid) vce(cluster mfeid)
eststo: reghdfe pxt2 bx##second birth_interval1 birth_interval2 birth_interval3 age educ private labors nchild visits nbl sex tlabor i.marital, absorb(cohort mfeid##grid) vce(cluster mfeid)
eststo: reghdfe pxt3 bx##second birth_interval1 birth_interval2 birth_interval3 age educ private labors nchild visits nbl sex tlabor i.marital, absorb(cohort mfeid##grid) vce(cluster mfeid)
eststo: reghdfe pxp bx##secondlearn birth_interval1 birth_interval2 birth_interval3 age educ private labors nchild visits nbl sex tlabor i.marital, absorb(cohort mfeid##grid) vce(cluster mfeid)
eststo: reghdfe pxt1 bx##secondlearn birth_interval1 birth_interval2 birth_interval3 age educ private labors nchild visits nbl sex tlabor i.marital, absorb(cohort mfeid##grid) vce(cluster mfeid)
eststo: reghdfe pxt2 bx##secondlearn birth_interval1 birth_interval2 birth_interval3 age educ private labors nchild visits nbl sex tlabor i.marital, absorb(cohort mfeid##grid) vce(cluster mfeid)
eststo: reghdfe pxt3 bx##secondlearn birth_interval1 birth_interval2 birth_interval3 age educ private labors nchild visits nbl sex tlabor i.marital, absorb(cohort mfeid##grid) vce(cluster mfeid)

esttab using tableA4.rtf, keep(1.bx 1.second 1.secondlearn 1.bx#1.second 1.bx#1.secondlearn) se r2 scalars("N_clust Number of mothers") replace

discard
clear all

********************************************************************************************************************************************************************************************************************************************************
*Table A5 – Effects of exposition to pesticides on birth weight
********************************************************************************************************************************************************************************************************************************************************

discard
clear all

use bananas3.dta, clear
eststo: reghdfe weight pxpb age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic if dist<=2500 & preterm==0 & buffmatched==3, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight pxt1b pxt2b pxt3b age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic if dist<=2500 & preterm==0 & buffmatched==3, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight pxpb age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic if dist<=2500 & preterm==0, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight pxt1b pxt2b pxt3b age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic if dist<=2500 & preterm==0, absorb(cohort grid) vce(cluster grid)
esttab using tableA5.rtf, keep(pxpb pxt1b pxt2b pxt3b) se r2 scalars("N_clust Number of grids") starlevels(* .1 ** .05 *** .01) replace

discard
clear all

********************************************************************************************************************************************************************************************************************************************************
*Table A6 - Effects of the seasonal intensification of fumigations on birth weight: sensitivity check on exposure length to intensive fumigations
********************************************************************************************************************************************************************************************************************************************************

discard
clear all

use bananas1.dta, clear
eststo: reghdfe weight bx##pxp_2 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight bx##pxt1_2 bx##pxt2_2 bx##pxt3_2 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
eststo: logit lbw bx##pxp_2 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid, or robust cluster(grid) nolog
eststo: logit lbw bx##pxt1_2 bx##pxt2_2 bx##pxt3_2 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid, or robust cluster(grid) nolog

esttab using tableA6.rtf, eform(0 0 1 1) keep(1.bx 1.pxp_2 1.bx#1.pxp_2 1.pxt1_2 1.pxt2_2 1.pxt3_2 1.bx#1.pxt1_2 1.bx#1.pxt2_2 1.bx#1.pxt3_2) se r2 pr2 scalars("N_clust Number of grids") starlevels(* .1 ** .05 *** .01) replace

discard
clear all

********************************************************************************************************************************************************************************************************************************************************
*Table A7 - Effects of the seasonal intensification of fumigations on birth weight: sensitivity check on the threshold level of intensive fumigations
********************************************************************************************************************************************************************************************************************************************************

discard
clear all

use bananas3.dta, clear
eststo: reghdfe weight bx##pxp_3 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic if dist<=2500 & vary3>0 & vary3<12, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight bx##pxt1_3 bx##pxt2_3 bx##pxt3_3 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic if dist<=2500 & vary3>0 & vary3<12, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight bx##pxp_5 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic if dist<=2500 & vary5>0 & vary5<12, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight bx##pxt1_5 bx##pxt2_5 bx##pxt3_5 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic if dist<=2500 & vary5>0 & vary5<12, absorb(cohort grid) vce(cluster grid)
esttab using tableA7.rtf, keep(1.bx 1.pxp_3 1.bx#1.pxp_3 1.pxt1_3 1.pxt2_3 1.pxt3_3 1.bx#1.pxt1_3 1.bx#1.pxt2_3 1.bx#1.pxt3_3 1.pxp_5 1.bx#1.pxp_5 1.pxt1_5 1.pxt2_5 1.pxt3_5 1.bx#1.pxt1_5 1.bx#1.pxt2_5 1.bx#1.pxt3_5) se r2 scalars("N_clust Number of grids") starlevels(* .1 ** .05 *** .01) replace

discard
clear all

********************************************************************************************************************************************************************************************************************************************************
*Table A8 - Effects of the seasonal intensification of fumigations on newborns’ health: exclusion of newborns at distances between 150 and 250 meters
********************************************************************************************************************************************************************************************************************************************************

discard
clear all

use bananas1.dta, clear
eststo: reghdfe weight bx##pxp age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic if donut!=1, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight bx##pxt1 bx##pxt2 bx##pxt3 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic if donut!=1, absorb(cohort grid) vce(cluster grid)
eststo: logit lbw bx##pxp age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid if donut!=1, or robust cluster(grid) nolog
eststo: logit lbw bx##pxt1 bx##pxt2 bx##pxt3 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid if donut!=1, or robust cluster(grid) nolog
eststo: reghdfe gweek bx##pxp age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic if donut!=1, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe gweek bx##pxt1 bx##pxt2 bx##pxt3 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic if donut!=1, absorb(cohort grid) vce(cluster grid)
eststo: logit preterm bx##pxp age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid if donut!=1, or robust cluster(grid) nolog
eststo: logit preterm bx##pxt1 bx##pxt2 bx##pxt3 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid if donut!=1, or robust cluster(grid) nolog
esttab using tableA8.rtf, eform(0 0 1 1 0 0 1 1) keep(1.bx 1.pxp 1.bx#1.pxp 1.pxt1 1.pxt2 1.pxt3 1.bx#1.pxt1 1.bx#1.pxt2 1.bx#1.pxt3) se r2 pr2 scalars("N_clust Number of grids") starlevels(* .1 ** .05 *** .01) replace

discard
clear all

********************************************************************************************************************************************************************************************************************************************************
*Table A9 – Maternal characteristics by exposure to banana plantations and to other crops
********************************************************************************************************************************************************************************************************************************************************

discard
clear all

use bananas3.dta, clear

*To create the TableA9 it is necessary to:
*1. Run each dmout command below
*2. Open the generated .csv files
*3. Copy and paste the contents of the .csv files into one file.
*4. Be aware that in all .csv the first column contains the same variables.
global vars weight gweek lbw preterm age dsex1 deduc1 dethnic6 tlabor1 dmarital1 dmarital2 dmarital3 dprivate1 labors nchild visits dnbl1
dmout $vars using banriceA9, by(banar) replace
dmout $vars using bancornA9, by(banco) replace
dmout $vars using bancocoaA9, by(banca) replace
dmout $vars using banocA9, by(banoc) replace

discard
clear all

********************************************************************************************************************************************************************************************************************************************************
*Table A10 - Placebo test - Exposure to other crops and intensification of fumigations
********************************************************************************************************************************************************************************************************************************************************

use bananas3.dta, clear
eststo: reghdfe weight ocx##pxp age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic if dist>150 & vary>0 & vary<12, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight ocx##pxt1 ocx##pxt2 ocx##pxt3 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic if dist>150 & vary>0 & vary<12, absorb(cohort grid) vce(cluster grid)
eststo: logit lbw ocx##pxp age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid if dist>150 & vary>0 & vary<12, or robust cluster(grid) nolog
eststo: logit lbw ocx##pxt1 ocx##pxt2 ocx##pxt3 age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid if dist>150 & vary>0 & vary<12, or robust cluster(grid) nolog
esttab using tableA10.rtf, eform(0 0 1 1) keep(1.ocx 1.pxp 1.ocx#1.pxp 1.pxt1 1.pxt2 1.pxt3 1.ocx#1.pxt1 1.ocx#1.pxt2 1.ocx#1.pxt3) se r2 pr2 scalars("N_clust Number of grids") replace

discard
clear all

********************************************************************************************************************************************************************************************************************************************************
*Table A11 - Falsification test - Exposure lags and leads time distortion of the treatment on birthweight
********************************************************************************************************************************************************************************************************************************************************

discard
clear all

use bananas1.dta, clear
eststo: reghdfe weight bx##pxp_lag age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight bx##pxt1_lag bx##pxt2_lag bx##pxt3_lag age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
eststo: logit lbw bx##pxp_lag age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid, or robust cluster(grid) nolog
eststo: logit lbw bx##pxt1_lag bx##pxt2_lag bx##pxt3_lag age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid, or robust cluster(grid) nolog
eststo: reghdfe weight bx##pxp_lead age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
eststo: reghdfe weight bx##pxt1_lead bx##pxt2_lead bx##pxt3_lead age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic, absorb(cohort grid) vce(cluster grid)
eststo: logit lbw bx##pxp_lead age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid, or robust cluster(grid) nolog
eststo: logit lbw bx##pxt1_lead bx##pxt2_lead bx##pxt3_lead age educ private labors nchild visits nbl sex tlabor i.marital i.ethnic i.cohort i.grid, or robust cluster(grid) nolog
esttab using tableA11.rtf, eform(0 0 1 1 0 0 1 1) keep(1.bx 1.pxp_lag 1.bx#1.pxp_lag 1.pxt1_lag 1.pxt2_lag 1.pxt3_lag 1.bx#1.pxt1_lag 1.bx#1.pxt2_lag 1.bx#1.pxt3_lag 1.pxp_lead 1.bx#1.pxp_lead 1.pxt1_lead 1.pxt2_lead 1.pxt3_lead 1.bx#1.pxt1_lead 1.bx#1.pxt2_lead 1.bx#1.pxt3_lead) se r2 pr2 scalars("N_clust Number of grids") starlevels(* .1 ** .05 *** .01) replace

discard
clear all

********************************************************************************************************************************************************************************************************************************************************
********************************************************************************************************************************************************************************************************************************************************
********************************************************************************************************************************************************************************************************************************************************

*END*

*For any question contact hmoscoso@espol.edu.ec

********************************************************************************************************************************************************************************************************************************************************
********************************************************************************************************************************************************************************************************************************************************
********************************************************************************************************************************************************************************************************************************************************

