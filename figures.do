cd "C:\Users\jsanc\Downloads\UGA_2009_UNPS_v01_M_STATA"

/* Data is in logs and in shellings */
use inc2.dta, clear
merge m:m HHID using cons.dta
drop if _merge <3
drop _merge
merge 1:m HHID using wealth.dta
drop if _merge <3
drop _merge

/* Data trasnformation into 2009 USD */


gen cons = exp(cons_t)*0.00045
gen inc = exp(inc_t)*0.00045
gen wealth = exp(wealth_t)*0.00045

/* Q1 */
//1

tabstat cons inc wealth [aw=wgt09wosplits] , by(urban_x)  stat(me)

//2

twoway (histogram cons_t if urban_x==1) (histogram cons_t if urban_x==2, fcolor(none) lcolor(black)), legend(order(1 "Rural" 2 "Urban" )) ylabel(0(.1).6) saving("Q1.2.1", replace)
twoway (histogram inc_t if urban_x==1) (histogram inc_t if urban_x==2, fcolor(none) lcolor(black)), legend(order(1 "Rural" 2 "Urban" )) ylabel(0(.1).6) saving("Q1.2.2", replace)
twoway (histogram wealth_t if urban_x==1) (histogram wealth_t if urban_x==2, fcolor(none) lcolor(black)), legend(order(1 "Rural" 2 "Urban" )) ylabel(0(.1).6) saving("Q1.2.3", replace)
graph combine "Q1.2.1" "Q1.2.2" "Q1.2.3"
graph export "Q1_2.png", replace 
	   
tabstat cons_t inc_t wealth_t [aw=wgt09wosplits], by(urban_x) stat(v)

// 3 

corr cons_t inc_t wealth_t [aw=wgt09wosplits]
corr cons_t inc_t wealth_t [aw=wgt09wosplits] if urban_x == 1
corr cons_t inc_t wealth_t [aw=wgt09wosplits] if urban_x == 2


// 4 

gen int_edad=int(age/5)*5 if age < 70 & age > 19 // Create 5-year smoothing

foreach var of varlist cons inc wealth {
	
	bysort int_edad: egen edad_`var'=mean(`var')
	bysort int_edad: egen edad_`var'_R=mean(`var') if urban_x == 1
	bysort int_edad: egen edad_`var'_U=mean(`var') if urban_x == 2
}

foreach var of varlist cons_t inc_t wealth_t {
	
	bysort int_edad: egen edad_t_`var'=sd(`var')
	bysort int_edad: egen edad_t_`var'_R=sd(`var') if urban_x == 1
	bysort int_edad: egen edad_t_`var'_U=sd(`var') if urban_x == 2
	replace edad_t_`var' = edad_t_`var'^2
	replace edad_t_`var' = edad_t_`var'_R^2
	replace edad_t_`var' = edad_t_`var'_U^2
}

*Mean
twoway (line edad_cons* int_edad, sort xlabel(20(5)65)),legend(order(1 "All" 2 "Rural" 3 "Urban" )) ytitle("Consumption") saving("Q1.4.1", replace)
twoway (line edad_inc* int_edad, sort xlabel(20(5)65)), legend(order(1 "All" 2 "Rural" 3 "Urban" )) ytitle("Income") saving("Q1.4.2", replace)
twoway (line edad_wealth* int_edad, sort xlabel(20(5)65)), legend(order(1 "All" 2 "Rural" 3 "Urban" )) ytitle("Wealth") saving("Q1.4.3", replace)
graph combine "Q1.4.1" "Q1.4.2" "Q1.4.3"
graph export "Q1_4_1.png", replace 

*Variance of logs
twoway (line edad_t_cons* int_edad, sort xlabel(20(5)65)), xtitle("Consumption by age") legend(order(1 "All" 2 "Rural" 3 "Urban" ))   saving("Q1.4.4", replace)
twoway (line edad_t_inc* int_edad, sort xlabel(20(5)65)), xtitle("Income by age")  saving("Q1.4.5", replace)
twoway (line edad_t_wealth* int_edad, sort xlabel(20(5)65)), xtitle("Wealth by age") legend(order(1 "All" 2 "Rural" 3 "Urban" )) saving("Q1.4.6", replace)
graph combine "Q1.4.4" "Q1.4.5" "Q1.4.6"
graph export "Q1_4_2.png", replace 


*Correlation
corr edad_cons edad_inc edad_wealth


// 5

xtile x_inc = inc, nq(100)
xtile q_inc = inc, nq(5)
foreach var of varlist cons wealth {
	bysort x_inc: egen cum_`var' = total(`var')
	bysort q_inc: egen qcum_`var' = total(`var')
	sum `var'
	return list
	gen perc_`var'= cum_`var'/r(sum)*100
	gen quin_`var'= qcum_`var'/r(sum)*100
}

twoway (line perc_cons x_inc, sort ) , xtitle("Percentage of income") ytitle("Percentiles of consumption") saving("Q1.5.1", replace)
twoway (line perc_wealth x_inc, sort ) , xtitle("Percentage of income") ytitle("Percentiles of wealth") saving("Q1.5.2", replace)
graph combine "Q1.5.1" "Q1.5.2"
graph export "Q1_5.png", replace 

/* Q2 */

use lab.dta, clear
gen hours = hours1 + hours2
replace employed = 0 if employed == 2
replace employed = employed*100
 // 1---
//1

tabstat  hours employed , by(urban_x)  stat(me)

//2

twoway (histogram hours if urban_x==1) (histogram hours if urban_x==2, fcolor(none) lcolor(black)), xtitle("Hours by place") legend(order(1 "Rural" 2 "Urban")) saving("Q2.1.1.1", replace)
twoway (histogram employed if urban_x==1) (histogram employed if urban_x==2, fcolor(none) lcolor(black)), xtitle("Employment by place") legend(order(1 "Rural" 2 "Urban")) saving("Q2.1.1.2", replace)
graph combine "Q2.1.1.1" "Q2.1.1.2"
graph export "Q2_1_1.png", replace 

gen l_hours = log(hours)	   
tabstat l_hours , by(urban_x) stat(v)

// 3 

corr hours employed
corr hours employed if urban_x == 1
corr hours employed if urban_x == 2

// 4
gen int_edad=int(h2q8/5)*5 if h2q8< 70 & h2q8> 19

foreach var of varlist hours employed {
	
	bysort int_edad: egen edad_`var'=mean(`var')
	bysort int_edad: egen edad_`var'_R=mean(`var') if urban_x == 1
	bysort int_edad: egen edad_`var'_U=mean(`var') if urban_x == 2
}

foreach var of varlist l_hours {
	
	bysort int_edad: egen edad_t_`var'=sd(`var')
	bysort int_edad: egen edad_t_`var'_R=sd(`var') if urban_x == 1
	bysort int_edad: egen edad_t_`var'_U=sd(`var') if urban_x == 2
	replace edad_t_`var' = edad_t_`var'^2
	replace edad_t_`var' = edad_t_`var'_R^2
	replace edad_t_`var' = edad_t_`var'_U^2
}

twoway (line edad_hours* int_edad, sort xlabel(20(5)65)), ytitle("Mean hours by place") xtitle("Age") legend(order(1 "All" 2 "Rural" 3 "Urban")) saving("Q2.2.1.3", replace)
twoway (line edad_emp* int_edad, sort xlabel(20(5)65)), ytitle("Mean hours by place") xtitle("Age") legend(order(1 "All" 2 "Rural" 3 "Urban")) saving("Q2.2.1.4", replace)
*Variance of logs
twoway (line edad_t_l_hours* int_edad, sort xlabel(20(5)65)), ytitle("Var Logs hours by place") xtitle("Age") legend(order(1 "All" 2 "Rural" 3 "Urban")) saving("Q2.1.1.5", replace)
graph combine "Q2.2.1.3" "Q2.2.1.4" "Q2.1.1.5"
graph export "Q2_1_1_2.png", replace 


 //2----
tabstat  hours employed , by(h2q3)  stat(me)

//2

twoway (histogram hours if h2q3==1) (histogram hours if h2q3==2, fcolor(none) lcolor(black)), xtitle("Hours by sex") legend(order(1 "Male" 2 "Female")) saving("Q2.2.1.1", replace)
twoway (histogram employed if h2q3==1) (histogram employed if h2q3==2, fcolor(none) lcolor(black)), xtitle("Employment by sex") legend(order(1 "Male" 2 "Female")) saving("Q2.2.1.2", replace)
graph combine "Q2.2.1.1" "Q2.2.1.2" 
graph export "Q2_2_1_1.png", replace 

tabstat l_hours , by(h2q3) stat(v)

// 3 

corr hours employed
corr hours employed if h2q3 == 1
corr hours employed if h2q3 == 2
// 4

drop edad_* edad_t_*
foreach var of varlist hours employed {
	
	bysort int_edad: egen edad_`var'=mean(`var')
	bysort int_edad: egen edad_`var'_M=mean(`var') if h2q3 == 1
	bysort int_edad: egen edad_`var'_F=mean(`var') if h2q3 == 2
}

foreach var of varlist l_hours {
	
	bysort int_edad: egen edad_t_`var'=sd(`var')
	bysort int_edad: egen edad_t_`var'_M=sd(`var') if h2q3 == 1
	bysort int_edad: egen edad_t_`var'_F=sd(`var') if h2q3 == 2
	replace edad_t_`var' = edad_t_`var'^2
	replace edad_t_`var' = edad_t_`var'_M^2
	replace edad_t_`var' = edad_t_`var'_F^2
}

twoway (line edad_hours* int_edad, sort xlabel(20(5)65)), ytitle("Mean hours by sex") xtitle("Age") legend(order(1 "All" 2 "Male" 3 "Female")) saving("Q2.2.1.3", replace)
twoway (line edad_emp* int_edad, sort xlabel(20(5)65)), ytitle("Employment rate by sex") xtitle("Age") legend(order(1 "All" 2 "Male" 3 "Female")) saving("Q2.2.1.4", replace)

*Variance of logs
twoway (line edad_t_l_hours* int_edad, sort xlabel(20(5)65)), ytitle("Var Logs hours by sex") xtitle("Age") legend(order(1 "All" 2 "Male" 3 "Female")) saving("Q2.2.1.5", replace)
graph combine "Q2.2.1.3" "Q2.2.1.4" "Q2.2.1.5"
graph export "Q2_2_1_2.png", replace 

 //------ Educ
 
 gen educ = 1 if h4q7 <8
 replace educ = 2 if h4q7 >7 & h4q7 < 17
 replace educ = 3 if h4q7 > 16 & h4q7 < 21
 
tabstat  hours employed , by(educ)  stat(me)

//2

twoway (histogram hours if educ==1) (histogram hours if educ==2, fcolor(none) lcolor(black)) (histogram hours if educ==3, fcolor(none) lcolor(green)),  xtitle("Hours by education") legend(order(1 "Low" 2 "Medium" 3 "High")) saving("Q2.2.2.1", replace)
twoway (histogram employed if educ==1) (histogram employed if educ==2, fcolor(none) lcolor(black)) (histogram employed if educ==3, fcolor(none) lcolor(green)), title("Employment by education") legend(order(1 "Low" 2 "Medium" 3 "High")) saving("Q2.2.2.2", replace)
graph combine "Q2.2.2.1" "Q2.2.2.2"
graph export "Q2_2_2_1.png", replace 


tabstat l_hours , by(educ) stat(v)

// 3 (casi no da nada)

corr hours employed
corr hours employed if educ == 1
corr hours employed if educ == 2
corr hours employed if educ == 3

// 4

drop edad_* edad_t_*
foreach var of varlist hours employed {
	
	bysort int_edad: egen edad_`var'=mean(`var')
	bysort int_edad: egen edad_`var'_N=mean(`var') if educ == 1
	bysort int_edad: egen edad_`var'_P=mean(`var') if educ == 2
	bysort int_edad: egen edad_`var'_S=mean(`var') if educ == 3
}

foreach var of varlist l_hours {
	
	bysort int_edad: egen edad_t_`var'=sd(`var')
	bysort int_edad: egen edad_t_`var'_N=sd(`var') if educ == 1
	bysort int_edad: egen edad_t_`var'_P=sd(`var') if educ == 2
	bysort int_edad: egen edad_t_`var'_S=sd(`var') if educ == 2
	replace edad_t_`var' = edad_t_`var'^2
	replace edad_t_`var' = edad_t_`var'_N^2
	replace edad_t_`var' = edad_t_`var'_P^2
	replace edad_t_`var' = edad_t_`var'_S^2
}

twoway (line edad_hours* int_edad, sort xlabel(20(5)65)), ytitle("Mean hours by education") xtitle("Age") legend(order(1 "All" 2 "Low" 3 "Medium" 4 "High")) saving("Q2.2.2.3", replace)
twoway (line edad_emp* int_edad, sort xlabel(20(5)65)), ytitle("Employment rate by education") xtitle("Age") legend(order(1 "All" 2 "Low" 3 "Medium" 4 "High")) saving("Q2.2.2.4", replace)

*Variance of logs
twoway (line edad_t_l_hours* int_edad, sort xlabel(20(5)65)), ytitle("Var Logs hours by education") xtitle("Age") legend(order(1 "All" 2 "Low" 3 "Medium" 4 "High")) saving("Q2.2.2.5", replace)
graph combine "Q2.2.2.3" "Q2.2.2.4" "Q2.2.2.5"
graph export "Q2_2_2_2.png", replace 

 
/* Q3 */
use inc.dta, clear
merge m:m HHID using cons.dta
drop if _merge <3
drop _merge
merge m:m HHID using wealth.dta
drop if _merge <3
drop _merge
merge m:m HHID using trab.dta
drop if _merge <3
drop _merge

gen cons = exp(cons_t)*0.00045
gen inc = exp(inc_t)*0.00045
gen wealth = exp(wealth_t)*0.00045

preserve

collapse (mean) hours cons inc wealth, by(h1aq1)

foreach var of varlist cons inc wealth {
	replace `var' = log(`var')
	}
//1
twoway scatter hours inc, ytitle("Mean hours") xtitle("Mean Income") legend(order(1 "Districts")) saving("Q3.1.1", replace)
twoway scatter cons inc, ytitle("Mean consumption") xtitle("Mean Income") legend(order(1 "Districts")) saving("Q3.1.2", replace)
twoway scatter wealth inc, ytitle("Mean wealth") xtitle("Mean Income") legend(order(1 "Districts")) saving("Q3.1.3", replace)
graph combine "Q3.1.1" "Q3.1.2" "Q3.1.3"
graph export "Q3_1.png", replace 

restore

//2 
preserve

gen l_hours = log(hours)
collapse (sd) l_hours cons_t inc_t wealth_t (mean) inc, by(h1aq1)
foreach var of varlist l_hours cons_t inc_t wealth_t {
	replace `var' = `var'^2
	}
foreach var of varlist inc {
	replace `var' = log(`var')
	}

twoway scatter l_hours inc, ytitle("Var Logs hours") xtitle("Mean Income") legend(order(1 "Districts")) saving("Q3.2.1", replace)
twoway scatter cons_t inc_t, ytitle("Var Logs consumption") xtitle("Mean Income") legend(order(1 "Districts")) saving("Q3.2.2", replace)
twoway scatter wealth_t inc_t, ytitle("Var Logs wealth") xtitle("Mean Income") legend(order(1 "Districts")) saving("Q3.2.3", replace)
graph combine "Q3.2.1" "Q3.2.2" "Q3.2.3"
graph export "Q3_2.png", replace 

restore

//3
preserve

gen l_hours = log(hours)
bysort h1aq1: egen CI = corr(cons_t inc_t)
bysort h1aq1: egen CW = corr(cons_t wealth_t)
bysort h1aq1: egen IW = corr(inc_t wealth_t)
bysort h1aq1: egen IL = corr(inc_t l_hours)

collapse (mean) CI CW IW IL inc, by(h1aq1)

twoway scatter CI inc, ytitle("Corr. Cons. Inc.") xtitle("Mean Income") legend(order(1 "Districts")) saving("Q3.3.CI", replace)
twoway scatter CW inc, ytitle("Corr. Cons. Wealth") xtitle("Mean Income") legend(order(1 "Districts")) saving("Q3.3.CW", replace)
twoway scatter IW inc, ytitle("Corr. Inc. Wealth") xtitle("Mean Income") legend(order(1 "Districts")) saving("Q3.3.IW", replace)
twoway scatter IL inc, ytitle("Corr. Inc. Hours") xtitle("Mean Income") legend(order(1 "Districts")) saving("Q3.3.IL", replace)
graph combine "Q3.3.CI" "Q3.3.CW" "Q3.3.IW" "Q3.3.IL"
graph export "Q3_3.png", replace 


restore

//4 

gen l_hours = log(hours)

bysort h1aq1: egen reg_inc=sum(inc)
bysort h1aq1: egen reg_hours=sum(hours)

gen iph = log(reg_inc/reg_hours)
gen wph = log(iph*0.0004)
gen age2 = age^2

xi: reg l_hours iph age age2, vce(cluster h1aq1)
xi: reg l_hours wph age age2, vce(cluster h1aq1)
xi: reg l_hours iph wph age age2, vce(cluster h1aq1)
xi: reg l_hours wph i.h1aq1 age age2, vce(cluster h1aq1)
















