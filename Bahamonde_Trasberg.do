/*
_Bahamonde and Trasberg_
This DO file is to run all the TS pre-tests.
Analyses are done in R, but first, here we test for stationarity, and other things.
*/


set more off, permanently
set scheme s1color, permanently
clear all

* Now, load the DF directly from the processed R code.

*Hector
use "/Users/hectorbahamonde/RU/research/Inequality_State_Capacities/df.dta", clear

*Mart
* WRITE YOUR ENTIRE PATH HERE.


** This below is to find duplicates
* sort country year
* capture drop dup
* quietly by country year:  gen dup = cond(_N==1,0,_n)
* tabulate dup

** This below is to convert categorical countries, and convert them into numerical (so we can then index every row by country--and year). It's not nec. if the countries already enter as numerical.
* transform country var into numeric
* capture drop countrynum
* encode country, generate(countrynum) 
* capture drop country
* rename countrynum country

*tset df
xtset country year, yearly

* unit root tests
** null hypothesis: all the panels contain unit roots
*** with significative p-value: reject null
*** with non-significative p-value: accept null

*** --> we want to find non-significative p-values

xtunitroot fisher polity, dfuller lags(10) /* two tests UNIT ROOT, two tests NO UNIT ROOT*/
xtunitroot fisher cum_census, dfuller lags(10) /* unit root */
xtunitroot fisher wdi_gdp, dfuller lags(10) /* unit root */
xtunitroot fisher wdi_InflationGdp, dfuller lags(10) /* NO unit root */
xtunitroot fisher wdi_ForeignDirect, dfuller lags(10) /* unit root */
xtunitroot fisher wdi_TradeOfGdp, dfuller lags(10) /* unit root */
xtunitroot fisher wdi_AgricultureVa, dfuller lags(10) /* unit root */
xtunitroot fisher wdi_PopulationAges, dfuller lags(10) /* two tests UNIT ROOT, two tests NO UNIT ROOT*/
xtunitroot fisher wdi_UrbanPopulatio, dfuller lags(10) /* two tests UNIT ROOT, two tests NO UNIT ROOT*/


xtunitroot fisher v2x_polyarchy, dfuller lags(10) /* unit root */
xtunitroot fisher wdi_GovernmentExpe, dfuller lags(1) /* unit root; it takes only 1 lag */
xtunitroot fisher wdi_health_exp_gdp, dfuller lags(5) /* unit root */
xtunitroot fisher wdi_govt_exp_gdp, dfuller lags(10) /* unit root */
xtunitroot fisher wdi_govt_exp_gdp, dfuller lags(10) /* unit root */
xtunitroot fisher state_history_5, dfuller lags(1) /* unit root; two tests worked, the other two didn't */
xtunitroot fisher wdi_TaxesOnIncome, dfuller lags(2) /* unit root; but with 3 lags it's NOT UNIT ROOT; with 10 lags won't work */
xtunitroot fisher wdi_TaxRevenue, dfuller lags(5) /* unit root; won't work with 10 */


xtunitroot fisher fraser_econ_fred_sum, dfuller lags(6) /* NOT UNIT ROOT */
xtunitroot fisher gfm_Cim1, dfuller lags(4) /* UNIT ROOT; above 5, it won't work */
xtunitroot fisher fraser_econ_fred_sum, dfuller lags(6) /* NOT UNIT ROOT */
xtunitroot fisher financial_system_pcrdbofgdp, dfuller lags(10) /* unit root with 5 and 10 lags */
xtunitroot fisher financial_system_fdgdp, dfuller lags(10) /* unit root with */
