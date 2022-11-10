cd "/Users/alexandrahill/Library/CloudStorage/OneDrive-Colostate/AREC 705/Fall 2020/sfbook_demo/"

sfbook_install


* Load dataset
use "dairy.dta"


* create a global for all x vars
*global xvar lland llabor lfeed lcattle
global xvar llabor
* Stage 1: OLS
regress ly $xvar 

** ASIDE **
    * Check RTS
    *display _b[lland]+_b[llabor]+_b[lfeed]+_b[lcattle]

* Stage 2: Corrected OLS -> adjust the intercept by the maximum error term
* store residuals
predict e_oo, resid
sum e_oo

* get maximum residual
display r(max)

* Generate OO inefficiency for each DM/Farm
gen double u_oo = -(e_oo -r(max))
gen double eff_oo = exp(-u_oo)

sum u_oo eff_oo

* Generate IO inefficiency
*gen u_io = u_oo/(_b[lland]+_b[llabor]+_b[lfeed]+_b[lcattle])
gen u_io = u_oo/(_b[llabor])
gen eff_io = exp(-u_io)

* Get Production Frotier
display _cons + r(max) 

* Plot with efficiency scores
gen eff_oo_round = round(eff_oo, 0.01)
gen eff_io_round = round(eff_io, 0.01)

twoway (scatter ly llabor, mlabel(eff_oo_round)), legend(off) ytitle("log of milk prod") xtitle("log of labor") name(eff_oo2, replace) title("OO Efficiency Scores")
twoway (scatter ly llabor, mlabel(eff_io_round)), legend(off) ytitle("log of milk prod") xtitle("log of labor") name(eff_io2, replace) title("IO Efficiency Scores")

* can plot OLS and COLS lines:
regress ly $xvar
predict y_hat, xb
qui sum e_oo
gen double y_hat_cols = y_hat + r(max)
twoway (scatter ly llabor, mlabel(eff_io_round)) (line y_hat llabor) (line y_hat_cols llabor), legend(pos(6) col(2) label(2 "OLS") label(3 "COLS") order(2 3)) ytitle("log of milk production") name(cols, replace)

*************************************
*************************************
*************** CMAD ****************
*************************************
*************************************

* stage 1: quantile regression:
    qreg ly $xvar, quant(0.5)

* stage 2: adjust intercept and estimate efficiency
    * store resids
        predict e_cmad, resid
    * get max
        sum e_cmad
    * gen efficiency and inefficiency
        gen u_oo_cmad =  -(e_cmad - r(max))
        gen eff_oo_cmad = exp(-u_oo_cmad)

        sum *oo_cmad
* Generate IO inefficiency
    gen u_io_cmad = u_oo_cmad/(_b[llabor])
    gen eff_io_cmad = exp(-u_io_cmad)
        sum *io_cmad

* plot 
    qreg ly $xvar, quant(0.5)
    predict y_hat_mad, xb
    qui sum e_cmad
    gen y_hat_cmad = y_hat_mad +r(max)

    gen eff_oo_cmad_round = round(eff_oo_cmad, 0.01)

    twoway (scatter ly llabor,  mlabel(eff_oo_cmad_round)) (line y_hat_mad llabor) (line y_hat_cmad llabor), legend(pos(6) col(2) label(2 "Mean Absolute Deviation") label(3 "Corrected Mean Absolute Deviation") order(2 3)) ytitle("log of milk production") name(cmad, replace)

*************************************
*************************************
************** COMPARE **************
*************************************
*************************************


* compare COLS and CMAD frontiers
		twoway (scatter ly llabor) (line y_hat_mad llabor) (line y_hat_cmad llabor) (line y_hat llabor) (line y_hat_cols llabor), legend(pos(6) col(2) label(2 "Mean Absolute Deviation") label(3 "Corrected Mean Absolute Deviation") label(4 "OLS") label(5 "COLS") order(4 5 2 3 )) ytitle("log of milk production") name(cmad_cols, replace)

	* compare with DEA frontier
		* create DEA efficiency score for one-input one-output
		gen ratio = ly/llabor
		qui sum ratio
		gen eff_dea = ratio/r(max)
		sum eff_dea

		* scatter plot with labels of efficiency score (round score so plot looks nicer)
		gen eff_dea_round = round(eff_dea, 0.01)
		twoway scatter ly llabor , legend(off) mlabel(eff_dea_round) ytitle("log of milk production") name(dea_eff, replace)

		* create a line that goes through the origin and the most efficient point
		sum ly if eff_dea==1
		local delta_y = r(max)
		sum llabor if eff_dea==1
		local delta_x = r(max)

		local slope = `delta_y'/`delta_x'

		gen y_hat_dea = `slope'*llabor


	* compare all 
		twoway (scatter ly llabor) (line y_hat_mad llabor) (line y_hat_cmad llabor) (line y_hat llabor) (line y_hat_cols llabor) (line y_hat_dea llabor, lcolor(blue)), legend(pos(6) col(2) label(2 "MAD") label(3 "CMAD") label(4 "OLS") label(5 "COLS") label(6 "CRS DEA") order(4 5 2 3 6 )) ytitle("log of milk production") name(cmad_cols_dea, replace)


    * compare measures of efficiency:   
		order eff_dea eff_io_cmad eff_io farmid 
		gsort -eff_dea

    * compare ranks:
		foreach var of varlist eff_io eff_io_cmad eff_dea {
			gsort -`var' 
			gen rank_`var' = _n
		}

		order eff_dea eff_io_cmad eff_io farmid rank_* farmid 
		gsort -eff_dea


*** Test for skewnesss
* create a global for all x vars
global xvar lland llabor lfeed lcattle
* Stage 1: OLS
regress ly $xvar 

* store residuals
predict e_oo2, resid
sum e_oo2

* start with a visual test
* get std deviation from residuals (ols)
sum e_oo2
global sd = r(sd)

graph twoway histogram e_oo2, xlabel(-.6(.2).6) legend(label(2 "normal density") label(3 "observed density") order(2 3)) || function normalden(x,0, $sd), range(-.6 .6)||kdensity e_oo

sum e_oo2, detail
* Check that that this is the test statistic we are interested in
qui sum e_oo2
local e_mean = r(mean)
egen double m2 = mean((e_oo2-`e_mean')^2)
egen double m3 = mean((e_oo2-`e_mean')^3)
local sqrt_b1 = m3/(m2*m2^(1/2))
display `sqrt_b1'

sktest e_oo2, noadj /* unaltered test */
sktest e_oo2 /*Royston (1991) altered test */

* Coelli test
local N = _N
local m3t = m3/sqrt((6*(m2^3))/`N')
display `m3t'
* -4.2164605
* compare this with normal distribution critical value of (e.g.) -1.96.
* Confirms our rejection of the null of no skewness

*** Half-Normal Model ***
* Use (user-written) sfmodel command
sfmodel ly, prod dist(h) frontier($xvar) usigmas() vsigmas() show
ml max, difficult gradient gtol(1e-5) nrtol(1e-5)
* note that variance parameters are exponentials, to get variance parameters:
sf_transform

* compare with OLS -> note should be same betas bc those are consistent.
* OLS regression
reg ly $xvar 

** provide starting values from OLS
* store coefs as vector
matrix b_ols = e(b)
* look at vector
matrix list b_ols

* Use (user-written) sfmodel command
sfmodel ly, prod dist(h) frontier($xvar) usigmas() vsigmas() show
* sf_init to set initial values, need inputs for each parameter
sf_init, frontier(b_ols) usigmas(0.1) vsigmas(0.1)

/* Optional: wrapper for ml plot to search for better initial values before starting
sf_srch, frontier($xvar) usigmas() vsigmas() n(2)

* note that this will flash a bunch of graphs because it is in fact running ml plot for each variable
ml plot lcattle
*/

ml max, difficult gradient gtol(1e-5) nrtol(1e-5)
* note that the ml estimation converged in only 9 iterations (vs 13 previously) 
sf_transform

* get log likelihood from SFA half normal * Test for validity
sfmodel ly, prod dist(h) frontier($xvar) usigmas() vsigmas() 
qui ml max, difficult gradient gtol(1e-5) nrtol(1e-5)
* store log likelihood
scalar ll_hn = e(ll)

* log likelihood from OLS
qui regress ly $xvar
scalar ll_ols = e(ll)

* get LR stat:
display -2*(ll_ols - ll_hn)

* look at critical values
* note that DOF = 1 bc only one parameter (sigma_u is restricted in the test)
sf_mixtable, dof(1)

*** Technical Efficiency ***
* use sf_predict to get efficiency scores
sfmodel ly, prod dist(h) frontier($xvar) usigmas() vsigmas() // production frontier type model
qui ml max, difficult gradient gtol(1e-5) nrtol(1e-5)

sf_predict, bc(eff_hn) jlms(u_hn) ci(95)

* look at summary stats
sum eff_hn u_hn
sum eff_hn* u_hn*

hist eff_hn


* half-normal with heterogeneity
* we are going to use the variable comp 
* (IT expenditure as a percentage of total expenditure) 
* as the exogenous determininant of inefficiency. 
* Note this is a somewhat 'leanient' use of the word exogenous :)

sfmodel ly, prod dist(h) frontier($xvar) usigmas(comp) vsigmas() show
sf_init, frontier(b_ols) usigmas(0.1 0.1) vsigmas(0.1)
sf_srch, frontier($xvar) usigmas(comp) n(1) nograph fast
ml max, difficult  gtol(1e-5) nrtol(1e-5)

* recover the variance parameters
sf_transform

* efficiency index (and marginal effect)
sf_predict, bc(eff_hn2) jlms(u_hn2) marginal
gsort -eff_hn2
order eff_hn2 eff_hn


*** Truncated Normal
* in the above code, replace dist(h) with: (other than that use the same options)
sfmodel ly, prod dist(t) frontier($xvar) usigmas() vsigmas() mu() show
sf_init, frontier(b_ols) usigmas(0.1 0.1) vsigmas(0.1) mu(0.1)
sf_srch, frontier($xvar) usigmas() vsigmas() n(2)
ml max, difficult gradient gtol(1e-5) nrtol(1e-5)
sf_transform
sf_predict, bc(eff_t) jlms(u_t) marginal ci(95)

*** T Normal with Heteroskedasticity
sfmodel ly, prod dist(t) frontier($xvar) usigmas(comp) vsigmas() mu(comp) show
sf_srch, frontier($xvar) usigmas(comp) vsigmas() mu(comp) n(1) nograph fast
ml max, difficult gradient gtol(1e-5) nrtol(1e-5)
** lltest store
scalar ll_ht = e(ll)
sf_transform
* ll test perform
di -2*(ll_ols - ll_ht)
* check crit values
sf_mixtable, dof(4)
sf_predict, bc(eff_ht) jlms(u_ht) marginal ci(95)

** note IO TE:
gen u_io_ht = u_ht/(_b[lland] +_b[llabor] +_b[lfeed] +_b[lcattle])

sum u_ht u_io_ht
gen eff_io_ht = exp(-u_io_ht)
sum eff_*ht