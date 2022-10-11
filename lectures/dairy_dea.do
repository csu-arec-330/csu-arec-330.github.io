** Set working directory **
cd "/Users/alexandrahill/Library/CloudStorage/OneDrive-Colostate/AREC 705/Fall 2022/Lectures/Week 8/"

** import data
import delimited "dairy_dea.csv"

** install dea package
search dea
help dea

** verify correct CRS and VRS IO efficiencies

* need DMU variable
rename v1 dmu

* run VRS DEA IO (will also return CRS)
dea  llabor = ly , rts(vrs) saving(dairy_dea_stata.dta) ort(in)

** look at results
use dairy_dea_stata.dta, clear
