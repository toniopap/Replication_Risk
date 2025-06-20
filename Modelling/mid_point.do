clear all
cap cd /Users/tonio/Documents/GitHub/Replication_Risk/Modelling
cap cd "C:\Users\tonio\OneDrive\Documenti\GitHub\Replication_Risk\Modelling"
*#I would choose Lottery A from Row 1 to Row
import delimited "t_table.csv"
drop v1
save t_table , replace

clear all
import delimited Italy_results.csv
replace v1 = "0" if v1 == "0 I would never choose lottery A"
replace v2 = "0" if v2 == "0 I would never choose lottery A"
replace v3 = "0" if v3 == "0 I would never choose lottery A"

replace v1 = "12" if v1 == "12 (always lottery A)"
replace v2 = "14" if v2 == "14 (always lottery A)"
replace v3 = "7" if v3 == "7 (always lottery A)"

destring v1 v2 v3, replace
drop if v1 == . | v2 == . | v3 == .
ren v1 s1point
ren v2 s2point
ren v3 s3point

sort s1point s2point s3point 
merge m:1 s1point s2point s3point using t_table
drop if _merge != 3
drop _merge
su
kdensity lambda
