clear all
cap cd /Users/tonio/Documents/GitHub/Replication_Risk/Modelling
cap cd "C:\Users\tonio\OneDrive\Documenti\GitHub\Replication_Risk\Modelling"
import delimited Italy_results.csv
replace v1 = "0" if v1 == "0 I would never choose lottery A"
replace v2 = "0" if v2 == "0 I would never choose lottery A"
replace v3 = "0" if v3 == "0 I would never choose lottery A"

replace v1 = "12" if v1 == "12 (always lottery A)"
replace v2 = "14" if v2 == "14 (always lottery A)"
replace v3 = "7" if v3 == "7 (always lottery A)"

destring v1 v2 v3, replace
drop if v1 == . | v2 == . | v3 == .

//Calculate the switching points
