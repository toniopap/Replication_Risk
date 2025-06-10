clear all
cap cd /Users/tonio/Documents/GitHub/Replication_Risk/Modelling
cap cd "C:\Users\tonio\OneDrive\Documenti\GitHub\Replication_Risk\Modelling"
*#I would choose Lottery A from Row 1 to Row
import excel "Replication Risk (Risposte).xlsx", sheet("Risposte del modulo 1") firstrow
rename Series1Pleasecarefullyconsid s1point // rename variables
rename Series2Pleasecarefullyconsid s2point
rename Series3Pleasecarefullyconsid s3point

replace s1point = "0" if s1point == "0 I would never choose lottery A"
replace s2point = "0" if s2point == "0 I would never choose lottery A"
replace s3point = "0" if s3point == "0 I would never choose lottery A"

replace s1point = "12" if s1point == "12 (always lottery A)"
replace s2point = "14" if s2point == "14 (always lottery A)"
replace s3point = "7" if s3point == "7 (always lottery A)"

destring s* , replace
sort s1point s2point s3point
merge 1:m s1point s2point s3point using t_table
drop if _merge != 3
