\docType{data}
\encoding{utf8}
\name{HighSchoolEligibility}
\alias{HighSchoolEligibility}
\title{High school eligibility}
\format{A table with 4 variables, to be queried with the SCB() function.
\describe{
\item{Municipality}{Also known as: Municipality, Kommun.
Municipality the observation is from.}
\item{Gender}{Also known as: Gender, Kön.
A column of factor data. Its levels are: Pojkar (a.k.a.: Men, Boys, Män, Male.), Flickor (a.k.a.: Girls, Women, Kvinnor, Female.), Samtliga (a.k.a.: Totalt, Total.).}
\item{SchoolOrganizer}{Also known as: SchoolOrganiser, Huvudman.
A column of factor data. Its levels are: Kommunal (a.k.a.: Municipal.), Enskild (a.k.a.: Private.), Samtliga (a.k.a.: Totalt, Total.).}
\item{HighSchoolEligibility}{Also known as: Students, HSEligibility, Gymnasiebehörighet.
A column of factor data. Its levels are: Eligible (a.k.a.: Gymnasiebehöriga, True, Behöriga.), Ineligible (a.k.a.: Gymnasieobehöriga, False, Obehöriga.), Total (a.k.a.: Samtliga, All.).}
}}
\description{A table containing data on how many students graduated ninth grade, and what proportion were eligible for high school, divided by gender and organizational form (municipal or private). The original data reported total number of students and percentage ineligible for any high school program, thus the non-integer numbers of students. See the data source for more detail. Note that values less than ten for total number of students are rounded to zero, and if there are less than five ineligible students, the percentage of ineligible students was rounded to 0.}
\keyword{datasets}
