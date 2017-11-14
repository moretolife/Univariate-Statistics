#' ---
#' title: "UNIVARIATE STATISTICS TERMINOLOGIES"
#' author: "Executed by Neha More"
#' date: "Nov 3rd, 2017"
#' ---

#'This mini project involves learning the basic terminologies of univariate statistics in R.
#'It involves the following:
#'1.Analysing and finding mean,median,mode,sd,quatile ranges of a dataset.
#'2.Loading library psych and learning its functionality for storing statistical summaries in data frames.
#'3.Plotting boxplots and histograms.
#'4.Summarising categorical data,using frequencies.
#'Here we use arthritis dataset and analyse the dataset using terms like crosstables,xtabs,collapsing tables and adding margins,etc.
#'5.Assignment: To tell whether automatic transition results in better mileage of the cars or no.(using data set mtcars)
#'6.Assignment: To check whetehr age impacts the arthritis patients treatement.ie "treated" people.(using arthritis dataset)
#'

##-----------------------------------------------------------------------------------
#'--------------------------------------------------------------------------------
#'
#'**1. lets load dataset mtcars consisting of 32 obs and 11 variables.**
data("mtcars")
View(mtcars)

#---------------
#'various stat functions are as follows:
mean(mtcars$mpg) #gives the mean of variable(column) mpg
median(mtcars$mpg) #gives the median of variable(column) mpg
min(mtcars$mpg)#gives the min value of variable(column) mpg
max(mtcars$mpg)#gives the mex value of variable(column) mpg
var(mtcars$mpg)#gives the variance of variable(column) mpg
sd(mtcars$mpg)#gives the sd of variable(column) mpg
IQR(mtcars$mpg)#gives the Inter quartile range of variable(column) mpg

summary(mtcars) #summarises data for all variables together giving its statistical values.
#'---------------------------------------------------------------------------------------
s=summary(mtcars$mpg) #stores the summary of column mpg in table format in s.
class(s)
names(s) #gives the names of table data
#'we can access the values stored in s by following command:
s["Max."] #to access the maximum value in s
s["1st Qu."] #to access the first quatile value in s
#'----------------------------------------------------------------------------------------
ss=summary(mtcars) #similarly we can store entire summary and access individual values.
class(ss)
names(ss)

#----------
#'how to access value of data in ss
ss[1,2] #1st row n 2nd col::cyl column min data
ss[5,6] #5th row n 6th col:: 3rd quartile of wt variable.

#-------------------------------------
#'**2. Loading library psych..it is used to store summary data in different manner using describe function.**
library(psych)
describe(mtcars) #gives detailed statistical measurments of all column variables.
#'kurtosis above means measure of peakness.
#-----------------------------------
#'store it in object
sss=describe(mtcars)
class(sss)  #class will be DF wic was table earlier
View(sss)
View(ss) #note the difference.earlier "ss" was a table .This "sss" is a DF
#------------------
#'accessing values within df
rownames(sss) #gives all the column headers(ie variable names)
colnames(sss) #gives all the statistical measurement names.
#'lets acces the values in the above dataframe.
#'
sss["gear","max"] #gear is row n max is col

sss[c("gear","mpg","wt"),c("max","sd","skew")] #multiple elements access

#'--------------------------------------------------------------------------
#'**3. Plotting boxplots,histograms**
#'boxplot is made as follows:
#+ fig.width=10, fig.height=7
boxplot(mtcars)
boxplot(mtcars$mpg)
boxplot(mtcars$hp)

#'plotting histograms:

hist(mtcars$vs)
hist(mtcars$mpg,breaks=10) #do not use breaks if data is less
#'------------------------------------------------------
#'lets generate random normal distributuion data set(x) and plot histogram
x=rnorm(10000) 
hist(x)
hist(x,breaks=100)

##'-------------------------------------------------------------------
#'**4. lets summarise categorical data,using frequencies.**
#'loading library vcd and grid and dataset arthritis which consists of 85obs and 5 variables.
#'Arthritis dataset consists Data from Koch \& Edwards (1988) from a double-blind clinical trial investigating a new treatment for rheumatoid arthritis.
#'Format of dataset is as follows:
#'ID:patient ID.
#'Treatment:factor indicating treatment (Placebo, Treated).
#'Sex:factor indicating sex (Female, Male).
#'Age:age of patient.
#'Improved:ordered factor indicating treatment outcome (None, Some, Marked).
#'------------------------------


library(vcd)
library(grid)
data("Arthritis")
View(Arthritis)
#------------------
#'lets Summarise it
summary(Arthritis)

describe(Arthritis)
tab=table(Arthritis$Improved) #storing count of values of variable improved in tab.
tab
#--------------------------
#'prop.table converts into proportion ie percentages
t=round(prop.table(tab)*100,2)
t
#'output shows that out of 84 patients 50%shows no improvemnt,16.67% has some improvement and 33.33% has marked improvement.
#----------------------------------------------
#'lets create more dimension table ie cross tables
tab=table(Arthritis$Improved,Arthritis$Sex)
tab
class(Arthritis) #its a df
#'another way to create cross table is xtabs
xtabs(~Improved+Sex,data=Arthritis)
xtabs(~Improved+Treatment+Sex,data=Arthritis)

##------------------------------------------------
#'the diff is just the syntax btw xtabs ntable function.o/p is the same.
#'
#'**lets create a two way table btw treatemnt and improved for further analysis.**
tab=table(Arthritis$Treatment,Arthritis$Improved)
tab
#'**output:we can say that placebo received patients had no improvmnet & treated patients had marked improvmtn.**
#'but instead of values lets convert into percentages. by using prop.table
round(prop.table(tab)*100,2)
#'NOTE:but this percentage is overall.ie 34% is outof 84 entries n not out of people who received placebo.
#'i need % across placebo sum n across treatemnt sum
#'hence i write 1 for first value ie treatment n 2 across improvemnt.
final=round(prop.table(tab,1)*100,2) #gives percentage across treatment.ie row % will add upto 100%
final
addmargins(final,2)
#'**CONCLUSION:
#'67% WHO RECEIVED PLACEBO HAD NO IMPROVMNT N 51% WHO WERE TREATED SHOWED IMPROVEMNT.
#'HENCE DOING TREATEMENT  IS BETTER THAN giving PLACEBO to arthritis patients..**
#'
#'lets check summary across improvement
final1=round(prop.table(tab,2)*100,2) #Gives % across improved.ie columns will add upto 100%.
addmargins(final1,1)

#'lets see how to collapse table
tab
margin.table(tab) #collapses the table n says it has 84 observations ie rows
margin.table(tab,1)
margin.table(tab,2)

#'lets add margins
tab=table(Arthritis$Treatment,Arthritis$Improved)
tab
addmargins(tab)
addmargins(tab,1) #nos.across treatment are added
addmargins(tab,2) #nos across improved are added.

#'
#'lets see a three dimensional example.
tab3=xtabs(~Treatment+Sex+Improved,data=Arthritis)
tab3
ftable(tab3) #displays better way
#=======================
margin.table(tab3)
margin.table(tab3,1)
margin.table(tab3,c(1,3))
margin.table(tab3,c(1,2))
#-------------------------------------
ftable(prop.table(tab3,c(1,3))*100) #gives percentages in dimension of treatemtn n improved.ie.placebo and none will add upto 100%
#also treated and marked will add upto 100%

#---------------------------------------------------
ftable(addmargins(prop.table(tab3,c(1,2)),3)) # 3 is for improved.so improved is summation.
#'**CONCLUSION:
#'59 % of femals who were treated had marked improvemnt
#'90% of males who were on placebo n 50 % who were treated had no improvment
#'hence we can say that the drug has more impact on females dan males.**

##-----------------------------------------------------------------------------------
#'--------------------------------------------------------------------------------
#'
#'**e.g of mtcars
#'5. QUESTION: To tell whether automatic transition results in better mileage of the cars or no.**
#'lets load data mtcars
data(mtcars)
View(mtcars)
#'lets create cross table of mileage n automation
mycars=mtcars[,c("mpg","am")]
mycars
xtabs(~am+mpg,data=mycars) #bad idea bcz we cannot retrive any information from this.bcz frequency value is extreme less.

mean(mycars$mpg)
#-----------------
#'lets subset data acc to am value 0 & 1
mycars.am.0=mycars[mycars$am==0,]
mycars.am.0
mycars.am.1=mycars[mycars$am==1,]

#'now lets find mean of both wen am =0 n am=1
mean(mycars.am.0$mpg)
mean(mycars.am.1$mpg)
#'**CONCLUSION:hence average mileage is better(mpg) wehn transmision is automatic.(am=1)**

##-----------------------------------------------------------------------------------
#'--------------------------------------------------------------------------------
#'
#'**QUESTION:To check whetehr age impacts the arthritis patients treatement.ie "treated" people.**

myarth=Arthritis[Arthritis$Treatment=="Treated",] #we subset people who were treated in object myarth.

none.improved=myarth[myarth$Improved=="None",] #subsetted none improved
some.improved=myarth[myarth$Improved=="Some",] #subsetted none improved
marked.improved=myarth[myarth$Improved=="Marked",] #subsetted none improved

mean(none.improved$Age)
mean(some.improved$Age)
mean(marked.improved$Age)

#'**CONCLUSION:IT SHOWS THAT WHEN D AGE OF PATIENTS IS ON THE HIGHER SIDE THEN MEDICINE WORKS BETTER.
#'OLDER PEOPLE MEDICINES WORKS BETTER N R TREATED BETTER.
#'BUSSINESS UNDERSTNADING IS THAT OLDER PEOPLE HAVE MAJOR SYMPTOMS HENCE THE RESULT OF TREATMENT IS ALSO MUCH PRONOUNCED ON THEM DEN YOUNGER PEOPLE.**
#'
#'                                                **FINISH**