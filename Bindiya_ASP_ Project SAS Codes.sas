*************Project - Sas Codes ******************;

libname Telecom "C:\Users\16479\Desktop\Data Science\ASP-ADVANCE SAS PROGRAMMING\FROM CLASS\Project\From class";

Title'Telecom Customers Dataset';
DATA Telecom.nwf;
 INFILE 'C:\Users\16479\Desktop\Data Science\ASP-ADVANCE SAS PROGRAMMING\New_Wireless_Fixed.txt' DLM = ' ' TRUNCOVER;
 INPUT @1 Acctno $13. @ 15 Actdt MMDDYY10. @ 26 Deactdt MMDDYY10. @41 DeactReason $4. @53 GoodCredit 1. @62 RatePlan 1. @65 DealerType $2. @74 Age 2. @ 80 Province $2. Sales DOLLAR10.2 ;
 FORMAT Actdt Deactdt MMDDYY10. SALES DOLLAR10.2;
RUN;

proc print data = Telecom.nwf ( obs = 10) ;
run;

proc contents data = Telecom.nwf order = varnum; run;

***********************************************************************************************************************************************************************************

*Q1-
1.1 Explore and describe the dataset briefly. For example, is the acctno unique? What
is the number of accounts activated and deactivated? When are the earliest and
latest activation/deactivation dates available? And so on….;

*** Distinct account number;
Title ' Unique Customer Accounts';
proc sql;
	select count(*) as Total_Accounts,count(distinct Acctno) as Unique_Accounts from Telecom.nwf ;
quit;
*No Duplicated values for account number were found - Total distinct account = 102255;

Title 'Total Number of Activated Accounts';
proc sql;
	select count(Acctno) as Activated_Accounts
	from Telecom.nwf
	where actdt is not null and deactdt is null;
quit;


Title ' Total Number of deactivated Accounts';
proc sql;
	select count(Acctno) as DeActivated_Accounts
	from Telecom.nwf
	where deactdt is not null;
quit;


proc sql;
	select distinct deactreason from ds;
quit;

Title "Classifying customers based on their activation status";
Data Status_ActDeact;
    set Telecom.nwf;
    if not missing(Deactdt) then Status = 'Deactivated';
    else if not missing(Actdt) and missing(Deactdt) then Status = 'Activated';
    else Status = 'Unknown';
Run;
Proc Print data=Status_ActDeact(obs=20);
Run;
Proc Freq data=Status_ActDeact;
    tables status;
Run;

TITLE "UNIVARIATE ANALYSIS";
TITLE " PIE CHART SHOWING CLASSIFICATION OF STATUS";
PROC GCHART DATA= Status_ActDeact;
PIE3D Status /DISCRETE VALUE = INSIDE PERCENT=OUTSIDE;
RUN;
QUIT;

Goptions reset=All;

**When are the earliest and latest activation/deactivation dates available;

Title ' Latest and Earliest Activation and Deactivation Dates';
proc sql;
	select max(actdt)as latest_Activation_Date format =date9., 
		   max(deactdt) as latest_Deactivation_Date format=date9.,
	       min(actdt)as Earliest_Activation_Date format =date9., 
           min(deactdt) as Earliest_Deactivation_Date format=date9.
	from Telecom.nwf;
quit;
********************************************************************************************************************************************************************************
**1.2 What are the age and province distributions of active and deactivated customers?;

Title "Age and Province distributions of activated and deactivated customers";
Proc Freq data=Status_ActDeact nlevels;
    tables Status * Age / nocum  norow nocol;
    tables Status * Province / nocum ;
Run;

Title1 'Histogram and Density Curve of Age Variable';
Title2 'Status = Activated Customers';
Proc Sgplot data=Status_ActDeact;
	histogram Age/fillattrs=(color=lightblue)filltype=gradient;
	density Age;
	where Status = 'Activated';
Run;

Proc Sgplot data=Status_ActDeact;
	Title1 'Histogram and Density Curve of Age Variable';
	Title2 'Status = Deactivated Customers';
	histogram Age/fillattrs=(color=lightblue)filltype=gradient;
	density Age;
	where Status = 'Deactivated';
Run;

proc freq data = Status_ActDeact;
	table province;
run;

TITLE1 "CLUSTER BAR CHART ";
TITLE2 "COMPARISON BETWEEN ACTIVATED AND DEACTIVATED ACCOUNTS BY PROVINCE "; 
PROC SGPLOT DATA = Status_ActDeact;
VBAR PROVINCE / GROUP = STATUS GROUPDISPLAY=CLUSTER FILLTYPE = GRADIENT;
RUN;

Title1 ' Pie Chart ';
Title2 ' Distribution of Total accounts across Province';
proc gchart data = telecom.nwf;
pie3d province / discrete value = inside percent = outside ;
run;

TITLE1 ' GROUP BOX PLOT ';
TITLE2 'COMPARISON BETWEEN ACTIVATED AND DEACTIVATED ACCOUNTS BY AGE';
PROC SGPLOT DATA = Status_ActDeact;
VBOX AGE / GROUP = STATUS;
RUN;
*Active accounts and deactivate accounts are mainly concentrated in the ON province. 
 Among the provinces, the QC province with the smallest proportion of active accounts and deactivate accounts 
 The overall age is mainly concentrated in the middle-aged crowd;

****************************************************************************************************************************************************************************************

*1.3 Segment the customers based on age, province, and sales amount:
Sales segment: < $100, $100-$500, $500-$800, $800 and above.
Age segments: < 20, 21-40, 41-60, 60 and above;

proc format ;
	value Age_segm
		.		= 'Missing'
   low -< 21 	= '< 20'
	21 -< 41 	= '21-40'
	41 -< 61 	= '41-60'
	61 - High	= '60 and above';

	value Sales_segm
		     .   ='Missing'
	low  -< 100   = '< $100'
	100 -< 500  = '$100-$500'
	500 -< 800  = '$500-$800'
	800 - High  = '$800 and above';
Run;

Title ' Segmentation of the customers based on Age, province and Sales Amount ';
proc Tabulate data = Status_ActDeact;
	class Status Age Province Sales ;
	format Age Age_segm. Sales Sales_segm.;
	tables Status, Age*(n) Province*(n) Sales*(n);
run;
	
data Formatted_data;
set Status_ActDeact;
format sales sales_segm. age age_segm.;
run;

proc print data = Formatted_data ( obs=10);
run;

****************************************************************************************************************************************************************************************

*1.4. Statistical Analysis:
1) Calculate the tenure in days for each account and give its simple statistics.;

Title "Tenure in days for each account[From 01/20/1999 To 01/20/2001]";
Data Tenure_Days;
	set Status_ActDeact;
	if not missing(Deactdt) then Tenure = Deactdt - Actdt;
	else if not missing(Actdt) and missing(Deactdt) then Tenure = '20JAN2001'd - Actdt; 
Run;
Proc Print data=Tenure_Days (obs=20);
Run;

Proc Means data=Tenure_Days Missing Exclusive;
	var Tenure;
	class Status;
	output out=Tenure_stats mean=Mean median=Median min=Min max=Max p25=Q1 p75=Q3 std=StdDev;
Run;
Proc Print data=Tenure_stats;
Run;


TITLE1 ' GROUP BOX PLOT ';
TITLE2 'COMPARISON BETWEEN TENURE AND ACCOUNT STATUS';
PROC SGPLOT DATA = TENURE_DAYS;
VBOX TENURE / GROUP = STATUS;
RUN;

proc univariate data = Tenure_Days normal plot;
var tenure;
where status = 'Activated';
run;

*2) Calculate the number of accounts deactivated for each month.;
*A) Deactiavtion Count;
Data Tenure_Month;
	set Tenure_Days;
	if not missing(Deactdt) then Tenure_Mon = intck('month',Actdt,Deactdt);
	else if not missing(Actdt) and missing(Deactdt) then 
		Tenure_Mon = intck('month',Actdt,'21Jan2001'd); 
Run;
Proc Print data=Tenure_Month(obs=10);
Run;
Proc Freq data=Tenure_Month;
    tables Deactdt / out=Deact_MonthCounts;
	format Deactdt monyy7.;
Run;
Data Deact_MonthCounts;
	set Deact_MonthCounts;
	if _n_ ne 1;
Run;
Proc Print data=Deact_MonthCounts;
Run;


 *Creating table with Deactivation in the year of 1999 and 2000;
data Deact_1999 Deact_2000;
set Deact_monthcounts;
if year(deactdt)=1999 then output Deact_1999;
else if year(deactdt)=2000 then output Deact_2000;
run;
title;

proc print data = Deact_1999;run;
title 'Accounts Deactivated in 1999';

Proc print data = Deact_2000;run;
title ' Accounts Deactivated in 2000';
*Total accounts deactivated from 1999 till 2001;

proc sgplot data = Deact_Monthcounts;
series x = deactdt y = percent /markers;
xaxis display=(nolabel) valuesformat = monname3.;
xaxis label = 'Months';
yaxis label ='Percent of Accounts Deactivated';
title1 'Time Series plot';
title2 'Accounts deactivated from Jan 1999 to Jan 2001';

proc sgplot data = Deact_1999 ;
series x=deactdt y=percent / markers;
xaxis display=(nolabel) valuesformat=monname3.;
yaxis display =(nolabel);
yaxis label = 'Percent';
title 'Time Series Plot for 1999';
run;

proc sgplot data = Deact_2000;
series x=deactdt y = percent / markers;
xaxis display =(nolabel) valuesformat = monname3.;
yaxis display =(nolabel);
yaxis label = 'Percent';
title 'Time Series Plot for 2000';
run;


*3) Segment the account, first by account status “Active” and “Deactivated”, then by
Tenure: < 30 days, 31---60 days, 61 days--- one year, over one year. Report the
number of accounts of percent of all for each segment.;

Data MissingTenure;
	set Tenure_Month;
	where Tenure = .; 
Run;
Proc Print data=MissingTenure;
Run;
* The data set MISSINGTENURE has 0 observations and 12 variables, which means there are no missing values;


proc print data = Status_ActDeact (obs=10);run;

Title "Report the number of accounts as a percent of all for each segment";
Data Account_Segments;
	set Tenure_Month; 
	if Tenure =< 30 then Tenure_Segment = '< 30 days                            ';
	else if Tenure <= 60 then Tenure_Segment = '31 - 60 days';
	else if Tenure <= 365 then Tenure_Segment = '61 days - one year';
	else Tenure_Segment = 'over one year';
Run;

proc print data = Account_segments (obs=10);
run;

Title1 'Frequency Table';
Title2 'Status & Tenure Segment';
Proc Freq data=Account_Segments;
	table Status * Tenure_Segment / out=Segmented_Counts outpct  norow nocol;
Run;

proc sort data=Segmented_Counts;
  by descending Status descending Percent ;
run;

Proc Print data=Segmented_Counts noobs;
	var Status Tenure_Segment Count Percent;
Run;

title 'Cluster Bar Chart - Tenure Segment and Customer Account Status';
proc sgplot data = segmented_counts;
vbar tenure_segment/response = percent group= status groupdisplay=cluster filltype=gradient;
yaxis label = 'Percentage';
run;


**4) Test the general association between the tenure segments and “Good Credit” “RatePlan ” and “DealerType” */;
*Categorical v/s Categorical ;
*A)Tenure segment old
Title1 "Chi Square Test";
Title2 'Association between the tenure segments and “Good Credit” “RatePlan ” and “DealerType.';
Proc Freq data=Account_Segments;
	tables Tenure_Segment * GoodCredit / chisq expected norow nocol;
	tables Tenure_Segment * RatePlan / chisq expected norow nocol;
	tables Tenure_Segment * DealerType / chisq expected norow nocol;
Run;
* H0: There is no significant association between the Tenure segments and GoodCredit, RatePlan, DealerType.
  Ha: There is an association between Tenure segments and GoodCredit, RatePlan, DealerType.

  All the Chi-Square Probability values are <.05, indicating that we can reject this null hypothesis, 
  and conclude that there is an ASSOCIATION between the Tenure segments and GoodCredit, RatePlan, DealerType, 
  at 5% significance level;

*B)Tenure segment B
Title1 "Chi Square Test";
Title2 'Association between the tenure segments and “Good Credit” “RatePlan ” and “DealerType.';
Proc Freq data=Tenure_Segments_opt;
	tables Tenure_Segment_B * GoodCredit / chisq expected norow nocol;
	tables Tenure_Segment_B * RatePlan / chisq expected norow nocol;
	tables Tenure_Segment_B * DealerType / chisq expected norow nocol;
Run;
/*==========================================================================================================*/


/* 5) Is there any association between the account status and the tenure segments?
Could you find out a better tenure segmentation strategy that is more associated with the account status? */
*Categorical v/s Categorical ;

/* Better Tenure Segmentation */
Title "New tenure segmentation strategy";
Data Tenure_Segments_Opt;
	set Formatt_Segments;
	if Tenure = 0 then Tenure_Segment_B = '0 days                            ';
	else if Tenure =< 7 then Tenure_Segment_B = '1 - 7 days';
	else if Tenure =< 30 then Tenure_Segment_B = '8 - 30 days';
	else if Tenure <= 90 then Tenure_Segment_B = '31 - 90 days';
	else if Tenure <= 180 then Tenure_Segment_B = '91 - 180 days';
	else if Tenure <= 365 then Tenure_Segment_B = '181 days - one year';
	else Tenure_Segment_B = 'over one year';
Run;

proc sort data=Tenure_Segments_opt;
    by Tenure_Segment_B;
run;

proc print data = tenure_segments_opt(obs=10);run;

Title "Chi Square Test - Association between the account status and the tenure segments";
Proc Freq data=Tenure_Segments_opt;
	tables Status * Tenure_Segment_B / chisq expected norow nocol;
Run;
* H0: There is no significant association between the account status and Tenure segments.
  Ha: There is an assocoation between Tenure Segments and GoodCredit, RatePlan, DealerType.

  All the Chi-Square Probability are <.05, which indicates that we can reject this null hypothesis, 
  and conclude that there is an ASSOCIATION between the account status and Tenure segments at 5% significance level.;


Proc Sgplot data=Tenure_Segments_Opt;
	vbar Tenure_Segment_B /  stat=sum group=Status 
		groupdisplay=cluster filltype = gradient ;
	xaxis display=(nolabel);
	yaxis grid;
Run;

Title 'Tenure segmentation by old strategy';
proc freq data = formatt_segments;
tables status*Tenure_segment/ outpct nocum norow;
run;

Title 'Tenure segmentation by Old Strategy';
proc sgplot data = formatt_segments;
vbar Tenure_segment / stat=sum group=status groupdisplay = cluster filltype = gradient;
xaxis display = (nolabel);
yaxis grid;
run;

*6) Does the Sales amount differ among different account statuses, GoodCredit, and
customer age segments?;


Title "Segmenting Sales and Age variables";
Data Formatt_Segments;
	set Account_Segments;
	Sales_Segment = put(Sales, Sales_segm.);
	Age_Segment = put(Age, Age_segm.);
Run;
Proc Print data=Formatt_Segments (obs=10);
Run;
Proc Freq data=Formatt_Segments;
	table Sales_Segment;
Run;

** ( A ) Sales and Account Status;
*Continuous v/s Categorical;

*1) Descriptive Statistics ;
proc means data=formatt_Segments nmiss mean std stderr cv lclm uclm median min max qrange maxdec=2;
var Sales;
class Status;
run; 

*2) Test of Normailty ;
TITLE "Test of Normality";
PROC UNIVARIATE DATA = FORMATT_SEGMENTS NORMAL PLOT;
CLASS STATUS;
VAR SALES;
RUN;

*The p value is less than .05 and indicates the data is not normally distributed , but as per CLT if the number of observations are above 30, the data should be considered as normal.
Hence , we will perform "Equality of Variance" Test;

*3) Equality Of Variance Test;
title;
Title ' Test of "Equality of Variance"';
Proc GLM data=Formatt_Segments;
	class Status;
	model Sales = Status;
	means Status / hovtest=levene(type=abs) welch;
Run;


* H0: Equality of variance between the groups
  Ha: No Equality of variance between the groups
  The Levene's Test p-value= 0.0505, higher than 0.05, which indicates that fail to reject the Null Hypotheses,
  There is a Equlity of variance between the customers holding Activation and Deactivation Status
  To support our findings we will perform T Test;

*4) T Test ;
proc ttest data=Formatt_Segments;
var Sales;
class Status;
run;

* H0: The mean sales for different account statuses are equal.
  Ha: There is a significant difference in mean sales between different account statuses.
  The p-value= 0.3963, higher than 0.05, which indicates that we can not reject the Null Hypotheses
  and conclude that there is no significant difference in mean sales between the categories of account status at 5% significance level;

TITLE1 'GROUP BOX PLOT';
TITLE2 'Sales by Account Status';
PROC CORR DATA = Formatt_Segments plots= Matrix(histogram) ;
Var Age;
with Sales;
RUN;

*( B ) Sales and GoodCredit;

*1) Descriptitve Statistics;
proc means data=formatt_Segments nmiss mean std stderr cv lclm uclm median min max qrange maxdec=2;
var Sales;
class GoodCredit;
run; 

*2)Test of Normailty;
TITLE "Test of Normality";
PROC UNIVARIATE DATA = FORMATT_SEGMENTS NORMAL PLOT;
CLASS GoodCredit;
VAR SALES;
RUN;
*As the number of observations are above 30 , hence we will consider the data as normally distributed .

*3) Equality of Variance Test;
Proc GLM data=Formatt_Segments;
	class GoodCredit;
	model Sales = GoodCredit;
	means GoodCredit / hovtest=levene(type=abs) welch;
Run;
* H0: There is Equality of Variance between the groups
  Ha: There is no Equality of Variance between the groups

  The Levene's Test p-value= 0.6795, higher than 0.05, which indicates that we fail to reject the Null Hypotheses,
  we haven't found strong evidence to conclude that there is no significant difference in the variance of the groups ;

*4) Pooled TTest;
proc ttest data = Formatt_Segments;
var Sales;
class GoodCredit;
run;
*H0: The mean sales for different groups of Goodcredit are equal.
 Ha: There is significant difference in mean sales between different groups of GoodCredit.

 The p-value= 0.7788, higher than 0.05, which indicates that we can not reject the Null Hypotheses
 and conclude that there is no significant difference in mean sales between the groups of GoodCredit at 5% significance level;

Title1 'Box Plot';
Title2 'Sales by GoodCrdeit';
PROC SGPLOT DATA = Formatt_Segments ;
VBOX Sales / GROUP = GoodCredit;
RUN;

*( C ) Sales and Age segments;

*1) Descriptive Statistics;
proc means data=formatt_Segments nmiss mean std stderr cv lclm uclm median min max qrange maxdec=2;
var Sales;
class Age_segment;
run; 

*2) Test of Normality;
TITLE "Test of Normality";
PROC UNIVARIATE DATA = FORMATT_SEGMENTS NORMAL PLOT;
CLASS Age_Segment;
VAR SALES;
RUN;
* The number of observations are above 30, we will consider the data as notmally distributed.

*3)Equality of Variance Test;
 Proc GLM data=Formatt_Segments;
	class Age_Segment;
	model Sales = Age_Segment;
	means Age_Segment / hovtest=levene(type=abs) welch;
Run;
* H0: There is Equality of Variance between different Age Segments.
  Ha: There is no Equality of Variance between different Age Segments.
  The Levene's Test p-value= 0.2328, higher than 0.05, which indicates that we can not reject the Null Hypotheses,
  as we haven't found strong evidence to conclude that there is no significant difference in Variance among different age segments we will perform One Way ANOVA test;

*4) One Way ANOVA test;
title;
Proc ANOVA data=Formatt_Segments;
	class Age_Segment;
	model Sales = Age_Segment;
	means Age_Segment;
Run;
Quit;
* H0: There is no difference between the mean sales of age segment groups.
  Ha: The mean sales of atleast one Age segment group differs.
  The p-value= 0.7583, higher than 0.05, which indicates that we fail to reject the Null Hypotheses at 5% significance level
  and conclude that there is no difference between the mean sales of age segment groups;

Title1 'Group Box Plot';
Title2 'Sales by Age_segments';
PROC SGPLOT DATA = Formatt_Segments ;
VBOX Sales / GROUP = Age_Segment;
RUN;


* Sales and Status, GoodCredit, Age segments;
Proc GLM data=Formatt_Segments;
	class Status GoodCredit Age_Segment;
	model Sales = Status GoodCredit Age_Segment;
	means Status GoodCredit Age_Segment / hovtest=levene(type=abs) welch;
Run;
* H0: There is no significant difference in the sales amount among different account status, GoodCredit, and age segments.
  All the p-values are higher than 0.05, which indicates that we can not reject the Null Hypotheses,
  we haven't found strong evidence to conclude that there are significant differences 
  in sales amount among different account statuses, GoodCredit, and age segments.;
Proc ANOVA data=Formatt_Segments;
	class Status GoodCredit Age_Segment;
	model Sales = Status GoodCredit Age_Segment;
	means Status GoodCredit Age_Segment;
Run;
Quit;
* H0: There is no significant association between Sales and account status, GoodCredit, or age segments.
  All the p-values are higher than 0.05, which indicates that we can not reject the Null Hypotheses
  and conclude that there is no association between the Sales and account status, GoodCredit, and age segments.;

*LINEAR REGRESSION -;

PROC CORR DATA = formatt_segments   plots=matrix(histogram);
 VAR Sales age ;
RUN;

proc reg data=formatt_segments;
model Sales=age ;*Hint: in Model statement we must put target variable(response)  in the left side of
						equal sign and put predictor(s)on the right side of equal sign; 
run;

data Reg1;
set formatt_segments;
predicted_s=180.54 + 0.01*(age);
error=sales-predicted_s;
SE=error**2;
run;
proc print data=reg1(obs=10); run;
proc means data=reg12 sum;
var error SE;
run;
proc means data=formatt_segments mean;
var sales;
run;

proc contents data = formatt_segments order = varnum;run;

*UNIVARIATE ANALYSIS:

(A) CATEGORICAL ANALYSIS;

Title "Exploring Data: Categorical Variables";
Proc Freq data=Formatt_Segments nlevels;
	tables DeactReason GoodCredit RatePlan DealerType Province ;
Run;

Title 'Frequency table of Tenure_segment(old)';
proc freq data = formatt_segments nlevels;
	tables tenure_segment;
run;

proc sgplot data=Formatt_Segments;
title "Bar Chart of DeActivation Reason ";
vbar DeactReason/GROUP = DeactReason filltype=GRADIANT datalabel;
yaxis grid;
run;

proc sgplot data=Formatt_Segments;
title "Bar Chart of Good Credit";
vbar GoodCredit/Group = GoodCredit filltype=Gradiant datalabel;
yaxis grid;
run;

proc sgplot data=Formatt_Segments;
title "Bar Chart of Rate Plan ";
vbar Rateplan / Group = Rateplan Filltype = Gradiant datalabel;
yaxis grid;
run;

proc sgplot data=Formatt_Segments;
title "Bar Chart of Dearler Type ";
vbar DealerType / Group = DealerType Filltype = Gradiant datalabel;
yaxis grid;
run;

proc sgplot data = Formatt_Segments;
title 'Bar Chart of Province';
vbar Province/Group = province filltype = Gradiant datalabel;
yaxis grid;
run;

proc sgplot data = Formatt_Segments;
title ' Bar Chart of Tenure_Segments';
vbar Tenure_Segment/ Group = Tenure_Segment filltype = Gradiant datalabel;
yaxis grid;
run;

*( B ) CONTINUOUS VARIABLE ;

Title "Exploring Data: Age Variable";
Proc Means data=Formatt_Segments;
	var Age ;
	class status;
Run;


Proc Univariate data=Formatt_Segments normal;
    var Age Sales;
    histogram Age Sales / normal;
Run;

**OUTLIERS in AGE **;

Title "Outliers for AGE";
Proc Sgplot data=Formatt_Segments;
	vbox AGE / CATEGORY = STATUS;
Run;

title 'Five Number Summary - AGE' ;
proc means data=Formatt_Segments N NMISS MIN Q1 MEDIAN Q3 MAX MAXDEC=2;
  var AGE;
RUN;

Title "Outliers for Sales";
Proc Sgplot data=Formatt_Segments;
	vbox Sales/CATEGORY=STATUS;
Run;

TITLE "Computing Four Measures of Association";
PROC CORR DATA = FORMATT_SEGMENTS pearson spearman kendall hoeffding
          plots=matrix(histogram);
 VAR age sales;
RUN;

proc options option = macro;run;

/* Analysis: Association between CATEGORICAL variables - CHI SQUARE TEST */
%macro Association(d1 ,var, class);
    Proc Freq data=&d1;
        table &var*&class/ chisq expected norow nocol;
    Run;
    proc sgplot data=&d1;
        vbar &var / group=&class groupdisplay=stack;
    Run;
%mend Association;

*Association of Sales_segment with other categorical variables ;
%Association(Formatt_segments,Sales_segment, Province);
%Association(Formatt_segments,Sales_segment, Status);
%Association(Formatt_segments,Sales_segment, GoodCredit);
%Association(Formatt_segments,Sales_segment, DealerType);
%Association(Formatt_segments,Sales_segment, RatePlan);

*Association of Age_segments with other categorical variable ;
%Association(Formatt_segments,Age_segment, Province);
%Association(Formatt_segments,Age_segment, Status);
%Association(Formatt_segments,Age_segment, GoodCredit);
%Association(Formatt_segments,Age_segment, RatePlan);
%Association(Formatt_segments,Age_segment, DealerType);
%Association(Formatt_segments,Age_segment, Sales_segment);

*Assoaciation of Tenure_segment with categorical variables ;
Title'Association of Tenure_segment with Goodcredit , Rateplan and Dealertype';
%Association(Formatt_segments,Tenure_Segment, Goodcredit);
%Association(Formatt_segments,Tenure_Segment, Rateplan);
%Association(Formatt_segments,Tenure_Segment, dealertype);

%Association(Formatt_segments,Tenure_Segment, Province);
%Association(Formatt_segments,Tenure_Segment, Age_segment);
%Association(Formatt_segments,Tenure_Segment, Sales_segment);

*Association of Tenure_segment_B(bettertenure segmentation) with categorical variables ;
%Association(Tenure_segments_opt,Tenure_Segment_B, Goodcredit);
%Association(Tenure_segments_opt,Tenure_Segment_B, Rateplan);
%Association(Tenure_segments_opt,Tenure_Segment_B, dealertype);

****************************************************************************THANK YOU******************************************************************************************************
