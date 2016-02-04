
R version : 3.2.2
RStudio Version: 0.99.484

The following packages should be installed to run the programs
e1071
caret
RWeka 
class


1.Open RStudio and Set working directory to the current directory where the files and data are present.

2. Install the required packages.

3. For pattern recognition
	- Open pattern.R 
	- Select the total code using Control+A and Run using Control+R
	- The graphs generated are exported as .png files into the working directory
		Note: Add ".png" extension to the filenames which doesn't contain it in order to view the files.
4. For predictive analysis
	- Open svm.R 
	- Select the total code using Control+A and Run using Control+R
 A performance table of SVM is displayed in the console at the end of program execution.
	- Open decisiontrees.R 
	- Select the total code using Control+A and Run using Control+R
 A performance table of Decision Trees is displayed in the console at the end of program 
	- Open naive.R 
	- Select the total code using Control+A and Run using Control+R
 A performance table of Naive Bayes is displayed in the console at the end of program execution.

Note: The performance table contains "Baseline" values. These values are generated in "svm.R" . Since the dataset used 
is same in svm.R , decisiontrees.R, naive.R , calculating these values in each of them seemed to be redundant. 
So for getting proper results on predictive analysis, it is mandatory to execute svm.R before executing decisiontrees.R/naive.R .