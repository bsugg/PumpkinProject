/*Libref established for clean Excel data file.*/
libname pumpData "C:/Users/suggb/Documents/ST542/Project/PumpkinProject/CleanData/Excel/";

/*Libref established to output clean SAS data file.*/
libname sendData "C:/Users/suggb/Documents/ST542/Project/PumpkinProject/CleanData/SAS/";

/*Import clean Excel file to save back to CleanData SAS directory for sharing.*/
PROC IMPORT OUT=sendData.spacingData
	DATAFILE="C:\Users\suggb\Documents\ST542\Project\PumpkinProject\CleanData\Excel\spacingData.xlsx"
  DBMS=xlsx REPLACE;
  SHEET="Sheet1"; 
  GETNAMES=YES;
RUN;

PROC SORT DATA=sendData.spacingData OUT=sendData.spacingData;
	BY year plot descending color pumpkinNum;
RUN;

PROC IMPORT OUT=sendData.nitrogenData
	DATAFILE="C:\Users\suggb\Documents\ST542\Project\PumpkinProject\CleanData\Excel\nitrogenData.xlsx"
  DBMS=xlsx REPLACE;
  SHEET="Sheet1"; 
  GETNAMES=YES;
RUN;

PROC SORT DATA=sendData.nitrogenData OUT=sendData.nitrogenData;
	BY year plot descending color pumpkinNum;
RUN;

PROC IMPORT OUT=sendData.leafData
	DATAFILE="C:\Users\suggb\Documents\ST542\Project\PumpkinProject\CleanData\Excel\leafData.xlsx"
  DBMS=xlsx REPLACE;
  SHEET="Sheet1"; 
  GETNAMES=YES;
RUN;

PROC SORT DATA=sendData.leafData OUT=sendData.leafData;
	BY year plot date;
RUN;

/*Optional proc to check data set structure.*/
/*
PROC CONTENTS DATA=sendData.nitrogenWeight;
RUN;
*/
