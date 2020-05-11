/*
 * This program will read the output file from running klee and summarize the
 * results in a table
 * Fileline     Number of Invalid    Number of Divide by Zero  Number of Overflow  Number of Underflow
 */

#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include <vector>
#include <string>
#include <sstream>
#include <iostream>
#include <sys/stat.h>
#include <sys/statfs.h>
#include <unistd.h>
#include <stdlib.h>
#include <vector>
#include <map>
#include <set>
#include <fstream>
#include <iomanip>

using namespace std;

//Declare global statistic variables
// The index implies the bound used
// 0 -> 1
// 1 -> 2
// 2 -> 4
// 3 -> 8
// 4 -> 16
// 5 -> 32
// 6 -> 64
// 7 -> infinity
vector <double> ratiosList[8]; // list of exploration ratios.
int solvedPaths[8]; // Number of solved paths
int SAT[8]; // Number of SAT paths
int UNSAT[8]; // Number of UNSAT paths
int UNKNOWN[8]; // Number of UNKNOWN paths
int completedPaths[8]; // Number of completed paths
int exceptionFree[8]; // Number of exception-free functions per loop bound
int exceptions[8]; // Number of exceptions per loop bound
int new_exceptions[8]; // Number of new exceptions per loop bound
int discharged[8]; // Number of discharged functions
int Z3_queries[8]; // Number of Z3_queries
int Z3_sat_queries[8]; // Number of Z3 sat queries
int Z3_unsat_queries[8]; // Number of Z3 unsat queries
int poly_queries[8]; // Number of poly queries
int poly_sat_queries[8]; // Number of sat queries
int poly_sat_single_queries[8]; // Number of sat single queries
int poly_sat_multiple_queries[8]; //Number of sat multiple-variance queries
int poly_unsat_queries[8]; // Number of unsat queries
int poly_unsat_single_queries[8]; // Number of unsat single queries
int poly_unsat_multiple_queries[8]; // Number of unsat multiple queries
vector <double> timeList[8]; // list of running time
map < string, set <string> > familyList;
map < string, set <int> > invalidList;
map < string, set <int> > divideByZeroList;
set< string > exceptionSet[8];
int newExceptionNum[8];
int cacheHits[8];
int cacheMisses[8];

string boundList[8] = {
    "1", "2", "4", "8", "16", "32", "64", "infinity"
};

int functionNum; // total number of functions
//int handledFunctionNum; //total number of handled functions
int handledFunctionNum[8]; // list of number of handled functions

double totalTime[8];
double meanTime[8];
	
int TOTAL_FUNCTION = 480;

set <string> exceptionList;

// List of output csv files
ofstream reportFile("report.csv");
ofstream pathInfoFile("pathInfo.csv");
ofstream reportTexFile("report.tex");
ofstream performanceFile("performance.tex");
ofstream summaryFile("summary.csv");
ofstream exceptionFile("exceptionCount.tex");
ofstream partitionFile("partition.csv");
ofstream timeFile("time.csv");
ofstream percentageFile("percentage.csv");
ofstream divideByZeroFile("divideByZero.csv");
ofstream invalidFile("invalid.csv");

int numOfOverflow = 0;
int numOfUnderflow = 0;
int numOfInvalid = 0;
int numOfDivideByZero = 0;
int newExceptions = 0;

int totalOverflow = 0;
int totalUnderflow = 0;
int totalInvalid = 0;
int totalDivideByZero = 0;

// List of messages needed to process in output files.
string overflowMsg("Potential Overflow ");
string underflowMsg("Potential Underflow ");
string invalidMsg("Potential Invalid ");
string divideByZeroMsg("Potential Division By Zero ");
string inputString("arr");
string summaryMsg ("KLEE: done:");
	
// Timeout messages
string haltMsg("KLEE: HaltTimer invoked");
string timeoutMsg("KLEE: WATCHDOG: time expired");

// The number of exceptions when the program timeout
int timeoutExceptions[8];

double MAXTIMEOUT = 300.0;

double maxTimeOut = 0;

int numOfMaxTimeOut = 0;

void processOutFiles( string );

// Replace a string in a string by another one.
void replace(string &str, string searchString, string replaceString) {
	string::size_type pos = 0;
	while ( (pos = str.find(searchString, pos)) != string::npos ) {
		str.replace( pos, searchString.size(), replaceString );
		pos++;
	}
}

int getStat(ifstream &file, string msg) {
  string line;
  getline(file,line);
  replace (line, msg,"");
  istringstream Iss(line);
  int value;
  Iss >> value;
  std::cout << msg << "\t" << value << std::endl; // debugging
  return value;
}

int main(int argc, char* argv[]) {	
	
	/// Initialize global variables
	for (int i=0; i< 8; i++) {
	    handledFunctionNum[i] = 0;
	    solvedPaths[i] = 0;
	    completedPaths[i] = 0;
	    exceptionFree[i] = 0;
	    discharged[i] = 0;
	    exceptionFree[i] = 0;
	    exceptions[i] = 0;
	    new_exceptions[i] = 0;
	    SAT[i] = 0;
	    UNSAT[i] = 0;
	    UNKNOWN[i] = 0;
	    Z3_queries[i] = 0;
	    Z3_sat_queries[i] = 0;
	    Z3_unsat_queries[i] = 0;
	    poly_queries[i] = 0;
	    poly_sat_queries[i] = 0;
	    poly_sat_single_queries[i] = 0;
	    poly_sat_multiple_queries[i] = 0;
	    poly_unsat_queries[i] = 0;
	    poly_unsat_single_queries[i] = 0;
	    poly_unsat_multiple_queries[i] = 0;
	    newExceptionNum[i] = 0;
	    timeoutExceptions[i] = 0;
	    cacheHits[i] = 0;
	    cacheMisses[i] = 0;
	}

	/// PRINT OUT THE RESULTS
	// Write the fist line to out file
	summaryFile << "Filename;Overflows;Underflows;Invalid;DivisionByZero\n";
	invalidFile << "Function;Index\n";
	divideByZeroFile << "Function;Index\n";
	percentageFile << "Bound;Percentage\n";
	
	/// Process the output files
	processOutFiles("Infinity");
	processOutFiles("1");
	processOutFiles("2");
	processOutFiles("4");
	processOutFiles("8");
	processOutFiles("16");
	processOutFiles("32");
	processOutFiles("64");
	
	exceptionFile << totalOverflow << " & " << totalUnderflow << " & ";
	exceptionFile << totalInvalid << " & " << totalDivideByZero << "\\\\\n";

	// Summarize the partitions
	int minPartition = 1000;
	int maxPartition = 0;
	double meanPartition;
	int funcTotal = 0 ;	

	for (
		map< string, set<string> >::iterator it = familyList.begin();
		it != familyList.end(); it++
	) {
		int size = it->second.size();
		funcTotal += size;
		if (maxPartition < size)
			maxPartition = size;
		if (minPartition > size)
			minPartition = size;
	}

	meanPartition = double(funcTotal) / double (familyList.size());

	partitionFile << "Min;Max;Mean" << endl;
	partitionFile << minPartition << ";" << maxPartition << ";" << meanPartition << endl;

	// Print report file
	reportFile << "Bound;Discharged;Exception-free;SAT;UNSAT;UNKNOWN;MPERatio;Total;New;Missing;TimedOut;Poly-Ratio;Mean-time;CacheHits;CacheMisses\n";
	for (int i = 7; i >= 0; i --) {

	    reportTexFile.precision(2);
	    reportFile.precision(2);
	    reportTexFile << boundList[i] << " & ";
	    reportFile << boundList[i] << ";";
	    reportTexFile << discharged[i] << " & ";
	    reportFile << discharged[i] << ";";
	    reportTexFile << exceptionFree[i] << " & ";
	    reportFile << exceptionFree[i] << ";";
	    reportTexFile << SAT[i] << " & ";
	    reportFile << SAT[i] << ";";
	    reportTexFile << UNSAT[i] << " & ";
	    reportFile << UNSAT[i] << ";";
	    reportTexFile << UNKNOWN[i] << " & ";
	    reportFile << UNKNOWN[i] << ";";
	    double ratio = double(SAT[i] + UNSAT[i]) / double(SAT[i] + UNSAT[i] + UNKNOWN[i]);
	    reportTexFile << ratio << " & ";
	    reportFile << ratio << ";";

	    /// Add one to each number to avoid nan values. 
	    double polyRatio = double(poly_sat_queries[i] + poly_unsat_single_queries[i] + 1 ) /
		double(Z3_sat_queries[i] + poly_sat_queries[i] + Z3_unsat_queries[i] + poly_unsat_single_queries[i] + 1);
	    reportTexFile << new_exceptions[i] << "&";
	    reportTexFile << polyRatio << "\\\\\n";
	    reportFile << exceptionSet[i].size() << ";";
	    reportFile << newExceptionNum[i] << ";";
	    reportFile << ( exceptionSet[i].size() - newExceptionNum[i] ) << ";";
	    reportFile << timeoutExceptions[i] << ";";
	    reportFile << polyRatio << ";";	    
	    reportFile << setprecision(2);
	    int meanTime = totalTime[i]/TOTAL_FUNCTION;
	    reportFile << meanTime << ";";
	    reportFile << cacheHits[i] << ";";
	    reportFile << cacheMisses[i] << endl;	    
	}

	// Print path info fileName
	pathInfoFile << "Bound;Z3_Queries;Z3_sat_queries;Z3_unsat_queries;Poly_queries;Poly_sat_queries;Poly_sat_single_queries;Poly_sat_multiple_queries;Poly_unsat_queries;Poly_unsat_single_queries;Poly_unsat_multiple_queries" << endl;

	for (int i = 7; i >= 0; i--) {
	    pathInfoFile << boundList[i] << ";";
	    pathInfoFile << Z3_queries[i] << ";";
	    pathInfoFile << Z3_sat_queries[i] << ";";
	    pathInfoFile << Z3_unsat_queries[i] << ";";
	    pathInfoFile << poly_queries[i] << ";";
	    pathInfoFile << poly_sat_queries[i] << ";";
	    pathInfoFile << poly_sat_single_queries[i] << ";";
	    pathInfoFile << poly_sat_multiple_queries[i] << ";";
	    pathInfoFile << poly_unsat_queries[i] << ";";
	    pathInfoFile << poly_unsat_single_queries[i] << ";";
	    pathInfoFile << poly_unsat_multiple_queries[i] << endl;
	}

	// Print performance data
	for (int i = 7; i >= 0; i --) {
	    performanceFile << boundList[i] << "&";
	    performanceFile << discharged[i] << "&";
	    performanceFile << exceptionFree[i] << "&";
	    double ratio = double(solvedPaths[i])/double(completedPaths[i]);
	    performanceFile << ratio << "\\\\\n";
	}

	// Print the time list
	for (int i = 7; i >= 0; i--) {
		timeFile << boundList[i] << ";";
		for(
		    vector<double>::iterator it = timeList[i].begin();
		    it != timeList[i].end(); it++
		) {
			double time = *it;
			timeFile << time << ";";
		}
		timeFile << endl;
	}

	// Print function percentage
	int handledFunctions = 0;

	for (int i=0; i < 8; i++)
		handledFunctions += handledFunctionNum[i];

	ofstream percentageTex ("percentage.tex");
	percentageTex << "\\begin {tabular}{|l|l|}\n";
	percentageTex << "\\hline\n";
	percentageTex << "Bound & Percentage\n";
	for (int i = 7; i >= 0; i--) {
		percentageFile << boundList[i] << ";";
		double percentNum = ((double) handledFunctionNum[i] / (double) handledFunctions) * 100;
		percentageFile << percentNum << "%" << endl;
		percentageTex << boundList[i] << " & " << percentNum << "%\n";
	}
	percentageTex << "\\hline\n";
	percentageTex << "\\end{tabular}\n";

	// Print divideByZero list
	for (
		map <string, set<int> >::iterator it = divideByZeroList.begin();
		it != divideByZeroList.end(); it++
	) {
		string function = it->first;
		divideByZeroFile << function << ";";
		for (
			set<int>::iterator i = it->second.begin();
			i != it->second.end(); i++
		) {
			int val = *i;
			divideByZeroFile << val << ";";
		}
		divideByZeroFile << endl;
	}

	// Print invalid list
	for (
	  map <string, set<int> >::iterator it = invalidList.begin();
	  it != invalidList.end(); it++
	) {
		string function = it->first;
		invalidFile << function << ";";
		for (
			set<int>::iterator i = it->second.begin();
			i != it->second.end(); i++
		) {
			int val = *i;
			invalidFile << val << ";";
		}
		invalidFile << endl;
	}
	
	// Print out the maxTimeOut 
	std::cout << "Max time out: " << maxTimeOut << std::endl; 
	std::cout << "Number of runs that approximate max timeout: " << numOfMaxTimeOut << std::endl;
	return 0;
}

void processOutFiles( string dirline) {	  
	DIR *dp = opendir(dirline.c_str());
	struct dirent *dirp;
	
	int index; // Get the index of the running bound

	if ( dirline.compare("1") == 0) {
		index = 0;
	} else if (dirline.compare("2") == 0) {
		index = 1;
	} else if (dirline.compare("4") == 0) {
		index = 2;
	} else if (dirline.compare("8") == 0) {
		index = 3;
	} else if (dirline.compare("16") == 0) {
		index = 4;
	} else if (dirline.compare("32") == 0) {
		index = 5;
	} else if (dirline.compare("64") == 0) {
		index = 6;
	} else
		index = 7;

	
	while ((dirp = readdir(dp)) != NULL) {
		// Read all outfiles in the result directory
		string fileName = std::string (dirp->d_name);
		if (fileName.compare(".") == 0 || fileName.compare("..") == 0)
			continue;
		// Get the family function name
		string functionName(fileName);
		
		numOfOverflow = 0;
		numOfUnderflow = 0;
		numOfDivideByZero = 0;
		numOfInvalid = 0;

		replace(functionName, "main_",""); // Remove prefix main
		string originalFunctionName(functionName);
		// Extract functionName from fileName
		replace(functionName, "gsl_sf_", "");
		replace(functionName, ".", " ");
		replace(originalFunctionName, ".", " ");
		string familyName(functionName);
		replace(familyName, "_", " ");
		istringstream functionIss(functionName);
		functionIss >> functionName;
		istringstream originalFunctionIss(originalFunctionName);
		originalFunctionIss >> originalFunctionName;

		// Extract familyName from functionName
		istringstream familyIss(familyName);
		familyIss >> familyName;

		map< string, set<string> >::iterator it = familyList.find(familyName);
		if (it != familyList.end())
			familyList[familyName].insert(functionName);
		else {
			set <string> functionList;
			functionList.insert(functionName);
			familyList.insert(pair<string, set<string> >(familyName, functionList));
		}
		fileName = dirline + "/" + fileName;
		ifstream file(fileName.c_str());
		string line;
		summaryFile << fileName << ";";
		bool needTogetNewLine = true;
		getline(file,line);
		set<string> overflowStrSet;
		set<string> underflowStrSet;
		set<string> invalidStrSet;
		set<string> divByZeroStrSet;
		
		bool timedOut = false;
		int generatedPaths = 0;

		while (!file.eof()) {
			needTogetNewLine = true;
			/// keep exception line to check if an exception is in the set or not
			string exception_line = originalFunctionName;
			if (line.find(timeoutMsg) != string::npos || line.find(haltMsg) != string::npos) 
			  timedOut = true;
			size_t found = line.find(overflowMsg);
			
			// Case overflow exceptions
			if (found != string::npos) {
				exception_line += line;
				getline(file,line);
				found = line.find(inputString);
				if (found != std::string::npos) {
					string overflowName = "Overflow/" + originalFunctionName;
					ofstream overflowFile(overflowName.c_str(), ios::app);
					//Write the inputs to files
					string overflowStr = "";
					do {
						getline(file,line); // Read the input line
						overflowStr += line;
						overflowStr += " ";
						getline(file,line); // Check the next line
						found = line.find(inputString);
					} while (found != std::string::npos);
					set<string>::iterator it = overflowStrSet.find(overflowStr);
					if (it == overflowStrSet.end()) {
					  overflowFile << overflowStr << "\n";
					  overflowStrSet.insert(overflowStr);
					  numOfOverflow++;
					  totalOverflow++;
					  exceptions[index]++;
					}				
					exception_line += overflowStr;
					exceptionSet[index].insert(exception_line);
					/// The exception is not in the set of infinity exceptions
					if (index < 7 && exceptionSet[7].find(exception_line) == exceptionSet[7].end()) {
					    newExceptionNum[index]++;
					}
					it = exceptionList.find(exception_line);
					if (it == exceptionList.end()) {
					  new_exceptions[index]++;
					  exceptionList.insert(exception_line);
					}
				}				
				needTogetNewLine = false;				
			}
			found = line.find(underflowMsg);
			// Case Underflow exceptions
			if (found != std::string::npos) {
				exception_line += line;
				getline(file,line);
				found = line.find(inputString);
				if (found != std::string::npos){
					string underflowName = "Underflow/" + originalFunctionName;
					ofstream underflowFile(underflowName.c_str(), ios::app);
					//Write the inputs to files
					string underflowStr = "";
					do {
						getline(file,line); // Read the input line
						underflowStr += line;
						underflowStr += " ";
						getline(file,line); // Check the next line
						found = line.find(inputString);
					} while (found != std::string::npos);
					set<string>::iterator it = underflowStrSet.find(underflowStr);
					if(it == underflowStrSet.end()) {
					  underflowFile << underflowStr << "\n";
					  underflowStrSet.insert(underflowStr);
					  numOfUnderflow++;
					  totalUnderflow++;
					  exceptions[index]++;
					}
					exception_line += underflowStr;
					/// The exception is not in the set of infinity exceptions
					if (index < 7 && exceptionSet[7].find(exception_line) == exceptionSet[7].end()) {
					    newExceptionNum[index]++;
					}					
					exceptionSet[index].insert(exception_line);
					it = exceptionList.find(exception_line);
					if (it == exceptionList.end()) {
					  new_exceptions[index]++;
					  exceptionList.insert(exception_line);
					}
				}
				needTogetNewLine = false;				
			}
			found = line.find(invalidMsg);

			// Case Invalid exceptions
			if (found != std::string::npos) {
				exception_line += line;
				replace(line, invalidMsg, "");
				int dIndex = atoi(line.c_str());
				getline(file,line);
				found = line.find(inputString);
				if (found != string::npos) {
					map< string, set<int> >::iterator it = invalidList.find(functionName);

					if (it != invalidList.end())
						invalidList[functionName].insert(dIndex);

					else {
						set <int> idList;
						idList.insert(dIndex);
						invalidList.insert(pair<string, set<int> >(functionName, idList));
					}
					string invalidName = "Invalid/" + originalFunctionName;
					ofstream invalidFile(invalidName.c_str(), ios::app);
					string invalidStr = "";
					//Write the inputs to files
					do {
						getline(file,line); // Read the input line
						invalidStr += line;
						invalidStr += " ";
						getline(file,line); // Check the next line
						found = line.find(inputString);
					} while (found != std::string::npos);
					exception_line += invalidStr;
					set<string>::iterator newIt = invalidStrSet.find(invalidStr);
					if (newIt == invalidStrSet.end()) {
					  invalidFile << invalidStr << "\n";
					  invalidStrSet.insert(invalidStr);
					  numOfInvalid++;
					  totalInvalid++;
					  exceptions[index]++;
					}
					exceptionSet[index].insert(exception_line);
					/// The exception is not in the set of infinity exceptions
					if (index < 7 && exceptionSet[7].find(exception_line) == exceptionSet[7].end()) {
					    newExceptionNum[index]++;
					}
					newIt = exceptionList.find(exception_line);
					if (newIt == exceptionList.end()) {
					  new_exceptions[index]++;
					  exceptionList.insert(exception_line);
					}
				}
				needTogetNewLine = false;				
			}
			found = line.find(divideByZeroMsg);

			// Case divideByZero exceptions
			if (found != string::npos) {
				exception_line += line;
				replace(line, divideByZeroMsg, "");
				int dIndex = atoi(line.c_str());
				getline(file,line);
				found = line.find(inputString);
				if (found != std::string::npos){
					map< string, set<int> >::iterator newIt = divideByZeroList.find(functionName);
					if (newIt != divideByZeroList.end())
						divideByZeroList[functionName].insert(dIndex);
					else {
						set <int> idList;
						idList.insert(dIndex);
						divideByZeroList.insert(pair<string, set<int> >(functionName, idList));
					}
					string divByZeroName = "DivByZero/" + originalFunctionName;
					ofstream divByZeroFile(divByZeroName.c_str(), ios::app);
					string divByZeroStr = " ";
					//Write the inputs to files
					do {
						getline(file,line); // Read the input line
						divByZeroStr += line;
						divByZeroStr += " ";
						getline(file,line); // Check the next line
						found = line.find(inputString);
					} while (found != std::string::npos);
					set<string>::iterator it = divByZeroStrSet.find(divByZeroStr);
					if(it == divByZeroStrSet.end()) {
					    divByZeroFile << divByZeroStr << "\n";
					    divByZeroStrSet.insert(divByZeroStr);
					    numOfDivideByZero++;
					    totalDivideByZero++;
					    exceptions[index]++;
					}
					exception_line += divByZeroStr;
					exceptionSet[index].insert(exception_line);
					/// The exception is not in the set of infinity exceptions
					if (index < 7 && exceptionSet[7].find(exception_line) == exceptionSet[7].end()) {
					    newExceptionNum[index]++;
					}
					it = exceptionList.find(exception_line);
					if (it == exceptionList.end()) {
					  new_exceptions[index]++;
					  exceptionList.insert(exception_line);
					}
				}
				needTogetNewLine = false;				
			}

			found = line.find(summaryMsg);

			// Process summary part of output files.
			if (found != string::npos) {
				discharged[index] ++;
				handledFunctionNum[index] ++;
				//Move to next line
				std::getline(file,line);
				replace (line, "KLEE: done: completed paths = ", "");
				istringstream completedPathsIss(line);
				int totalPaths;
				completedPathsIss >> totalPaths;

				std::getline(file,line);
				replace (line, "KLEE: done: generated tests = ", "");
				istringstream generatedPathsIss(line);
				int generatedPaths;
				generatedPathsIss >> generatedPaths;
				double ratio = double(generatedPaths) / double (totalPaths);
			
				int z3_queriesNum = getStat (
				    file, "KLEE: done: Z3 queries = "
				);

				int z3_sat_queriesNum = getStat( 
				    file, "KLEE: done: Z3 sat queries = "
				);
				
				int z3_unsat_queriesNum = getStat(
				    file, "KLEE: done: Z3 unsat queries = "
				);
				
				int poly_queriesNum = getStat (
				    file, "KLEE: done: Poly queries = "
				);
				
				int poly_sat_queriesNum = getStat(
				    file, "KLEE: done: Poly sat queries = "
				);
				
				int poly_sat_single_queriesNum = getStat(
				    file, "KLEE: done: Poly sat single queries = "
				);
								
				int poly_sat_multiple_queriesNum = getStat(
				    file, "KLEE: done: Poly sat multiple queries = "
				);			

				int poly_unsat_queriesNum = getStat( 
				    file, "KLEE: done: Poly unsat queries = "
				);				

				int poly_unsat_single_queriesNum = getStat( 
				    file, "KLEE: done: Poly unsat single variable queries = "
				);
				

				int poly_unsat_multiple_queriesNum = getStat(
				    file, "KLEE: done: Poly unsat multiple variable queries = "
				);				
				
				int successful_concretization_Num = getStat(
				    file, "KLEE: done: Number of successful concretization: "
				);
				
				int failed_concretization_Num = getStat(
				    file, "KLEE: done: Number of failed concretization: "
				);
				
				int concretization_Rate = getStat(
				    file, "Successful rate of concretization: " 
				);
				
				int intensive_successes = getStat(
				    file, "KLEE: done: Number of intensive successes: "
				);				
				
				int intensive_fails = getStat(
				    file, "KLEE: done: Number of intensive fails: "
				);				
				
				int cachehits = getStat(
				    file, "KLEE: done: Number of cache hits = "
				);
				cacheHits[index] += cachehits;
				
				int cache_misses = getStat(
				    file, "KLEE: done: Number of cache misses = "
				);
				cacheMisses[index] += cache_misses;
						
				int cexCacheHits = getStat(
				    file, "KLEE: done: Number of cex cache hits = "
				);
								
				int cexCacheMisses = getStat(
				    file, "KLEE: done: Number of cex cache misses = "
				);			
				
				//Ignore the next empty line
				std::getline(file,line);

				//Get the time elapsed line
				std::getline(file,line);
				replace (line, "Elapsed = ", "");
				double time;
				istringstream timeIss(line);
				timeIss >> time;
				std::cout << time << std::endl; // debugging
				
				if (time > maxTimeOut && time < MAXTIMEOUT)
				  maxTimeOut = time;
				
				if ( time > ( MAXTIMEOUT - 10 ) )
				  numOfMaxTimeOut++;
				
				totalTime[index] += time;

				ratiosList[index].push_back(ratio);
				timeList[index].push_back(time);
				solvedPaths[index] += generatedPaths;
				SAT[index] += generatedPaths;
				solvedPaths[index] += z3_unsat_queriesNum;
				solvedPaths[index] += poly_unsat_single_queriesNum;
				UNSAT[index] += z3_unsat_queriesNum;
				UNSAT[index] += poly_unsat_single_queriesNum;
				//UNKNOWN[index] += unknownPaths;
				UNKNOWN[index] += poly_unsat_multiple_queriesNum;
				completedPaths[index] += totalPaths;
				Z3_queries[index] += z3_queriesNum;
				Z3_sat_queries[index] += z3_sat_queriesNum;
				Z3_unsat_queries[index] += z3_unsat_queriesNum;
				poly_queries[index] += poly_queriesNum;
				poly_sat_queries[index] += poly_sat_queriesNum;
				poly_sat_single_queries[index] += poly_sat_single_queriesNum;
				poly_sat_multiple_queries[index] += poly_sat_multiple_queriesNum;
				poly_unsat_queries[index] += poly_unsat_queriesNum;
				poly_unsat_single_queries[index] += poly_unsat_single_queriesNum;
				poly_unsat_multiple_queries[index] += poly_unsat_multiple_queriesNum;
				if (
				  (numOfDivideByZero + numOfInvalid +
				  numOfOverflow + numOfUnderflow) == 0
				)
				   exceptionFree[index]++;
				break;
			}

			if (needTogetNewLine)
			  getline(file,line);
		}

		summaryFile << numOfOverflow << ";" << numOfUnderflow << ";";
		summaryFile << numOfInvalid << ";" << numOfDivideByZero << "\n";
		exceptions[index] += numOfDivideByZero;
		exceptions[index] += numOfInvalid;
		exceptions[index] += numOfOverflow;
		exceptions[index] += numOfUnderflow;
		if (timedOut) {
		  timeoutExceptions[index] += numOfDivideByZero;
		  timeoutExceptions[index] += numOfInvalid;
		  timeoutExceptions[index] += numOfOverflow;
		  timeoutExceptions[index] += numOfUnderflow;
		}
	}	
}