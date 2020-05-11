/*
 * ExceptionHandler.cpp
 *
 *  Created on: Oct 2, 2011
 *      Author: thanh
 */
#include "klee/ExceptionHandler.h"
#include "klee/util/AriadneUtils.h"

#include <sstream>

using namespace klee;
using namespace std;

bool ExceptionHandler::check(Assignment assignment) {
	/// Get the string inputs
	std::string inputs = "";
	for (
		std::map<const Array*, std::vector<unsigned char> >::iterator it =
		assignment.bindings.begin(), ie = assignment.bindings.end(); it != ie; ++it
	) {
		std::pair<const Array*, std::vector<unsigned char> > valuePair = *it;
		const Array* os = valuePair.first;
		std::string prefix = "arr";
		if (!os->name.compare(0, prefix.size(), prefix)) {
			std::vector<unsigned char> &arr = valuePair.second;
			unsigned char* val = new unsigned char[os->size];
			for (unsigned offset = 0; offset < os->size; offset++) {
				val[offset] = arr[offset];
			}
			double input = *reinterpret_cast<double*> (val);
			mpq_t mpqValue;
			mpq_init(mpqValue);
			mpq_set_d(mpqValue, input);
			string value(mpq_get_str(NULL, 10, mpqValue));
			inputs += value + " ";
		}
	}

	std::string exceptionStr;

	switch (getException()) {
		case Overflow:
			exceptionStr = "Potential Overflow";
			break;
		case Underflow:
			exceptionStr = "Potential Underflow";
			break;
		case Invalid:
			exceptionStr = "Potential Invalid";
			break;
		case DivideByZero:
			exceptionStr = "Potential Division By Zero";
			break;
		default:
			return true;
	}

	exceptionStr += " ";
	ostringstream sStream;
	sStream << getLocation();
	exceptionStr += sStream.str();
	chomp(exceptionStr);

	/// Execute the inputs with concrete executable, and compare the output
	/// with the printed messages
	std::string command = "./test " + inputs;
	std::string output = exec(command.c_str());
	chomp(output);

	/// If the printed message is the same as the checking message, return 1
	/// mean that the exception is valid. Otherwise, the found exception is invalid
	if ( exceptionStr.compare(output.c_str()) == 0 )
		return true;
	else
		return false;
}
