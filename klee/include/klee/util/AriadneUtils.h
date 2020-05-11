/*
 * AriadneSolverUtil.h
 *
 *  Created on: Apr 14, 2011
 *      Author: thanh
 */

#ifndef ARIADNEUTIL_H_
#define ARIADNEUTIL_H_

#include <string>

/// Util functions dynamically check the test cases
extern std::string exec(const char* cmd);
extern void chomp(std::string& szString);

#endif /* ARIADNEUTIL_H_ */
