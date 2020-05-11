/*
 * ExceptionHandler.h
 *
 *  Created on: Sep 29, 2011
 *      Author: thanh
 */

#ifndef EXCEPTIONHANDLER_H_
#define EXCEPTIONHANDLER_H_

#include "klee/util/Assignment.h"

namespace klee {
	class ExceptionHandler {

	private:
		int location;

	public:
		enum ExceptionType {
			Overflow,
			Underflow,
			Invalid,
			DivideByZero,
			NotException
		};
		ExceptionType exception;

		ExceptionHandler() {
			location = -1;
			exception = NotException;
		}

		~ExceptionHandler() {}

		void setException(ExceptionType _exception) {
			this->exception = _exception;
		}

		ExceptionType getException() {
			return exception;
		}

		void setLocation(int location) {
			this->location = location;
		}

		int getLocation() {
			return location;
		}

		bool check(Assignment assignment);

	};
}


#endif /* EXCEPTIONHANDLER_H_ */
