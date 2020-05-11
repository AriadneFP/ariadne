/*
 * AriadneUtils.cpp
 *
 *  Created on: Oct 2, 2011
 *      Author: thanh
 */

#include "klee/util/AriadneUtils.h"
#include <fstream>
#include <errno.h>
#include <signal.h>
#include <sys/wait.h>
#include <cstdlib>
#include <iostream>

using namespace std;

int SIG_TIME_OUT = 5;

static void timed_out(int x) {
	std::cerr << "Timeout on the concrete executable" << std::endl;
	_exit(0);
}

string exec(const char* cmd) {
	int pid = fork();
	if (pid==-1) {
	    fprintf(stderr, "error: fork failed to execute the concrete check");
	    return NULL;
	}
	if (pid == 0) {
		struct sigaction action;
		action.sa_handler = timed_out;
		action.sa_flags = 0;
		sigaction(SIGALRM, &action, 0);
		int sig_timeout = SIG_TIME_OUT;
		alarm(sig_timeout);
		FILE* pipe = popen(cmd, "r");
		if (!pipe)
			return "ERROR";
		char buffer[128];
		ofstream sharedFile("shared.tmp");
		while (!feof(pipe)) {
			if (fgets(buffer, 128, pipe) != NULL) {
				sharedFile << buffer << std::endl;
			}
		}
		pclose(pipe);
		alarm(0);
		_exit(0);
    } else {
        int status;
        pid_t res;
        do {
          res = waitpid(pid, &status, 0);
        } while (res < 0 && errno == EINTR);

        if (res < 0) {
          fprintf(stderr, "error: waitpid() failed to execute the concrete check");
        }
        if (WIFSIGNALED(status) || !WIFEXITED(status)) {
          fprintf(stderr, "error: exec did not return successfully");
        }
    }
	ifstream inputSharedFile("shared.tmp");
	std::string result;
	getline(inputSharedFile, result);
	return result;
}

void chomp(string& szString)
{
	string whitespaces(" \t\f\v\n\r");
	size_t found = szString.find_last_not_of(whitespaces);
	if (found != string::npos)
		szString.erase(found + 1);
	else
		szString.clear();
}
