/***
*sudokuLIA.cpp - program for sudoku solving.
*
*Authors:
*	    Dmitry Ermolov, 9nT
*
*Date:
*		26.04.2008
*
*Description:
*		This program takes txt file with sudoku as input
*		and build a .z3 file with theory corresponding
*		to this sudoku.
*
****/

#include <stdio.h>
#include <stdlib.h>

// Text in the begining of z3 file
#define Z3_BEGIN "Solver LIA  \n"\
                 "Type int int\n"

// Text in the end of z3 file
#define Z3_END   "Check\n"

// writes part of the theory 
int  writeZ3Theory(FILE *in, FILE *out);

// writes part of the theory independent of concrete puzzle
void writeRules(FILE *out);

// writes part of the theory, which depended on concrete puzzle
int  writeInitData(FILE *in, FILE *out);

// writes numbers defenitions
void writeNumDef(FILE *out);

// writes constants defenitions
void writeConstDef(FILE *out);

// writes statements about constants bounds
void writeBounds(FILE *out);

// writes statement: "the numbers must occur only once in each horizontal line"
void writeHUnique(FILE *out);

// writes statement: "the numbers must occur only once in each vertical line"
void writeVUnique(FILE *out);

// writes statement: "the numbers must occur only once in each square 3*3"
void writeSUnique(FILE *out);

int curId = 100;

int main(int argc, char* argv[])
{
	FILE *in, *out;
	if (argc < 2){
		printf("Using: \nsudoku.exe <input file> [<output file>]\n");
		return 0;
	} else if (argc == 2){
		out = fopen("sudoku.z3", "w");
	} else {
		out = fopen(argv[2], "w");
	}
	if(out == NULL){
		printf("Error: can't open output file\n");
		return 0;
	}
	in = fopen(argv[1], "r");
	if(in == NULL){
		printf("Error: can't open input file\n");
		fclose(out);
		return 0;
	}	
	if (!writeZ3Theory(in, out)){
		printf("Error: bad input file\n");
	}
	fclose(in);
	fclose(out);
	return 0;
}

int writeZ3Theory(FILE *in, FILE *out)
{
	fprintf(out, Z3_BEGIN);
	writeRules(out);
	if(!writeInitData(in, out)) return 0;
	fprintf(out, Z3_END);
	return 1;
}

void writeRules(FILE *out)
{
	writeNumDef(out);
	writeConstDef(out);	
	writeBounds(out);
	writeHUnique(out);
	writeVUnique(out);
	writeSUnique(out);
}

int  writeInitData(FILE *in, FILE *out)
{
	int i,j;
	char c;
	for(j=1; j < 10; j++){
		for(i=1; i < 10; i++){
			c = fgetc(in);
			switch(c){
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
				fprintf(out, "App $%d = f%d%d  %c\n", curId, j, i, c);
				fprintf(out, "Assert $%d\n", curId);
				curId++;
				break;
			case '.':
				break;
			default:
				return 0;
			}
		}
		c = getc(in);
		if (c != '\n' && c != 0)
			return 0;
	}
	return 1;
}

void writeNumDef(FILE *out)
{
	int i;
	for(i=1; i < 10; i++)
		fprintf(out, "Num %d %d int\n", i, i);
}

void writeConstDef(FILE *out)
{
	int i,j;
	for(i=1; i < 10; i++)
		for(j=1; j < 10; j++)
			fprintf(out, "Const f%d%d f%d%d int\n", i, j, i, j);
}

void writeBounds(FILE *out)
{
	int i, j;
	for (i = 1; i < 10; i++){
		for (j = 1; j < 10; j++){ 
			fprintf(out, "App $%d <= f%d%d 9\n", curId , i,j);
			fprintf(out, "App @%d >= f%d%d 1\n", curId , i,j);
			fprintf(out, "Assert $%d\n", curId);
			fprintf(out, "Assert @%d\n", curId);
			curId ++;
		}
	}
}

void writeHUnique(FILE *out)
{
	int i, j, k;
	for (i = 1; i < 10; i++){
		for (j =1; j < 10; j++){
			for (k  = j+1; k < 10; k++){
				fprintf(out, "App $%d = f%d%d f%d%d\n",curId, i,j , i,k);
				fprintf(out, "App @%d not $%d\n",curId, curId);
	            fprintf(out, "Assert @%d\n",curId);
		        curId ++;
			}
		}
	}
}

void writeVUnique(FILE *out)
{
	int i, j, k;
	for (i = 1; i < 10; i++){
		for (j =1; j < 10; j++){
			for (k  = j+1; k < 10; k++){
				fprintf(out, "App $%d = f%d%d f%d%d\n",curId, j,i , k,i);
				fprintf(out, "App @%d not $%d\n",curId, curId);
	            fprintf(out, "Assert @%d\n",curId);
		        curId ++;
			}
		}
	}
}

void writeSUnique(FILE *out)
{
	int i, j, k,l, m,n;
	for (m=0; m<3; m++){
		for (n=0; n<3; n++){
			for (i = 3*m+1; i < 3*m+4; i++){
				for (j = 3*n+1; j < 3*n+4; j++){
					for (k = 3*m+1; k < 3*m+4; k++){
						for (l = 3*n+1; l < 3*n+4; l++){
							if ((k < i) || (k == i && l <= j)) continue;                        
							fprintf(out, "App $%d = f%d%d f%d%d\n", curId, i,j , k,l);
							fprintf(out, "App @%d not $%d\n", curId, curId);
							fprintf(out, "Assert @%d\n", curId); 
							curId += 1;
						}
					}
				}
			}
		}
	}
}

