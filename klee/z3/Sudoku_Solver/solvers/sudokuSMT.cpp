/***
*sudokuSMT.cpp - program for sudoku solving.
*
*Authors:
*	    Nikolaj Bjorner, Dmitry Ermolov, 9nT
*
*Date:
*		26.04.2008
*
*Description:
*		This program takes txt file with sudoku as input
*       and build a .smt file with theory corresponding
*       to this sudoku.
*
****/

#include <stdio.h>
#include <stdlib.h>

// Text in the begining of z3 file
#define Z3_BEGIN "(benchmark sudoku :logic QF_LIA\n"

// Text in the end of z3 file
#define Z3_END   ":formula true\n)\n"

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

// writes statement: "the numbers must occur only once in row"
void writeHUnique(FILE *out);

// writes statement: "the numbers must occur only once in each column"
void writeVUnique(FILE *out);

// writes statement: "the numbers must occur only once in each square 3x3"
void writeSUnique(FILE *out);

// variable which help us to generate ast node id
int curId = 100;

int main(int argc, char* argv[])
{
    FILE *in, *out;
    if (argc < 2){
        printf("Using: \nsudoku.exe <input file> [<output file>]\n");
        return 0;
    } else if (argc == 2){
        out = fopen("sudoku.smt", "w");
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
                    fprintf(out, ":assumption (= f%d%d  %c)\n", j, i, c);
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
}

void writeConstDef(FILE *out)
{
	int i,j;
	for(i=1; i < 10; i++)
            for(j=1; j < 10; j++)
                fprintf(out, ":extrafuns ((f%d%d Int))\n", i, j);
}

void writeBounds(FILE *out)
{
    int i, j;
    for (i = 1; i < 10; i++){
        for (j = 1; j < 10; j++){ 
            fprintf(out, ":assumption (and (<= f%d%d 9) (<= 1 f%d%d))\n", i, j, i, j);
        }
    }
}

void writeHUnique(FILE *out)
{
    int i, j;
    for (i = 1; i < 10; i++){
        fprintf(out, ":assumption (distinct ");
        for (j =1; j < 10; j++){
            fprintf(out, "f%d%d ", i, j);
        }
        fprintf(out, ")\n");
    }
}

void writeVUnique(FILE *out)
{
    int i, j;
    for (i = 1; i < 10; i++){
        fprintf(out, ":assumption (distinct ");
        for (j =1; j < 10; j++){
            fprintf(out, "f%d%d ", j, i);
        }
        fprintf(out, ")\n");
    }
}

void writeSUnique(FILE *out)
{
    
    // I don't play soduko, so I don't know what this means, but
    // this can possibly also be condensed using "unique"
    int i, j, k,l, m,n;
    for (m=0; m<3; m++){
        for (n=0; n<3; n++){
            fprintf(out, ":assumption (distinct");
            for (i = 3*m+1; i < 3*m+4; i++){
                for (j = 3*n+1; j < 3*n+4; j++){
                    fprintf(out, " f%d%d", i, j);
                }
            }
            fprintf(out, ")\n");
        }
    }
}


