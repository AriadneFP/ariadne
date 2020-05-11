CC      = "g++"

all: sudokuLIA.exe sudokuBV.exe sudokuSMT.exe
		
sudokuLIA.exe: sudokuLIA.cpp
		$(CC)  sudokuLIA.cpp -o sudokuLIA.out
		
sudokuBV.exe: sudokuBV.cpp
		$(CC)  sudokuBV.cpp -o sudokuBV.out
		
sudokuSMT.exe: sudokuSMT.cpp
		$(CC)  sudokuSMT.cpp -o sudokuSMT.out

clean:
		del *.exe
