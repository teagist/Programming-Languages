//****************************************************************************
//	Driver File for Process Scheduler
//	Programmed by: Houston Brown
//	Last Compiled Date: 11/14/2024
//
//	Copyright (c) 2024 Houston Brown
//
//	Permission is hereby granted to any person that obtains a copy of this
//	software and associated documentation files (the "Software"), to deal
//	within the Software for free and without restriction.  Including the
//	rights to use, copy, modify, merge, publish, redistribute, sublicense,
//	and/or sell copies of the Software.  Furthermore, to the people who the
//	Software is furnished to do so, subject to the following:
//
//	The above copyright notice and this permission notice shall be included
//	in all copies or substantial portions of the Software.
//
//	The Software is provided "as is", without any warranty of any kind,
//	express or implied, including but not limited to the warranties of 
//	merchantability, fitness for a particular purpose and noninfringement.
//	In not event shall the liability, whether in action of contract, tort or
//	otherwise, arising from, out of or in connection with the Software or the
//	use or other dealings in the Software.
//
//	DISCLAIMER: This program uses system calls that will only work on 
//				Windows operating system.
//****************************************************************************
#include "WinUtils.h"

void printHeader();
void printErrorMsg(char* msg);
void printResultMsg(int error);


int main()
{
	std::string greeting;
	
	printHeader();
	std::cout << "pcb>> ";
	std::cin >> greeting;
	printResultMsg(0);
	
	return 0;
}





/*
 *	METHOD NAME: printHeader.
 *
 *	DESCRIPTION: Function to display Header for Houston's Pascal Compiler. 
 *
 *	PARAMETERS: N/A.
 */
void printHeader()
{
	/////////////////////////////////////////////////////
	// Coloring Output
	HANDLE hConsole;
	CONSOLE_SCREEN_BUFFER_INFO myBuffInfo;
	
	hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
	GetConsoleScreenBufferInfo(hConsole, &myBuffInfo);
	/////////////////////////////////////////////////////
	
	SetConsoleTextAttribute(hConsole, 10);	
	printf("   ######    #####   ######\n");
	printf("   ##   ##  ##    #  ##   ##\n");
	printf("   ##   ##  ##       ##   ##\n");
	printf("   ######   ##       ######\n");
	printf("   ##       ##       ##   ##\n");
	printf("   ##       ##    #  ##   ##\n");
	printf("   ##        #####   ######\n\n\n");	

	// Restoring console defaults
	SetConsoleTextAttribute(hConsole, myBuffInfo.wAttributes);	
}



/*
 *	METHOD NAME: printErrorMsg.
 *
 *	DESCRIPTION: If the pointer passed to this function is not NULL, then
 *				 the error message will be displayed to the console with
 *				 appropriate highlighting.
 *
 *	PARAMETERS: msg (char*): The error message to be displayed.
 */
void printErrorMsg(char* msg)
{
	HANDLE hConsole;
	CONSOLE_SCREEN_BUFFER_INFO myBuffInfo;
		
	hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
	GetConsoleScreenBufferInfo(hConsole, &myBuffInfo);    
	SetConsoleTextAttribute(hConsole, 79);
	
	if (msg != NULL)
	{
		printf("\n Error: %s \n", msg);
	}
	
	SetConsoleTextAttribute(hConsole, myBuffInfo.wAttributes);	
}



/*
 *	METHOD NAME: printResultMsg.
 *
 *	DESCRIPTION: Function to display the success or failure to compile.  If 
 *				 the number of errors is 0, a successful compilation message
 *				 will be displayed, else a failure message will be displayed. 
 *
 *	PARAMETERS: error (int): number of errors in the program.
 */
void printResultMsg(int error)
{
    if (error == 0)
    {
		HANDLE hConsole;
		CONSOLE_SCREEN_BUFFER_INFO myBuffInfo;
		
		hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
		GetConsoleScreenBufferInfo(hConsole, &myBuffInfo);    
		SetConsoleTextAttribute(hConsole, 47);
		printf(" Processed Successfully ");
		SetConsoleTextAttribute(hConsole, myBuffInfo.wAttributes);
	}
	else
	{
		HANDLE hConsole;
		CONSOLE_SCREEN_BUFFER_INFO myBuffInfo;
		
		hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
		GetConsoleScreenBufferInfo(hConsole, &myBuffInfo);    
		SetConsoleTextAttribute(hConsole, 79);
		printf(" Processing Failed ");
		SetConsoleTextAttribute(hConsole, myBuffInfo.wAttributes);		
	}	
}
