//****************************************************************************
//	Implementation File for GroceryStore Class
//	Programmed by: Houston Brown
//	Last Compiled Date: 11/13/2024
//****************************************************************************

#include "GroceryStore.h"


//****************************************************************************
//	DEFAULT CONSTRUCTOR.   
//****************************************************************************

GroceryStore::GroceryStore()
{
	storeName  = "Houston's Grocery Store (HGS)";
	numOfLanes = 10;

	simLogger.setTest("Grocery Store Checkout Lane Simulation");
	simLogger.setVersion("1.00.00.01");
	simLogger.setTitlebar();	
}



//****************************************************************************
//	NAME: printSimInfo.
//
//	DESCRIPTION: Print the preliminary information about the simulation.  
//****************************************************************************

void GroceryStore::printSimInfo()
{
	try
	{
		std::string stars = "****************************************";
		std::cout << stars << stars << std::endl;
		simLogger.printLineHeader();
		std::cout << "Simulation Started." << std::endl;
		
		simLogger.printLineHeader();
		std::cout << "Simulation Name: " << simLogger.getTest() << std::endl;
		
		std::cout << "\t\t Group: Alpha" << std::endl;
		std::cout << "\t\t Simulation Runner: Houston Brown" << std::endl;
		
		std::cout << "\t\t Hardware Information: " << std::endl;
		WinUtils::printHardwareDetails();
		std::cout << std::endl;
		
		std::cout << "\t\t Operating System: ";
		WinUtils::printOSDetails();
		
		std::cout << std::endl << std::endl;
		simLogger.printLineHeader();
		std::cout << "Store Name: " << storeName << std::endl;
		
		simLogger.printLineHeader();		
		std::cout << "Number of Lanes: " << numOfLanes << std::endl;
		std::cout << std::endl << stars << stars;
	}
	catch(...)
	{
		std::cerr << "Error retrieving test info." << std::endl;
	}
}



//****************************************************************************
//	NAME: runSimulation.
//
//	DESCRIPTION: This method will run the simulation. 
//****************************************************************************

void GroceryStore::runSimulation()
{
	printSimInfo();
}


