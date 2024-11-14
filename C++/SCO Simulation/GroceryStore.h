//****************************************************************************
//	Header File for GroceryStore Class
//	Programmed by: Houston Brown
//	Last Compiled Date: 11/13/2024
//****************************************************************************

#ifndef _GROCERYSTORE_H
#define _GROCERYSTORE_H

#include "Logger.h"
#include "Customer.h"
#include "Queue.h"


class GroceryStore
{
public:
	GroceryStore();
	std::string getName() const { return storeName; }
	void printSimInfo();	
	void runSimulation();
	
private:
	std::string storeName;
	int numOfLanes;
	Queue laneQueue;
	Logger simLogger;	
};

#endif
