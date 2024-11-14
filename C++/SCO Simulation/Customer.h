//****************************************************************************
//	Header File for Customer Class
//	Programmed by: Houston Brown
//	Last Compiled Date: 11/13/2024
//****************************************************************************

#ifndef _CUSTOMER_H
#define _CUSTOMER_H

#include <iostream>
#include <ctime>
#include <cstdlib>
//using namespace std;



class Customer
{
public:
    Customer();
    void setName(std::string newName);
    std::string getName() const;
    void setArrival(int newArrival);
    int getArrival() const;
    void setDeparture(int newDeparture);
    int getDeparture() const;
    void setService(int newService);
    int getService() const;
    void setProcess(int newProcess);
    int getProcess() const;
    
	//friend class Queue;
	
private:
    std::string name;
    int         arrival;
    int         departure;
    int         service;
    int         process;
};


#endif
