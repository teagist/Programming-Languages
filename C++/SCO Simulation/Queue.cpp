//****************************************************************************
//	Implementation File for Queue Class
//	Programmed by: Houston Brown
//	Last Compiled Date: 11/13/2024
//****************************************************************************

#include "Queue.h"


Queue::Queue()
{
    front = -1;
    rear  = -1;
    count =  0;

    for (int i = 0; i < ARRSIZE; i++)
    {
        array[i].setName("NULL");
    }	
}



void Queue::enqueue()
{
    if (count == ARRSIZE)
    {
        std::cerr << std::endl << "Queue is full!!!" << std::endl;
    }
    else
    {
        if (front == -1)
        {
            front = 0;
        }

		Customer myCustomer;

        rear = (rear + 1) % ARRSIZE;
        array[rear].setArrival(myCustomer.getArrival());
        array[rear].setDeparture(myCustomer.getDeparture());
        array[rear].setName(myCustomer.getName());
        array[rear].setProcess(myCustomer.getProcess());
        array[rear].setService(myCustomer.getService());
        count++;
    }
}



void Queue::dequeue()
{
    if (isEmpty())
    {
        std::cout << "Queue is empty!!!" << std::endl;
    }
    else
    {
        array[front] = Customer();

        if(front == rear)
        {
            front = -1;
            rear = -1;
        }
        else
        {
            front = (front + 1) % ARRSIZE;
        }
        count--;
    }
}



void Queue::printQueue()
{
    for (int i = 0; i < ARRSIZE; i++)
    {
    	if (!(array[i].getName() == "NULL"))
    	{
	    	std::cout << i + 1 << ".) ";
	        std::cout << array[i].getName() << std::endl;
	        
	        std::cout << "\t" << array[i].getArrival() << std::endl;
	        std::cout << "\t" << array[i].getDeparture() << std::endl;
	        std::cout << "\t" << array[i].getProcess() << std::endl;
	        std::cout << "\t" << array[i].getService() << std::endl;
	    }   
    }
    std::cout << std::endl << std::endl << std::endl;
}
