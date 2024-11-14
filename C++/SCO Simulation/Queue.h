//****************************************************************************
//	Header File for Queue Class
//	Programmed by: Houston Brown
//	Last Compiled Date: 11/20/2024
//****************************************************************************

#ifndef _QUEUE_H
#define _QUEUE_H

#include "Customer.h"


class Customer;
const int ARRSIZE = 20;


class Queue
{
public:
    Queue();
    void enqueue();
    void dequeue();
    bool isEmpty() { return (front == -1 && rear == -1); }
    void printQueue();
    
private:
    int      front;
    int      rear;
    int      count;
    Customer array[ARRSIZE];
};
#endif
