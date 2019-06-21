/*********************************************************************
**  NAME:  PtdCommandList.h
**  Description:
**				all member function and variable for class PtdCommandList
**				class to maintain a list of command objects
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdCommandList.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:29
*********************************************************************/

#ifndef PTDCOMMANDLISTH
#define PTDCOMMANDLISTH

#include "PtdCommandArray.h"
#include "PtdCommand.h"

class PtdCommandList : public PtdCommand {
    
  private:
    
    PtdCommandArray m_CommandArray;    // The list of objects
    int   _numElements; // Current size of list

    virtual void DoIt();  

  public:
 
	/*
	 * Function:	PtdCommandList
	 *
	 * Description: constructor
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning:
	 *
	 */
    PtdCommandList();    
    PtdCommandList(char *); 

	/*
	 * Function:	~PtdCommandList
	 *
	 * Description: destructor
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning: Destroys list, but not objects in list
	 *
	 */
    virtual ~PtdCommandList();
    
	/*
	 * Function:	Add
	 *
	 * Description: Add a single PtdCommand object to list
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning: 
	 *
	 */
    void Add(PtdCommand *);
    
	/*
	 * Function:	SetRadioMode
	 *
	 * Description: Sets the commands such that only one command
	 *				in the group remains active at any instant
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning: 
	 *
	 */
    void SetRadioMode();

	/*
	 * Function:	
	 *
	 * Description: Access functions
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning: 
	 *
	 */
    PtdCommandArray *Contents() {return &m_CommandArray;} // Return the list
    int Size() {return m_CommandArray.Size();}    // Return list size
    PtdCommand *operator[](int);            // Return an element of the list
};

#endif

