/*********************************************************************
**  NAME:  PtdCommandArray.h
**  Description:
**				all member function and variable for class PtdCommandArray
**				Manage an array of PtdCommand objects
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdCommandArray.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:29
*********************************************************************/
                           
#ifndef PTDCOMMANDARRAYH
#define PTDCOMMANDARRAYH

#include "PtdPArray.h"

class PtdCommand;


class	PtdCommandArray : public PtdPArray
{
	public:

	/*
 	 * Function:	Constructor with initial size
 	 *
	 * Description:	Initialize obj
 	 *
 	 */
	PtdCommandArray(int initSize = 10)
			: PtdPArray(initSize)
	{ 
		// do nothing
	}

	
	/*
     * Function:	[]
 	 *
	 * Description:	Get the pointer to the [i]th  element
 	 *
 	 */
	PtdCommand*	operator[](int i) const
			{ return (PtdCommand *) PtdPArray::operator[](i); }


	/*
 	 * Function:	First
 	 *
	 * Description:	Get the pointer to the first element
 	 *
 	 */
	PtdCommand*	First()
			{ return (PtdCommand *) PtdPArray::First(); }

	/*
 	 * Function:	Last
 	 *
	 * Description:	Get the pointer to the last element
 	 *
 	 */
	PtdCommand*	Last() const
			{ return (PtdCommand *) PtdPArray::Last(); }

	/*
 	 * Function:	IsInArray
 	 *
	 * Description:	Is the element in the array 
 	 *
 	 * Input:	void* Element to be checked
 	 *
 	 * Output:int	
 	 *
 	 * Warnings/Notes: -1 returned as position, for FALSE return
 	 */
	int  	IsInArray(const PtdCommand *pCommand, 
				int &position) const 
			{ return PtdPArray::IsInArray(pCommand, position); }

	/*
 	 * Function:	DeallocateElements
 	 *
	 * Description:	remove all references and delete the elements
 	 *
 	 * Input:	void* Element to be checked
 	 *
 	 * Output:	
 	 *
 	 * Warnings/Notes: 
 	 */
   	void DeallocateElements();
};
#endif
