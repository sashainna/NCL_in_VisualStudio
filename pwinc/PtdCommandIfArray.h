/*********************************************************************
**  NAME:  PtdCommandIfArray.h 
**  Description:
**				all member function and variable for class PtdCommandIfArray
**				class to manage an array of command interface objects
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdCommandIfArray.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:29
*********************************************************************/

#include "PtdPArray.h"

class PtdCommandInterface;


class	PtdCommandInterfaceArray : public PtdPArray
{
protected:
 
public:

	/*
 	 * Function:	Constructor with initial size
 	 *
	 * Description:	Initialize obj
 	 *
 	 */
	PtdCommandInterfaceArray(int initSize = 10)
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
	PtdCommandInterface*	operator[](int i) const
			{ return (PtdCommandInterface *) PtdPArray::operator[](i); }


	/*
 	 * Function:	First
 	 *
	 * Description:	Get the pointer to the first element
 	 *
 	 */
	PtdCommandInterface*	First()
			{ return (PtdCommandInterface *) PtdPArray::First(); }

	/*
 	 * Function:	Last
 	 *
	 * Description:	Get the pointer to the last element
 	 *
 	 */
	PtdCommandInterface*	Last() const
			{ return (PtdCommandInterface *) PtdPArray::Last(); }

	/*
 	 * Function:	IsInArray
 	 *
	 * Description:	Is the element in the array 
 	 *
 	 * Input:	void* Element to be checked
 	 *
 	 * Output:	int
 	 *
 	 * Warnings/Notes: -1 returned as position, for FALSE return
 	 */
	int  	IsInArray(const PtdCommandInterface *pCommandInterface, 
			int &position) const 
			{ return PtdPArray::IsInArray(pCommandInterface, position); }

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
