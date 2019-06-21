/*********************************************************************
**  NAME:  PtdMainWindowArray.h
**  Description:
**				all member function and variable for class PtdMainWindowArray
**				manage an array of main windows
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdMainWindowArray.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:31
*********************************************************************/
#ifndef PTDMAINWINDOWARRAYH
#define PTDMAINWINDOWARRAYH
#include "PtdPArray.h"

#include "PtdMainWindow.h"

class	PtdMainWindowArray : public PtdPArray
{
	protected:
 
	public:

	/*
 	 * Function:	Constructor with initial size
 	 *
	 * Description:	Initialize obj
 	 *
 	 */
	PtdMainWindowArray(int initSize = 10)
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
	PtdMainWindow*		operator[](int i) const
						{ return (PtdMainWindow *) PtdPArray::operator[](i); }


	/*
 	 * Function:	First
 	 *
	 * Description:	Get the pointer to the first element
 	 *
 	 */
	PtdMainWindow*		First()
						 { return (PtdMainWindow *) PtdPArray::First(); }

	/*
 	 * Function:	Last
 	 *
	 * Description:	Get the pointer to the last element
 	 *
 	 */
	PtdMainWindow*		Last() const
						 { return (PtdMainWindow *) PtdPArray::Last(); }

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
	int  	IsInArray(const 	PtdMainWindow *pWindow, 
									int &position) const 
							{ return PtdPArray::IsInArray(pWindow, position); }

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
   	void DeallocateElements()
	{
		PtdMainWindow *pWindow;
		for(int i=0;i<Size();i++)
		{
			pWindow = (PtdMainWindow *)(*this)[i];
			if(pWindow !=NULL)
				delete pWindow;
		}
	}	
};
#endif
