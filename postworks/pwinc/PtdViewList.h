/*********************************************************************
**  NAME:  PtdViewList.h
**  Description:
**				all member function and variable for class PtdViewList
**				A doubly linked list of PtdView pointers
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdViewList.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:33
*********************************************************************/
#include "PtdList.h"

class	PtdView;

class PtdViewList : public PtdPList
{
	public:	

	/*
	 * Function:	Head
	 *
	 * Description: get the head node
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
 	PtdPNode* Head()
	{	
		return (PtdPNode*) PtdPList::Head();
	}

	/*
	 * Function:	First
	 *
	 * Description: get the first data
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
	PtdView *	First()
	{
		return (PtdView*) PtdPList::First();
	}
	
	/*
	 * Function:	Next
	 *
	 * Description: get the next node data
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
	PtdView*	Next()
	{
		return (PtdView*) PtdPList::Next();
	}

	/*
	 * Function:	Tail
	 *
	 * Description: get the tail node
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
	PtdView* Tail()const
	{
		return (PtdView*) PtdPList::Tail();
	}
	
	/*
	 * Function:	Empty
	 *
	 * Description: Empty the list set. Involves explicitly deleting
	 *				the list elements
	 *
	 * Input:		
	 *
	 * Output:		
	 *
	 * Warning:
	 *
	 */
	virtual void Empty()
	{
		PtdView* pView;
		PtdPNode* pViewNode=Head();

		while (pViewNode != NULL)
		{
			pView = (PtdView *) pViewNode->Data();
			if (pView != NULL)
				delete pView;
      	pViewNode = pViewNode->Next();
		}
		PtdPList::Empty();
	}
};

