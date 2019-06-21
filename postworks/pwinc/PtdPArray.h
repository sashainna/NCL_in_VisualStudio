/*********************************************************************
**  NAME:  PtdPArray.h
**  Description:
**				all member function and variable for class PtdPArray
**				Represent an array of pointers
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdPArray.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:32
*********************************************************************/
#ifndef PTDPARRAYH
#define PTDPARRAYH 

#include <stdio.h> 
 
 
class	PtdPArray 
{ 
protected: 
 
	int		m_nSize;		 
	int		m_nInitialSize;		 
	int		m_nMaxSize;	 
	int     	m_nCurrentIndex;	 
	void**		m_pData; 
 
	void		Resize(); 
	void		Resize(int newSize); 
 
	void		Deallocate(); 
 
 
public: 
 
	PtdPArray(); 
	PtdPArray(int initSize); 
 
	~PtdPArray(); 
 
 
 
	int		Size() const 
				{ return m_nSize; } 
	/* 
 	 * Function:	[] 
 	 * 
	 * Description:	Get the pointer to the [i]th  element 
 	 * 
 	 */ 
	void*		operator[](int i) const; 
 
 
	/* 
 	 * Function:	Set 
 	 * 
	 * Description:	Set/Replace an element in the Array 
 	 * 
 	 * Input:	Void * 
 	 * 
 	 */ 
	void	Set(const void *pNewData, int position); 
  
 
	/* 
 	 * Function:	First 
 	 * 
	 * Description:	Get the pointer to the first element 
 	 * 
 	 */ 
	void*		First(); 
 
 
	/* 
 	 * Function:	Last 
 	 * 
	 * Description:	Get the pointer to the last element 
 	 * 
 	 */ 
	void*		Last() const; 
 
 
    /* 
     * Function:		SetCurrentIndex		 
     * 
     * Description:		Sets the current array index to the specified 
     *					value. 
     *					  
     * Input:			index  
     * 
 	 * Output: 
 	 * 
 	 * Warnings/Notes:	This function will facilitate repeated invocation 
 	 *					of the 'Next' function. 
 	 * 
 	 */ 
	void SetCurrentIndex(const int index); 
 
 
	/* 
 	 * Function:		Next		 
	 * 
 	 * Description:		This function returns the next element corresponding 
 	 *					to m_nCurrentIndex. 
 	 *					  
 	 * 
 	 * Warnings/Notes:	m_nCurrentIndex is incremented by 1. If the 
 	 *					CurrentIndex happens to be set to the last  
 	 *					element in the array, this function will return 
 	 *					null pointer, indicating end of array, 
 	 *					without updating the CurrentIndex. 
 	 *					It is the caller's responsibility to avoid going 
 	 *					into an infinite loop and check null pointer. 
 	 *					 
 	 * 
 	 */ 
	virtual void* Next(); 
 
 
	/* 
 	 * Function:	insert 
 	 * 
	 * Description:	Insert an element into the Array 
 	 * 
 	 * Input:	Void * 
 	 * 
 	 */ 
	virtual void	insert(const void *pNewData, int position = 0L); 
 
 
	/* 
 	 * Function:	append 
 	 * 
	 * Description:	append an element to the end of the Array 
 	 * 
 	 * Input:	Void * 
 	 * 
 	 */ 
	virtual void	append(const void *pNewData); 
 
	// append another array 
	virtual	void	append(PtdPArray *arrayToBeAppended); 
 
 
	/* 
 	 * Function:	remove 
 	 * 
	 * Description:	remove an element from the Array 
 	 * 
 	 * Input:	void *Element to be removed  
 	 * 		(or) position i 
 	 * 
 	 */ 
			void	remove(const int position); 
	virtual void	remove(const void *pDataToBeRemoved); 
 
 
	/* 
 	 * Function:	IsInArray 
 	 * 
	 * Description:	Is the element in the array  
 	 * 
 	 * Input:	void* Element to be checked 
 	 * 
 	 * Output:	Boolean 
 	 * 
 	 * Warnings/Notes: -1 returned as position, for FALSE return 
 	 */ 
	int  	IsInArray(const void *pCheckElement,  
						     	int &position) const; 
 
 	/* 
 	 * Function:	Common 
 	 * 
	 * Description:	find the common element across arrays  
 	 * 
 	 * Input:	PtdPArray* 
 	 * 
 	 * Output:	void* commonElement (first common element) 
 	 * 
 	 * Warnings/Notes: 
 	 */ 
	void*  		Common(const PtdPArray &secondArray) const; 
 
 
	/* 
 	 * Function:	Dump 
 	 * 
	 * Description:	Dump the object into a file and check for validity 
 	 * 
 	 * Input:	File*	to dump into the file  
	 *		MsShort	- 0 check only 
	 *			- 1 check and dump 
	 *			- 2 Extensive check and dump 
 	 */ 
	virtual void	Dump(FILE *fptr, int LevelFlag = 1) const; 
	 
	 
	/* 
 	 * Function:	DeallocateElements 
 	 * 
	 * Description:	Deallocate elements/objects pointer by data pointers.  
	 *		This will NOT be called automatically by the destructor 
 	 *		and hence has to be specifically called by the owner 
	 *		sussystem to deallocate the elements/objects. This func 
	 *		need not be called, if the owner subsystem has other 
	 *		mechanisms to delete the elements/objects. 
	 * 
	 * Note/Warning: Callers have to make sure the elements are not  
	 *		deleted earlier or twice. 
 	 */ 
	void		DeallocateElements(); 
 
 
 	/* 
 	 * Function:	Empty 
 	 * 
	 * Description:	Empty the array 
	 * 
 	 */ 
	void	Empty(); 
 
 
	// internal test function 
	static	void	Test(); 
	 
 
 	/* 
 	 * Function:	Assignment = operator (copy arrays) 
 	 * 
	 * Description:	copy the array 
	 * 
 	 */ 
	void	operator=(const PtdPArray&); 
	  
 
 	/* 
 	 * Function:	Copy the elements (Caution!) 
 	 * 
	 * Description:	This is a special purpose function  
	 *				This just copies the m_pData 
	 *	 
	 * Warning:		Caller has to make sure there is room 
	 *				in the array. Otherwise you will have exception 
 	 */ 
	void	CopyElements(const PtdPArray&); 
	  
 
 	/* 
 	 * Function:	Reverse 
 	 * 
	 * Description:	reverse the array 
	 * 
 	 */ 
	void	Reverse(); 
 
};                  
#endif
