/*********************************************************************
**  NAME:  PtdNode.h
**  Description:
**				all member function and variable for class PtdNode
**			
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdNode.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:31
*********************************************************************/
#ifndef PTDNODEH
#define PTDNODEH
 
class PtdNode
{ 
 
	friend class PtdList; 
protected: 
 
	//data memebers 
	PtdNode* m_pPrevious; 
	PtdNode* m_pNext; 
	 
public: 
 
	//constructor 
	PtdNode() 
	{ 
		m_pPrevious = 0; 
		m_pNext = 0; 
	} 
	 
	//get node immediately before the current node 
	PtdNode* Previous()const 
	{	 
		return m_pPrevious; 
	} 
 
	//get node immediately after the current node 
	PtdNode* Next()const 
	{ 
		return m_pNext; 
	} 
 
	//check to see if the current node is the head node 
	int IsHead()const 
	{ 
		return (m_pPrevious == 0); 
	} 
 
	//check to see if the current node is the tail node 
	int IsTail()const 
	{ 
		return (m_pNext == 0); 
	}
	// Remove the node
	int Remove() 
	{ 
		PtdNode* beforeNode; 
		PtdNode* afterNode; 
		 
		if (m_pPrevious == 0) 
		{ 
			return 1; 
		} 
		if (m_pNext == 0) 
		{ 
			return 2; 
		} 
 
		beforeNode = m_pPrevious; 
		afterNode  = m_pNext; 
		beforeNode->m_pNext = m_pNext; 
		afterNode->m_pPrevious = m_pPrevious;		 
		delete this;	  
		return 0; 
	} 
}; 
 
class PtdPNode : public PtdNode 
{ 
 
protected: 
 
	void* m_pData; 
 
public: 
	 
	PtdPNode() 
	{ 
		m_pData = 0; 
	}; 
	 
	PtdPNode* Previous()const 
	{ 
		return (PtdPNode*) PtdNode::Previous(); 
	} 
 
	PtdPNode* Next()const 
   { 
		return (PtdPNode*) PtdNode::Next(); 
	} 
	 
	void* Data() 
	{ 
		return m_pData; 
	} 
	 
	void SetData(const void* inData) 
	{ 
		m_pData = (void*) inData; 
	}		 
 
};									             
#endif
