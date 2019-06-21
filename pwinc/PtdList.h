/*********************************************************************
**  NAME:  PtdList.h
**  Description:
**				all member function and variable for class PtdList
**				
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdList.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:30
*********************************************************************/
#ifndef PTDLISTH
#define PTDLISTH

#include "PtdNode.h" 
 
class PtdList
{ 
protected: 
    
   //data members 
   PtdNode* m_pHead; 
   PtdNode* m_pTail; 
   PtdNode* m_pCurrent; 
 
 
	private:
	 
   void SetHead(PtdNode* inNode) 
   { 
      m_pHead = inNode; 
   } 
 
   void SetTail(PtdNode* inNode) 
   { 
      m_pTail = inNode; 
   } 
          
	public: 
    
   PtdList() 
   { 
      m_pHead = 0; 
      m_pTail = 0; 
      m_pCurrent = 0; 
   } 
 
   ~PtdList(); 
 
	void  RemoveAll();	 
   
   PtdNode* Head() 
   { 
      m_pCurrent = m_pHead; 
      return m_pHead; 
   } 
 
   PtdNode* Current() 
   { 
      return m_pCurrent; 
   } 
 
   // First is same as Head, just to be consistent with Arrays 
	PtdNode* First() 
	{ 
		return Head(); 
	} 
 
   PtdNode* Tail()const 
   { 
      return m_pTail; 
   } 
             
   // get next node 
   // can only be used sequencially after Head or First func 
   PtdNode*  Next(); 

   // check if the list is at End of List 
   // can only be used in sequencial reading like Head() and Next() 
   // current node pointer is important fr this function to work 
   int   EndOfList() const 
   { 
      if (m_pCurrent == m_pTail) 
            return 1; 
      else  
		return 0; 
   } 
 
   virtual void insertHead(PtdNode*); 
    
   virtual void append(PtdNode*);     
    
   virtual void RemoveHead(); 
    
   virtual void RemoveTail(); 
      
   int   Count()const; 
   int   Size() const 
            { return Count(); } 
 
 
   /* 
    * Function:      Remove    
    * 
    * Description:      Remove reference node 
    * 
    * Input:         Node to be removed 
    * 
    * Output:         
    * 
    * Warnings/Notes:  
    */    
   virtual void remove(PtdNode*); 
 
   virtual void Empty(); 
 
   /* 
    * Function:      insertBefore    
    * 
    * Description:   Insert new node before reference node   
    * 
    * Input:         newNode: pointer to new node 
    *             refNode: pointer to reference node 
    * 
    * Output:         
    * 
    * Warnings/Notes:     
    *                  
    */  
   virtual void insertBefore(PtdNode*, PtdNode*); 
 
   /* 
    * Function:      insertAfter  
    * 
    * Description:      Insert new node after reference node 
    * 
    * Input:         newNode: pointer to new node 
    *             refNode: pointer to reference node 
    * 
    * Output:         
    * 
    * Warnings/Notes:   
    */ 
   virtual void insertAfter(PtdNode*, PtdNode*); 
 
}; 
 
/* 
 * Class Name:    PtdPList 
 * 
 * Description:   Represent a doubly linked list of pointers 
 * 
 * Notes/Warnings:   This class is derived from PtdList 
 * 
 */ 
 
 
 
class PtdPList : public PtdList 
 
{ 
public:   
 
   PtdPNode* Head() 
   {   
      return (PtdPNode*) PtdList::Head(); 
   } 
 
   void* First() 
   { 
      PtdPNode* firstNode = Head(); 
      if (firstNode != 0) 
         return firstNode->Data(); 
      else  return 0; 
   } 
 
   void* Next() 
   { 
      PtdPNode* nextNode = (PtdPNode*) PtdList::Next(); 
      if (nextNode != 0) 
            return nextNode->Data(); 
      else  return 0; 
   } 
 
   void* Current() 
   { 
      PtdPNode* curNode = (PtdPNode*) PtdList::Current(); 
      if (curNode != 0) 
            return curNode->Data(); 
      else  return 0; 
   } 
 
   virtual void InsertHead(const void* data) 
   { 
      PtdPNode* inNode = new PtdPNode(); 
      inNode->SetData(data); 
      PtdList::insertHead(inNode); 
   } 
    
   PtdPNode* Tail()const 
   { 
      return (PtdPNode*) PtdList::Tail(); 
   } 
    
   void Append(const void* data) 
   { 
      PtdPNode* inNode = new PtdPNode(); 
      inNode->SetData(data); 
      PtdList::append(inNode); 
   }  
   
   virtual void InsertBefore(PtdPNode* refNode, const void* data) 
   { 
      PtdPNode* inNode    = new PtdPNode(); 
      inNode->SetData(data); 
      PtdList::insertBefore(refNode, inNode); 
   } 
 
   virtual void InsertAfter(PtdPNode* refNode, const void* data) 
   { 
      PtdPNode* inNode = new PtdPNode(); 
      inNode->SetData(data); 
      PtdList::insertAfter(refNode, inNode); 
   }
   virtual void Remove(const void* data);
   
   // find and return a specific node based on data
   virtual void *Find(const void *data);
};                
#endif
