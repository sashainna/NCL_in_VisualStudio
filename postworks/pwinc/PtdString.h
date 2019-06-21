/*********************************************************************
**  NAME:  PtdString.h
**  Description:
**				all member function and variable for class PtdString
**				Defines standard string functions
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdString.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:32
*********************************************************************/
#ifndef PTDSTRINGH
#define PTDSTRINGH
 
class PtdString 
{ 
	struct strCl 
	{	 
		char* dataPtr; //pointer to data 
		int  refCnt;  //reference count to strCl 
		  
		 //constructor initializes ref.count to 1 and nulls data ptr 
		strCl()    
			{    dataPtr = 0; 
				refCnt=1; 
			}    
	}; 
	strCl* strClPtr; 
 
public: 
	//constructor 
	PtdString(); 
	 
	//copy constructor 
	PtdString(const char* aIn); 
	PtdString(const PtdString& sIn); 
 
   	//length constructor 
   	PtdString(const int &lLength); 
 
	//destructor 
	~PtdString(); 
 
 
	// automatic conversion operator to char * 
	operator char*() 
			{ return strClPtr->dataPtr; }
			 
	// automatic conversion operator to const char * 					 
	operator const char*() 
			{ return strClPtr->dataPtr; } 
 
	//define '=' operator 
	PtdString& operator=(const char*); 
 
	//define '=' operator 
	PtdString& operator=(const PtdString&); 
 
	//compare two strings: one is a PtdString object, the other 
	//is a char* 
	friend int operator==(const PtdString&, const char*); 
 
	//compare two strings: both are PtdString objects 
	friend int operator==(const PtdString&, const PtdString&); 
 
	//compare two strings: one is a PtdString object, the other 
	//is a char* 
	friend int operator!=(const PtdString&, const char*); 
 
	//compare two strings: both are PtdString objects 
	friend int operator!=(const PtdString&, const PtdString&); 
 
	//get size of string 
	int Size()const; 
 
	//extract character from specified 'pos' in the string 
	char* operator()(const int pos)const; 
 
	//extract substring of specified length 
	PtdString Substr(const int start, const int  len = -1)const; 
 
	//concatenate 2 strings 
	PtdString operator+(const PtdString& sIn)const; 
 
	// strip front and back blanks 
	PtdString StripBlanks(); 
 
	// strip all blanks	(not implemented yet) 
	//PtdString StripAllBlanks(); 
 
	// internal test 
	static	void	Test();	 
 
	// check if the name match the pattern 
	int	Matches(char *,  int IgnoreCase = 0); 
 
}; //end of class PtdString definition             
#endif
