/*********************************************************************
**  NAME:  PtdFileOpenCommand.h
**  Description:
**				all member function and variable for class PtdFileOpenCommand
**				which encapsulation of a file open dialog
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdFileOpenCommand.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:30
*********************************************************************/
#include "PtdNoUndoCommand.h"

#include <Xm/Xm.h>

typedef void (*FileCallback)(void *,char *, char *);

typedef struct
{
	char *title;
	char *pattern;
	char *dirMask;
	char *dirLabel;
	char *fileLabel;
	char *filterLabel;
	void *cPtr;
	void *cmdPtr;
}fileDlgResStruct;


class PtdFileOpenCommand : public PtdNoUndoCommand 
{
    
	private:
    
	/*
	 * Function:	FileSelectedCallback
	 *
	 * Description: callback function for ok button
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning:
	 *
	 */
    static void FileSelectedCallback(Widget, XtPointer, XtPointer);
    
	/*
	 * Function:	CancelCallback
	 *
	 * Description: callback function for cancel button
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning:
	 *
	 */
	static void CancelCallback(Widget, XtPointer userData, XtPointer);
		
	protected:
	
    fileDlgResStruct	*m_pFilSel;
    FileCallback	m_Callback; 
    Widget			m_wFileBrowser;
    Widget			m_wShellWidget;
	
	/*
	 * Function:	FileSelected
	 *
	 * Description: 
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning:
	 *
	 */
    virtual void FileSelected (char *, char *);

	/*
	 * Function:	DoIt
	 *
	 * Description: Called by base class
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning:
	 *
	 */
/*
    void DoIt(); 
   */ 

	public:
    
    void DoIt(); 
	/*
	 * Function:	UnManage, Manage
	 *
	 * Description: managing and unmanaging the widget
	 *
	 * Input:		
	 *
	 * Output: 
	 *
	 * Warning: base member function override
	 *
	 */
	virtual void UnManage();
	virtual void Manage();

	/*
	 * Function:	PtdFileOpenCommand
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
    PtdFileOpenCommand(char *,int,fileDlgResStruct *, FileCallback);
};
