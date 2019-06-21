/*********************************************************************
**  NAME:  PtdDialogCbData.h
**  Description:
**				all member function and variable for class PtdDialogCbData
**			
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdDialogCbData.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:29
*********************************************************************/
#ifndef PTDDIALOGCBDATAH
#define PTDDIALOGCBDATAH
                           
#include "PtdBase.h"

class PtdDialogManager;

typedef void (*PtdDialogCallback)(void *);

class PtdDialogCbData 
{
	private:
    
    PtdDialogManager  *m_pDialogManager;
    PtdDialogCallback  m_Ok;
    PtdDialogCallback  m_Help;
    PtdDialogCallback  m_Cancel;
    void           *m_pClientData;
    
	public:
    
	/*
	 * Function:	PtdDialogCbData
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
    PtdDialogCbData ( PtdDialogManager *dialog, 
			void          *clientData,
			PtdDialogCallback ok,
			PtdDialogCallback cancel,
			PtdDialogCallback help)
    {
		m_pDialogManager = dialog;
		m_Ok            = ok;
		m_Help          = help;
		m_Cancel        = cancel;
		m_pClientData    = clientData;
    }
    
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
    PtdDialogManager  *DialogManager() { return m_pDialogManager; }
    PtdDialogCallback  Ok() { return m_Ok; }
    PtdDialogCallback  Help() { return m_Help; }
    PtdDialogCallback  Cancel() { return m_Cancel; }
    void           *ClientData() { return m_pClientData; }
};
#endif
