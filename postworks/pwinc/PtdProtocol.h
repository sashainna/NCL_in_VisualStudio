/*********************************************************************
**  NAME:  PtdProtocol.h
**  Description:
**          all member function and variable for class PtdProtocol
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdProtocol.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:32
*********************************************************************/
#ifndef PTDPROTOCOLH
#define PTDPROTOCOLH

#include "PtdString.h"
#include <Xm/Xm.h>

class PtdProtocol {
    
	protected:
    
	PtdString m_sWidgetName;
	Widget   m_wBaseWidget;    
   PtdString m_sClassName;
	
	PtdProtocol (char *);
    
	public:
    
		virtual ~PtdProtocol();
		virtual void Manage();
		virtual void UnManage();

		const Widget BaseWidget() { return m_wBaseWidget; }
		const PtdString BaseName() { return m_sWidgetName;}
		virtual const PtdString ClassName() { return m_sClassName;}
		virtual void ShowWindow();
};
#endif
