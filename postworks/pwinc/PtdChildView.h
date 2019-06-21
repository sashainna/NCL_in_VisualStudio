/*********************************************************************
**  NAME:  PtdChildView.h
**  Description:
**				all member function and variable for class PtdChildView
**				
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdChildView.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:28
*********************************************************************/
#ifndef PTDCHILDVIEWH
#define PTDCHILDVIEWH

#include "PtdView.h"
#include <Xm/Xm.h>
#include "PtdMainView.h"


class PtdChildView : public PtdView
{
	public:
		PtdChildView(char *, int type = 1);
		~PtdChildView();
		void InitData();
		virtual void InitialUpdate();
		virtual void  Update(PtdView *view = NULL);
		void Set_ftype(int ftype);
};
#endif

