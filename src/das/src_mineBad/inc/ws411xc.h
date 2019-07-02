/*********************************************************************
**    NAME         :  ws411xc.h
**       CONTAINS:		
**			Tek 411x workstation macros to issue 411x hardware commands.
**			This only compiles if UU_DEBUG is not defined. Otherwise, ws411x.c
**			compiles subroutines.
**			uw_411xSET_TEXT_INDEX(indx)
**			uw_411xBEGIN_SEG(segno)
**			uw_411xEND_SEG()
**			uw_411xDELETE_SEG(segno)
**			uw_411xSET_SEG_VIS(segno,vis)
**			uw_411xSELECT_VIEW(viewno)
**			uw_411xSET_WINDOW(ll,ur) -- issue esc RW cmd 
**			uw_411xSET_VIEWPORT(ll,ur) -- issue esc RW cmd.
**			uw_411xSET_GIN_AREA(dev,typ,ll,ur) --	issue esc IV cmd
**			uw_411xSET_VIEW_ATTS(surf,wipe,border)
**			uw_411xSET_PICKID(n) -- issue esc MI cmd.
**			uw_411xENABLE_GIN(devfn,n); -- issue esc IE cmd.
**			uw_411xDISABLE_GIN(devfn) -- issue esc ID cmd.
**			uw_411xSET_GIN_CURSOR(devfn,cseg) -- isue esc IC cmd.
**			uw_411xSET_GIN_DISP_START(devfn,xy) -- issue esc IX cmd.
**			uw_411xSET_SEG_DET(seg,det) - issue esc SD cmd.
**			uw_411xENAB_DIALOG_AREA(n) - issue 3KA cmd.
**			uw_411xPASSIGN(dev,str)  - issue esc PA cmd.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ws411xc.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:08
*********************************************************************/

#ifndef WS41XXCH


/************* uw_411xSET_TEXT_INDEX(indx) ****************/
#define uw_411xSET_TEXT_INDEX(indx) \
	{uu_ttput(uw_411x.ttfd,"\033MT",3); uw_ws411xinteg(indx);}

/************ uw_411xBEGIN_SEG(segno) **************/
#define uw_411xBEGIN_SEG(segno) \
	{uu_ttput(uw_411x.ttfd,"\033SO",3); uw_ws411xinteg(segno);}

/************** uw_411xEND_SEG() ***************/
#define uw_411xEND_SEG() \
	{uu_ttput(uw_411x.ttfd,"\033SC",3);}

/****************** uw_411xDELETE_SEG(segno) ***************/
#define uw_411xDELETE_SEG(segno) \
	{uu_ttput(uw_411x.ttfd,"\033SK",3); uw_ws411xinteg(segno);}
	
/************ uw_411xSET_SEG_VIS(segno,vis) **************/
#define uw_411xSET_SEG_VIS(segno,vis) \
	{uu_ttput(uw_411x.ttfd,"\033SV",3); uw_ws411xinteg(segno);	 uw_ws411xinteg(vis);}

/************* uw_411xSELECT_VIEW(viewno) ****************/
#define uw_411xSELECT_VIEW(viewno) \
	{uu_ttput(uw_411x.ttfd,"\033RC",3); uw_ws411xinteg(viewno); \
	uw_411x.curr_view_surf = viewno;}

/************* uw_411xSET_WINDOW(ll,ur) ****************/
#define uw_411xSET_WINDOW(ll,ur) \
	{uu_ttput(uw_411x.ttfd,"\033RW",3); uw_411xxy(ll); uw_411xxy(ur);}
	
/************* uw_411xSET_VIEWPORT(ll,ur) ****************/
#define uw_411xSET_VIEWPORT(ll,ur)\
	{uu_ttput(uw_411x.ttfd,"\033RV",3); uw_411xxy(ll); uw_411xxy(ur);}
	
/************* uw_411xSET_GIN_AREA(dev,typ,ll,ur) ****************/
#define uw_411xSET_GIN_AREA(dev,typ,ll,ur) \
	{uu_ttput(uw_411x.ttfd,"\033IV",3); uw_ws411xinteg(dev); uw_ws411xinteg(typ); \
	uw_411xxy(ll); uw_411xxy(ur);}
	
/**************** uw_411xSET_VIEW_ATTS(surf,wipe,border) ***************/
#define uw_411xSET_VIEW_ATTS(surf,wipe,border) \
	{uu_ttput(uw_411x.ttfd,"\033RA",3); \
	uw_ws411xinteg(surf); uw_ws411xinteg(wipe); uw_ws411xinteg(border);}
	
/*************** uw_411xSET_PICKID(n) *****************/
#define uw_411xSET_PICKID(n) \
	{uu_ttput(uw_411x.ttfd,"\033MI",3); uw_ws411xinteg(n);}
	
/******************** uw_411xENABLE_GIN(devfn,n) ****************/
#define uw_411xENABLE_GIN(devfn,n) \
	{uu_ttput(uw_411x.ttfd,"\033IE",3); \
	uw_ws411xinteg(devfn); uw_ws411xinteg(n);}
	
/******************** uw_411xDISABLE_GIN(devfn) ***************/
#define uw_411xDISABLE_GIN(devfn) \
	 {uu_ttput(uw_411x.ttfd,"\033ID",3); uw_ws411xinteg(devfn);	}

/****************** uw_411xSET_GIN_CURSOR(devfn,cseg) *************/
#define uw_411xSET_GIN_CURSOR(devfn,cseg) \
	{uu_ttput(uw_411x.ttfd,"\033IC",3); \
	uw_ws411xinteg(devfn);  uw_ws411xinteg(cseg);}
	
/******************* uw_411xSET_GIN_DISP_START(devfn,xy) **************/
#define uw_411xSET_GIN_DISP_START(devfn,xy) \
	{uu_ttput(uw_411x.ttfd,"\033IX",3); uw_ws411xinteg(devfn); uw_411xxy(xy);}
	
/********** uw_411xSET_SEG_DET(seg,det) ************/
#define uw_411xSET_SEG_DET(seg,det) \
	{uu_ttput(uw_411x.ttfd,"\033SD",3); uw_ws411xinteg(seg); uw_ws411xinteg(det);}

/************ uw_411xENAB_DIALOG_AREA(n) ******************/
#define uw_411xENAB_DIALOG_AREA(n) \
	{uu_ttput(uw_411x.ttfd,"\033KA",3); uw_ws411xinteg(n); uw_411x.ansienbl=n;}

/************ uw_411xPASSIGN(dev,str) *******************/
#define uw_411xPASSIGN(dev,str) \
{ int len; \
	uu_ttput(uw_411x.ttfd,"\033PA",3); len=strlen(dev); \
	uw_ws411xinteg(len); uu_ttput(uw_411x.ttfd,dev,len); \
	len=strlen(str); \
	uw_ws411xinteg(len); uu_ttput(uw_411x.ttfd,str,len); }

#define WS41XXCH
#endif
