/*********************************************************************
**    NAME         :  ws410xc.h
**       CONTAINS:		
**			Tek 41xx workstation macros to issue 41xx hardware commands.
**			This only compiles if UU_DEBUG is not defined. Otherwise, ws410x.c
**			compiles subroutines.
**			uw_41xxSET_TEXT_INDEX(indx)
**			uw_41xxBEGIN_SEG(segno)
**			uw_41xxEND_SEG()
**			uw_41xxDELETE_SEG(segno)
**			uw_41xxSET_SEG_VIS(segno,vis)
**			uw_41xxSELECT_VIEW(viewno)
**			uw_41xxSET_WINDOW(ll,ur) -- issue esc RW cmd 
**			uw_41xxSET_VIEWPORT(ll,ur) -- issue esc RW cmd.
**			uw_41xxSET_GIN_AREA(dev,typ,ll,ur) --	issue esc IV cmd
**			uw_41xxSET_VIEW_ATTS(surf,wipe,border)
**			uw_41xxSET_PICKID(n) -- issue esc MI cmd.
**			uw_41xxENABLE_GIN(devfn,n); -- issue esc IE cmd.
**			uw_41xxDISABLE_GIN(devfn) -- issue esc ID cmd.
**			uw_41xxSET_GIN_CURSOR(devfn,cseg) -- isue esc IC cmd.
**			uw_41xxSET_GIN_DISP_START(devfn,xy) -- issue esc IX cmd.
**			uw_41xxSET_SEG_DET(seg,det) - issue esc SD cmd.
**			uw_41xxENAB_DIALOG_AREA(n) - issue 3KA cmd.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ws410xc.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:08
*********************************************************************/

#ifndef WS41XXCH


/************* uw_41xxSET_TEXT_INDEX(indx) ****************/
#define uw_41xxSET_TEXT_INDEX(indx) \
	{uu_ttput(uw_41xx.ttfd,"\033MT",3); uw_ws410xinteg(indx);}

/************ uw_41xxBEGIN_SEG(segno) **************/
#define uw_41xxBEGIN_SEG(segno) \
	{uu_ttput(uw_41xx.ttfd,"\033SO",3); uw_ws410xinteg(segno);}

/************** uw_41xxEND_SEG() ***************/
#define uw_41xxEND_SEG() \
	{uu_ttput(uw_41xx.ttfd,"\033SC",3);}

/****************** uw_41xxDELETE_SEG(segno) ***************/
#define uw_41xxDELETE_SEG(segno) \
	{uu_ttput(uw_41xx.ttfd,"\033SK",3); uw_ws410xinteg(segno);}
	
/************ uw_41xxSET_SEG_VIS(segno,vis) **************/
#define uw_41xxSET_SEG_VIS(segno,vis) \
	{uu_ttput(uw_41xx.ttfd,"\033SV",3); uw_ws410xinteg(segno);	 uw_ws410xinteg(vis);}

/************* uw_41xxSELECT_VIEW(viewno) ****************/
#define uw_41xxSELECT_VIEW(viewno) \
	{uu_ttput(uw_41xx.ttfd,"\033RC",3); uw_ws410xinteg(viewno); \
	uw_41xx.curr_view_surf = viewno;}

/************* uw_41xxSET_WINDOW(ll,ur) ****************/
#define uw_41xxSET_WINDOW(ll,ur) \
	{uu_ttput(uw_41xx.ttfd,"\033RW",3); uw_41xxxy(ll); uw_41xxxy(ur);}
	
/************* uw_41xxSET_VIEWPORT(ll,ur) ****************/
#define uw_41xxSET_VIEWPORT(ll,ur)\
	{uu_ttput(uw_41xx.ttfd,"\033RV",3); uw_41xxxy(ll); uw_41xxxy(ur);}
	
/************* uw_41xxSET_GIN_AREA(dev,typ,ll,ur) ****************/
#define uw_41xxSET_GIN_AREA(dev,typ,ll,ur) \
	{uu_ttput(uw_41xx.ttfd,"\033IV",3); uw_ws410xinteg(dev); uw_ws410xinteg(typ); \
	uw_41xxxy(ll); uw_41xxxy(ur);}
	
/**************** uw_41xxSET_VIEW_ATTS(surf,wipe,border) ***************/
#define uw_41xxSET_VIEW_ATTS(surf,wipe,border) \
	{uu_ttput(uw_41xx.ttfd,"\033RA",3); \
	uw_ws410xinteg(surf); uw_ws410xinteg(wipe); uw_ws410xinteg(border);}
	
/*************** uw_41xxSET_PICKID(n) *****************/
#define uw_41xxSET_PICKID(n) \
	{uu_ttput(uw_41xx.ttfd,"\033MI",3); uw_ws410xinteg(n);}
	
/******************** uw_41xxENABLE_GIN(devfn,n) ****************/
#define uw_41xxENABLE_GIN(devfn,n) \
	{uu_ttput(uw_41xx.ttfd,"\033IE",3); \
	uw_ws410xinteg(devfn); uw_ws410xinteg(n);}
	
/******************** uw_41xxDISABLE_GIN(devfn) ***************/
#define uw_41xxDISABLE_GIN(devfn) \
	 {uu_ttput(uw_41xx.ttfd,"\033ID",3); uw_ws410xinteg(devfn);	}

/****************** uw_41xxSET_GIN_CURSOR(devfn,cseg) *************/
#define uw_41xxSET_GIN_CURSOR(devfn,cseg) \
	{uu_ttput(uw_41xx.ttfd,"\033IC",3); \
	uw_ws410xinteg(devfn);  uw_ws410xinteg(cseg);}
	
/******************* uw_41xxSET_GIN_DISP_START(devfn,xy) **************/
#define uw_41xxSET_GIN_DISP_START(devfn,xy) \
	{uu_ttput(uw_41xx.ttfd,"\033IX",3); uw_ws410xinteg(devfn); uw_41xxxy(xy);}
	
/********** uw_41xxSET_SEG_DET(seg,det) ************/
#define uw_41xxSET_SEG_DET(seg,det) \
	{uu_ttput(uw_41xx.ttfd,"\033SD",3); uw_ws410xinteg(seg); uw_ws410xinteg(det);}

/************ uw_41xxENAB_DIALOG_AREA(n) ******************/
#define uw_41xxENAB_DIALOG_AREA(n) \
	{uu_ttput(uw_41xx.ttfd,"\033KA",3); uw_ws410xinteg(n); uw_41xx.ansienbl=n;}

#define WS41XXCH
#endif
