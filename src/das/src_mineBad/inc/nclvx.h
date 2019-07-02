/*********************************************************************
**    NAME         :  nclvx.h
**       CONTAINS: NCL/VX defines.
**    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**        nclvx.h , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**        10/26/15 , 10:06:49
*********************************************************************/

#ifndef NCL_VXDEF

#include "mfort.h"
#include "nconst.h"

struct NCL_macro_list_data
{
	UM_real8 dvalue;
	UM_real8 avalue;
	UM_int2 iflg;
	char plab[NCL_MAX_LABEL+1];
	UM_int4 psub;
/*
.....default value
*/
	char dlab[NCL_MAX_LABEL+1];
	UM_int4 dsub;
	UM_int2 dclas;
	UM_int2 detype;
/*
.....assigned value
*/
	char alab[NCL_MAX_LABEL+1];
	UM_int4 asub;
	UM_int2 iclas;
	UM_int2 ietype;
	UM_int4 keyscl;
/*
.....retained/saved value
*/
/*
......the retain value here can't just use a real value to save now
......because the label value now extend to 64 chars, so we need
......a real value for value and rlab &rsub for save the retain label
......and subnum
*/
	UM_real8 rvalue;
	char rlab[NCL_MAX_LABEL+1];
	UM_int4 rsub;
	UM_int2 rtype;
/*
.....used for prompt or description
*/
	UM_int2 pclas;
	char prompt[41];
	int count;
/*
.....word_list/geo_list use one string but seperate with 24 chars
.....I don't use pointer to assign word list because
.....the word list is not long (480 + 1 chars: 20 word at most)
.....for whole list
*/
	char wrdlist[481];
	UM_int2 geomflag;
	UM_real8 min, max;
	UM_int2 prsvalue, lenvalue;
/*
.....RETAIN/LABEL/NUM
*/	
	int substr;
/*
.....NOW/NEXT
*/
	int pflag;
/*
.....SELECT/ONCE
*/
	int psel; 
/*
.....SELECT color
*/
	int pclr; 
};

struct NCL_macro_define_data
{
/*
......PROMPT value
*/
	char classname[21];
/*
.....OUT/OMIT
*/
	int outflag;
/*
.....DEFAULT/RETAIN
*/
	int dispflag;
/*
.....used for prompt or description
*/
	char prompt[41];
/*
.....if the retain value saved
*/
	short rvsaved;
/*
......header value
*/
	UM_int4 maclin;
	UM_int4 next;
	UM_int4 termln;
	UM_int2 mode;
	UM_int2 pr_rec;
	UM_int2 pr_ele;
	int callin;
};

struct NCL_macro_datakey
{
	int key;
	short indx;
};

struct NCL_macro_call_data
{
	char macnam[NCL_MAX_LABEL];
	char *macptr;
	struct NCL_macro_datakey *tmpdkey;
};

#define NCL_VXDEF
#endif
