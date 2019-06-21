/*********************************************************************
**	FILENAME: lmparse.c
**	CONTAINS:		
**		ul_modal_check
**		ul_modal_toggle
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lmparse.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:19
*********************************************************************/

#include "lcom.h"
#include "lumb.h"
#include "usysdef.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "ustdio.h"
#include "nccs.h"
#include "nclfc.h"

extern char uw_color_name[64][96];
/*********************************************************************
**	 I_FUNCTION : ul_modal_check(buf,ityp,ctyp,cmsg)
**			This function parses a modals record and returns
**			the type of record & the values specified.
**	 PARAMETERS	
**		 INPUT  :  buf = Modals record.
**		 OUTPUT :  ityp = 	0 = Comment.
**					1 = Main Modals form.
**					2 = Sub modal.
**			ctyp = Modal begin defined.
**			cmsg = Modal ctyp's parameter.
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/

int ul_modal_check (buf,ityp,ctyp,cmsg)
	char *buf,ctyp[],cmsg[];
	int *ityp;
{
	int i,status,is,inc,nc;
/*
.....Assume success
*/
	status = UU_SUCCESS;
	*ityp = 0;
/*
.....Check type of record
.....Main form, form entry or comment
*/
	if (((nc=strlen(buf)) == 0) || (strcmp(buf,"\n") == 0)) goto done;
	for (i=0;i<nc;i++)
	{
		if (buf[i] != ' ' && buf[i] != '\t') break;
	}
	if (i >= nc || buf[i] == '!') goto done;
	if (buf[i] == '#') *ityp = 1;
	else if (buf[i] == '/') *ityp = 2;
	else goto failed;
/*
.....Get modal to define
*/
	inc = i;
	is = 0;
	ctyp[0] = '\0';
	for (i=i+1;i<nc;i++)
	{
		if (buf[i] == buf[inc] && i > (inc+1)) break;
/*
...Why 38? need read more
...changed by yurong
*/
/*		if (is == 38) goto failed;   */
/*		if (is == 78) goto failed;   */
/*		if (is == 255) goto failed;  */
		if (is == 2048) goto failed;
		ctyp[is] = buf[i];
		is++;
	}
	if (i >= nc) goto failed;
	ctyp[is] = '\0';
/*
.....keep the letter case as it is
.....Yurong 3/10/00
*/
/*	ul_to_upper(ctyp); */

/*
.....Get modal's parameter
.....First search for 1st non-blank
*/
	for (inc=i+1;inc<nc;inc++)
	{
		if (buf[inc] != ' ' && buf[inc] != '\t') break;
	}
	cmsg[0] = '\0';
	if (inc >= nc) goto done;
	is = 0;
	for (i=inc;i<nc;i++)
	{
/*
...Why 38? need read more
...changed by yurong
*/
/*		if (is == 38) goto failed;   */
/*		if (is == 78) goto failed;   */
/*		if (is == 255) goto failed; */
		if (is == 2048) goto failed;
		cmsg[is] = buf[i];
		is++;
	}
	cmsg[is] = '\0';
	goto done;
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**	 I_FUNCTION : ul_modal_toggle(cmsg,toggle,max,var)
**			This function sets a toggle form entry.
**	 PARAMETERS	
**		 INPUT  :  cmsg = Input toggle value.
**			toggle = Acceptable toggle value.
**			max = # of acceptable toggles.
**		 OUTPUT :  var = toggle chosen.
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
int ul_modal_toggle(cmsg,toggle,max,var)
	char *cmsg,toggle[][64];
	int max,*var;
{
	char buf[80];
	int i,status;
/*
.....Assume success
*/
	status = UU_SUCCESS;
/*
.....Convert input to uppercase
*/
	if (*cmsg == '\0') goto done;
	strcpy (buf,cmsg);
	ul_to_upper(buf);
/*
.....Check for recognized toggle value
*/
	for (i=0;i<max;i++)
	{
		ul_to_upper(buf);
		if (strcmp(buf,toggle[i]) == 0) break;
	}
	if (i >= max) goto failed;
	*var = i;
	goto done;
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**	 I_FUNCTION : ul_modal_color(cmsg, color, addtl_colors, ncolors)
**			This function parse a color string.
**	 PARAMETERS	
**		 INPUT  :  cmsg = Input color string with (or without) starting "*".
**			addtl_colors = Acceptable additonal color name.
**			ncolors = # of additonal color.
**		 OUTPUT :  
**			color = output color index value.
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
ul_modal_color(cmsg, color, addtl_colors, ncolors)
	char *cmsg, addtl_colors[][96];
	int ncolors,*color;
{
	char buf[96], buf1[96];
	int i,status;
/*
.....Assume success
*/
	status = UU_SUCCESS;
	if (*cmsg == '\0') return -1;
	if (cmsg[0]=='*')
		strcpy(buf, &(cmsg[1]));
	else
		strcpy(buf, cmsg);
/*
.....remove trailing space and '\t'
*/
	for (i=strlen(buf); i>0; i--)
	{
		if ((buf[i-1]==' ')||(buf[i-1]=='\t'))
			buf[i-1] = '\0';
		else
			break;
	}
/*
.....Convert input to uppercase
*/
	if (*buf == '\0') return -1;
	ul_to_upper(buf);
/*
.....Check for basic/custom colorname value
*/
	for (i=0;i<64;i++)
	{
		if (strcmp(buf, uw_color_name[i]) == 0) break;
	}
	if (i <= 63) 
	{
		*color = i;
		return status;
	}
	if (ncolors<=0)
		return -1;
	for (i=0;i<ncolors;i++)
	{
		strcpy(buf1, addtl_colors[i]);
		ul_to_upper(buf1);
		if (strcmp(buf, buf1) == 0) break;
	}
	if (i < ncolors) 
	{
		*color = -1*(i+1);
		return status;
	}
	return -1;
}
