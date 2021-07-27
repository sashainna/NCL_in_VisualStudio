/********************************************************************* 
**    NAME         :  nudata.c
**       CONTAINS:
**      nclu_data_stmt
**
**    COPYRIGHT 2010 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nudata.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:06
*********************************************************************/
#include <string.h>
#include "dselmask.h"
#include "nclmplay.h"
#include "mdrel.h"
#include "mdpick.h"
#include "mxxx.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nkeywd.h"
#include "nclvx.h"
#include "nclupok.h"

/*
.....Action buttons
*/
#define FLFN 1
#define FVIW 5
#define FAPP 6
/*
.....Data File Action Buttons/Menu/Check Boxes
*/
#define FBLK 3
#define FVAL 4
#define FSFL 5
#define FSTT 6
#define FEFL 7
#define FEND 8
#define FSTF 11
#define FSTO 12

#define MXLAB NCL_MAX_LABEL_AND_SUBSCRIPT+1

static UU_LIST Ssurf;
static int Snsurf = 0, Sftyp;
static UU_LOGICAL Sinit;

static int choice=0,strvoc=0,strlab=0;
static int Scolor=8,Sflag=0,Stflag=0,Eflag=0,Storflag=0,Stype=0,Sblank=0;
static char Sload[UX_MAX_PATH_LEN],Label[MXLAB],Slabel[MXLAB],Svalue[MXLAB],
				Sline[MXLAB],Eline[MXLAB],Storvar[MXLAB],Sclassnm[21];

static UD_FSTAT OnSelect(),OnColor(),OnDesel(),OnBrowse();
static UD_FSTAT OnAction(),OnFSelect();
static int S_build_cmd();
static void S_free_geo();

/*********************************************************************
**    E_FUNCTION     : nclu_data_stmt()
**       Interface for creating a DATA statement consisting of picked
**       geometry.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_data_stmt()
{
	int nc,status;
	UU_LOGICAL cmdreject;
	UU_REAL rnum;
	UM_sgeo *geo;
/*
.....Set up form fields
*/
	static char traverse[] = {1,1,1, 1, 1,1,1};
	static char called[]   = {6,6,6, 6, 6,6,6};
	static char display[]  = {1,1,1, 1, 1,1,1};

	static UD_METHOD methods[] = {OnSelect,OnColor,OnDesel,UU_NULL,UU_NULL,
		OnAction,OnAction};

	static int *ans[] = {UU_NULL, &Scolor, UU_NULL, (int *)&Slabel, &Sflag,
		UU_NULL, UU_NULL};
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject != 0)	goto done;
/*
.....Initialize answers
*/
	uu_list_init(&Ssurf,sizeof(UM_sgeo),50,50);
	Sinit = UU_TRUE;
	Snsurf = 0;
	Slabel[0] = '\0';
/*
.....Get the Form input
*/
repeat:
	status = ud_form1("datastmt.frm",ans,ans,methods,called,display,traverse);
	if (status == -1) goto done;
/*
.....Output the DATA command
*/
	if (S_build_cmd() != UU_SUCCESS) goto repeat;
/*
.....End of routine
*/
done:
	S_free_geo();
	UD_UNMARK(cmdreject); 	
	return;
}

/*********************************************************************
**    E_FUNCTION     : nclu_data_file_stmt()
**       Interface for creating DATA statement(s) consisting of file
**       entries read from a comma delimited file.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_data_file_stmt()
{
	int nc,status;
	UU_LOGICAL cmdreject;
	UU_REAL rnum;
/*
.....Set up form fields
*/
	static char traverse[] = {1,1,1, 1,0, 1,0,1,0, 1,1, 1,0};
	static char called[]   = {6,6,6, 6,6, 6,6,6,6, 6,6, 6,6};
	static char display[]  = {1,1,1, 1,1, 1,1,1,1, 1,1, 1,1};

	static UD_METHOD methods[] = {OnBrowse,UU_NULL,UU_NULL,
		OnFSelect,UU_NULL, OnFSelect,UU_NULL,OnFSelect,UU_NULL,
		OnFSelect,OnFSelect, OnFSelect,UU_NULL};

	static int *ans[]={UU_NULL,(int *)&Sload,(int *)&Label,&choice,
		(int *)Svalue,&Stflag,(int *)&Sline,&Eflag,(int *)&Eline,&strvoc,
		&strlab,&Storflag,(int *)&Storvar};
/*
.....Initialize form
*/
   Sload[0] = '\0';
	Label[0] = '\0';
   Sftyp = 1;
	Snsurf = 0;
	Stflag = 0;
	Eflag = 0;
	strcpy(Sline,"0");
	strcpy(Eline,"0");
	Storflag = 0;
/*
.....Get the form input
*/
repeat:
	traverse[FSTT] = Stflag;
	traverse[FEND] = Eflag;
	traverse[FSTO] = Storflag;
	if (choice == 1 || choice == 2) traverse[FVAL] = UU_TRUE;
	else traverse[FVAL] = UU_FALSE;
   status = ud_form1("datafile.frm",ans,ans,methods,called,display,traverse);
   if (status == -1) goto done;
/*
.....Make sure a filename was specified
*/
   nc = ul_cut_string(Sload,UX_MAX_PATH_LEN);
   if (nc == 0)
   {
      ud_wrerr("A filename must be specified.");
      goto repeat;
   } 
/*
.....Make sure a label was specified
*/
	nc = ul_cut_string(Label,MXLAB);
	if (nc == 0) 
	{
		ud_wrerr("A label must be specified.");
		goto repeat;
	}
/*
.....Make sure a value is given if an error type is chosen 
.....that requires input
*/
	if ((choice == 2 || choice == 3) && Svalue[0] == 0) 
	{
		ud_wrerr("A value must be specified.");
		goto repeat;
	}
/*
.....Make sure a variable is given when the checkbox is selected
*/
	if (Storflag == 1)
	{
		nc = ul_cut_string(Storvar,MXLAB);
		if (nc == 0) 
		{
			ud_wrerr("A variable must be specified.");
			goto repeat;
		}
	}
/*
.....Output the DATA command
*/
	if (S_build_filecmd() != UU_SUCCESS) goto repeat;
/*
.....End of routine
*/
done:
	return;
}

/*********************************************************************
**    S_FUNCTION     :  OnSelect(fieldno, val, stat)
**       Routine to select a list of geometry.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnSelect(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int numint,init,color;
	struct NCL_fixed_databag e;
	UU_LOGICAL cmdreject;
	UM_sgeo geo;
/*
.....Take down form
*/
	ud_form_invis();
/*
.....Trap Reject Op
*/
	UD_MARK (cmdreject, UU_TRUE);
	if (cmdreject != 0) goto done;
/*
.....Get the next geometry selection
*/
	ud_ldas(UD_DASSELECT,UA_NCL,238,UU_NULL,1,&numint,UD_NODEFAULT);
	if (numint == 0) goto done;
/*
.....Loop through selections
*/
	init = UU_TRUE;
	color = Scolor;
	while(ud_gnxt(init,UU_NULL,&e.key,1))
	{
		init = UU_FALSE;
/*
.....Store this item in the list
*/
		if (ncl_retrieve_data_fixed(&e) != 0) continue;
		geo.key = e.key;
		geo.relnum = e.rel_num;
		ncl_get_label(&e,geo.label);
		ncl_get_geo_color(e.key,&geo.color);
		nclu_add_list(&Ssurf,&geo,&color);
/*
.....Update the entities color
*/
		if (color != -1)
			ncl_update_geo_color(e.key,color,UU_TRUE);
		else
			ncl_update_geo_color(e.key,geo.color,UU_FALSE);

		uc_display(&e);
		color = Scolor;
	}
	Snsurf = UU_LIST_LENGTH(&Ssurf);
/*
.....End of routine
.....Redisplay form
*/
done:;
	ud_form_vis();
	UD_UNMARK(cmdreject);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnColor(fieldno, val, stat)
**			Color change callback.  Changes the color of all entities.
**    PARAMETERS   
**       INPUT  : fieldno = Form field which initiated this call.
**			         val     = Current field value.
**			         stat    = Not used.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnColor(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UM_sgeo *geo = UU_NULL;
/*
.....Call the default method
.....This causes the answer field to be updated
*/
	ud_default_method(fieldno, val, stat);
/*
.....Change the color of all entities
.....in this list
*/
	if (Snsurf > 0)
	{
		geo = (UM_sgeo *)UU_LIST_ARRAY(&Ssurf);
		nclu_repaint (geo,Snsurf,Scolor);
	}
	return (UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnDesel(fieldno, val, stat)
**			Deselects all geometry from the list.
**    PARAMETERS   
**       INPUT  : fieldno = Form field which initiated this call.
**			         val     = Not used, form initiated through pushbutton.
**			         stat    = Not used.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnDesel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UM_sgeo *geo;
/*
.....Remove all entities in this list
*/
	geo = (UM_sgeo *) UU_LIST_ARRAY(&Ssurf);
	nclu_repaint (geo,Snsurf,-1);
	UU_LIST_EMPTY(&Ssurf); Snsurf = 0;
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnAction(fieldno, val, stat)
**			Method called when an Action button is pressed.
**    PARAMETERS   
**       INPUT  : fieldno = Form field which initiated this call.
**			         val     = Current field value.
**			         stat    = Not used.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnAction(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int isw,status;
/*
.....Set correct switch
.....depending on form type that is active
*/
	isw = *fieldno;
	switch (isw)
	{
/*
.....Enter viewing mode
*/
	case FVIW:
		ud_form_invis();
		uz_dyn_mouse();
		ud_form_vis();
		break;
/*
.....Apply - Output command
*/
	case FAPP:
		status = S_build_cmd();
		if (status == UU_SUCCESS)
		{
			uu_list_init(&Ssurf,sizeof(UM_sgeo),50,50);
			Sinit = UU_TRUE;
		}
		break;
	}
	return (UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_build_cmd()
**		   Build and output command(s).
**    PARAMETERS   
**       INPUT  :
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_FAILURE if form data is not complete,
**                   UU_SUCCESS otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_build_cmd()
{
	int i,nc,status;
	UU_LOGICAL cmdreject;
	char buf[MXLAB],*p;
	NCL_cmdbuf cmdbuf;
	UM_sgeo *geo;
/*
.....Geometry must be selected
*/
	if (Snsurf == 0)
	{
		ud_wrerr("No geometry selected.");
		goto failed;
	}
/*
.....Check for valid label
*/
	if (!ncl_is_valid_label(Slabel))
		goto failed;
/*
.....Initialize loop
*/
	geo = (UM_sgeo *) UU_LIST_ARRAY(&Ssurf);
	nc = strlen(Slabel);
	ul_strip_blanks(Slabel,&nc);
	i = 0;
/*
.....Loop through geometry
*/
	while (i < Snsurf)
	{
/*
........Initialize command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
........Add label
*/
		sprintf(buf,"%s=",Slabel);
		ncl_add_token(&cmdbuf,buf,NCL_nocomma);
/*
........Add DATA word
*/
		ncl_add_token(&cmdbuf, NCL_data, NCL_nocomma);
/*
........Add DATA label if not 1st time through
*/
		if (i != 0) ncl_add_token(&cmdbuf,Slabel,NCL_comma);
/*
........Loop through geometry
*/
		for (; i < Snsurf && cmdbuf.num_cmd < 3; i++)
		{
/*
...........Label only
*/
			if (Sflag == 2)
			{
				strcpy(buf,geo[i].label);
				p = strchr(buf,'(');
				if (p != UU_NULL) *p = '\0';
				ncl_add_token(&cmdbuf, buf, NCL_comma);
			}
/*
...........Subscript
*/
			else if (Sflag == 1)
			{
				strcpy(buf,geo[i].label);
				p = strchr(buf,'(');
				if (p != UU_NULL)
				{
					strcpy(buf,p+1);
					p = strchr(buf,')');
					if (p != UU_NULL) *p = '\0';
				}
/*				else
					strcpy(buf,"1");*/
				ncl_add_token(&cmdbuf, buf, NCL_comma);
			}
/*
...........Label and Subscript
*/
			else
				ncl_add_token(&cmdbuf, geo[i].label, NCL_comma);
		}
/*
........Issue the command
*/
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		status = ncl_call(&cmdbuf);
		if (status != 0) goto failed;
	}
/*
.....Free the geometry list
*/
	S_free_geo();
	status = UU_SUCCESS;
	goto done;
/*
.....Form data is not complete
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    I_FUNCTION     : S_build_filecmd()
**		   Build and output command(s).
**    PARAMETERS   
**       INPUT  :
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_FAILURE if form data is not complete,
**                   UU_SUCCESS otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_build_filecmd()
{
	int i,j,nc,status;
	UU_LOGICAL cmdreject;
	char buf[MXLAB];
	NCL_cmdbuf cmdbuf;

	nc = strlen(Sload);
//	ul_strip_blanks(Sload,&nc);
/*
........Initialize command buffer
*/
	ncl_init_cmdbuf(&cmdbuf);
/*
........Add label
*/
	sprintf(buf,"%s=",Label);
	ncl_add_token(&cmdbuf,buf,NCL_nocomma);
/*
........Add DATA word
*/
	ncl_add_token(&cmdbuf, NCL_data, NCL_nocomma);
/*
.....Find/Add file name and check for extension. If there
.....is no extension, then default to .csv
*/
	j = 0;
	i = nc-1;
	while (i > 0 && Sload[i] != 92)
	{
		if (Sload[i] == '.') j = i;
		i--;
	}
	if (i > 0) i += 1;
	if (j > 0) sprintf(buf,"\"%s\"",&Sload);
	else sprintf(buf,"\"%s%s\"",&Sload,".csv");
	ncl_add_token(&cmdbuf,buf,NCL_nocomma);
/*
.....Check for additional parameters
*/
   if (Storflag != 0)
   {
		sprintf(buf,",%s",Storvar);
		ncl_add_token(&cmdbuf,buf,NCL_nocomma);
   } 
   if (Stflag != 0) 
	{
		sprintf(buf,",%s",Sline);
		ncl_add_token(&cmdbuf,buf,NCL_nocomma);
	}
	if (Eflag != 0) 
	{
		sprintf(buf,",%s",Eline);
		ncl_add_token(&cmdbuf,buf,NCL_nocomma);
	}
	if (choice != 0)
	{
		if (choice == 3) sprintf(buf,",ERROR");
		else if (choice == 1) sprintf(buf,",SCALAR,%s",Svalue);
   	else if (choice == 2) sprintf(buf,",%s",Svalue);
		ncl_add_token(&cmdbuf,buf,NCL_nocomma);
	}
	if (strvoc != 0)
   {
		sprintf(buf,",VOCABF,OFF");
		ncl_add_token(&cmdbuf,buf,NCL_nocomma);
   } 
	if (strlab != 0)
		ncl_add_token(&cmdbuf,",LABEL,OFF",NCL_nocomma);
/*
........Issue the command
*/
	ncl_set_cmdmode(UU_TRUE);
	ncl_add_cmdbuf(&cmdbuf);
	status = ncl_call(&cmdbuf);
	if (status != 0) goto failed;
/*
.....Free the geometry list
*/
	S_free_geo();
	status = UU_SUCCESS;
	goto done;
/*
.....Form data is not complete
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    I_FUNCTION     : S_free_geo()
**		   Free the geometry list.
**    PARAMETERS   
**       INPUT  :
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_free_geo()
{
	UM_sgeo *geo;
/*
.....Free the geometry list
*/
	if (Sinit)
	{
		geo = (UM_sgeo *)UU_LIST_ARRAY(&Ssurf);
		nclu_repaint (geo,Snsurf,-1);
		uu_list_free(&Ssurf); Snsurf = 0;
		Sinit = UU_FALSE;
	}
}

/*********************************************************************
**    I_FUNCTION     :  OnBrowse(filedno, val, stat)
**       Routine to get the Stock file name.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnBrowse(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
   char sbuf[80],ext[UX_SUFFIX_LEN],descrip[80];
   int inum;
/*
.....Get filename to load
*/
   strcpy(sbuf,"Load File");
   strcpy(ext,"*.csv, *.txt");
   strcpy(descrip,"All Comma Delimited Files (*.csv, *.txt)");
   
   inum = 0;
   Sload[0] = '\0';
   ud_get_filename(sbuf,sbuf,ext,Sload,&inum,descrip, 1,UU_TRUE);
   if (Sload[0] != 0) ud_update_answer(FLFN,(int *)Sload);
   return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnFSelect()
**       Check box selection routine.
**    PARAMETERS
**       INPUT  : fieldno = Form field which initiated this call.
**                val     = Not used, form initiated through pushbutton.
**                stat    = Not used.
**       OUTPUT :
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT  OnFSelect(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
   int status,i;
/*
.....Process geometry selection buttons
*/
   switch (*fieldno)
   {
   case FSFL:
      ud_set_traverse_mask(FSTT,val->frmint[0]);
      break;
   case FEFL:
      ud_set_traverse_mask(FEND,val->frmint[0]);
      break;
   case FSTF:
      ud_set_traverse_mask(FSTO,val->frmint[0]);
      break;
	case FBLK:
		i = val->frmint[0] == 1 || val->frmint[0] == 2;
      ud_set_traverse_mask(FVAL,i);
      break;
   }
   return(UD_FLDOK);
}
