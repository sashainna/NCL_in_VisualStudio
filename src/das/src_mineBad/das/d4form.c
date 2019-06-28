/*********************************************************************
**    NAME         :  d4form.c
**       CONTAINS:
**          int ud_form
**          int ud_form1
**          static ud_cpyans
**          int ud_form_display
**          int ud_form_display1
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       d4form.c , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**       05/22/18 , 10:13:03
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "udfmracs.h"
#include "driver.h"
#include "gtbl.h"
#include "gdidd.h"
/* #define NULL 0 */

UD_FSTAT ud_trvfrm();
int **ans1 = NULL;
char **ud_curdflt = NULL;
int *dfltno = NULL;                                                                             /* default number of each field */

int UD_form_bypick = 0;
int UD_macroflag = 0;
/*static ud_cpyans( char *in, char *out, short datatyp);*/
/*********************************************************************
**    I_FUNCTION :  ud_cpyans(in, out, datatyp) 
**                      copy answer from in to out.
**    PARAMETERS   
**       INPUT  :  char *in; -- pointer to data to copy.
**                                               char *out; -- pointer to where to copy it.
**                                               short datatyp; -- one of 11 das data types.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

static void ud_cpyans(in, out, datatyp)
char *in;
char *out;
short datatyp;
{
	UU_REAL *fp, *fq;
	int *ip, *iq;
	int n;                                  /* length of UD_DASSTRING datatyp*/
	int i;

	uu_denter(UU_DTRC, (us, "ud_cpyans(datatyp = %d)", datatyp));

	switch(datatyp) 
	{

/*      -- cartesian world coord, vector coord, or cartesian ndc coord */

		case UD_DASCART:
		case UD_DASVEC:
		case UD_DASNDC:
			fp = (UU_REAL *)in;
			fq = (UU_REAL *)out;
			for(i=0; i<3; i++) 
				fq[i] = fp[i];
			break;

/*      -- value, distance, angle, unitless real -- */

		case UD_DASVAL:
		case UD_DASDISTANCE:
		case UD_DASANGLE:
		case UD_DASUNITLESS:
			fp = (UU_REAL *)in;
			fq = (UU_REAL *)out;
			*fq  =  *fp;
			break;

/*      -- integer -- */

		case UD_DASINT:
			ip = (int *)in;
			iq = (int *)out;
			*iq = *ip;
			break;

/*      -- string -- */

		case UD_DASSCALAR:
		case UD_SCACART:
		case UD_SCAVAL:
		case UD_SCAUNITLESS:
		case UD_SCADISTANCE:
		case UD_SCAINT:
		case UD_SCAVEC:
		case UD_SCANDC:
		case UD_SCAANGLE:
		case UD_DASSTRING:
			n = strlen(in);
			if(n > 100)
			{
				uu_dprint(UU_DTRC,(us, "ud_cpyans. bad in. len = %d in=%s", n, in));
			}
			else
				strcpy(out, in);
			break;

		default:
			uu_dprint(UU_DTRC,(us, "ud_cpyans. error. bad datatyp = %d", datatyp));
			break;
	}                                                                       /* end switch(datatyp) */
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  UD_FSTAT ud_form(fname, def, ans) - display form,  get answers.
**    PARAMETERS   
**       INPUT  :  char *fname; -- form's file name.
**                 char *def[] -- an array of pointers to default answers, 
**                 NULL ifno default.
**       OUTPUT :  char *ans[]; -- answers placed here.
**    RETURNS      : 
**         UD_FRMOK - form traversed successfully,  good data returned
**                                              UD_BADFILL - form data fill error,  data no good
**         UD_BADPRST - form presentation error,  data no good,  or
**         form file wouldn't open.
**         UD_BADACT - form activate error,  data no good
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UD_FSTAT ud_form(fname, def, ans)
char *fname;                            /* form file name */
int *def[];                                     /* array of pointers to defaults. NULL ifno default.*/
int *ans[];                                     /* array of pointers.  Answers placed here */
{
	UD_FSTAT ud_form1();

	return(ud_form1(fname, def, ans,  NULL/* methods*/, NULL/*disp mask*/, 
						NULL/*trav mask*/,  NULL/* Method return mask*/));
}

/*********************************************************************
**    E_FUNCTION :  UD_FSTAT ud_form1(fname, def, ans, methods, 
**                                                                              method_ret, disp_mask, trav_mask) - 
**                                              display form,  get answers.  Display and traverse fields
**                                              as set in display and return masks.  Call functions defined
**                                              in methods and required by method_ret.
**    PARAMETERS   
**       INPUT  :    char *fname; -- form's file name.
**                   char *def[] -- an array of pointers to default answers, 
**                                  NULL ifno default.
**                                                      UD_METHOD *methods; -- Array of user defined methods.  
**                                                              May be NULL or any of its members may be NULL.  
**                                                              If NULL,  call ud_default_method()
**                                                      char *method_ret; -- Array of charactors telling forms
**                                                              traverser when to call user defined methods.  One char
**                                                              per input field.  Bitwise 'or' of:
**                                                                              UD_ENTER - Call method on entry to field
**                                                                              UD_EXIT - Call method on leaving field
**                                                                              UD_TOGGLE- Call method on toggling field
**                                                                                                      ( Ignored except fortoggle fields )
**                                                      char *disp_mask; - 1 char per field.  if = = 0,  don't 
**                                                              display,  if = =1 then display.  Note that all fields
**                                                              are counted,  starting with display fields.
**                                                      char *trav_mask; - 1 char per field.  if = = 0,  don't 
**                      traverse,  if = =1 then traverse.  Note that only
**                                                              input fields are counted.
**
**       OUTPUT :    char *ans[]; -- answers placed here.
**    RETURNS      : 
**                UD_FRMOK - form traversed successfully,  good data returned
**                UD_BADFILL - form data fill error,  data no good
**                UD_BADPRST - form presentation error,  data no good,  or
**                               form file wouldn't open.
**                UD_BADACT - form activate error,  data no good
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UD_FSTAT ud_form1(fname, def, ans, methods, method_ret, disp_mask, trav_mask)
char *fname;                            /* form file name */
int *def[];                                     /* array of pointers to defaults. NULL ifno default.*/
int *ans[];                                     /* array of pointers.  Answers placed here */
UD_METHOD *methods;             /* Array of ptrs to methods     */
char *disp_mask;                        /* Display mask */
char *trav_mask;                        /* Traversal mask       */
char *method_ret;                       /* Method return masks */
{
	UD_FSTAT ud_form2();

	return(ud_form2(fname, def, ans, methods, method_ret, disp_mask, trav_mask, 0 /* display type flag */));
}

/*********************************************************************
**    E_FUNCTION :  UD_FSTAT ud_form2(fname, def, ans, methods, 
**                                                                              method_ret, disp_mask, trav_mask) - 
**                                              display form,  get answers.  Display and traverse fields
**                                              as set in display and return masks.  Call functions defined
**                                              in methods and required by method_ret.
**                                      if flag = 0: display a modal form and waiting for answer
**                                              flag = 1: display form but not wait
**    PARAMETERS   
**       INPUT  :    char *fname; -- form's file name.
**                   char *def[] -- an array of pointers to default answers, 
**                                  NULL ifno default.
**                                                      UD_METHOD *methods; -- Array of user defined methods.  
**                                                              May be NULL or any of its members may be NULL.  
**                                                              If NULL,  call ud_default_method()
**                                                      char *method_ret; -- Array of charactors telling forms
**                                                              traverser when to call user defined methods.  One char
**                                                              per input field.  Bitwise 'or' of:
**                                                                              UD_ENTER - Call method on entry to field
**                                                                              UD_EXIT - Call method on leaving field
**                                                                              UD_TOGGLE- Call method on toggling field
**                                                                                                      ( Ignored except fortoggle fields )
**                                                      char *disp_mask; - 1 char per field.  if = = 0,  don't 
**                                                              display,  if = =1 then display.  Note that all fields
**                                                              are counted,  starting with display fields.
**                                                      char *trav_mask; - 1 char per field.  if = = 0,  don't 
**                      traverse,  if = =1 then traverse.  Note that only
**                                                              input fields are counted.
**
**       OUTPUT :    char *ans[]; -- answers placed here.
**    RETURNS      : 
**                UD_FRMOK - form traversed successfully,  good data returned
**                UD_BADFILL - form data fill error,  data no good
**                UD_BADPRST - form presentation error,  data no good,  or
**                               form file wouldn't open.
**                UD_BADACT - form activate error,  data no good
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UD_FSTAT ud_form2(fname, def, ans, methods, method_ret, disp_mask, trav_mask, flag)
char *fname;                            /* form file name */
int *def[];                                     /* array of pointers to defaults. NULL ifno default.*/
int *ans[];                                     /* array of pointers.  Answers placed here */
UD_METHOD *methods;             /* Array of ptrs to methods     */
char *disp_mask;                        /* Display mask */
char *trav_mask;                        /* Traversal mask       */
char *method_ret;                       /* Method return masks */
int flag;                   /* display type flag */
{
	int fno;                                                        /* current field number. */
	short datatyp;                                  /* das data type of current field */
	short fldtyp;                                   /* 0 = display only, 1=input, 2=toggle */
	int ierr;
	UD_FSTAT irtn;
	int i;
	UD_FSTRUCT fstruct;
	UD_FDATA fdata;

	UD_FSTAT ud_default_method();
	
	uu_denter(UU_DTRC, (us, "ud_form1(%s)", fname));
	ans1 = ans;
	irtn = UD_FRMOK;
/*
.....always set UD_form_bypick = 0, only
.....when form in pick mode, UD_form_bypick = 1;
*/
	UD_form_bypick = 0;
/* -- load the form file -- */
/*
.....before load form into fstruct
.....check if there is a malloc memory
.....ud_curdflt, free them first,
.....because we only keep one copy 
.....of global form structure but on WIN2K
.....we can have a couple of form objects
.....exist at same time, ud_curdflt only for 
.....default value, we don't use them for WIN2K,
.....acturally not for MOTIF version also.
.....but I don't want to change the logic for other
.....platform, so keep this value, just check 
.....and free to make sure only one memory space 
.....for this value
.....Yurong 11/4/00
*/
/*
.....temp for WIN2K now because UNIX version using "jump"
.....to cancel the form, never free this space (may many times)
.....so the UD_frm is not the original form we want free
.....it will cause the problem, so, for now, only for UD_winnt
.....only, we may remove the condition after we made changes
.....for "jump" cancal form
.....Yurong
*/
/*
.....we are not use ud_curdflt at all for UD_motif/UD_winnt
.....Yurong 8/20/01
*/
	UD_macroflag = 0;
	ierr = ud_loadfrm(fname, &fstruct);
	if(ierr != 0)
	{
		irtn=(UD_FSTAT)-1;
		goto rtn;
	}

/* -- init fdata struct -- */

	fdata.ud_ndf = fstruct.n_input_fields;
	fdata.ud_data = (UD_DPIECE *) uu_malloc(fdata.ud_ndf * sizeof(UD_DPIECE));
	fstruct.ud_display_mask = (char *)
			uu_malloc((fdata.ud_ndf+fstruct.n_display_fields+1) * sizeof(char));
/* -- Copy users display mask to form display mask, ifthe user 
		supplied one.  else assume all fields are to be displayed -- */

	for(i = 0; i<fstruct.n_display_fields+fdata.ud_ndf; i++) 
	{
		fstruct.ud_display_mask[i] = -1;
		if (i < fdata.ud_ndf)
		{
			if (fstruct.input_flds[i].active_flag == -1)
				fstruct.ud_display_mask[i] = 0;
			else if (fstruct.input_flds[i].active_flag == 1)
				fstruct.ud_display_mask[i] = 1;
		}
		if (fstruct.ud_display_mask[i] == -1)
		{
			if(disp_mask != NULL)
				fstruct.ud_display_mask[i] = disp_mask[i];
			else
				fstruct.ud_display_mask[i] = 1;
		}
	}

/* -- Copy users traverse mask to form traverse mask, ifthe user
		supplied one.  else assume all fields are to be traversed -- */

	fstruct.traverse_mask = (char *)uu_malloc((fdata.ud_ndf+1) * sizeof(char));
	for(i = 0; i<fdata.ud_ndf; i++)
	{
		if (fstruct.input_flds[i].active_flag == -1)
			fstruct.traverse_mask[i] = 0;
		else if (fstruct.input_flds[i].active_flag == 1)
			fstruct.traverse_mask[i] = 1;
		else
		{
			if(trav_mask !=  NULL)
				fstruct.traverse_mask[i] = trav_mask[i];
			else
				fstruct.traverse_mask[i] = 1;
		}
	}

/* -- Copy users methods to form methods,  ifthe user supplied one.
		else use the default method (ud_default_method).  Also set the
		method returns.  If the user did not supply return mask,  
		call method when leaving fields.  Toggles also call method 
		on toggle comand. -- */

	for(i=0; i<fdata.ud_ndf; i++)
	{
		fdata.ud_data[i].dtyp = fstruct.input_flds[i].ud_datatyp;
		if(method_ret != NULL)
			fstruct.input_flds[i].method_returns = method_ret[i];

		fstruct.input_flds[i].method = NULL;
		if((methods != NULL) && (methods[i] != NULL))
			fstruct.input_flds[i].method = methods[i];
	}
/*
.....if this is a display type form,
.....the last method callback is "CLOSE FORM" callback
*/
	if (flag==1)
	{
		if(method_ret != NULL)
			fstruct.input_flds[i].method_returns = method_ret[i];

		fstruct.input_flds[i].method = NULL;
		if((methods != NULL) && (methods[i] != NULL))
			fstruct.input_flds[i].method = methods[i];
	}

/*  -- Set up defaults.  If user supplied defaults in the default parm,  use
		them,  else use what is in form.  If there is not a default supplied
		in either,  the no default supplied -- */

	for(fno=0; fno<fstruct.n_input_fields; fno++) 
	{
		fldtyp = fstruct.input_flds[fno].toggle;

/*      -- is an input or toggle field -- */

		if(fldtyp != 0)
		{
			fdata.ud_data[fno].ud_delem.frmint = ans[fno];  /* answer ptr */

/*              -- no overriding default in fdata -- */

			if((def == NULL) || (def[fno] == NULL))
			{
				fdata.ud_data[fno].dflg = 0;
			}

/*              -- def[i] has overriding default -- */

			else 
			{
				fdata.ud_data[fno].dflg = 1;    /* set default in fdata */
				datatyp = fstruct.input_flds[fno].ud_datatyp;   /* das data type */

/*                      -- if a toggle  -- */

				if(fldtyp == 2)
					*ans[fno]  =  *def[fno];
				else 
					/* copy def to ans */
/*
.....copy def to ans only if def[fno]!=ans[fno]
.....have problem if ans[fno]=def[fno] and ans[fno] 
.....pass in as const value, for example, pass in as "****"
.....instead of a char string with assigned value "****"
.....Yurong 3/28/00
*/
				{
					if (def[fno]!=ans[fno])
						ud_cpyans((char *)def[fno], (char *)ans[fno], datatyp);
				}
			}                                                                                       /* end def[i] has default */
		}                                                                                               /* end is an input or toggle field */
	}                                                                                                       /* end foreach field */

/* -- bring up form,  get user answers -- */

	if (flag==0)
		irtn = (UD_FSTAT)(*(ug_gksstli.wsopen[0].connid)[UW_FORM])(&fstruct,
			&fdata);
	else
	{
		irtn = (UD_FSTAT)(*(ug_gksstli.wsopen[0].connid)[UW_FORM_DISPLAY])
			(&fstruct,&fdata);
	}

	UD_form_bypick = 0;
/* -- release memory used malloc'ed for form    -- */
	if (ud_curdflt!=NULL)
	{
		for(i = 0; i<fstruct.n_input_fields; i++)
		{
			uu_free(ud_curdflt[i]);
		}
		uu_free(ud_curdflt);
		ud_curdflt = NULL;
		uu_free(dfltno);
		dfltno = NULL;
	}
/*
.....For Motif & WinNT version
.....the structure is copy to (uu_move_byte) UW form structure (include the
.....pointer), and we can have form display (display type) while we get here
.....in this case, we don't free space until we close the form
*/
rtn:
	uu_dexit;
/*
.....this value may change when doing form, reset it
*/
	UD_macroflag = 0;
	return(irtn);
}
/*********************************************************************
**    E_FUNCTION :  int ud_form_display(fname, def, ans)
**    PARAMETERS   
**       INPUT  :       char *fname; -- form's file name.
**                                      char *def[] -- an array of pointers to default answers, 
**                                                                                              NULL ifno default.
**       OUTPUT :       char *ans[]; -- answers placed here.
**    RETURNS      : 
**                                      ID of the form displayed
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ud_form_display(fname, def, ans)
char *fname;                            
int *def[];             
int *ans[];     
{
	int ud_form_display1();

	return(ud_form_display1(fname, def, ans,  NULL/* methods*/, NULL/*disp mask*/, 
						NULL/*trav mask*/,  NULL/* Method return mask*/));
}

/*********************************************************************
**    E_FUNCTION :  int ud_form_display1(fname, def, ans, methods, 
**                                                                              method_ret, disp_mask, trav_mask) - 
**                                              display form,  get answers.  Display and traverse fields
**                                              as set in display and return masks.  Call functions defined
**                                              in methods and required by method_ret.
**    PARAMETERS   
**       INPUT  :    char *fname; -- form's file name.
**                   char *def[] -- an array of pointers to default answers, 
**                                  NULL ifno default.
**                                      UD_METHOD *methods; -- Array of user defined methods.  
**                                                              May be NULL or any of its members may be NULL.  
**                                                              If NULL,  call ud_default_method()
**                                      char *method_ret; -- Array of charactors telling forms
**                                                              traverser when to call user defined methods.  One char
**                                                              per input field.  Bitwise 'or' of:
**                                                                              UD_ENTER - Call method on entry to field
**                                                                              UD_EXIT - Call method on leaving field
**                                                                              UD_TOGGLE- Call method on toggling field
**                                                                                                      ( Ignored except fortoggle fields )
**                                      char *disp_mask; - 1 char per field.  if = = 0,  don't 
**                                                              display,  if = =1 then display.  Note that all fields
**                                                              are counted,  starting with display fields.
**                                      char *trav_mask; - 1 char per field.  if = = 0,  don't 
**                      traverse,  if = =1 then traverse.  Note that only
**                                                              input fields are counted.
**
**       OUTPUT :    char *ans[]; -- answers placed here.
**    RETURNS      : 
**                ID of the form displayed
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ud_form_display1(fname, def, ans, methods, method_ret, disp_mask, trav_mask)
char *fname;                            /* form file name */
int *def[];                                     /* array of pointers to defaults. NULL ifno default.*/
int *ans[];                                     /* array of pointers.  Answers placed here */
UD_METHOD *methods;             /* Array of ptrs to methods     */
char *disp_mask;                        /* Display mask */
char *trav_mask;                        /* Traversal mask       */
char *method_ret;                       /* Method return masks */
{
	return(ud_form2(fname, def, ans, methods, method_ret, disp_mask, trav_mask, 1 /* display type flag */));
}

int ud_close_dispfrm(frmid)
int frmid;
{
	int stat;
	stat = (*(ug_gksstli.wsopen[0].connid)[UW_CLOSE_DISPFRM])(frmid);
	return stat;
}
