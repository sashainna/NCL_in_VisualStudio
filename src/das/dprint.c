/*********************************************************************
**
**    NAME         :  dprint.c
**
**       CONTAINS:
**            ud_print_screen()
**            ud_print_ipvscreen()
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       dprint.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:14
**
*********************************************************************/
#include "driver.h"
#include "udforms.h"
#include "udfdata.h"
#include "gtbl.h"
#include "gdidd.h"
#include  "lipv.h"
#include  "lcom.h"

int UD_printipv = 0;
int UL_nclipv_print = 0;

static int paper_size = 0;
static int temp_size = 0;
static char ptcmd[20] = "lp";
static UX_pathname ptfile = "";
static  int output_choice = 0;
static  int temp_choice = 0;
static int print_ftype = 1;

/*********************************************************************
**    S_FUNCTION     :  static ud_ftype(fieldno, val, stat)
**       Method called at 'File type' toggle field.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT ud_ftype(fieldno, val, stat)
int *fieldno;
UD_FSTAT stat;
UD_DDATA *val;
{
#if UU_COMP!=UU_WIN2K
	switch (print_ftype)
	{
		case 0:
		case 2:
			ud_set_traverse_mask(6, UU_TRUE);
			ud_set_traverse_mask(7, UU_TRUE);
			break;
		case 1:
		case 3:
			ud_set_traverse_mask(6, UU_FALSE);
			ud_set_traverse_mask(7, UU_FALSE);
			break;
	}
	return UD_FLDOK;
#else
	*fieldno = -1;
	return UD_FLDOK;
#endif
}

/*********************************************************************
**    S_FUNCTION     :  static ud_prtchc(fieldno, val, stat)
**       Method called at 'Print Output' toggle field.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT ud_prtchc(fieldno, val, stat)
int *fieldno;
UD_FSTAT stat;
UD_DDATA *val;
{
	switch (temp_choice)
	{
		case 0:
			ud_set_traverse_mask(1, UU_TRUE);
			ud_set_traverse_mask(2, UU_TRUE);
			ud_set_traverse_mask(3, UU_TRUE);
			ud_set_traverse_mask(4, UU_FALSE);
			ud_set_traverse_mask(5, UU_FALSE);
#if UU_COMP!=UU_WIN2K
			if ((print_ftype==0)||(print_ftype==2))
			{
				ud_set_traverse_mask(6, UU_TRUE);
				ud_set_traverse_mask(7, UU_TRUE);
			}
			else
			{
				ud_set_traverse_mask(6, UU_FALSE);
				ud_set_traverse_mask(7, UU_FALSE);
			}
#endif
			ud_set_display_mask(UD_INPUTF, 9, UU_FALSE);
			break;
		case 1:
			ud_set_traverse_mask(1, UU_FALSE);
			ud_set_traverse_mask(2, UU_FALSE);
			ud_set_traverse_mask(3, UU_FALSE);
#if UU_COMP!=UU_WIN2K
			ud_set_traverse_mask(4, UU_TRUE);
			ud_set_traverse_mask(6, UU_TRUE);
			ud_set_traverse_mask(7, UU_TRUE);
#endif
			ud_set_traverse_mask(5, UU_TRUE);
			ud_set_display_mask(UD_INPUTF, 9, UU_TRUE);
			break;
	}
	return UD_FLDOK;
}
/*********************************************************************
**    S_FUNCTION     :  static ud_browse_ptfile(fieldno, val, stat)
**       Method called at 'Print file browser' toggle field.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT ud_browse_ptfile(fieldno, val, stat)
int *fieldno;
UD_FSTAT stat;
UD_DDATA *val;
{
	UX_pathname filename,descrip,ext;
	char *ux_getenv();
	int len;

	filename[0] = '\0';
	if (print_ftype==0)
	{
		strcpy(descrip, "Bitmap Files (*.bmp)");
		strcpy(ext,"*.bmp");
	}
	else if (print_ftype==1)
	{
		strcpy(descrip, "JPEG Files (*.jpg)");
		strcpy(ext,"*.jpg");
	}
	else if (print_ftype==3)
	{
		strcpy(descrip, "GIF Files (*.gif)");
		strcpy(ext,"*.gif");
	}
	else
	{
		strcpy(descrip, "PostScript Files (*.ps)");
		strcpy(ext,"*.ps");
	}
	ud_get_filename("browse", "Print file", ext,
						filename, &len,descrip, UU_FALSE) ;
	if (filename[0]!='\0')
	{
		ud_update_answer(*fieldno-1, (int*)filename);
	}
	else
		*fieldno = -1;
	return UD_FLDOK;
}

/**************************************************************************
**
**  E_FUNCTION         : ud_print_screen(cfil,kfl,ktyp,ksiz)
**     print out the scrren as it is
**  PARAMETERS
**      INPUT  :
**         cfil    = Filename to print to.
**         kfl     = -1 = Display Print Screen form, 0 = Print to file,
**                   1 = Print to printer.
**         ktyp    = Type of file to create (BMP,JPG,PS,GIF).
**         ksiz    = Paper size.
**      OUTPUT :
**          none
**
**  RETURNS      :  
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
void ud_print_screen(cfil,kfl,ktyp,ksiz)
char *cfil;
int kfl,ktyp,ksiz;
{
#if UU_COMP == UU_WIN2K
	int i;
#endif
	int status;
	char *p;
	UX_pathname filename;
	int *ans[10];
	UD_FSTAT uj_noop();
	int opt, len;
	char *indx;
	int dum_create = 0;
	static int setflag = 0;
	static int fit_on = 1;
	static int bcolor = 0;
	static int pcenter = 0;
	static UD_METHOD methods[10] = {
					ud_prtchc, ud_ftype, uj_noop, ud_browse_ptfile, uj_noop, uj_noop, uj_noop, uj_noop, uj_noop, uj_noop};
	static char called[] = { 6,6,6,6,6,6,6,6,6,6};
	static char traverse[] = { 1,1,1,1,1,1,1,1,1,1};
	static char display[] = {1,1,1,1,1,1,1,1,1,1};  
/*
.....Initialize routine
*/
	opt = 0;
/*
.....Filename is provided
*/
	if (kfl != -1)
	{
		output_choice = kfl;
		print_ftype = ktyp;
		strcpy(ptfile,cfil);
	}
/*
.....Display form
*/
	else
	{
		temp_choice = output_choice;
		temp_size = paper_size;
		if (ptfile[0]=='\0')
		{
/*
......default name to part program file
*/
			if (UL_program[0]!='\0')
				strcpy(ptfile, UL_program);
		}
		else
		{
/*
......remove file extension
*/
			p = (char *)strrchr(ptfile,'.');
			if (p!=0) 
			{
				*p = 0;
			}
		}
#if UU_COMP!=UU_WIN2K
		if (print_ftype==0)
			print_ftype = 1;
#endif
		ans[0] = (int *)&temp_choice;
		ans[1] = (int *) &print_ftype;
		ans[2] = (int *)ptfile;
		ans[3] = (int *)&opt;
		ans[4] = (int *)ptcmd;
		ans[5] = (int *) &setflag;
		ans[6] = (int *)&temp_size;
		ans[7] = (int *)&fit_on;
		ans[8] = (int *)&bcolor;
		ans[9] = (int *)&pcenter;
		if (output_choice==1)
		{
			display[9] = 1;
		}
		else
		{
			display[9] = 0;
		}
		if (temp_choice==0)
		{
			traverse[1] = 1;
			traverse[2] = 1;
			traverse[3] = 1;
			traverse[4] = 0;
			traverse[5] = 0;
		}
		else
		{
			traverse[1] = 0;
			traverse[2] = 0;
			traverse[3] = 0;
			traverse[4] = 1;
			traverse[5] = 1;
		}
#if UU_COMP!=UU_WIN2K
		if ((print_ftype==0)||(print_ftype==2))
		{
			traverse[6] = 1;
			traverse[7] = 1;
		}
		else
		{
			traverse[6] = 0;
			traverse[7] = 0;
		}
		display[4] = 1;
		display[5] = 0;
#else
		display[4] = 0;
		display[5] = 1;
#endif
		if (UD_printipv)
		{
			display[8] = 0;
			bcolor = 1;
		}
		else
			display[8] = 1;
		status = ud_form1("print.frm", ans, ans, methods, called, display,
				traverse);
		if (status==-1)
			return ;
#if UU_COMP!=UU_WIN2K
		if (print_ftype==0)
		{
			ud_winerror("BMP file is not supported!");
			return;
		}
#endif
	output_choice = temp_choice;
	paper_size = temp_size;
	}
/*
.....doing the full screen repaint before we read pixels
*/
	if (UD_printipv==0)
		uz_repaint(0);

	if (output_choice==0)
	{
		len = strlen(ptfile);
/*
.....for WinNT, we allow filename with spaces, 
.....only remove trailling spaces for WinNT
.....Yurong 1/17/02
*/
#if UU_COMP!=UU_WIN2K
		ul_strip_blanks(ptfile, &len);
#else
/*
.....we also need to remove preceding spaces
.....Yurong 3/1/02
*/
		for (i=0; i<strlen(ptfile); i++)
		{
			if (ptfile[i]!=' ') break;
		}
		strcpy(ptfile, &(ptfile[i]));
		for (i=strlen(ptfile); i>0; i--)
		{
			if (ptfile[i-1]==' ')
				ptfile[i-1] = '\0';
			else
				break;
		}
#endif
		indx = (char *)strstr(ptfile, ".");
		if (indx==UU_NULL)
		{
			if (print_ftype==1)
				strcat(ptfile, ".jpg");
			else if (print_ftype==2)
				strcat(ptfile, ".ps");
			else if (print_ftype==3)
				strcat(ptfile, ".gif");
			else
				strcat(ptfile, ".bmp");
		}
		strcpy(filename, ptfile);
	}
#if UU_COMP!=UU_WIN2K
	else
	{
		tmpnam(filename);
		dum_create = 1;
	}
#endif
/*
.....doing the full screen repaint before we read pixels
*/
/*
......use jump table
*/
	(*(ug_gksstli.wsopen[0].connid)[UW_PRINT_SCREEN])(output_choice, filename, ptcmd, paper_size, print_ftype, setflag, fit_on, bcolor, pcenter);

	if (dum_create==1)
		ux_delete(filename);
}

/**************************************************************************
**
**  E_FUNCTION         : ud_print_ipvscreen(cfil,kfl,ktyp,ksiz)
**     print out the NCLIPV screen as it is
**  PARAMETERS
**      INPUT  :
**         cfil    = Filename to print to.
**         kfl     = -1 = Display Print Screen form, 0 = Print to file,
**                   1 = Print to printer.
**         ktyp    = Type of file to create (BMP,JPG,PS,GIF).
**         ksiz    = Paper size.
**      OUTPUT :
**          none
**
**  RETURNS      :  
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
void ud_print_ipvscreen(cfil,kfl,ktyp,ksiz)
char *cfil;
int kfl,ktyp,ksiz;
{
/*
.....Only Print if NCLIPV is active
*/
#ifdef UU_IPV
	if (LW_active == 1)
	{
		UD_printipv = 1;
		ud_print_screen(cfil,kfl,ktyp,ksiz);
		UD_printipv = 0;
	}
#endif
}
