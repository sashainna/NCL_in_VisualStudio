/*********************************************************************
**    NAME         :  jfmgt.c
**				This file changed to use form&browser interface by Yurong
**       CONTAINS:
**				uj_renmf
**				uj_copyf
**				uj_renmf_plot
**				uj_copyf_plot
**				uj_renmf_ppart
**				uj_copyf_ppart
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       jfmgt.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:46
*********************************************************************/

#include "umath.h"
#include "usysdef.h"
#include "ustdio.h"
#include "dasnog.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "dmark.h"
#include "dinput.h"
#include "usysg.h"
#include "gtblopst.h"
#include "gtblws.h"
#include "gtblst.h"
#include "gtblvar4.h"
#include "ginqdsiz.h"
#include "uims.h"
#include "xenv1.h"
#include "xfsys2.h"
#include "xfsys1.h"
#include "xfsys0.h"
#include "gcolors.h"			/* color definitions - ud_crwin */
#include "umoveb.h"
#include "udforms.h"
#include "udfdata.h"

static int rows = 26;		/* row return cell */
static int columns = 86;	/* columns return cell */
/*
.....added for browse filter
.....Yurong 9/24/98
*/
static char *UJ_browse_flt[100];
static char *UJ_browse_dspt[100];
/*********************************************************************
**    S_FUNCTION     : uj_browse_set_filter(fieldno, filter) 
**       Set browse filter global variable.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          filter:  browse filter changed to
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uj_browse_set_filter(fieldno, filter,descript)
int fieldno;
char *filter;
char *descript;
{
	int len;
	len = strlen(filter);
	UJ_browse_flt[fieldno] = (char *) uu_malloc((len+1)*sizeof(char));
	strcpy(UJ_browse_flt[fieldno], filter);
	len = strlen(descript);
	UJ_browse_dspt[fieldno] = (char *) uu_malloc((len+1)*sizeof(char));
	strcpy(UJ_browse_dspt[fieldno], descript);
}

/*********************************************************************
**    S_FUNCTION     : uj_browse_reset_filter(fieldno) 
**       Reset browse filter global variable.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uj_browse_reset_filter(fieldno)
int fieldno;
{
	uu_free(UJ_browse_flt[fieldno]);
	uu_free(UJ_browse_dspt[fieldno]);
}

/*********************************************************************
**    S_FUNCTION     :  uj_browse_file(fieldno, val, stat)
**       Method called at browser toggle field.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form display mask.
**    WARNINGS     : none
*********************************************************************/
UD_FSTAT uj_browse_file(fieldno, val, stat)
int *fieldno;
UD_FSTAT stat;
UD_DDATA *val;
{
	UX_pathname filename, filter, descript;
	int len;
	if (UJ_browse_flt[*fieldno]!=NULL)
		strcpy(filter, UJ_browse_flt[*fieldno]);
	else
		strcpy(filter, "*.*");
	if (UJ_browse_dspt[*fieldno]!=NULL)
		strcpy(descript, UJ_browse_dspt[*fieldno]);
	else
		descript[0] = '\0';

	filename[0] = '\0';
	ud_get_filename("browse", "Browse Files", filter, filename, &len,descript, 1,
		UU_FALSE);
	if (filename[0]!='\0')
	{
		ud_update_answer(*fieldno-1, (int*)filename);
	}
	else
/*
.....canceled
*/
	*fieldno = -1;
	return(UD_FLDOK);
}
/*********************************************************************
**    I_FUNCTION :  uj_renmf()
** 	rename file
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uj_renmf(outpathnm, outpathto, ftype)
UX_pathname outpathnm, outpathto;
char *ftype;
{
	ux_add_ftype(ftype,outpathnm,UX_PRTERRS);
   ux_add_ftype(ftype,outpathto,UX_PRTERRS);
   ux_rename(outpathnm,outpathto,UX_PRTERRS);

}	/* uj_renmf	*/

/*********************************************************************
**    I_FUNCTION :  uj_copyf(ftype)
**       copy an old file to a new file
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uj_copyf(outpathnm, outpathto, ftype)
UX_pathname outpathnm, outpathto;
char *ftype;
{
	uu_denter( UU_XTRC,(us,"uj_copyf()"));

	ux_add_ftype(ftype,outpathnm,UX_PRTERRS);
	ux_add_ftype(ftype,outpathto,UX_PRTERRS);

	ux_copy(outpathnm, outpathto, UU_NULL, UX_PRTERRS);
	uu_dexit;

}	/* uj_copyf */

/*********************************************************************
**    I_FUNCTION :  uj_renmf_plot()
** 	renames the pair of plot files
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uj_renmf_plot(outpathnm, outpathto)
UX_pathname outpathnm, outpathto;
{
	UX_pathname outpathp2,outpathp2to;

	uu_denter( UU_XTRC,(us,"uj_renmf_plot()"));

	strcpy(outpathp2,outpathnm);
	ux_add_ftype("UJ_PLOT1_SUFFIX",outpathnm,UX_PRTERRS);
	ux_add_ftype("UJ_PLOT2_SUFFIX",outpathp2,UX_PRTERRS);

	strcpy(outpathp2to,outpathto);
	ux_add_ftype("UJ_PLOT1_SUFFIX",outpathto,UX_PRTERRS);
	ux_add_ftype("UJ_PLOT2_SUFFIX",outpathp2to,UX_PRTERRS);

	if (ux_rename(outpathnm,outpathto,UX_PRTERRS) != UU_SUCCESS)
		uu_uerror2(UJ_SUPPORT,25,outpathnm,outpathto);
		/* Unable to rename plot file outpathnm to outpathto */
	if (ux_rename(outpathp2,outpathp2to,UX_PRTERRS) != UU_SUCCESS)
		uu_uerror2(UJ_SUPPORT,25,outpathp2,outpathp2to);
		/* Unable to rename plot file outpathp2 to outpathp2to */
	uu_dexit;

}	/* uj_renmf_plot */

/*********************************************************************
**    I_FUNCTION :  uj_copyf_plot()
**       copy a pair of old plot files to a pair of new plot files
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uj_copyf_plot(outpathnm, outpathto)
UX_pathname outpathnm, outpathto;
{
	UX_pathname outpathp2, outpathp2to;
	uu_denter( UU_XTRC,(us,"uj_copyf_plot()"));

	strcpy(outpathp2,outpathnm);
	ux_add_ftype("UJ_PLOT1_SUFFIX",outpathnm,UX_PRTERRS);
	ux_add_ftype("UJ_PLOT2_SUFFIX",outpathp2,UX_PRTERRS);

	strcpy(outpathp2to, outpathto);
	ux_add_ftype("UJ_PLOT1_SUFFIX", outpathto,UX_PRTERRS);
	ux_add_ftype("UJ_PLOT2_SUFFIX", outpathp2to,UX_PRTERRS);

	if (ux_copy(outpathnm, outpathto,UU_NULL,UX_PRTERRS) != UU_SUCCESS)
		uu_uerror2(UJ_SUPPORT,26,outpathnm,outpathto);
	if (ux_copy(outpathp2,outpathp2to,UU_NULL,UX_PRTERRS) != UU_SUCCESS)
		uu_uerror2(UJ_SUPPORT,26,outpathp2,outpathp2to);
	uu_dexit;

}	/* uj_copyf_plot*/

/*********************************************************************
**    I_FUNCTION :  uj_renmf_ppart(ftype1,ftype2, ftype3,ftype4,ftype5,ftype6)
** 	rename the part file
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uj_renmf_ppart(outpathnm, outpathto,ftype1,ftype2, ftype3,ftype4,ftype5,ftype6)
UX_pathname outpathnm, outpathto;
char *ftype1;
char *ftype2;
char *ftype3;
char *ftype4;
char *ftype5;
char *ftype6;
{
	UX_pathname outpathnm2, outpathto2, outpathnm3, outpathto3;
	UX_pathname outpathnm4, outpathto4, outpathnm5, outpathto5;
	UX_pathname outpathnm6, outpathto6;
	int mode;
	uu_denter( UU_XTRC,(us,"uj_renmf_ppart(.,.,.)"));

	strcpy(outpathnm2,outpathnm);
	strcpy(outpathnm3,outpathnm);
	strcpy(outpathnm4,outpathnm);
	strcpy(outpathnm5,outpathnm);
	strcpy(outpathnm6,outpathnm);
	ux_add_ftype(ftype1,outpathnm,UX_PRTERRS);
	ux_add_ftype(ftype2,outpathnm2,UX_PRTERRS);
	ux_add_ftype(ftype3,outpathnm3,UX_PRTERRS);
	ux_add_ftype(ftype4,outpathnm4,UX_PRTERRS);
	ux_add_ftype(ftype5,outpathnm5,UX_PRTERRS);
	ux_add_ftype(ftype6,outpathnm6,UX_PRTERRS);

	strcpy(outpathto2, outpathto);
	strcpy(outpathto3, outpathto);
	strcpy(outpathto4, outpathto);
	strcpy(outpathto5, outpathto);
	strcpy(outpathto6, outpathto);
	ux_add_ftype(ftype1, outpathto,UX_PRTERRS);
	ux_add_ftype(ftype2, outpathto2,UX_PRTERRS);
	ux_add_ftype(ftype3, outpathto3,UX_PRTERRS);
	ux_add_ftype(ftype4, outpathto4,UX_PRTERRS);
	ux_add_ftype(ftype5, outpathto5,UX_PRTERRS);
	ux_add_ftype(ftype6, outpathto6,UX_PRTERRS);

	mode = 0;
	if (( ux_access1(outpathnm, &mode, UX_PRTERRS) == UU_SUCCESS )
		&& (mode != ( mode | UX_NEXISTS)))
	{
		ux_rename( outpathnm, outpathto,UX_PRTERRS);
		mode = 0;
		if ( ux_access1(outpathnm2, &mode, UX_PRTERRS) == UU_SUCCESS )
		{
			if (mode != ( mode | UX_NEXISTS))
				ux_rename(outpathnm2, outpathto2,UX_PRTERRS);
		}
		mode = 0;
		if ( ux_access1(outpathnm3, &mode, UX_PRTERRS) == UU_SUCCESS )
		{
			if (mode != ( mode | UX_NEXISTS))
				ux_rename(outpathnm3, outpathto3,UX_PRTERRS);
		}
	}
	else 
	{
		mode = 0;
		if ( ux_access1(outpathnm4, &mode, UX_PRTERRS) == UU_SUCCESS )
		{
			if (mode != (mode | UX_NEXISTS))
			{	
				ux_rename(outpathnm4, outpathto4, UX_PRTERRS);
				mode = 0;
				if ( ux_access1(outpathnm5, &mode, UX_PRTERRS) == UU_SUCCESS )
				{
					if (mode != ( mode | UX_NEXISTS))
						ux_rename(outpathnm5, outpathto5,UX_PRTERRS);
				}
				mode = 0;
				if ( ux_access1(outpathnm6, &mode, UX_PRTERRS) == UU_SUCCESS )
				{
					if (mode != ( mode | UX_NEXISTS))
						ux_rename(outpathnm6, outpathto6,UX_PRTERRS);
				}
			}
			else 
				ux_rename(outpathnm, &mode, UX_PRTERRS);
		}
	}	
	uu_dexit;

}	/* uj_renmf_ppart	*/

/*********************************************************************
**    I_FUNCTION :  uj_copyf_ppart(ftype1,ftype2,ftype3,ftype4,ftype5,ftype6)
**       copy an old file to a new file
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uj_copyf_ppart(outpathnm, outpathto, ftype1, ftype2,ftype3,ftype4,ftype5,ftype6)
UX_pathname outpathnm, outpathto;
char *ftype1;
char *ftype2;
char *ftype3;
char *ftype4;
char *ftype5;
char *ftype6;
{
	UX_pathname outpathnm2, outpathto2, outpathnm3, outpathto3;
	UX_pathname outpathnm4, outpathto4, outpathnm5, outpathto5;
	UX_pathname outpathnm6, outpathto6;
	int mode;

	strcpy(outpathnm2,outpathnm);
	strcpy(outpathnm3,outpathnm);
	strcpy(outpathnm4,outpathnm);
	strcpy(outpathnm5,outpathnm);
	strcpy(outpathnm6,outpathnm);
	ux_add_ftype(ftype1,outpathnm,UX_PRTERRS);
	ux_add_ftype(ftype2,outpathnm2,UX_PRTERRS);
	ux_add_ftype(ftype3,outpathnm3,UX_PRTERRS);
	ux_add_ftype(ftype4,outpathnm4,UX_PRTERRS);
	ux_add_ftype(ftype5,outpathnm5,UX_PRTERRS);
	ux_add_ftype(ftype6,outpathnm6,UX_PRTERRS);

	strcpy(outpathto2,outpathto);
	strcpy(outpathto3,outpathto);
	strcpy(outpathto4,outpathto);
	strcpy(outpathto5,outpathto);
	strcpy(outpathto6,outpathto);
	ux_add_ftype(ftype1,outpathto,UX_PRTERRS);
	ux_add_ftype(ftype2,outpathto2,UX_PRTERRS);
	ux_add_ftype(ftype3,outpathto3,UX_PRTERRS);
	ux_add_ftype(ftype4,outpathto4,UX_PRTERRS);
	ux_add_ftype(ftype5,outpathto5,UX_PRTERRS);
	ux_add_ftype(ftype6,outpathto6,UX_PRTERRS);

	mode = 0;
	if (( ux_access1(outpathnm, &mode, UX_PRTERRS) == UU_SUCCESS )
		&& (mode != ( mode | UX_NEXISTS)))
	{
		ux_copy(outpathnm, outpathto, UU_NULL, UX_PRTERRS);
		mode = 0;
		if ( ux_access1(outpathnm2, &mode, UX_PRTERRS) == UU_SUCCESS )
		{
			if (mode != ( mode | UX_NEXISTS))
				ux_copy(outpathnm2, outpathto2, UU_NULL, UX_PRTERRS);
		}
		mode = 0;
		if ( ux_access1(outpathnm3, &mode, UX_PRTERRS) == UU_SUCCESS )
		{
			if (mode != ( mode | UX_NEXISTS))
				ux_copy(outpathnm3, outpathto3, UU_NULL, UX_PRTERRS);
		}
	}
	else 
	{
		mode = 0;
		if ( ux_access1(outpathnm4, &mode, UX_PRTERRS) == UU_SUCCESS )
		{
			if (mode != (mode | UX_NEXISTS))
			{	
				ux_copy(outpathnm4, outpathto4, UU_NULL, UX_PRTERRS);
				mode = 0;
				if ( ux_access1(outpathnm5, &mode, UX_PRTERRS) == UU_SUCCESS )
				{
					if (mode != ( mode | UX_NEXISTS))
						ux_copy(outpathnm5, outpathto5, UU_NULL, UX_PRTERRS);
				}
				mode = 0;
				if ( ux_access1(outpathnm6, &mode, UX_PRTERRS) == UU_SUCCESS )
				{
					if (mode != ( mode | UX_NEXISTS))
						ux_copy(outpathnm6, outpathto6, UU_NULL, UX_PRTERRS);
				}
			}
			else 
				ux_copy(outpathnm, outpathto, UU_NULL, UX_PRTERRS);
		}
	}	
	uu_dexit;

}	/* uj_copyf_ppart */

