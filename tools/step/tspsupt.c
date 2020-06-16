/*********************************************************************
**    NAME         :  tspsupt.c
**       CONTAINS:
**					utp_count_translated
**					utp_get_logicals
**					utp_get_logical_str
**             utp_get_units_str
**					utp_get_translated
**					utp_reset_translated
**					utp_print_statistics
**					utp_remove_dup
**    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tspsupt.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:13:24
*********************************************************************/

#include "mdcoord.h"
#include "mdrel.h"
#include "nccs.h"
#include "nconst.h"
#include "tiges.h"
#include "tigdefs.h"
#include "tstep.h"
#include "udebug.h"
#include "ulist.h"

#define NRELT 14
static int Scount[NRELT]={0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int Snot[NRELT]={0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static char Srelnam[NRELT][20]={"Unknown", "Points", "Lines", "Circles",
	"Matrixes", "Vectors", "B-spline Curves", "Composite Curves",
	"NURBS Surfaces", "Revolved Surfaces", "Trimmed Surfaces", "Solids",
	"Planes","Ellipses"};

/*********************************************************************
**    E_FUNCTION     :  utp_count_translated(relnum,nent,count)
**				Counts an entity as being translated or not translated.
**    PARAMETERS   
**       INPUT  : 
**          relnum   Relation number of entity.
**          nent     Number of entities to count.
**          count    UU_TRUE = this entity was translated.
**                   UU_FALSE = this entity was not translated.
**       OUTPUT : 
**          flags    Logical values stored in command.
**    RETURNS      : UU_SUCESS if parameters are parsed successfully.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_count_translated(relnum,nent,count)
int relnum;
UU_LOGICAL count;
{
	int inc;
/*
.....Determine place setting for entity type
*/
	inc = S_get_relnum_inc(relnum);
/*
.....Mark as counted/uncounted
*/
	if (count) Scount[inc] = Scount[inc] + nent;
	else Snot[inc] = Snot[inc] + nent;
}

/*********************************************************************
**    E_FUNCTION     :  utp_get_logicals(parm,flags,nparm)
**				Parses logical strings (.T., .F., .U., .TRUE., .FALSE.,
**          and .UNDEFINED.)
**          parameters from a STEP command.
**    PARAMETERS   
**       INPUT  : 
**          parm     Pointer to first parameter to parse.
**          nparm    Number of parameters to parse.
**       OUTPUT : 
**          flags    Logical values stored in command.
**    RETURNS      : UU_SUCESS if parameters are parsed successfully.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_get_logicals(parm,flags,nparm)
UTPs_step_parm *parm;
UU_LOGICAL *flags;
int nparm;
{
	int i,status;
/*
.....Parse logical parameters
*/
	for (i=0;i<nparm;i++)
	{
		if (parm[i].type != UTP_STRING) goto failed;
		if (strcmp(parm[i].ptype.str,".T.") == 0 ||
			strcmp(parm[i].ptype.str,".TRUE.") == 0) flags[i] = UU_TRUE;
		else if (strcmp(parm[i].ptype.str,".F.") == 0 ||
			strcmp(parm[i].ptype.str,".FALSE.") == 0) flags[i] = UU_FALSE;
		else if (strcmp(parm[i].ptype.str,".U.") == 0 ||
			strcmp(parm[i].ptype.str,".UNDEFINED.") == 0) flags[i] = UU_FALSE;
		else
			goto failed;
	}
	status = UU_SUCCESS;
	goto done;
/*
.....Error processing parameters
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  utp_get_logical_str(flag,str)
**				Returns the logical string (.T., .F.) based on the settting
**          of the input flag.
**    PARAMETERS   
**       INPUT  : 
**          flag     UU_TRUE or UU_FALSE.
**       OUTPUT : 
**          str      '.T.' or '.F.'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_get_logical_str(flag,str)
UU_LOGICAL flag;
char *str;
{
	if (flag) strcpy(str,".T.");
	else strcpy(str,".F.");
}

/*********************************************************************
**    E_FUNCTION     :  utp_get_units_str(units,fcnv,ustr1,ustr2,astr1,astr2)
**				Returns the units conversion factor, units strings, and
**          angle units.
**    PARAMETERS   
**       INPUT  : 
**          units    0 = Inches are active, 1 = Millimeters.
**       OUTPUT : 
**          fcnv     .0254 for Inches, 1. for Millimeters.
**          ustr1    INCH or MILLIMETRE.
**          ustr2    .METRE.
**          astr1    RADIAN
**          astr2    .STERADIAN.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_get_units_str(units,fcnv,ustr1,ustr2,astr1,astr2)
int units;
UU_REAL *fcnv;
char *ustr1,*ustr2,*astr1,*astr2;
{
/*
.....Inches
*/
	if (units == 0)
	{
		*fcnv = .0254;
		strcpy(ustr1,"INCH");
		strcpy(ustr2,".METRE.");
	}
/*
.....Millimeters
*/
	else
	{
		*fcnv = 1.;
		strcpy(ustr1,"MILLIMETRE");
		strcpy(ustr2,".METRE.");
	}
/*
.....Angular units
*/
	strcpy(astr1,".RADIAN.");
	strcpy(astr2,".STERADIAN.");
}

/*********************************************************************
**    E_FUNCTION     :  utp_get_translated(inc,nent,label,which)
**				Returns the number of entity types that were translated.
**    PARAMETERS   
**       INPUT  : 
**          inc      Index into relation table to return count for.
**          which    UU_TRUE = return count from translated entities.
**                   UU_FALSE = return count from not translated entities.
**       OUTPUT : 
**          inc      Points to next entity or -1 if no more entities.
**          nent     Number of entities translated.
**          label    Name of translated entities.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_get_translated(inc,nent,label,which)
int *inc,*nent;
char *label;
UU_LOGICAL which;
{
/*
.....Index out of range
*/
	if (*inc <0 || *inc >= NRELT)
	{
		*nent = 0;
		*inc = -1;
	}
/*
.....Return number of translated entities
*/
	else
	{
		if (which) *nent = Scount[*inc];
/*
.....Return number of not translated entities
*/
		else *nent = Snot[*inc];
	}
/*
.....Return label of entity and increment index
*/
	strcpy(label,Srelnam[*inc]);
	*inc = *inc + 1;
	if (*inc >= NRELT) *inc = -1;
}

/*********************************************************************
**    E_FUNCTION     :  utp_reset_translated()
**				Resets the number of translated entities to 0.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_reset_translated()
{
	int i;
/*
.....Clear translated counts
*/
	for (i=0;i<NRELT;i++) Scount[i] = Snot[i] = 0;
}

/*********************************************************************
**    E_FUNCTION     :  utp_reset_translated()
**				Resets the number of translated entities to 0.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_print_statistics()
{
	int inc,nent;
	char label[20];
/*
.....Print entities translated
*/
	inc =0;
	uig_list_out("\nTranslation Statitics\n",UU_TRUE);
	uig_list_out("---------------------\n",UU_TRUE);
	do
	{
		utp_get_translated(&inc,&nent,label,UU_TRUE);
		if (nent != 0)
		{
			sprintf(p_buff,"%d %s Translated.\n",nent,label);
			uig_list_out(p_buff,UU_TRUE);
		}
	} while (inc >= 0);
/*
.....Print entities not translated
*/
	uig_list_out(" \n",UU_TRUE);
	inc =0;
	do
	{
		utp_get_translated(&inc,&nent,label,UU_FALSE);
		if (nent != 0)
		{
			sprintf(p_buff,"%d %s Not Translated.\n",nent,label);
			uig_list_out(p_buff,UU_TRUE);
		}
	} while (inc >= 0);
}

/*********************************************************************
**    E_FUNCTION     :  utp_remove_dup(eptr,key)
**       Deletes the entity from the Unibase and increases the duplicates
**       count.
**    PARAMETERS   
**       INPUT  : 
**          key    - Key of entity to be deleted.
**       OUTPUT :
**          key    - Reset to 0.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_remove_dup(key)
UU_KEY_ID *key;
{
	ur_delete_all(*key);
	*key = 0;
	UIG_dupcount++;
}

/*********************************************************************
**    I_FUNCTION     :  S_get_relnum_inc(relnum)
**				Returns the index into the relation count arrays.
**    PARAMETERS   
**       INPUT  : 
**          relnum   Relation number of entity.
**       OUTPUT : none
**    RETURNS      : Index into arrays for relation number;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_get_relnum_inc(relnum)
int relnum;
{
	int inc;
/*
.....Return the relation index 
*/
	switch(relnum)
	{
	case UM_POINT_REL:
		inc = 1; break;
	case UM_LINE_REL:
		inc = 2; break;
	case UM_CIRCLE_REL:
		inc = 3; break;
	case UM_RBSPLCRV_REL:
		inc = 6; break;
	case UM_COMPCRV_REL:
		inc = 7; break;
	case UM_RBSPLSRF_REL:
		inc = 8; break;
	case NCL_REVSURF_REL:
		inc = 9; break;
	case NCL_TRIMSF_REL:
		inc = 10; break;
	case UM_SOLID_REL:
		inc = 11; break;
	case NCL_PLN_REL:
		inc = 12; break;
	case UM_CONIC_REL:
		inc = 13; break;
	default:
		inc = 0; break;
	}
	return(inc);
}
