/*********************************************************************
**    NAME         :  tigmisc.c
**       CONTAINS:
**         iges_parsecmd
**         uig_error
**         uig_init_color
**         uig_init_sublab
**         uig_set_sublab
**         uig_compcv_contin_check
**         uig_change_labelmdl
**         uig_check_uv
**         uig_check_poly_uv 
**         uig_check1_uv
**         uig_put_unistat
**         ncl_create_entity
**
**    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       tigmisc.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       04/05/18 , 15:16:07
*********************************************************************/
#if UU_COMP == UU_WIN2K
#include <string.h>
#endif
#include "usysdef.h"
#include "tiges.h"
#include "umath.h"
#include "mattr.h"
#include "mdattr.h"
#include "mcrv.h"
#include "mdrel.h"
#include "msrf.h"
#include "udebug.h"
#include "mxxx.h"
#include "mdeval.h"
#include "rbase.h"
#include "nccs.h"
#include "class.h"
#include "riddle.h"

char lpt[] = "PTT   ";
char lln[] = "LNN   ";
char lpl[] = "PLL   ";
char lci[] = "CII   ";
char lcv[] = "CVV   ";
char lsf[] = "SFF   ";
char lpv[] = "PVV   ";
char lpn[] = "PNN   ";
char lxx[] = "XXX   ";

extern int NAUTSTEP;

void uig_set_sublab();

/*********************************************************************
**    I_FUNCTION     :  iges_parsecmd(cmdline)
**       parse the command ine of IGES.
**    PARAMETERS
**       INPUT  :
**          cmdline: batch command line
**       OUTPUT :
**          none
**    RETURNS      : 1: display process window
**						0: not display process window
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int iges_parsecmd(cmdline)
char* cmdline;
{
	char *indx, *str;
	int nc, j, i = 0;
	char tmpcmd[UX_MAX_PATH_LEN], tmpcmd2[UX_MAX_PATH_LEN];
	char files[2][UX_MAX_PATH_LEN],ftype[20];
	char *strstr(),*strrchr();
	if (cmdline[0]=='\0')
		return 0;

	strcpy(tmpcmd, cmdline);
	j = 0;
	while ((tmpcmd[j]==' ')||(tmpcmd[j]=='\t'))
		j++;
	strcpy(tmpcmd, &tmpcmd[j]);
	files[0][0] = '\0';
	files[1][0] = '\0';
		
	if (tmpcmd[0]!='\0')
	{
		if (tmpcmd[0]=='\"')
		{
			strcpy(tmpcmd, &(tmpcmd[1]));
			indx = (char*)strchr(tmpcmd, '\"');
			if (indx!=0)
			{
				*indx = '\0';
				strcpy(files[0], tmpcmd);
				strcpy(tmpcmd, indx+1);
			}
		}
		else
		{
			nc = strcspn (tmpcmd, " \t");
			if (nc!=strlen(tmpcmd))
			{
				strncpy(files[0], tmpcmd, nc);
				files[0][nc] = '\0';
				strcpy(tmpcmd, &(tmpcmd[nc+1]));
			}
			else
			{
				strncpy(files[0], tmpcmd, nc);
				files[0][nc] = '\0';
			}
		}
		j = 0;
		while ((tmpcmd[j]==' ')||(tmpcmd[j]=='\t'))
			j++;
		strcpy(tmpcmd, &tmpcmd[j]);
	}
	if (files[0][0] == '\0')
		return 0;
	strcpy(tmpcmd2, tmpcmd);
	str = strstr(tmpcmd, "-out:");
	if (str!=NULL)
	{
		strcpy(tmpcmd, &(str[5]));
		if (tmpcmd[0]!='\0')
		{
			indx = (char*)strchr(tmpcmd, '\"');
			if (indx!=0)
			{
				strcpy(tmpcmd, &(tmpcmd[1]));
				indx = (char*)strchr(tmpcmd, '\"');
				if (indx!=0)
				{
					*indx = '\0';
					strcpy(files[1], tmpcmd);
					strcpy(tmpcmd, indx+1);
				}
			}
			else
			{
				nc = strcspn (tmpcmd, " \t");
				if (nc!=strlen(tmpcmd))
				{
					strncpy(files[1], tmpcmd, nc);
					files[1][nc] = '\0';
					strcpy(tmpcmd, &(tmpcmd[nc+1]));
				}
				else
				{
					strncpy(files[1], tmpcmd, nc);
					files[1][nc] = '\0';
				}
			}
		}
	}
	str = strstr(tmpcmd2, "-progress");
	if (str!=0)
		iges_process = 1;
	else
		iges_process = 0;
	indx = strrchr (files[0], '.');
/*
......Unibase file must have file type of "."
......Otherwise it is an input IGES file
*/
	if (indx != NULL && stricmp(indx+1,"u") != 0)
	{
		strcpy(iges_igsfile, files[0]);
		if (files[1][0] != '\0')
			strcpy(iges_unifile, files[1]);
		else
/*
.....default to iges file name with ".u" extension
*/
		{
			strcpy(iges_unifile, files[0]);
			indx = strrchr (iges_unifile, '.');
			strcpy(indx, ".u");
		}
		iges_in = 1; 
	}
	else
	{
		strcpy(iges_ptfile, files[0]);
		if (files[1][0] != '\0')
			strcpy(iges_outfile, files[1]);
		else
		{
			strcpy(iges_outfile, files[0]);
			indx = strrchr (iges_outfile, '.');
			if (NAUTSTEP) strcpy(ftype,".stp");
			else strcpy(ftype,".igs");
			if (indx == 0) strcat(iges_outfile,ftype);
			else strcpy(indx,ftype);
		}
		iges_in = 0; 
	}
	return iges_process;
}

/*********************************************************************
**    I_FUNCTION     :  uig_error(string)
**				Print and display error messages.
**    PARAMETERS   
**       INPUT  : 
**				string					error message       
**       OUTPUT :  
**				none   
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uig_error(string)
char string[];
{
	if (Iges_batch)
		uig_list_out(string, UU_FALSE);
	else
		uig_list_out(string, UU_TRUE);
}

/*********************************************************************
**		E_FUNCTION     :  uig_init_color(file)
**			Initialize the color arrays.
**
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT : 
**          none 
**    RETURNS      :	none 
**    SIDE EFFECTS :	none
**    WARNINGS     : none	
*********************************************************************/
void uig_init_color()
{
	int	i, entnum,status;
	int color_table[UIG_STDCOLOR][3] =
	{
		0, 0, 0,				/* Black */
		255,  255, 255, 	/* White */
		0, 0, 255,			/* Dodger Blue */
		255,  0,   0, 		/* Red */
		0,    255, 0,		/* Green */
		255,  0,   255, 	/* Magenta */
		255,  255, 0, 		/* Yellow */
		0,    255, 255,	/* Cyan */
		184,  134, 11, 	/* Dark Goldenrod */
		210,  180, 140,	/* Tan */
		173,  216, 230,	/* Light Blue */
		84,   255, 159,	/* SeaGreen1 */
		255,  165, 0,		/* Orange */
		255,  195, 203,	/* Pink */
		221,  160, 221, 	/* Plum */
		192,  192, 192,	/* Gray */
	};
	char color_name[UIG_STDCOLOR][96] =
	{
		"BLACK",
		"WHITE",
		"BLUE",
		"RED",
		"GREEN",
		"MAGNTA",
		"YELLOW",
		"CYAN",
		"BROWN",
		"LTTAN",
		"LTBLUE",
		"SEAGRN",
		"ORANGE",
		"PINK",
		"PURPLE",
		"GREY"
	};
	status = 0;
	for (i=0; i<UIG_STDCOLOR; i++)
	{
		strcpy(uw_color_name[i], color_name[i]);
		uw_color_table[i][0] = color_table[i][0];
		uw_color_table[i][1] = color_table[i][1];
		uw_color_table[i][2] = color_table[i][2];
	}
	for (i=UIG_STDCOLOR; i<UIG_MAXCOLOR; i++)
	{
		uw_color_name[i][0] = '\0';
		uw_color_table[i][0] = 192;
		uw_color_table[i][1] = 192;
		uw_color_table[i][2] = 192;
	}
	if (UIG_color_array!=0)
	{
		UIG_ncolor_iges = 0;
		uu_free(UIG_color_array);
		UIG_color_array = 0;
	}
}

/*********************************************************************
**    I_FUNCTION     :  uig_init_sublab()
**            Initialize subscripted label generation name part.
**    PARAMETERS   
**       INPUT  : 
**            none
**       OUTPUT :  
**            none   
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uig_init_sublab()
	{
	char *p, *ux_getenv();

	iln = 0;
	ipt = 0;
	ipl = 0;
	ici = 0;
	icn = 0;
	icv = 0;
	isf = 0;
	isg = 0;
	ipv = 0;
	ipn = 0;
	ixx = 0;

	if (p=ux_getenv("UG_POINT_NAME"))  uig_set_sublab (p, lpt);
	if (p=ux_getenv("UG_LINE_NAME"))   uig_set_sublab (p, lln);
	if (p=ux_getenv("UG_PLANE_NAME"))  uig_set_sublab (p, lpl);
	if (p=ux_getenv("UG_CIRCLE_NAME")) uig_set_sublab (p, lci);
	if (p=ux_getenv("UG_CURVE_NAME"))  uig_set_sublab (p, lcv);
	if (p=ux_getenv("UG_SURF_NAME"))   uig_set_sublab (p, lsf);
	if (p=ux_getenv("UG_PATERN_NAME")) uig_set_sublab (p, lpn);

	}

/*********************************************************************
**    I_FUNCTION     :  uig_set_sublab(p1,p2)
**            Initialize a subscripted label generation name part.
**    PARAMETERS   
**       INPUT  : 
**	         p1      - name part from environmental variable.
**       OUTPUT :  
**	         p2      - name part, blank filled.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uig_set_sublab(p1,p2)
char *p1,*p2;
	{
	int len;

	len = strlen(p1);
	if (len > 0)
		{
		if (len>6)
			{
			len=6;
			p1[6] = 0;
			}
		strcpy (p2,p1);
		if (len<6) strncpy (&p2[len],"      ",6-len);
		}
	}

/*********************************************************************
**    I_FUNCTION     :  uig_compcv_contin_check(comp_start, comp_end, \
**                                               sub_start,  sub_end, tol)
**       Check to see if subcurve continuously joins up to the composite
**       curve.  The subcurve might be reversed, so check to see if
**       the end of the subcurve is closer, than the start, to the
**       composite.  If this is the second subcurve in the composite
**       and the first subcurve was reversed, we would notice that
**       the new (2nd) subcurve was actually closer to the starting
**       point of the composite curve (1st subcurve).  Finally, check
**       to see if both the composite and the subcurve are reversed.
**
**    PARAMETERS
**       INPUT  :
**            comp_start              Starting point of composite curve
**            comp_end                Ending point of composite curve
**            sub_start               Starting point of subcurve
**            sub_end                 Ending point of subcurve
**            tol                     endpts must be closer than this
**       OUTPUT :
**            pass_flags              Array of test results (1 = pass 0 = not)
**
**    RETURNS      : 1      Start of subcurve is within tol of end of comp
**                   2      End of subcurve is within tol of end of comp
**                   3      Start of subcurve is within tol of start of comp
**                   4      End of subcurve is within tol of start of comp
**                  -1      subcurve too far away, not continuous
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_compcv_contin_check(comp_start, comp_end, sub_start, sub_end, \
                            pass_flags, tol)
UM_coord comp_start, comp_end, sub_start, sub_end;
int pass_flags[4];
UU_REAL tol;
{
    int status, min;
    UU_REAL dist[4], extra;

    dist[0] = um_dcccc(comp_end, sub_start);
    dist[1] = um_dcccc(comp_end, sub_end);
    dist[2] = um_dcccc(comp_start, sub_start);
    dist[3] = um_dcccc(comp_start, sub_end);

    min = -1;
	extra = 0.1 * tol;

    if (dist[0] < tol)
    {
        min = 0;
        pass_flags[0] = 1;
    }
    else
        pass_flags[0] = 0;

    if (dist[1] < tol)
    {
        if (min == -1)
            min = 1;
        else
        {
/*
.....Give a slight preference for the forward orientation. In other
.....words, the reverse orientation must be a better choice by more
.....than (tolerance / 10).
*/
            if ((dist[1] + extra) < dist[min])
                min = 1;
        }

        pass_flags[1] = 1;
    }
    else
        pass_flags[1] = 0;

    if (dist[2] < tol)
    {
        if (min == -1)
            min = 2;
        else
        {
            if (dist[2] + extra < dist[min])
                min = 2;
        }

        pass_flags[2] = 1;
    }
    else
        pass_flags[2] = 0;

    if (dist[3] < tol)
    {
        if (min == -1)
            min = 3;
        else
        {
            if (dist[3] + extra < dist[min])
                min = 3;
        }

        pass_flags[3] = 1;
    }
    else
        pass_flags[3] = 0;


    if (min != -1)
        status = min+1;
    else
        status = -1;

    return (status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_change_labelmdl(change)	
**  If change = UU_TRUE, modify global data structure UM_labelmdl
**  with label and prefix/subscript flag info (either user supplied
**  by an options form or by setting up an IGES modals file).  This
**  will effect the names that geometric entities are automatically
**  assign when converting from IGES to Unibase.
**    PARAMETERS   
**       INPUT  : 
**          change   =  UU_TRUE   Change data structure
**                      UU_FALSE  Read the defaults of UM_labelmdl
**       OUTPUT :  
**    RETURNS      :	UU_SUCCESS or UU_FAILURE 
**    SIDE EFFECTS :	modifies global name modals data structure
**							UM_labelmdl 
**    WARNINGS     : none
**		AUTHOR		 :	Ed Ames  16 Jan 01
*********************************************************************/
int uig_change_labelmdl (change)
int change;
{
	int i, status;
   static int relnum[] = {UM_POINT_REL, NCL_POINTVEC_REL, UM_LINE_REL,
      NCL_VECTOR_REL, NCL_PLN_REL, UM_CIRCLE_REL, UM_RBSPLCRV_REL,
      UM_RBSPLSRF_REL, NCL_SHAPE_REL, NCL_MATRIX_REL, NCL_PATERN_REL,
		UM_SOLID_REL};

 	status = UU_SUCCESS;
		
	for (i = 0; i < NREL; i++)
	{
		if (change == UU_TRUE)
			um_update_rel_label (relnum[i], geo_lab[i], lab_flag[i]);
		else
			um_get_rel_label (relnum[i], geo_lab[i], &lab_flag[i]);
	}

	return (status);
}

/*********************************************************************
** E_FUNCTION: uig_check_uv (uv, keycv)
**       Checks if UV curve (trimmed SF boundry) is in the base surf
**       domain. UV curve can be composite curve.  
**    PARAMETERS
**       INPUT  : uv    - min, max, and periods of u and v
**                        uv[0] = u min
**                        uv[1] = u max
**                        uv[2] = v min
**                        uv[3] = v max
**                        uv[4] = u period ( = 0.0 if not periodic)
**                        uv[5] = u period ( = 0.0 if not periodic)
**                keycv - unibase key to the boudry UV curve
**       OUTPUT : none
**    RETURNS      : 0 = UV curve is OK, -1 = bad UV curve 
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/

int uig_check_uv (uv,keycv)
UU_REAL uv[6];
UU_KEY_ID keycv;
{
	int i, status;
	struct NCL_fixed_databag crv; 
	UM_transf tfmat;

	crv.key = keycv;
	status = -1;
/*
.....The programs that call check_uv are set up to look for when
.....the return value is 0 or -2, so changed status to -2 when it 
.....fails. JLS 1/18/99
*/
	status = -2;
	if (ncl_retrieve_data_fixed (&crv) == UU_SUCCESS)
	{
		status = uc_retrieve_transf (keycv,tfmat);


		if (status == UU_SUCCESS)
		{
			switch (crv.rel_num)
			{
				case UM_COMPCRV_REL:
				{
					struct UM_compcrv_rec *ccrv;
					ccrv = (struct UM_compcrv_rec *) &crv;
					for (i=0; i<ccrv->no_cid && status == UU_SUCCESS; i++)
					{
						status = uig_check_uv (uv, ccrv->cid[i].crvid);
						crv.key = ccrv->cid[i].crvid;
					}
					break;
				}
				case UM_POLYLINE_REL:
					status = uig_check_poly_uv (uv,&crv,tfmat);
					break;
				default:
					status = uig_check1_uv (uv,&crv,tfmat);
			}
		}
	}
	return (status);
}
/*********************************************************************
** E_FUNCTION: uig_check_poly_uv (uv, crv, tfmat)
**       Checks if UV polyline (trimmed SF boundry) is in the base surf
**       domain. 
**    PARAMETERS
**       INPUT  : uv    - min, max, and periods of u and v
**                        uv[0] = u min
**                        uv[1] = u max
**                        uv[2] = v min
**                        uv[3] = v max
**                        uv[4] = u period ( = 0.0 if not periodic)
**                        uv[5] = u period ( = 0.0 if not periodic)
**                crv   - pointer to the boudry UV polyline
**                tfmat - pointer to the id matrix of UV curve.
**       OUTPUT : none
**    RETURNS      : 0 = UV curve is OK, -2 = bad UV curve 
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
int uig_check_poly_uv (uv,crv,tfmat)
UU_REAL uv[6];
struct UM_polyline_rec *crv; 
UM_transf tfmat;
{
	int i, j, status, fail_u, fail_v;
	UU_REAL umin, umax, vmin, vmax, du, dv;
	UM_coord cp;

	status = 0;
	du     = .005*(uv[1]-uv[0]);
	dv     = .005*(uv[3]-uv[2]);
	umin    = uv[0] - du;
	umax    = uv[1] + du;
	vmin    = uv[2] - dv;
	vmax    = uv[3] + dv;
/*
.....check all points of UV polyline for uv limits
*/
	for (i=0, j=0; i<crv->no_pt; i++, j+=3)
	{
		um_cctmtf (&crv->pt[j],tfmat,cp);

		fail_u = fail_v = 0;
		if ((cp[0] < umin) || (cp[0] > umax))
		{
			fail_u = 1;

			if (uv[4] > 0.0)
			{
				while (cp[0] < umin)
				{
					cp[0] += uv[4];
				}
				while (cp[0] > umax)
				{
					cp[0] -= uv[4];
				}
				if ((cp[0] > umin) && (cp[0] < umax))
					fail_u = 0;
			}
		}
		if ((cp[1] < vmin) || (cp[1] > vmax))
		{
			fail_v = 1;

			if (uv[5] > 0.0)
			{
				while (cp[1] < vmin)
				{
					cp[1] += uv[5];
				}
				while (cp[1] > vmax)
				{
					cp[1] -= uv[5];
				}
				if ((cp[1] > vmin) && (cp[1] < vmax))
					fail_v = 0;
			}
		}

		if ((fail_u == 1) || (fail_v == 1))
		{
/*
.....The routines that check uv curves work with 0 and -2, so changed -1
.....to a -2.  JLS 1/18/99
*/
			status = -2;
			break;
		}
	}
	return (status);
}
/*********************************************************************
** E_FUNCTION: uig_check1_uv (uv, crv, tfmat)
**       Checks if UV curve (trimmed SF boundry) is in the base surf
**       domain. UV curve can not be composite curve.  
**    PARAMETERS
**       INPUT  : uv    - min, max, and periods of u and v
**                        uv[0] = u min
**                        uv[1] = u max
**                        uv[2] = v min
**                        uv[3] = v max
**                        uv[4] = u period ( = 0.0 if not periodic)
**                        uv[5] = v period ( = 0.0 if not periodic)
**                crv   - pointer to the boudry UV curve
**                tfmat - pointer to the id matrix of UV curve.
**       OUTPUT : none
**    RETURNS      : 0 = UV curve is OK, -2 = bad UV curve 
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
int uig_check1_uv (uv, crv, tfmat)
UU_REAL uv[6];
struct NCL_fixed_databag *crv; 
UM_transf tfmat;
{
	int i, k, status, fail_u, fail_v, no_inf_lup, maxcount, cou;
	struct UM_evcrvout evout;
	UU_REAL u, umin, umax, vmin, vmax, du, dv;

	status = 0;
	maxcount = 5;
	cou = 0;
	du     = .005*(uv[1]-uv[0]);
	dv     = .005*(uv[3]-uv[2]);
	umin    = uv[0] - du;
	umax    = uv[1] + du;
	vmin    = uv[2] - dv;
	vmax    = uv[3] + dv;
	
    k = crv->data[12];
	
ite:	uc_init_evcrvout (crv,&evout);  
	for (i=0; i<8; i++)
	{
		evout.cp[0]=1.0;
		evout.cp[1]=0.0;
		u = .125*i; 
		status = uc_evcrv (UM_POINT, u, crv, tfmat, &evout);
		fail_u = fail_v = 0;
		if ((evout.cp[0] < umin) || (evout.cp[0] > umax))
		{
			fail_u = 1;

			if (uv[4] > 0.0)
			{
				no_inf_lup = 0;
				while ((evout.cp[0] < umin) && (no_inf_lup < 100))
				{
					evout.cp[0] += uv[4];
					no_inf_lup++;
				}
				no_inf_lup = 0;
				while ((evout.cp[0] > umax) && (no_inf_lup < 100))
				{
					evout.cp[0] -= uv[4];
					no_inf_lup++;
				}
				if ((evout.cp[0] > umin) && (evout.cp[0] < umax))
					fail_u = 0;
			}
		}
		if ((evout.cp[1] < vmin) || (evout.cp[1] > vmax))
		{
			fail_v = 1;

			if (uv[5] > 0.0)
			{
				no_inf_lup = 0;
				while ((evout.cp[1] < vmin) && (no_inf_lup < 100))
				{
					evout.cp[1] += uv[5];
					no_inf_lup++;
				}
				no_inf_lup = 0;
				while ((evout.cp[1] > vmax) && (no_inf_lup < 100))
				{
					evout.cp[1] -= uv[5];
					no_inf_lup++;
				}
				if ((evout.cp[1] > vmin) && (evout.cp[1] < vmax))
					fail_v = 0;
			}
		}
		if ((fail_u == 1) || (fail_v == 1))
		{
/*
.....The routines that check uv curves work with 0 and -2, so changed -1
.....to a -2.  JLS 1/18/99
*/
			status = -2;
			if ((status!=0) && (cou<maxcount))
			{
				crv->data[12]=k+cou;
				cou++;
				goto ite;
			}
			if (status!=0)
				break;
		}
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION :  uig_put_unistat(unistat)
**       Stores the Unibase Statistics record in the Unibase.
**    PARAMETERS
**       INPUT  :
**          unistat - Unibase Statistics record.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_put_unistat(unistat)
struct UR_unistat_rec *unistat;
{
	int status;
/*
.....Store fixed portion of record
*/
	if (unistat->key == 0) return(UU_FAILURE);
	status = ur_update_data(unistat);
/*
.....Update notes if any
*/
/*
	if (unistat->no_notes != 0)
	{
		status = ur_update_data_varlist(unistat->key,1,unistat->notes,1,
			unistat->no_notes);
		uu_free(unistat->notes);
	}
*/
/*
.....End of routine
*/
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  ncl_create_entity (entity)
**       Creates the entity given in the generic entity format in unibase.
**    PARAMETERS
**       INPUT  :
**          entity - generic entity to save
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ncl_create_entity(eptr, view_key)
struct  UM_entitydatabag *eptr;
int view_key;
   {
   struct UM_transf_rec tran;
   struct UC_attributedatabag /*NCL_nclattr_rec*/ attr;
   int i, status;


   uu_denter(UU_MTRC,(us,"ncl_create_entity(rel_num=%d,)",eptr->rel_num));

   /* create UNIBASE entity */
   status = ur_create_data(eptr);

   /* update display segment id */
   status = ur_update_disp_segid(eptr->key, 0);

   /* create UNIBASE attribute bundle and associate with entity */
/*
.......Fixed surface attribute initialization - ASF 7/2/13.
*/
	if (eptr->rel_num == UM_RBSPLSRF_REL || eptr->rel_num == NCL_SURF_REL ||
	eptr->rel_num == NCL_REVSURF_REL || eptr->rel_num == NCL_MESHSURF_REL ||
	eptr->rel_num == NCL_NETSF_REL || eptr->rel_num == NCL_SHAPE_REL ||
	eptr->rel_num == NCL_TRIMSF_REL )
	{
		uig_init_surf_attr(eptr->key,&attr);
	}
	else
	{
		ncl_uni_init_attr_rec(eptr->key, &attr, eptr->rel_num);
	}

   /* create UNIBASE identity transformation and associate with entity */
   tran.key = eptr->key;
   tran.rel_num = UM_TRANSFORM_REL;
   um_tftotf(UM_idmat,tran.tfmat);
   status = ur_update_transf(&tran);

	/* define view priority */
 	/* MPE 2.5 stuff ... **/
	if(view_key == 0)
		{
		ur_update_view_key(eptr->key, 0);
		uu_dexit;
		return(UU_SUCCESS);
		}
	else
		{
		for(i=0;i<no_of_views;i++)
			{
			if(view_key == view_keys[i][0])
				{
				ur_update_view_key(eptr->key, view_keys[i][1]);
				uu_dexit;
				return(UU_SUCCESS);
				}
			}
		}
	/** End of MPE 2.5 stuff **/

   /* make sure the entity can be viewed in all views */
   status = ur_update_view_key(eptr->key, 0);

   uu_dexit;
   return(status);
   }

