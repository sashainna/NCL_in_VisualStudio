/********************************************************************r
**    NAME: m5esrf1.c
**       CONTAINS: query routines for AG surfaces
**			int um_agsrf_query(key, list)
**  COPYRIGHT  1986  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       m5esrf1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:06
**********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "mderror.h"
#include "mdebug.h"
#include "ulist.h"
#include "mdgenent.h"
#include "mdrel.h"
#include "mdeval.h"
#include "msrf.h"
#include "mattr.h"
#include "mdunits.h"
#include "mdcpln.h"
#include "class.h"
#include "jquery.h"

#include "ag_incl.h"
#include "ag_crv_def.h"
#include "ag_srf_def.h"

um_len_query(list, str, len)
	UU_LIST *list;
	char str[];
	UM_length len;

	{
	UM_length elen;
	char buf[256];

	UM_len_inttoext(len, elen);
	sprintf(buf, "%s   %8.3f                      %8.3f", str, elen, elen);
	uj_putstr(list, buf);
	}

um_cc_query(list, str, cc)
	UU_LIST *list;
	char str[];
	UM_coord cc;

	{
	UM_coord ecc, wpecc;
	char buf[256];

	UM_cc_inttoext(cc, ecc);
	um_mcstoccs(0, ecc, wpecc);
	sprintf(buf,"%s  <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>",
		str, ecc[0], ecc[1], ecc[2], wpecc[0], wpecc[1], wpecc[2]);
	uj_putstr(list, buf);
	}

um_uvc_query(list, str, uvc)
	UU_LIST *list;
	char str[];
	UM_vector uvc;

	{
	UM_vector wpuvc;
	char buf[256];

	um_mcstoccs(1, uvc, wpuvc);
	sprintf(buf,"%s  <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>",
		str, uvc[0], uvc[1], uvc[2], wpuvc[0], wpuvc[1], wpuvc[2]);
	uj_putstr(list, buf);
	}

um_vc_query(list, str, vc)
	UU_LIST *list;
	char str[];
	UM_vector vc;

	{
	UM_vector evc,wpevc;
	char buf[256];

	UM_cc_inttoext(vc, evc);
	um_mcstoccs(1, evc, wpevc);
	sprintf(buf,"%s  <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>",
		str, evc[0], evc[1], evc[2], wpevc[0], wpevc[1], wpevc[2]);
	uj_putstr(list, buf);
	}

/*********************************************************************
**    I_FUNCTION : int um_agsrf_query(key, list)
**			This function produces the query output for an agsrf.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS:
**			UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agsrf_query(key, list)
	UU_KEY_ID	key;
	UU_LIST 	*list;

	{
	int status;
	struct UM_agsrf_rec srf;
	struct UM_attrdata_rec attr;
	char	 buf[256];

	uu_denter(UU_MTRC,(us, "um_agsrf_query(key=%d)", key));

	/* get the entity data */
	srf.key = key;
	status = uc_retrieve_data(&srf, sizeof(struct UM_agsrf_rec));
	if (status != UU_SUCCESS) goto done;
	status = uc_retrieve_attr(key, &attr);
	if (status != UU_SUCCESS) goto done;

	/* print surface information */
	umi_agsrf_query(&srf, list);

done:;
	uu_dexitstatus("um_agsrf_query", status);
	return(status);
	}	

/*********************************************************************
**    I_FUNCTION : int umi_agsrf_query(srf, list)
**			This function produces the query output for a surface (SRF).
**    PARAMETERS   
**       INPUT  : 
**				srf					surface entity
**				list					list to put data onto
**       OUTPUT :  
**    RETURNS:
**			UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umi_agsrf_query(srf, list)
	struct UM_agsrf_rec *srf;
	UU_LIST 	*list;

	{
	int status;
	int ok;
	char	 buf[256];
	AG_SURFACEP sf;
	AG_SRF_DATA sfdata;
	AG_PLN_DATAP plane;
	AG_CYL_DATAP cyl;
	AG_CNE_DATAP cone;
	AG_SPH_DATAP sph;
	AG_TOR_DATAP torus;
	AG_SRV_DATAP revsrf;

	uu_denter(UU_MTRC,(us, "umi_agsrf_query(key=%d)", srf->key));

	/* print surface specific data */
	sf = (AG_SURFACEP) srf->srfaddr;

	sprintf(buf, "                           Model Space                        Working Plane");
	uj_putstr(list, buf);

	/* surface specific data */
	ok = ag_pro_srf(sf, &sfdata);
	if (!ok)
		{
		uj_putstr(list, " ");
		uj_putstr(list, " ");
		uj_putstr(list, "error in surface properties calculation");
		goto done;
		}

	switch(sf->stype)
		{
		case AG_PLANE:
			{
			plane = (AG_PLN_DATAP) &(sfdata.srf_data.plane);

			sprintf(buf, "Plane");
			uj_putstr(list, buf);
			uj_putstr(list, " ");

			if (srf->rev_normal) um_vctmsc(plane->N, -1.0, plane->N);
			um_cc_query(list,		"point      ", plane->P);
			um_uvc_query(list,	"normal     ", plane->N);

			}
			break;

		case AG_CYLINDER:
			{
			cyl = (AG_CYL_DATAP) &(sfdata.srf_data.cylinder);

			sprintf(buf,"Cylinder");
			uj_putstr(list, buf);
			uj_putstr(list, " ");

			um_len_query(list,	"radius     ", cyl->radius);
			um_cc_query(list,		"top point  ", cyl->P0);
			um_cc_query(list,		"bot point  ", cyl->P1);
			um_uvc_query(list,	"axis       ", cyl->AXIS);
			um_len_query(list,	"height     ", cyl->height);

			}
			break;

		case AG_CONE:
			{
			cone = (AG_CNE_DATAP) &(sfdata.srf_data);

			sprintf(buf,"Cone");
			uj_putstr(list, buf);
			uj_putstr(list, " ");

			um_len_query(list,	"top radius ", cone->rad0);
			um_cc_query(list,		"    point  ", cone->P0);
			um_len_query(list,	"bot radius ", cone->rad1);
			um_cc_query(list,		"    point  ", cone->P1);
			um_uvc_query(list,	"axis       ", cone->AXIS);
			um_len_query(list,	"height     ", cone->height);

			}
			break;

		case AG_SPHERE:
			{
			sph = (AG_SPH_DATAP) &(sfdata.srf_data);

			sprintf(buf,"Sphere");
			uj_putstr(list, buf);
			uj_putstr(list, " ");

			um_cc_query(list,		"center     ", sph->CENTER);
			um_len_query(list,	"radius     ", sph->radius);

			}
			break;

		case AG_TORUS:
			{
			torus = (AG_TOR_DATAP) &(sfdata.srf_data);

			sprintf(buf,"Torus");
			uj_putstr(list, buf);
			uj_putstr(list, " ");

			um_cc_query(list,		"center     ", torus->CENTER);
			um_uvc_query(list,	"axis       ", torus->AXIS);
			um_len_query(list,	"maj radius ", torus->major_radius);
			um_len_query(list,	"min radius ", torus->minor_radius);

			}
			break;

		case AG_SRF_REV:
			{
			revsrf = (AG_SRV_DATAP) &(sfdata.srf_data);

			sprintf(buf,"Rev Surf");
			uj_putstr(list, buf);
			uj_putstr(list, " ");

			um_cc_query(list,		"axis point ", revsrf->C);
			um_uvc_query(list,	"     vector", revsrf->AXIS);
			um_len_query(list,	"radius     ", revsrf->rad);

			}
			break;

		default:
			sprintf(buf,"Rational Bspline");
			uj_putstr(list, buf);
			uj_putstr(list, " ");
			break;
		}
	uj_putstr(list, " ");
	uj_putstr(list, " ");

	/* U direction data */
	sprintf(buf, "  U curves: ");
	sprintf(buf, "%s  degree=%d ", buf, sf->mu);
	if (sf->ratu == 0)
		sprintf(buf, "%s  non-rational", buf);
	else if (sf->ratu == 1)
		sprintf(buf, "%s  rational    ", buf);
	else if (sf->ratu == -1)
		sprintf(buf, "%s  homogeneous ", buf);
	if (sf->formu == 0)
		sprintf(buf, "%s  open     ", buf);
	else if (sf->formu == 1)
		sprintf(buf, "%s  closed   ", buf);
	else if (sf->formu == 2)
		sprintf(buf, "%s  periodic ", buf);
	if (sf->poleu == 0)
		sprintf(buf, "%s  no poles       ", buf);
	else if (sf->ratu == 1)
		sprintf(buf, "%s  pole at u0     ", buf);
	else if (sf->ratu == 2)
		sprintf(buf, "%s  pole at u1     ", buf);
	else if (sf->ratu == 3)
		sprintf(buf, "%s  pole at u0 & u1", buf);
	sprintf(buf, "%s", buf);
	uj_putstr(list, buf);

	/* V direction data */
	sprintf(buf, "  V curves: ");
	sprintf(buf, "%s  degree=%d ", buf, sf->mv);
	if (sf->ratv == 0)
		sprintf(buf, "%s  non-rational", buf);
	else if (sf->ratv == 1)
		sprintf(buf, "%s  rational    ", buf);
	else if (sf->ratv == -1)
		sprintf(buf, "%s  homogeneous ", buf);
	if (sf->formv == 0)
		sprintf(buf, "%s  open     ", buf);
	else if (sf->formv == 1)
		sprintf(buf, "%s  closed   ", buf);
	else if (sf->formv == 2)
		sprintf(buf, "%s  periodic ", buf);
	if (sf->polev == 0)
		sprintf(buf, "%s  no poles       ", buf);
	else if (sf->ratv == 1)
		sprintf(buf, "%s  pole at u0     ", buf);
	else if (sf->ratv == 2)
		sprintf(buf, "%s  pole at u1     ", buf);
	else if (sf->ratv == 3)
		sprintf(buf, "%s  pole at u0 & u1", buf);
	sprintf(buf, "%s", buf);
	uj_putstr(list, buf);
	uj_putstr(list, " ");

	status = UU_SUCCESS;

done:;
	uu_dexitstatus("umi_agsrf_query", status);
	return(status);
	}	
