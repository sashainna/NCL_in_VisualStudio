/********************************************************************r
**    NAME: m2query.c
**       CONTAINS:
**         um_query()
**         um_point_query()
**         um_line_query()
**         um_vector_query()
**         um_pntvec_query()
**         um_matrix_query()
**         um_circle_query()
**         um_conic_query()
**         um_compcrv_query()
**         um_patern_query()
**         um_curve_query()
**         um_surf_query()
**         um_revsurf_query()
**         um_netsf_query()
**         um_shape_query()
**         um_primitive_query()
**         um_meshsf_query()
**  COPYRIGHT  1986  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**      m2query.c , 25.1 
**  DATE AND TIME OF LAST MODIFICATION
**      04/29/15 , 15:07:48 
**********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "mderror.h"
#include "mdebug.h"
#include "ulist.h"
#include "mdgenent.h"
#include "nclfc.h"
#include "mfort.h"
#include "mdrel.h"
#include "mdeval.h"
#include "mcrv.h"
#include "msrf.h"
#include "mxxx.h"
#include "mattr.h"
#include "mdunits.h"
#include "mdcpln.h"
#include "modef.h"
#include "nccs.h"
#include "ncl.h"
#include "class.h"		/* for UC_attributedatabag */
#define UJ_QPGM		/* to allocate storage in jquery.h */
#include "jquery.h"	/* query macros, etc. */
#undef UJ_QPGM
#include "nclvx.h"
#include "mrender.h"
static char label[NCL_MAX_LABEL_AND_SUBSCRIPT];

/*********************************************************************
**    I_FUNCTION: int um_query(key, list) 
**      Build a character array of the data from unibase
**      given an entity key, data array and the extants of
**      of that array.
**	
**		  This function must be modified much the same as a 
**		  dispatcher to take into account new entities in Unibase.
**
**      If this function cannot extract the contents for the
**		  given key, the function returns the following strings
**		  in the first 3 rows:
**				
**				Entity type: ???
**				 key . . . . . . . . . . . . (input key)
**				 rel_num . . . . . . . . . . (entity relation number)
**	
**			Additionally the function returns a status of UU_FAILURE
**			so the calling application has the option of operating
**			on the contents of the data area or not.
**    PARAMETERS   
**       INPUT  :
**				key					Unibase key.
**				data     			2-D character array.
**				maxRows				Number of rows in array.
**				maxCols 				Maximum number of characters per row including 
**										the string terminator, "\0".	
**       OUTPUT :
**				lines    			Number of rows containing data.
**    RETURNS      : UU_SUCCESS    if data has been extracted.
**							UU_FAILURE    if rows < 3, or if maxCols < 51.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_query(key, list) 
	UU_KEY_ID  key;
	UU_LIST 	  *list;

	{
	int  status = UU_SUCCESS;
	UM_transf tfmat;
	struct UM_entitydatabag e;
	struct UC_attributedatabag attr;
	
   uu_denter(UU_MTRC, (us, "um_query(key=%d)", key));

	e.key = key;
	if (uc_retrieve_data(&e, sizeof(struct UM_entitydatabag)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
	if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS)	goto failed;
	UJ_PUT0(list,"- Unknown Method for the individual information. -");
	goto done;

failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("um_query", status);
	return(status);
	}	/* um_query */

/*********************************************************************
**    I_FUNCTION : int um_point_query(key, list)
**			This function produces the query output for a point.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_point_query(key, list)
	UU_KEY_ID	key;
	UU_LIST 	*list;

	{
    UM_int2 idx, ival;
	struct UM_point_rec 	pt;
	struct UM_attrdata_rec  attr;
	UM_transf tfmat;
	char	 buf[300];
	UM_coord spt, wpspt;
	int	status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us, "um_point_query(key=%d)", key));

	/* get the entity data */
	pt.key = key;
	if (uc_retrieve_data(&pt, sizeof(struct UM_point_rec)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
	if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS)	goto failed;
	if (uc_transform(&pt, tfmat, UU_FALSE) != UU_SUCCESS) goto failed;

	sprintf(buf, "                     Model Space                           Working Plane");
	uj_putstr(list, buf);
	uj_putstr(list, " ");

	UM_cc_inttoext(pt.pt, spt);
	um_mcstoccs(0, pt.pt, wpspt);
	UM_cc_inttoext(wpspt, wpspt);

    idx = 264;
    getifl (&idx, &ival);
    if (ival == 0)        /* inches  */
	    sprintf(buf,"point    <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
		spt[0], spt[1], spt[2],
		wpspt[0], wpspt[1], wpspt[2]);
    else
        sprintf(buf,"point    <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
        spt[0], spt[1], spt[2],
        wpspt[0], wpspt[1], wpspt[2]);

	uj_putstr(list, buf);

	goto done;

failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("um_point_query", status);
	return(status);
	}	

/*********************************************************************
**    I_FUNCTION : int um_relation_name(relnum, relname)
**			Retrieve query relation name (RELNAME) for the specified 
**			relation (RELNUM). If this routine does not have a name
**			in its case statment, it will retrieve the relation name
**			from UNIBASE. 
**    PARAMETERS   
**       INPUT  : 
**				relnum				UNIBASE relation number
**       OUTPUT :  
**				relname				relation name
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_relation_name(key, relname)
	UU_KEY_ID key;
	char relname[];

	{
	int status = UU_SUCCESS;
	int relnum;

	uu_denter(UU_MTRC,(us, "um_relation_name(key=%d)", key));

	ur_retrieve_data_relnum(key, &relnum);

	switch (relnum)
		{
		case UM_POINT_REL:
			strcpy(relname, "POINT");
			break;
		case UM_LINE_REL:
			strcpy(relname, "LINE");
			break;
		case UM_CIRCLE_REL:
			strcpy(relname, "ARC/CIRCLE");
			break;
		case UM_CONIC_REL:
			strcpy(relname, "CONIC");
			break;
		case UM_COMPCRV_REL:
			strcpy(relname, "COMPOSITE");
			break;
		case UM_RBSPLCRV_REL:
		case UM_AGCRV_REL:
			strcpy(relname, "BSPLINE");
			break;
		case UM_AGSRF_REL:
			strcpy(relname, "SURFACE");
			break;
		case UM_AGSHELL_REL:
			strcpy(relname, "SHELL");
			break;
		case UM_BODY_REL:
			strcpy(relname, "ROMULUS SOLID");
			break;
		case UM_POLY_REL:
			strcpy(relname, "POLYGON");
			break;
		case UM_POLYLINE_REL:
			strcpy(relname, "POLYLINE");
			break;
		case UA_LINEAR_DIMS_REL:
			strcpy(relname, "DRAFTING");
			break;
		case UA_TEXT_REL:
			strcpy(relname, "TEXT");
			break;
		case UB_INSTANCE_REL:
			strcpy(relname, "SYMBOL");
			break;
		case UM_UVCVONSF_REL:
			strcpy(relname, "SSPLINE");
			break;
		default:
			ur_retrieve_rel_name(key, relname);
			break;
		}

	uu_dexitstatus("um_relation_name", status);
	return(status);
	}	

/*********************************************************************
**    I_FUNCTION : int um_line_query(key, list)
**			This function produces the query output for a line.
**    PARAMETERS   
**       INPUT  : 
**          lineptr	Pointer to the line entity being queries.
**				attrptr	Pointer to the attribute bundle for the line.
**				data		Storage for query buffer.
**				maxRows	Number rows in the output array.
**				maxCols 	Maximum number of characters per row including the string 
**							terminator, "\0".	
**       OUTPUT :  
**				data			Filled query buffer.
**				rowsUsedptr	Pointer to the number of rows used in "data".
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_line_query(key, list)
	UU_KEY_ID	key;
	UU_LIST	*list;

	{
	struct UM_line_rec 	lnptr;
    UM_int2 idx, ival;
	struct UM_attrdata_rec 	attr;
	UM_transf tfmat;
	UU_REAL	len, extlen;
	UU_REAL	ang, intang;
	UM_coord	origin;
	UM_coord spt, wpspt;
	UM_coord ept, wpept;
	UM_vector	xaxis, yaxis, zaxis;
	UM_vector	vec;
	char buf[256];
	int  status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us, "um_line_query(key=%d)", key));

	/* get the entity data */
	lnptr.key = key;
	if (uc_retrieve_data(&lnptr, sizeof(struct UM_line_rec)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
	if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS)	goto failed;
	if (uc_transform(&lnptr, tfmat, UU_FALSE) != UU_SUCCESS) goto failed;

	sprintf(buf, "                     Model Space                           Working Plane");
	uj_putstr(list, buf);
	uj_putstr(list, " ");

	UM_cc_inttoext(lnptr.spt, spt);
	um_mcstoccs(0, lnptr.spt, wpspt);
	UM_cc_inttoext(wpspt, wpspt);
    idx = 264;           /* inches or millimeters */
    getifl (&idx, &ival);
    if (ival == 0)       /* inches */
	    sprintf(buf,"start   <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
		spt[0], spt[1], spt[2],
		wpspt[0], wpspt[1], wpspt[2]);
    else                 /* millimeters */
        sprintf(buf,"start   <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
        spt[0], spt[1], spt[2],
        wpspt[0], wpspt[1], wpspt[2]);
	uj_putstr(list, buf);

	UM_cc_inttoext(lnptr.ept, ept);
	um_mcstoccs(0, lnptr.ept, wpept);
	UM_cc_inttoext(wpept, wpept);
    if (ival == 0)       /* inches */
	    sprintf(buf,"end     <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>",
		ept[0], ept[1], ept[2],
		wpept[0], wpept[1], wpept[2]);
    else                 /* millimeters */
        sprintf(buf,"end     <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
        ept[0], ept[1], ept[2],
        wpept[0], wpept[1], wpept[2]);
	uj_putstr(list, buf);

	uj_putstr(list, " ");
	uj_putstr(list, " ");

	len = um_getarclen(&lnptr,UM_DEFAULT_TF);
	UM_len_inttoext(len, extlen);
    if (ival == 0)       /* inches */
    {
        UJ_PUT1(list,"Length:                %10.5f", extlen);
    }
    else
    {
        UJ_PUT1(list,"Length:                %10.4f", extlen);
    }

	um_vcmnvc(lnptr.ept, lnptr.spt, vec);
	um_getcpln(origin, xaxis, yaxis, zaxis);
	ang = um_angle(xaxis, vec);
	UM_ang_inttoext(ang, intang);

    if (ival == 0)       /* inches */
    {
	    UJ_PUT1(list,"Angle to WP-xaxis:     %10.5f", intang);
    }
    else                 /* millimeters */
    {
        UJ_PUT1(list,"Angle to WP-xaxis:     %10.4f", intang);
    }

	goto done;

failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("um_line_query", status);
	return(status);
	}	

/*********************************************************************
**    I_FUNCTION : int um_circle_query(key, list)
**			This function produces the query output for a circle.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_circle_query(key, list)
	UU_KEY_ID	key;
	UU_LIST	*list;

	{
    UM_int2 idx, ival;
	struct UM_circle_rec 	circptr;
	struct UM_attrdata_rec  attr;
	UM_transf tfmat;
	struct UM_evcrvout  crvout;
	UM_length radius;
	UM_length arclen;
	UM_angle ang;
	UM_coord center, wpcenter;
	UM_vector wpnvec;
	UM_vector wpsvec;
	UM_coord spt;
	UM_coord origin;
	UM_vector xaxis, yaxis, zaxis;
	UM_vector vec;
	UU_REAL um_getarclen();
	char buf[256];
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us, "um_circle_query(key=%d)", key));

	/* get the entity data */
	circptr.key = key;
	if (uc_retrieve_data(&circptr, sizeof(struct UM_circle_rec)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
	if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS)	goto failed;
	if (uc_transform(&circptr, tfmat, UU_FALSE) != UU_SUCCESS) goto failed;

	sprintf(buf, "                     Model Space                           Working Plane");
	uj_putstr(list, buf);
	uj_putstr(list, " ");

	UM_len_inttoext(circptr.radius, radius);

    idx = 264;           /* inches or millimeters */
    getifl (&idx, &ival);
    if (ival == 0)       /* inches */
	    sprintf(buf,"radius   %10.5f                          %10.5f", 
		radius, radius);
    else
        sprintf(buf,"radius   %10.4f                          %10.4f",
        radius, radius);

	uj_putstr(list, buf);

	arclen = um_getarclen(&circptr, UM_idmat);
	UM_len_inttoext(arclen, arclen);

    if (ival == 0)       /* inches */
	    sprintf(buf,"length   %10.5f                          %10.5f", 
		arclen, arclen);
    else
        sprintf(buf,"length   %10.4f                          %10.4f",
        arclen, arclen);

	uj_putstr(list, buf);

	UM_ang_inttoext(circptr.dang, ang);

    if (ival == 0)       /* inches */
	    sprintf(buf,"angle    %10.5f                          %10.5f", 
		ang, ang);
    else
        sprintf(buf,"angle    %10.4f                          %10.4f",
        ang, ang);

	uj_putstr(list, buf);

	UM_cc_inttoext(circptr.center, center);
	um_mcstoccs(0, circptr.center, wpcenter);
	UM_cc_inttoext(wpcenter, wpcenter);

    if (ival == 0)       /* inches */
	    sprintf(buf,"center  <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
		center[0], center[1], center[2],
		wpcenter[0], wpcenter[1], wpcenter[2]);
    else       
        sprintf(buf,"center  <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
        center[0], center[1], center[2],
        wpcenter[0], wpcenter[1], wpcenter[2]);

	uj_putstr(list, buf);

	um_mcstoccs(1, circptr.nvec, wpnvec);
    
    if (ival == 0)       /* inches */
	    sprintf(buf,"normal  <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
		circptr.nvec[0], circptr.nvec[1], circptr.nvec[2],
		wpnvec[0], wpnvec[1], wpnvec[2]);
    else
        sprintf(buf,"normal  <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
        circptr.nvec[0], circptr.nvec[1], circptr.nvec[2],
        wpnvec[0], wpnvec[1], wpnvec[2]);

	uj_putstr(list, buf);

	um_mcstoccs(1, circptr.svec, wpsvec);
    
    if (ival == 0)       /* inches */
	    sprintf(buf,"startvec<%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
		circptr.svec[0], circptr.svec[1], circptr.svec[2],
		wpsvec[0], wpsvec[1], wpsvec[2]);
    else
        sprintf(buf,"startvec<%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
        circptr.svec[0], circptr.svec[1], circptr.svec[2],
        wpsvec[0], wpsvec[1], wpsvec[2]);

	uj_putstr(list, buf);

	uc_evcrv(UM_POINT, (UU_REAL) 0.0, &circptr, UM_DEFAULT_TF, &crvout);
	um_vctovc(crvout.cp, spt);
	uc_evcrv(UM_POINT, (UU_REAL) 1.0, &circptr, UM_DEFAULT_TF, &crvout);
	um_vcmnvc(crvout.cp, spt, vec);
	um_getcpln(origin, xaxis, yaxis, zaxis);
	ang = um_angle(xaxis, vec);
	UM_ang_inttoext(ang, ang);
	uj_putstr(list, " ");
	uj_putstr(list, " ");
	sprintf(buf, "Angle of the Circular Arc -");
	uj_putstr(list, buf);

    if (ival == 0)       /* inches */
	    sprintf(buf, "Line Angle  %10.5f", ang);
    else
        sprintf(buf, "Line Angle  %10.4f", ang);

	uj_putstr(list, buf);

	goto done;

failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("um_circle_query", status);
	return(status);
	}	

/*********************************************************************
**    I_FUNCTION : int um_conic_query(key, list)
**			This function produces the query output for a conic.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_conic_query(key, list)
	UU_KEY_ID	key;
	UU_LIST	*list;

	{
	struct UM_conic_rec  	conicptr;
	struct UM_attrdata_rec  attr;
	UM_transf tfmat;
	int status = UU_SUCCESS;
	UM_cn_defn cn_defn;
	UM_length arclen;
	UM_coord spt, wpspt;
	UM_coord ept, wpept;
	UM_vector spt_derv, wpspt_derv;
	UM_vector ept_derv, wpept_derv;
	UM_coord pln_point, wppln_point;
	UM_vector wppln_normal;
	UM_coord center, wpcenter;
	UM_vector wpmajor_axis;
	UM_length major_length;
	UM_vector wpminor_axis;
	UM_length minor_length;
	int ii;
	char buf[256];

	uu_denter(UU_MTRC,(us, "um_conic_query(key:%d)", key));

	/* get the entity data */
	conicptr.key = key;
	if (uc_retrieve_data(&conicptr, sizeof(struct UM_conic_rec)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
	if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS)	goto failed;
	um_cn_defn(&conicptr, tfmat, &cn_defn);

	sprintf(buf, "                              Model Space                       Working Plane");
	uj_putstr(list, buf);
	uj_putstr(list, " ");

	UM_len_inttoext(cn_defn.arc_length, arclen);
	sprintf(buf,"curve length      %10.5f                      %10.5f", 
		arclen, arclen);
	uj_putstr(list, buf);
	uj_putstr(list, " ");

	UM_cc_inttoext(cn_defn.spt, spt);
	um_mcstoccs(0, cn_defn.spt, wpspt);
	UM_cc_inttoext(wpspt, wpspt);
	sprintf(buf,"start point      <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
		spt[0], spt[1], spt[2],
		wpspt[0], wpspt[1], wpspt[2]);
	uj_putstr(list, buf);

	UM_cc_inttoext(cn_defn.spt_derv, spt_derv);
	um_mcstoccs(1, cn_defn.spt_derv, wpspt_derv);
	UM_cc_inttoext(wpspt_derv, wpspt_derv);
	sprintf(buf,"      derivative <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
		cn_defn.spt_derv[0], cn_defn.spt_derv[1], cn_defn.spt_derv[2],
		wpspt_derv[0], wpspt_derv[1], wpspt_derv[2]);
	uj_putstr(list, buf);

	sprintf(buf,"      parameter   %10.5f                      %10.5f", 
		conicptr.t0, conicptr.t0);
	uj_putstr(list, buf);
	uj_putstr(list, " ");

	UM_cc_inttoext(cn_defn.ept, ept);
	um_mcstoccs(0, cn_defn.ept, wpept);
	UM_cc_inttoext(wpept, wpept);
	sprintf(buf,"end   point      <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
		ept[0], ept[1], ept[2],
		wpept[0], wpept[1], wpept[2]);
	uj_putstr(list, buf);

	UM_cc_inttoext(cn_defn.ept_derv, ept_derv);
	um_mcstoccs(1, cn_defn.ept_derv, wpept_derv);
	UM_cc_inttoext(wpept_derv, wpept_derv);
	sprintf(buf,"      derivative <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
		ept_derv[0], ept_derv[1], ept_derv[2],
		wpept_derv[0], wpept_derv[1], wpept_derv[2]);
	uj_putstr(list, buf);

	sprintf(buf,"      parameter    %10.5f                      %10.5f", 
		conicptr.t1, conicptr.t1);
	uj_putstr(list, buf);
	uj_putstr(list, " ");

	UM_cc_inttoext(cn_defn.pln_point, pln_point);
	um_mcstoccs(0, cn_defn.pln_point, wppln_point);
	UM_cc_inttoext(wppln_point, wppln_point);
	sprintf(buf,"plane point      <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
		pln_point[0], pln_point[1], pln_point[2],
		wppln_point[0], wppln_point[1], wppln_point[2]);
	uj_putstr(list, buf);

	um_mcstoccs(1, cn_defn.pln_normal, wppln_normal);
	sprintf(buf,"      normal     <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
		cn_defn.pln_normal[0], cn_defn.pln_normal[1], cn_defn.pln_normal[2],
		wppln_normal[0], wppln_normal[1], wppln_normal[2]);
	uj_putstr(list, buf);
	uj_putstr(list, " ");

	UM_cc_inttoext(cn_defn.center, center);
	um_mcstoccs(0, cn_defn.center, wpcenter);
	UM_cc_inttoext(wpcenter, wpcenter);

	switch (conicptr.type)
		{
		case UM_PARABOLA:
			{
			sprintf(buf,"parabola vertex  <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
				center[0], center[1], center[2],
				wpcenter[0], wpcenter[1], wpcenter[2]);
			uj_putstr(list, buf);
			uj_putstr(list, " ");

			um_mcstoccs(1, cn_defn.major_axis, wpmajor_axis);
			sprintf(buf,"principal axis   <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
				cn_defn.major_axis[0], cn_defn.major_axis[1], cn_defn.major_axis[2],
				wpmajor_axis[0], wpmajor_axis[1], wpmajor_axis[2]);
			uj_putstr(list, buf);

			UM_len_inttoext(cn_defn.major_length, major_length);
			sprintf(buf,"          length  %10.5f                      %10.5f", 
				major_length, major_length);
			uj_putstr(list, buf);
			uj_putstr(list, " ");
			}
			break;
		case UM_HYPERBOLA:
		case UM_ELLIPSE:
	  		{
			if (conicptr.type == UM_HYPERBOLA)
				
				sprintf(buf,"hyperbola center <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>",
					center[0], center[1], center[2],
					wpcenter[0], wpcenter[1], wpcenter[2]);
			else
				sprintf(buf,"ellipse center   <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
					center[0], center[1], center[2],
					wpcenter[0], wpcenter[1], wpcenter[2]);
			uj_putstr(list, buf);
			uj_putstr(list, " ");

			um_mcstoccs(1, cn_defn.major_axis, wpmajor_axis);
			sprintf(buf,"transverse axis  <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
				cn_defn.major_axis[0], cn_defn.major_axis[1], cn_defn.major_axis[2],
				wpmajor_axis[0], wpmajor_axis[1], wpmajor_axis[2]);
			uj_putstr(list, buf);

			UM_len_inttoext(cn_defn.major_length, major_length);
			sprintf(buf,"           length %10.5f                      %10.5f", 
				major_length, major_length);
			uj_putstr(list, buf);
			uj_putstr(list, " ");

			um_mcstoccs(1, cn_defn.minor_axis, wpminor_axis);
			sprintf(buf,"conjugate axis   <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
				cn_defn.minor_axis[0], cn_defn.minor_axis[1], cn_defn.minor_axis[2],
				wpminor_axis[0], wpminor_axis[1], wpminor_axis[2]);
			uj_putstr(list, buf);

			UM_len_inttoext(cn_defn.minor_length, minor_length);
			sprintf(buf,"          length  %10.5f                      %10.5f", 
				minor_length, minor_length);
			uj_putstr(list, buf);
			uj_putstr(list, " ");

			}
			break;
		default:
			break;
		}

	UJ_PUT0(list," ");
	UJ_PUT0(list,"Definition Space Equation:");
   UJ_PUT3(list,"  %10.5fXX + %10.5fXY + %10.5fYY +", cn_defn.cn[0], cn_defn.cn[1], cn_defn.cn[2]);
   UJ_PUT3(list," %10.5fX + %10.5fY + %10.5fF = 0.0", cn_defn.cn[3], cn_defn.cn[4], cn_defn.cn[5]);
	UJ_PUT0(list," ");
	UJ_PUT0(list," ");
	UJ_PUT0(list,"Definition Space to Model Space Transform -");
	for (ii=0; ii<4; ii++)
		UJ_PUT3(list,"          %10.5f  %10.5f  %10.5f", 
		conicptr.tfmat[ii][0], conicptr.tfmat[ii][1], conicptr.tfmat[ii][2]);

	goto done;

failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("um_conic_query", status);
	return(status);
	}	

/*********************************************************************
**    I_FUNCTION : int um_compcrv_query(key, list)
**			This function produces the query output for a composite curve.
**    PARAMETERS   
**       INPUT  : 
**          key	   key to a composite curve.
**       OUTPUT :  
**				list		list to contain the query informaiton.
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_compcrv_query(key, list)
	UU_KEY_ID	key;
	UU_LIST		*list;

	{
	UM_int2 idx, ival;
	struct UM_compcrv_rec  	compcrv;
	struct UM_attrdata_rec  attr;
	UM_transf tfmat, tfmat1, tfmat2;
	struct UM_evcrvout  crvout;
	struct UM_cid_rec	  *cptr;
	struct UC_entitydatabag	*dbag,tdbag;
	int i, rel_num;
	UM_coord pt,pt1;
	UU_REAL	dist, len, t0, t1;
	UU_REAL  um_getarclen();
	int   status = UU_SUCCESS;
	char buf[256];

	uu_denter(UU_MTRC,(us, "um_compcrv_query(eptr>key=%d)", key));

	dbag = (struct UC_entitydatabag *) uu_malloc(sizeof(struct UC_entitydatabag));

	/* get the entity data */
	compcrv.key = key;
	if (uc_retrieve_data(&compcrv, sizeof(struct UM_compcrv_rec)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
	if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS)	goto failed;

	ncl_closed_part(list, &compcrv.closdinu, NULL);

	UM_len_inttoext(compcrv.arclen, len);

	idx = 264;  
	getifl (&idx, &ival);
	t0 = compcrv.t0;
	t1 = compcrv.t1;
	uc_init_evcrvout(&compcrv, &crvout);
	uc_evcrv(UM_POINT,(UU_REAL) 0.0,&compcrv,tfmat,&crvout);    /* find the start point */
	UM_cc_inttoext(crvout.cp, pt);

	uc_evcrv(UM_POINT,(UU_REAL) 1.0,&compcrv,tfmat,&crvout);	  /* find the end point */
	UM_cc_inttoext(crvout.cp, pt1);

	UJ_PUT0(list," ");
	UJ_PUT0(list,"***The following information is displayed in Model Space");

	if (ival == 0)       /* inches */
	{
		UJ_PUT0(list,"***The current units are INCHES:");
	}
	else
	{
		UJ_PUT0(list,"***The current units are MILLIMETERS:");
	}
	UJ_PUT0(list," ");
	UJ_PUT0(list,"COMPOSITE CURVE:");
	sprintf(buf,"Total length %10.5f",len);
	UJ_PUT0(list,buf);
	if (t0 != 0. || t1 != 1.)
	{
		sprintf(buf, "Redefined from t0 = %6.4f to t1 = %6.4f",t0,t1);
		UJ_PUT0(list,buf);
	}
	if (compcrv.addflg != 0)
	{
		if (compcrv.addflg == 1)
			sprintf(buf, "First component added to extend curve.");
		else if (compcrv.addflg == 2)
			sprintf(buf, "Last component added to extend curve.");
		else
			sprintf(buf, "First and last components added to extend curve.");
		UJ_PUT0(list,buf);
	}
	sprintf(buf,"From:  <%10.5f,%10.5f,%10.5f>",pt[0],pt[1],pt[2]);
	UJ_PUT0(list,buf);
	sprintf(buf,"To:  <%10.5f,%10.5f,%10.5f>", pt1[0], pt1[1], pt1[2]);
	UJ_PUT0(list,buf);
	uj_putstr(list, " ");

	UJ_PUT0(list,"COMPONENTS:");
	UJ_PUT0(list,"Key  Type             Length               Start Point                                   End Point");

	i = 0;
	cptr = compcrv.cid;
	while (i < compcrv.no_cid)
	{
		if (cptr->endparam < t0) 
		{
			i++;
			cptr++;
			continue;
		}
		dbag->key = cptr->crvid;
		if (uc_retrieve_data(dbag, sizeof(struct UC_entitydatabag)) != UU_SUCCESS)
			goto failed;
		if (uc_retrieve_transf(dbag->key, tfmat1) != UU_SUCCESS)	goto failed;
		um_tftmtf(tfmat, tfmat1, tfmat2);
		um_c5_trimpart(dbag,compcrv.cid,compcrv.no_cid,t0,t1,i,tfmat1);
		tdbag.key = dbag->key;
		ncl_retrieve_data_fixed(&tdbag);
		/* Entity key and type */
		rel_num = tdbag.rel_num;
		if (tdbag.rel_num == NCL_CURVE_REL) rel_num = 6;
		dist = um_getarclen(&tdbag,tfmat2);
		UM_len_inttoext(dist, len);

		if (ival == 0)       /* inches */
			sprintf(buf,"%-4d %s %-10.5f",dbag->key,UJ_type[rel_num],len);
		else
			sprintf(buf,"%-4d %s %-10.4f",dbag->key,UJ_type[rel_num],len);
		uj_putstr(list, buf);

		uc_init_evcrvout(&tdbag, &crvout);
		uc_evcrv(UM_POINT, (UU_REAL) 0.0, &tdbag, tfmat2, &crvout);
		UM_cc_inttoext(crvout.cp, pt);
 
        if (ival == 0)       /* inches */
        {
            UJ_PUT3(list,"<%10.5f,%10.5f,%10.5f> ", pt[0], pt[1], pt[2]);
        }
        else
        {
            UJ_PUT3(list,"<%10.4f,%10.4f,%10.4f> ", pt[0], pt[1], pt[2]);
        }

		uc_evcrv(UM_POINT, (UU_REAL) 1.0, &tdbag, tfmat2, &crvout);
		UM_cc_inttoext(crvout.cp, pt);

        if (ival == 0)       /* inches */
        {
		    UJ_PUT3(list,"<%10.5f,%10.5f,%10.5f>", pt[0], pt[1], pt[2]);
        }
        else
        {
            UJ_PUT3(list,"<%10.4f,%10.4f,%10.4f>", pt[0], pt[1], pt[2]);
        }

		if (cptr->reverse == UU_TRUE)
		{
			UJ_PUT0(list, "  REVERSED");
		}
		else
		{
			UJ_PUT0(list, " ");
		}
		ur_update_data_fixed(dbag);
		if (cptr->endparam > t1) break;
		cptr++;
		i++;
	}

	goto done;
failed: status = UU_FAILURE;
done:;
	uu_free(dbag);
	uu_dexitstatus("um_compcrv_query", status);
	return(status);
	}

/*********************************************************************
**    I_FUNCTION : int um_group_query(key, list)
**			This function produces the query output for a group.
**    PARAMETERS   
**       INPUT  : 
**          key	   key to a group
**       OUTPUT :  
**				list		list to contain the query informaiton.
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_group_query(key, list)
	UU_KEY_ID	key;
	UU_LIST		*list;

	{
	struct UM_grouper_rec  	group;
	struct UM_attrdata_rec  attr;
	UM_transf tfmat;
	char buf[256];
	char relname[100];
	int	 i;
	int   status = UU_SUCCESS;
	UU_KEY_ID memkey;

	uu_denter(UU_MTRC,(us, "um_group_query(eptr>key=%d)", key));

	/* get the entity data */
	group.key = key;
	if (uc_retrieve_data(&group, sizeof(struct UM_grouper_rec)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
	if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS)	goto failed;

	sprintf(buf, " There are %d entities in the group", group.no_member);
	uj_putstr(list, buf);
	uj_putstr(list, " ");

	sprintf(buf, "   Key   Type");
	uj_putstr(list, buf);

	for (i=0; i<group.no_member; i++)
		{
		memkey = group.member[i];
		um_relation_name(memkey, relname);
		UJ_PUT2(list, "  %-4d %-20s ", memkey, relname);
		}

	goto done;

failed: status = UU_FAILURE;

done:;
	uu_dexitstatus("um_group_query", status);
	return(status);
	}

/*********************************************************************
**    I_FUNCTION : int um_vector_query(key, list)
**			This function produces the query output for a vector.
**    PARAMETERS   
**       INPUT  : 
**          lineptr	Pointer to the vector entity being queries.
**				attrptr	Pointer to the attribute bundle for the line.
**				data		Storage for query buffer.
**				maxRows	Number rows in the output array.
**				maxCols 	Maximum number of characters per row including the string 
**							terminator, "\0".	
**       OUTPUT :  
**				data			Filled query buffer.
**				rowsUsedptr	Pointer to the number of rows used in "data".
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_vector_query(key, list)
	UU_KEY_ID	key;
	UU_LIST	*list;

	{
	struct NCL_vector_rec 	veptr;
    UM_int2 idx, ival;
	struct UM_attrdata_rec 	attr;
	UM_transf tfmat;
	UU_REAL	len, extlen;
	UU_REAL	ang, intang;
	UM_coord	origin;
	UM_coord vec, wpvec;
	UM_vector	xaxis, yaxis, zaxis;
	char buf[256];
	int  status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us, "um_vector_query(key=%d)", key));

	/* get the entity data */
	veptr.key = key;
	if (uc_retrieve_data(&veptr, sizeof(struct NCL_vector_rec)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
	if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS)	goto failed;
	if (uc_transform(&veptr, tfmat, UU_FALSE) != UU_SUCCESS) goto failed;

	sprintf(buf, "                     Model Space                           Working Plane");
	uj_putstr(list, buf);
	uj_putstr(list, " ");

	UM_cc_inttoext(veptr.vec, vec);
	um_mcstoccs(0, veptr.vec, wpvec);
	UM_cc_inttoext(wpvec, wpvec);
    idx = 264;           /* inches or millimeters */
    getifl (&idx, &ival);
    if (ival == 0)       /* inches */
	    sprintf(buf,"vector  <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
		vec[0], vec[1], vec[2],
		wpvec[0], wpvec[1], wpvec[2]);
    else                 /* millimeters */
        sprintf(buf,"vector  <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
        vec[0], vec[1], vec[2],
        wpvec[0], wpvec[1], wpvec[2]);
	uj_putstr(list, buf);

	UJ_PUT0(list," ");
	UJ_PUT0(list," ");

	len = um_mag(veptr.vec);
	UM_len_inttoext(len, extlen);
    if (ival == 0)       /* inches */
        UJ_PUT1(list,"Length:                %10.5f", extlen)
    else
        UJ_PUT1(list,"Length:                %10.4f", extlen);

	um_getcpln(origin, xaxis, yaxis, zaxis);
	ang = um_angle(xaxis, veptr.vec);
	UM_ang_inttoext(ang, intang);
	  UJ_PUT1(list,"Angle to WP-xaxis:     %10.5f", intang);
	ang = um_angle(yaxis, veptr.vec);
	UM_ang_inttoext(ang, intang);
	  UJ_PUT1(list,"Angle to WP-yaxis:     %10.5f", intang);
	ang = um_angle(zaxis, veptr.vec);
	UM_ang_inttoext(ang, intang);
	  UJ_PUT1(list,"Angle to WP-zaxis:     %10.5f", intang);

	goto done;

failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("um_vector_query", status);
	return(status);
	}	

/*********************************************************************
**    I_FUNCTION : int um_pntvec_query(key, list)
**			This function produces the query output for a vector.
**    PARAMETERS   
**       INPUT  : 
**          lineptr	Pointer to the vector entity being queries.
**				attrptr	Pointer to the attribute bundle for the line.
**				data		Storage for query buffer.
**				maxRows	Number rows in the output array.
**				maxCols 	Maximum number of characters per row including the string 
**							terminator, "\0".	
**       OUTPUT :  
**				data			Filled query buffer.
**				rowsUsedptr	Pointer to the number of rows used in "data".
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_pntvec_query(key, list)
	UU_KEY_ID	key;
	UU_LIST	*list;

	{
	struct NCL_nclpv_rec 	veptr;
    UM_int2 idx, ival;
	struct UM_attrdata_rec 	attr;
	UM_transf tfmat;
	UU_REAL	len, extlen;
	UU_REAL	ang, intang;
	UM_coord	origin;
	UM_coord spt, wpspt, vec, wpvec;
	UM_vector	xaxis, yaxis, zaxis;
	char buf[256];
	int  status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us, "um_pntvec_query(key=%d)", key));

	/* get the entity data */
	veptr.key = key;
	if (uc_retrieve_data(&veptr, sizeof(struct NCL_nclpv_rec)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
	if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS)	goto failed;
	if (uc_transform(&veptr, tfmat, UU_FALSE) != UU_SUCCESS) goto failed;

	sprintf(buf, "                     Model Space                           Working Plane");
	uj_putstr(list, buf);
	uj_putstr(list, " ");

 idx = 264;           /* inches or millimeters */
 getifl (&idx, &ival);
	UM_cc_inttoext(veptr.pt, spt);
	um_mcstoccs(0, veptr.pt, wpspt);
	UM_cc_inttoext(wpspt, wpspt);
    if (ival == 0)       /* inches */
	    sprintf(buf,"start   <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
		spt[0], spt[1], spt[2],
		wpspt[0], wpspt[1], wpspt[2]);
    else                 /* millimeters */
        sprintf(buf,"start   <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
        spt[0], spt[1], spt[2],
        wpspt[0], wpspt[1], wpspt[2]);
	uj_putstr(list, buf);

	UM_cc_inttoext(veptr.ve, vec);
	um_mcstoccs(0, veptr.ve, wpvec);
	UM_cc_inttoext(wpvec, wpvec);
    if (ival == 0)       /* inches */
	    sprintf(buf,"vector  <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
		vec[0], vec[1], vec[2],
		wpvec[0], wpvec[1], wpvec[2]);
    else                 /* millimeters */
        sprintf(buf,"vector  <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
        vec[0], vec[1], vec[2],
        wpvec[0], wpvec[1], wpvec[2]);
	uj_putstr(list, buf);

	UJ_PUT0(list," ");
	UJ_PUT0(list," ");

	len = um_mag(veptr.ve);
	UM_len_inttoext(len, extlen);
    if (ival == 0)       /* inches */
    {
        UJ_PUT1(list,"Length:                %10.5f", extlen);
    }
    else
    {
        UJ_PUT1(list,"Length:                %10.4f", extlen);
    }

	um_getcpln(origin, xaxis, yaxis, zaxis);
	ang = um_angle(xaxis, veptr.ve);
	UM_ang_inttoext(ang, intang);
	  UJ_PUT1(list,"Angle to WP-xaxis:     %10.5f", intang);
	ang = um_angle(yaxis, veptr.ve);
	UM_ang_inttoext(ang, intang);
	  UJ_PUT1(list,"Angle to WP-yaxis:     %10.5f", intang);
	ang = um_angle(zaxis, veptr.ve);
	UM_ang_inttoext(ang, intang);
	  UJ_PUT1(list,"Angle to WP-zaxis:     %10.5f", intang);

	goto done;

failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("um_pntvec_query", status);
	return(status);
	}	

/*********************************************************************
**    I_FUNCTION : int um_plane_query(key, list)
**			This function produces the query output for a plane.
**    PARAMETERS   
**       INPUT  : 
**          lineptr	Pointer to the vector entity being queries.
**				attrptr	Pointer to the attribute bundle for the plane.
**				data		Storage for query buffer.
**				maxRows	Number rows in the output array.
**				maxCols 	Maximum number of characters per row including the string 
**							terminator, "\0".	
**       OUTPUT :  
**				data			Filled query buffer.
**				rowsUsedptr	Pointer to the number of rows used in "data".
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_plane_query(key, list)
	UU_KEY_ID	key;
	UU_LIST	*list;

	{
	struct NCL_nclpl_rec 	veptr;
    UM_int2 idx, ival;
	struct UM_attrdata_rec 	attr;
	UM_transf tfmat;
	UU_REAL	len, extlen, rr;
	UU_REAL	ang, intang;
	UM_coord	origin;
	UM_coord spt, wpspt, vec, wpvec;
	UM_vector	xaxis, yaxis, zaxis;
	char buf[256];
	int  status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us, "um_pntvec_query(key=%d)", key));

	/* get the entity data */
	veptr.key = key;
	if (uc_retrieve_data(&veptr, sizeof(struct NCL_nclpl_rec)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
	if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS)	goto failed;
	if (uc_transform(&veptr, tfmat, UU_FALSE) != UU_SUCCESS) goto failed;

	sprintf(buf, "                     Model Space                           Working Plane");
	uj_putstr(list, buf);
	uj_putstr(list, " ");

 idx = 264;           /* inches or millimeters */
 getifl (&idx, &ival);
 rr   = 25.4;
 if (ival == 0) rr = 1.0;
	UM_cc_inttoext(veptr.pt, spt);
	um_mcstoccs(0, veptr.pt, wpspt);
	UM_cc_inttoext(wpspt, wpspt);
    if (ival == 0)       /* inches */
	    sprintf(buf,"point   <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
		spt[0], spt[1], spt[2],
		wpspt[0], wpspt[1], wpspt[2]);
    else                 /* millimeters */
        sprintf(buf,"point   <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
        spt[0], spt[1], spt[2],
        wpspt[0], wpspt[1], wpspt[2]);
	uj_putstr(list, buf);

	UM_cc_inttoext(veptr.nvec, vec);
	um_mcstoccs(1, veptr.nvec, wpvec);
	UM_cc_inttoext(wpvec, wpvec);
	sprintf(buf,"normal  <%10.7f,%10.7f,%10.7f>  <%10.7f,%10.7f,%10.7f>", 
	     	vec[0]/rr, vec[1]/rr, vec[2]/rr, wpvec[0]/rr, wpvec[1]/rr, wpvec[2]/rr);
	uj_putstr(list, buf);

	UJ_PUT0(list," ");
	UJ_PUT0(list," ");

	len = um_dot(veptr.nvec,veptr.pt);
	UM_len_inttoext(len, extlen);
    if (ival == 0)       /* inches */
        UJ_PUT1(list,"Offset:                %10.5f", extlen)
    else
        UJ_PUT1(list,"Offset:                %10.4f", extlen);

	um_getcpln(origin, xaxis, yaxis, zaxis);
	ang = um_angle(xaxis, veptr.nvec);
	UM_ang_inttoext(ang, intang);
	  UJ_PUT1(list,"Angle to WP-xaxis:     %10.5f", intang);
	ang = um_angle(yaxis, veptr.nvec);
	UM_ang_inttoext(ang, intang);
	  UJ_PUT1(list,"Angle to WP-yaxis:     %10.5f", intang);
	ang = um_angle(zaxis, veptr.nvec);
	UM_ang_inttoext(ang, intang);
	  UJ_PUT1(list,"Angle to WP-zaxis:     %10.5f", intang);

	goto done;

failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("um_plane_query", status);
	return(status);
	}	

/*********************************************************************
**    I_FUNCTION : int um_matrix_query(key, list)
**			This function produces the query output for a matrix.
**    PARAMETERS   
**       INPUT  : 
**          lineptr	Pointer to the vector entity being queries.
**				attrptr	Pointer to the attribute bundle for the matrix
**				data		Storage for query buffer.
**				maxRows	Number rows in the output array.
**				maxCols 	Maximum number of characters per row including the string 
**							terminator, "\0".	
**       OUTPUT :  
**				data			Filled query buffer.
**				rowsUsedptr	Pointer to the number of rows used in "data".
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_matrix_query(key, list)
	UU_KEY_ID	key;
	UU_LIST	*list;

	{
	struct NCL_matrix_rec 	veptr;
    UM_int2 idx, ival;
	struct UM_attrdata_rec 	attr;
	UM_transf tfmat;
	UU_REAL	extlen;
	UU_REAL	ang, intang;
	UM_coord	origin, mxor, wpor;
	UM_coord org, wpmx[3];
	UM_vector	xaxis, yaxis, zaxis;
	UM_vector	mx[3];
	char buf[256], cnam[3];
	int  i, status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us, "um_matrix_query(key=%d)", key));

 cnam[0] = 'x';
 cnam[1] = 'y';
 cnam[2] = 'z';
	/* get the entity data */
	veptr.key = key;
	if (uc_retrieve_data(&veptr, sizeof(struct NCL_matrix_rec)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
	if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS)	goto failed;
/*	if (uc_transform(&veptr, tfmat, UU_FALSE) != UU_SUCCESS) goto failed;*/

	sprintf(buf,"                     Model Space                           Working Plane");
	uj_putstr(list, buf);
	uj_putstr(list, " ");
	sprintf(buf,"              x(1)       y(1)       z(1)             x(1)       y(1)       z(1)");
	uj_putstr(list, buf);
	uj_putstr(list, " ");

 idx = 264;           /* inches or millimeters */
 getifl (&idx, &ival);

 for (i=0; i<3; i++)
   {
    org[i] = veptr.mat[i][3];
/*   	UM_cc_inttoext(veptr.mat[i], mx[i]); */
    um_vctovc (veptr.mat[i], mx[i]);
   	um_mcstoccs(0, veptr.mat[i], wpmx[i]);
/*   	UM_cc_inttoext(wpmx[i], wpmx[i]); */
       if (ival == 0)       /* inches */
	       sprintf(buf,"%c(2)    <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
	            	cnam[i],mx[i][0], mx[i][1], mx[i][2],
	            	wpmx[i][0], wpmx[i][1], wpmx[i][2]);
    else                 /* millimeters */
        sprintf(buf,"%c(2)    <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
              cnam[i],mx[i][0], mx[i][1], mx[i][2],
              wpmx[i][0], wpmx[i][1], wpmx[i][2]);
   	uj_putstr(list, buf);
   }

 UM_cc_inttoext(org,mxor);
 um_mcstoccs(0, org, wpor);
 UM_cc_inttoext(wpor, wpor);
    if (ival == 0)       /* inches */
	       sprintf(buf,"tran(1) <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
	            	mxor[0], mxor[1], mxor[2], wpor[0], wpor[1], wpor[2]);
    else                 /* millimeters */
        sprintf(buf,"tran(1) <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
              mxor[0], mxor[1], mxor[2], wpor[0], wpor[1], wpor[2]);
 uj_putstr(list, buf);

	UJ_PUT0(list," ");
	UJ_PUT0(list," ");

	um_getcpln(origin, xaxis, yaxis, zaxis);
	ang = um_angle(xaxis, veptr.mat[0]);
	UM_ang_inttoext(ang, intang);
	  UJ_PUT1(list,"X-asix ang to WP-xaxis: %10.5f", intang);
	ang = um_angle(yaxis, veptr.mat[1]);
	UM_ang_inttoext(ang, intang);
	  UJ_PUT1(list,"Y-axis ang to WP-yaxis: %10.5f", intang);
	ang = um_angle(zaxis, veptr.mat[2]);
	UM_ang_inttoext(ang, intang);
	  UJ_PUT1(list,"Z-axis ang to WP-zaxis: %10.5f", intang);
		UJ_PUT0(list," ");

	UM_len_inttoext(veptr.dalen, extlen);
    if (ival == 0)       /* inches */
        UJ_PUT1(list,"Axis display length:    %10.5f", extlen)
    else
        UJ_PUT1(list,"Axis display length:    %10.4f", extlen);

 UM_cc_inttoext(veptr.dbox, wpor);
 if (ival == 0)       /* inches */
	   sprintf(buf,"dsplbox:<%10.5f,%10.5f,%10.5f>", 
	      	wpor[0], wpor[1], wpor[2]);
 else                 /* millimeters */
	   sprintf(buf,"dsplbox:<%10.4f,%10.4f,%10.4f>", 
        wpor[0], wpor[1], wpor[2]);
 uj_putstr(list, buf);

	goto done;

failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("um_matrix_query", status);
	return(status);
	}	

/*********************************************************************
**    I_FUNCTION : int um_patern_query(key, list)
**			This function produces the query output for a patern.
**    PARAMETERS   
**       INPUT  : 
**          lineptr	Pointer to the vector entity being queries.
**				attrptr	Pointer to the attribute bundle for the patern.
**				data		Storage for query buffer.
**				maxRows	Number rows in the output array.
**				maxCols 	Maximum number of characters per row including the string 
**							terminator, "\0".	
**       OUTPUT :  
**				data			Filled query buffer.
**				rowsUsedptr	Pointer to the number of rows used in "data".
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_patern_query(key, list)
	UU_KEY_ID	key;
	UU_LIST	*list;

	{
	struct NCL_patern_rec 	veptr;
    UM_int2 idx, ival;
	struct UM_attrdata_rec 	attr;
	UM_transf tfmat;
	UU_REAL	pntrec[6];
	UM_coord spt, wpspt, vec, wpvec;
	UM_vector	vecs;
	char buf[256], cnam[2][3];
	int  i, typ, np, m, status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us, "um_patern_query(key=%d)", key));

 strcpy (cnam[0],"pt");
 strcpy (cnam[1],"pv");
	/* get the entity data */
	veptr.key = key;
	if (uc_retrieve_data(&veptr, sizeof(struct NCL_patern_rec)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
	if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS)	goto failed;
	if (uc_transform(&veptr, tfmat, UU_FALSE) != UU_SUCCESS) goto failed;

 idx = 264;           /* inches or millimeters */
 getifl (&idx, &ival);

 typ = veptr.pntype;
 np  = veptr.no_patpnt;
 m   = typ * 3;
 np  = np/m;
 if (typ == 1)
   sprintf (buf,"Number of points: %5d", np);
 else 
   sprintf (buf,"Number of pointvectors: %5d", np);
 uj_putstr(list, buf);
 uj_putstr(list, " ");
 uj_putstr(list, " ");
	sprintf(buf, "                     Model Space                       Working Plane");
	uj_putstr(list, buf);
	uj_putstr(list, " ");
 for (i=1; i<=np; i++)
   {
    ur_retrieve_data_varlist(veptr.key, 1, pntrec, (i-1)*m+1, m);
   	UM_cc_inttoext(pntrec, spt);
   	um_mcstoccs(0, pntrec, wpspt);
	   UM_cc_inttoext(wpspt, wpspt);
    if (ival == 0)       /* inches */
	    sprintf(buf,"%s(%3d) <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
            	cnam[typ-1],i, spt[0], spt[1], spt[2], wpspt[0], wpspt[1], wpspt[2]);
    else                 /* millimeters */
        sprintf(buf,"%s(%3d) <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
             cnam[typ-1],i, spt[0], spt[1], spt[2], wpspt[0], wpspt[1], wpspt[2]);
   	uj_putstr(list, buf);
    if (typ == 2)
      {
       vecs[0] = pntrec[3];
       vecs[1] = pntrec[4];
       vecs[2] = pntrec[5];
      	UM_cc_inttoext(vecs, vec);
      	um_mcstoccs(0, vecs, wpvec);
      	UM_cc_inttoext(wpvec, wpvec);
       if (ival == 0)       /* inches */
      	   sprintf(buf,"        <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
	                	vec[0], vec[1], vec[2], wpvec[0], wpvec[1], wpvec[2]);
       else                 /* millimeters */
          sprintf(buf,"        <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
	                	vec[0], vec[1], vec[2], wpvec[0], wpvec[1], wpvec[2]);
      	uj_putstr(list, buf);
      }
   }

	goto done;

failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("um_patern_query", status);
	return(status);
	}	

/*********************************************************************
**    I_FUNCTION : int um_curve_query(key, list)
**			This function produces the query output for a patern.
**    PARAMETERS   
**       INPUT  : 
**          lineptr	Pointer to the vector entity being queries.
**				attrptr	Pointer to the attribute bundle for the patern.
**				data		Storage for query buffer.
**				maxRows	Number rows in the output array.
**				maxCols 	Maximum number of characters per row including the string 
**							terminator, "\0".	
**       OUTPUT :  
**				data			Filled query buffer.
**				rowsUsedptr	Pointer to the number of rows used in "data".
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_curve_query(key, list)
	UU_KEY_ID	key;
	UU_LIST	*list;

	{
	struct NCL_curve_rec 	cvptr;
	struct NCL_segment_rec 	*sgptr;
	UM_int2 idx, ival;
	struct UM_attrdata_rec 	attr;
/*	UM_transf tfmat;*/
	UU_REAL	rr, ro;
	UU_REAL	pt[3], dlt[3], du1,du0;
	UM_coord spt, wpspt, vec, wpvec;
	char buf[256];
	int  i, j, ns, m, status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us, "um_curve_query(key=%d)", key));

	/* get the entity data */
	cvptr.key = key;
	if (ncl_retrieve_data_fixed (&cvptr) != UU_SUCCESS) goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
/*	if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS)	goto failed;
	  if (uc_transform(&cvptr, tfmat, UU_FALSE) != UU_SUCCESS) goto failed; */

 ncl_closed_part(list, &cvptr.closdinu, NULL);

 idx = 264;           /* inches or millimeters */
 getifl (&idx, &ival);

 m     = cvptr.no_param;
 rr    = 0.;
 ns    = cvptr.no_segment;
 sgptr = (struct NCL_segment_rec *) cvptr.segment;   
 
 sprintf (buf, "Number of segments: %5d",ns); 
	uj_putstr(list, buf);
	uj_putstr(list, " ");

	sprintf(buf, "                     Model Space                              Working Plane");
	uj_putstr(list, buf);
	uj_putstr(list, " ");
 for (i=0; i<ns; i++)
   {
    for (j=0; j<3; j++)
      {
       pt[j] = sgptr->point[j];
       dlt[j] = sgptr->delta[j];
      }
   	UM_cc_inttoext(pt, spt);
   	um_mcstoccs(0, pt, wpspt);
	   UM_cc_inttoext(wpspt, wpspt);
    if (ival == 0)       /* inches */
	      sprintf(buf,"pt(%3d) <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
                   	i+1, spt[0], spt[1], spt[2], wpspt[0], wpspt[1], wpspt[2]);
    else                 /* millimeters */
       sprintf(buf,"pt(%3d) <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
                    i+1, spt[0], spt[1], spt[2], wpspt[0], wpspt[1], wpspt[2]);
   	uj_putstr(list, buf);

    UM_cc_inttoext(dlt, vec);
    um_mcstoccs(0, dlt, wpvec);
    UM_cc_inttoext(wpvec, wpvec);
    if (ival == 0)       /* inches */
    	  sprintf(buf,"delta   <%10.5f,%10.5f,%10.5f>  <%10.5f,%10.5f,%10.5f>", 
	                 	 vec[0], vec[1], vec[2], wpvec[0], wpvec[1], wpvec[2]);
    else                 /* millimeters */
       sprintf(buf,"delta   <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
	               	   vec[0], vec[1], vec[2], wpvec[0], wpvec[1], wpvec[2]);
    uj_putstr(list, buf);
    du1  = sgptr->duds1;
    du0  = sgptr->duds0;
    ro   = sgptr->rho;
    if (i > 0) rr   = cvptr.param[i-1];
    sprintf (buf, "du/ds(1):  %8.5f   du/ds(0):  %8.5f",du1,du0);
    uj_putstr(list, buf);
    sprintf (buf, "rho:     %10.5f      param:   %8.6f",ro,rr);
    uj_putstr(list, buf);
    uj_putstr(list, " ");
    sgptr++;
   }

	if (cvptr.t0 > 0. || cvptr.t1 < 1.)
	{
		uj_putstr(list," ");
		sprintf (buf, "Redefined from t0 = %6.4f to t1 = %6.4f",cvptr.t0,cvptr.t1); 
		uj_putstr(list, buf);
		uj_putstr(list," ");
	}
	goto done;

failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("um_curve_query", status);
	return(status);
	}	

/*********************************************************************
**    I_FUNCTION : int um_surf_query(key, list)
**			This function produces the query output for a surface.
**    PARAMETERS   
**       INPUT  : 
**          key	   key to an NCL surface.
**       OUTPUT :  
**			list   list to contain the query informaiton.
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_surf_query(key, list)
UU_KEY_ID	key;
UU_LIST	*list;
{
	struct NCL_surface_rec surf;
	struct NCL_panel_rec panel;
	struct UM_surfattr_rec attr;
	int np, ptype, status = UU_SUCCESS;
	char buf[256];

	uu_denter(UU_MTRC,(us, "um_surf_query(key=%d)", key));

	surf.key = key;
	if (uc_retrieve_data(&surf, sizeof(struct NCL_surface_rec)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 

	ncl_closed_part(list, &surf.closdinu, &surf.closdinv);
	ncl_params_part(list, surf.swapuv, surf.rev_normal);

	sprintf(buf,"- Curves on surface = %d", surf.no_sskey);
	uj_putstr(list, buf);
	uj_putstr(list, " ");

	np   = surf.no_panelkey;
	panel.key = surf.panelkey[0];
	if (ncl_retrieve_data_fixed (&panel) != UU_SUCCESS) goto failed;
	ptype = panel.type;   

	if (ptype == 0)
	{
		if (np < 2)
			sprintf (buf, "NCL Surface,   1 panel.");
		else
			sprintf (buf, "NCL Surface,   %5d panels.",np);
	}
	else
	{
		if (np < 2)
			sprintf (buf, "NCL Ruled Surface,   1 panel.");
		else
			sprintf (buf, "NCL Ruled Surface,   %5d panels.",np);
	}
	uj_putstr(list, buf);
	uj_putstr(list, " ");
/*
.....Primitive data
*/
	um_primitive_query(surf.primitive,surf.prim_param,list);

	goto done;

failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("um_surf_query", status);
	return(status);
}	

/*********************************************************************
**    I_FUNCTION : int um_revsurf_query(key, list)
**			This function produces the query output for a surface of revolution.
**    PARAMETERS   
**       INPUT  : 
**          key   -  unibase key of the queried surface
**       OUTPUT :  
**			list  -  list to contain the query informaiton.
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_revsurf_query(key, list)
UU_KEY_ID	key;
UU_LIST	*list;
{
	struct NCL_revsurf_rec surf;
	struct UM_surfattr_rec attr;
	int status = UU_SUCCESS;
	char buf[256];
	UM_coord pta;
	UM_vector vca;
	UM_int2 idx, ival;

	uu_denter(UU_MTRC,(us, "um_revsurf_query(key=%d)", key));

	surf.key = key;
	if (uc_retrieve_data(&surf, sizeof(struct NCL_revsurf_rec)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 

	ncl_closed_part(list, &surf.closdinu, &surf.closdinv);
	ncl_params_part(list, surf.swapuv, surf.rev_normal);

	sprintf(buf,"- Curves on surface = %d", surf.no_sskey);
	uj_putstr(list, buf);
	uj_putstr(list, " ");

	sprintf (buf, "NCL Surface of Revolution.");
	uj_putstr(list, buf);
	uj_putstr(list, " ");

	sprintf(buf,"   Generating Curve: key = %-4d",surf.cvkey);
	uj_putstr(list, buf);
	uj_putstr(list, " ");

	sprintf(buf,"   Start Angle:  %8.3f degrees;      End Angle:  %8.3f degrees", 
				surf.sa,surf.ta);
	uj_putstr(list, buf);
	uj_putstr(list, " ");

/*
	UJ_PUT0(list,"Axis of Rotation  (the coordinates are in Model Space, ");
	idx = 264;
*/
	getifl (&idx, &ival);

	UM_cc_inttoext(surf.pta, pta);
	UM_cc_inttoext(surf.vca, vca);
	if (ival == 0)       /* inches */
	{
		uj_putstr(list,"Axis of Rotation  (Model Space - Units:Inches):");
		sprintf(buf,"   Point = <%10.5f,%10.5f,%10.5f>,   Vector = <%10.5f,%10.5f,%10.5f>",
					pta[0], pta[1], pta[2], vca[0], vca[1], vca[2]);
	}
	else                 /* millimeters */
	{
		uj_putstr(list,"Axis of Rotation  (Model Space - Units:Millimeters):");
		sprintf(buf,"   Point = <%10.4f,%10.4f,%10.4f>,   Vector = <%10.4f,%10.4f,%10.4f>",
					pta[0], pta[1], pta[2], vca[0], vca[1], vca[2]);
	}
	uj_putstr(list, buf);
	uj_putstr(list, " ");
/*
.....Primitive data
*/
	um_primitive_query(surf.primitive,surf.prim_param,list);
	goto done;

failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("um_revsurf_query", status);
	return(status);
}	

/*********************************************************************
**    I_FUNCTION : int um_netsf_query(key, list)
**			This function produces the query output for a net surface.
**    PARAMETERS   
**       INPUT  : 
**          lineptr	Pointer to the net surface entity being queries.
**				attrptr	Pointer to the attribute bundle for the net surface.
**				data		Storage for query buffer.
**				maxRows	Number rows in the output array.
**				maxCols 	Maximum number of characters per row including the string 
**							terminator, "\0".	
**       OUTPUT :  
**				data			Filled query buffer.
**				rowsUsedptr	Pointer to the number of rows used in "data".
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_netsf_query(key, list)
	UU_KEY_ID	key;
	UU_LIST	*list;

	{
	struct NCL_netsf_rec surf;
	struct UM_surfattr_rec attr;
	int ns, status = UU_SUCCESS;
	 char buf[256];

	 uu_denter(UU_MTRC,(us, "um_netsf_query(key=%d)", key));

  surf.key = key;
	 if (uc_retrieve_data(&surf, sizeof(struct NCL_netsf_rec)) != UU_SUCCESS)
		   goto failed;
	 if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 

   sprintf(buf,"- Curves on surface = %d", surf.no_sskey);
   uj_putstr(list, buf);
   uj_putstr(list, " ");

  ns   = surf.no_netkey;
  
  sprintf (buf, "Net Surface,   %5d surfaces.",ns);
	 uj_putstr(list, buf);
   uj_putstr(list, " ");

 	goto done;

failed: status = UU_FAILURE;
done:;
	 uu_dexitstatus("um_netsf_query", status);
	 return(status);
	 }	
/*********************************************************************
**    I_FUNCTION : int um_meshsf_query(key, list)
**			This function produces the query output for a mesh surface.
**    PARAMETERS   
**       INPUT  : 
**          lineptr	Pointer to the mesh surface entity being queries.
**				attrptr	Pointer to the attribute bundle for the mesh surface.
**				data		Storage for query buffer.
**				maxRows	Number rows in the output array.
**				maxCols 	Maximum number of characters per row including the string 
**							terminator, "\0".	
**       OUTPUT :  
**				data			Filled query buffer.
**				rowsUsedptr	Pointer to the number of rows used in "data".
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_meshsf_query(key, list)
	UU_KEY_ID	key;
	UU_LIST	*list;

	{
	struct NCL_meshsf_rec surf;
	struct UM_surfattr_rec attr;
	int ns, status = UU_SUCCESS;
	 char buf[256];

	 uu_denter(UU_MTRC,(us, "um_meshsf_query(key=%d)", key));

  surf.key = key;
	 if (uc_retrieve_data(&surf, sizeof(struct NCL_meshsf_rec)) != UU_SUCCESS)
		   goto failed;
	 if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 

  ncl_closed_part(list, &surf.closdinu, &surf.closdinv);
	ncl_params_part(list, surf.swapuv, surf.rev_normal);

  ns   = surf.no_mpatch;
  sprintf (buf, "Mesh Surface,   %5d patches.",ns);
	 uj_putstr(list, buf);
	 uj_putstr(list, " ");

 	goto done;

failed: status = UU_FAILURE;
done:;
	 uu_dexitstatus("um_meshsf_query", status);
	 return(status);
	 }	
/*********************************************************************
**    I_FUNCTION : int um_shape_query(key, list)
**			This function produces the query output for a shape.
**    PARAMETERS   
**       INPUT  : 
**          lineptr	Pointer to the shape entity being queries.
**				attrptr	Pointer to the attribute bundle for the shape.
**				data		Storage for query buffer.
**				maxRows	Number rows in the output array.
**				maxCols 	Maximum number of characters per row including the string 
**							terminator, "\0".	
**       OUTPUT :  
**				data			Filled query buffer.
**				rowsUsedptr	Pointer to the number of rows used in "data".
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_shape_query(key, list)
	UU_KEY_ID	key;
	UU_LIST	*list;

	{
  struct NCL_shape_rec shape;
	 struct UM_attrdata_rec 	attr;
	int status = UU_SUCCESS;

	 uu_denter(UU_MTRC,(us, "um_shape_query(key=%d)", key));

  shape.key = key;
	 if (uc_retrieve_data(&shape, sizeof(struct NCL_netsf_rec)) != UU_SUCCESS)
		   goto failed;
	 if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 

 	goto done;

failed: status = UU_FAILURE;
done:;
	 uu_dexitstatus("um_netsf_query", status);
	 return(status);
	 }	

/*********************************************************************
**    I_FUNCTION : int um_primitive_query(primtyp,primdat,list)
**			This function produces the query output for a surface primitive.
**    PARAMETERS   
**       INPUT  : 
**				primtyp = Primitive type.
**				primdat = Primitive data.
**				list    = Query List.
**       OUTPUT :  
**				list    = Query List.
**    RETURNS: UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_primitive_query(primtyp,primdat,list)
enum nclsf_prim_type primtyp;
UU_REAL primdat[16];
UU_LIST *list;
{
	UU_REAL rval[4],*rpt,ang;
	char buf[256];
	switch (primtyp)
	{
/*
.....Unknown
*/
		case NCLSF_UNKNOWN:
   		sprintf(buf,"Primitive Type:    Unknown ");
			break;
/*
.....Freeform
*/
		case NCLSF_FREEFORM: 
   		sprintf(buf,"Primitive Type:    Freeform ");
			break;
/*
.....Ruled
*/
		case NCLSF_RULED:
			if (primdat[0] == 1)
   			sprintf(buf,"Primitive Type:    Ruled in u ");
			else
   			sprintf(buf,"Primitive Type:    Ruled in v ");
			break;
/*
.....Plane
*/
		case NCLSF_PLANE:
   		sprintf(buf,"Primitive Type:    Plane ");
			uj_putstr(list, buf);
			rpt = &primdat[4];
			UM_cc_inttoext(rpt,rval);
			sprintf(buf," point = <%11.4f,%11.4f,%11.4f>,",
				rval[0],rval[1],rval[2]);
			uj_putstr(list, buf);
			UM_len_inttoext(primdat[3],rval[0]);
			sprintf(buf," normal = <%11.4f,%11.4f,%11.4f>,    dis   = %10.4f",
				primdat[0],primdat[1],primdat[2],rval[0]);
			break;
/*
.....Sphere
*/
		case NCLSF_SPHERE:
			sprintf(buf,"Primitive Type:    Sphere ");
			uj_putstr(list, buf);
			rpt = &primdat[0];
			UM_cc_inttoext(rpt,rval);
			UM_len_inttoext(primdat[3],rval[3]);
			sprintf(buf," center = <%11.4f,%11.4f,%11.4f>,   radius = %10.4f",
				rval[0],rval[1],rval[2],rval[3]);
			break;
/*
.....Cylinder
*/
		case NCLSF_CYLINDER:
			sprintf(buf,"Primitive Type:    Cylinder ");
			uj_putstr(list, buf);
			rpt = &primdat[0];
			UM_cc_inttoext(rpt,rval);
			UM_len_inttoext(primdat[6],rval[3]);
			sprintf(buf," center = <%11.4f,%11.4f,%11.4f>,   radius = %10.4f,",
			rval[0],rval[1],rval[2],rval[3]);
			uj_putstr(list, buf);
			UM_len_inttoext(primdat[7],rval[0]);
			sprintf(buf," axis    = <%11.4f,%11.4f,%11.4f>,   height = %10.4f",
				primdat[3],primdat[4],primdat[5],rval[0]);
			break;
/*
.....Cone
*/
		case NCLSF_CONE:
			sprintf(buf,"Primitive Type:    Cone ");
			uj_putstr(list, buf);
			ang = primdat[6] * UM_RADIAN;
			rpt = &primdat[0];
			UM_cc_inttoext(rpt,rval);
			sprintf(buf,
				" apex   = <%11.4f,%11.4f,%11.4f>,   angle  = %10.4f degrees",
				rval[0],rval[1],rval[2],ang);
			uj_putstr(list, buf);
			UM_len_inttoext(primdat[7],rval[0]);
			sprintf(buf," axis    = <%11.4f,%11.4f,%11.4f>,   height = %10.4f",
				primdat[3],primdat[4],primdat[5],rval[0]);
			uj_putstr(list, buf);
			UM_len_inttoext(primdat[8],rval[0]);
			sprintf(buf," distance from apex to top of cone = %10.4f",rval[0]);
			break;
/*
.....Torus
*/
		case NCLSF_TORUS:
			sprintf(buf,"Primitive Type:    Torus ");
			uj_putstr(list, buf);
			rpt = &primdat[0];
			UM_cc_inttoext(rpt,rval);
			UM_len_inttoext(primdat[6],rval[3]);
			sprintf(buf," center = <%11.4f,%11.4f,%11.4f>,   major radius = %10.4f",
				rval[0],rval[1],rval[2],rval[3]);
			uj_putstr(list, buf);
			UM_len_inttoext(primdat[7],rval[0]);
			sprintf(buf," axis    = <%11.4f,%11.4f,%11.4f>,   minor radius = %10.4f",
				primdat[3],primdat[4],primdat[5],rval[0]);
			break;
/*
.....Not set
*/
		default:
			goto done;
			break;
	}
	uj_putstr(list, buf);
	uj_putstr(list, " ");
done:;
	return(UU_SUCCESS);
}
