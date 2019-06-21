
/*********************************************************************
**    NAME         :  m2debmdl.c
**       CONTAINS: debugging routines to print various data
**			um_p_drwmdl(title)
**			um_dump_unibase()
**			um_print_attribute(attr)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m2debmdl.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:45
*********************************************************************/
#include "usysdef.h"
#include "ustdio.h"
#include "udebug.h"
#include "dasnog.h"
#include "uhep.h"
#include "class.h"
#include "mdcoord.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdattr.h"
#include "mattr.h"
#include "mxxx.h"
#include "mdraw.h"
#include "mdebug.h"

/*********************************************************************
**    E_FUNCTION     : um_p_drwmdl(title)
**       Print the current drawing modals.
**    PARAMETERS   
**       INPUT  : 
**          title						title to print
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_p_drwmdl(title)
	char title[];

	{
	UU_KEY_ID drwsn;
	UU_KEY_ID drwvp;
	UU_KEY_ID drwvw;
	UU_KEY_ID curdrw;
	char	modsname[16];
	UU_REAL	aspect;
	char	unitsstr[100];
	int	drwsize;
	UU_REAL	drwscale;
	int	drwunits;
	UU_REAL	modscale;
	int	modunits;
	UU_REAL	plotprec;

	uu_denter(UU_MTRC,(us,"um_p_drwmdl()"));

	um_pscroll(title);

	drwsn = ur_get_drwmdl_drwsn();
	drwvp = ur_get_drwmdl_drwvp();
	drwvw = ur_get_drwmdl_drwvw();
	sprintf(UM_sbuf,"screen=%x, vport=%x, view=%x", drwsn, drwvp, drwvw);
	um_pscroll(UM_sbuf);

	curdrw = ur_get_drwmdl_curdrw();
	sprintf(UM_sbuf,"current drawing=%x", curdrw);
	um_pscroll(UM_sbuf);

	drwsize = ur_get_drwmdl_drwsize();
	drwscale = ur_get_drwmdl_drwscale();
	drwunits = ur_get_drwmdl_drwunits();
	sprintf(UM_sbuf,"drwsize=%d, drwscale=%f, drwunits=%d", drwsize, 
		drwscale, drwunits);
	um_pscroll(UM_sbuf);

	modscale = ur_get_drwmdl_modscale();
	modunits = ur_get_drwmdl_modunits();
	plotprec = ur_get_drwmdl_plotprec();
	sprintf(UM_sbuf,"modscale=%f, modunits=%d, plotprec=%f", modscale, 
		modunits, plotprec);
	um_pscroll(UM_sbuf);

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : um_dump_unibase()
**       Print the data, the system attributes, and the transformation
**			for each master tuple entity in UNIBASE.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_dump_unibase()

	{
	UU_KEY_ID key;
	int status;
	int next_tupleid;
	int master_tuple_relation;
	struct UC_entitydatabag eptr;
	struct UC_attributedatabag attr;
	UM_transf tfmat;
	UU_KEY_ID viewkey;

	uu_denter(UU_MTRC,(us,"um_dump_unibase()"));

	status = 0;
	next_tupleid = 1;
	master_tuple_relation = 0;

repeat:
	status = ur_get_next_key(&next_tupleid, &key);
	if (status >= 0)
		{
		sprintf(UM_sbuf,"MASTER TUPLE %x", next_tupleid);
		um_pscroll(UM_sbuf);
		if (status > 0)
			{
			sprintf(UM_sbuf,".....WARNING status %x",status);
			um_pscroll(UM_sbuf);
			}
		else
			{
			eptr.key = key;
			if (uc_retrieve_data(&eptr, sizeof(eptr)) == UU_SUCCESS)
				{
				if (uc_print(&eptr) != UU_SUCCESS)
					{
					sprintf(UM_sbuf,".....FAILURE uc_print(key=%d, rel=%d)",
						eptr.key, eptr.rel_num);
					um_pscroll(UM_sbuf);
					}
				}
			else
				{
				sprintf(UM_sbuf,".....FAILURE uc_retrieve_data(key=%d)",key);
				um_pscroll(UM_sbuf);
				}
			if (ur_retrieve_view_key(key, &viewkey) != 0)
				{
				sprintf(UM_sbuf,".....FAILURE ur_retrieve_view_key(key=%d)",key);
				um_pscroll(UM_sbuf);
				}
			else
				{
				sprintf(UM_sbuf,"viewkey = %x",viewkey);
				um_pscroll(UM_sbuf);
				}
			if (uc_retrieve_attr(key, &attr) == UU_SUCCESS)
				um_print_attribute(&attr);
			else
				{
				sprintf(UM_sbuf,".....FAILURE uc_retrieve_attr(key=%d)",key);
				um_pscroll(UM_sbuf);
				}
			if (uc_retrieve_transf(key, tfmat) == UU_SUCCESS)
				umi_print_transformation(tfmat);
			else
				{
				sprintf(UM_sbuf,".....FAILURE uc_retrieve_transf(key=%d)",key);
				um_pscroll(UM_sbuf);
				}
			}
		next_tupleid++;
		goto repeat;
		}
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : um_print_attribute(attr)
**       Print the system attribute bundle.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_print_attribute(attr)
	struct UM_attrdata_rec *attr;

	{
	uu_denter(UU_MTRC,(us,"um_print_attribute(attr=%x)",attr));

	um_pscroll("ATTRIBUTE BUNDLE:");
	um_p_ary(UM_PINT, "color", 1, &attr->color);
	um_p_ary(UM_PINT, "layer", 1, &attr->layer);
	um_p_ary(UM_PINT, "pen", 1, &attr->pen);
	um_p_ary(UM_PINT, "line_style", 1, &attr->line_style);
	um_p_ary(UM_PFLOAT, "line_width", 1, &attr->line_width);
	um_p_ary(UM_PFLOAT, "line_weight", 1, &attr->line_weight);
	switch (attr->displayable)
		{
		case UM_DISPLAYABLE:
			sprintf(UM_sbuf,"displayable");
			break;
		case UM_UNDISPLAYABLE:
			sprintf(UM_sbuf,"not displayable");
			break;
		case UM_NEVERDISPLAYABLE:
			sprintf(UM_sbuf,"never displayable");
			break;
		default:
			sprintf(UM_sbuf,"illegal displayable value");
			break;
		}
	um_pscroll(UM_sbuf);
	switch (attr->selectable)
		{
		case UU_TRUE:
			sprintf(UM_sbuf,"selectable");
			break;
		case UU_FALSE:
			sprintf(UM_sbuf,"not selectable");
			break;
		default:
			sprintf(UM_sbuf,"illegal selectable value");
			break;
		}
	um_pscroll(UM_sbuf);
	uu_dexit;
	}


