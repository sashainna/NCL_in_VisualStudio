/*********************************************************************
**    NAME         :  nedisplst.c
**       CONTAINS: Routines to create, delete & display a display list.
**
**           int ncl_displst
**           int ncl_displst_finish
**           int ncl_displst_init
**           int ncl_displst_display
**           int ncl_displst_display1
**           UU_LOGICAL ncl_displst_OK
**           int ncl_bndr_display
**           int ncl_bndr_display1
**           int ncl_displst_proj_to_drawing
**           int ncl_displst_xform
**           int ncl_displst_delete
**           int ncl_displst_setckey
**           int ncl_displst_getckey
**           int nuldsp 
**
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nedisplst.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:31
*********************************************************************/
#include "usysdef.h"
#include "class.h"
#include "mcrv.h"
#include "mdattr.h"
#include "mfort.h"
#include "mdrel.h"
#include "nccs.h"
#include "nclfc.h"
#include "mdcoord.h"
#include "mgeom.h"
#include "mattr.h"

extern UU_LOGICAL UM_set_constituent_pickids;

/*
..... The display list consists of one word representing the number of
..... polylines in the list and is followed by the display polylines.
..... The format of the display polylines is either:
.....   a) the number of points in the polyline followed by the points or
.....   b) the negative of the number of points in the polyline followed
.....      by the key of the entity the points were created from followed
.....      by the points.
*/

static UU_KEY_ID NCL_skey, NCL_ckey = 0;
static int NCL_lix, NCL_lstix, NCL_nlist;

/*********************************************************************
**    E_FUNCTION     : int ncl_displst (num, pts)
**       Save display points in a surface display list.
**    PARAMETERS   
**       INPUT  : 
**          num        - number of points to save
**          pts        - points to save.
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_displst (num, pts)
int num;
UU_REAL *pts;
{
	int status;
	UM_coord xn;
/*
.....vp 2/15/98 pack control data in first point tuple
.....all data is organized in points according to unibase
.....definition of displst field.
*/
	xn[0] = num;
	xn[1] = 0;
	xn[2] = 0;
/*
..... If NCL_ckey is set, negate the number of points as a flag and
..... save the key as the first item in the list.
*/
	if (NCL_ckey)
	{
		xn[1] = NCL_ckey;
		NCL_ckey = 0;
	}
	status = ur_update_data_varlist(NCL_skey, NCL_lstix, xn, NCL_lix, 1);
	NCL_lix++;
	status = ur_update_data_varlist(NCL_skey, NCL_lstix, pts, NCL_lix, num);
	NCL_lix += num;
	NCL_nlist++;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_displst_finish (told)
**       Set first word of display list to number of display lists.
**       Turn off display list variables.
**    PARAMETERS   
**       INPUT  : 
**          told - current display tolerance
**          bflg - 1 = Boundary list only is stored (shaded surfaces)
**                 0 = Entire surface display list is stored
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_displst_finish (told,bflg)
UU_REAL told;
int bflg;
{
	int status;
	UM_coord xn;

	xn[0] = NCL_nlist;
	xn[1] = told;
	xn[2] = bflg;
	status = ur_update_data_varlist(NCL_skey, NCL_lstix, xn, 1, 1);
	gsetdsplst(0);
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_displst_init (listix, key)
**       Initialize variables needed to save display points in a
**       surface display list.
**    PARAMETERS   
**       INPUT  : 
**          listix     - Index of display list for this surface type.
**          key        - Key of surface.
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_displst_init (listix, key)
int listix;
UU_KEY_ID key;
{
	NCL_nlist = 0;
	NCL_lix   = 2;
	NCL_lstix = listix;
	NCL_skey  = key;
	gsetdsplst(1);
	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_displst_display (lstp,tfmat)
**       Display a display list.
**    PARAMETERS   
**       INPUT  : 
**          lstp       - list to display
**          tfmat      - matrix to move the display thru
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_displst_display (lstp,tfmat)
UM_coord *lstp;
UM_transf tfmat;
{
	return (ncl_displst_display1(lstp,tfmat,NULLKEY,UU_NULL,0));
}

/*********************************************************************
**    E_FUNCTION     : int ncl_displst_display1(lstp,tfmat,sfkey,attrptr,color0)
**       Display a display list. The part marked with a surface key is displayed
**       in original surface color. The other part (the boundary edges) is
**       displayed using the current color in attrptr.
**    PARAMETERS   
**       INPUT  : 
**          lstp       - list to display
**          tfmat      - matrix to move the display thru
**          sfkey      - key of surface
**          attrptr    - surface display attribute
**          color0     - original surface color
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_displst_display1(lstp,tfmat,sfkey,attrptr,color0)
UM_coord *lstp;
UM_transf tfmat;
struct UM_attrdata_rec *attrptr;
int color0;
UU_KEY_ID sfkey;
{
	int i, nlists, n,j;
	UU_KEY_ID key;
	UM_coord *pline;
	UU_LOGICAL do_transform,changecolor;
/*
... aak 15-apr-1998: added transformation of the display 
... thru a matrix (is used to project a surface onto a drawing).
*/
	do_transform =  !um_is_idmat (tfmat); 
	changecolor = UU_FALSE;
	if (attrptr != UU_NULL)
	{
		if (attrptr->color != color0) changecolor = UU_TRUE;
	}

	nlists = lstp[0][0];
	lstp++;

	for (i=0;i<nlists;i++)
	{
/*
.....vp 2/14/98 control data is in first point tuple
.....need unpack this numbers
*/
		n = lstp[0][0];
		key = NULLKEY;
		if (lstp[0][1] > 0)
			key = lstp[0][1];
/*
..... If the number of points is negative, set the pick id from the key
..... that follows.
*/
		if ((key == NULLKEY || key == sfkey) && changecolor)
		{
			attrptr->color = color0;
			um_set_disp_attr (attrptr);
		}

		if (key != NULLKEY && UM_set_constituent_pickids) gspickid (key);
		lstp++;

		if (do_transform)
		{
			pline = (UM_coord *) uu_malloc (n*sizeof(UM_coord));
			for (j=0;j<n;j++)  um_cctmtf (lstp[j],tfmat,pline[j]);
		}
		else
			pline = lstp;

		if (n > 1) gpolyline3(n, pline);
		if (do_transform) uu_free (pline);

		lstp += n;
	}

	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_bndr_display (lstp,tfmat,sfkey)
**       Display the boundary part of display list.
**    PARAMETERS   
**       INPUT  : 
**          lstp       - display list 
**          tfmat      - matrix to move the display through
**          sfkey      - surface key
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_bndr_display (lstp,tfmat,sfkey)
UM_coord *lstp;
UM_transf tfmat;
UU_KEY_ID sfkey;
{
	int i, nlists, n,j;
	UU_KEY_ID key;
	UM_coord *pline;
	UU_LOGICAL do_transform;
/*
... aak 15-apr-1998: added transformation of the display 
... thru a matrix (is used to project a surface onto a drawing).
*/
	do_transform =  !um_is_idmat (tfmat); 

	nlists = lstp[0][0];
	lstp++;

	for (i=0;i<nlists;i++)
	{
/*
.....vp 2/14/98 control data is in first point tuple
.....need unpack this numbers
*/
		n = lstp[0][0];
		key = lstp[0][1];
/*
..... If the number of points is negative, set the pick id from the key
..... that follows.
*/
		if (key <= 0 || key == sfkey) break;
		key += sfkey;

		if (UM_set_constituent_pickids) gspickid (key);
		lstp++;

		if (do_transform)
		{
			pline = (UM_coord *) uu_malloc (n*sizeof(UM_coord));
			for (j=0;j<n;j++)  um_cctmtf (lstp[j],tfmat,pline[j]);
		}
		else
			pline = lstp;

		gpolyline3(n, pline);
		if (do_transform) uu_free (pline);

		lstp += n;
	}

	key = sfkey;
	if (UM_set_constituent_pickids) gspickid (key);

	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_bndr_display1 (lstp,tfmat)
**       Display the boundary part of display list for untrimmed surface.
**    PARAMETERS   
**       INPUT  : 
**          lstp       - display list 
**          tfmat      - matrix to move the display through
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_bndr_display1 (lstp,tfmat)
UM_coord *lstp;
UM_transf tfmat;
{
	int i, nlists, n,j;
	UU_KEY_ID key;
	UM_coord *pline;
	UU_LOGICAL do_transform;
/*
... aak 15-apr-1998: added transformation of the display 
... thru a matrix (is used to project a surface onto a drawing).
*/
	do_transform = !um_is_idmat (tfmat); 

	nlists = lstp[0][0];
	lstp++;

	for (i = 0; i < nlists; i++)
	{
		n = lstp[0][0];
		key = lstp[0][1];
		if (UM_set_constituent_pickids) gspickid (key);
		lstp++;
/*
..... display only boundary segments
*/
		if (key > 0 && key < 5)
		{
			if (do_transform)
			{
				pline = (UM_coord *) uu_malloc (n*sizeof(UM_coord));
				for (j=0;j<n;j++) um_cctmtf (lstp[j],tfmat,pline[j]);
			}
			else
				pline = lstp;

			gpolyline3(n, pline);
			if (do_transform) uu_free (pline);
		}

		lstp += n;
	}

	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_displst_proj_to_drawing(eptr,pts,attr,drwmat,
**                         vrefpt,vpnorm,keylist)
**       Create a list of keys (KEYLIST) of entities to be included
**       in a drawing for the given display list.
**       The transformation (DRWMAT) will position the viewplane
**       (VREFPT, VPNORM) on the XY plane of the drawing appropriately 
**       scaled to map the MCS to the DCS.
**    PARAMETERS   
**       INPUT  : 
**          eptr     entity structure containing label of entity
**          pts      display list
**          attr     pointer to attribute bundle 
**          drwmt    transformation to convert MCS to DCS
**          vrefpt   reference point of viewing plane
**          vpnorm   normal of viewing plane
**       OUTPUT :  
**          keylist  keys of projected entities are pushed onto this list
**    RETURNS      : 
**       UU_SUCCESS if no error; else UU_FAILURE 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_displst_proj_to_drawing(eptr,pts,attr,drwmat,vrefpt,vpnorm,keylist)
struct UC_entitydatabag *eptr;
UM_coord pts[];
struct UC_attributedatabag *attr;
UM_transf drwmat;
UM_coord vrefpt;
UM_vector vpnorm;
UU_LIST *keylist;
{
	int i,j,status,savelabel,nlists,n;
	UM_coord *lstp,ptx;
	UU_LIST list;
	struct UM_polyline_rec pline;
/*
.....Initialize routine
*/
	lstp = pts;
/*
.....Save the label display status
*/
	savelabel = attr->label_on;
/*
.....Initialize list to store projected points
*/
	uu_list_init(&list,sizeof(UM_coord),100,100);
	if (UU_LIST_NULLPTR(&list)) goto failed;
/*
.....Project the display list
*/
	nlists = lstp[0][0];
	lstp++;
	for (i=0;i<nlists;i++)
	{
/*
........Initialize polyline record
*/
		ur_setup_data(UM_POLYLINE_REL,&pline,sizeof(pline));
/*
........Project label onto drawing
*/
		ncl_proj_label_to_drw(eptr,&pline,attr,drwmat,vrefpt,vpnorm);
/*
........Project polyline to drawing
*/
		n = lstp[0][0];
		lstp++;
		for (j=0;j<n;j++)
		{
			um_nptpln(lstp[j],vrefpt,vpnorm,ptx);
			um_cctmtf (ptx,drwmat,ptx);
			uu_list_push(&list,ptx);
		}
		lstp += j;
/*
........Create polyline
*/
		pline.no_pt = n;
		pline.pt = (UU_REAL *)UU_LIST_ARRAY(&list);
		ncl_create_geom_drw(&pline,UM_DEFAULT_TF,attr);
		ur_update_displayable(pline.key,UM_DISPLAYABLE);
		uc_display(&pline);
/*
........Push key onto list
*/
		uu_list_push(keylist,&pline.key);
/*
........Turn off label for subsequent polylines
*/
		attr->label_on = 0;
		UU_LIST_EMPTY(&list);
	}
	status = UU_SUCCESS;
	goto done;
/*
.....Failed to project display list
*/
failed:
	status = UU_FAILURE;
/*
.....End of routine
*/
done:
	attr->label_on = savelabel;
	if (!UU_LIST_NULLPTR(&list)) uu_list_free(&list);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_displst_xform(pts,tf)
**       Transforms a display list.
**    PARAMETERS   
**       INPUT  : 
**          pts      Display list
**          tf       Transformation matrix.
**       OUTPUT :  
**          pts      Updated display list.
**    RETURNS      : 
**       UU_SUCCESS if no error; else UU_FAILURE 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_displst_xform(pts,tf)
UM_coord pts[];
UM_transf tf;
{
	int i,j,status,nlists,n;
	UM_coord *lstp;
/*
.....Initialize routine
*/
	lstp = pts;
/*
.....Transform the display list
*/
	nlists = lstp[0][0];
	lstp++;
	for (i=0;i<nlists;i++)
	{
/*
........Transform polyline
*/
		n = lstp[0][0];
		lstp++;
		for (j=0;j<n;j++) um_cctmtf (lstp[j],tf,lstp[j]);
		lstp += j;
	}
	status = UU_SUCCESS;
	goto done;
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL S_displst_OK (key,bf,bflg)
**       Returns OK if stored display is OK; deletes display list otherwise
**    PARAMETERS   
**       INPUT  : 
**          key        - Key of surface
**          bf         - current stored display type
**                       1 : only the boundary is stored
**                       0 : the whole surface (wireframe) display is stored
**          bflg       - required display type
**                       1 : Surface is shaded and only the boundary is needed
**                       0 : the whole surface (wireframe) display is needed
**       OUTPUT : none  
**    RETURNS      : 
**         UU_TRUE/UU_FALSE
**    SIDE EFFECTS : displst deleted if not OK
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_displst_OK (key,bf,bflg)
UU_KEY_ID key;
int bf,bflg;
{
	UU_LOGICAL ret = ((bf == 0 || bflg == 1));
/*
..... bf = 0 means the whole surface wireframe is stored, 
..... bf = 1 means only the boundary part is stored.
.....
..... So, OK if the whole wireframe is here or we only need the boundary part
*/
	if (!ret) ncl_lst_delete(DISPLAY_LIST, &key);
	return (ret);
}

/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL ncl_displst_OK (key,lstp)
**       Returns OK if stored display is OK; deletes display list otherwise
**    PARAMETERS   
**       INPUT  : 
**          lstp       - display list
**          key        - Key of surface
**          told       - current display tolerance
**          bflg       - 1 : Surface is shaded and only the boundary is needed
**                       0 : the whole surface (wireframe) display is needed
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_TRUE/UU_FALSE
**    SIDE EFFECTS : displst deleted if not OK
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_displst_OK (key,lstp,told,bflg)
UU_KEY_ID key;
UM_coord *lstp;
UU_REAL told; 
int bflg;
{
	int bf = lstp[0][2];

	return (S_displst_OK (key,bf,bflg));
}

/*********************************************************************
**    E_FUNCTION     : int ncl_displst_type (lstp)
**       Returns the stored display type
**    PARAMETERS   
**       INPUT  : 
**          lstp       - display list
**       OUTPUT :  none
**    RETURNS      : 
**          bf         - 1 : only the boundary is stored
**                       0 : the whole surface wireframe is stored
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_displst_type (lstp)
UM_coord *lstp;
{
	int bf = lstp[0][2];
	return (bf);
}

#if 0
/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL ncl_displst_OK1 (key,lstp)
**       Returns OK if tolerance is OK; deletes display list otherwise
**    PARAMETERS   
**       INPUT  : 
**          lstp       - display list
**          key        - Key of surface
**          told       - current display tolerance
**          bflg       - 1 : Surface is shaded and only the boundary is needed
**                       0 : the whole surface (wireframe) display is needed
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_TRUE/UU_FALSE
**    SIDE EFFECTS : displst deleted if not OK
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_displst_OK1 (key,lstp,told,bflg)
UU_KEY_ID key;
UM_coord *lstp;
UU_REAL told; 
int bflg;
{
	int bf = lstp[0][2];
 /*
..... two is added to the bf flag starting at 9.7 - it means the boundary is
..... displayed at a small offset away from a surface 
*/
 	if (!ncl_setver(96) && bf < 2)
		return (UU_FALSE);
	else
	{
		if (bf >= 2) bf -= 2;
/*
	UU_REAL tol = lstp[0][1];
	UU_LOGICAL ret = ((told < UM_BOUNDARY_TOLER || tol <= 1.2*told) &&
		(bf == 0 || bflg == 1));
*/
		return (S_displst_OK (key,bf,bflg));
	}
}
#endif

/*********************************************************************
**    E_FUNCTION     : int ncl_displst_delete (nclkey)
**       Delete a surface display list.
**    PARAMETERS
**       INPUT  :
**          nclkey          - Key of surface.
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_displst_delete(nclkey)
UM_int4 *nclkey;
{
	int status;

	status = ncl_lst_delete(DISPLAY_LIST, (UU_KEY_ID *)nclkey);
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_displst_setckey (nclkey)
**       Set NCL_ckey to be the key of the entity currently being
**       displayed.
**    PARAMETERS
**       INPUT  :
**          key          - Key of entity.
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_displst_setckey (key)
UM_int4 key;
{
   NCL_ckey = key;

   return (UU_SUCCESS);
}  

/*********************************************************************
**    E_FUNCTION     : int ncl_displst_getckey ()
**       Get the key of the entity currently being
**       displayed.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**       key of the entity currently being displayed
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_displst_getckey ()
{
   return (NCL_ckey);
}   

/*********************************************************************
**    E_FUNCTION     : int nuldsp ()
**       Set (global) displst to zero
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nuldsp ()
{
	gsetdsplst(0);
	return (0);
}
