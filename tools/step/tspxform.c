/*********************************************************************
**    NAME         :  tspxform.c
**       CONTAINS:
**             utp_activate_transform
**             utp_deactivate_transform
**	            utp_align_transforms
**             utp_free_xform_list
**             utp_print_xform_list
**					utp_push_transform
**					utp_transform
**					utp_transform_cyl
**					utp_transform_line
**					utp_transform_plane
**					utp_transform_point
**					utp_transform_sphere
**					utp_transform_vector
**    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tspxform.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:13:24
*********************************************************************/

#include "mdcoord.h"
#include "nccs.h"
#include "tiges.h"
#include "tigdefs.h"
#include "tstep.h"
#include "udebug.h"

typedef struct
{
	int recno[2];
	UU_LOGICAL used;
	UM_transf tf;
} Sxform_struc;

static int Sactive=-1,Sinc=0;
static UU_LOGICAL Sxform_init=UU_FALSE;
static UU_LIST Sxform_list;
static UM_transf Sactive_xform={1,0,0, 0,1,0, 0,0,1, 0,0,0};

/*********************************************************************
**    E_FUNCTION     :  utp_activate_transform(recno,merge)
**				Activates a transformation from the stack if one matches
**          the input label.
**    PARAMETERS   
**       INPUT  : 
**          recno    Record number of the entity.
**          merge    UU_TRUE if the activated matrix should be married
**                   to the currently active matrix.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL utp_activate_transform(recno,merge)
int recno;
UU_LOGICAL merge;
{
	int i,status,np;
	UU_LOGICAL flag;
	Sxform_struc *sptr,sxf;
/*
.....Initialize xform
*/
	flag = UU_FALSE;
	if (!merge) um_identtf(Sactive_xform);
/*
.....See if xform is on the stack
*/
	if (Sxform_init && recno != 0)
	{
		sptr = (Sxform_struc *)UU_LIST_ARRAY(&Sxform_list);
		for (i=0;i<UU_LIST_LENGTH(&Sxform_list);i++)
		{
			if (recno == sptr[i].recno[0] && (merge || !sptr[i].used))
			{
				flag = UU_TRUE;
				if (merge)
					um_tftmtf(Sactive_xform,sptr[i].tf,Sactive_xform);
				else
					um_tftotf(sptr[i].tf,Sactive_xform);
				if (!merge) sptr[i].used = UU_TRUE;
				if (recno != sptr[i].recno[1])
					utp_activate_transform(sptr[i].recno[1],UU_TRUE);
				break;
			}
		}
	}
	return(flag);
}

/*********************************************************************
**    E_FUNCTION     :  utp_deactivate_transform(recno)
**				Mark transformation matrix as unused since no geometry
**          was defined.
**    PARAMETERS   
**       INPUT  : 
**          recno    Record number of the entity.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : Note that the activate transform routine above
**      can activate multiple matrices that are associated with the
**      same entity and linked together via record numbers.  This
**      routine currently only deactivates the top level matrix.  It
**      may need to be changed later to deactivate all levels of the
**      associated matrices.
**    WARNINGS     : none
*********************************************************************/
void utp_deactivate_transform(recno)
int recno;
{
	int i;
	Sxform_struc *sptr;
/*
.....See if xform is on the stack and set it to unused.
*/
	if (Sxform_init && recno > 0)
	{
		sptr = (Sxform_struc *)UU_LIST_ARRAY(&Sxform_list);
		for (i=0;i<UU_LIST_LENGTH(&Sxform_list);i++)
		{
			if (recno == sptr[i].recno[0])
			{
				sptr[i].used = UU_FALSE;
				break;
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     :  utp_align_transforms()
**				Makes sure individual transformation is listed first in
**          transformation list and then the parent transformation.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_align_transforms()
{
	int i,irec,inc0,inc1;
	Sxform_struc *sptr;
/*
.....Count identical xforms in stack
.....The one with the least is the individual array
*/
	if (Sxform_init)
	{
		inc0 = inc1 = 0;
		sptr = (Sxform_struc *)UU_LIST_ARRAY(&Sxform_list);
		for (i=1;i<UU_LIST_LENGTH(&Sxform_list);i++)
		{
			if (sptr[0].recno[0] == sptr[i].recno[0]) inc0++;
			if (sptr[0].recno[1] == sptr[i].recno[1]) inc1++;
		}
/*
.....The transformation pointers must be swapped
*/
		if (inc0 > inc1)
		{
			for (i=0;i<UU_LIST_LENGTH(&Sxform_list);i++)
			{
				irec = sptr[i].recno[0];
				sptr[i].recno[0]= sptr[i].recno[1];
				sptr[i].recno[1]= irec;
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     :  utp_push_transform(rec1,rec2,tf,ntf)
**				Pushes a transformation relationship onto the stack.
**    PARAMETERS   
**       INPUT  : 
**          rec1     Record number of the relationship.
**          rec2     Parent of this relationship.
**          tf       Transformation to assign to label.
**          ntf      Estimated number of transformations to allocate
**                   space for.  Can be set to 0.
**       OUTPUT : none
**    RETURNS      : UU_SUCESS on success, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_push_transform(rec1,rec2,tf,ntf)
int rec1,rec2;
UM_transf tf;
int ntf;
{
	int i,status,np;
	char sbuf[80];
	Sxform_struc sxf,*sptr;
/*
.....Create stack if not existing
*/
	if (!Sxform_init)
	{
		np = ntf==0 ? 25 : ntf;
		uu_list_init(&Sxform_list,sizeof(Sxform_struc),np,np);
		Sxform_init = UU_TRUE;
	}
/*
.....Push xform onto stack
*/
	status = UU_SUCCESS;
	sxf.recno[0] = rec1;
	sxf.recno[1] = rec2;
	sxf.used = UU_FALSE;
	um_tftotf(tf,sxf.tf);
	uu_list_push(&Sxform_list,&sxf);
	status = UU_SUCCESS;
/*
sprintf(sbuf,"\nMatrix = %d,%d\n",rec1,rec2);
uig_list_out(sbuf,UU_FALSE);
for (i=0;i<4;i++)
{
	sprintf(sbuf,"%12.4f,%12.4f,%12.4f\n",tf[i][0],tf[i][1],tf[i][2]);
	uig_list_out(sbuf,UU_FALSE);
}
*/
	goto done;
/*
.....Could not push xform onto stack
*/
failed:
	status = UU_FAILURE;
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  utp_free_xform_list()
**				Empties the xform list and frees the memory.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_free_xform_list()
{
	if (Sxform_init)
	{
		uu_list_free(&Sxform_list);
		Sxform_init = UU_FALSE;
	}
}

/*********************************************************************
**    E_FUNCTION     :  utp_print_xform_list()
**				Outputs the xform list to the list file.
**    PARAMETERS   
**       INPUT  : 
**          flag     UU_TRUE:  Print matrix labels and entries.
**                   UU_FALSE: Print only matrix labels.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_print_xform_list(flag)
UU_LOGICAL flag;
{
	int i,j;
	Sxform_struc *sptr;
	char sbuf[80];

	if (Sxform_init)
	{
		sprintf(sbuf,"\nMatrix Stack\n");
		uig_list_out(sbuf,UU_FALSE);
		sptr = (Sxform_struc *)UU_LIST_ARRAY(&Sxform_list);
		for (i=1;i<UU_LIST_LENGTH(&Sxform_list);i++)
		{
			sprintf(sbuf,"\tMatrix = %d,%d\n",sptr[i].recno[0],sptr[i].recno[1]);
			uig_list_out(sbuf,UU_FALSE);
			if (flag)
			{
				for (j=0;j<4;j++)
				{
					sprintf(sbuf,"\t%12.4f,%12.4f,%12.4f\n",sptr[i].tf[j][0],
						sptr[i].tf[j][1],sptr[i].tf[j][2]);
					uig_list_out(sbuf,UU_FALSE);
				}
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     :  utp_transform(eptr)
**				Transforms an entity.
**    PARAMETERS   
**       INPUT  : 
**          eptr     Pointer to entity to transform.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_transform(eptr)
struct NCL_fixed_databag *eptr;
{
/*
.....Only transforms entities
.....if not an identity matrix
*/
	if (!um_is_idmat(Sactive_xform))
		uig_transform(eptr,Sactive_xform,UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION     :  utp_transform_cyl(cyl)
**				Transforms a cylinder.
**    PARAMETERS   
**       INPUT  : 
**          cyl      Cylinder to transform.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_transform_cyl(cyl)
UU_REAL cyl[];
{
/*
.....Only transforms entities
.....if not an identity matrix
*/
	if (!um_is_idmat(Sactive_xform))
	{
		um_cctmtf(&cyl[0],Sactive_xform,&cyl[0]);
		um_vctmtf(&cyl[3],Sactive_xform,&cyl[3]);
	}
}

/*********************************************************************
**    E_FUNCTION     :  utp_transform_line(spt,ept)
**				Transforms a line.
**    PARAMETERS   
**       INPUT  : 
**          spt      Starting point of line.
**          ept      Ending point of line.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_transform_line(spt,ept)
UM_coord spt,ept;
{
/*
.....Only transforms entities
.....if not an identity matrix
*/
	if (!um_is_idmat(Sactive_xform))
	{
		um_cctmtf(spt,Sactive_xform,spt);
		um_cctmtf(ept,Sactive_xform,ept);
	}
}

/*********************************************************************
**    E_FUNCTION     :  utp_transform_plane(pl)
**				Transforms a plane.
**    PARAMETERS   
**       INPUT  : 
**          pl       Plane to transform.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_transform_plane(pl)
struct NCL_nclpl_rec *pl;
{
/*
.....Only transforms entities
.....if not an identity matrix
*/
	if (!um_is_idmat(Sactive_xform))
	{
		um_cctmtf(pl->pt,Sactive_xform,pl->pt);
		um_vctmtf(pl->nvec,Sactive_xform,pl->nvec);
	}
}

/*********************************************************************
**    E_FUNCTION     :  utp_transform_point(cpt)
**				Transforms a point.
**    PARAMETERS   
**       INPUT  : 
**          cpt      Coordinate point to transform.
**       OUTPUT :
**          cpt      Transformed coordinate point.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_transform_point(cpt)
UM_coord cpt;
{
/*
.....Only transforms entities
.....if not an identity matrix
*/
	if (!um_is_idmat(Sactive_xform)) um_cctmtf(cpt,Sactive_xform,cpt);
}

/*********************************************************************
**    E_FUNCTION     :  utp_transform_sphere(cpt,nvec,svec)
**				Transforms a sphere.
**    PARAMETERS   
**       INPUT  : 
**          cpt      Center point of sphere.
**          nvec     Normal vector of sphere.
**          svec     Starting vector of sphere.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_transform_sphere(cpt,nvec,svec)
UM_coord cpt;
UM_vector nvec,svec;
{
/*
.....Only transforms entities
.....if not an identity matrix
*/
	if (!um_is_idmat(Sactive_xform))
	{
		um_cctmtf(cpt,Sactive_xform,cpt);
		um_vctmtf(nvec,Sactive_xform,nvec);
		um_vctmtf(svec,Sactive_xform,svec);
	}
}

/*********************************************************************
**    E_FUNCTION     :  utp_transform_sphere(vec)
**				Transforms a vector.
**    PARAMETERS   
**       INPUT  : 
**          vec      Vector to transform.
**       OUTPUT :
**          vec      Transformed vector.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_transform_vector(vec)
UM_vector vec;
{
/*
.....Only transforms entities
.....if not an identity matrix
*/
	if (!um_is_idmat(Sactive_xform)) um_vctmtf(vec,Sactive_xform,vec);
}
