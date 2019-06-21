
/*********************************************************************
**    NAME:  m8ersol1.c
**       CONTAINS:
**			um_drw31_body(ptr, tfmat, attrptr)
**			um_p31_body(ptr)
**			um_tr31_tranbody(eptr,dir)
**			um_rt31_rotbody(eptr,pt,dir,angle)
**			um_mr31_mirrbody(eptr,pt,normal)
**			um_sc31_scalbody(eptr,pt,scale)
**			um_cp31_copybody(e1, e2, bagsize)
**			um_dl31_delbody(key)
**			um_rm31_romini()
**			um_c31lamina(p,e)
**			um_m31_moment(eptr, option, rotpt, rotvec, pt, uvec, vvec)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m8ersol1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:10
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "usysg.h"
#include "class.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mattr.h"
#include "mcrv.h"
#include "msol.h"
#include "modef.h"
#include "mromcom.h"
#include "mdebug.h"
#include "mdcoord.h"
#include "view.h"

#define CURRENT_BODY 0
#define NEWEST_BODY 1

/*********************************************************************
**    E_FUNCTION     : um_drw31_body(ptr, tfmat, attrptr)
**      Draw a solid body.
**    PARAMETERS   
**       INPUT  : 
**				ptr						pointer to body record
**				tfmat						transformation matrix
**				attrptr					pointer to attribute entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_drw31_body(ptr, tfmat, attrptr)
	struct  UM_body_rec *ptr;
	UM_transf tfmat;
	struct UM_attrdatabag *attrptr;

	{
	char cmd[120];					/* ROMULUS command */
	UU_REAL	mat[4][4];			/* view transformation matrix */

	uu_denter( UU_MTRC,(us,"um_drw31_body(key=%x, tfmat=%x, attr=%x)",
		ptr->key, tfmat, attrptr));

	um_set_disp_attr(attrptr);

	um_view_mat(UV_act_vports[0][0].view, mat);
	um_init_rombuf();
	um_add_rombuf("OPT TERM 4014");
	sprintf(cmd,"VIEW MATRIX %f %f %f %f@",mat[0][0],mat[0][1],mat[0][2],mat[0][3]);
	um_add_rombuf(cmd);
	sprintf(cmd,"%f %f %f %f@",mat[1][0],mat[1][1],mat[1][2],mat[1][3]);
	um_add_rombuf(cmd);
	sprintf(cmd,"%f %f %f %f@",mat[2][0],mat[2][1],mat[2][2],mat[2][3]);
	um_add_rombuf(cmd);
	sprintf(cmd,"%f %f %f %f",mat[3][0],mat[3][1],mat[3][2],mat[3][3]);
	um_add_rombuf(cmd);
	sprintf(cmd,"VIEW INFINITY");
	um_add_rombuf("VIEW SCALE MANUAL 1.0");
	um_add_rombuf(cmd);
	sprintf(cmd,"USKET %s",ptr->name);
	um_add_rombuf(cmd);
	um_callromulus();
	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : um_p31_body(ptr)
**      Print the contents of a body record.
**    PARAMETERS   
**       INPUT  : 
**				ptr			pointer to data of line record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_p31_body(ptr)
	struct  UM_body_rec  *ptr;

	{
	char s1[80];

	uu_denter( UU_MTRC,(us,"um_p31_body(%8x)",ptr));
	sprintf(s1, "BODY %d", ptr->key);
	um_pscroll(s1);
	sprintf(UM_sbuf, "label %7.7s", ptr->label);
	um_pscroll(UM_sbuf);
	sprintf(s1, "body id %d", ptr->id);
	um_pscroll(s1);
	sprintf(s1,"name %s",ptr->name);
	um_pscroll(s1);
	um_p_ary(UM_PINT, "body number", 1, &(ptr->body_number));
	um_p_ary(UM_PINT, "num edges", 1, &(ptr->no_edge));
	um_p_ary(UM_PINT, "edge ids", ptr->no_edge, ptr->edge);
	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : um_tr31_tranbody(eptr, dir)
**			Translate the specified body (EPTR) along the specified 
**			direction (DIR).
**    PARAMETERS   
**       INPUT  : 
**				eptr					pointer to body entity data
**				dir					direction to move along
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_tr31_tranbody(eptr, dir)
	struct UM_body_rec *eptr;
	UM_vector dir;

	{
	UU_REAL dist;
	char cmd[120];					/* ROMULUS command */
	int status = UU_SUCCESS;

/*------------------------------------------------------------------------
**  Build a Romulus command to translate the specified body a 
**  specified distance along a vector.
*/
	uu_denter(UU_MTRC,(us,"um_tr31_tranbody(?,?,?)"));
	dist = um_mag(dir);
	if (! UM_ZILCH(dist))
		{
		if (um_update_geom(eptr, UM_DEFAULT_TF) != UU_SUCCESS)
			{
			uu_uerror1(UM_MODEL,313, "translate");
			/* can't translate body */
			goto failed;
			}
		um_init_rombuf();
		sprintf(cmd,"MOVE %s %f@",eptr->name,dist);
		um_add_rombuf(cmd);
		sprintf(cmd,"%f,%f,%f",dir[0],dir[1],dir[2]);
		um_add_rombuf(cmd);
		um_callromulus();
		}
	goto done;
failed: status = UU_FAILURE;
done: uu_dexitstatus("um_tr31_tranbody", status);
	return(status);
	}	

/*********************************************************************
**    E_FUNCTION     : um_rt31_rotbody(eptr, pt, dir, angle, rotmat)
**      Rotate the specified body.
**    PARAMETERS   
**       INPUT  : 
**				eptr						entity pointer
**				pt							point on axis of rotation
**				dir						direction of axis of rotation
**				angle						angle of rotation
**				rotmat					rotation matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_rt31_rotbody(eptr, pt, dir, angle, rotmat)
	struct UM_body_rec *eptr;
	UM_coord pt;
	UM_vector dir;
	UM_angle angle;
	UM_transf rotmat;

	{
	char cmd[120];						/* ROMULUS command */
	UU_REAL angdeg;					/* angle of rotation in degrees */
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"um_rt31_rotbody(?,?,?,?)"));
	angdeg=angle*180.0/UM_PI;
	if (!UM_ZILCH(angle) && !UM_ZILCH(angle - UM_TWOPI))
		{
		if (um_update_geom(eptr, UM_DEFAULT_TF) != UU_SUCCESS)
			{
			uu_uerror1(UM_MODEL,313, "rotate");
			/* can't rotate body */
			goto failed;
			}
		um_init_rombuf();
		sprintf(cmd,"ROTATE %s ANGLE %f@",eptr->name,angdeg);
		um_add_rombuf(cmd);
		sprintf(cmd,"ABOUT %f,%f,%f %f,%f,%f", pt[0],pt[1],pt[2],
				  dir[0],dir[1],dir[2]);
		um_add_rombuf(cmd);
		um_callromulus();
		}
	goto done;
failed: status = UU_FAILURE;
done: uu_dexitstatus("um_rot31_rotbody", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : um_mr31_mirrbody(eptr, pt, normal, mirrmat)
**      Mirror the specified body.
**    PARAMETERS   
**       INPUT  : 
**				eptr						entity pointer
**				pt							point on the mirror plane
**				normal					normal to mirror plane
**				mirrmat					mirror matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_mr31_mirrbody(eptr, pt, normal, mirrmat)
	struct UM_body_rec *eptr;
	UM_coord pt;
	UM_vector normal;
	UM_transf mirrmat;

	{
	char cmd[120];						/* ROMULUS command */
	int status = UU_SUCCESS;

	uu_denter( UU_MTRC,(us,"um_mr31_mirrbody(?,?,?,?)"));
	if (um_update_geom(eptr, UM_DEFAULT_TF) != UU_SUCCESS)
		{
		uu_uerror1(UM_MODEL,313, "mirror");
		/* can't mirror body */
		goto failed;
		}
	um_init_rombuf();
	sprintf(cmd,"MIRROR %s @",eptr->name);
	um_add_rombuf(cmd);
	sprintf(cmd,"%f,%f,%f %f,%f,%f", pt[0],pt[1],pt[2],normal[0],normal[1],normal[2]);
	um_add_rombuf(cmd);
	um_callromulus();
	goto done;
failed: status = UU_FAILURE;
done: uu_dexitstatus("um_mr31_mirrbody", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : um_sc31_scalbody(eptr, pt, scalmat, scale)
**      Scale the specified body.
**    PARAMETERS   
**       INPUT  : 
**				eptr						entity pointer
**				pt							point on axis of rotation
**				scalmat					scale matrix
**				scale						scale ratio
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_sc31_scalbody(eptr, pt, scalmat, scale)
	struct UM_body_rec *eptr;
	UM_coord pt;
	UM_transf scalmat;
	UU_REAL scale;

	{
	char cmd[120];						/* ROMULUS command */
	UU_REAL dist;						/* distance to translate to origin */
	int status = UU_SUCCESS;

/*  Since the Romulus Scale Command only scales about origin, the body
**  must be translated so that pt is at the origin, scaled, then translated
**  back to its original place
*/
	uu_denter( UU_MTRC,(us,"um_sc31_scalbody(?,?,?,?)"));
	if (!UM_ZILCH(scale))
		{
		if (um_update_geom(eptr, UM_DEFAULT_TF) != UU_SUCCESS)
			{
			uu_uerror1(UM_MODEL,313, "scale");
			/* can't scale body */
			goto failed;
			}
		um_init_rombuf();

		/* translate pt to origin */
		dist = um_mag(pt);
		if (dist > UM_FUZZ)
			{
			sprintf(cmd,"MOVE %s %f@",eptr->name,dist);
			um_add_rombuf(cmd);
			sprintf(cmd,"%f,%f,%f",-pt[0],-pt[1],-pt[2]);
			um_add_rombuf(cmd);
			}

		/* scale body */
		sprintf(cmd,"MAGNIFY %s %f",eptr->name,scale);
		um_add_rombuf(cmd);

		/* translate body back to its original spot */
		if (dist > UM_FUZZ)
			{
			sprintf(cmd,"MOVE %s %f@",eptr->name,dist);
			um_add_rombuf(cmd);
			sprintf(cmd,"%f,%f,%f",pt[0],pt[1],pt[2]);
			um_add_rombuf(cmd);
			}
		um_callromulus();
		}
	goto done;
failed: status = UU_FAILURE;
done: uu_dexitstatus("um_sc31_scalbody", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : um_cp31_copybody(e1, e2, bagsize)
**      Copy the specified body.
**    PARAMETERS   
**       INPUT  : 
**				e1     	pointer to the body entity.
**				bagsize	size of storage for entity.
**       OUTPUT :  
**				e2       pointer to new entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_cp31_copybody(e1, e2, bagsize)
	struct UM_body_rec *e1;
	struct UM_body_rec *e2;
	int bagsize;

	{
	struct UM_attrdata_rec attrbag;
	char cmd[120];									/* ROMULUS command */
	UU_REAL body_number[2];

	uu_denter(UU_MTRC,(us,"um_cp31_copybody(?,?,?)"));

	/* copy the geometric entity in ROMULUS	*/
	ur_setup_data(e1->rel_num, e2, bagsize);
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e2->label, "");
	e2->subscr = 0;
	sprintf(cmd,"COPY %s", e1->name);
	um_romulus(cmd);

	/* update the body number in the ROMULUS database	*/
	UM_NUM_OF_SOLIDS++;
	um_get_by_name(NEWEST_BODY, &(e2->id), e2->name);
	um_get_disp_attr(e1->key, &attrbag);

	/* set the color, and body number in the ROMULUS data base */
	um_rom_appear(e2->name, attrbag.color, UM_NUM_OF_SOLIDS);

	/* put the new copy into UNIBASE */
	um_get_by_edge(UM_MAXEDGE, &(e2->id), &(e2->no_edge), e2->edge);
	e2->pitch = e1->pitch;
	e2->body_number = UM_NUM_OF_SOLIDS;
	um_create_geom(e2, UM_DEFAULT_TF, &attrbag); 
	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : um_dl31_delbody(key, association)
**      Delete the specified body.  
**    PARAMETERS   
**       INPUT  : 
**				key						body entity key id
**				associate				association entity key id (NULL)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_dl31_delbody(key, association)
	UU_KEY_ID key;
	UU_KEY_ID association;

	{
	struct UM_body_rec e;
	char cmd[120];						/* ROMULUS command */
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"um_dl31_delbody(%d)",key));
	e.key = key;
	if (um_get_all_geom(&e, sizeof(struct UM_body_rec)) 
		!= UU_SUCCESS) goto failed;

	/* find out if we are allowed to delete it */
	if (ur_force_delete_all(key) != UU_SUCCESS)
		{
		uu_uerror1(UM_MODEL,313, "delete");
		/* can't delete body */
		goto failed;
		}

	sprintf(cmd,"DELETE %s",e.name);
	um_romulus(cmd);

	goto done;
failed: status = UU_FAILURE;
done: uu_dexitstatus("um_dl31_delbody", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : um_rm31_romini()
**      Issue a ROMULUS command to initialize the ROMULUS system.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_rm31_romini()

	{
/*------------------------------------------------------------------------
**  Start of Executable Code
**------------------------------------------------------------------------
**
**  block comment
*/
	uu_denter( UU_MTRC,(us,"um_rm31_romini()"));
	um_romulus("ROMULUS");
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     : um_c31lamina(p,e)
**      Create a lamina in ROMULUS.
**    PARAMETERS   
**    INPUT:
**      	p							picked  composite curve entity
**    OUTPUT:
**      	e							ROMULUS created  entity
**    RETURNS		: 
**			status         UU_FALSE => no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL
um_c31lamina(p,e)
	struct UM_compcrv_rec *p;
	struct UM_body_rec *e;

	{
	UM_coord spt;					/* start point of curve */
	UM_coord ept;					/* end point of curve */
	UM_coord temp;					/* temporary */
	UM_vector axis;				/* axis of circle */
	struct UC_entitydatabag pc;/* constituent curve of picked entity */
	char cmd1[120];				/* ROMULUS command */
	char cmd[120];					/* ROMULUS command */
	char terminator;				/* ROMULUS command terminator */
	int i;							/* index */
	int status;
	UU_LOGICAL err;            /* error status */
	UU_REAL body_color;
	UU_REAL body_number;			/* body number (used to match unibase and
											the ROMULUS data base entities durring
											save/load/merge)	*/

	uu_denter( UU_MTRC,(us,"um_c31lamina(%8x,?)",p->key));

	err = UU_FALSE;
	um_romulus("CREATE ACORN");
	um_get_by_name(CURRENT_BODY,&e->id,e->name);
	UM_NUM_OF_SOLIDS++;
	e->body_number = UM_NUM_OF_SOLIDS;

	/* Create the lamina */
	um_init_rombuf();
	for (i=0; i<p->no_cid; i++)
		{
		pc.key = p->cid[i].crvid;
		status = uc_retrieve_data(&pc, sizeof(struct UC_entitydatabag));
		if (status != UU_SUCCESS)
			err = UU_TRUE;
		else if ((pc.rel_num != UM_LINE_REL) && (pc.rel_num != UM_CIRCLE_REL))
			{ /* Only lines/circles in composites can be lifted into solids */
			uu_uerror0( UM_MODEL,82);
			err = UU_TRUE;
			}
		else
			{
			um_get_endpts(&pc, UM_idmat, spt, ept);
			if (p->cid[i].reverse)
				{
				um_vctovc(spt, temp);
				um_vctovc(ept, spt);
				um_vctovc(temp, ept);
				}
			if (i==0) 
				sprintf(cmd1,"LINE %f,%f,%f TO",spt[0],spt[1],spt[2]); 
			else 
				sprintf(cmd1,"LINE TO ");
			if (pc.rel_num !=  UM_CIRCLE_REL) terminator = ' '; 
			else terminator = '@';
			sprintf(cmd,"%s  %f,%f,%f%c",cmd1,ept[0],ept[1],ept[2],terminator);
			um_add_rombuf(cmd);
			switch (pc.rel_num)
				{
				case  UM_CIRCLE_REL:
					{
					struct UM_circle_rec *ptr;
					ptr = (struct UM_circle_rec *) &pc;
					um_vctovc(ptr->nvec,axis);
					if (!(p->cid[i].reverse))
						{
						if (ptr->dang < 0.0) 
							um_vctmsc(axis,(UU_REAL) -1.0,axis);
						}
					else
						{
						if (ptr->dang > 0.0) 
							um_vctmsc(axis,(UU_REAL) -1.0,axis);
						}
	
					sprintf(cmd," CIRCLE %f,%f,%f %f,%f,%f %f",
							ptr->center[0], ptr->center[1],ptr->center[2],
							axis[0],axis[1],axis[2],ptr->radius);
					um_add_rombuf(cmd);
					}
					break;
				default:
					break;
				}
			}
		if (err == UU_TRUE) break;
		}

	if (err)
		{
		um_init_rombuf();
		sprintf(cmd,"DELETE %s",e->name);
		um_romulus(cmd);
		}
/*		You cannot call um_dl31_delbody here because the acorn
		has not been put into unibase yet.
		um_dl31_delbody(e);    */
	else
		{
		um_callromulus();
		}
	uu_dexit;
	return (err);
	}
/*********************************************************************
**    I_FUNCTION    : um_m31_moment(eptr, option, rotpt, rotvec, pt, uvec, vvec)
**      Calculate the moment of inertia one the specified body.
**    PARAMETERS   
**       INPUT  : 
**          eptr                 entity pointer
**          option               entry point option
**          rotpt                point on axis of rotation
**          rotvec               direction of axis of rotation
**          pt                   point lying on u and v axes
**          uvec                 axes about which the inertial tensor
**          vvec                 is calculated (default principal axes)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_m31_moment(eptr, option, rotpt, rotvec, pt, uvec, vvec)
   struct UM_body_rec *eptr;
   int option;                   /* entry option */
   UM_coord rotpt;    	         /* point on the rotation axis */
   UM_vector rotvec;   	         /* rotation axis dirsction vector */
   UM_coord pt;        	        /* point lying on the u and v axes */
   UM_vector uvec;              /* axes about which the inertial tensor is */
   UM_vector vvec;              /* calculated, default = principal axes */

  	{
   char cmd[120];                /* ROMULUS command */

   uu_denter( UU_MTRC,(us,"um_m31_moment(?,%d,?,?,?,?,?)",option));
   um_init_rombuf();
   switch (option)
    	{
      case 0:
         sprintf(cmd,"UMOMNT %s @", eptr->name);
         um_add_rombuf(cmd);
         sprintf(cmd,"ABOUT %f,%f,%f %f,%f,%f", rotpt[0], rotpt[1], rotpt[2],
            rotvec[0], rotvec[1], rotvec[2]);
         um_add_rombuf(cmd);
         break;
      case 1:
         sprintf(cmd,"UMOMNT %s ", eptr->name);
         um_add_rombuf(cmd);
         break;
      case 2:
         sprintf(cmd,"UMOMNT %s @", eptr->name);
         um_add_rombuf(cmd);
         sprintf(cmd,"ABOUT %f,%f,%f %f,%f,%f @", pt[0], pt[1], pt[2], 
            uvec[0], uvec[1], uvec[2]);
            um_add_rombuf (cmd);
         sprintf(cmd,"VDIR %f,%f,%f",vvec[0], vvec[1], vvec[2]);
         um_add_rombuf (cmd);
         break;
     	}
   um_callromulus();
   uu_dexit;
   }
