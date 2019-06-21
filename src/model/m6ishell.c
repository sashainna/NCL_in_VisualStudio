/*********************************************************************
**    NAME         :  m6ieshell.c
**       CONTAINS: internal AG shell support routines
**			int umi_agshell_delete(eptr)
**			int umi_agshell_copy(e1ptr, e2ptr, bagsize)
**			int umi_drw_uv_on_face(face)
**			AG_SPLINEP umi_drw_face_u(face, u_draw)
**			AG_SPLINEP umi_drw_face_v(face, v_draw)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m6ishell.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:08
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "ulist.h"
#include "go.h"
#include "ginq.h"
#include "dasnog.h"
#include "class.h"
#include "mdrel.h"
#include "mattr.h"
#include "mdattr.h"
#include "mdebug.h"
#include "mdeval.h"
#include "mcrv.h"
#include "msol.h"
#include "modef.h"
#include "mderror.h"

#include "ag_incl.h"
#include "ag_global.h"

extern UU_LOGICAL UM_set_constituent_pickids;

/*********************************************************************
**    I_FUNCTION     : int umi_agshell_delete(eptr)
**			Delete the specified shell from AG only.
**    PARAMETERS   
**       INPUT  : 
**				eptr						pointer to AG shell entity
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : 
**			it is assumed that ONLY the AG shell data structure must be
**			deleted and that NO shell entity is in UNIBASE.
**
*********************************************************************/
int
umi_agshell_delete(eptr)
	struct UM_agshell_rec *eptr;

	{
	int status = UU_SUCCESS;
	int ag_status;
	AG_SHELLP shell;

	uu_denter(UU_MTRC,(us,"umi_agshell_delete(key=%d)", eptr->key));

	shell = (AG_SHELLP) eptr->shelladdr;
	if (shell != NULL)
		{
		ag_status = ag_db_sh(&shell);
		if (ag_status != 0) status = UU_FAILURE;
		}

done:
	uu_dexitstatus("umi_agshell_delete", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int umi_agshell_copy(e1ptr, e2ptr, bagsize)
**			Make a copy of the AG shell (E1PTR->CRVADDR) and store the
**			address of the copy in E2PTR->CRVADDR.
**    PARAMETERS   
**       INPUT  : 
**				e1ptr    pointer to entity to be copied
**				bagsize	size of the data bags pointed to by e1 and e2.
**       OUTPUT :  
**				e2ptr    pointer to new entity
**    RETURNS      : 
**				UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umi_agshell_copy(e1ptr, e2ptr, bagsize)
   struct UM_agshell_rec *e1ptr;
   struct UM_agshell_rec *e2ptr;
	int bagsize;

	{
	int status;
	AG_SHELLP  ag_fin_sh();
 

   uu_denter(UU_MTRC, (us,"umi_agshell_copy(key=%x,bagsize=%d)",e1ptr,bagsize));

	status = UU_SUCCESS;

	ag_fout_sh(e1ptr->shelladdr, "agcopy.tmp", AG_BINARY);
	e2ptr->shelladdr = (int) ag_fin_sh("agcopy.tmp", AG_BINARY);

	uu_dexitstatus("um_agshell_copy",status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int umi_drw_uv_on_face(face)
**       Draw uv constant parameter curves on the given face.
**    PARAMETERS   
**       INPUT  : 
**          face					pointer to AG face node
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umi_drw_face_uv(face)
	AG_FACEP face;

	{ 
 
	if (face != NULL)
		{
		/*umi_drw_face_u(face, 3);
		umi_drw_face_v(face, 5)*/;
		umi_face_tessellate(face, UU_FALSE);
		} 

	return;
	}

/*********************************************************************
**    E_FUNCTION     : AG_SPLINEP umi_drw_face_u(face, u_draw)
**       draw constant u parametric curves on a face.
**    PARAMETERS   
**       INPUT  : 
**          face				pointer to face
**				u_draw			0 => draw u0 and un only
**									1 => draw all u knot values
**									n => draw n-1 curves between knots
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umi_drw_face_u(face, u_draw)
	AG_FACEP face;
	int u_draw;

	{ 
	int				status = UU_SUCCESS;
	int				nu, i, j;
	UU_REAL			*u0, *u1, *t0, *tn, s, ds;
	AG_SURFACEP		sf;
	AG_CURVEP		cv, ag_crv_uface();
	AG_BOUNDARYP	bndy;
	AG_SNODEP		snd, snd_save;

	uu_denter(UU_MTRC,(us,"umi_drw_face_u(face=%x, u_draw=%d)", face, u_draw));

	sf = face->srf;
	bndy = face->ob;
	snd = sf->node0;
	nu = sf->nu;
	u0 = snd->u;
	snd_save = sf->node;

	/* draw u0 */
	s = *u0;
/*
	sprintf(UM_sbuf,"extract s=%g", s);
	um_pscroll(UM_sbuf);
	cv = ag_crv_uface(s, sf, bndy);
	sprintf(UM_sbuf,"cv=%x", cv);
	um_pscroll(UM_sbuf);
	if (cv != NULL)
		{
	sprintf(UM_sbuf,"draw 1");
	um_pscroll(UM_sbuf);
		ag_dr_crv(cv);
	sprintf(UM_sbuf,"debuild 1");
	um_pscroll(UM_sbuf);
		ag_db_crv(&cv);
		}
*/

	for (i=1; i<=nu; i++)
		{
		snd = snd->unext; u1 = snd->u;
		if (u0 != u1)
			{/* span has length */
			if (u_draw > 1)
				{ 
				s = *u0;
				ds = (*u1 - s)/u_draw;
				for (j=1; j<u_draw; j++)
					{/* extract and draw s = s + ds */
					s += ds;
	sprintf(UM_sbuf,"extract s=%g", s);
	um_pscroll(UM_sbuf);
					cv = ag_crv_uface(s, sf, bndy);
	sprintf(UM_sbuf,"cv=%x", cv);
	um_pscroll(UM_sbuf);
					if (cv != NULL)
						{
	sprintf(UM_sbuf,"draw 2");
	um_pscroll(UM_sbuf);
						ag_dr_crv(cv);
	sprintf(UM_sbuf,"debuild 2");
	um_pscroll(UM_sbuf);
						ag_db_crv(&cv);
						}
					}
				}
			if (u_draw >= 1  &&  i != nu)
				{
				/* draw u1 */
				s = *u1;
	sprintf(UM_sbuf,"extract s=%g", s);
	um_pscroll(UM_sbuf);
				cv = ag_crv_uface(s, sf, bndy);
				if (cv != NULL)
					{
	sprintf(UM_sbuf,"draw 3");
	um_pscroll(UM_sbuf);
					ag_dr_crv(cv);
	sprintf(UM_sbuf,"debuild 3");
	um_pscroll(UM_sbuf);
					ag_db_crv(&cv);
					}
				}
 			}
		u0 = u1;
		}

	uu_dexitstatus("umi_drw_face_u", status);
	return(status);
	} 

/*********************************************************************
**    E_FUNCTION     : AG_SPLINEP umi_drw_face_v(face, v_draw)
**       draw constant v parametric curves on a face.
**    PARAMETERS   
**       INPUT  : 
**          face				pointer to face
**				v_draw			0 => draw v0 and vn only
**									1 => draw all v knot values
**									n => draw n-1 curves between knots
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umi_drw_face_v(face, v_draw)
	AG_FACEP face;
	int v_draw;

	{ 
	int				status = UU_SUCCESS;
	int				nv, i, j;
	UU_REAL			*v0, *v1, s, ds;
	AG_SURFACEP		sf;
	AG_CURVEP		cv, ag_crv_vface();
	AG_BOUNDARYP	bndy;
	AG_SNODEP		snd, snd_save;

	uu_denter(UU_MTRC,(us,"umi_drw_face_v(face=%x, v_draw=%d)", face, v_draw));

	sf = face->srf;
	bndy = face->ob;
	snd = sf->node0;
	nv = sf->nv;
	v0 = snd->v;
	snd_save = sf->node;

	/* draw v0 */
	s = *v0;
/*
	cv = ag_crv_vface(s, sf, bndy);
	if (cv != NULL)
		{
		ag_dr_crv(cv);
		ag_db_crv(&cv);
		}
*/

	for (i=1; i<=nv; i++)
		{
		snd = snd->vnext; v1 = snd->v;
		if (v0 != v1)
			{/* span has length */
			if (v_draw > 1)
				{ 
				s = *v0;
				ds = (*v1 - s)/v_draw;
				for (j=1; j<v_draw; j++)
					{/* extract and draw s = s + ds */
					s += ds;
					cv = ag_crv_vface(s, sf, bndy);
					if (cv != NULL)
						{
						ag_dr_crv(cv);
						ag_db_crv(&cv);
						}
					}
				}
			if (v_draw >= 1  &&  i != nv)
				{
				/* draw v1 */
				s = *v1;
				cv = ag_crv_vface(s, sf, bndy);
				if (cv != NULL)
					{
					ag_dr_crv(cv);
					ag_db_crv(&cv);
					}
				}
 			}
		v0 = v1;
		}

	uu_dexitstatus("umi_drw_face_u", status);
	return(status);
	} 

