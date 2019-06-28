/*********************************************************************
**    NAME         :  m3ecomp.c
**       CONTAINS: composite curve support routines
**       void um_set_attrfl()
**       void um_reset_attrfl()
**			int um_p5_compcrv(ptr)
**			int um_disp5_compcrv(eptr)
**			int um_drw5_compcrv(ptr,tfmat,attrptr)
**			int um_dl5_compcrv(eptr)
**			int um_undl5_compcrv(key, undeleteok)
**			int um_dis5_compcrv(compptr)
**			int um_tr5_trancompcrv(eptr, offset)
**			int um_tf5_tranfcompcrv(eptr, tfmat, store)
**			int um_cp5_copycompcrv(eptr1, eptr2, bagsize)
**			um_fll5_fillcomp(ptr, tfmat, attrptr)
**			um_c5_mergecrv(numkeys, keys, comp)
**       um_cctou_compcrv(eptr)
**       um_cp5_copycompcrv1(eptr1,isubid,isubid1,idir,eptr2)
**			um_getcrvlen(eptr,isub,isub1,idir)
**       um_redef_compcrv(eptr)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3ecomp.c , 25.3
**    DATE AND TIME OF LAST MODIFICATION
**       08/24/15 , 17:35:29
*********************************************************************/
#include "nccs.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "class.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdattr.h"
#include "mattr.h"
#include "mcrv.h"
#include "modef.h"
#include "mdeval.h"
#include "mdebug.h"
#include "nclfc.h"

#include "misect.h"

#define	UM_MAXFILLSEGS	100
#define	UM_MAXSPC	50		/* maximum number of segments drawn per curve	*/
#define	UM_MAX(A, B)	((A)>(B)?(A):(B))
#define	UM_MIN(A, B)	((A)<(B)?(A):(B))

#define UM_TRACE 0

extern UU_LOGICAL UM_set_constituent_pickids;
/*
.....Moved definition to m2dba2.c so they will be defined in iges.
*/
extern UU_LOGICAL NCL_create_compcrv;
extern UU_LOGICAL NCL_copy_compcrv;

void um_c5_update_len();

static UU_LOGICAL Suse_attr = UU_FALSE;
/*********************************************************************
**    E_FUNCTION     : int um_set_attrfl(ptr)
**       Set secondary attribute flag.  When set, the components of
**       the composite curve will be displayed using two attribute
**       records for the components.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_set_attrfl()
{
	Suse_attr = UU_TRUE;
}

/*********************************************************************
**    E_FUNCTION     : int um_reset_attrfl(ptr)
**       Reset flag
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_reset_attrfl()
{
	Suse_attr = UU_FALSE;
}

/*********************************************************************
**    E_FUNCTION     : int um_p5_compcrv(ptr)
**       Print the composite curve data
**    PARAMETERS   
**       INPUT  : 
**				ptr					pointer to data for composite curve
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_p5_compcrv(ptr)
	struct  UM_compcrv_rec *ptr;
	{
	int i;
	char rev[100];

	uu_denter(UU_MTRC,(us,"um_p5_compcrv(%8x)",ptr));

	sprintf(UM_sbuf,"COMPCRV %d ",ptr->key);
	um_pscroll(UM_sbuf);

	sprintf(UM_sbuf, "label %7.7s", ptr->label);
	um_pscroll(UM_sbuf);

	um_p_ary(UM_PFLOAT,"arclen",1,&ptr->arclen);

	if (ptr->t0 != 0. || ptr->t1 != 1.)
	{
		sprintf(UM_sbuf,"trimmed to t0=%g, t1=%g",ptr->t0,ptr->t1);
		um_pscroll(UM_sbuf);
	}

	if (ptr->planar == UU_TRUE)
		um_pscroll("planar curve");
	else if (ptr->planar == UU_FALSE)
		um_pscroll("non planar curve");
	else
		um_pscroll("planar not set correctly");

	if (ptr->open == UU_TRUE)
		um_pscroll("open curve");
	else if (ptr->open == UU_FALSE)
		um_pscroll("closed curve");
	else
		um_pscroll("open not set correctly");

	if (ptr->open == UU_TRUE)
		um_pscroll("continuous curve");
	else if (ptr->open == UU_FALSE)
		um_pscroll("non continuous curve");
	else
		um_pscroll("continuity not set correctly");

 	um_p_ary(UM_PINT,"fill color ",1,&ptr->fcolor);

	um_p_ary(UM_PINT,"no_cid ",1,&ptr->no_cid); 

	for (i=0; i<ptr->no_cid; i++)
		{
		if (ptr->cid[i].reverse == UU_TRUE)
			strcpy(rev, "reversed");
		else if (ptr->cid[i].reverse == UU_FALSE)
			strcpy(rev, "not reversed");
		else
			strcpy(rev, "reverse not set correctly");
		sprintf(UM_sbuf,"subcurve key=%x, param=%g, %s",
			ptr->cid[i].crvid, ptr->cid[i].endparam, rev);
		um_pscroll(UM_sbuf);
		}
	umi_print_transf(ptr->key);
	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int um_disp5_compcrv(eptr)
**			Set global flag to set pickids in DIGS segment, call VIEWING
**			subsystem to create DIGS segment and draw composite curve.
**			Finally reset global flag to inhibit setting pick ids when
**			composite curves are drawn.
**    PARAMETERS   
**       INPUT  : 
**				eptr            pointer to entity record
**       OUTPUT :  
**          none
**    RETURNS: UU_SUCCESS if no problems encountered, UM_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_disp5_compcrv(eptr)
	struct UM_compcrv_rec *eptr;

	{
	int status;

	uu_denter(UU_MTRC,(us,"um_disp5_compcrv(key=%d)",eptr->key));

	status = uv_disp_entity(eptr);

	uu_dexitstatus("um_disp5_compcrv", status);
	return(status);
	}	
/*********************************************************************
**    E_FUNCTION     : int um_drw5_compcrv(ptr,tfmat,attrptr)
**    Draw a composite curve in the current segment using one or two
**    attribute records to set the display properties of the component
**    curves.
**    PARAMETERS   
**       INPUT  : 
**				ptr					pointer to composite record 
**				tfmat					transformation matrix
**				attrptr				pointer to attribute record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_drw5_compcrv(ptr,tfmat,attrptr)
	struct  UM_compcrv_rec *ptr;
	UM_transf tfmat;
	struct UM_attrdata_rec *attrptr;

	{
	struct UC_entitydatabag e;			/* one of the composite entities */
	UM_transf subcrv_tfmat;				/* transformation for subcurves */
	int i;									/* index */
	UM_int2 idx, ival;
	int status = UU_SUCCESS;
	UU_REAL t0,t1;
	UU_LOGICAL match,first = UU_TRUE;
	struct UM_crvdatabag tptr;
	struct UM_attrdata_rec attrptr2;
/*
.....Added the option to use an additional attribute setting for
.....Advanced Pocket form.  The second attribute settings are given
.....in the routine ncl_pocket_checkind - ASF 11/6/13.
*/
	t0 = ptr->t0;
	t1 = ptr->t1;
	if (Suse_attr) uc_retrieve_attr(ptr->key,&attrptr2);

	uu_denter( UU_MTRC,(us,"um_drw5_compcrv(key:%d,tfmat:%x,attrptr:%x)",
					ptr->key, tfmat, attrptr));
 
 	/**	--	added for filled regions	--	**/
 	if (ptr->fcolor != UM_BACKGROUND && ptr->planar)
 	{
		/** use alternate drawing method	**/
 		um_fll5_fillcomp(ptr, tfmat, attrptr);
 	}
	else
	{ /**	--	not filled region	--	**/

		idx = 136;
		getifl(&idx, &ival);
		if (ival > 1)
			status = ncl_disp_curve (ptr,tfmat,attrptr);
		else
		{
			for (i=0; i<ptr->no_cid; i++)
			{
				if (t0 > ptr->cid[i].endparam) continue;
				if (Suse_attr)
					match = ncl_pocket_checkind(ptr->key,(i+1),&attrptr2);
				e.key = ptr->cid[i].crvid;
				uc_retrieve_data(&e, sizeof(e));
				uc_retrieve_transf(e.key, subcrv_tfmat);
				um_tftmtf(subcrv_tfmat, tfmat, subcrv_tfmat);
				um_c5_trimpart(&e,ptr->cid,ptr->no_cid,t0,t1,i,subcrv_tfmat);
				if (UM_set_constituent_pickids) gspickid(e.key);
				tptr.key = e.key;
				uc_retrieve_data(&tptr, sizeof(e));
				if (Suse_attr && match) uc_draw(&tptr, subcrv_tfmat, &attrptr2);
				else uc_draw(&tptr, subcrv_tfmat, attrptr);
				ur_update_data_fixed(&e);
				if (t1 - ptr->cid[i].endparam < UM_DFUZZ) break;
			}
		}
	}
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_dl5_compcrv(eptr)
**       Delete a composite curve's component curves.
**    PARAMETERS   
**       INPUT  : 
**				eptr			pointer to composite curve
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_dl5_compcrv(key)
	UU_KEY_ID key;

	{
	struct UM_compcrv_rec e;
	int i;							/* index */
	UU_KEY_ID lkey, NULKEY = 0;

	if (key > NULKEY)
	{
		e.key = key;
		ncl_retrieve_data(&e, sizeof(e));
		for (i=0; i<e.no_cid; i++) 
		{
			lkey = e.cid[i].crvid;
			if (lkey > NULKEY) uc_delete(lkey);
		}
		ur_delete_all(key);
	}
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int um_undl5_compcrv(key, undeleteok)
**       Check to see if the composite curve can be undeleted.
**    PARAMETERS   
**       INPUT  : 
**				key				key of composite curve
**       OUTPUT :  
**          undeleteok		UU_TRUE => can be undeleted
**									UU_FALSE => can not be undeleted
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_undl5_compcrv(key, undeleteok)
	UU_KEY_ID key;
	UU_LOGICAL *undeleteok;

	{
	int status;
	int i;
	struct UM_compcrv_rec e;

	uu_denter(UU_MTRC,(us,"um_undl5_compcrv(key:%d)",key));
	e.key = key;
	status = uc_retrieve_data(&e, sizeof(e));
	if (status != UU_SUCCESS) goto done;
	for (i=0; i<e.no_cid; i++) 
		{
		status = uc_canbe_undeleted(e.cid[i].crvid, undeleteok);
		if ((status != UU_SUCCESS) || (!*undeleteok)) goto done;
		}

done:;
	uu_dexitstatus("um_undl5_compcrv", status);
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_dis5_compcrv(compptr)
**       Break a composite curve into all of its constituent curves.
**    PARAMETERS   
**       INPUT  : 
**				compptr:   pointer to composite curve to be dissolved
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_dis5_compcrv(compptr)
struct UM_compcrv_rec *compptr;
{
	int status;
	int dsegid;
	struct UM_attrdata_rec attr;
	struct UC_entitydatabag cons;
	int i;

	uu_denter(UU_MTRC, (us,"um_dis5_compcrv(%d)",compptr->key)); 


	ur_retrieve_disp_segid(compptr->key, &dsegid);
	if (dsegid >= 0) uv_delsegs(dsegid);
	for (i = 0; i < compptr->no_cid; i++)
	{
		cons.key = compptr->cid[i].crvid;
		uc_retrieve_data(&cons, sizeof(cons)); 
/*
.....Get the attribute bundle for the entity. JLS 10/21/99
*/
		ncl_init_attr_rec(cons.key,&attr,cons.rel_num);
/*
.....Create the entity.  This will give the entity
.....a name and the appropriate attributes.
*/
		um_create_geom(&cons,UM_DEFAULT_TF, &attr);
/*
.....Make sure that it is displayable.
*/
		ur_update_displayable(cons.key, UM_DISPLAYABLE); 
/*
.....Display the entity.
*/
		uc_display(&cons); 
	}
/*
.....Delete the composite curve.
*/
	uc_delete(compptr->key);
	status = UU_SUCCESS;

	done:
	uu_dexitstatus("um_dis5_compcrv", status);
	return (status);
}
/*********************************************************************
**    E_FUNCTION :  int um_tr5_trancompcrv(eptr, offset)
**       Translates a composite curve by translating all the composite's 
**			subcurves.
**    PARAMETERS   
**       INPUT  : 
**          eptr		pointer to composite curve entity; the composite curve
**							record has been obtained.
**				offset	vector specifying the offset to translate the curve.
**       OUTPUT : 	none. 
**
**    RETURNS      : none
**    SIDE EFFECTS : updates UNIBASE with translation for each subcurve.
**    WARNINGS     : none
*********************************************************************/
um_tr5_trancompcrv(eptr, offset)
	struct UM_compcrv_rec *eptr;
	UM_vector offset;

	{
	struct UC_entitydatabag subcrv;
	UM_transf tfmat;
	UM_transf transmat;
	int i;

	uu_denter(UU_MTRC,(us,"um_tr5_trancompcrv(key:%d, %x)",
						eptr->key,  offset));

	um_disptf(offset, transmat);
  	for (i=0; i<eptr->no_cid; i++) 
  		{
     	subcrv.key = eptr->cid[i].crvid;
		uc_retrieve_data(&subcrv, sizeof(subcrv));
		uc_retrieve_transf(subcrv.key, tfmat);
		um_tftmtf(tfmat, transmat, tfmat);
     	uc_transform(&subcrv, tfmat, UU_TRUE);
		}
	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION :  int um_tf5_tranfcompcrv(eptr, tfmat, store)
**			Transform a composite by the given 4X3 transformation and
**			store in UNIBASE iff store is true.
**    PARAMETERS   
**       INPUT  : 
**      		eptr		pointer to the composite curve.
**				tfmat		transformation matrix.
**				store		TRUE iff transformed entity is to be put in UNIBASE.
**       OUTPUT :  none.
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_tf5_tranfcompcrv(eptr,tfmat, store)
	struct UM_compcrv_rec *eptr;
	UM_transf tfmat;
	UU_LOGICAL store;
{
	struct NCL_fixed_databag subcrv;
	UM_transf subcrv_tfmat;
	UU_REAL length;
	int i;

	uu_denter(UU_MTRC,(us,"um_tf5_tranfcompcrv(eptr->key:%d,tfmat:%x,store:%d)",
		eptr->key,tfmat,store));
	length = 0;
	for (i=0; i<eptr->no_cid; i++)
	{
  		subcrv.key = eptr->cid[i].crvid;
/*
... vp 12/4/97 this retrieve is safe (removed uc_retrieve_data)
*/
 		ncl_retrieve_data_fixed (&subcrv);
		uc_retrieve_transf (subcrv.key, subcrv_tfmat);
		um_tftmtf (subcrv_tfmat, tfmat, subcrv_tfmat);
     	uc_transform (&subcrv, subcrv_tfmat, store);
		length = length + um_getarclen(&subcrv, UM_DEFAULT_TF);
	}
	eptr->arclen = length;
	if (store && eptr->arclen != length)
		ur_update_data_fixed (eptr);
	uu_dexit;
	return (UU_SUCCESS);
}
/*********************************************************************
**    E_FUNCTION : int um_cp5_copycompcrv(eptr1, eptr2, bagsize)
**       This function copies a composite curve.  Note all subcurves 
**			are copied as well.
**    PARAMETERS   
**       INPUT  : 
**          eptr1		pointer to the composite curve entity to be copied.
**				bagsize	size of storage for composite entity.
**       OUTPUT :  
**          eptr2		pointer to the copy of the composite curve.
**    RETURNS      : none
**    SIDE EFFECTS : puts copy in UNIBASE.
**    WARNINGS     : none
*********************************************************************/
int
um_cp5_copycompcrv(eptr1, eptr2, bagsize)
	struct UM_compcrv_rec *eptr1;
	struct UM_compcrv_rec *eptr2;
	int bagsize;

	{
	struct UM_attrdata_rec attrbag;
	struct UM_crvdatabag *e1temp;	
	struct UM_crvdatabag *e2temp;
	struct UM_cid_rec cidi;
	int i, isze, status;
	UM_int2 i2;

	uu_denter(UU_MTRC,(us,"um_cp5_copycompcrv(%d, %x, %d)", 
						eptr1->key, eptr2, bagsize));

	NCL_copy_compcrv = UU_TRUE;

	isze = sizeof (struct UM_crvdatabag);
	e1temp = (struct UM_crvdatabag *) uu_malloc(isze);
	e2temp = (struct UM_crvdatabag *) uu_malloc(isze);
/*
..... Eduard 11/3/99. Changed the type of e1temp, e2temp from
..... UC_entitydatabag (size=25008) to NCL_fixed_databag (size=800),
..... by Ian's advise. It might cause some problems of the same type 
..... as this routine had, which then similar changes should fix.
*/

	ur_setup_data(eptr1->rel_num, eptr2, bagsize);
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (eptr2->label, "");
	eptr2->subscr = 0;
	eptr2->arclen = eptr1->arclen;
	eptr2->planar = eptr1->planar;
	eptr2->open = eptr1->open;
	eptr2->continuity = eptr1->continuity;
	eptr2->fcolor = eptr1->fcolor;
	eptr2->t0 = eptr1->t0;
	eptr2->t1 = eptr1->t1;
	eptr2->addflg = eptr1->addflg;
/*
.....initialize labloc to avoid UMR in purify
*/
   for (i=0; i<3; i++)
      eptr2->labloc[i] = eptr1->labloc[i];

	eptr2->closdinu = eptr1->closdinu;
	uc_retrieve_attr(eptr1->key, &attrbag);
	status = uc_create_data(eptr2, UM_DEFAULT_TF, &attrbag); 
	if (status != UU_SUCCESS) return (UU_FAILURE);
	i2 = 1;
	stunlb (&i2);

	for (i=0; i<eptr1->no_cid; i++) 
	{
		e1temp->key = eptr1->cid[i].crvid;
		status = ncl_retrieve_data_fixed(e1temp);
		if (status != UU_SUCCESS) return (UU_FAILURE);
		status = uc_copy(e1temp, e2temp, isze);
		if (status != UU_SUCCESS) return (UU_FAILURE);
		ur_update_displayable(e2temp->key, UM_NEVERDISPLAYABLE);
/*
..... Instead of updating the structure, which has a fixed
..... size, we update the varlist stored in unibase.

		eptr2->cid[i].crvid = e2temp->key;
		eptr2->cid[i].reverse = eptr1->cid[i].reverse;
		eptr2->cid[i].endparam = eptr1->cid[i].endparam;
*/
		cidi.crvid = e2temp->key;
		cidi.reverse = eptr1->cid[i].reverse;
		cidi.endparam = eptr1->cid[i].endparam;
		status = ur_update_data_varlist(eptr2->key,1,&cidi,i+1,1);
		if (status != UU_SUCCESS) return (UU_FAILURE);
	}                                      

	stunlb (&i2);
	NCL_copy_compcrv = UU_FALSE;
	uu_free(e1temp);
	uu_free(e2temp);
	status = ncl_retrieve_data_fixed(eptr2);

	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION :  um_fll5_fillcomp(ptr, tfmat, attrptr)
**       draws a composite curve as a single (filled) polygon
**    PARAMETERS   
**       INPUT  : 
**          ptr	composite curve entity
**				tfmat	
**				attrptr
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : This is optimized for smoothness, not speed.
*********************************************************************/

int um_fll5_fillcomp(eptr, tfmat, attrptr)
	struct UM_compcrv_rec	*eptr;
	UM_transf	tfmat;
	struct	UM_attrdata_rec	*attrptr;

	{
	struct	UC_entitydatabag	e;
	UM_transf subcrv_tfmat;
	int	numlines = 0;	/* number of line segments in composite curve	*/
	int	non_lines;		/* number of other subcurves							*/
	int	rel_num;			/* holder for relation numbers						*/
	int	segs_per_crv;	/* segments which can be spared for each non-line	*/
	UU_REAL	u;				/* parameter for sub-curves							*/
	UU_REAL	increment;	/* parameter change for sub-curves					*/
	UU_REAL	inc;			/* +/- increment											*/
	int	numsegs;			/* total number of chords actually used			*/
	Gwpoint3	*gpt;			/* dynamic array of polyfill vertices				*/
	int	gindex = 0;
	int	i;
	int	j;
	UU_REAL	temppt[3];
	struct UM_evcrvout	evout;	/* evaluation record for subcurves	*/

	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"um_fll5_fillcomp()"));

	/*	--	count the line segments	--	*/
	for (i=0; i<eptr->no_cid; i++)
		{
		ur_retrieve_data_relnum(eptr->cid[i].crvid, &rel_num);
		if (rel_num == UM_LINE_REL)
			++numlines;
		}

	/*	--	figure out how many segments can be spared for 
	 * non-lines, and how many per non-line are to be used
	 *	expressed as a part of the evaluator range [0,1].
	 */
		/* integer truncation division	*/
	if (non_lines = (eptr->no_cid  - numlines) )
		{
		segs_per_crv = UM_MIN((UM_MAXFILLSEGS - numlines )/non_lines, UM_MAXSPC);
		increment = 1.0/segs_per_crv;
		numsegs = numlines + segs_per_crv * (eptr->no_cid - numlines);
		}
	else
		{
		numsegs = numlines;
		}

	gpt = (Gwpoint3 *) uu_malloc((numsegs+1) * (sizeof (Gwpoint3)) );

	/*
	 * load each curve's sample points into array for gpolyfill3().
	 *	lines are loaded by endpoints, curves are via evaluators.
	 */
	for (i=0; i<eptr->no_cid; i++)
		{
		e.key = eptr->cid[i].crvid;

		uc_retrieve_data(&e, sizeof(e));
		uc_retrieve_transf(e.key, subcrv_tfmat);
		um_tftmtf(subcrv_tfmat, tfmat, subcrv_tfmat);

		if (e.rel_num == UM_LINE_REL)
			{
			if (eptr->cid[i].reverse)
				um_vctmtf( ((struct UM_line_rec *) &e)->ept, subcrv_tfmat, temppt);
			else
				um_vctmtf( ((struct UM_line_rec *) &e)->spt, subcrv_tfmat, temppt);

			gpt[gindex].x = temppt[0];
			gpt[gindex].y = temppt[1];
			gpt[gindex].z = temppt[2];
			gindex++;
			}
		else	/* not a line segment	*/
			{
			if (eptr->cid[i].reverse)
				{
				inc = -increment;
				u = 1;
				}
			else	/* work forward	*/
				{
				inc = increment;
				u = 0;
				}

			uc_init_evcrvout(&e, &evout);
			for (j = 0; j < segs_per_crv; j++)
				{
				uc_evcrv(UM_POINT, u, &e, subcrv_tfmat, &evout);
				u += inc;

				gpt[gindex].x = evout.cp[0];
				gpt[gindex].y = evout.cp[1];
				gpt[gindex].z = evout.cp[2];

				gindex++;
				}
			}
		}

		/* last point = first	*/
	gpt[gindex].x = gpt[0].x;
	gpt[gindex].y = gpt[0].y;
	gpt[gindex].z = gpt[0].z;

	gsfillcolor(eptr->fcolor);
	gfillarea3(numsegs, gpt);

		/* draw boundary	*/
	um_set_disp_attr(attrptr);
	gpolyline3(numsegs+1, gpt);

done:
	uu_free(gpt);
	uu_dexit;
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : um_c5_mergecrv(numkeys, keys, comp)
**			The entities to use in making the composite curve are
**			specified by UNIBASE keys. The curves are assumed to be
**			in order in the array KEY. A composite curve (COMP) is
**			created in UNIBASE while the constituent curves (KEYS)
**			are marked in UNIBASE as never displayable and the
**			current display segment are deleted.
**    PARAMETERS   
**       INPUT  : 
**          numpicks				number of keys in array
**				keys					array of keys
**       OUTPUT :  
**          comp					composite curve
**    RETURNS      : stat
**				stat = -1, iff procedure cannot complete
**						  0, otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c5_mergecrv(numkeys, keys, comp)
	int numkeys; 		          	  /* Number of subcurves picked */
	UU_KEY_ID keys[];							  /* id's of picked subcurves */
	struct UM_compcrv_rec *comp;	  /* new composite entity */  
	{
	struct UC_entitydatabag cons;   /* constituent entities */
	struct UC_attributedatabag attr; /* attribute data */
	UM_transf tfmat,tfmat2;		  /* transformation matrix */
	struct UM_point_rec pt;		  /* point of discontinuity */
	UM_coord   fpt;              /* first point of composite */
	UM_coord   lpt;              /* current end point of composite */
	int	  dsegid;              /* display segment ID */
	int     i;                   /* index */
	int 	  status;				  /* status of append procedure */
	int 	  stat;					  /* status of this procedure */
	int	  ncrv;					  /* curve number */
	UU_LOGICAL *del=UU_NULL;     /* to mark comp crvs for deletion */
	int dim;							  /* dimension of space defined by comp. curve */
	UU_REAL space[2][3];			  /* definition of the space defined by the comp.
											  curve */
	UU_LOGICAL um_cceqcc();
	struct UC_entitydatabag *e1temp;	/* temp storage for creating dup sub-entities */
	UU_KEY_ID *nkeys;
/*	int bskey,bskey1; */
	int nocid;
	UM_int2 lfl_77;
	struct UM_uvcvonsf_rec *uvcv;

	UU_LIST key_list,copy_list; /*,dbg_list;*/
	int ix,type,j,rel;
	UM_int2 ifl;
	UU_LOGICAL closed;
	UU_KEY_ID *lkeys,*copy;
	UU_LOGICAL connected;
	int tnumkeys;
	struct UC_entitydatabag tcrv,tcomp,*dbgs;
	struct UM_compcrv_rec *ccv;
	int count = 0,ind;
	UM_coord spt,ept,spt2,ept2;
	UU_REAL d11,d12,d21,d22,ttol = 1.e-3;
	UU_LOGICAL fwd;
/*
.....Check if curve is connected.
*/
	lkeys = UU_NULL;
	ix = 396; getifl(&ix,&ifl);
	if (ifl > 0)
		connected = um_check_connected(numkeys,keys,&closed);
	else
	{
		connected = UU_TRUE;
		closed = UU_FALSE;
	}
/*
.....Skip doing any more work if the components will not be connected.
*/
	if (!connected && ifl == 0)
		return (-1);

	uu_denter( UU_MTRC, (us, "um_c5_mergecrv()"));

	NCL_create_compcrv = UU_TRUE;
	stat = 0;
	e1temp = (struct UC_entitydatabag *)uu_malloc(sizeof(struct UC_entitydatabag));

	if (numkeys > 0)
		{
		uu_list_init(&copy_list,sizeof(UU_KEY_ID),numkeys,10);
/*		uu_list_init(&dbg_list,sizeof(struct UC_entitydatabag),numkeys,1);*/
		for (i=0;i<numkeys;i++)
		{
			ur_retrieve_data_relnum(keys[i], &rel);
			tcrv.key = keys[i];
			ncl_retrieve_data_fixed(&tcrv);
			ccv = (struct UM_compcrv_rec *)&tcrv;
			if (rel == UM_COMPCRV_REL)
			{
/*
.....Determine whether to flip the component order so end points match
.....when the new curve is merged - ASF 9/24/13.
*/
				if (i == 0 && numkeys == 1) fwd = UU_TRUE;
				else 
				{
					if (i == numkeys-1) ind = i - 1;
					else ind = i + 1;
					tcomp.key = keys[ind];
					ncl_retrieve_data_fixed(&tcomp);
					uc_retrieve_transf(tcrv.key, tfmat);
					uc_retrieve_transf(tcomp.key, tfmat2);
					um_get_endpts(&tcrv,tfmat,spt,ept);
					um_get_endpts(&tcomp,tfmat2,spt2,ept2);
					d11 = um_dcccc(spt,spt2); d12 = um_dcccc(spt,ept2);
					d21 = um_dcccc(ept,spt2); d22 = um_dcccc(ept,ept2);
/*
.....Find closest end points and use the forward direction if
.....the end point of the current entity is closest to the entity
.....it is connected to.  Note that this will be a little different for
.....the last component.
*/
					if (i == numkeys - 1)
					{
						if ((d11 < d12 && d11 < d21 && d11 < d22) || 
							(d12 < d11 && d12 < d21 && d12 < d22))
							fwd = UU_TRUE;
						else
							fwd = UU_FALSE;
					}
					else
					{
						if ((d21 < d11 && d21 < d12) || (d22 < d11 && d22 < d12))
							fwd = UU_TRUE;
						else
							fwd = UU_FALSE;
					}
				}
				if (fwd) ind = 0;
				else ind = ccv->no_cid-1;
				for (j=0;j<ccv->no_cid;j++)
				{
					if (ccv->cid[ind].endparam < ccv->t0)
					{
						if (fwd) goto skip;
						else break;
					}
					else if (ccv->cid[ind].endparam < ccv->t0 && !fwd) break;
					tcomp.key = ccv->cid[ind].crvid;
					ncl_retrieve_data_fixed(&tcomp);
					uu_list_push(&copy_list,&ccv->cid[ind].crvid);
/*					uu_list_push(&dbg_list,&tcomp);*/
					uc_retrieve_transf(tcomp.key,tfmat);
					um_c5_trimpart(&tcomp,ccv->cid,ccv->no_cid,ccv->t0,
						ccv->t1,ind,tfmat);
					count++;
					if (ccv->cid[ind].endparam > ccv->t1)
					{
						if (fwd) break;
						else goto skip;
					}
skip:
					if (fwd) ind++;
					else ind--;
				}
			}
			else
			{
				uu_list_push(&copy_list,&keys[i]);
/*				uu_list_push(&dbg_list,&tcrv);*/
				count++;
			}
		}
		numkeys = count;
		/* create duplicate keys and put in nkeys */
		nkeys = (UU_KEY_ID *)uu_malloc(sizeof(UU_KEY_ID) * numkeys);
		for (i = 0; i<numkeys; i++)
			nkeys[i] = 0;	/* initialize in case of errors */

/* 
... create duplicate geometry for composite curve 
... aak 02-dec-1997: if there is one CVonSF, all other curves
... must also be CVonSF; don't allow the mixture of a CVonSF with other types
... aak 07-apr-1998: don't allow a mixture of CVonSF's with different base 
... surfaces
c.....Allow CVonSF's from multiple surfaces - Bobby 09/29/14
*/
		lfl_77 = 1;
		stunlb (&lfl_77);
/*		bskey = 0;*/
		nocid = numkeys;
		copy = (UU_KEY_ID *)UU_LIST_ARRAY(&copy_list);
		for (i = 0; i<numkeys; i++)
		{
			e1temp->key = copy[i];
			cons.key = 0;
			uc_retrieve_data(e1temp, sizeof(struct UC_entitydatabag));
/*
			bskey1 = 0;
			if(ncl_itsa_uvcv_onsf(e1temp)) 
			{
					uvcv = (struct UM_uvcvonsf_rec *)e1temp;
					bskey1 = uvcv->bskey;
					if (i==0) bskey = uvcv->bskey;					
			}
*/
			if (comp->key < 1 && e1temp->rel_num == UM_COMPCRV_REL)
			{
				struct UM_compcrv_rec *ptr;
				ptr = (struct UM_compcrv_rec *) e1temp;
				nocid += (ptr->no_cid - 1);
			}

/*
			if (bskey != bskey1)
			{
				uu_uerror1(/*illegal  entity for composite curve %d//UM_MODEL,40,
				cons.rel_num);
				goto failure;
			}
*/
/*
.....If CVonSF and label is blank
.....then this is already an unassociated curve
.....so use this entity without creating a new entity
.....Bobby - 10/15/14
*/
			status = UU_SUCCESS;
			uvcv = (struct UM_uvcvonsf_rec *)e1temp;
			if (uvcv->rel_num == UM_UVCVONSF_REL && uvcv->label[0] == '\0')
				uu_move_byte(e1temp,&cons,sizeof(struct UM_uvcvonsf_rec));
			else
				status = uc_copy(e1temp, &cons, sizeof(struct UC_entitydatabag));
			if (status != UU_SUCCESS) goto failure;
/*
.....Update transformation.
*/
			uc_retrieve_transf(e1temp->key, tfmat);
			if (!um_is_idmat(tfmat))
			{
				if (um_update_transformation(cons.key, tfmat) != UU_SUCCESS)
					goto failure;
			}

			nkeys[i] = cons.key;
/* 
... RAH: force labels to be off 
*/
			ncl_set_label_attr(UU_FALSE, cons.key);
		}
/*
.....Connect the components if requested
*/
		uu_list_init(&key_list,sizeof(UU_KEY_ID),numkeys,5);
		if (!connected || (!closed && ifl != 0))
		{
			type = ifl;
			tnumkeys = numkeys;
			um_c5_connect_comp(&tnumkeys,nkeys,&key_list,type);
			lkeys = (UU_KEY_ID *)UU_LIST_ARRAY(&key_list);
			nocid += (tnumkeys-numkeys);
			numkeys = tnumkeys;
		}
		else
		{
			lkeys = (UU_KEY_ID *)UU_LIST_ARRAY(&key_list);
			for (i=0;i<numkeys;i++) lkeys[i] = nkeys[i];
		}

		stunlb (&lfl_77);
/* 
... RAH: for regenerating from source/redefine, ... preserves label 
*/
		if (comp->key < 1)
		{
			ur_setup_data(UM_COMPCRV_REL, comp, sizeof(struct UM_compcrv_rec));
			/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
			strcpy (comp->label, "");
			comp->subscr = 0;
/*
..... allocate memory for the list of components
*/
			comp->cid = (struct UM_cid_rec *) 
				uu_malloc(sizeof(struct UM_cid_rec) * nocid);
		}

		comp->closdinu = 0;
		comp->arclen = 0.0;
		comp->no_cid = 0;
		del = (UU_LOGICAL *)uu_malloc(numkeys*sizeof(UU_LOGICAL));
		i = -1;
		ncrv = 1;

		while (ncrv <= numkeys)
			{
			i++;
			cons.key = lkeys[ncrv - 1];
			ncrv++;
			uc_retrieve_data(&cons, sizeof(cons));
			uc_retrieve_transf(cons.key, tfmat);
			if (uc_super_class(cons.rel_num) == UM_CURVE_CLASS)
				del[i] = (cons.rel_num == UM_COMPCRV_REL);
			else
				{
				uu_uerror1(/*illegal  entity for composite curve %d*/UM_MODEL,40,
					cons.rel_num);
				goto failure;
				}
			status = umi_appcompcrv(comp, UM_idmat, &cons, tfmat, fpt, lpt) ;
			if (!status)
				{   
				uu_uerror0(/*ERROR - curves not connected*/UM_MODEL,37);
				um_vctovc(lpt,pt.pt);
				/* RAH: */
				/* pt.key is UU_LOGICAL - can't hold negative number ! */
				/* pt.key = -1; */
				pt.key = 0;
				pt.rel_num = UM_POINT_REL;
				pt.markertype = UM_ptattr.markertype;
				um_current_default_attr(UM_POINT_REL, &attr);
				uv_draw_allvp(&pt, UM_idmat, &attr);
				goto failure;
				}
			}/* endwhile */

		/* delete constituent curve display segments */
		i = -1;
		ncrv = 1;
		while(ncrv <= numkeys)
			{
			cons.key = lkeys[ncrv - 1];
			ur_retrieve_disp_segid(cons.key, &dsegid);
			if (dsegid >= 0) uv_delsegs(dsegid);                                 
			ur_update_disp_segid(cons.key, -1);
			ur_update_displayable(cons.key, UM_NEVERDISPLAYABLE);
			i++;
			ncrv++;
			if (del[i]) ur_delete_all(cons.key);
	  		}

		comp->t0 = 0.;
		comp->t1 = 1.;
		comp->addflg = 0;
		umi_fix_subcurve_fields(comp, tfmat);
		comp->planar = UU_FALSE;
		uc_span_entity(comp, &dim, space);
		if (dim == 2) comp->planar = UU_TRUE;  /* create composite geometry */
		comp->continuity = 0;
		comp->fcolor = UM_BACKGROUND;

		um_tftotf(UM_idmat, tfmat);
		um_get_endpts(comp, tfmat, fpt, lpt);
		comp->open = !um_cceqcc(fpt, lpt);
		}

    goto done;

failure:;
	if (lkeys != UU_NULL)
	{
		for (i = 0; i < numkeys; i++)
			if (lkeys[i] > 0)
				ur_delete_all(lkeys[i]);
	}
	stat = -1;

done:;
/*
	dbgs = (struct UC_entitydatabag *)UU_LIST_ARRAY(&dbg_list);
	for (i=0;i<dbg_list.cur_cnt;i++)
		ur_update_data_fixed(&dbgs[i]);
	uu_list_free(&dbg_list);
*/
	NCL_create_compcrv = UU_FALSE;
	uu_free(nkeys);
	uu_free(e1temp);
	if (lkeys != UU_NULL) uu_list_free(&key_list);
	if (del) uu_free(del);
	uu_dexit;
	return(stat);
	}
/*********************************************************************
**    E_FUNCTION     : int um_cctou_compcrv(eptr)
**			Determine the parameter value (U) of the composite curve 
**			which corresponds to the given coordinat (CC).
**    PARAMETERS   
**       INPUT  : 
**          eptr						pointer to compcrv
**          tfmat						transformation matrix
**				cc							coordintate on curve
**       OUTPUT :  
**          u							parameter value at cc
**				dist						error estimate
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_cctou_compcrv(eptr, tfmat, cc, u, dist)
	struct UM_compcrv_rec *eptr;
	UM_transf tfmat;
	UM_coord cc;
	UM_param *u;
	UM_length *dist;

	{
	int status;
	int i;
	struct UC_entitydatabag cons;
	UU_LOGICAL inside = UU_FALSE,found = UU_FALSE;
	UM_param cu, cu0, cu1, t0, t1;
	UM_length cdist;


	uu_denter(UU_MTRC,(us,"um_cctou_compcrv(key=%d)", eptr->key));

	cu0 = 0.;
	t0 = eptr->t0;
	t1 = eptr->t1;
	for (i=0; i<eptr->no_cid; i++)
		{
		cu1 = eptr->cid[i].endparam;
		cons.key = eptr->cid [i].crvid;
		status = uc_retrieve_data(&cons, sizeof(cons));
		if (status == UU_SUCCESS)
			{
			status = uc_cctou(&cons, tfmat, cc, &cu, &cdist);
			if (status == UU_SUCCESS)
				{
				if ((!found) || (cdist < *dist) ||
					(fabs(*dist-cdist)<UM_DFUZZ && 0. < cu && cu < 1.))
					{
					*dist = cdist;
					if (eptr->cid[i].reverse)
						*u = cu1 - ((cu1 - cu0) * cu);
					else
						*u = cu0 + ((cu1 - cu0) * cu);
					found = UU_TRUE;
					}
				}
			cu0 = cu1;
			}
		}

	if (found) status = UU_SUCCESS;

	uu_dexitstatus("um_cctou_compcrv", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION : int um_cp5_copycompcrv1(eptr1,isubid,isubid1,idir,eptr2)
**       This function copies a composite curve.  Note all subcurves 
**			are copied as well. Components are reversed if necessary.
**    PARAMETERS   
**       INPUT  : 
**			eptr1		: pointer to the composite curve entity to be copied.
**			isubid		: start id of the sub_curve to be copied.
**			isubid1		: end id of the sub_curve to be copied.
**			idir		: CLW/CCLW direction from start id isubid to end id subid1
**       OUTPUT :  
**			eptr2		: pointer to the copy of the composite curve.
**    RETURNS      : none
**    SIDE EFFECTS : puts copy in UNIBASE.
**    WARNINGS     : none
*********************************************************************/
int um_cp5_copycompcrv1(eptr1,isubid,isubid1,idir,eptr2)
struct UM_compcrv_rec *eptr1,*eptr2;
int isubid,isubid1,idir;
{
	struct UM_attrdata_rec attrbag;
	struct NCL_fixed_databag *e1temp;	
	struct NCL_fixed_databag *e2temp;
	struct UM_cid_rec cidi;
	int i, i3,isze, status;
	UM_int2 i2;
	UM_transf tfmat;
	UM_coord spt,ept,lpt,pt;
	int rev;

	uu_denter(UU_MTRC,(us,"um_cp5_copycompcrv1(%x, %x)", eptr1, eptr2));

	NCL_copy_compcrv = UU_TRUE;

	isze = sizeof (struct NCL_fixed_databag);
	e1temp = (struct NCL_fixed_databag *) uu_malloc(isze);
	e2temp = (struct NCL_fixed_databag *) uu_malloc(isze);

	ur_setup_data(eptr1->rel_num, eptr2, isze);
	strcpy (eptr2->label, "");
	eptr2->subscr = 0;
	eptr2->arclen = eptr1->arclen;
	eptr2->planar = eptr1->planar;
	eptr2->open = eptr1->open;
	eptr2->continuity = eptr1->continuity;
	eptr2->fcolor = eptr1->fcolor;
	eptr2->t0 = eptr1->t0;
	eptr2->t1 = eptr1->t1;
	eptr2->addflg = eptr1->addflg;

	eptr2->closdinu = eptr1->closdinu;
	uc_retrieve_attr(eptr1->key, &attrbag);
	status = uc_create_data(eptr2, UM_DEFAULT_TF, &attrbag); 
	if (status != UU_SUCCESS) return (UU_FAILURE);
	i2 = 1;
	stunlb (&i2);
	i3 = 0;

	if (isubid == 0 && isubid1 == 0 && idir == 0)
	{
		isubid = 0;
		isubid1 = eptr1->no_cid;
	}

	for (i = isubid; ; ) 
	{
		e1temp->key = eptr1->cid[i].crvid;
		status = ncl_retrieve_data_fixed(e1temp);
		if (status != UU_SUCCESS) return (UU_FAILURE);

		rev = 0;
		uc_retrieve_transf(e1temp->key, tfmat);
		um_get_endpts(e1temp,tfmat,spt,ept);
	
		if (eptr1->cid[i].reverse)
		{
			um_vctovc (spt,pt);
			um_vctovc (ept,spt);
			um_vctovc (pt,ept);
		}

		if (i > 0)
		{
			if (UM_DCCCC(spt,lpt) > 5.e-3 && UM_DCCCC(ept,lpt) < 1.e-3)
			rev = 1;
		}
		if (rev)
			um_vctovc (spt,lpt);
		else
			um_vctovc (ept,lpt);

		status = uc_copy(e1temp, e2temp, isze);
		if (status != UU_SUCCESS) return (UU_FAILURE);
		cidi.crvid = e2temp->key;
		if (rev)
			cidi.reverse = 1 - eptr1->cid[i].reverse;
		else
			cidi.reverse = eptr1->cid[i].reverse;
		cidi.endparam = eptr1->cid[i].endparam;

		if (idir == -1 && !cidi.reverse)	
			cidi.crvid = -cidi.crvid;

		status = ur_update_data_varlist(eptr2->key,1,&cidi,i3+1,1);
		if (status != UU_SUCCESS) return (UU_FAILURE);

		i3++;
		if (idir == 0)
		{
			i++;
			if (i >= isubid1) break;
		}
		else
		{			
			if (i == isubid1) break;
			i = i + idir;
			if (i == eptr1->no_cid) i = 0;
			if (i < 0) i = eptr1->no_cid-1;
		}
	}                                      

	stunlb (&i2);
	NCL_copy_compcrv = UU_FALSE;
	uu_free(e1temp);
	uu_free(e2temp);
	status = ncl_retrieve_data_fixed(eptr2);

	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION :  int um_getcrvlen(eptr,isub,isub1,idir)
**			Get the compcuve length from isub to isub1 along idir direction
**    PARAMETERS   
**       INPUT  : 
**      	eptr		: pointer to the composite curve.
**			isubid		: start id of the sub_curve.
**			isubid1		: end id of the sub_curve.
**			idir		: CLW/CCLW direction from start isubid to end isubid1
**       OUTPUT : none.
**          
**    RETURNS      : length from isubid to isubid1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL
um_getcrvlen(eptr,isubid,isubid1,idir)
	struct UM_compcrv_rec *eptr;
	int isubid, isubid1, idir;
{
	struct NCL_fixed_databag subcrv;
	UU_REAL length;
	int i;

	length = 0.0;
	for (i = isubid; ; )
	{
  		subcrv.key = eptr->cid[i].crvid;
 		ncl_retrieve_data_fixed (&subcrv);
		length = length + um_getarclen(&subcrv, UM_DEFAULT_TF);

		if (idir == 0)
		{
			i++;
			if (i >= isubid1) break;
		}
		else
		{					
			if (i == isubid1) break;
			i = i + idir;
			if (i == eptr->no_cid) i = 0;
			if (i < 0) i = eptr->no_cid-1;
		}
	}

	return (length);
}

/*********************************************************************
**    E_FUNCTION :  um_redef_compcrv(eptr)
**			Untrims a composite curve by resetting t0 and t1 to default.
**    PARAMETERS   
**       INPUT  : 
**      	  eptr - Pointer to original composite curve
**       OUTPUT : 
**         none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_redef_compcrv(eptr)
struct UM_compcrv_rec *eptr;
{
	int i,last,no_cids,status;
	UM_transf tfmat;
	UU_LIST cid_list;
	struct UM_cid_rec tcid,*cids;

	eptr->t0 = 0.;
	eptr->t1 = 1.;
	status = UU_SUCCESS;
	if (eptr->addflg > 0)
	{
		no_cids = 0;
		uu_list_init(&cid_list,sizeof(struct UM_cid_rec),eptr->no_cid,1);
		if (eptr->addflg != 2) 
		{
			i = 1;
			uc_delete(eptr->cid[0].crvid);
		}
		else i = 0;
		if (eptr->addflg != 1)
		{
			last = eptr->no_cid - 1;
			uc_delete(eptr->cid[eptr->no_cid-1].crvid);
		}
		else last = eptr->no_cid;
		for (i;i<last;i++)
		{
			tcid.crvid = eptr->cid[i].crvid;
			tcid.endparam = eptr->cid[i].endparam;
			tcid.reverse = eptr->cid[i].reverse;
			uu_list_push(&cid_list,&tcid);
			no_cids++;
		}
		cids = (struct UM_cid_rec *) UU_LIST_ARRAY (&cid_list);
		ur_delete_data_varlist(eptr->key, 1);
		status = ur_update_data_varlist (eptr->key, 1, cids, 1, no_cids);
		uu_list_free(&cid_list);
	}
	eptr->addflg = 0;
	eptr->no_cid = no_cids;
	ur_update_data_fixed(eptr);
	uc_retrieve_transf(eptr->key, tfmat);
	ncl_retrieve_data_fixed(eptr);
	um_c5_update_len(eptr,tfmat,eptr->t0,eptr->t1);
	if (um_is_curve_closed(eptr, tfmat))
		eptr->open = 0;
	else
		eptr->open = 1;
	ur_update_data_fixed(eptr);
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION :  um_c5_update_len(comptr,tfmat,t0,t1)
**			Updates the arc length of a trimmed composite curve.
**    PARAMETERS   
**       INPUT  : 
**      	  comptr - Pointer to composite curve
**         tfmat  - Transformation matrix for composite curve
**         t0     - First trim parameter
**         t1     - Second trim parameter
**       OUTPUT :
**         none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_c5_update_len(comptr,tfmat,t0,t1)
struct UM_compcrv_rec *comptr;
UM_transf tfmat;
UU_REAL t0,t1;
{
	int i,status,size;
	UU_REAL length;
   struct NCL_fixed_databag cons;
	UM_transf constfmat;			/* transformation for constituent */
	UM_transf inv_tfmat;			/* inverse transformation for composite */
	struct UM_crvdatabag tptr;

	if (fabs(t0) > UM_DFUZZ || fabs(t1-1.) > UM_DFUZZ)
	{
		comptr->arclen = 0.;
		for (i=0; i<comptr->no_cid; i++)
		{
			if (t0 > comptr->cid[i].endparam) continue;
			cons.key = abs( (int)comptr->cid[i].crvid );
			ncl_retrieve_data_fixed(&cons);
			uc_retrieve_transf(cons.key, constfmat);

			/* transform constituent entity geometry into the coordinate
				* space of the (non-transformed) composite curve
				*/
			um_inverttf(tfmat, inv_tfmat);
			um_tftmtf(constfmat, inv_tfmat, constfmat);
			size = um_curve_size (&cons);
			status = um_c5_trimpart(&cons,comptr->cid,comptr->no_cid,t0,t1,i,
				constfmat);
			tptr.key = cons.key;
			ncl_retrieve_data_fixed(&tptr);
			length = um_getarclen(&tptr, constfmat);
			comptr->arclen += length;
			ur_update_data_fixed(&cons);
			if (t1 - comptr->cid[i].endparam < UM_DFUZZ) 
				break; 
		}
	}
}
