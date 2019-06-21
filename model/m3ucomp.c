/*********************************************************************
**    NAME         :  m3ucomp.c
**       CONTAINS: user interface routines for manipulating composite
**							curves
**			umu_spl5_compcrv()
**			umu_dis5_compcrv()
**			umu_c5_mergecrv()
**			umu_c5_string()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3ucomp.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:58
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dmark.h"
#include "class.h"
#include "dselmask.h"
#include "mdrel.h"
#include "mcrv.h"
#include "mdpick.h"
#include "mdebug.h"
/*
.....Added for composite curve connection type option
*/
#include "mfort.h"
#include "nclinp.h"
/*
....define FORTRAN call crvcls() to determine if curve is closed.
*/
#include "nclfc.h"

UU_LOGICAL	ud_gnxt();
extern UU_LOGICAL NCL_create_compcrv;

/*********************************************************************
**    E_FUNCTION     : umu_spl5_compcrv()
**       Break a composite curve into two new composite curves.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_spl5_compcrv()
	{
	UM_PICKENT	pent;					/* pick path to picked entity */
   UU_KEY_ID   compid;				/* Composite curve ID */
   UU_KEY_ID   consid;				/* Constituent curve ID */
   struct UM_compcrv_rec comp;	/* Old composite curve */
	UM_transf tfmat;					/* transform for composite curve */
   struct UM_compcrv_rec comp2;	/* New composite curve */
   UM_coord   spt, ept;				/* Start and end points of curves */
   int     i, j;						/* indices */
	int	dsegid;						/* display segment ID */
   int     numint;
	UU_REAL arclenratio;

	uu_denter(UU_MTRC,(us,"umu_spl5_compcrv()"));
	um_dl_pdas(UD_DASPICK, /*Pick composite curve to split: */ 
						UM_MODEL, 132, &pent, 1, &numint, 1);
	if (numint != 0)
		{
		compid = um_get_pickkey(&pent, 1);
		um_dl_pdas(UD_DASPICK, /*Pick first constituent of second composite: */ 
					UM_MODEL,133, &pent, 1, &numint, 2);
		if (numint != 0)
			{
			consid = um_get_pickkey(&pent, 2);
			comp.key = compid;
			ur_retrieve_data_relnum(comp.key, &comp.rel_num);
			if (comp.rel_num !=  UM_COMPCRV_REL)
				uu_uerror0(/*must pick composite curve*/UM_MODEL,118);
			else
				{
				uc_retrieve_data(&comp, sizeof(comp));
				uc_retrieve_transf(comp.key, tfmat);
				for (i=0; i<comp.no_cid && comp.cid[i].crvid != consid; i++)  ;
				um_p5_compcrv(&comp);
				sprintf(UM_sbuf,"consid=%d, i=%d", consid, i);
				um_pscroll(UM_sbuf);
				if (comp.no_cid == 1)
					uu_uerror0(/*ERROR - can't split a composite with single curve*/
									UM_MODEL,126);
				else if (i == 0)
					uu_uerror0(/*ERROR-can't split a composite at first constituent*/
									UM_MODEL,127);
				else if (comp.cid[i].crvid != consid)
					uu_uerror0(/*ERROR - curve not in composite*/UM_MODEL,38);
				else
					{
					ur_retrieve_disp_segid(comp.key, &dsegid);
					if (dsegid >= 0) uv_delsegs(dsegid);
					ur_update_disp_segid(comp.key, -1);

					ur_setup_data(UM_COMPCRV_REL, &comp2, sizeof(comp2));
					/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
					strcpy (comp2.label, "");
					comp2.subscr = 0;
					/* fill in new composite curve record */
					comp2.planar = comp.planar;
					comp2.continuity = comp.continuity;
					comp2.fcolor = comp.fcolor;
					comp2.no_cid = comp.no_cid - i; /* set nbr of atoms */
					comp2.arclen = comp.arclen * (1.0 - comp.cid[i-1].endparam);
					arclenratio = 1.0 / (1.0 - comp.cid[i-1].endparam);
					for (j = 0; j < comp2.no_cid; j++)
						{
						comp2.cid[j].crvid = comp.cid[i+j].crvid;
						comp2.cid[j].reverse = comp.cid[i+j].reverse;

						/* update "endparam" field for each subcurve in composite */
						comp2.cid[j].endparam = (comp.cid[i+j].endparam 
													  - comp.cid[i-1].endparam)*arclenratio;
						}

					um_get_endpts(&comp2, tfmat, spt, ept);
					comp2.open = !um_cceqcc(spt, ept);
					uc_create_data(&comp2, tfmat, UM_CURRENT_ATTR);
/*
.....................Set closdinu flag
*/
					crvcls(&comp2.key);
					uc_display(&comp2);

					/* update fields of old compcrv */
					comp.no_cid = i; /* update nbr of atoms in old compcrv */
					arclenratio = comp.arclen / (comp.arclen - comp2.arclen);
					comp.arclen = comp.arclen - comp2.arclen;

					/* update "endparam" fields of old composite */
					for (j=0; j<comp.no_cid; j++)
						comp.cid[j].endparam = comp.cid[j].endparam * arclenratio;
						
					um_get_endpts(&comp, tfmat, spt, ept);
					comp.open = !um_cceqcc(spt, ept);
					um_update_geom(&comp, UM_DEFAULT_TF);
/*
.....................Set/reset closdinu flag
*/
					crvcls(&comp.key);
					uc_display(&comp);
					}
				}
			}
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : umu_dis5_compcrv()
**       Prompt the user for the composite entity to dissolve, and call the
**       appropriate routine to dissolve it.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/* NOTE: this subroutine is obsolete.  navigation into um_dis5_compcrv() handled
		through the class table 
*/
umu_dis5_compcrv()
	{
	UM_PICKENT	pent;					/* pick path to picked entity */
	struct UM_compcrv_rec comp;	/* composite curve */  
	int     numint;

	uu_denter(UU_MTRC,(us,"umu_dis5_compcrv()"));
	ud_lgeo(UU_TRUE, UD_compcrv);
	um_dl_pdas(UD_DASPICK, /*Pick composite curve to dissolve:*/ UM_MODEL,134, 
					&pent,1,&numint,1);
	if (numint > 0)
		{
		comp.key = um_get_pickkey(&pent, 1);
/*RAH: make dissolve like delete */
/*		uc_retrieve_data(&comp, sizeof(comp));			
/*		um_dis5_compcrv(&comp);
/* */
        uc_delete(comp.key);
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : umu_c5_mergecrv()
**       Prompt the user to enter constituent curves to be merged into a
**       composite curve, and concatenate them into a single composite.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c5_mergecrv()
	{
	int numpicks;           	  /* Number of curves picked */
	int ids[UM_MAXPICK];			  /* curve ids */
	int ncrv;						  /* curve index */
	int status;						  /* status of append procedure */
	UU_LOGICAL um_initialize;	  /* initialize get next entity */
	struct UM_compcrv_rec comp;  /* new composite entity */
	int choice;						  /* connection type */
	UM_int2 ifl,choice2;			  /* connection type */
	UU_LOGICAL closefl,closed;	  /* create closed curve flags */

	uu_denter( UU_MTRC, (us, "umu_c5_mergecrv()"));

/*	-- Limit DAS to only editable entities -- */

	ud_leditable(UU_TRUE);
	ud_ldas(UD_DASSELECT, /*Pick curves to merge: */UM_MODEL, 27, 
		 			UU_NULL,  UM_MAXPICK, &numpicks, UD_NODEFAULT);

	if (numpicks > 0)
		{
		struct UM_cid_rec *cids;
		cids = UU_NULL;
		ncrv = 0;
		um_initialize = UU_TRUE;
		while (ud_gnxt(um_initialize, UU_NULL, &ids[ncrv], 1) == UU_TRUE)
			{
			ncrv++;
			um_initialize = UU_FALSE;
	  		}
		/* RAH: initialize key to zero for proper label generation. */
		comp.key = 0;
/*
.....Added option to create compcrv from disconnected entities.
.....Andrew 3/13/13
*/
		status = um_check_connected(ncrv,ids,&closed);
		if (status == UU_FALSE)
		{
			choice = 0;
			status = nclu_compcv_contype(&choice,&closefl,closed);
			if (choice < 0) choice = 0;
			ifl = 396;
			choice2 = choice+1;
			if (closefl) choice2 = choice2 * -1;
			setifl(&ifl,&choice2);
		}
		status = um_c5_mergecrv(numpicks, ids, &comp);
		cids = comp.cid;
		if (status == 0)
			{
   			uc_create_data(&comp, UM_DEFAULT_TF, UM_CURRENT_ATTR);
/*
.............Set closdinu flag
*/
			crvcls(&comp.key);
			uc_display(&comp);
			}
		if (cids) uu_free (cids);
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : umu_c5_string()
**      Create a composite curve consisting of sequential entry
**      of straight lines.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c5_string()
	{
	struct UM_compcrv_rec c;		/* composite curve */
	struct UM_line_rec l;			/* line curve */
    UD_NDCLOCREC tmp;
    int j;

	UU_KEY_ID keys[5000];			/* keys of created line segments */
	int numkeys;						/* number of keys in array */
	int numint;							/* number of DAS entries */
	int markval;						/* value indicating cmd reject */
	int i;
	int status;

	uu_denter(UU_MTRC, (us, "umu_c5_string"));

	NCL_create_compcrv = UU_TRUE;	/* so label counters won't get bumped */
	numint = 1;
	numkeys = 0;
	ur_setup_data(UM_LINE_REL, &l, sizeof(l));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (l.label, "");
	l.subscr = 0;
	UD_MARK(markval, UU_FALSE);
	if (markval == 0) /* first time return */
		{
		ud_ldas(UD_DASCART, /*enter  coordinate*/UM_MODEL, 35, &tmp, 1, 
					&numint, UD_NODEFAULT);
        for(j=0; j < 3; j++) l.spt[j] = tmp.cord[j];

		while (numint > 0) 
			{
			ud_ldas(UD_DASCART, /*enter  coordinate*/UM_MODEL, 35, &tmp, 1, 
						&numint, UD_NODEFAULT);
        for(j=0; j < 3; j++) l.ept[j] = tmp.cord[j];

			if (numint > 0)
				{
				uc_create_data(&l, UM_DEFAULT_TF, UM_CURRENT_ATTR);
				uc_display(&l);
				keys[numkeys] = l.key;
				numkeys++;
				um_vctovc(l.ept, l.spt);
				}
			}
		if (numkeys > 0)
			{
			/* RAH: force create and label generation */
			struct UM_cid_rec *cids;
			cids = UU_NULL;
			c.key = 0;
			status = um_c5_mergecrv(numkeys, keys, &c);
			cids = c.cid;
			if (status == 0)
				{
   			    uc_create_data(&c, UM_DEFAULT_TF, UM_CURRENT_ATTR);
/*
.................Set closdinu flag
*/
				crvcls(&c.key);
				/* RAH: cleanup since um_c5_mergecrv() creates its own sub-geometry */
				for (i=0; i<numkeys; i++) 
					uc_delete(keys[i]);
				uc_display(&c);
				}
			if (cids) uu_free (cids);
			}
		}
	else /* cleanup for command reject option */
		{
		for (i=0; i<numkeys; i++) uc_delete(keys[i]);
		}

	UD_UNMARK(markval);
	uu_dexit;
	}
