/*********************************************************************
**    NAME         :  igidraft.c
**       CONTAINS:
**					uig_in_gnote
**					uig_in_angdim
**					uig_in_diadim
**					uig_in_leader
**					uig_in_lindim
**					uig_in_radim
**             uig_in_label(gblk,dblk,pblk,key)
**             int uig_in_gsymbol(gblk,dblk,pblk,key)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tigindrf.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:46
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "udebug.h"

extern UU_LOGICAL drw_flag;
extern int drw_ptr;
extern int MAX_PARA_REC;

/*********************************************************************
**    I_FUNCTION     :  uig_in_gnote(gblk,dblk,pblk,key)
**				Create a data base general note from an iges general note.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uig_in_gnote(gblk,dblk,pblk,key)
	struct global_rec *gblk;				/* global record sturcture */
	struct dir_rec *dblk;					/* directory record */
	struct IG_igesnote_rec *pblk;		/* iges gnote parameter record */
	UU_KEY_ID *key;
	{
	UU_REAL t[12];
	int lkey;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_in_gnote"));

	*key = 0;

	/* collect data */

	uig_get_trans(dblk->matrix_ptr,t);

	uig_map_gnote(dblk,t,pblk,&lkey);
	*key = lkey;

fexit:;
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     :  uig_in_angdim(gblk,dblk,pblk,key)
**				Create a data base angular dim from an iges angular dim.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_in_angdim(gblk,dblk,pblk,key)
	struct global_rec *gblk;				/* global record sturcture */
	struct dir_rec *dblk;					/* directory record */
	struct IG_igesangd_rec *pblk;		/* iges angdim parameter record */
	UU_KEY_ID *key;
	{

	/* temp storage for related IGES entities */

	struct dir_rec ldir;
	char *pl1, *pl2, *pw1, *pw2;
	char *note, *l1, *l2, *w1, *w2, *uu_toolmalloc();

	int irec,status,lkey;
	int l1_form,l2_form;
	UU_REAL t[12], center[3], rad;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_in_angdim"));

	*key = 0;
	note = uu_toolmalloc(MAX_PARA_REC);
	l1 = uu_toolmalloc(MAX_PARA_REC);
	l2 = uu_toolmalloc(MAX_PARA_REC);
	w1 = uu_toolmalloc(MAX_PARA_REC);
	w2 = uu_toolmalloc(MAX_PARA_REC);
	 
	/* collect data */

	uig_get_trans(dblk->matrix_ptr,t);

	center[0] = pblk->xyt[0];
	center[1] = pblk->xyt[1];
	center[2] = 0;

	rad = pblk->rad;

	irec = pblk->note_ptr;
	status = uig_get_data(irec,0,&ldir,note);    /* get associated note */
	if(status != 0) goto fexit;
	update_counts(1, irec);		/*jkd5: update translated count */

	irec = pblk->l1_ptr;
	if(irec != 0)
		{
		status = uig_get_data(irec,0,&ldir,l1);	/* get first leader line */
		if(status != 0) goto fexit;
		update_counts(1, irec);     /*jkd5: update translated count */
		if(ldir.rel_type != 214)
			{
			pl1 = 0;
			l1_form = 0;
			}
		else
			{
			l1_form = ldir.form_no;
			pl1 = l1;
			}
		}
	else
		{
		pl1 = 0;
		}

	irec = pblk->l2_ptr;
	if(irec != 0 )
		{
		status = uig_get_data(irec,0,&ldir,l2);	/* get second leader line */
		if(status != 0) goto fexit;
		update_counts(1, irec);     /*jkd5: update translated count */
		if(ldir.rel_type != 214 )
			{
			pl2 = 0;
			l2_form = 0;
			}
		else
			{
			l2_form = ldir.form_no;
			pl2 = l2;
			}
		}
	else
		{
		pl2 = 0;
		}
															/* get first witness line */

	irec = pblk->w1_ptr;
	if(irec != 0)
		{
		status = uig_get_data(irec,0,&ldir,w1);
		if(status != 0) goto fexit;
		update_counts(1, irec);     /*jkd5: update translated count */
		pw1 = w1;
		}
	else
		{
		pw1 = 0;
		}
															/* get second witness line */

	irec = pblk->w2_ptr;
	if(irec != 0)
		{
		status = uig_get_data(irec,0,&ldir,w2);
		if(status != 0) goto fexit;
		update_counts(1, irec);     /*jkd5: update translated count */
		pw2 = w2;
		}
	else
		{
		pw2 = 0;
		}

	uig_map_angdim(dblk,t,note,l1_form,pl1,l2_form,pl2,pw1,pw2,center,rad,&lkey);
	*key = lkey;

fexit:;
	uu_toolfree(note);
	uu_toolfree(l1);
	uu_toolfree(l2);
	uu_toolfree(w1);
	uu_toolfree(w2);
	uu_dexit;
	return 0;
	}

/*********************************************************************
**    I_FUNCTION     :  uig_in_diadim(gblk,dblk,pblk,key)
**				Create a data base diameter dim from an iges diameter dim.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_in_diadim(gblk,dblk,pblk,key)
	struct global_rec *gblk;				/* global record sturcture */
	struct dir_rec *dblk;					/* directory record */
	struct IG_igesdiad_rec *pblk;		/* iges diadim parameter record */
	UU_KEY_ID *key;
	{

	/* temp storage for associated IGES entities */

	struct dir_rec ldir;

	int status,l1_form,l2_form,lkey,irec;
	UU_REAL t[12];
	char *note, *l1, *l2, *uu_toolmalloc();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_in_diadim"));

	*key = 0;
	note = uu_toolmalloc(MAX_PARA_REC);
	l1 = uu_toolmalloc(MAX_PARA_REC);
	l2 = uu_toolmalloc(MAX_PARA_REC);

	/* collect data */

	uig_get_trans(dblk->matrix_ptr,t);
	irec = pblk->note_ptr;
	status = uig_get_data(irec,0,&ldir,note);    /* get associated note */
	if(status != 0) goto fexit;
	update_counts(1, irec);     /*jkd5: update translated count */
	irec = pblk->l1_ptr;
	status = uig_get_data(irec,0,&ldir,l1);	/* get first leader line */
	if(status != 0) goto fexit;
	update_counts(1, irec);     /*jkd5: update translated count */
	l1_form = ldir.form_no;
	irec = pblk->l2_ptr;
                                 /* get second if required */
	if(irec != 0)
		{
		status = uig_get_data(irec,0,&ldir,l2);
		if(status != 0) goto fexit;
		update_counts(1, irec);     /*jkd5: update translated count */
		l2_form = ldir.form_no;
		}
	else
		{
		l2_form = -1;
		}

	uig_map_diadim(dblk,t,note,l1_form,l1,l2_form,l2,&lkey);
	*key = lkey;

fexit:;
	uu_toolfree(note);
	uu_toolfree(l1);
	uu_toolfree(l2);
	uu_dexit;
	return 0;
	}

/*********************************************************************
**    I_FUNCTION     :  uig_in_leader(gblk,dblk,pblk,key)
**				Create a data base leader from an iges leader.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_in_leader(gblk,dblk,pblk,key)
	struct global_rec *gblk;				/* global record sturcture */
	struct dir_rec *dblk;					/* directory record */
	struct IG_igeslead_rec *pblk;		/* iges leader parameter record */
	UU_KEY_ID *key;
	{
	UU_KEY_ID lkey;
	UU_REAL t[12];

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_in_leader"));
		
	*key = 0;

	/* collect data */

	uig_get_trans(dblk->matrix_ptr,t);

	uig_map_leader(dblk,pblk,t,&lkey);
	*key = lkey;

fexit:;
	uu_dexit;
	return 0;
	}

/*********************************************************************
**    I_FUNCTION     :  uig_in_radim(gblk,dblk,pblk,key)
**				Create a data base radial dimension from an iges radial dimension.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_in_radim(gblk,dblk,pblk,key)
	struct global_rec *gblk;				/* global record sturcture */
	struct dir_rec *dblk;					/* directory record */
	struct IG_igesrad_rec *pblk;		/* iges radim parameter record */
	UU_KEY_ID *key;
	{

	/* temp storage for associated IGES entities */

	struct dir_rec ldir;

	int status,l_form,lkey,irec;
	UU_REAL t[12];
	char *note, *l1, *uu_toolmalloc();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_in_radim"));

	*key = 0;
	note = uu_toolmalloc(MAX_PARA_REC);
	l1 = uu_toolmalloc(MAX_PARA_REC);

	/* collect data */

	uig_get_trans(dblk->matrix_ptr,t);
	irec = pblk->note_ptr;
	status = uig_get_data(irec,0,&ldir,note);    /* get associated note */
	if(status != 0) goto fexit;
	update_counts(1, irec);     /*jkd5: update translated count */
	irec = pblk->l_ptr;
	status = uig_get_data(irec,0,&ldir,l1);			/* get leader line */
	if(status != 0) goto fexit;
	update_counts(1, irec);     /*jkd5: update translated count */
	l_form = ldir.form_no;

	uig_map_radim(dblk,t,note,l_form,l1,&lkey);
	*key = lkey;

fexit:;
	uu_toolfree(note);
	uu_toolfree(l1);
	uu_dexit;
	return 0;
	}

/*********************************************************************
**    I_FUNCTION     :  uig_in_lindim(gblk,dblk,pblk,key)
**				Create a data base linear dim from an iges linear dim.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_in_lindim(gblk,dblk,pblk,key)
	struct global_rec *gblk;				/* global record sturcture */
	struct dir_rec *dblk;					/* directory record */
	struct IG_igeslind_rec *pblk;		/* iges lindim parameter record */
	UU_KEY_ID *key;
	{

	/* temp storage for related IGES entities */

	struct dir_rec ldir;
	char *pw1;
	char *pw2;
	char *note, *l1, *l2, *w1, *w2, *uu_toolmalloc();
	char p_buff2[81];

	int irec,status,lkey;
	int l1_form,l2_form;
	UU_REAL t[12];

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_in_lindim"));

	*key = 0;
	note = uu_toolmalloc(MAX_PARA_REC);
	l1 = uu_toolmalloc(MAX_PARA_REC);
	l2 = uu_toolmalloc(MAX_PARA_REC);
	w1 = uu_toolmalloc(MAX_PARA_REC);
	w2 = uu_toolmalloc(MAX_PARA_REC);

	/* collect data */

	uig_get_trans(dblk->matrix_ptr,t);

	irec = pblk->note_ptr;
	status = uig_get_data(irec,0,&ldir,note);    /* get associated note */
	if(status != 0) goto fexit;
	update_counts(1, irec);     /*jkd5: update translated count */

    irec = pblk->l1_ptr;        /* get first leader line */
    if (irec == 0) 
	{
		sprintf(p_buff2, "            No pointer to directory entry of 1st leader line.\n");
		goto err1;
	}
    status = uig_get_data(irec,0,&ldir,l1);
    if(status != 0) 
	{
		sprintf(p_buff2, "            Couldn't get data for 1st leader line.\n");
		goto err1;
	}
    if (ldir.rel_type != GLEADER) 
	{
		sprintf (p_buff2, "            Pointer to directory entry of 1st leader line incorrect.\n");
		goto err1;
	}
    update_counts(1, irec);     /*jkd5: update translated count */
    l1_form = ldir.form_no;

    irec = pblk->l2_ptr;          /* get second leader line */
    if (irec == 0) 
    {
        sprintf(p_buff2, "            No pointer to directory entry of 2nd leader line.\n");
        goto err1;
    }
    status = uig_get_data(irec,0,&ldir,l2);
    if(status != 0)
    {
        sprintf(p_buff2, "            Couldn't get data for 2nd leader line.\n");
        goto err1;
    }
    if (ldir.rel_type != GLEADER)
    {
        sprintf (p_buff2, "            Pointer to directory entry of 2nd leader line incorrect.\n");
        goto err1;
    }
    update_counts(1, irec);     /*jkd5: update translated count */
    l2_form = ldir.form_no;

	irec = pblk->w1_ptr;        /* get first witness line */
	if(irec != 0)
		{
		status = uig_get_data(irec,0,&ldir,w1);
        if(status != 0) 
        {
            sprintf(p_buff2, "            Couldn't get data for 1st witness line.\n");
            goto err1;
        }
		update_counts(1, irec);     /*jkd5: update translated count */
		pw1 = w1;
		}
	else
		{
		pw1 = 0;
		}

	irec = pblk->w2_ptr;        /* get second witness line */
	if(irec != 0)
		{
		status = uig_get_data(irec,0,&ldir,w2);
		if(status != 0)
	    {
    	    sprintf(p_buff2, "            Couldn't get data for 2nd witness line.\n");
    	    goto err1;
    	}
		update_counts(1, irec);     /*jkd5: update translated count */
		pw2 = w2;
		}
	else
		{
		pw2 = 0;
		}

	uig_map_lindim(dblk,t,note,l1_form,l1,l2_form,l2,pw1,pw2,&lkey);
	*key = lkey;
    goto fexit;

err1:;
    sprintf(p_buff, "(DREC = %d) Invalid leader in linear dimension.\n",
      dblk->drec_num);
    uig_error (p_buff);
	uig_error (p_buff2);

fexit:;
	uu_toolfree(note);
	uu_toolfree(l1);
	uu_toolfree(l2);
	uu_toolfree(w1);
	uu_toolfree(w2);
	uu_dexit;
	return 0;
	}

/********************************************************************
**    I_FUNCTION     :  uig_in_label(gblk,dblk,pblk,key)
**				Create a data base label dim from an iges label dim.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_in_label(gblk,dblk,pblk,key)
	struct global_rec *gblk;				/* global record sturcture */
	struct dir_rec *dblk;					/* directory record */
	struct IG_igeslabl_rec *pblk;		/* iges label parameter record */
	UU_KEY_ID *key;
	{

	/* temp storage for related IGES entities */

	struct dir_rec ldir;
	char *note, *l1, *ll[10], *uu_toolmalloc();

	int i,irec,status,lkey, leader_num;
	int l1_form[10];
	UU_REAL t[12], note_t[12], leader_t[12];

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_in_label"));

	*key = 0;

	/* collect data */

	uig_get_trans(dblk->matrix_ptr,t);

	note = uu_toolmalloc(MAX_PARA_REC);
	irec = pblk->note_ptr;
	status = uig_get_data(irec,0,&ldir,note);    /* get associated note */
		update_counts(1, irec);     /*jkd5: update translated count */
	if(status != 0) goto fexit;

	uig_get_trans(ldir.matrix_ptr,note_t);

	leader_num = pblk->no_cid;
	if(leader_num > 10) leader_num = 10;
	uu_dprint(UU_MTRC,(us,"number of leaders = %d", leader_num));

	for(i=0;i<leader_num;i++)
		{
		ll[i] = uu_toolmalloc(MAX_PARA_REC);
		irec = pblk->cid[i];
		l1 = ll[i];
		status = uig_get_data(irec,0,&ldir,l1);	/* get leader lines */
		if(status != 0) goto fexit;
		l1_form[i] = ldir.form_no;
		update_counts(1, irec);
		uig_get_trans(ldir.matrix_ptr, leader_t);
		}

	uu_dprint(UU_MTRC,(us,"leader_t %g %g %g %g", leader_t[0], leader_t[1],
	leader_t[3], leader_t[4]));

	uig_map_label(dblk, t, note_t, leader_t, note,leader_num,l1_form,ll,&lkey);
	*key = lkey;
fexit:;
	uu_toolfree(note);
	for(i=0;i<leader_num;i++)
		uu_toolfree(ll[i]);

	uu_dexit;
	return 0;
	}
/********************************************************************
**    I_FUNCTION     :  uig_in_gsymbol(gblk,dblk,pblk,key)
**      Create a data base balloon label from an iges general symbol.
**    PARAMETERS
**       INPUT  :
**         gblk      - global block
**         dblk      - directory block
**         pblk      - parameter block
**       OUTPUT :
**         key       - key of entity created.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_in_gsymbol(gblk,dblk,pblk,key)
struct global_rec *gblk;         /* global record structure */
struct dir_rec *dblk;            /* directory record */
struct IG_igesgsym_rec *pblk;    /* iges general symbol parameter record */
UU_KEY_ID *key;
{
	struct dir_rec ldir;
	struct IG_igesline_rec *lptr;
	char *note, *l1, *ll[10], **pgeo, *uu_toolmalloc();
	int i,irec,status, leader_num, ngeo;
	int l1_form[10];
	UU_REAL t[12], note_t[12], leader_t[10][12], *geo_t;

	pgeo  = 0;
	geo_t = 0;
	leader_num = 0;
	*key = 0;
	status = UU_FAILURE;

	/* collect data */

	uig_get_trans(dblk->matrix_ptr,t);

	note = uu_toolmalloc(MAX_PARA_REC);
	if (!note) goto fexit;
	irec = pblk->note_ptr;
	status = uig_get_data(irec,0,&ldir,note);    /* get associated note */
	update_counts(1, irec);     /*jkd5: update translated count */
	if(status != 0) goto fexit;
	uig_get_trans(ldir.matrix_ptr,note_t);

	ngeo = pblk->no_cid;
	if (ngeo > 0)
	{
		pgeo  = (char **)uu_toolmalloc (ngeo*sizeof(*pgeo));
		if (!pgeo) goto fexit;
		geo_t = (UU_REAL *)uu_toolmalloc (12*ngeo*sizeof(UU_REAL));
		if (!geo_t) goto fexit;
		for(i=0;i<ngeo;i++) pgeo[i] = 0;
		for(i=0;i<ngeo;i++)
		{
			pgeo[i] = uu_toolmalloc(MAX_PARA_REC);
			lptr = (struct IG_igesline_rec *)pgeo[i];
			if (!lptr) goto fexit;
			irec = pblk->cid[i];
			status = uig_get_data(irec,0,&ldir,lptr);   /* get geometry */
			if(status != 0) goto fexit;
			lptr->rel_num = ldir.rel_type;
			update_counts(1, irec);
			uig_get_trans(ldir.matrix_ptr,&geo_t[12*i]);
		}
	}

	leader_num = pblk->no_cid1;
	if(leader_num > 10) leader_num = 10;
	for(i=0;i<leader_num;i++) ll[i] = 0;

	for(i=0;i<leader_num;i++)
	{
		ll[i] = uu_toolmalloc(MAX_PARA_REC);
		l1 = ll[i];
		if (!l1) goto fexit;
		irec = pblk->cid1[i];
		status = uig_get_data(irec,0,&ldir,l1);   /* get leader lines */
		if(status != 0) goto fexit;
		l1_form[i] = ldir.form_no;
		update_counts(1, irec);
		uig_get_trans(ldir.matrix_ptr, leader_t[i]);
	}

	uig_map_gsymbol(dblk, t, note_t, leader_t, note,leader_num, l1_form, ll,
                   ngeo, pgeo, geo_t, key);
	status = UU_SUCCESS;
fexit:;
	if (note) uu_toolfree(note);
	for(i=0;i<leader_num;i++)
		if (ll[i]) uu_toolfree(ll[i]);
	if (pgeo)
	{
		for(i=0;i<ngeo;i++)
			if (pgeo[i]) uu_toolfree(pgeo[i]);
		uu_toolfree(pgeo);
	}
	if (geo_t) uu_toolfree(geo_t);

	return (status);
}
