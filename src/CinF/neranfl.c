/*********************************************************************
**    NAME         :  neranfl.c
**       CONTAINS:  routines to manipulate NCL ranfile.
**         int ncl_randel_label(label) (-- not used --)
**         ncl_ranstr(eptr)
**         ncl_post_load(loadoperation)
**         ncl_lp04()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       neranfl.c , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**       04/10/18 , 15:23:59
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "umath.h"
#include "uhep.h"
#include "class.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mfort.h"
#include "mdebug.h"

#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclver.h"

#include "ribase.h"
#include "rmtuple.h"
#include "mlab.h"
#include "nclmodals.h"
#include "dmark.h"
#include "nclvx.h"

/* Defined in ur_lp02b the lowest key of the geometries merged. */
extern UU_KEY_ID UR_low_key;
extern UU_LOGICAL ncl_where;
extern int  UR_active;
UU_LOGICAL yesno();
static UU_LOGICAL wind_stat = UU_FALSE;
UU_LOGICAL NCL_merge_overwrite; /* true if merging unibase && overwriting entities, false otherwise */

/*********************************************************************
**    E_FUNCTION     : int ncl_randel_label(labin, subscr)
**       If the specified entity label has an entry in the NCL
**         ranfile, it will be deleted.
**    PARAMETERS
**       INPUT  :
**          label               UNIBASE labin
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff ranfile entry deleted; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/* int
 ncl_randel_label(labin, subscr)
    char *labin;
    int subscr;

    {
    int status;
    int len;
    int i;
    char label[8];
    UM_f77_str ncllabel;
    UM_int2 nclsubr;
    UM_int2 nclstatus;

    uu_denter(UU_MTRC,(us,"ncl_randel_label(label=%s)", label));

    status = UU_SUCCESS;
    strcpy(label, labin);
    len = strlen(label);
    for (i=len;i<7;i++) label[i] = ' ';
    UM_init_f77_str(ncllabel, label, 6);
    nclsubr = subscr;
    randel(UM_addr_of_f77_str(ncllabel), &nclsubr, &nclstatus);
    if (nclstatus != 0)
       status = UU_FAILURE;

    uu_dexit;
    return(status);
    }
 */
/*********************************************************************
**    E_FUNCTION     : ncl_ranstr(eptr)
**        Update the NCL ranfile with a new entity name.
**        If unibase being merged check for geometry's label and if
**        already define ask for rename option and act accordingly.
**    PARAMETERS
**       INPUT  :
**          eptr            entity pointer
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_ranstr(eptr)
   struct UC_entitydatabag *eptr;

   {
   union {                  /* to force double word alignment of label */
      double junk;
      char label[NCL_MAX_LABEL];
      } ncl;
   UM_int2 ncltype;
   UM_int4 nclsubscript;
   UM_int4 nclkey;
   UM_int2 nclstatus;
   UM_f77_str ncllabel;
   struct NCL_vector_rec *e;
   struct NCL_scalar_rec *s1;
	int *isubptr;
   int i;
   int status;
   UU_LOGICAL noerr;
   UM_int2   ifl, ifnd;

   uu_denter(UU_MTRC,(us,"ncl_ranstr(key=%x relnum=%d)",
      eptr->key, eptr->rel_num));

   status = ncl_get_type (eptr->rel_num, &ncltype);
   /* wind_stat = UU_FALSE; */
   if (status == UU_SUCCESS)
       {
       if ((ncl_geom_type(eptr->rel_num)==UU_SUCCESS)||
       (wf_geom_type(eptr->rel_num)==UU_SUCCESS) ||
       (ncltype == NCLI_SCALAR || ncltype == NCLI_MATRIX ||
        ncltype == NCLI_DATAST || ncltype == NCLI_TEXTVAR)) 
           {
           e = (struct NCL_vector_rec *) eptr;
			if (strcmp (e->label, "       ") == 0)
			{
				if (ncltype == 2) ur_delete_all(e->key);
				goto done;
			}
/*
.....Geometry which makes up net surface or boundary of trimsf.
.....don't put in ranfile
*/
			if (e->label[0] == '\0') goto done;

/*
.....Geometry which makes up composite curves, among other things.
.....don't put in ranfile
*/
		   if (strncmp (e->label,"@UN",3) == 0) goto done;
/*
.....Labels starting with a '#' are drawing entities
.....Do not store in ranfile
.....Bobby  -  2/28/94
*/
			if (e->label[0] == '#') goto done;

           for (i=0; i<NCL_MAX_LABEL; i++) ncl.label[i] = e->label[i];
           if (e->rel_num == NCL_SCALAR_REL || e->rel_num == NCL_DATAST_REL ||
               e->rel_num == NCL_TEXTVAR_REL)
           {
             s1 = (struct NCL_scalar_rec *)e;
             isubptr = &s1->subscr;
           }
           else
             isubptr = &e->subscr;

           nclsubscript = *isubptr;

           UM_init_f77_str(ncllabel, ncl.label, NCL_MAX_LABEL);
           noerr = UU_TRUE;
                  /* if AUTO label is set to off check to see if label has already been
                        defined. */
           if (NCL_merge_overwrite)
				{
				chklab(UM_addr_of_f77_str(ncllabel), &nclkey, &nclsubscript,
										 &ifnd, &ifl);
				if (ifnd == 1)
					if (ifl == 0)
					{
					if (!(nclsubscript >0 && nclsubscript >= 
							UM_labelmdl.subscr[UM_labelmdl.rel[e->rel_num]]))
						{
						wind_stat = UU_TRUE;
						opnwin();
						/* Geometry label already defined and canon is off */
						uerror(UM_addr_of_f77_str(ncllabel), &nclsubscript);
						noerr = (ud_yesno(0, "Do you want this entity to be auto renamed", "Question?"));
						if (!noerr)  /* delete the entity from unibase */
                    {
/* ur_delete_all(e->key); 
...replaced by uc_delete (vp 6-mar-95)
...fake that it is secondary unibase so uc_delete will not call ncl_randel
...which is not neccessary since entity has not been stored in ran file yet  
*/
                     i = UR_active;
                     UR_active = 2;
                     uc_delete(e->key);
                     UR_active = i;
                    }
						}	
					}
					else
					{
/*
.....Entity with this name already exists in the ranfile and canon is on.
.....Call ranstr to store new entity in ranfile and delete old entiy from
.....ranfile and unibase.
*/
						ranstr (UM_addr_of_f77_str(ncllabel), &nclsubscript, &ncltype,
						        &nclkey, &nclstatus);
						noerr = UU_FALSE;
					}
				}
			/* If AUTO label is set to on or it's a new label store it in  ranfile
			   and update the unibase if rename is done. */
			if (noerr)
				{
				if ((ncl_geom_type(e->rel_num)==UU_SUCCESS) ||
					 (ncltype == NCLI_SCALAR || ncltype == NCLI_MATRIX ||
       			  ncltype == NCLI_DATAST || ncltype == NCLI_TEXTVAR)) 
					{
					ncl_label_cam(e->rel_num, e->label,
	       						&nclsubscript, e->key,&nclstatus);
					/* Update the subscript after renaming. */
      			*isubptr = nclsubscript;
					}
				else if (wf_geom_type(e->rel_num)==UU_SUCCESS)
					{
					ncl_where = UU_TRUE;
					ncl_label_wf(e->rel_num, e->label, isubptr, e->key, &nclstatus);
					ncl_where = UU_FALSE;
					}

				if (nclstatus == 1)
					ur_update_data_fixed(e);
				}
			}
			
		}
done:;
   uu_dexit;
	return (0);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_post_load(loadoperation)
**       Reinitialize the ranfile.
**    PARAMETERS
**       INPUT  :
**          loadoperation            UU_TRUE => load operation
**                                    UU_FALSE => merge operation
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_post_load(loadoperation)
   UU_LOGICAL loadoperation;

{
   UU_LOGICAL init;
   UU_LOGICAL ncl_legal_relation();
   UU_LOGICAL sav_auto_label;
   struct UC_entitydatabag e;

   uu_denter(UU_MTRC,(us,"ncl_post_load(loadoperation=%d)",
      loadoperation));

	wind_stat = UU_FALSE;

	/* If LOAD and not MERGE */
	if (loadoperation)
   		ranini();

	/* save the value of NCL_auto_label and reset it after finishing this
	   task. */
	sav_auto_label = NCL_auto_label;
   init = UU_TRUE;
   while (um_getallobjs(init, &e.key) == 0)
	{
		init = UU_FALSE;
/*
..... Update ranfile only for entity's being loaded or merged 
.....vp 2/24/98 removed UR_low_key replaced by ur_svld_test
.....which tests bit map of newly loaded tuples. Note: requires
.....reset save bits before loading/merging (ur_svld_clear)
.....vp 3/3/98 remove label table here before any atempt to
.....to retrieve of incomplete tuple (can fail).
*/
		if (loadoperation || ur_svld_test(e.key))
		{
			ur_retrieve_data_relnum(e.key,&e.rel_num);
			if (e.rel_num == NCL_LABTBL_REL) 
				ur_delete_all (e.key);
			else
			{
				um_retrieve_data_fixed(&e);
				if (e.rel_num == NCL_CURVE_REL)
				{
					ncl_fix_curve (&e);
				}
/*
......added routine to handle data with text string
*/
				if ((NCL_infile_version < 10.202)&&(e.rel_num == NCL_DATAST_REL))
				{
					ncl_fix_data (&e);
				}
				if (ncl_legal_relation(e.rel_num)) ncl_ranstr(&e);
			}
		}
	}

	if (wind_stat)
	{
		wind_stat = UU_FALSE;
		clswin();
	}
	NCL_auto_label = sav_auto_label;

	uu_dexit;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_lp04()
**       Set all dsegid's to -1 for batch.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_lp04()
	{
	int				status;		/* holds status of unibase calls */
	UU_KEY_ID		key_id;		/* a key_id tuple */
	long			tuple_indx;		/* an entry id */
	int rel;

	status = 0 ;

	tuple_indx = 1 ;
	while(tuple_indx > 0)
		{
		ur_get_next_new_tuple_index(UR_MTUPLE_REL, &tuple_indx);
		if(tuple_indx > 0)
			{
			ur_rt2k(UR_MTUPLE_REL, tuple_indx, &key_id);
			ur_update_disp_segid(key_id, -1);	/* force disp seg to none */
/*
.....Update attibutes from older unibase to support new features
*/
			ur_retrieve_data_relnum(key_id,&rel);
			if (NCL_infile_version != NCL_version)
				ur_update_attribut (key_id,rel);
			tuple_indx++;
			}
		}
	return(status);
	}

