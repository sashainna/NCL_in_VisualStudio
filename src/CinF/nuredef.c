/*********************************************************************
**    NAME         :  nuredef.c
**       CONTAINS: User interface routines for redefinition statements
**                 creation.
**
**           nclu_redef()    **** NOT USED (see m3utrim1.c) vp 27-mar-95 ****
**           nclu_redef_fillet()
**		       nclu_redef_close()
**       int nclu_put_cmd (type, e, ptio1, ptio2, nrpt)
**           predef (keys,idir,cnnam,sub,ierr)
**       int ncl_prep_trim_curve (e,tfmat,idir,ptkey,pt,uall,ptp,uend)
**       int ncl_prep_midtrim_curve (e,tfmat,idir,ptkey,pt1,pt2,uall,ptp,uend)
**       int um_nearest_isect_to_point(ptnr, nint, ibuff)
**       int um_nearest_isect_to_direc(dvec, nint, ibuff)
**       int um_verify_uvalue (crv1,tfmat,pt,uu)
**           um_set_geoname (cnnam,ksub)
**           um_reset_geoname ()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nuredef.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       08/17/15 , 18:03:42
*********************************************************************/
#include "usysdef.h"
#include "mfort.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "class.h"
#include "mdrel.h"
#include "mdpick.h"
#include "modef.h"
#include "mdcoord.h"
#include "mdeval.h"
#include "mcrv.h"
#include "misect.h"

#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"
#include "nclfc.h"
#include "nclvx.h"

char UM_RD_name[NCL_MAX_LABEL];
int UM_RD_namefl, UM_RD_subscr;
extern int NCL_ubcopy;

/*********************************************************************
**    E_FUNCTION     : nclu_redef()   **** CODE REMOVED ****
**       generate redef statements
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*********************************************************************
**
**  THIS ROUTINE HANDLES THE FOLLOWING REDEF CASES:
**    1.  REDEF/LN,LN,<mod>
**    2.  REDEF/LN/PT,<mod>
**    3.  REDEF/LN,CV,PT,<mod>
**    4.  REDEF/LN,CI,mod1,mod2
**    5.  REDEF/CI,PT,dir
**    6.  REDEF/CI,LN,mod,dir
**    7.  REDEF/CI,CI,mod,dir
**
*********************************************************************/

/*********************************************************************
**    E_FUNCTION     : nclu_redef_fillet()
**       generate redef statement for circle tanto two lines
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_redef_fillet()

   {
   NCL_cmdbuf cmdbuf;
   int status;
   int target_rel;
   int defining_rel;

   uu_denter(UU_MTRC,(us,"nclu_redef_fillet()"));

   while (UU_TRUE)
      {
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;

/*       add redef keyword to command buffer */
      if (status == NCL_OKINPUT)
         status = ncl_add_token(&cmdbuf, NCL_redef, NCL_nocomma);

/*       pick target geometry */
      if (status == NCL_OKINPUT)
         status = ncl_add_label_rel(UD_DASPCKLOC, &cmdbuf, 439, UD_ncl_curve, 
         &target_rel);

/*       pick defining geometry line 1 */
      if (status == NCL_OKINPUT)
         status = ncl_add_label_rel(UD_DASPCKLOC, &cmdbuf, 440, UD_ncl_ln,
         &defining_rel);
      if (status == NCL_DONE) break;

/*       pick defining geometry line 1 */
      if (status == NCL_OKINPUT)
         status = ncl_add_label_rel(UD_DASPCKLOC, &cmdbuf, 441, UD_ncl_ln,
         &defining_rel);
      if (status == NCL_DONE) break;

/*       choose direction modifier indicating which part of target geometry
         is to be kept */
      if (status == NCL_OKINPUT)
         status = ncl_add_modifier(&cmdbuf, NCL_REDEF_KEEP);
      if (status == NCL_NOINPUT) status = NCL_DONE;

      if ((status == NCL_OKINPUT) || (status == NCL_DONE))
         {
         ncl_add_cmdbuf(&cmdbuf);
         ncl_call(&cmdbuf);
         }

      }

   uu_dexit;
	return 0;
   }

/*********************************************************************
**    E_FUNCTION     : nclu_redef_close()
**       generate redef statements
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

/*********************************************************************
**
**  THIS ROUTINE HANDLES THE FOLLOWING REDEF CASES:
**    1.  REDEF/CV,{OPEN,CLOSE}
**    2.  REDEF/SF1,{OPEN,CLOSE},n
**
*********************************************************************/

nclu_redef_close()

   {
   NCL_cmdbuf cmdbuf;
   int status,choice;
   int target_rel;
   int tlcurv;
	static char l0[] = {"0"}, l1[] = {"1"};

   uu_denter(UU_MTRC,(us,"nclu_redef_closed()"));

	while (UU_TRUE)
	{
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;

/*
.....Add REDEF keyword to command buffer
*/
		status = ncl_add_token(&cmdbuf, NCL_redef, NCL_nocomma);
		if (status != NCL_OKINPUT) goto done;
/*
.....Pick target geometry
*/
		status = ncl_add_label_rel(UD_DASPCKLOC, &cmdbuf, 244, UD_ncl_close,
				&target_rel);
		if (status != NCL_OKINPUT) goto done;
/*
.....Determine whether a Curve or Surface was picked
*/
		tlcurv = (target_rel == UM_CONIC_REL) ||
				(target_rel == UM_COMPCRV_REL) ||
				(target_rel == UM_RBSPLCRV_REL) ||
				(target_rel == UM_AGCRV_REL) ||
				(target_rel == NCL_CURVE_REL) ||
				(target_rel == NCL_EVALCV_REL);
/*
.....A curve was picked
.....Get OPEN or CLOSE
*/
		if (tlcurv)
		{
			status = ncl_popup(NCL_CLOSE_CV,&choice);
			if (status != NCL_OKINPUT) goto done;
			switch (choice)
			{
			case 1:
				ncl_add_token(&cmdbuf, NCL_close, NCL_comma);
				break;
			case 2:
				ncl_add_token(&cmdbuf, NCL_open, NCL_comma);
				break;
			default:
				goto endloop;
			}
		}
/*
.....A surface was picked
.....Get OPEN/CLOSE in U or V
*/
		else
		{
			status = ncl_popup(NCL_CLOSE_SF,&choice);
			if (status != NCL_OKINPUT) goto done;
			switch (choice)
			{
			case 1:
				ncl_add_token(&cmdbuf, NCL_close, NCL_comma);
				ncl_add_token(&cmdbuf, l0, NCL_comma);
				break;
			case 2:
				ncl_add_token(&cmdbuf, NCL_close, NCL_comma);
				ncl_add_token(&cmdbuf, l1, NCL_comma);
				break;
			case 3:
				ncl_add_token(&cmdbuf, NCL_open, NCL_comma);
				ncl_add_token(&cmdbuf, l0, NCL_comma);
				break;
			case 4:
				ncl_add_token(&cmdbuf, NCL_open, NCL_comma);
				ncl_add_token(&cmdbuf, l1, NCL_comma);
				break;
			default:
				goto endloop;
			}
		}
/*
.....Output the REDEF command
*/
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
endloop:;
	}
done:;
	uu_dexit;
	return 0;
}

/**********************************************************************
**    E_FUNCTION     : nclu_put_cmd(type, e, ptio1, ptlab1,ptio2,ptlab2, nrpt)
**       generate redef statements for pp file after it is processed
**    PARAMETERS   
**       INPUT  : 
**          type  - 1 = trim/extend, 2 = midtrim statement
**          e     - pointer to array of curves: (0) - CV to trim,
**                  (1) - trimmer 1, (2) - trimmer 2 (midtrim), (3) -
**                  second CV when split curve (midtrim).
**          ptio1 - i/o point with the first trimer
**          ptlab1 - label associated with 'ptio1'.
**          ptio2 - i/o point with the second trimer (midtrim)
**          ptlab2 - label associated with 'ptio2'.
**          inp   - Use nrpt in statement.
**          nrpt  - pick point on intput CV. 
**          ldir  - label associated with 'nrpt'.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
int
nclu_put_cmd (type, elst, ptio1, ptlab1, ptio2, ptlab2, inp, nrpt,ldir)
  int type, inp;
	struct NCL_fixed_databag *elst[3];
  UM_coord ptio1, ptio2, nrpt;
  char *ptlab1,*ptlab2,*ldir;

 {
  UM_f77_str fcmd;
  struct NCL_fixed_databag *crv;
  NCL_cmdbuf cmdbuf;
  char label[NCL_MAX_LABEL+1],lab[NCL_MAX_LABEL+1];
  char str[80];
  char tbuf[80];
  int j,n,nc;
  UM_coord pt;
  static char ll[] = {"(PT/"}, lp[] = {")"};

  ncl_init_cmdbuf(&cmdbuf);
/*
.....Add REDEF keyword to command buffer
*/
  ncl_add_token(&cmdbuf, NCL_redef, NCL_nocomma);

  crv = (struct NCL_fixed_databag *) elst[0];
  um_vctovc (nrpt, pt);
  strcpy(lab,ldir);
  j   = 1;
/*
...Loop thru all entities (1st is CV to trim)
...dumping pairs of CV id & PT coordinates
*/
loop:;
	strncpy(label,crv->label, NCL_MAX_LABEL); 
/*
..... vp 12/4/96 make sure label is terminated.
*/
	label[NCL_MAX_LABEL] = 0; 
	if (crv->subscr > 0)
	{
		nc = strlen(label);
		ul_strip_blanks(label,&nc);
		sprintf(tbuf,"(%d)", crv->subscr);
		strcat(label,tbuf);
	}
	ncl_add_token(&cmdbuf, label, NCL_comma);
/*
...Add newly created CV1 name when Mid-Trim splits the curve
     !!! put this name in elst[3] when created in um_midtrim_curve !!!
*/
  if (type == 2 && j == 1)
    {
     if (strlen (UM_RD_name) > 0)
       { 
        strcpy(label,UM_RD_name);
        if (UM_RD_subscr > 0)
          {
			  ul_strip_blanks(label,&nc);
			  sprintf(tbuf,"(%d)", crv->subscr);
			  strcat(label,tbuf);
           ncl_add_token(&cmdbuf, label, NCL_nocomma);
          }
        ncl_add_token(&cmdbuf, label, NCL_comma);
       }
    }

  crv = (struct NCL_fixed_databag *) elst[j];
  if (j == 1 && crv->key == 0) goto done;
/*
.....Allow for label of near point
.....Bobby  -  7/17/96
*/
  if (lab[0] != ' ' && lab[0] != '\0' && lab[0] != '#')
  {
     ncl_add_token(&cmdbuf,lab,NCL_comma);
  }
  else if (lab[0] != '#')
  {
     ncl_add_token(&cmdbuf, ll, NCL_nocomma);
     ncl_cctostr (3,pt,str);
     ncl_add_token(&cmdbuf, str, NCL_nocomma);
     ncl_add_token(&cmdbuf, lp, NCL_comma);
  }
/*
...get next entity (trimming curve) 
*/
  if (j == 1)
  {
     if (!inp) um_vctovc (ptio1, pt);
     strcpy(lab,ptlab1);
  }
  else
  {
     um_vctovc (ptio2, pt);
     strcpy(lab,ptlab2);
  }

  j++;
  if (j < 4 && crv->key > 0) 
    { 
     ncl_add_token(&cmdbuf, "EDGE", NCL_comma);
     goto loop;
    }
/*
...Finish command line
*/
done:;
  ncl_del_token(&cmdbuf,"", UU_TRUE);
  ncl_add_cmdbuf(&cmdbuf);
  ncl_call(&cmdbuf);
/*
...put in w2 (Fortran) array to output in pp file
*/
  for (j=0; j<cmdbuf.num_cmd; j++)
    {
     n = strlen (cmdbuf.cmd[j]);
     UM_init_f77_str(fcmd,cmdbuf.cmd[j],80);
     putinw (UM_addr_of_f77_str(fcmd),&n,&j);
    }
  putinf (&cmdbuf.num_cmd);
  uu_dexit;
  return(UU_SUCCESS);
 }
/**********************************************************************
**    E_FUNCTION     : predef(keys, idir, cnnam, sub, ierr)
**       Fortran callable routine used to prepare the REDEF 
**       command (command mode) to process like in CADD.
**    PARAMETERS   
**       INPUT  : 
**          keys  - (0) - CV to trim, (1) - not used, (2) - near pt 
**                  on CV, (3) - TR1 curve, (4) - i/o CV/TR1, (5) - TR2
**                  curve, (6) - i/o CV/TR2. (5,6 for midtrim only). 
**          idir  - direction modifiers used if points are not supplied
**                  in 'keys' array (+=large, -=small, 1,2,3 = x,y,z).
**          cnnam - second CV name (for midtrim)
**          sub   - second CV subscript if CV2 is subscripted.
**          ierr  - return status.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void
  predef (keys,idir,cnnam,sub,ierr)
  int *keys, *idir, *ierr, *sub;
  UM_f77_str_ptr cnnam;
 {
  UU_KEY_ID cvkey[3], ptkey[3];
  struct UM_crvdatabag *e;
	struct UM_crvdatabag *elst[3];
  UM_transf *tfmat;
  UM_coord pt, pt2, ptp;
  UM_param uall[10], uend;
  int type,i,j,modir[3],status;
/*
...
*/
  *ierr  = 0;
  status = UU_FAILURE;
  type   = (keys[5])? 2: 1;
  type   = (keys[3])? type: 0;
  tfmat = (UM_transf *) uu_malloc(3*sizeof(UM_transf));
  e = (struct UM_crvdatabag *) uu_malloc(3*sizeof(struct UM_crvdatabag));
	elst[0] = &e[0];
	elst[1] = &e[1];
	elst[2] = &e[2];
  if (type == 2)
    {
    }

  cvkey[0] = keys[3];
  cvkey[1] = keys[5];
  cvkey[2] = keys[0];
  status = um_get_trim_curves (cvkey,elst,tfmat);
  if (status == UU_FAILURE) goto fail;
/*
...Check supported curve type
.....Trim and midtrim are now allowed - Andrew 3/20/13
  if (e[0].rel_num == UM_COMPCRV_REL && type == 0)
  {
	  *ierr = 383;goto done;
  }
*/
/*
...Get points if specified
*/
  for (i=2, j=0; i<7; i+=2, j++)
    {
     ptkey[j] = keys[i];
     modir[j] = idir[i];
    }
/*
...REDEF, TRIM or MIDTRIM
*/
  switch (type)
    {
     case 0:
       status = um_redef_curve (elst);
       break;
     case 1:
       status = ncl_prep_trim_curve (elst,tfmat,modir,ptkey,pt,uall,ptp,&uend);
       if (status == UU_SUCCESS && e[0].rel_num != UM_COMPCRV_REL)
           status = uc_trim_extend_curve (&e[0], tfmat, pt, uall, uend);
		 else if (status == UU_SUCCESS)
			  status = um_trim_extend_curve (&e[0], tfmat, pt, uall, uend);
       break;
/*
...for midtrim intersect CV with the second trimmer
*/
     case 2:
       status = ncl_prep_midtrim_curve (elst,tfmat,modir,ptkey,pt,pt2,uall,ptp,&uend);
       if (status == UU_SUCCESS)
          {
           um_set_geoname (cnnam,sub);
			  if (e[0].rel_num != UM_COMPCRV_REL)
				  status = uc_midtrim_curve (&e[0], tfmat, pt, pt2, uall, uend);
			  else
				  status = um_midtrim_curve (&e[0], tfmat, pt, pt2, uall, uend);
           um_reset_geoname ();
          }
       break;
    }

  if (status == UU_SUCCESS) goto done;

fail:;
  *ierr = 1;

done:;
  uu_free(e);
  uu_free(tfmat);
/*
...direction vectors (if used) are removed from Unibase
...in f77 calling routine (redefn)
*/
  uu_dexit;
  return; 
 }

/**********************************************************************
**    E_FUNCTION : ncl_prep_trim_curve (e,tfmat,idir,ptkey,pt,uall,
**                                      ptp,uend)
**       Prepare data to trim curve using CADD trim/extend routines
**    PARAMETERS   
**       INPUT  : 
**          idir  - Flag indicating that direction modifiers are used:
**                  0 -  points are supplied, 1 - direction vectors used.
**          ptkey - keys of entities used in in command: (0) - point on
**                  CV to trim, (1) - near pt (or vector) to I/O with TR1,
**                  (2) - near point (or vector) to I/O with TR2
**                  (2 for midtrim only). 
**       OUTPUT :  
**          e     - pointer to entities data: 0 - CV to trim, 1 - TR1,
**                  2 - TR2 (midtrim only).
**          tfmat - transformation matr. of e entities.
**          pt    - I/O CV-TR1 point coordinates.
**          uall  - list of all I/O CV-TR1 (1-st is the closest to the
**                  near point on CV).   
**          ptp   - CV identity near point coordinates.
**          uend  - parameter u for 'ptp' point on CV.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
  ncl_prep_trim_curve (elst,tfmat,idir,ptkey,pt,uall,ptp,uend)
  struct UM_crvdatabag **elst;
  UM_transf *tfmat;               /* transformation for curves to trim */
  int *idir;
  UU_KEY_ID *ptkey;
  UM_coord pt, ptp;
  UM_param *uall, *uend; 

  {
  struct UM_crvdatabag *crv1, *e;
  struct UM_point_rec *pnts;
  struct NCL_vector_rec *vecs;
  struct UM_evcrvout evout;
  UM_transf *tfmpt;     
  UM_isect ibuff[20];
  UM_coord tmpt;
  UM_vector vec1;
  UM_param uu,cs,dist; 
  int i, j, nint, closest, status;

  status = UU_SUCCESS;
  tfmpt = (UM_transf *) uu_malloc(3*sizeof(UM_transf));
  pnts  = (struct UM_point_rec *) uu_malloc(3*sizeof(struct UM_point_rec));
  vecs  = (struct NCL_vector_rec *) uu_malloc(3*sizeof(struct NCL_vector_rec));
/*
...Get points if specifed
*/
  for (i=0; i<3; i++)
	{
     if (ptkey[i])
       if (idir[i] == 0)
         {
          pnts[i].key = ptkey[i];
          status = uc_retrieve_data(&pnts[i], sizeof(struct UM_point_rec));
          if (status != UU_SUCCESS) goto fail;
          status = uc_retrieve_transf(pnts[i].key, tfmpt[i]);
          if (status != UU_SUCCESS) goto fail;
         }
       else
         {
          vecs[i].key = ptkey[i];
          status = uc_retrieve_data(&vecs[i], sizeof(struct NCL_vector_rec));
          if (status != UU_SUCCESS) goto fail;
         }
         
    }
/*
...Intersect curve with trimming entity
*/
	crv1 = (struct UM_crvdatabag *) elst[0];
	e    = (struct UM_crvdatabag *) elst[1];
  tmpt[0] = tmpt[1] = tmpt[2] = 0.0;
  i = 0;
  if (ptkey[1])
  {
    if (idir[1])
      um_vctovc (vecs[1].vec, tmpt);
    else
    {
      i = 1;
      um_vctovc (pnts[1].pt, tmpt);
    }
  }
  else
  {
    if (idir[0])
      um_vctovc (vecs[0].vec, tmpt);
    else
    {
      i = 1;
      um_vctovc (pnts[0].pt, tmpt);
    }
  }
/*
.....Project both the curves to the plane of the curved to be trimmed if planar
.....then find the intersections
*/
  status = um_proj_isect_curves (crv1,e,&i,tmpt,&nint,10,ibuff);
  if (((status!=UU_SUCCESS)||(nint==0))&&crv1->rel_num==UM_COMPCRV_REL)
	status = um_c5_isectcomp (crv1,tfmat[0],e,&i,tmpt,&nint,10,ibuff);
  if ((status != UU_SUCCESS) || (nint  ==  0))
    {
    uu_uerror0(/*entity to trim doesn't intersect trim element*/
      UM_MODEL,112);
    goto fail;
    }
/*
...if there is more than one, select that which is the closest to the
...point specified in command as 'near point' 
*/
  if (nint == 1 || ptkey[1] == 0)
    closest = 0;
  else
    {
     if (idir[1] == 0)
       {
        um_vctovc (pnts[1].pt,tmpt);
        closest = um_nearest_isect_to_point(tmpt, nint, ibuff);
       }
     else
/*
...direction modifier used, find point in that direction
*/
       {
        um_vctovc (vecs[1].vec,tmpt);
        closest = um_nearest_isect_to_direc(tmpt, nint, ibuff);
       }
    }
  um_vctovc (ibuff[closest].pt,pt);
/*
...Determine the part of the curve CV to keep or extend:
...nearest point specified - 
...intersect the nearest point with CV
*/
  if (ptkey[0])
    if (idir[0] == 0)
      {
/* use in future when pt I/O is supported with rbsp and conics */
/*       status = uc_crv_intersect(crv1, tfmat[0],
                       &pnts[0], tfmpt[0], &ncin, 10, ibufn);
       if (status != UU_SUCCESS || ncin  ==  0) goto fail;  
       um_vctovc (pnts[0].pt,tmpt);
       pickpt = um_nearest_isect_to_point(tmpt, ncin, ibufn);
       *uend   = ibufn[pickpt].u0;
       um_vctovc (ibufn[pickpt].pt,ptp);   */

/* use this stuff temporary */ 

       uc_init_evcrvout (crv1,&evout);
		 if (crv1->rel_num == UM_COMPCRV_REL)
			status = um_cctou_compcrv(crv1, tfmat[0], pnts[0], &uu, &dist);
       else
			status = uc_cctou (crv1, tfmat[0], pnts[0].pt, &uu, &dist);
       if (status != UU_SUCCESS) goto fail; 
/*
...Verify what end of CV is ment when outside curve definition
*/
       if (uu < 0.0 || uu > 1.0)
          um_verify_uvalue (crv1,tfmat[0],pnts[0].pt,&uu);
       uc_evcrv (UM_POINT, uu, crv1, tfmat[0], &evout);
       um_vctovc (evout.cp,ptp);
       *uend   = uu;
      }
/*
...modifier specified -
...use direction vector to select the correct part of CV
...relative to the selected intersection point
*/
    else
      {
       uc_init_evcrvout (crv1,&evout);
       uu     = ibuff[closest].u0;
       if (uu <= 0.0 || uu >= 1.0)
         {
          uc_evcrv (UM_POINT, (UU_REAL) 0.0, crv1, tfmat[0], &evout);
          um_vctovc (evout.cp,tmpt);
          uc_evcrv (UM_POINT, (UU_REAL) 1.0, crv1, tfmat[0], &evout);
          um_vcmnvc (evout.cp,tmpt,vec1);
          um_unitvc (vec1,vec1);
          *uend = (um_dot (vec1,vecs[0].vec) > 0.)? 1.0 - UM_FUZZ: UM_FUZZ; 
         }
       else
         {
          status = uc_evcrv(UM_FRSTDERIV,uu,crv1,tfmat[0],&evout);
          if (status != UU_SUCCESS) goto fail;
          um_unitvc (evout.dcdu,vec1);
          cs    = (um_dot (vec1,vecs[0].vec) > 0.0)? UM_FUZZ: -UM_FUZZ;
          *uend = uu + 2.0 * cs;
         }
      }
  else
    *uend = .5;
/*
...'uall' contains all i/o's but the first is the closest
...to pick of crv1.
*/
  j = 0;
  for (i = closest; i < nint; i++)
     {
      uall[j] = ibuff[i].u0;
      j++;
     }
  if (j < nint)
     for (i = 0; i < closest; i++) uall[j+i] = ibuff[i].u0;
  goto done;

fail:;
  status = UU_FAILURE;

done:;	
  uu_free(pnts);
  uu_free(vecs);
  uu_free(tfmpt);
  uu_dexit;
  return(status);
 }

/**********************************************************************
**    E_FUNCTION : ncl_prep_midtrim_curve (e,tfmat,idir,ptkey,pt,uall,
**                                         ptp,uend)
**       Prepare data to trim curve using CADD trim/extend routines
**    PARAMETERS   
**       INPUT  : 
**          idir  - Flag indicating that direction modifiers are used:
**                  0 -  points are supplied, 1 - direction vectors used.
**          ptkey - keys of entities used in in command: (0) - point on
**                  CV to trim, (1) - near pt (or vector) to I/O with TR1,
**                  (2) - near point (or vector) to I/O with TR2
**       OUTPUT :  
**          e     - pointer to entities data: 0 - CV to trim, 1 - TR1,
**                  2 - TR2.
**          tfmat - transformation matr. of e entities.
**          pt    - I/O CV-TR1 point coordinates.
**          uall  - list of all I/O CV-TR1 (1-st is the closest to the
**                  near point on CV).   
**          ptp   - CV identity near point coordinates.
**          uend  - parameter u for 'ptp' point on CV.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
  ncl_prep_midtrim_curve (elst,tfmat,idir,ptkey,pt1,pt2,uall,ptp,uend)
	struct UM_crvdatabag *elst[3];
  UM_transf *tfmat;               /* transformation for curves to trim */
  int *idir;
  UU_KEY_ID *ptkey;
  UM_coord pt1,pt2,ptp;
  UM_param *uall, *uend; 
 {
  struct UM_crvdatabag *crv1, *e;
  struct UM_point_rec *pnts;
  struct NCL_vector_rec *vecs;
  struct UM_evcrvout evout;
  UM_transf *tfmpt;     
  UM_isect ibuff[20];
  UM_coord tmpt,nppt;
  UM_vector vec1;
  UM_param uu,uio[2],cs,dist; 
  int i,ipt, j, nint, closest, status;
	UM_int2 idx = 169;
	UM_real8 ver;
	UU_LOGICAL NCL_lv96;

	getsc(&idx, &ver);
	NCL_lv96 = (ver < 9.549);

  status = UU_SUCCESS;
  tfmpt = (UM_transf *) uu_malloc(3*sizeof(UM_transf));
  pnts  = (struct UM_point_rec *) uu_malloc(3*sizeof(struct UM_point_rec));
  vecs  = (struct NCL_vector_rec *) uu_malloc(3*sizeof(struct NCL_vector_rec));
/*
...Get points if specifed
*/
  for (i=0; i<3; i++)
    {
     if (ptkey[i])
       if (idir[i] == 0)
         {
          pnts[i].key = ptkey[i];
          status = uc_retrieve_data(&pnts[i], sizeof(struct UM_point_rec));
          if (status != UU_SUCCESS) goto fail;
          status = uc_retrieve_transf(pnts[i].key, tfmpt[i]);
          if (status != UU_SUCCESS) goto fail;
         }
       else
         {
          vecs[i].key = ptkey[i];
          status = uc_retrieve_data(&vecs[i], sizeof(struct NCL_vector_rec));
          if (status != UU_SUCCESS) goto fail;
         }
         
    }
/*
...Intersect curve with trimming entity
*/
  crv1 = (struct UM_crvdatabag *) elst[0];
  for (i=1; i<3; i++)
    {
		e = (struct UM_crvdatabag *) elst[i];
		ipt=2;
		tmpt[0] = tmpt[1] = tmpt[2] = 0.0;
		if (ptkey[i])
		{
			if (idir[i])
				um_vctovc (vecs[i].vec, nppt);
			else
				um_vctovc (pnts[i].pt, nppt);
		}
		else
		{
			if (idir[0])
			  um_vctovc (vecs[0].vec, nppt);
			else
			  um_vctovc (pnts[0].pt, nppt);
		}
/*
.....Intersect non-planar curves by projecting them to an appropriate plane
.....replaced to match the trim-extend logic
.....status = uc_crv_intersect(crv1, tfmat[0], e, tfmat[i], &nint, 20, ibuff);
*/
		if(NCL_lv96)
			status = uc_crv_intersect(crv1, tfmat[0], e, tfmat[i], &nint, 20, ibuff);
		else
			status = um_proj_isect_curves (crv1,e,&ipt,nppt,&nint,10,ibuff);
    
		if ((status != UU_SUCCESS) || (nint  ==  0))
		{
			uu_uerror0(/*entity to trim doesn't intersect trim element*/
			UM_MODEL,112);
			goto fail;
		}
/*
...if there is more than one, select that which is the closest to the
...point specified in command as 'near point' 
*/
     if (nint == 1)
       closest = 0;
     else
       {
/*        if (e[1].key == e[2].key) j = i;  */
        j = i;
        if (idir[j] == 0)
          {
           um_vctovc (pnts[j].pt,tmpt);
           closest = um_nearest_isect_to_point(tmpt, nint, ibuff);
          }
        else
/*
...direction modifier used, find point in that direction
*/
          {
           um_vctovc (vecs[j].vec,tmpt);
           closest = um_nearest_isect_to_direc(tmpt, nint, ibuff);
          }
       }
     if (i == 1) um_vctovc (ibuff[closest].pt,pt1);
     else um_vctovc (ibuff[closest].pt,pt2);
     uio[i-1] = uall[i-1] = ibuff[closest].u0;
    }
  if (uall[0] > uall[1]) { uio[0] = uall[1]; uio[1] = uall[0]; }
/*
...Determine the part of the curve CV to keep:
...nearest point specified - 
...intersect the nearest point with CV
*/
  if (ptkey[0])
    if (idir[0] == 0)
      {
/* use in future when pt I/O is supported with rbsp and conics */
/*       status = uc_crv_intersect(crv1, tfmat[0],
                       &pnts[0], tfmpt[0], &ncin, 10, ibufn);
       if (status != UU_SUCCESS || ncin  ==  0) goto fail;  
       um_vctovc (pnts[0].pt,tmpt);
       pickpt = um_nearest_isect_to_point(tmpt, ncin, ibufn);
       *uend   = ibufn[pickpt].u0;
       um_vctovc (ibufn[pickpt].pt,ptp);   */

/* use this stuff temporary */ 

       uc_init_evcrvout (crv1,&evout);
       status = uc_cctou (crv1, tfmat[0], pnts[0].pt, &uu, &dist);
       if (status != UU_SUCCESS) goto fail; 
/*
...Verify what end of CV is ment when outside curve definition
*/
       if (uu < 0.0 || uu > 1.0)
          um_verify_uvalue (crv1,tfmat[0],pnts[0].pt,&uu);
       uc_evcrv (UM_POINT, uu, crv1, tfmat[0], &evout);
       um_vctovc (evout.cp,ptp);
       *uend   = uu;
      }
/*
...modifier specified -
...use direction vector to select the correct part of CV
...relative to the intersection points
*/
    else
      {
       uc_init_evcrvout (crv1,&evout);
/*     if (uu <= 0.0 || uu >= 1.0)
       {
        uc_evcrv (UM_POINT, (UU_REAL) 0.0, crv1, tfmat[0], &evout);
        um_vctovc (evout.cp,tmpt);
        uc_evcrv (UM_POINT, (UU_REAL) 1.0, crv1, tfmat[0], &evout);
        um_vcmnvc (evout.cp,tmpt,vec1);
        um_unitvc (vec1,vec1);
        *uend = (um_dot (vec1,vecs[0].vec) > 0.)? 1.0 - UM_FUZZ: UM_FUZZ; 
       }
     else */
       if (um_is_curve_closed(crv1, UM_DEFAULT_TF))
         {
          uu = dist = .5 * (uall[0] + uall[1]);
          cs = (uu > .5)? uu - .5: uu + .5;
          if (uio[0] != uall[0]) { uu = cs; cs = dist; }  
          uc_evcrv(UM_POINT,uall[0]+2*UM_FUZZ,crv1,tfmat[0],&evout);
          um_vcmnvc (evout.cp,pt1,vec1);
          um_unitvc (vec1,vec1);
          *uend = (um_dot (vec1,vecs[0].vec) < 0.0)? cs: uu;
         }
       else
         {
          uc_evcrv(UM_POINT,uio[0],crv1,tfmat[0],&evout);
          um_vctovc (evout.cp,tmpt);
          uc_evcrv(UM_POINT,uio[1],crv1,tfmat[0],&evout);
          um_vcmnvc (evout.cp,tmpt,vec1);
          um_unitvc (vec1,vec1);
          uu    = (um_dot (vec1,vecs[0].vec) > 0.0)? 1. + uio[1]: uio[0];
          *uend = .5 * uu;
         }
      }
  else 
/*
...Nothing specified or CENTER
...keep the mid part
*/
    if (idir[0] == 0)
       *uend = .5 * (uall[0] + uall[1]);
    else
       *uend = (uall[0] < uall[1])? .5*uall[0]: .5*uall[1];  
  goto done;

fail:;
  status = UU_FAILURE;

done:;
  uu_free(pnts);
  uu_free(vecs);
  uu_free(tfmpt);
  uu_dexit;
  return(status);
 }

/**********************************************************************
**    E_FUNCTION     : um_nearest_isect_to_point (ptnr, nint, ibuff)
**       Find nearest intersection to the point (in world cc system)
**    PARAMETERS   
**       INPUT  : 
**          ptnr  - nearest point coordinates
**          nint  - number of i/o points supplied in 'ibuff'
**          ibuff - buffer of intersection data.
**       OUTPUT :  
**          none
**    RETURNS      : intersection index (in ibuff)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
  um_nearest_isect_to_point(ptnr, nint, ibuff)
  UM_coord ptnr;
  int nint;
  UM_isect *ibuff; 
 {
  int i, isec;
  UU_REAL dist,d;

  dist = 10.0e10;
  for (i=0; i<nint; i++)
    {
     d = um_dcccc (ptnr,ibuff[i].pt);
     if (d < dist) { isec = i; dist = d; }
    }

  return(isec);
 }
/**********************************************************************
**    E_FUNCTION     : um_nearest_isect_to_direc (dvec, nint, ibuff)
**       Find intersection point in direction defined by 'dvec'
**       (in world cc system)
**    PARAMETERS   
**       INPUT  : 
**          dvec  - direction in which the requested point is located
**          nint  - number of i/o points supplied in 'ibuff'
**          ibuff - buffer of intersection data (here point cc used).
**       OUTPUT :  
**          none
**    RETURNS      : intersection index (in ibuff)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
  um_nearest_isect_to_direc(dvec, nint, ibuff)
  UM_vector dvec;
  int nint;
  UM_isect *ibuff; 
 {
  int i, n, isec;
  UM_coord pt0,pt;
  UM_vector dv;
  UU_REAL d, dd, dist;

/*
...assume first point in buffer as reference point
*/
  isec = 0;
  um_vctovc (ibuff[isec].pt,pt0);
  dist = 0.0;
/*
...scan thru other points for the best fit
*/
  for (i=1; i<nint; i++)
    {
     um_ilnpln(pt0,dvec,ibuff[i].pt,dvec,&n,pt);
     um_vcmnvc (pt,pt0,dv);
     dd = (um_dot(dv,dvec) > 0.0)? 1.0: -1.0;    
     d = dd * um_dcccc (pt0,pt);
     if (d > dist) { dist = d; isec = i; }
    }
  return(isec);
 }
/**********************************************************************
**    E_FUNCTION     : um_verify_uvalue (crv1,tfmat,pt,uu)
**       Find intersection point in direction defined by 'dvec'
**       (in world cc system)
**    PARAMETERS   
**       INPUT  : 
**          dvec  - direction in which the requested point is located
**          nint  - number of i/o points supplied in 'ibuff'
**          ibuff - buffer of intersection data (here point cc used).
**       OUTPUT :  
**          none
**    RETURNS      : intersection index (in ibuff)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
  um_verify_uvalue (crv1,tfmat,pt,uu)
  struct UM_crvdatabag *crv1;
  UM_transf *tfmat;     
  UM_coord pt;
  UM_param *uu;
 {
  UM_coord tmpt, dlt;
  struct UM_evcrvout evout;
  UM_param dist;
/*
...Check what CV end is closer to the point
*/    
  uc_init_evcrvout (crv1,&evout);
  uc_evcrv (UM_POINT, (UU_REAL) 0.0 , crv1, tfmat, &evout);
  um_vctovc (evout.cp,tmpt);
  uc_evcrv (UM_POINT, (UU_REAL) 1.0 , crv1, tfmat, &evout);
  um_vcmnvc (evout.cp,pt,dlt);
  dist = um_mag (dlt);
  um_vcmnvc (tmpt,pt,dlt);
  if (dist < um_mag (dlt)) *uu = 1.0 - UM_FUZZ;
  else *uu = UM_FUZZ; 
  uu_dexit;
  return (UU_SUCCESS);
 }

/**********************************************************************
**    E_FUNCTION     : um_set_geoname (cnnam,ksub)
**       Set supplied label & subsctript to created geom entity 
**       instead of autonaming.
**    PARAMETERS   
**       INPUT  : 
**          cnnam  - f77 string containing name.
**          ksub   - subscript value.
**       OUTPUT :  
**          none
**    RETURNS      : 
**    SIDE EFFECTS : NCL_ubcopy is changed to force um_create_geom
**                   create entity with given name and store its id
**                   in ranfile. !!! Important: if use of this flag is
**                   changed or expanded adjust this routine !!!
**    WARNINGS     : none
*********************************************************************/
um_set_geoname (cnnam,ksub)
  UM_f77_str_ptr cnnam;
  int *ksub;
 {
  char *p;

  p     = UM_cstr_of_f77_str(cnnam);
  if (p[0] != ' ')
    {
     strncpy (UM_RD_name,p,NCL_MAX_LABEL);
     UM_RD_subscr = *ksub;
     NCL_ubcopy = 1;
    }
   else
    {
     strcpy (UM_RD_name,""); 
     NCL_ubcopy = 0;
    }

  uu_dexit;
	return 0;
 }
/**********************************************************************
**    E_FUNCTION     : um_reset_geoname ()
**       Reset forced naming when creating geom entity, use autonaming.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_reset_geoname ()
 {
  strcpy (UM_RD_name,""); 
  NCL_ubcopy = 0;

  uu_dexit;
	return 0;
 }
