/*********************************************************************
**    NAME         :  nuci.c
**       CONTAINS: User interface routines for circle creation.
**			nclu_ci_xyz_ra()
**			nclu_ci_xy_ra()
**			nclu_ci_pt_pt_ve()
**			nclu_ci_pt_pt_pt()
**			nclu_ci_tanto_ln_ln_ln()
**			nclu_ci_pt_pt_ra()
**			nclu_ci_ln_pt_ra()
**			nclu_ci_pt_tanto_ci()
**			nclu_ci_tanto()
**			nclu_ci_ce_pt_tanto_ln()
**			nclu_ci_ce_pt_ra()
**			nclu_ci_ce_pt_pt()
**			nclu_ci_ce_tt()
**			nclu_ci_ce_pt_tanto_ci()
**			nclu_ci_canon_params()
**			nclu_ci_canon_geom()
**			nclu_ci_transl()
**			nclu_ci_offset()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nuci.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:04
*********************************************************************/
#include "usysdef.h"
#include "dselmask.h"
#include "mdeval.h"
#include "mdrel.h"
#include "mdpick.h"
#include "mcrv.h"
#include "modef.h"
#include "uhep.h"
#include "udebug.h"
#include "gmat4.h"
#include "dasnog.h"
#include "dasg.h"
#include "class.h"
#include "umath.h"
#include "go.h"
#include "nclfc.h"
#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"

/*********************************************************************
**    E_FUNCTION     : nclu_ci_xyz_ra()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ci_xyz_ra()

	{
	NCL_cmdbuf tmp, tmpz, cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ci_xyz_ra"));

	while (UU_TRUE)
	{
      status = NCL_OKINPUT;
/* 
.....Get radius, to append later 
*/
		ncl_init_cmdbuf(&tmp);
      status=ncl_add_str1(&tmp,84);
      if (status==NCL_NOINPUT) continue;
      if (status==NCL_DONE) goto done;

      while (UU_TRUE)
	   {
		   ncl_init_cmdbuf(&tmpz);

         status=ncl_add_str1(&tmpz,3);
         if (status==NCL_DONE) break;

         while (UU_TRUE)
            {
		          ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
                status = NCL_OKINPUT;
		          if (!NCL_auto_label)
                   status = ncl_add_name(&cmdbuf, 1);
                if (status == NCL_DONE) goto done;
/*
.....CI/ was being added before the label, this caused problems if
.....auto_label was off.  JLS 6/3/99
*/
                ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);
                
			       status = ncl_add_coord(&cmdbuf, 65, 2);
                if (status!=NCL_OKINPUT) break;
                strcat(cmdbuf.cur_str, tmpz.cur_str); 
                strcat(cmdbuf.cur_str, tmp.cur_str); 
					 ncl_set_cmdmode(UU_TRUE);
                ncl_add_cmdbuf(&cmdbuf);
                ncl_call(&cmdbuf);
            }
                
      }
	}
done:;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_ci_xy_ra()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ci_xy_ra()

	{
	NCL_cmdbuf cmdbuf, tmp;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ci_xy_ra"));

	while (UU_TRUE)
	{
      status = NCL_OKINPUT;
/* 
.....Get radius, to append later 
*/

		ncl_init_cmdbuf(&tmp);
      status=ncl_add_str1(&tmp,84);
      if (status==NCL_NOINPUT) continue;
      if (status==NCL_DONE) goto done;

      while (UU_TRUE)
	   {
/*
.....If not autolabel, then get label
*/
		   ncl_init_cmdbuf(&cmdbuf);
         status = NCL_OKINPUT;
		   if (!NCL_auto_label)
            status = ncl_add_name(&cmdbuf, 1);
         if (status == NCL_DONE) goto done;

/*
.....The command buffer should be initialize before the user
.....supplies the label.  Moving this up a couple of lines.  
.....Not sure why it was here.  JLS 6/3/99
		   ncl_init_cmdbuf(&cmdbuf);
*/
         ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);

			status = ncl_add_coord(&cmdbuf, 65, 2);

         if (status == NCL_OKINPUT)
         {
            strcat(cmdbuf.cur_str, tmp.cur_str); 
				ncl_set_cmdmode(UU_TRUE);
            ncl_add_cmdbuf(&cmdbuf);
            ncl_call(&cmdbuf);
         }
         else break;
      }
      if (status==NCL_DONE) goto done;
	}
done:;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_ci_pt_pt_ve()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ci_pt_pt_ve()

	{
	NCL_cmdbuf cmdbuf, tmp;
	int status, reltyp;

	uu_denter(UU_MTRC,(us,"nclu_ci_pt_pt_ve"));

	while (UU_TRUE)
		{
		ncl_init_cmdbuf(&cmdbuf);
/*
.....This should be after label prompt.
      ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);
*/
		status = NCL_OKINPUT;
/*
.....If not autolabel, then get label
*/
      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
      if (status == NCL_DONE) goto done;

      ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);
		status = ncl_add_label_rel1(&cmdbuf,90,UD_ncl_ptpv,&reltyp);
      if (status == NCL_DONE) goto done;

      if (reltyp == NCL_POINT_REL || reltyp == UM_POINT_REL)
      { 
	      while (UU_TRUE)
            {
		      ncl_init_cmdbuf(&tmp);

            status = ncl_add_label_rel1(&tmp,91,UD_ncl_ptpve,&reltyp);
            if (status == NCL_DONE) goto repeat;

            if (reltyp == NCL_POINT_REL || reltyp == UM_POINT_REL)
            {
		       	status = ncl_add_label1(&tmp, 92, UD_ncl_vepv);
               if (status == NCL_DONE) continue;
            }
            else if (reltyp == NCL_VECTOR_REL)
            {
		       	status = ncl_add_label1(&tmp, 88, UD_ncl_ptpv);
               if (status == NCL_DONE) continue;
            }

	         ncl_add_token(&cmdbuf, tmp.cur_str, NCL_nocomma);
            break;
            }
      }

      else /* first pick was point vector */
      {
		   status = ncl_add_label1(&cmdbuf, 88, UD_ncl_ptpv);
         if (status == NCL_DONE) continue;
      }

		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
repeat:;
      }
done:;

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_ci_pt_pt_pt()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ci_pt_pt_pt()

	{
	NCL_cmdbuf cmdbuf, tmp;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ci_pt_pt_pt"));

	while (UU_TRUE)
		{
		ncl_init_cmdbuf(&cmdbuf);
/*
.....Again, this should be after label prompt.
	   ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);
*/
		status = NCL_OKINPUT;
/*
.....If not autolabel, then get label
*/
      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
      if (status == NCL_DONE) goto done;

	   ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);
		status = ncl_add_label1(&cmdbuf, 87, UD_ncl_pt);
      if (status == NCL_DONE) goto done;
  
	   while (UU_TRUE)
		   {
		   ncl_init_cmdbuf(&tmp);
		   status = ncl_add_label1(&tmp, 88, UD_ncl_pt);
         if (status == NCL_DONE) break;
	      
		   status = ncl_add_label1(&tmp, 89, UD_ncl_pt);
         if (status == NCL_DONE) continue;

	      ncl_add_token(&cmdbuf, tmp.cur_str, NCL_nocomma);
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
         break;
			}

      } /* end of while #1 */
 
done:;

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_ci_pt_pv()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
nclu_ci_pt_pv()

	{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ci_pt_pv"));

	while (UU_TRUE)
		{
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;

		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);

		if (status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);

		if (status == NCL_OKINPUT)
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 87, UD_ncl_ptpv);

		if (status == NCL_OKINPUT)
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 88, UD_ncl_ptpv);

		if ((status == NCL_OKINPUT) || (status == NCL_DONE))
			{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
			}
		}
	uu_dexit;
	}
*/
/*********************************************************************
**    E_FUNCTION     : nclu_ci_tanto_ln_ln()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
nclu_ci_tanto_ln_ln()

	{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ci_tanto_ln_ln"));

	while (UU_TRUE)
		{
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;

		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);

		if (status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);

		if (status == NCL_OKINPUT)
			status = ncl_add_modifier(&cmdbuf, NCL_XY_MODIFIER);

		if (status == NCL_OKINPUT)
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 96, UD_ncl_ln);

		if (status == NCL_OKINPUT)
			status = ncl_add_modifier(&cmdbuf, NCL_XY_MODIFIER);

		if (status == NCL_OKINPUT)
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 97, UD_ncl_ln);

		if (status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbuf, NCL_radius, NCL_comma);

		if (status == NCL_OKINPUT)
			status = ncl_add_length(&cmdbuf, 98);

		if ((status == NCL_OKINPUT) || (status == NCL_DONE))
			{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
			}

		}

	uu_dexit;
	}
*/
/*********************************************************************
**    E_FUNCTION     : nclu_ci_tanto_ln_ln_ln()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ci_tanto_ln_ln_ln()

	{
	NCL_cmdbuf cmdbuf, tmp;
	int numint, status;
   UM_PLOCREC pick;
   UU_KEY_ID  key1, key2, key3;
   int relnum;
   UM_coord picked;
   UM_vector vpnorm;

	uu_denter(UU_MTRC,(us,"nclu_ci_tanto_ln_ln_ln"));

	while (UU_TRUE)
		{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....This goes after label prompt.
	   ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);
*/
		status = NCL_OKINPUT;
/*
.....If not autolabel, then get label
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
      if (status == NCL_DONE) goto done;
	   ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);
/* 
.....Pick first line 
*/
      ud_lgeo(UU_TRUE, UD_ncl_ln);
      ua_dl_pldas(UD_DASPCKLOC,UA_NCL,108,&pick,1,&numint,1); 
      if (numint <= 0) goto done;
      key1 = um_get_pickkey(&pick.pent, 1);
      ur_retrieve_data_relnum(key1, &relnum);
      um_ploctocc(&pick.ploc,picked);
      um_vpnorm(pick.ploc.transform, vpnorm);
      ncl_proj_to_wp (picked, vpnorm, picked);
      status = add_ln_label(&cmdbuf,key1,relnum,picked,vpnorm);
      if (status != NCL_OKINPUT) continue;

	   while (UU_TRUE)
	      {
		   ncl_init_cmdbuf(&tmp);
/* 
.....Pick second line
*/
         ud_lgeo(UU_TRUE, UD_ncl_ln);
         ua_dl_pldas(UD_DASPCKLOC,UA_NCL,109,&pick,1,&numint,1); 
         if (numint <= 0) break;
         key2 = um_get_pickkey(&pick.pent, 1);
         ur_retrieve_data_relnum(key2, &relnum);
         if (key1 == key2)
             uu_uerror0(/*same line picked twice*/UM_MODEL,79);
         else
            {
            um_ploctocc(&pick.ploc,picked);
            um_vpnorm(pick.ploc.transform, vpnorm);
            ncl_proj_to_wp (picked, vpnorm, picked);
            status=add_ln_label(&tmp,key2,relnum,picked,vpnorm);
            if (status != NCL_OKINPUT) continue;

	         while (UU_TRUE)
	            {
/* 
.....Pick third line
*/
               ud_lgeo(UU_TRUE, UD_ncl_ln);
               ua_dl_pldas(UD_DASPCKLOC,UA_NCL,110,&pick,1,&numint,1); 
               if (numint <= 0) break;
               key3 = um_get_pickkey(&pick.pent, 1);
               ur_retrieve_data_relnum(key3, &relnum);
               if ((key3 == key1) || (key3 == key2))
                   uu_uerror0(/*same line picked twice*/UM_MODEL,79);
               else
                  {
                    um_ploctocc(&pick.ploc,picked);
                    um_vpnorm(pick.ploc.transform, vpnorm);
                    ncl_proj_to_wp (picked, vpnorm, picked);
                    status=add_ln_label(&tmp,key3,relnum,picked,vpnorm);
                    if (status != NCL_OKINPUT) continue;
	      
                    ncl_add_token(&cmdbuf, tmp.cur_str, NCL_nocomma);
/*
						  ncl_set_cmdmode(UU_TRUE);
*/
			           ncl_add_cmdbuf(&cmdbuf);
			           ncl_call(&cmdbuf);
                    goto repeat;
		            }
    			   } /*end of while #3 */
            }
      } /* end of while #2 */
repeat:;
      } /* end of while #1 */
 
done:;

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_ci_pt_pt_ra()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/* Now used for CAD only --- Eduard */
nclu_ci_pt_pt_ra()

	{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ci_pt_pt_ra"));

	while (UU_TRUE)
		{
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;

		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);

		if (status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);

		if (status == NCL_OKINPUT)
			status = ncl_add_modifier(&cmdbuf, NCL_XY_MODIFIER);

		if (status == NCL_OKINPUT)
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 77, UD_ncl_ptpv);

		if (status == NCL_OKINPUT)
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 78, UD_ncl_ptpv);

		if (status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbuf, NCL_radius, NCL_comma);

		if (status == NCL_OKINPUT)
			status = ncl_add_length(&cmdbuf, 79);

		if ((status == NCL_OKINPUT) || (status == NCL_DONE))
			{
		   ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
			}

		}

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_ci_tanto()
**       Create a circle tangent to two planar curves (which can be 
**       lines or circles; note: only one of two entities may be a 
**       generic curve). A user is prompted for a radius, then picks 
**       two entities.  
**       Direction modifiers are determined by pick locations; a near
**       point is determined by the second pick location, if necessary.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ci_tanto()

	{
   int relnum1, relnum2;
   UU_KEY_ID  key1, key2;
	NCL_cmdbuf cmdbuf, tmp;
	int numint, status;
   UM_PLOCREC pick1, pick2;
   UM_coord picked1, picked2;
   struct UM_line_rec ln;
   struct UM_point_rec pt;
   UM_vector vc, vc1;
   UM_vector vpnorm1, vpnorm2;
   UM_coord  ppt;
   struct NCL_nclpv_rec *pv;

	uu_denter(UU_MTRC,(us,"nclu_ci_tanto"));

	while (UU_TRUE)
	{
      status = NCL_OKINPUT;
/* 
.....Get radius, to append later 
*/
		ncl_init_cmdbuf(&tmp);
      status=ncl_add_str1(&tmp,84);
      if (status==NCL_NOINPUT) continue;
      if (status==NCL_DONE) goto done;

	while (UU_TRUE)
	{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
/*
.....If not autolabel, then get label
*/
		if (!NCL_auto_label)
           status = ncl_add_name(&cmdbuf, 1);
      if (status == NCL_DONE) goto done;
/* 
.....Pick first entity
*/
         ua_dl_pldas(UD_DASPCKLOC,UA_NCL,80,&pick1,1,&numint,1); 
         if (numint <= 0) break;
         key1 = um_get_pickkey(&pick1.pent, 1);
         ur_retrieve_data_relnum(key1, &relnum1);
         if (UC_CURVE_CLASS != uc_super_class(relnum1) &&
            relnum1 != UM_POINT_REL && relnum1 != NCL_POINTVEC_REL)
         {
            uu_uerror0(/*curve not picked*/UM_MODEL,323);
            continue;
         }
         um_ploctocc(&pick1.ploc,picked1);
         um_vpnorm(pick1.ploc.transform, vpnorm1);
         ncl_proj_to_wp (picked1, vpnorm1, picked1);
/* 
.....Pick second entity
*/
         ua_dl_pldas(UD_DASPCKLOC,UA_NCL,81,&pick2,1,&numint,1); 
         if (numint <= 0) continue;
         key2 = um_get_pickkey(&pick2.pent, 1);
         ur_retrieve_data_relnum(key2, &relnum2);
         if (UC_CURVE_CLASS != uc_super_class(relnum2) &&
            relnum2 != UM_POINT_REL && relnum2 != NCL_POINTVEC_REL)
         {
              uu_uerror0(/*curve not picked*/UM_MODEL,323);
         }
	      else
         {   
            if (key1 == key2)
                uu_uerror0(/*same curve picked for both 
                                           curves*/UM_MODEL,324);
            else
            {
/* 
.....Remember pick locations and start command line 
*/
               um_ploctocc(&pick2.ploc,picked2);
               um_vpnorm(pick2.ploc.transform, vpnorm2);
               ncl_proj_to_wp (picked2, vpnorm2, picked2);
               ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);

switch(relnum1)
   {
   case UM_POINT_REL:
   case NCL_POINTVEC_REL: /* First picked is point or pointvector */
      switch(relnum2)
         {
         case UM_POINT_REL:/* Second picked is point or pointvector */
         case NCL_POINTVEC_REL: 
/*
..... CI/XLARGE,PT1,PT2,RADIUS,1
*/
/* 
.....Determine the correct direction modifier by the mutual
.....position of the second point and the second picked location 
*/
            if (relnum2 == NCL_POINTVEC_REL)
              {
                 pv = (struct NCL_nclpv_rec *)
                       uu_malloc(sizeof(struct NCL_nclpv_rec));
                 pv->key=key2;
                 ncl_retrieve_data_fixed(pv);
                 ncl_proj_to_wp(pv->pt,vpnorm2,ppt);
                 uu_free(pv);
              }
            else
              {
                 pt.key = key2;
                 um_get_all_geom(&pt, sizeof(pt));
                 ncl_proj_to_wp(pt.pt,vpnorm2,ppt);
              }

            um_vcmnvc(picked2, ppt, vc);

            if (fabs(vc[0]) >= fabs(vc[1])) /*vector is more horizontal*/
            {
                  if (vc[0] >= 0.0) 
                      ncl_add_token(&cmdbuf,NCL_xlarge,NCL_comma);
                  else
                      ncl_add_token(&cmdbuf,NCL_xsmall,NCL_comma);
            }
            else /* vector is more vertical than horizontal */
            {
                  if (vc[1] >= 0.0) 
                      ncl_add_token(&cmdbuf,NCL_ylarge,NCL_comma);
                  else
                      ncl_add_token(&cmdbuf,NCL_ysmall,NCL_comma);
            }

	         ncl_add_token(&cmdbuf,pick1.ploc.label,NCL_comma);
	         status=ncl_add_token(&cmdbuf,pick2.ploc.label,NCL_comma);
            break;

         case UM_LINE_REL: /* Second picked is a line */
/*
..... CI/TANTO,LN1,YLARGE,PT2,RADIUS,1
*/
            ncl_add_token(&cmdbuf,NCL_tanto,NCL_comma);
	         ncl_add_token(&cmdbuf,pick2.ploc.label,NCL_comma);
/* 
.....Determine the correct direction modifier by the mutual
.....position of the line and the picked point 
*/
            ln.key = key2;
            um_get_all_geom(&ln, sizeof(ln));
            ncl_proj_to_wp (ln.spt, vpnorm2, ln.spt);
            ncl_proj_to_wp (ln.ept, vpnorm2, ln.ept);
               
            um_vcmnvc(ln.ept, ln.spt, vc);
            um_vcmnvc(picked2, picked1, vc1);

            if (fabs(vc[0]) >= fabs(vc[1])) /*line is more horizontal*/
            {
                  if (vc1[0] >= 0.0) 
                      ncl_add_token(&cmdbuf,NCL_xlarge,NCL_comma);
                  else
                      ncl_add_token(&cmdbuf,NCL_xsmall,NCL_comma);
            }
            else /* line is more vertical than horizontal */
            {
                  if (vc1[1] >= 0.0) 
                      ncl_add_token(&cmdbuf,NCL_ylarge,NCL_comma);
                  else
                      ncl_add_token(&cmdbuf,NCL_ysmall,NCL_comma);
            }

	         status=ncl_add_token(&cmdbuf,pick1.ploc.label,NCL_comma);
            break;

         case UM_CIRCLE_REL: /* Second picked is a circle */
/*
..... CI/PT1,YLARGE,IN,CI2,RADIUS,1
*/
            ncl_add_token(&cmdbuf,pick1.ploc.label,NCL_comma);
            status=add_ci_label1(&cmdbuf,key2,relnum2,picked2,vpnorm2);
            break;

         default:  /* Second picked is a curve */         

            uu_uerror0(/*circle thru point tanto curve
                         is not available at the moment*/ UM_MODEL,325);
            status = NCL_NOINPUT;
            break;
         }
      break;

   case UM_LINE_REL: /* First picked is a line */
      switch(relnum2)
         {
         case UM_POINT_REL:/* Second picked is point or pointvector */
         case NCL_POINTVEC_REL: 
/*
..... CI/TANTO,LN1,YLARGE,PT2,RADIUS,1
*/
            ncl_add_token(&cmdbuf,NCL_tanto,NCL_comma);
	         ncl_add_token(&cmdbuf,pick1.ploc.label,NCL_comma);
/* 
.....Determine the correct direction modifier by the mutual
.....position of the line and the picked point 
*/
            ln.key = key1;
            um_get_all_geom(&ln, sizeof(ln));

            ncl_proj_to_wp (ln.spt, vpnorm1, ln.spt);
            ncl_proj_to_wp (ln.ept, vpnorm1, ln.ept);

            um_vcmnvc(ln.ept, ln.spt, vc);
            um_vcmnvc(picked2, picked1, vc1);

            if (fabs(vc[0]) >= fabs(vc[1])) /*line is more horizontal*/
            {
                  if (vc1[1] >= 0.0) 
                      ncl_add_token(&cmdbuf,NCL_ylarge,NCL_comma);
                  else
                      ncl_add_token(&cmdbuf,NCL_ysmall,NCL_comma);
            }
            else /* line is more vertical than horizontal */
            {
                  if (vc1[0] >= 0.0) 
                      ncl_add_token(&cmdbuf,NCL_xlarge,NCL_comma);
                  else
                      ncl_add_token(&cmdbuf,NCL_xsmall,NCL_comma);
            }

	         status=ncl_add_token(&cmdbuf,pick2.ploc.label,NCL_comma);
            break;

         case UM_LINE_REL: /* Second picked is a line */
/*
......CI/XLARGE,LN1,YLARGE,LN2,RADIUS,1
*/
            add_ln_label(&cmdbuf,key1,relnum1,picked1,vpnorm1);
            status=add_ln_label(&cmdbuf,key2,relnum2,picked2,vpnorm2);
            break;

         case UM_CIRCLE_REL: /* Second picked is a circle */
/*
......CI/XLARGE,LN1,YLARGE,OUT,CI2,RADIUS,1
*/
            add_ln_label(&cmdbuf,key1,relnum1,picked1,vpnorm1);
            status=add_ci_label1(&cmdbuf,key2,relnum2,picked2,vpnorm2);
            break;

         default:   /* Second picked is a curve */                   
/*
......CI/XLARGE,LN1,YLARGE,CV2,PT6,RADIUS,1
*/
            add_ln_label(&cmdbuf,key1,relnum1,picked1,vpnorm1);
            status=add_cv_label(&cmdbuf,key2,&pick2.ploc,vpnorm2);
            if (status == NCL_OKINPUT)
                  status=add_pt(&cmdbuf,picked2);
            break;
         }
      break;

   case UM_CIRCLE_REL: /* First picked is a circle */
      switch(relnum2)
         {
         case UM_POINT_REL: /* Second picked is point or pointvector */
         case NCL_POINTVEC_REL: 
/*
..... CI/PT1,YLARGE,IN,CI2,RADIUS,1
*/
            ncl_add_token(&cmdbuf,pick2.ploc.label,NCL_comma);
            status=add_ci_label1(&cmdbuf,key1,relnum1,picked1,vpnorm1);
            break;

         case UM_LINE_REL: /* Second picked is a line */
/*
......CI/XLARGE,LN1,YLARGE,OUT,CI2,RADIUS,1
*/
            add_ln_label(&cmdbuf,key2,relnum2,picked2,vpnorm2);
            status=add_ci_label1(&cmdbuf,key1,relnum1,picked1,vpnorm1);
            break;

         case UM_CIRCLE_REL: /* Second picked is a circle */
/*
......CI/XLARGE,IN,CI1,OUT,CI2,RADIUS,1
*/
            add_ci_label1(&cmdbuf,key1,relnum1,picked1,vpnorm1);
            status=add_ci_label(&cmdbuf,key2,relnum2,picked2,vpnorm2);
            break;

         default:   /* Second picked is a curve */                   
/*
......CI/OUT,CI1,YLARGE,CV2,PT6,RADIUS,1
*/
            add_ci_label(&cmdbuf,key1,relnum1,picked1,vpnorm1);
            status=add_cv_label(&cmdbuf,key2,&pick2.ploc,vpnorm2);
            if (status == NCL_OKINPUT)
                  status=add_pt(&cmdbuf,picked2);
            break;
         }
      break;

   default:   /* First picked is a curve */                   
      switch(relnum2)
         {
         case UM_POINT_REL:/* Second picked is point or pointvector */
         case NCL_POINTVEC_REL: 

            uu_uerror0(/*circle thru a point tangent to a curve
                      is not available at the moment*/ UM_MODEL,325);
            status = NCL_NOINPUT;
            break;
 
         case UM_LINE_REL: /* Second picked is a line */
/*
......CI/XLARGE,LN1,YLARGE,CV2,PT6,RADIUS,1
*/
            add_ln_label(&cmdbuf,key2,relnum2,picked2,vpnorm2);
            status=add_cv_label(&cmdbuf,key1,&pick1.ploc,vpnorm1);
            if (status == NCL_OKINPUT)
                  status=add_pt(&cmdbuf,picked2);
            break;

         case UM_CIRCLE_REL: /* Second picked is a circle */
/*
......CI/OUT,CI1,YLARGE,CV2,PT6,RADIUS,1
*/
            add_ci_label(&cmdbuf,key2,relnum2,picked2,vpnorm2);
            status=add_cv_label(&cmdbuf,key1,&pick1.ploc,vpnorm1);
            if (status == NCL_OKINPUT)
                  status=add_pt(&cmdbuf,picked2);
            break;

         default:   /* Second picked is a curve */                   
 
            uu_uerror0(/*circle tangent to two curves 
                         is not available at the moment*/ UM_MODEL,326);
            status = NCL_NOINPUT;
            break;
         }
      break;
   }

/* 
.....Add radius and finish 
*/
               if (status == NCL_OKINPUT)
               {
                  ncl_add_token(&cmdbuf, NCL_radius, NCL_comma);
                  strcat(cmdbuf.cur_str, tmp.cur_str); 
		   			ncl_set_cmdmode(UU_TRUE);
                  ncl_add_cmdbuf(&cmdbuf);
                  ncl_call(&cmdbuf);
               }
               if (status == NCL_DONE) goto done;
			   }
         }
      } /* end of while #2 */
      } /* end of while #1 */
 
done:;

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ncl_proj_to_wp(pt,vpnorm,ppt);
**       Project point down the view port normal to refsys/modsys xy-plane.
**    PARAMETERS   
**       INPUT  : 
**          pt         point location
**          vpnorm     viewport normal.
**       OUTPUT :  
**          ppt        point projection
**    RETURNS      : 
**                   none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_proj_to_wp(pt,vpnorm,ppt)
UM_coord pt, ppt;
UM_vector vpnorm;

{
   UM_transf lref_tm, lmod_tm;
   UM_int2 ifl1,ifl2;
   UM_vector vz;
   UM_coord  p0;
   int nint;
/*
.....Transform zaxis thru refsys or modsys matrix, if necessary.
*/
            p0[0] = p0[1] = p0[2] = vz[0] = vz[1] = 0.0;
            vz[2] = 1.0;
            gtref (lref_tm,&ifl1);
            if (ifl1)
            {
               um_vctmtf (vz,lref_tm,vz);
               um_inverttf(lref_tm,lref_tm);
            }
            gtmod (lmod_tm,&ifl2);
            if (ifl2)
            {
               um_vctmtf (vz,lmod_tm,vz);
               um_inverttf(lmod_tm,lmod_tm);
            }
/*
.....Project point along view normal to refsys/modsys xy-plane.
*/
            um_unitvc (vz, vz);
            um_ilnpln(pt,vpnorm,p0,vz,&nint,ppt);
            if (ifl2)
               um_vctmtf (ppt,lmod_tm,ppt);
            if (ifl1)
               um_vctmtf (ppt,lref_tm,ppt);

            return(0);
}

/*********************************************************************
**    E_FUNCTION     : add_ln_label(cmdbuf,key,rel_num,picked);
**       Add the label of a picked line to a command buffer. 
**       The label is preceded by a direction modifier, determined 
**       by the pick location.  
**    PARAMETERS   
**       INPUT  : 
**          cmdbuf     command buffer to place modifier and label   
**          key
**          rel_num
**          picked     pick location coordinates
**       OUTPUT :  
**          cmdbuf     modifier and label are appended to current line
**    RETURNS      : 
**                   NCL_OKINPUT iff modifier and label added to cmdbuf
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
add_ln_label(cmdbuf,key,rel_num,picked,vpnorm)
NCL_cmdbuf  *cmdbuf;
UU_KEY_ID   key;
int         rel_num;
UM_coord    picked;
UM_vector   vpnorm;

   {
	int status;
   struct UM_line_rec ln; 
   UM_vector vc, unit_vc, vc1, proj, normal;
	char str[80];

	uu_denter(UU_MTRC,(us,"add_ln_label"));

   ln.key = key;
   um_get_all_geom(&ln, sizeof(ln));
 
   ncl_proj_to_wp (ln.spt, vpnorm, ln.spt);
   ncl_proj_to_wp (ln.ept, vpnorm, ln.ept);

   um_vcmnvc(ln.ept, ln.spt, vc);
   um_unitvc(vc, unit_vc);
   um_vcmnvc(picked, ln.spt, vc1);
   um_vctmsc(unit_vc, um_dot(vc1, unit_vc), proj);
   um_vcmnvc(vc1, proj, normal); 

   if (fabs(vc[0]) >= fabs(vc[1])) /* line is more horizontal */
   {
       if (normal[1] >= 0.0) 
           status = ncl_add_token(cmdbuf,NCL_ylarge,NCL_comma);
       else
           status = ncl_add_token(cmdbuf,NCL_ysmall,NCL_comma);
   }
   else /* line is more vertical than horizontal */
   {
       if (normal[0] >= 0.0) 
           status = ncl_add_token(cmdbuf,NCL_xlarge,NCL_comma);
       else
           status = ncl_add_token(cmdbuf,NCL_xsmall,NCL_comma);
   }
   if (status == NCL_OKINPUT)
	{
		ncl_get_label(&ln,str);
      status = ncl_add_token(cmdbuf, str, NCL_comma);
	}

   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : add_ci_label1(cmdbuf,key,rel_num,picked);
**       Add the label of a picked circle to a command buffer. 
**       The label is preceded by a direction modifier and a
**       positional modifier, determined by the pick location.  
**    PARAMETERS   
**       INPUT  : 
**          cmdbuf     command buffer to place modifier and label   
**          key
**          rel_num
**          picked     pick location coordinates
**       OUTPUT :  
**          cmdbuf     modifier and label are appended to current line
**          
**    RETURNS      : 
**                   NCL_OKINPUT iff modifier and label added to cmdbuf
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
add_ci_label1(cmdbuf,key,rel_num,picked,vpnorm)
NCL_cmdbuf  *cmdbuf;
UU_KEY_ID   key;
int         rel_num;
UM_coord    picked;
UM_vector   vpnorm;

   {
	int status;
   struct UM_circle_rec ci; 
   UM_vector vc;

	uu_denter(UU_MTRC,(us,"add_ci_label1"));

   ci.key = key;
   um_get_all_geom(&ci, sizeof(ci));
 
   ncl_proj_to_wp (ci.center, vpnorm, ci.center);

   um_vcmnvc(picked, ci.center, vc);

   if (fabs(vc[0]) >= fabs(vc[1])) 
   {
       if (vc[0] >= 0.0) 
           status=ncl_add_token(cmdbuf,NCL_xlarge,NCL_comma);
       else
           status=ncl_add_token(cmdbuf,NCL_xsmall,NCL_comma);
   }
   else 
   {
       if (vc[1] >= 0.0) 
           status=ncl_add_token(cmdbuf,NCL_ylarge,NCL_comma);
       else
           status=ncl_add_token(cmdbuf,NCL_ysmall,NCL_comma);
   }
   if (status == NCL_OKINPUT)
	      status = add_ci_label(cmdbuf,key,rel_num,picked,vpnorm);
   
   uu_dexit;
   return(status);

   }

/*********************************************************************
**    E_FUNCTION     : add_ci_label(cmdbuf,key,rel_num,picked);
**       Add the label of a picked circle to a command buffer. 
**       The label is preceded by a positional modifier, determined 
**       by the pick location.  
**    PARAMETERS   
**       INPUT  : 
**          cmdbuf     command buffer to place modifier and label   
**          key
**          rel_num
**          picked     pick location coordinates
**       OUTPUT :  
**          cmdbuf     modifier and label are appended to current line
**          
**    RETURNS      : 
**                   NCL_OKINPUT iff modifier and label added to cmdbuf
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void add_ci_label2(str,key,rel_num,picked,vpnorm)
char str[];
UU_KEY_ID key;
int rel_num;
UM_coord picked;
UM_vector vpnorm;
{
	struct UM_circle_rec ci;
	char labstr[80];

	uu_denter(UU_MTRC,(us,"add_ci_label"));

	ci.key = key;
	um_get_all_geom(&ci, sizeof(ci));

	ncl_proj_to_wp (ci.center, vpnorm, ci.center);

	if (UM_SQDIS(ci.center, picked) >= ci.radius*ci.radius)
		strcpy(str,NCL_out);
	else
		strcpy(str,NCL_in);
	strcat(str,",");

	ncl_get_label(&ci,labstr);
	strcat(str,labstr);

   uu_dexit;
   return;
}

/*********************************************************************
**    E_FUNCTION     : add_ci_label(cmdbuf,key,rel_num,picked);
**       Add the label of a picked circle to a command buffer. 
**       The label is preceded by a positional modifier, determined 
**       by the pick location.  
**    PARAMETERS   
**       INPUT  : 
**          cmdbuf     command buffer to place modifier and label   
**          key
**          rel_num
**          picked     pick location coordinates
**       OUTPUT :  
**          cmdbuf     modifier and label are appended to current line
**          
**    RETURNS      : 
**                   NCL_OKINPUT iff modifier and label added to cmdbuf
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
add_ci_label(cmdbuf,key,rel_num,picked,vpnorm)
NCL_cmdbuf  *cmdbuf;
UU_KEY_ID   key;
int         rel_num;
UM_coord    picked;
UM_vector   vpnorm;

   {
	int status;
   struct UM_circle_rec ci;
	char str[80];

	uu_denter(UU_MTRC,(us,"add_ci_label"));

   ci.key = key;
   um_get_all_geom(&ci, sizeof(ci));

   ncl_proj_to_wp (ci.center, vpnorm, ci.center);

   if (um_dcccc(ci.center, picked) >= ci.radius)
          status=ncl_add_token(cmdbuf,NCL_out,NCL_comma);
   else
          status=ncl_add_token(cmdbuf,NCL_in,NCL_comma);

   if (status == NCL_OKINPUT)
	{
		ncl_get_label(&ci,str);
	   status = ncl_add_token(cmdbuf,str,NCL_comma);
	}

   uu_dexit;
   return(status);

   }

/*********************************************************************
**    E_FUNCTION     : add_cv_label(cmdbuf,key,ploc);
**       Add the label of a picked curve to a command buffer. 
**       The label is preceded by a direction modifier, and followed 
**       by a near point, both determined by the pick location. 
**    PARAMETERS   
**       INPUT  : 
**          cmdbuf     command buffer to place modifier and label   
**          key
**          ploc       NDC pick location record
**       OUTPUT :  
**          cmdbuf     modifier and label are appended to current line
**          
**    RETURNS      : 
**                   NCL_OKINPUT iff modifier and label added to cmdbuf
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
add_cv_label(cmdbuf,key,ploc,vpnorm)
NCL_cmdbuf  *cmdbuf;
UU_KEY_ID   key;
UD_NDCLOCREC *ploc;
UM_vector   vpnorm;

   {
	int status;
   UM_coord coord, npt;
   UM_vector vc;

	uu_denter(UU_MTRC,(us,"add_cv_label"));

   status = uc_near_on_entity(key, ploc, npt);
   if(status != UU_SUCCESS) goto done;     

   ncl_proj_to_wp (npt, vpnorm, npt);

   um_ploctocc(ploc, coord);
   ncl_proj_to_wp (coord, vpnorm, coord);
   um_vcmnvc(coord, npt, vc);
   if (fabs(vc[0]) >= fabs(vc[1])) 
   {
       if (vc[0] >= 0.0) 
           status=ncl_add_token(cmdbuf,NCL_xlarge,NCL_comma);
       else
           status=ncl_add_token(cmdbuf,NCL_xsmall,NCL_comma);
   }
   else 
   {
       if (vc[1] >= 0.0) 
           status=ncl_add_token(cmdbuf,NCL_ylarge,NCL_comma);
       else
           status=ncl_add_token(cmdbuf,NCL_ysmall,NCL_comma);
   }

   if (status == NCL_OKINPUT)
	        status = ncl_add_token(cmdbuf,ploc->label,NCL_comma);
done:;
 
   uu_dexit;
   return(status);

   }

/*********************************************************************
**    E_FUNCTION     : add_pt(cmdbuf,coord);
**       Add a (nested) point to a command buffer. 
**       The point is to use as a near point, its coordinates are 
**       previously determined by a pick location. 
**    PARAMETERS   
**       INPUT  : 
**          cmdbuf     command buffer to place modifier and label   
**          coord      pick location coordinates
**       OUTPUT :  
**          cmdbuf     nested point appended to current line
**          
**    RETURNS      : 
**                   NCL_OKINPUT iff point added to cmdbuf
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
add_pt(cmdbuf,coord)
NCL_cmdbuf  *cmdbuf;
UM_coord coord;

   {
	int status;
   char str[80],str1[80];

	uu_denter(UU_MTRC,(us,"add_pt"));

	ncl_cctostr(2,coord,str);
	sprintf(str1,"(PT/%s)",str);
	status = ncl_add_token(cmdbuf,str1,NCL_comma);
   
   uu_dexit;
   return(status);

   }

/*********************************************************************
**    E_FUNCTION     : nclu_ci_ce_tt()
**       Create a circle with a given center, tangent to a line, circle,
**       or curve.
**       A user is prompted for a center point (by pick), then picks an
**       entity.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ci_ce_tt()

   {
    NCL_cmdbuf cmdbuf;
    int numint, relnum, status;
    UM_PLOCREC pick1, pick2;
    UU_KEY_ID key;
    UM_coord picked1, picked2;
    struct UM_circle_rec ci;
    UU_REAL d, l, r;
    char str[80];
    UM_vector vpnorm1, vpnorm2;

   uu_denter(UU_MTRC,(us,"nclu_ci_ce_tt"));

   while (UU_TRUE)
      {
      status = NCL_OKINPUT;
/*
.....If not autolabel, then get label
*/

      if (!NCL_auto_label)
          status = ncl_add_name(&cmdbuf, 1);
       if (status == NCL_DONE) goto done;

      ncl_init_cmdbuf(&cmdbuf);
       ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);
      ncl_add_token(&cmdbuf, NCL_center, NCL_comma);

/*
.....Pick center point
*/
      ud_lgeo(UU_TRUE, UD_ncl_ptpv);
      ua_dl_pldas(UD_DASPCKLOC,UA_NCL,75,&pick1,1,&numint,1);
      if (numint <= 0) break;

      ncl_add_token(&cmdbuf, pick1.ploc.label, NCL_comma);
/*
.....Pick ln, ci, or cv
*/
      ud_lgeo(UU_TRUE, UD_ncl_curve);
      ua_dl_pldas(UD_DASPCKLOC,UA_NCL,82,&pick2,1,&numint,1);
      if (numint <= 0) continue;
      key = um_get_pickkey(&pick2.pent, 1);
      ur_retrieve_data_relnum(key, &relnum);

      switch (relnum)
      {
         case UM_LINE_REL: /* Second picked is a line */
/*
..... CI/CENTER,PT1,TANTO,LN2
*/
            ncl_add_token(&cmdbuf,NCL_tanto,NCL_comma);
            status = ncl_add_token(&cmdbuf, pick2.ploc.label,
                                                 NCL_nocomma);
            break;

         case UM_CIRCLE_REL: /* Second picked is a circle */
/*
..... CI/CENTER,PT1,LARGE,TANTO,CI2
*/
            d=l=r = 0.0;
/*
.....Remember pick locations
*/
            um_ploctocc(&pick1.ploc,picked1);
            um_ploctocc(&pick2.ploc,picked2);
            um_vpnorm(pick1.ploc.transform, vpnorm1);
            um_vpnorm(pick2.ploc.transform, vpnorm2);
            ncl_proj_to_wp (picked1, vpnorm1, picked1);
            ncl_proj_to_wp (picked2, vpnorm2, picked2);
/*
.....Calculate and add the correct size modifier (large/small)
*/
            ci.key = key;
            um_get_all_geom(&ci, sizeof(ci));

            ncl_proj_to_wp (ci.center, vpnorm2, ci.center);
            d = um_dcccc(ci.center, picked1);
            l = um_dcccc(picked2, picked1);
            r = ci.radius;

            if (d <= r)
               {
                  if (l<=r)
                     ncl_add_token(&cmdbuf,NCL_small,NCL_comma);
                  else
                     ncl_add_token(&cmdbuf,NCL_large,NCL_comma);
               }
            else
               {
                  if (l<=d)
                     ncl_add_token(&cmdbuf,NCL_small,NCL_comma);
                  else
                     ncl_add_token(&cmdbuf,NCL_large,NCL_comma);
               }

            ncl_add_token(&cmdbuf,NCL_tanto,NCL_comma);
				ncl_get_label(&ci,str);
            status = ncl_add_token(&cmdbuf,str,NCL_nocomma);

            break;

         default:   /* Second picked is a curve */
/*
..... CI/CENTER,PT1,TANTO,CV2
*/
            ncl_add_token(&cmdbuf,NCL_tanto,NCL_comma);
            status = ncl_add_token(&cmdbuf, pick2.ploc.label,
                                                 NCL_nocomma);
            break;
      }

       if (status == NCL_DONE) continue;

       ncl_add_cmdbuf(&cmdbuf);
       ncl_call(&cmdbuf);
      }
done:;

   ncl_init_name_mod();
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : nclu_ci_ce_pt_tanto_ln()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ci_ce_pt_tanto_ln()

	{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ci_ce_pt_tanto_ln"));

	while (UU_TRUE)
		{
	   status = NCL_OKINPUT;
	   ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
	   if (!NCL_auto_label)
          status = ncl_add_name(&cmdbuf, 1);
      if (status == NCL_DONE) goto done;

/*
.....The buffer should be initialized before the label prompt.
	   ncl_init_cmdbuf(&cmdbuf);
*/
      ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);
		ncl_add_token(&cmdbuf, NCL_center, NCL_comma);

		status = ncl_add_label1(&cmdbuf, 69, UD_ncl_ptpv);
      if (status == NCL_DONE) goto done;

	   ncl_add_token(&cmdbuf, NCL_tanto, NCL_comma);
		status = ncl_add_label1(&cmdbuf, 70, UD_ncl_ln);
      if (status == NCL_DONE) continue;

		ncl_set_cmdmode(UU_TRUE);
      ncl_add_cmdbuf(&cmdbuf);
      ncl_call(&cmdbuf);
	   }
done:;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_ci_ce_pt_ra()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ci_ce_pt_ra()

	{
	NCL_cmdbuf cmdbuf, tmp;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ci_ce_pt_ra"));

	while (UU_TRUE)
	{
      status = NCL_OKINPUT;
/* 
.....Get radius, to append later 
*/
		ncl_init_cmdbuf(&tmp);
      status=ncl_add_str1(&tmp,84);
      if (status==NCL_NOINPUT) continue;
      if (status==NCL_DONE) goto done;

      while (UU_TRUE)
	   {
         status = NCL_OKINPUT;
/*
......Initialize the command buffer.
*/
		   ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		   if (!NCL_auto_label)
            status = ncl_add_name(&cmdbuf, 1);
         if (status == NCL_DONE) goto done;
/*
.....This goes before the label prompt.
		   ncl_init_cmdbuf(&cmdbuf);
*/
         ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);
			ncl_add_token(&cmdbuf, NCL_center, NCL_comma);

			status = ncl_add_label1(&cmdbuf, 71, UD_ncl_ptpv);
         if (status == NCL_DONE) break;

			ncl_add_token(&cmdbuf, NCL_radius, NCL_comma);
			ncl_add_token(&cmdbuf, tmp.cur_str, NCL_nocomma); 
			ncl_set_cmdmode(UU_TRUE);
         ncl_add_cmdbuf(&cmdbuf);
         ncl_call(&cmdbuf);
      }
	}
done:;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_ci_ce_pt_pt()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ci_ce_pt_pt()

{
	NCL_cmdbuf cmdbuf;
	int status;
	char str[256];

	uu_denter(UU_MTRC,(us,"nclu_ci_ce_pt_pt"));

   while (UU_TRUE)
	{
	   status = NCL_OKINPUT;
/*
.....Initialze the command buffer before asking for the 
.....label.
*/
	  	ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
	  	if (!NCL_auto_label)
       	  status = ncl_add_name(&cmdbuf, 1);
      
		ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);
		ncl_add_token(&cmdbuf, NCL_center, NCL_comma);
/*
.....Prompt for center point
*/
		status = ncl_get_dlabel(UD_DASPCKLOC,&str,73,UD_ncl_ptpv);
		if (status !=NCL_OKINPUT) goto done;

		while (status == NCL_OKINPUT)
		{
/*
......Put the center point into the buffer.
*/
			ncl_add_token(&cmdbuf,str,NCL_comma);
/*
.....Prompt for a circumference point.
*/
			status = ncl_add_label1(&cmdbuf, 74, UD_ncl_ptpv);
			if (status == NCL_OKINPUT)
			{
				ncl_set_cmdmode(UU_TRUE);
     			ncl_add_cmdbuf(&cmdbuf);
     			ncl_call(&cmdbuf);
/*
.....Initialize the command buffer and put label in command if needed,
.....and then CI/CENTER into the buffer.
*/
	  			ncl_init_cmdbuf(&cmdbuf);
	  			if (!NCL_auto_label)
     	  		   ncl_add_name(&cmdbuf, 1);
				ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);
				ncl_add_token(&cmdbuf, NCL_center, NCL_comma);
			}
		}
	}
done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_ci_ce_pt_tanto_ci()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ci_ce_pt_tanto_ci()

	{
	NCL_cmdbuf cmdbuf;
	int numint, status;
   UM_PLOCREC pick1, pick2;
   UU_KEY_ID key;
   UM_coord picked1, picked2;
   struct UM_circle_rec ci;
   UU_REAL d, l, r;
   char str[80];
   UM_vector vpnorm1, vpnorm2;

	uu_denter(UU_MTRC,(us,"nclu_ci_ce_pt_tanto_ci"));


	while (UU_TRUE)
	{
	   status = NCL_OKINPUT;
      d=l=r = 0.0;
/*
.....Need to initialize the command buffer before asking for 
.....a label.
*/
      ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
	   if (!NCL_auto_label)
          status = ncl_add_name(&cmdbuf, 1);
      if (status == NCL_DONE) goto done;

      ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);
	   ncl_add_token(&cmdbuf, NCL_center, NCL_comma);
/* 
.....Pick center point
*/
      ud_lgeo(UU_TRUE, UD_ncl_ptpv);
      ua_dl_pldas(UD_DASPCKLOC,UA_NCL,75,&pick1,1,&numint,1); 
      if (numint <= 0) break;

      ncl_add_token(&cmdbuf, pick1.ploc.label, NCL_comma);
/* 
.....Pick circle
*/
      ud_lgeo(UU_TRUE, UD_ncl_ci);
      ua_dl_pldas(UD_DASPCKLOC,UA_NCL,76,&pick2,1,&numint,1); 
      if (numint <= 0) continue;
      key = um_get_pickkey(&pick2.pent, 1);
/* 
.....Remember pick locations 
*/
      um_ploctocc(&pick1.ploc,picked1);
      um_ploctocc(&pick2.ploc,picked2);
      um_vpnorm(pick1.ploc.transform, vpnorm1);
      um_vpnorm(pick2.ploc.transform, vpnorm2);
      ncl_proj_to_wp (picked1, vpnorm1, picked1);
      ncl_proj_to_wp (picked2, vpnorm2, picked2);
/* 
.....Calculate and add the correct size modifier (large/small)
*/
      ci.key = key;
      um_get_all_geom(&ci, sizeof(ci));

      ncl_proj_to_wp (ci.center, vpnorm2, ci.center);
      d = um_dcccc(ci.center, picked1);
      l = um_dcccc(picked2, picked1);
      r = ci.radius;
         
      if (d <= r) 
         {
            if (l<=r) 
               ncl_add_token(&cmdbuf,NCL_small,NCL_comma);
            else
               ncl_add_token(&cmdbuf,NCL_large,NCL_comma);
         }
      else
         {
            if (l<=d) 
               ncl_add_token(&cmdbuf,NCL_small,NCL_comma);
            else
               ncl_add_token(&cmdbuf,NCL_large,NCL_comma);
         }
         
      ncl_add_token(&cmdbuf,NCL_tanto,NCL_comma);
		ncl_get_label(&ci,str);
      ncl_add_token(&cmdbuf,str,NCL_nocomma);

		ncl_set_cmdmode(UU_TRUE);
      ncl_add_cmdbuf(&cmdbuf);
      ncl_call(&cmdbuf);
	   }
done:;

	uu_dexit;
	}
/*   ------   OBSELETE ROUTINE   ------   */
#ifdef OBSELETE
/*********************************************************************
**    E_FUNCTION     : nclu_ci_ln_pt_ra()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/* Now used for CAD only --- Eduard */
nclu_ci_ln_pt_ra()

	{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ci_ln_pt_ra"));

	while (UU_TRUE)
		{
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;

		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);

		if (status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);

		if (status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbuf, NCL_tanto, NCL_comma);

		if (status == NCL_OKINPUT)
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 93, UD_ncl_ln);

		if (status == NCL_OKINPUT)
			status = ncl_add_modifier(&cmdbuf, NCL_XY_MODIFIER);

		if (status == NCL_OKINPUT)
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 94, UD_ncl_ptpv);

		if (status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbuf, NCL_radius, NCL_comma);

		if (status == NCL_OKINPUT)
			status = ncl_add_length(&cmdbuf, 95);

		if ((status == NCL_OKINPUT) || (status == NCL_DONE))
			{
/*
			ncl_set_cmdmode(UU_TRUE);
*/
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
			}

		}

	uu_dexit;
	}
#endif  /* OBSELETE */
/*********************************************************************
**    E_FUNCTION     : nclu_ci_tanto_ci_ci()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
nclu_ci_tanto_ci_ci()

	{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ci_tanto_ci_ci"));

	while (UU_TRUE)
		{
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;

		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);

		if (status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);

		if (status == NCL_OKINPUT)
			status = ncl_add_modifier(&cmdbuf, NCL_XY_MODIFIER);

		if (status == NCL_OKINPUT)
			status = ncl_add_modifier(&cmdbuf, NCL_CIRCLE_RELATION);

		if (status == NCL_OKINPUT)
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 99, UD_ncl_ci);

		if (status == NCL_OKINPUT)
			status = ncl_add_modifier(&cmdbuf, NCL_CIRCLE_RELATION);

		if (status == NCL_OKINPUT)
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 100, UD_ncl_ci);

		if (status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbuf, NCL_radius, NCL_comma);

		if (status == NCL_OKINPUT)
			status = ncl_add_length(&cmdbuf, 101);

		if ((status == NCL_OKINPUT) || (status == NCL_DONE))
			{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
			}

		}

	uu_dexit;
	}
*/
/*********************************************************************
**    E_FUNCTION     : nclu_ci_pt_tanto_ci()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/* Now used for CAD only --- Eduard */
nclu_ci_pt_tanto_ci()

	{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ci_pt_tanto_ci"));

	while (UU_TRUE)
		{
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;

		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);

		if (status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);

		if (status == NCL_OKINPUT)
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 102, UD_ncl_ptpv);

		if (status == NCL_OKINPUT)
			status = ncl_add_modifier(&cmdbuf, NCL_XY_MODIFIER);

		if (status == NCL_OKINPUT)
			status = ncl_add_modifier(&cmdbuf, NCL_CIRCLE_RELATION);

		if (status == NCL_OKINPUT)
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 103, UD_ncl_ci);

		if (status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbuf, NCL_radius, NCL_comma);

		if (status == NCL_OKINPUT)
			status = ncl_add_length(&cmdbuf, 104);

		if ((status == NCL_OKINPUT) || (status == NCL_DONE))
			{
/*
			ncl_set_cmdmode(UU_TRUE);
*/
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
			}

		}

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_ci_tanto_ln_ci()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
nclu_ci_tanto_ln_ci()

	{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ci_tanto_ln_ci"));

	while (UU_TRUE)
		{
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;

		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);

		if (status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);

		if (status == NCL_OKINPUT)
			status = ncl_add_modifier(&cmdbuf, NCL_XY_MODIFIER);

		if (status == NCL_OKINPUT)
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 105, UD_ncl_ln);

		if (status == NCL_OKINPUT)
			status = ncl_add_modifier(&cmdbuf, NCL_XY_MODIFIER);

		if (status == NCL_OKINPUT)
			status = ncl_add_modifier(&cmdbuf, NCL_CIRCLE_RELATION);

		if (status == NCL_OKINPUT)
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 106, UD_ncl_ci);

		if (status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbuf, NCL_radius, NCL_comma);

		if (status == NCL_OKINPUT)
			status = ncl_add_length(&cmdbuf, 107);

		if ((status == NCL_OKINPUT) || (status == NCL_DONE))
			{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
			}
		}

	uu_dexit;
	}
*/
/*********************************************************************
**    E_FUNCTION     : nclu_ci_canon_params()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
*********************************************************************/

void nclu_ci_canon_params()

	{
	NCL_cmdbuf cmdbuf, tmp, tmp1;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ci_canon_params"));

	while (UU_TRUE)
	{
      status = NCL_OKINPUT;
/* 
.....Get radius, to append later 
*/
		ncl_init_cmdbuf(&tmp);
      status=ncl_add_str1(&tmp,84);
      if (status==NCL_NOINPUT) continue;
      if (status==NCL_DONE) goto done;

	while (UU_TRUE)
	   {
      status = NCL_OKINPUT;
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		if (!NCL_auto_label)
           status = ncl_add_name(&cmdbuf, 1);
      if (status == NCL_DONE) goto done;
	   ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);
	   ncl_add_token(&cmdbuf, NCL_canon, NCL_comma);

		status = ncl_add_coord(&cmdbuf, 111, 3);
		if (status != NCL_OKINPUT) goto repeat;

	   while (UU_TRUE)
		   {
			   status = ncl_add_vector1(&cmdbuf, 112); 
		      if (status == NCL_NOINPUT) continue;
            if (status==NCL_DONE) break;

	         while (UU_TRUE)
		         {
		           ncl_init_cmdbuf(&tmp1);
                 status = ncl_add_vector1(&tmp1, 113);  
		           if (status != NCL_OKINPUT) goto finish;
              
	              while (UU_TRUE)
		              {
			              status = ncl_add_str1(&tmp1, 114); 
		                 if (status == NCL_NOINPUT) continue;
                       if (status==NCL_DONE) break;
                       strcat(tmp.cur_str, tmp1.cur_str); 
			              goto finish;
		              }
               }

		   } /* while #3 */

		} /* while #2 */

finish:;
            strcat(cmdbuf.cur_str, tmp.cur_str); 
				ncl_set_cmdmode(UU_TRUE);
            ncl_add_cmdbuf(&cmdbuf);
			   ncl_call(&cmdbuf);
repeat:;
	} /* while #1 */
done:;

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_ci_canon_geom()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ci_canon_geom()

	{
	NCL_cmdbuf cmdbuf, tmp;
	int status, reltyp;

	uu_denter(UU_MTRC,(us,"nclu_ci_canon_geom"));

	while (UU_TRUE)
	{
      status = NCL_OKINPUT;
/* 
.....Get radius, to append later 
*/
		ncl_init_cmdbuf(&tmp);
      status=ncl_add_str1(&tmp,84);
      if (status==NCL_NOINPUT) continue;
      if (status==NCL_DONE) goto done;

	   while (UU_TRUE)
		{
		   ncl_init_cmdbuf(&cmdbuf);
		   status = NCL_OKINPUT;
/*
.....If not autolabel, then get label
*/
         if (!NCL_auto_label)
            status = ncl_add_name(&cmdbuf, 1);
         if (status == NCL_DONE) break;

         ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);
         ncl_add_token(&cmdbuf, NCL_canon, NCL_comma);

		   status = ncl_add_label_rel1(&cmdbuf,115,UD_ncl_ptpv,&reltyp);
         if (status == NCL_DONE) break;

         if (reltyp == NCL_POINT_REL || reltyp == UM_POINT_REL)
         { 
            status = ncl_add_label1(&cmdbuf,116,UD_ncl_ptpve);
            if (status == NCL_DONE) goto repeat;
         }
         goto finish;
repeat:;
      } /* while #2 */

finish:;
	   ncl_add_token(&cmdbuf, tmp.cur_str, NCL_nocomma);
		ncl_add_label1(&cmdbuf, 118, UD_ncl_pl);
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);

      } /* while #1 */

done:;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_ci_transl()
**       Translate a circle by a vector
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void nclu_ci_transl()

	{
	NCL_cmdbuf cmdbuf;
	int status,save_loc;
	char str[256];
	int markval=0;

	uu_denter(UU_MTRC,(us,"nclu_ci_transl"));

	save_loc = UD_locint;
	UD_MARK(markval,UU_FALSE);
	if (markval != 0)
	{
		UD_locint = save_loc;
		UD_UNMARK (markval);
		return;
	}
	while (UU_TRUE)
	{
		status = NCL_OKINPUT;
/*
.....Initialize command buffer before adding anything to it.
*/
		ncl_init_cmdbuf(&cmdbuf);

	   if (!NCL_auto_label)
          status = ncl_add_name(&cmdbuf, 1);
      if (status == NCL_DONE) goto done;

      ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);
/* 
.....Made this change so that the user can use multiple
.....offsets and only have to pick the circle once.  Once
.....done is selected, the prompt will ask for another circle
.....to offset.  JLS  6/3/99
		status = ncl_add_label1(&cmdbuf, 85, UD_ncl_ci);
*/
		status = ncl_get_dlabel(UD_DASPCKLOC,&str, 85, UD_ncl_ci);
		if (status != NCL_OKINPUT) goto done;
		UD_locint = UD_STRING;
		while (status == NCL_OKINPUT)
		{
/*
.....Put the circle into the command and then prompt for
.....the offset vector.
*/
			ncl_add_token(&cmdbuf,str,NCL_comma);
/*
.....not use vector because vector can't allow (0,0,0)
*/
/*		   status = ncl_add_vector1(&cmdbuf, 86);  */
			status = ncl_add_coord(&cmdbuf, 86, 3);
/*
.....Process the command.
*/
			if (status == NCL_OKINPUT)
      	{
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
/*
.....Initialize the new command. Get label if needed and
.....put CI/ into the buffer.
*/
				ncl_init_cmdbuf(&cmdbuf);
	   		if (!NCL_auto_label)
					status = ncl_add_name(&cmdbuf, 1);
      		ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);
			}
      }
		UD_locint = save_loc;
	}
done:;
	UD_locint = save_loc;
	UD_UNMARK (markval);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_ci_offset()
**       Offset a circle inside or outside (change the radius by the
**       offset factor)
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ci_offset()
{
	NCL_cmdbuf cmdbuf;
	char str[256];
	int numint,status,relnum;
	UU_KEY_ID key;
	UM_PLOCREC pick;
	UM_coord picked;
	UM_vector vpnorm;

	uu_denter(UU_MTRC,(us,"nclu_ci_offset"));

	while (UU_TRUE)
	{
		status = NCL_OKINPUT;
/*
.....Initialize command buffer before adding anything to it.
*/
		ncl_init_cmdbuf(&cmdbuf);

		if (!NCL_auto_label) status = ncl_add_name(&cmdbuf, 1);
		if (status == NCL_DONE) goto done;

		ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);
		ncl_add_token(&cmdbuf, NCL_offset, NCL_comma);
/* 
..... a user can use multiple offsets with the once picked circle.  When
..... done is selected, the prompt will ask for another circle to offset.
*/
		ud_lgeo(UU_TRUE, UD_ncl_ci);

		ua_dl_pldas(UD_DASPCKLOC,UA_NCL,85,&pick,1,&numint,1); 
		if (numint <= 0) goto done;

		key = um_get_pickkey(&pick.pent, 1);
		ur_retrieve_data_relnum(key, &relnum);
		if (relnum != UM_CIRCLE_REL)
		{
            uu_uerror0(/*curve not picked*/UM_MODEL,323);
            continue;
		}
		um_ploctocc(&pick.ploc,picked);
		um_vpnorm(pick.ploc.transform, vpnorm);
			
		add_ci_label2(str,key,relnum,picked,vpnorm);

		while (status == NCL_OKINPUT)
		{
/*
.....Put the circle into the command and then prompt for
.....the offset factor.
*/
			ncl_add_token(&cmdbuf,str,NCL_comma);
			status = ncl_add_str(&cmdbuf, 434, NCL_nocomma);
/*
.....Process the command.
*/
			if (status == NCL_OKINPUT)
			{
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
/*
.....Initialize the new command. Get label if needed and
.....put CI/ into the buffer.
*/
				ncl_init_cmdbuf(&cmdbuf);
				if (!NCL_auto_label) status = ncl_add_name(&cmdbuf, 1);
				ncl_add_token(&cmdbuf, NCL_ci, NCL_nocomma);
				ncl_add_token(&cmdbuf, NCL_offset, NCL_comma);
			}
		}
	}
done:;
	ud_unlimit();

	uu_dexit;
}
