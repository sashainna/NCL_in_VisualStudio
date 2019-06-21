/*********************************************************************
**    NAME         : arepangdim.c
**       CONTAINS:
**    			ua_cre_repang_dim()
**
**    COPYRIGHT 1989 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       arepangdim.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:38
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) arepangdim.c 1.2 8/11/89 14:10:58 single"};
#else
static char uu_sccsident[]={"@(#) arepangdim.c 1.2 8/11/89 14:10:58 double"};
#endif

#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "dasnog.h"
#include "mdpick.h"
#include "mdcoord.h"
#include "adraft.h"
#include "adrf.h"
#include "adrfcom.h"
#include "adrfdefs.h"

extern int UD_draftable[UD_NMENTWD];
extern int UD_draft_line[UD_NMENTWD];
extern struct UA_generic_draft	ad_test;
extern int UA_angular_subtype;

/*********************************************************************
**    E_FUNCTION     : ua_cre_repang_dim(ad,plocrec1,spt1,ept1,spt2,ept2)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_cre_repang_dim(ad, plocrec1, spt1, ept1, spt2, ept2)
	struct UA_generic_draft	(*ad);
	struct UA_PLOCREC	*plocrec1;
	UM_coord	spt1, ept1, spt2, ept2;
	{
	struct UA_PLOCREC	plocrec2, plocrec3;
	int		n, relation1, relation, choice, num_arcs, nint,
				dim_type, key, curr_key, s_status, status, msgno;
	UM_coord	npt, cpln_origin, ewc, uvc1, base_vec, center,
				uvc2, uvcn, int_pt, xaxis, yaxis, zaxis, tmp_v1;
	UU_REAL	ang_eq, ang_test, differ;

	uu_denter(UU_STRC,(us,"SAL ua_cre_repang_dim()"));

	curr_key = (*ad).asso_blk[0].key;
	s_status = um_retrieve_data_relnum(curr_key,&(relation1));
	ua_getcpln(&((*ad)),cpln_origin,xaxis,yaxis,zaxis);
	if( ( relation1==UA_LINEAR_DIM ) )
		{
		ua_near_on_draft((*ad).asso_blk[0].key,plocrec1,ewc);
		ua_centerline_line((*ad).asso_blk[0].key,plocrec1,spt1, ept1);
		}
	else
		{
		uc_near_on_entity((*ad).asso_blk[0].key,&((*plocrec1).ndcloc),
		ewc);
		uc_draft_line((*ad).asso_blk[0].key,spt1,ept1);
		}
	um_nptpln(ewc,cpln_origin,zaxis,npt);
	um_vctovc(npt,(*ad).asso_blk[0].location);
	um_nptpln(spt1,cpln_origin,zaxis,int_pt);
	um_vctovc(int_pt,spt1);
	um_nptpln(ept1,cpln_origin,zaxis,int_pt);
	um_vctovc(int_pt,ept1);
		{
		UU_REAL	us_t146[3];
		um_vcmnvc(ept1,spt1,us_t146);
		um_unitvc(us_t146,uvc1);
		}

	/* Initialize the count. */
	n = 0;

entity_n:
	/* Obtain the set of equidistant angles to be dimensioned. */
	ud_lgeo(UU_TRUE,UD_draftable);
	/* Select next entity. */
	status = ua_select_ent_subf(171,&((*ad)),2,&(plocrec2));
	if( ( status==UA_REJECT ) )
		{
		/* Set the number of equidistant angles and the type. */
		if (ang_eq > 3.141593e+000)
			ang_eq = 6.283185e+00-ang_eq;
		if ((n * ang_eq) >= (3.141593e+000 - 1.000000e-5))
			{
			UA_angular_subtype = UA_ANGULAR_REP_COM_DIM;
			(*ad).subtype = UA_angular_subtype;
			}
		else
			{
			UA_angular_subtype = UA_ANGULAR_REP_INT_DIM;
			(*ad).subtype = UA_angular_subtype;
			}
		(*ad).dim2_value = (UU_REAL) n;
		goto create;
		}
	if( ( status!=UA_OPCOMPLETE ) )
		{
		goto entity_n;
		}
	(*ad).asso_blk_use = 2;
	curr_key = (*ad).asso_blk[1].key;
	s_status = um_retrieve_data_relnum(curr_key,&(relation));
	if( ( relation==UA_LINEAR_DIM ) )
		{
		ad_test.key = curr_key;
		uc_retrieve_data(&(ad_test),sizeof(struct UA_generic_draft));
		dim_type = ad_test.etype;
		num_arcs = ad_test.arc_blk_use;
		if( ( dim_type==UA_CENTERLINE ) )
			{
			if( ( num_arcs==0 ) )
				{
				/* Entity picked not a line. */
				uu_uerror0(UA_DRAFTING,8);
				goto entity_n;
				}
			}
		}
	else
		{
		status = uc_draft_type(curr_key,&(relation));
		if( ( relation!=UA_DRAFT_LINE ) )
			{
			/* Entity picked not a line. */
			uu_uerror0(UA_DRAFTING,8);
			goto entity_n;
			}
		}
	if( ( relation==UA_LINEAR_DIM ) )
		{
		ua_near_on_draft((*ad).asso_blk[1].key,&(plocrec2),ewc);
		ua_centerline_line((*ad).asso_blk[1].key,&(plocrec2),spt2, ept2);
		}
	else
		{
		uc_near_on_entity((*ad).asso_blk[1].key,&(plocrec2.ndcloc), ewc);
		uc_draft_line((*ad).asso_blk[1].key,spt2,ept2);
		}
	um_nptpln(ewc,cpln_origin,zaxis,npt);
	um_vctovc(npt,(*ad).asso_blk[1].location);
	um_nptpln(spt2,cpln_origin,zaxis,int_pt);
	um_vctovc(int_pt,spt2);
	um_nptpln(ept2,cpln_origin,zaxis,int_pt);
	um_vctovc(int_pt,ept2);
		{
		UU_REAL	us_t147[3];
		um_vcmnvc(ept2,spt2,us_t147);
		um_unitvc(us_t147,uvc2);
		}

	if (n == 0)
		{
		/* Obtain point of intersection. */
		um_ilnln(spt1,uvc1,spt2,uvc2,&nint,center);
		if (nint == 0)
			{
			status = ua_popmenu(17,&(choice));
			if( ( status==UA_OPCOMPLETE ) )
				{
				if( ( choice==1 ) )
					{
					uu_dexit;
					return(UA_FALSE);
					}
				if( ( choice==2 ) )
					{
					uu_dexit;
					return(UU_FAILURE);
					}
				}
			uu_dexit;
			return(UA_FALSE);
			}
		else
			{
			/* Adjust vectors to point of intersection. */
			um_vcmnvc((*ad).asso_blk[0].location,center,tmp_v1);
			um_unitvc(tmp_v1,uvc1);
			um_vcmnvc((*ad).asso_blk[1].location,center,tmp_v1);
			um_unitvc(tmp_v1,uvc2);
			}

		/* If first time, calculate equidistant angle. */
		ang_eq = um_angle(uvc1, uvc2);
	   um_vctovc(uvc2, uvcn);
		n = n + 1;
		goto entity_n;
		}
	else
		{
		/* Adjust vectors to point of intersection. */
		um_vcmnvc((*ad).asso_blk[1].location,center,tmp_v1);
		um_unitvc(tmp_v1,uvc2);
		ang_test = um_angle(uvc2, uvcn);
		differ = fabs(ang_test - ang_eq);	
		if(((int)((differ*(pow(10.0,((UU_REAL)((*ad).dim_places)))))+0.5)) != 0)
			{
			/* Dimension values not equal, cannot use REPETITIVE. */
			uu_uerror0(UA_DRAFTING,59);
			uu_dexit;
			return(UU_FAILURE);
			}
		else
			{
			n = n + 1;
	   	um_vctovc(uvc2, uvcn);
			goto entity_n;
			}
		}
create:
	/* correct display */
	uu_dexit;
	return(UA_TRUE);
	}
