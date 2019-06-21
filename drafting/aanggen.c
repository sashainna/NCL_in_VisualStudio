/*********************************************************************
**    NAME         : aanggen.c
**       CONTAINS:
**    		 ua_angular_generate(ad, spt1, ept1, spt2, ept2)
**    		 ua_text_angle_calc(vec1, vdotx, vdoty, txang)
**    		 
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       aanggen.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:29
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) aanggen.c 4.5 8/3/89 16:57:18 single"};
#else
static char uu_sccsident[]={"@(#) aanggen.c 4.5 8/3/89 16:57:18 double"};
#endif

#include "ustrings.h"
#include "usysdef.h"
#include "mdcoord.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"
#include "adrfdefs.h"

extern int UA_angular_subtype;

/*********************************************************************
**    E_FUNCTION     : ua_angular_generate(ad, spt1, ept1, spt2, ept2)
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

int		ua_angular_generate(ad, spt1, ept1, spt2, ept2)
	struct UA_generic_draft	(*ad);
	UM_coord	spt1, ept1, spt2, ept2;
	{
	int		nrep, save_site, sign, fret, nint, iangles, over, retcode, itxt,
				interior_type, ilinept, iarcpts, ia, iarrow, complement_type,
				iline, iarc, j, i, supplement_type, ext_suppress1,
				ext_suppress2;
	UU_REAL	y_box, temp, vdotx, vdoty, temp_rad, tempang, angdrot, angd2,
				angmid, angd1, angout, ang1, ang2, ang3, mvdist, dradx, distc,
				diste, diff, angd, drad, txang, dists;
	UU_REAL	proj, temp_dist, ang12;
	UU_REAL	angles[6], angles2[2];
	UM_coord	ept1_cc, ept2_cc, udim, npt1_cc, boxcentpt_cc, trvc, npt2_cc, x_cc,
				z_cc, dimarcpt_cc, dimorig, spt1_cc, spt2_cc, cornerpts_cc[4], ept,
				ext1spt, ext2spt, uvc, cpt, rad_vec, npt, cpln_origin, spt, y_cc,
				ext1ept_cc, dimorig_cc, ext2ept_cc, newboxcentpt_cc, trv, cpt_cc,
				ept_cc, ext1spt_cc, ext2spt_cc, npt_cc, vec1, vec2, temppt, uext1,
				spt_cc, uext2, uvc1, uvc2, uvc3, npt1, boxcentpt, tmp_v1, tmp_v2,
				npt2, ipts1_cc[2], xaxis, ipts2_cc[2], yaxis, zaxis, cornerpts[4];
	UU_LOGICAL	text_flag, inside_cone, swapted;

	uu_denter(UU_STRC,(us,"SAL ua_angular_generate(ad=%s, spt1=<%g,%g,%g>,  ept1=<%g,%g,%g>,\
		spt2=<%g,%g,%g>, ept2=<%g,%g,%g>)", "...",spt1[0],spt1[1],
		spt1[2],ept1[0],ept1[1],ept1[2],spt2[0],spt2[1],spt2[2],
		ept2[0],ept2[1],ept2[2]));

	retcode = 0;
	itxt = 1;
	text_flag = UU_FALSE;
	swapted = UU_FALSE;
	ua_getcpln(&((*ad)),cpln_origin,xaxis,yaxis,zaxis);
	ua_setmcpln(&((*ad)));
	um_mcstoccs(0,spt1,spt1_cc);
	um_mcstoccs(0,ept1,ept1_cc);
	um_mcstoccs(0,spt2,spt2_cc);
	um_mcstoccs(0,ept2,ept2_cc);
	um_mcstoccs(0,(*ad).asso_blk[0].location,npt1_cc);
	um_mcstoccs(0,(*ad).asso_blk[1].location,npt2_cc);
	um_vcmnvc(ept1_cc,spt1_cc,tmp_v1);
	um_unitvc(tmp_v1,uvc1);
	um_vcmnvc(ept2_cc,spt2_cc,tmp_v1);
	um_unitvc(tmp_v1,uvc2);

	um_ilnln(spt1_cc,uvc1,spt2_cc,uvc2,&(nint),cpt_cc);
	if( ( nint==0 ) )
		{
		/* Check special case of repetitive dimensioning of 180 
			or 360. */
		if ((UA_angular_subtype == UA_ANGULAR_REP_INT_DIM )
			|| (UA_angular_subtype == UA_ANGULAR_REP_COM_DIM))
			{
			proj = um_dot(uvc1,uvc2);
			if( ( ( 1.000000e+000-fabs(proj) )<1.000000e-005 ) )
				{
				uu_uerror0(UA_DRAFTING,60);
	 			retcode = UU_FAILURE;
				goto funcexit;
				}
			}
		else
			{
			uu_uerror0(13,9);
	 		retcode = UU_FAILURE;
			goto funcexit;
			}
		}
	if( um_cceqcc(spt1_cc,cpt_cc) )
		{
		um_vctovc(ept1_cc,npt1_cc);
		}
	else if( um_cceqcc(ept1_cc,cpt_cc) )
		{
		um_vctovc(spt1_cc,npt1_cc);
		}
	if( um_cceqcc(spt2_cc,cpt_cc) )
		{
		um_vctovc(ept2_cc,npt2_cc);
		}
	else if( um_cceqcc(ept2_cc,cpt_cc) )
		{
		um_vctovc(spt2_cc,npt2_cc);
		}
	if( um_cceqcc(npt1_cc,cpt_cc) )
		{
		uu_uerror0(13,12);
	 	retcode = UU_FAILURE;
		goto funcexit;
		}
	if( um_cceqcc(npt2_cc,cpt_cc) )
		{
		uu_uerror0(13,12);
	 	retcode = UU_FAILURE;
		goto funcexit;
		}
	um_vcmnvc(npt1_cc,cpt_cc,tmp_v1);
	um_unitvc(tmp_v1,uvc1);
	um_vcmnvc(npt2_cc,cpt_cc,tmp_v1);
	um_unitvc(tmp_v1,uvc2);

	x_cc[0] = 1.000000e+000;
	x_cc[1] = 0.000000e+000;
	x_cc[2] = 0.000000e+000;
	y_cc[0] = 0.000000e+000;
	y_cc[1] = 1.000000e+000;
	y_cc[2] = 0.000000e+000;
	z_cc[0] = 0.000000e+000;
	z_cc[1] = 0.000000e+000;
	z_cc[2] = 1.000000e+000;

	ang1 = um_angle2p(x_cc,uvc1,z_cc);
	ang2 = um_angle2p(x_cc,uvc2,z_cc);
	ang12 = um_angle2p(uvc1,uvc2,z_cc);

	if( ( ( ang1==0.000000e+000 )&&( ang2>=3.141593e+000 ) ) )
		{
		ang1 = 6.283185e+000;
		}
	if( ( ( ang2==0.000000e+000 )&&( ang1>=3.141593e+000 ) ) )
		{
		ang2 = 6.283185e+000;
		}
	if( ( ( ang1==6.283185e+000 )&&( ang2<=3.141593e+000 ) ) )
		{
		ang1 = 0.000000e+000;
		}
	if( ( ( ang2==6.283185e+000 )&&( ang1<=3.141593e+000 ) ) )
		{
		ang2 = 0.000000e+000;
		}

	um_mcstoccs(0,(*ad).dim_origin,dimorig_cc);

	switch( UA_angular_subtype )
		{
		case 1:
		case UA_ANGULAR_REP_INT_DIM:
			{
			um_vcmnvc(dimorig_cc,cpt_cc,tmp_v1);
			um_unitvc(tmp_v1,uvc3);
			ang3 = um_angle2p(x_cc,uvc3,z_cc);
			if( ( ang1<ang2 ) )
				if( ( ang1<ang3 )&&( ang3<ang2 ) )
					interior_type = 1;
				else
					interior_type = 2;
			else
				{
				if( ( ang2<ang3 )&&( ang3<ang1 ) )
					interior_type = 1;
				else
					interior_type = 2;
				ua_angular_swappt(spt1_cc,spt2_cc);
				ua_angular_swappt(ept1_cc,ept2_cc);
				ua_angular_swappt(npt1_cc,npt2_cc);
				ua_angular_swappt(uvc1,uvc2);
				temp = ang1;
				ang1 = ang2;
				ang2 = temp;
	         ang12 = um_angle2p(uvc1,uvc2,z_cc);
				swapted = UU_TRUE;
				}

			/* lines in first and fourth quadrants */
			if ( ( ang2>=4.712389e+000 )&&( ang1<=1.570796e+000 ) )
				{
				if( ( ang3>ang2 )||( ang3<ang1 ) )
					interior_type = 1;
				else
					interior_type = 2;
				ua_angular_swappt(spt1_cc,spt2_cc);
				ua_angular_swappt(ept1_cc,ept2_cc);
				ua_angular_swappt(npt1_cc,npt2_cc);
				ua_angular_swappt(uvc1,uvc2);
				temp = ang1;
				ang1 = ang2;
				ang2 = temp;
				swapted = UU_TRUE;
				}

         /* lines in 2nd and 4th quadrants */
			if (( ( ang1>=1.570796e+000 ) && (ang1<=3.141593e+000 ) )
				&&  ( ang2>=4.712389e+000 )  && ( ang12 >= 3.141593 ) )
				{
				if( ( ang3>ang2 )||( ang3<ang1 ) )
					interior_type = 1;
				else
					interior_type = 2;
				ua_angular_swappt(spt1_cc,spt2_cc);
				ua_angular_swappt(ept1_cc,ept2_cc);
				ua_angular_swappt(npt1_cc,npt2_cc);
				ua_angular_swappt(uvc1,uvc2);
				temp = ang1;
				ang1 = ang2;
				ang2 = temp;
				swapted = UU_TRUE;
				}

         /* lines in 1st and 3rd quadrants */
			if (( ( ang2>=3.141593e+000 ) && (ang2<=4.712389e+000 ) )
				&&  ( ang1<=1.570796e+000 )  && ( ang12 >= 3.141593 ) )
				{
				if( ( ang3>ang2 )||( ang3<ang1 ) )
					interior_type = 1;
				else
					interior_type = 2;
				ua_angular_swappt(spt1_cc,spt2_cc);
				ua_angular_swappt(ept1_cc,ept2_cc);
				ua_angular_swappt(npt1_cc,npt2_cc);
				ua_angular_swappt(uvc1,uvc2);
				temp = ang1;
				ang1 = ang2;
				ang2 = temp;
				swapted = UU_TRUE;
				}

			/* Interior angle < 180 */
			if( ( ang12<3.141593e+000 ) )
				(*ad).dim_value = ang12;
			else
				(*ad).dim_value = ( 6.283185e+000-ang12 );

			if((*ad).asso_blk_use == 3)
				interior_type = 1;
			}
			break;
		case 2:
			{
			if( ( ang12<3.141593e+000 ) )
				{
				(*ad).dim_value = ( 3.141593e+000-ang12 );
				supplement_type = 1;
				}
			else
				{
				(*ad).dim_value = ( ang12-3.141593e+000 );
				supplement_type = 2;
				}
			}
			break;
		case 3:
		case UA_ANGULAR_REP_COM_DIM:
			{
			(*ad).arrow_place = 0;
			if( ( ang12<3.141593e+000 ) )
				{
				(*ad).dim_value = ( 6.283185e+000-ang12 );
				complement_type = 1;
				}
			else
				{
				(*ad).dim_value = ang12;
				complement_type = 2;
				}
			}
			break;
		}
	uu_denter2(UU_STRC,(us,"SAL ua_angular_generate(dim_origin=<%g,%g,%g>,\
dimorig_cc=<%g,%g,%g>,ang1=%g,ang2=%g,ang12=%g,ang3=%g)",(*ad).dim_origin[0],
(*ad).dim_origin[1],(*ad).dim_origin[2],dimorig_cc[0],dimorig_cc[1],
dimorig_cc[2],ang1,ang2,ang12,ang3));
	uu_dexit;
	if( ( (*ad).txt_entry==0 ) )
		{
		if ((UA_angular_subtype == UA_ANGULAR_REP_INT_DIM )
			|| (UA_angular_subtype == UA_ANGULAR_REP_COM_DIM))
			{
			nrep = (int) ((*ad).dim2_value);
			ua_set_rep_dim_text(&((*ad)), &nrep);
			}
		else
			ua_set_dim_text(&((*ad)));
		}
	if( ( (*ad).txt_orent>=2 ) )
		{
		um_vcmnvc(dimorig_cc,cpt_cc,tmp_v1);
		um_unitvc(tmp_v1,vec1);
		vdotx = um_dot(vec1,x_cc);
		vdoty = um_dot(vec1,y_cc);
		um_cross(zaxis,vec1,vec2);
		diff = ua_dir_angle(zaxis,yaxis,vec2);
		if( ( diff>3.141593e+000 ) )
			{
			diff = ( diff-3.141593e+000 );
			}
		if(( diff<5.230000e-001 ) && ((*ad).draft_stand != UA_ISO) )
			{
			txang = 0.000000e+000;
			text_flag = UU_TRUE;
			}
		else
			{
			ua_text_angle_calc(vec1,vdotx,vdoty,&(txang));
			text_flag = UU_FALSE;
			}
		(*ad).txt_blk[0].tangle = txang;
		}
	save_site = (*ad).entity_site;
	(*ad).entity_site = 1;
	ua_box_site(&((*ad)),cornerpts,boxcentpt);
	ua_box_frame(&((*ad)),cornerpts);
	(*ad).entity_site = save_site;
	um_mcstoccs(0,boxcentpt,boxcentpt_cc);
	i = 1;
	for(;;)
		{
		if( i > 4 ) 	break;
		um_mcstoccs(0,cornerpts[i-1],cornerpts_cc[i-1]);
us_l167:
		i++ ;
		}
us_l168: 
	;
	if( ( (*ad).asso_blk_use>2 ) )
		{
		um_mcstoccs(0,(*ad).asso_blk[2].location,dimarcpt_cc);
		um_vctovc(dimarcpt_cc,rad_vec);
		drad = um_dcccc(dimarcpt_cc,cpt_cc);
		dradx = um_dcccc(boxcentpt_cc,cpt_cc);
		mvdist = ( drad-dradx );
		um_vcmnvc(boxcentpt_cc,dimarcpt_cc,tmp_v1);
		if( ( fabs(mvdist)>1.000000e-004 ) )
			{
			um_vcmnvc(dimorig_cc,tmp_v1,dimorig_cc);
			um_ccstomcs(0,dimorig_cc,(*ad).dim_origin);
			um_vcmnvc(boxcentpt_cc,tmp_v1,boxcentpt_cc);
			i = 1;
			for(;;)
				{
				if( i > 4 ) 	break;
				um_vcmnvc(cornerpts_cc[i-1],tmp_v1,cornerpts_cc[i-1]);
us_l171:
				i++ ;
				}
us_l172: 
			;
			}
		}
	else
		{
		if( ( (*ad).txt_orent>=2 ) )
			{
			if( text_flag )
				{
					{
					UU_REAL	us_t173[3];
					UU_REAL	us_t174[3];
					um_vcmnvc(cornerpts_cc[3],cornerpts_cc[0],tmp_v2);
					um_vctmsc(tmp_v2,(UU_REAL) 1.0 / 2.000000e+000,tmp_v1);
					um_vcplvc(cornerpts_cc[0],tmp_v1,rad_vec);
					}
				}
			else
				{
				um_vctovc(boxcentpt_cc,rad_vec);
				}
			}
		else
			{
			um_vctovc(boxcentpt_cc,rad_vec);
			}
		}

	uu_denter2(UU_STRC,(us,"SAL ua_angular_generate(dimarcpt_cc=<%g,%g,%g>\
rad_vec=<%g,%g,%g>,drad=%g,dradx=%g,mvdist=%g,trvc=<%g,%g,%g>)",dimarcpt_cc[0],
dimarcpt_cc[1],dimarcpt_cc[2],rad_vec[0],rad_vec[1],rad_vec[2],drad,dradx,
mvdist,trvc[0],trvc[1],trvc[2]));
	uu_dexit;
	drad = um_dcccc(rad_vec,cpt_cc);
	um_ilncir(npt1_cc,uvc1,cpt_cc,z_cc,drad,&(nint),ipts1_cc);
	um_vcmnvc(ipts1_cc[0],cpt_cc,tmp_v1);
	um_unitvc(tmp_v1,uext1);

	uu_denter2(UU_STRC,(us,"SAL ua_angular_generate(cpt_cc=<%g,%g,%g>\
boxcentpt_cc=<%g,%g,%g> cornerpts_cc=<%g,%g,%g>)",cpt_cc[0],cpt_cc[1],cpt_cc[2],
boxcentpt_cc[0],boxcentpt_cc[1],boxcentpt_cc[2],cornerpts_cc[0],
cornerpts_cc[1],cornerpts_cc[2])); 
uu_dexit;
	if( ( um_dot(uvc1,uext1)<0.000000e+000 ) )
		{
		um_vctovc(ipts1_cc[0],temppt);
		um_vctovc(ipts1_cc[1],ipts1_cc[0]);
		um_vctovc(temppt,ipts1_cc[1]);
		}
	um_ilncir(npt2_cc,uvc2,cpt_cc,z_cc,drad,&(nint),ipts2_cc);
	um_vcmnvc(ipts2_cc[0],cpt_cc,tmp_v1);
	um_unitvc(tmp_v1,uext2);
	if( ( um_dot(uvc2,uext2)<0.000000e+000 ) )
		{
		um_vctovc(ipts2_cc[0],temppt);
		um_vctovc(ipts2_cc[1],ipts2_cc[0]);
		um_vctovc(temppt,ipts2_cc[1]);
		}
	switch( UA_angular_subtype )
		{
		case 1:
		case UA_ANGULAR_REP_INT_DIM:
			{
			if( ( interior_type==2 ) )
				{
				um_vctovc(ipts1_cc[1],npt1_cc);
				um_vcmnvc(npt1_cc,cpt_cc,tmp_v1);
				um_unitvc(tmp_v1,uvc1);
				um_vctovc(ipts2_cc[1],npt2_cc);
				um_vcmnvc(npt2_cc,cpt_cc,tmp_v1);
				um_unitvc(tmp_v1,uvc2);
				ang2 = um_angle2p(x_cc,uvc2,z_cc);
				ang1 = um_angle2p(x_cc,uvc1,z_cc);
				ang12 = um_angle2p(uvc1,uvc2,z_cc);
				ua_angular_swappt(ipts1_cc[0],ipts1_cc[1]);
				ua_angular_swappt(ipts2_cc[0],ipts2_cc[1]);
				}
			}
	uu_denter2(UU_STRC,(us,"SAL ua_angular_generate(ipts1_cc=<%g,%g,%g>\
ipts2_cc=<%g,%g,%g>,ang1=%g,ang2=%g,ang12=%g)",ipts1_cc[0],ipts1_cc[1],
ipts1_cc[2],ipts2_cc[0],ipts2_cc[1],ipts2_cc[2],ang1,ang2,ang12));
	uu_dexit;
		break;
		case 2:
			{
			switch( supplement_type )
				{
				case 1:
					{
					ua_angular_swappt(spt1_cc,spt2_cc);
					ua_angular_swappt(ept1_cc,ept2_cc);
					swapted = UU_TRUE;
					um_vctovc(ipts2_cc[1],npt1_cc);
					um_vcmnvc(npt1_cc,cpt_cc,tmp_v1);
					um_unitvc(tmp_v1,uvc1);
					um_vctovc(ipts1_cc[0],npt2_cc);
					um_vcmnvc(npt2_cc,cpt_cc,tmp_v1);
					um_unitvc(tmp_v1,uvc2);
					ang1 = um_angle2p(x_cc,uvc1,z_cc);
					ang2 = um_angle2p(x_cc,uvc2,z_cc);
					ang12 = um_angle2p(uvc1,uvc2,z_cc);
					ua_angular_swappt(ipts2_cc[0],ipts2_cc[1]);
					ua_angular_swappt(ipts1_cc[0],ipts2_cc[0]);
					ua_angular_swappt(ipts1_cc[1],ipts2_cc[1]);
					}
					break;
				case 2:
					{
					um_vctovc(ipts2_cc[1],npt2_cc);
					um_vcmnvc(npt2_cc,cpt_cc,tmp_v1);
					um_unitvc(tmp_v1,uvc2);
					ang2 = um_angle2p(x_cc,uvc2,z_cc);
					ang12 = um_angle2p(uvc1,uvc2,z_cc);
					ua_angular_swappt(ipts2_cc[0],ipts2_cc[1]);
					}
					break;
				}
			}
			break;
		case 3:
		case UA_ANGULAR_REP_COM_DIM:
			{
			if( ( complement_type==1 ) )
				{
				ua_angular_swappt(spt1_cc,spt2_cc);
				ua_angular_swappt(ept1_cc,ept2_cc);
				swapted = UU_TRUE;
				um_vctovc(ipts2_cc[0],npt1_cc);
				um_vctovc(ipts1_cc[0],npt2_cc);
				ua_angular_swappt(uvc1,uvc2);
				ua_angular_swapval(&(ang1),&(ang2));
				ang12 = ( 6.283185e+000-ang12 );
				ua_angular_swappt(ipts1_cc[0],ipts2_cc[0]);
				ua_angular_swappt(ipts1_cc[1],ipts2_cc[1]);
				}
			}
			break;
		}

		/* create extension lines if required */

	if(ad->ext_line_sup != UA_SUPPRESS_BOTH)
		{
		ext_suppress1 = UA_SUPPRESS_FIRST;
		ext_suppress2 = UA_SUPPRESS_SECOND;
		if(swapted)
			{
			ext_suppress1 = UA_SUPPRESS_SECOND;
			ext_suppress2 = UA_SUPPRESS_FIRST;
			}
		iline = (*ad).line_blk_use;

		/* test for line 1 */

		if(ad->ext_line_sup != ext_suppress1)
			{
			fret = um_ptinseg(spt1_cc,ipts1_cc[0],ept1_cc);
			if( ( fret==0 ) )
				{
				distc = um_dcccc(ipts1_cc[0],cpt_cc);
				diste = um_dcccc(ipts1_cc[0],ept1_cc);
				dists = um_dcccc(ipts1_cc[0],spt1_cc);
				if( ( ( dists<=distc )&&( dists<=diste ) ) )
					{
					um_vctmsc(uvc1,(*ad).gap_to_geo,tmp_v1);
					um_vcplvc(spt1_cc,tmp_v1,ext1spt_cc);
					}
				else
					{
					if( ( ( diste<=distc )&&( diste<=dists ) ) )
						{
						um_vctmsc(uvc1,(*ad).gap_to_geo,tmp_v1);
						um_vcplvc(ept1_cc,tmp_v1,ext1spt_cc);
						}
					else
						{
						um_vctmsc(uvc1,(*ad).gap_to_geo,tmp_v1);
						um_vcplvc(cpt_cc,tmp_v1,ext1spt_cc);
						}
					}
				um_vctmsc(uvc1,(*ad).ext_past_line,tmp_v1);
				um_vcplvc(ipts1_cc[0],tmp_v1,ext1ept_cc);
				if( ( iline==(*ad).line_blk_use ) )
					{
					iline = ( iline+1 );
					ilinept = 0;
					(*ad).line_blk[iline-1].num_pts = 0;
					(*ad).line_blk[iline-1].subtype = ext_line;
					(*ad).line_blk[iline-1].line.line_font = UA_ext_line_font;
					(*ad).line_blk[iline-1].line.line_density = UA_ext_line_dens
					    ;
					(*ad).line_blk[iline-1].line.color = UA_ext_line_color;
					}
				ilinept = ( ilinept+1 );
				um_ccstomcs(0,ext1spt_cc,(*ad).line_blk[iline-1].
																		line_seg[ilinept-1]);
				ilinept = ( ilinept+1 );
				um_ccstomcs(0,ext1ept_cc,(*ad).line_blk[iline-1].
																		line_seg[ilinept-1]);
				(*ad).line_blk[iline-1].num_pts = ilinept;
				}
			}

			/* test for line 2 */

		if(ad->ext_line_sup != ext_suppress2)
			{
			fret = um_ptinseg(spt2_cc,ipts2_cc[0],ept2_cc);
			if( ( fret==0 ) )
				{
				distc = um_dcccc(ipts2_cc[0],cpt_cc);
				diste = um_dcccc(ipts2_cc[0],ept2_cc);
				dists = um_dcccc(ipts2_cc[0],spt2_cc);
				if( ( ( dists<=distc )&&( dists<=diste ) ) )
					{
					um_vctmsc(uvc2,(*ad).gap_to_geo,tmp_v1);
					um_vcplvc(spt2_cc,tmp_v1,ext2spt_cc);
					}
				else
					{
					if( ( ( diste<=distc )&&( diste<=dists ) ) )
						{
						um_vctmsc(uvc2,(*ad).gap_to_geo,tmp_v1);
						um_vcplvc(ept2_cc,tmp_v1,ext2spt_cc);
						}
					else
						{
						um_vctmsc(uvc2,(*ad).gap_to_geo,tmp_v1);
						um_vcplvc(cpt_cc,tmp_v1,ext2spt_cc);
						}
					}
				um_vctmsc(uvc2,(*ad).ext_past_line,tmp_v1);
				um_vcplvc(ipts2_cc[0],tmp_v1,ext2ept_cc);
				if( ( iline==(*ad).line_blk_use ) )
					{
					iline = ( iline+1 );
					ilinept = 0;
					(*ad).line_blk[iline-1].num_pts = 0;
					(*ad).line_blk[iline-1].subtype = ext_line;
					(*ad).line_blk[iline-1].line.line_font = UA_ext_line_font;
					(*ad).line_blk[iline-1].line.line_density = UA_ext_line_dens
					    ;
					(*ad).line_blk[iline-1].line.color = UA_ext_line_color;
					}
				ilinept = ( ilinept+1 );
				um_ccstomcs(0,ext2spt_cc,(*ad).line_blk[iline-1].
																	line_seg[ilinept-1]);
				ilinept = ( ilinept+1 );
				um_ccstomcs(0,ext2ept_cc,(*ad).line_blk[iline-1].
																	line_seg[ilinept-1]);
				(*ad).line_blk[iline-1].num_pts = ilinept;
				}
			}
			(*ad).line_blk_use = iline;
		}
	angd = ua_angular_arcang(cpt_cc,boxcentpt_cc,udim);
	if( ( (*ad).txt_place==0 ) )
		{
		angmid = ( ( (*ad).dim_value/2.000000e+000 )+ang1 );
		if( ( angmid>6.283185e+000 ) )
			{
			angmid = ( angmid-6.283185e+000 );
			}
		if( ( angd>angmid ) )
			{
			angdrot = ( ( 6.283185e+000-angd )+angmid );
			}
		else
			{
			angdrot = ( angmid-angd );
			}
		um_vctovc(boxcentpt_cc,newboxcentpt_cc);
		ua_rotatept(newboxcentpt_cc,z_cc,cpt_cc,angdrot);
		if( ( (*ad).txt_orent<=1 ) )
			{
			um_vcmnvc(newboxcentpt_cc,boxcentpt_cc,trvc);
			um_vctovc(newboxcentpt_cc,boxcentpt_cc);
			um_vcplvc(dimorig_cc,trvc,dimorig_cc);
			i = 1;
			for(;;)
				{
				if( i > 4 ) 	break;
				um_vcplvc(cornerpts_cc[i-1],trvc,cornerpts_cc[i-1]);
us_l206:
				i++ ;
				}
us_l207: 
			;
			}
		else
			{
			um_vctovc(newboxcentpt_cc,boxcentpt_cc);
			um_ccstomcs(0,boxcentpt_cc,boxcentpt);
			ua_rotatept(dimorig_cc,z_cc,cpt_cc,angdrot);
			i = 1;
			for(;;)
				{
				if( i > 4 ) 	break;
				ua_rotatept(cornerpts_cc[i-1],z_cc,cpt_cc,angdrot);
us_l209:
				i++ ;
				}
us_l210: 
			;
			}
		um_ccstomcs(0,dimorig_cc,(*ad).dim_origin);
		um_ccstomcs(0,boxcentpt_cc,boxcentpt);
		um_vcmnvc(boxcentpt_cc,cpt_cc,tmp_v1);
		um_unitvc(tmp_v1,udim);

	uu_denter2(UU_STRC,(us,"SAL ua_angular_generate(newboxcentpt_cc=\
<%g,%g,%g> angd= %g angmid = %g angdrot = %g)", 
newboxcentpt_cc[0],newboxcentpt_cc[1], newboxcentpt_cc[2],angd,angmid,angdrot));
	uu_dexit;

	uu_denter2(UU_STRC,(us,"SAL ua_angular_generate(dim_value= %g\
txt_orent = %d cornerpts_cc=<%g,%g,%g>)",(*ad).dim_value,(*ad).txt_orent,
cornerpts_cc[0], cornerpts_cc[1], cornerpts_cc[2]));
	uu_dexit;
		if( ( (*ad).txt_orent>=2 ) )
			{
			um_ccstomcs(0,cpt_cc,cpt);
			um_vcmnvc(dimorig_cc,cpt_cc,tmp_v1);
			um_unitvc(tmp_v1,vec1);
			vdotx = um_dot(udim,x_cc);
			vdoty = um_dot(udim,y_cc);
			switch( (*ad).txt_orent )
				{
				case 2:
					{
					if( ( vdoty>=0.000000e+000 ) )
						{
						sign = 1;
						over = 1;
						}
					else
						{
						sign = -1;
						over = -1;
						}
					}
					break;
				case 3:
					{
					if( ( vdoty>=0.000000e+000 ) )
						{
						sign = -1;
						over = 1;
						}
					else
						{
						sign = 1;
						over = -1;
						}
					}
					break;
				}
			um_vcmnvc(boxcentpt_cc,cpt_cc,tmp_v1);
			um_unitvc(tmp_v1,trvc);
			if( text_flag )
				{
				temp_dist = 0.000000e+000;
				}
			else
				{
				y_box = um_dcccc(cornerpts_cc[3],cornerpts_cc[0]);
				temp_rad = um_dcccc(dimorig_cc,cpt_cc);
				temp_dist = ( ( (UU_REAL)over )*( temp_rad-drad ) );
				um_vctmsc(z_cc,( (UU_REAL)over ),tmp_v1);
				temp_dist = ( temp_dist*cos(um_angle2p(trvc,vec1,tmp_v1)) ) ;
				temp_dist = ( ( ( y_box/2.000000e+000 )-temp_dist )+(*ad).
				    txt_gap );
				if( ( temp_dist<=1.000000e-004 ) )
					{
					temp_dist = 0.000000e+000;
					}
				}
			um_vctmsc(trvc,( ( (UU_REAL)sign )*( temp_dist+(*ad).txt_gap)),tmp_v1);
			um_vcplvc(dimorig_cc,tmp_v1,dimorig_cc);
			um_ccstomcs(0,dimorig_cc,(*ad).dim_origin);
			um_ccstomcs(0,cpt_cc,cpt);
			um_vcmnvc(dimorig_cc,cpt_cc,tmp_v1);
			um_unitvc(tmp_v1,vec1);
			vdotx = um_dot(vec1,x_cc);
			vdoty = um_dot(vec1,y_cc);
			if( text_flag )
				{
				txang = 0.000000e+000;
				}
			else
				{
				ua_text_angle_calc(vec1,vdotx,vdoty,&(txang));
				}
			(*ad).txt_blk[0].tangle = txang;
			save_site = (*ad).entity_site;
			(*ad).entity_site = 1;
			ua_box_site(&((*ad)),cornerpts,boxcentpt);
			ua_box_frame(&((*ad)),cornerpts);
			(*ad).entity_site = save_site;
			i = 1;
			for(;;)
				{
				if( i > 4 ) 	break;
				um_mcstoccs(0,cornerpts[i-1],cornerpts_cc[i-1]);
us_l223:
				i++ ;
				}
us_l224: ;
			}
		}
	angd1 = um_angle2p(udim,uvc1,z_cc);
	angd2 = um_angle2p(uvc2,udim,z_cc);
	iarc = (*ad).arc_blk_use;
	iarc = ( iarc+1 );
	(*ad).arc_blk_use = iarc;
	(*ad).arc_blk[iarc-1].num_pts = 0;
	iarcpts = 0;
	(*ad).arc_blk[iarc-1].subtype = dim_arc;
	(*ad).arc_blk[iarc-1].arc.line_font = UA_dim_line_font;
	(*ad).arc_blk[iarc-1].arc.line_density = UA_dim_line_dens;
	(*ad).arc_blk[iarc-1].arc.color = UA_dim_line_color;
	um_ccstomcs(0,cpt_cc,(*ad).arc_blk[iarc-1].center_pt);
	(*ad).arc_blk[iarc-1].radius = drad;
	angout = ( ( 3.000000e+000*(*ad).arrow_size )/drad );
	if( ( ( angd2>3.141593e+000 )&&( angd1>3.141593e+000 ) ) )
		{
		switch( (*ad).arrow_place )
			{
			case 0:
				{
				if( ( (*ad).txt_orent>=2 ) )
					{
					iangles = 2;
					angles[0] = ang1;
					angles[1] = ang2;
					}
				else
					{
					ua_iboxarc(cornerpts_cc,cpt_cc,z_cc,drad,ang1,ang2,&(
						iangles),angles);
					}
				}
				break;
			case 1:
				{
				angles[0] = ( ang1-angout );
				if( ( angles[0]<0.000000e+000 ) )
					{
					angles[0] = ( 6.283185e+000+angles[0] );
					}
				angles[1] = ang1;
				angles[2] = ang2;
				angles[3] = ( ang2+angout );
				if( ( angles[3]>6.283185e+000 ) )
					{
					angles[3] = ( angles[3]-6.283185e+000 );
					}
				if((*ad).draft_stand == 1)
					{
					angles[4] = ang1;
					angles[5] = ang2;
					iangles = 6;
					}
				else
					iangles = 4;
				}
				break;
			}
		}
	else
		{
		if( ( angd1<angd2 ) )
			{
			switch( (*ad).arrow_place )
				{
				case 0:
					{
					if( ( (*ad).txt_orent>=2 ) )
						{
						iangles = 2;
						angles[0] = angd;
						angles[1] = ang2;
						}
					else
						{
						ua_iboxarc(cornerpts_cc,cpt_cc,z_cc,drad,angd,ang2,&(
							iangles),angles);
						}
					}
					break;
				case 1:
					{
					ua_iboxarc(cornerpts_cc,cpt_cc,z_cc,drad,angd,ang1,&(
						iangles),angles);
					angles[2] = ang2;
					angles[3] = ( ang2+angout );
					if( ( angles[3]>6.283185e+000 ) )
						{
						angles[3]  = ( angles[3]-6.283185e+000 );
						}
					if((*ad).draft_stand == 1)
						{
						angles[4] = angd;
						angles[5] = ang2;
						iangles = 6;
						}
					else
						iangles = 4;
					}
					break;
				}
			}
		else
			{
			switch( (*ad).arrow_place )
				{
				case 0:
					{
					ua_iboxarc(cornerpts_cc,cpt_cc,z_cc,drad,ang1,angd,&(
						iangles),angles);
					}
					break;
				case 1:
					{
					angles[0] = ( ang1-angout );
					if( ( angles[0]<0.000000e+000 ) )
						{
						angles[0] = ( 6.283185e+000+angles[0] );
						}
					angles[1] = ang1;
					ua_iboxarc(cornerpts_cc,cpt_cc,z_cc,drad,ang2,angd,&(
						iangles),angles2);
					angles[2] = angles2[0];
					angles[3] = angles2[1];
					if((*ad).draft_stand == 1)
						{
						angles[4] = ang1;
						angles[5] = angles2[0];
						iangles = 6;
						}
					else
						iangles = 4;
					}
					break;
				}
			}
		}
		{
		int		us_t234;
		us_t234 = iangles;
		ia = 1;
		for(;;)
			{
			if( ia > us_t234 ) 	break;
			iarcpts = ( iarcpts+1 );
			(*ad).arc_blk[iarc-1].angles[iarcpts-1] = angles[ia-1];
us_l232:
			ia++ ;
			}
us_l233:;
		}
	(*ad).arc_blk[iarc-1].num_pts = iarcpts;
		{
		int		us_t237;
		us_t237 = (*ad).arc_blk[iarc-1].num_pts;
		iarcpts = 1;
		for(;;)
			{
			if( iarcpts > us_t237 ) 	break;
us_l235:
			iarcpts++ ;
			}
us_l236:;
		}
	iarrow = (*ad).arrow_blk_use;
	iarrow = ( iarrow+1 );
	um_ilncir(npt1_cc,uvc1,cpt_cc,z_cc,drad,&(nint),ipts1_cc);
	(*ad).arrow_blk_use = iarrow;
	(*ad).arrow_blk[iarrow-1].arrow_type = UA_arrow_symbol;
	(*ad).arrow_blk[iarrow-1].arrow.line_font = 0;
	(*ad).arrow_blk[iarrow-1].arrow.line_density = UA_arrow_dens;
	(*ad).arrow_blk[iarrow-1].arrow.color = UA_arrow_color;
	um_ccstomcs(0,ipts1_cc[0],(*ad).arrow_blk[iarrow-1].location);
	switch( (*ad).arrow_place )
		{
		case 0:
			{
			(*ad).arrow_blk[iarrow-1].aangle = ua_arrowang(uvc1,drad,(*
			    ad).arrow_size,0);
			}
			break;
		case 1:
			{
			(*ad).arrow_blk[iarrow-1].aangle = ua_arrowang(uvc1,drad,(*ad).
							arrow_size,1);
			}
			break;
		}
	(*ad).arrow_blk[iarrow-1].size = (*ad).arrow_size;
	iarrow = ( iarrow+1 );
	um_ilncir(npt2_cc,uvc2,cpt_cc,z_cc,drad,&(nint),ipts2_cc);
	(*ad).arrow_blk_use = iarrow;
	(*ad).arrow_blk[iarrow-1].arrow_type = UA_arrow_symbol;
	(*ad).arrow_blk[iarrow-1].arrow.line_font = 0;
	(*ad).arrow_blk[iarrow-1].arrow.line_density = UA_arrow_dens;
	(*ad).arrow_blk[iarrow-1].arrow.color = UA_arrow_color;
	um_ccstomcs(0,ipts2_cc[0],(*ad).arrow_blk[iarrow-1].location);
	switch( (*ad).arrow_place )
		{
		case 0:
			{
			(*ad).arrow_blk[iarrow-1].aangle = ua_arrowang(uvc2,drad,(*
			    ad).arrow_size,1);
			}
			break;
		case 1:
			{
			(*ad).arrow_blk[iarrow-1].aangle = ua_arrowang(uvc2,drad,(*
			    ad).arrow_size,0);
			}
			break;
		}
	(*ad).arrow_blk[iarrow-1].size = (*ad).arrow_size;
	if( ( (*ad).dim_type==1 ) )
		{
		(*ad).line_blk_use = ( (*ad).line_blk_use+1 );
		iline = (*ad).line_blk_use;
		(*ad).line_blk[iline-1].subtype = misc_line;
		(*ad).line_blk[iline-1].line.line_font = UA_dim_line_font;
		(*ad).line_blk[iline-1].line.line_density = UA_dim_line_dens
		    ;
		(*ad).line_blk[iline-1].line.color = UA_dim_line_color;
		i = 1;
		for(;;)
			{
			if( i > 4 ) 	break;
			um_ccstomcs(0,cornerpts_cc[i-1],cornerpts[i-1]);
us_l240:
			i++ ;
			}
us_l241: 
		;
		(*ad).line_blk[iline-1].num_pts = 8;
		um_vctovc(cornerpts[0],(*ad).line_blk[iline-1].line_seg[0]);
		um_vctovc(cornerpts[1],(*ad).line_blk[iline-1].line_seg[1]);
		um_vctovc(cornerpts[1],(*ad).line_blk[iline-1].line_seg[2]);
		um_vctovc(cornerpts[2],(*ad).line_blk[iline-1].line_seg[3]);
		um_vctovc(cornerpts[2],(*ad).line_blk[iline-1].line_seg[4]);
		um_vctovc(cornerpts[3],(*ad).line_blk[iline-1].line_seg[5]);
		um_vctovc(cornerpts[3],(*ad).line_blk[iline-1].line_seg[6]);
		um_vctovc(cornerpts[0],(*ad).line_blk[iline-1].line_seg[7]);
		}
	if( ( text_flag==UU_TRUE ) )
		{
		(*ad).txt_orent = 0;
		}
funcexit:
	ua_resetmcpln();
	uu_dexit;
	return(retcode);
	}

/*********************************************************************
**    E_FUNCTION     : ua_text_angle_calc(vec1, vdotx, vdoty, txang)
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
ua_text_angle_calc(vec1, vdotx, vdoty, txang)
UU_REAL	vec1[3];
UU_REAL	vdotx;
UU_REAL	vdoty;
UU_REAL	(*txang);
	{
	UM_coord x_cc, y_cc, z_cc;

	uu_denter(UU_STRC,(us,"SAL ua_text_angle_calc(vec1=<%g,%g,%g>, vdotx=%g,\
		vdoty=%g, txang=%s)", vec1[0],vec1[1],vec1[2], vdotx, vdoty, "..."));

	y_cc[0] = 0.000000e+000;
	y_cc[1] = 1.000000e+000;
	y_cc[2] = 0.000000e+000;
	z_cc[0] = 0.000000e+000;
	z_cc[1] = 0.000000e+000;
	z_cc[2] = 1.000000e+000;

	if( ( vdotx>0.000000e+000 ) )
		{
		if( ( vdoty>0.000000e+000 ) )
			{
			(*txang) = um_angle2p(vec1,y_cc,z_cc);
			(*txang) = ( -(*txang) );
			}
		else
			{
			y_cc[1] = -1.000000e+000;
			(*txang) = um_angle2p(y_cc,vec1,z_cc);
			}
		}
	else
		{
		if( ( vdoty>0.000000e+000 ) )
			{
			(*txang) = um_angle2p(y_cc,vec1,z_cc);
			}
		else
			{
			y_cc[1] = -1.000000e+000;
			(*txang) = um_angle2p(vec1,y_cc,z_cc);
			(*txang) = ( -(*txang) );
			}
		}
	uu_dexit;
	}
