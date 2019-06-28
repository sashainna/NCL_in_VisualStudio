
/*********************************************************************
**    NAME         : aarclen.c
**       CONTAINS:
**				ua_iboxcir
**				ua_arclen_arcang
**				ua_arclen_create
**				ua_arclen
**				
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       aarclen.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:30
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) aarclen.c 4.3 2/28/89 16:58:12 single"};
#else
static char uu_sccsident[]={"@(#) aarclen.c 4.3 2/28/89 16:58:12 double"};
#endif

#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "dasnog.h"
#include "adraft.h"
#include "adrfcom.h"
#include "adrfdefs.h"
#include "mdcoord.h"

extern int 	UD_draftable[UD_NMENTWD];

/*********************************************************************
**    E_FUNCTION     : int		ua_arclen_create(al)
**		Get arc information. Project center,start,end points onto
**		this entity drafting plane.
**		NOTE: Extension lines:
**		(1) Arc angle less than 90 degrees-
**			extension lines will be perpendicular to arc chord -
**			this is what you see in ANSI standard and functional
**			spec.
**		(2) Arc angle greater than 90 degrees-
**			have REAL problems with above type extension lines,
**			so make them radially away or in from arc (like
**			angular dimension ).
**    PARAMETERS   
**       INPUT  : 
**          al		Arc length draft record
**       OUTPUT :  
**          output
**    RETURNS   : 0	arc length dimen created ok
**		  1	dimen arc does not intersect
**			extension lines.
**		<>0	other error - NO CREATE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int		ua_arclen_create(al)
struct UA_generic_draft	*al;
	{
	int	pair, iarc, i, j, iarcpts, retcode, nint;
	UU_REAL	vdotx,vdoty,delta_angle, d1, d2, angpt1, arcang, angpt2, angept,
		angspt,chlen, extlen, arad, diff, eang, drad, txang, alen, sang;
	UM_coord svec, dspt, ueext_cc, dimorig, dimvect, dept_cc, rad_vec, cpt,
		ept, box[4],usext_cc,corners[4], uvc, dspt_cc, uept_cc, extvect,
		cpln_origin,dimorig_cc,spt, uch_cc, uspt_cc, cpt_cc, arcpts[2],
		ept_cc,corners_cc[4], exteept_cc, int_point1, int_point2, vec1,
		temppt, pts_cc[2], spt_cc, extespt_cc, extsept_cc, dir_vec1,
		dir_vec2, extsspt_cc, xaxis, chcpt_cc, yaxis, zaxis, nvec, dept,
		tmp_vec, tmpx, tmpy, tmpz;

	uu_denter(UU_STRC,(us,"ua_arclen_create(al=%s)", "..."));

	retcode = 0;
	tmpx[0] = 1.000000e+000;
	tmpx[1] = 0.000000e+000;
	tmpx[2] = 0.000000e+000;
	tmpy[0] = 0.000000e+000;
	tmpy[1] = 1.000000e+000;
	tmpy[2] = 0.000000e+000;
	tmpz[0] = 0.000000e+000;
	tmpz[1] = 0.000000e+000;
	tmpz[2] = 1.000000e+000;

	ua_getcpln(&((*al)),cpln_origin,xaxis,yaxis,zaxis);
	ua_setmcpln(&((*al)));
	uc_draft_arc((*al).asso_blk[0].key,cpt,&(arad),&(arcang),
	nvec,spt,ept,&(alen));
	if( ( um_dot(zaxis,nvec)<0.000000e+000 ) )
		{
		um_vctovc(spt,temppt);
		um_vctovc(ept,spt);
		um_vctovc(temppt,ept);
		}
	(*al).dim_value = alen;
        if( ( (*al).txt_entry==0 ) )
	{
	        ua_set_dim_text(&((*al)));
	}
	if( ( (*al).txt_orent!=0 ) )
		{
		um_vcmnvc((*al).dim_origin,cpt,tmp_vec);
		um_unitvc(tmp_vec,vec1);
		vdotx = um_dot(vec1,tmpx);
		vdoty = um_dot(vec1,tmpy);
		if( ( vdotx>0.000000e+000 ) )
			{
			if( ( vdoty>0.000000e+000 ) )
				{
				txang = um_angle2p(vec1,tmpy,tmpz);
				txang = ( -txang );
				}
			else
				{
				tmpy[1] = -1.000000e+000;
				txang = um_angle2p(tmpy,vec1,tmpz);
				}
			}
		else
			{
			if( ( vdoty>0.000000e+000 ) )
				{
				txang = um_angle2p(tmpy,vec1,tmpz);
				}
			else
				{
				tmpy[1] = -1.000000e+000;
				txang = um_angle2p(vec1,tmpy,tmpz);
				txang = ( -txang );
				}
			}
		(*al).txt_blk[0].tangle = txang;
		}
	ua_box_site(&((*al)),corners,dimorig);
	ua_box_frame(&((*al)),corners);
	um_nptpln(cpt,cpln_origin,zaxis,cpt);
	um_nptpln(spt,cpln_origin,zaxis,spt);
	um_nptpln(ept,cpln_origin,zaxis,ept);
	um_mcstoccs(0,cpt,cpt_cc);
	um_mcstoccs(0,spt,spt_cc);
	um_mcstoccs(0,ept,ept_cc);
	um_mcstoccs(0,dimorig,dimorig_cc);
	um_mcstoccs(0,corners[0],corners_cc[0]);
	um_mcstoccs(0,corners[1],corners_cc[1]);
	um_mcstoccs(0,corners[2],corners_cc[2]);
	um_mcstoccs(0,corners[3],corners_cc[3]);
	switch( (*al).txt_orent )
		{
		case 2:
			{
			um_vctovc(corners_cc[0],rad_vec);
			rad_vec[0] = ( rad_vec[0]+( sin(txang)*( (*al).txt_blk[0].
			    txt_size*(*al).txt_blk[0].line_spacing ) ) );
			rad_vec[1] = ( rad_vec[1]-( cos(txang)*( (*al).txt_blk[0].
			    txt_size*(*al).txt_blk[0].line_spacing ) ) );
			}
			break;
		case 3:
			{
			um_vctovc(corners_cc[3],rad_vec);
			rad_vec[0] = ( rad_vec[0]-( sin(txang)*( (*al).txt_blk[0].
			    txt_size*(*al).txt_blk[0].line_spacing ) ) );
			rad_vec[1] = ( rad_vec[1]+( cos(txang)*( (*al).txt_blk[0].
			    txt_size*(*al).txt_blk[0].line_spacing ) ) );
			}
			break;
		case 1:
		case 0:
			{
			um_vctovc(dimorig_cc,rad_vec);
			}
			break;
		}
	drad = um_dcccc(rad_vec,cpt_cc);
	if( ( arcang<1.570796e+000 ) )
		{
		chlen = um_dcccc(spt_cc,ept_cc);
		um_vcmnvc(ept_cc,spt_cc,tmp_vec);
		um_unitvc(tmp_vec,uch_cc);
		um_vctmsc(uch_cc,( chlen/2.000000e+000 ),tmp_vec);
		um_vcplvc(spt_cc,tmp_vec,chcpt_cc);
		if( ( drad<arad ) )
			{
			um_vcmnvc(cpt_cc,chcpt_cc,tmp_vec);
			um_unitvc(tmp_vec,usext_cc);
			}
		else
			{
			um_vcmnvc(chcpt_cc,cpt_cc,tmp_vec);
			um_unitvc(tmp_vec,usext_cc);
			}
		um_vctovc(usext_cc,ueext_cc);
		}
	else
		{
		if( ( drad<arad ) )
			{
			um_vcmnvc(cpt_cc,spt_cc,tmp_vec);
			um_unitvc(tmp_vec,usext_cc);
			um_vcmnvc(cpt_cc,ept_cc,tmp_vec);
			um_unitvc(tmp_vec,ueext_cc);
			}
		else
			{
			um_vcmnvc(spt_cc,cpt_cc,tmp_vec);
			um_unitvc(tmp_vec,usext_cc);
			um_vcmnvc(ept_cc,cpt_cc,tmp_vec);
			um_unitvc(tmp_vec,ueext_cc);
			}
		}
	um_ilncir(spt_cc,usext_cc,cpt_cc,tmpz,drad,&(nint),pts_cc);
	if( ( nint!=2 ) )
		{
		retcode = 1;
		goto funcexit;
		}
	if( ( um_dcccc(pts_cc[0],spt_cc)<um_dcccc(pts_cc[1], spt_cc) ) )
		{
		um_vctovc(pts_cc[0],extsept_cc);
		}
	else
		{
		um_vctovc(pts_cc[1],extsept_cc);
		}
	um_ilncir(ept_cc,ueext_cc,cpt_cc,tmpz,drad,&(nint),pts_cc);
	if( ( nint!=2 ) )
		{
		retcode = 1;
		goto funcexit;
		}
	if( ( um_dcccc(pts_cc[0],ept_cc)<um_dcccc(pts_cc[1],
	ept_cc) ) )
		{
		um_vctovc(pts_cc[0],exteept_cc);
		}
	else
		{
		um_vctovc(pts_cc[1],exteept_cc);
		}
	if( ( (*al).txt_orent==0 ) )
		{
		ua_iboxcir(corners_cc,cpt_cc,tmpz,drad,&(iarcpts),arcpts) ;
		if( ( iarcpts!=2 ) )
			{
			retcode = 2;
			goto funcexit;
			}
		}
	iarc = ( (*al).arc_blk_use+1 );
	(*al).arc_blk[iarc-1].subtype = dim_arc;
	(*al).arc_blk[iarc-1].arc.line_font = UA_dim_line_font;
	(*al).arc_blk[iarc-1].arc.line_density = UA_dim_line_dens;
	(*al).arc_blk[iarc-1].arc.color = UA_dim_line_color;
	um_vctovc(cpt,(*al).arc_blk[iarc-1].center_pt);
	(*al).arc_blk[iarc-1].radius = drad;
	ua_arclen_seang((*al).arrow_size,cpt_cc,extsept_cc,
	exteept_cc,&(drad),&(angspt),&(angept));
	(*al).arrow_blk[0].aangle = angspt;
	(*al).arrow_blk[1].aangle = angept;
	um_ccstomcs(0,extsept_cc,(*al).arrow_blk[0].location);
	um_ccstomcs(0,exteept_cc,(*al).arrow_blk[1].location);
	angspt = ua_arclen_arcang(cpt_cc,extsept_cc,uvc);
	angept = ua_arclen_arcang(cpt_cc,exteept_cc,uvc);
	if( ( fabs(( angept-angspt ))<UA_FUZZ ) )
		{
		angept = ( angspt+6.283185e+000 );
		}
	if( ( (*al).txt_orent!=0 ) )
		{
		(*al).arc_blk[iarc-1].angles[0] = angspt;
		(*al).arc_blk[iarc-1].angles[1] = angept;
		(*al).arc_blk[iarc-1].num_pts = 2;
		}
	else
		{
		(*al).arc_blk[iarc-1].angles[0] = angspt;
		(*al).arc_blk[iarc-1].angles[3] = angept;
		angpt1 = ua_arclen_arcang(cpt_cc,arcpts[0],uvc);
		angpt2 = ua_arclen_arcang(cpt_cc,arcpts[1],uvc);
		if( ( angept>angspt ) )
			{
			if( ( angpt2>angpt1 ) )
				{
				(*al).arc_blk[iarc-1].angles[1] = angpt1;
				(*al).arc_blk[iarc-1].angles[2] = angpt2;
				}
			else
				{
				(*al).arc_blk[iarc-1].angles[1] = angpt2;
				(*al).arc_blk[iarc-1].angles[2] = angpt1;
				}
			}
		else
			{
			if( ( ( angpt1>angspt )&&( angpt2>angspt ) ) )
				{
				if( ( angpt2>angpt1 ) )
					{
					(*al).arc_blk[iarc-1].angles[1]= angpt1;
					(*al).arc_blk[iarc-1].angles[2]= angpt2;
					}
				else
					{
					(*al).arc_blk[iarc-1].angles[1]= angpt2;
					(*al).arc_blk[iarc-1].angles[2]= angpt1;
					}
				}
			else
				{
				if( ( angpt1>angspt ) )
					{
					(*al).arc_blk[iarc-1].angles[1]= angpt1;
					(*al).arc_blk[iarc-1].angles[2]= angpt2;
					}
				else
					{
					if( ( angpt2>angspt ) )
						{
						(*al).arc_blk[iarc-1].angles[1] = angpt2;
						(*al).arc_blk[iarc-1].angles[2] = angpt1;
						}
					else
						{
						if( ( angpt2<angpt1 ) )
							{
							(*al).arc_blk[iarc-1].angles[1] = angpt2;
							(*al).arc_blk[iarc-1].angles[2] = angpt1;
							}
						else
							{
							(*al).arc_blk[iarc-1].angles[1] = angpt1;
							(*al).arc_blk[iarc-1].angles[2] = angpt2;
							}
						}
					}
				}
			}
		(*al).arc_blk[iarc-1].num_pts = 4;
			{
			int		us_t169;
			us_t169 = (*al).arc_blk[iarc-1].num_pts;
			pair = 1;
			for(;;)
				{
				if( pair > us_t169 ) 	break;
				delta_angle = ( (*al).arc_blk[iarc-1].angles[( pair+1 )-1]-
				    (*al).arc_blk[iarc-1].angles[pair-1] );
				if( ( fabs(delta_angle)<UA_FUZZ ) )
					{
					(*al).arc_blk[iarc-1].num_pts = ( (*al).arc_blk[iarc-1].
					    num_pts-2 );
					if( ( pair==1 ) )
						{
						(*al).arc_blk[iarc-1].angles[0] = (*al).arc_blk[iarc-1].
						    angles[2];
						(*al).arc_blk[iarc-1].angles[1] = (*al).arc_blk[iarc-1].
						    angles[3];
						}
					}
us_l167:
				pair += 2;
				}
us_l168: ;
			}
		}
	if( ( (*al).arc_blk[iarc-1].num_pts>2 ) )
		{
		d1 = fabs(( (*al).arc_blk[iarc-1].angles[0]-(*al).arc_blk[
		    iarc-1].angles[1] ));
		d2 = fabs(( (*al).arc_blk[iarc-1].angles[2]-(*al).arc_blk[
		    iarc-1].angles[3] ));
		if( ( ( d1+d2 )>6.283185e+000 ) )
			{
			retcode = 2;
			goto funcexit;
			}
		}
	(*al).arc_blk_use = iarc;
	if ((*al).draft_stand == UA_ISO)
		ua_arc_length_symbol(&(*al),corners);
	(*al).arrow_blk_use = 2;
	um_vctmsc(usext_cc,(*al).gap_to_geo,tmp_vec);
	um_vcplvc(spt_cc,tmp_vec,extsspt_cc);
	um_vctmsc(ueext_cc,(*al).gap_to_geo,tmp_vec);
	um_vcplvc(ept_cc,tmp_vec,extespt_cc);
	um_vctmsc(usext_cc,(*al).ext_past_line,tmp_vec);
	um_vcplvc(extsept_cc,tmp_vec,extsept_cc);
	um_vctmsc(ueext_cc,(*al).ext_past_line,tmp_vec);
	um_vcplvc(exteept_cc,tmp_vec,exteept_cc);
	if( ( (*al).ext_line_sup!=3 ) )
		{
		i = (*al).line_blk_use + 1;
		(*al).line_blk_use = i;
		(*al).line_blk[i-1].num_pts = 0;
		(*al).line_blk[i-1].subtype = ext_line;
		(*al).line_blk[i-1].line.line_font = UA_ext_line_font;
		(*al).line_blk[i-1].line.line_density = UA_ext_line_dens;
		(*al).line_blk[i-1].line.color = UA_ext_line_color;
		j = 0;
		if( ( (*al).ext_line_sup!=1 ) )
			{
			j = ( j+1 );
			um_ccstomcs(0,extsspt_cc,(*al).line_blk[i-1].line_seg[j-1]);
			j = ( j+1 );
			um_ccstomcs(0,extsept_cc,(*al).line_blk[i-1].line_seg[j-1]);
			}
		if( ( (*al).ext_line_sup!=2 ) )
			{
			j = ( j+1 );
			um_ccstomcs(0,extespt_cc,(*al).line_blk[i-1].line_seg[j-1]);
			j = ( j+1 );
			um_ccstomcs(0,exteept_cc,(*al).line_blk[i-1].line_seg[j-1]);
			}
		(*al).line_blk[i-1].num_pts = j;
		}
funcexit:
	ua_resetmcpln();
	uu_dexit;
	return(retcode);
	}

/*********************************************************************
**    E_FUNCTION     : void		ua_arclen_seang(size, cpt_cc, spt_cc, ept_cc,
**															radius, sang, eang)
**			Determine arrowhead angles
**    PARAMETERS   
**       INPUT  : 
**				size						arrowhead size
**				cpt_cc					center of arc
**				spt_cc					start point of the arc
**				ept_cc					end point of the arc
**				radius					arc radius
**       OUTPUT :  
**				sang						angle at the starting point
**				eang						angle at the ending point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_arclen_seang(size, cpt_cc, spt_cc, ept_cc, radius, sang, eang)
UU_REAL	size;
UU_REAL	(*radius);
UU_REAL	(*sang);
UU_REAL	(*eang);
UM_coord	cpt_cc;
UM_coord	spt_cc;
UM_coord	ept_cc;
	{
	UU_REAL	sine, cosine, ang1, ang2, ang;
	UM_coord	pt1, v1, v2, v3, tmp_vec, tmpx, tmpy, tmpz;

	uu_denter(UU_STRC,(us,
		"ua_arclen_seang(size=%g,cpt_cc=<%g,%g,%g>,spt_cc=<%g,%g,%g>,ept_cc=<%g,%g,%g>)",
		size, cpt_cc[0],cpt_cc[1],cpt_cc[2], 
		spt_cc[0],spt_cc[1],spt_cc[2],
		ept_cc[0],ept_cc[1],ept_cc[2]));

	ang = ( size/(*radius) );
	sine = sin(ang);
	cosine = cos(ang);
	um_vcmnvc(spt_cc,cpt_cc,v1);
	tmpx[0] = 1.000000e+000;
	tmpx[1] = 0.000000e+000;
	tmpx[2] = 0.000000e+000;
	tmpz[0] = 0.000000e+000;
	tmpz[1] = 0.000000e+000;
	tmpz[2] = 1.000000e+000;
	um_cross(tmpz,v1,v2);
	pt1[0] = ( ( ( sine*v2[0] )+( cosine*v1[0] ) )+cpt_cc[0] );
	pt1[1] = ( ( ( sine*v2[1] )+( cosine*v1[1] ) )+cpt_cc[1] );
	pt1[2] = ( ( ( sine*v2[2] )+( cosine*v1[2] ) )+cpt_cc[2] );
	um_cross(v1,tmpz,v3);
	um_vcmnvc(pt1,cpt_cc,v2);
	um_cross(v2,tmpz,v1);
	ang1 = um_angle2p(tmpx,v1,tmpz);
	ang2 = um_angle2p(tmpx,v3,tmpz);
	if( ( fabs(( ang1-ang2 ))>1.000000e+000 ) )
		{
		(*sang) = ( ang1+ang2 );
		}
	else
		{
		(*sang) = ( ( ang1+ang2 )/2.000000e+000 );
		}
	um_vcmnvc(ept_cc,cpt_cc,v1);
	um_cross(tmpz,v1,v2);
	pt1[0] = ( ( ( ( -sine )*v2[0] )+( cosine*v1[0] ) )+cpt_cc
	    [0] );
	pt1[1] = ( ( ( ( -sine )*v2[1] )+( cosine*v1[1] ) )+cpt_cc
	    [1] );
	pt1[2] = ( ( ( ( -sine )*v2[2] )+( cosine*v1[2] ) )+cpt_cc
	    [2] );
	um_cross(tmpz,v1,v3);
	um_vcmnvc(pt1,cpt_cc,v2);
	um_cross(tmpz,v2,v1);
	ang1 = um_angle2p(tmpx,v1,tmpz);
	ang2 = um_angle2p(tmpx,v3,tmpz);
	if( ( fabs(( ang1-ang2 ))>1.000000e+000 ) )
		{
		(*eang) = ( ang1+ang2 );
		}
	else
		{
		(*eang) = ( ( ang1+ang2 )/2.000000e+000 );
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_iboxcir(corners, cpt, nvec, radius, icirpts, 
**			Intersect a box and a circle. return the (2) intersect points.
**    PARAMETERS   
**       INPUT  : 
**				corners[4]				ll,lr,ur,ul corners points
**				cpt						circle cGenter point
**				nvec						circle normal vector
**				radius					circle radius
**       OUTPUT :  
**				icirpts					number intersect points
**				cirpts[2]				returned intersect points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_iboxcir(corners, cpt, nvec, radius, icirpts, 
cirpts)
UM_coord	corners[4], cpt, nvec, cirpts[2];
UU_REAL	radius;
int		(*icirpts);
	{
	int	ipt2, ipt1, nint;
	UM_coord	uvc, pts[2], uvc1, uvc2, us_t189, us_t190, us_t191, us_t193,
				us_t192;

	uu_denter(UU_STRC,(us,"ua_iboxcir(cpt=<%g,%g,%g>,nvec=<%g,%g,%g>,radius=%g, icirpts=%d)", 
		cpt[0],cpt[1],cpt[2],nvec[0],nvec[1],nvec[2],radius,*icirpts));

	(*icirpts) = 0;

	for(ipt1=0;ipt1<4;ipt1++)
		{
		if( ( ipt1<3 ) )
			{
			ipt2 = ( ipt1+1 );
			}
		else
			{
			ipt2 = 0;
			}
		um_vcmnvc(corners[ipt2],corners[ipt1],us_t189);
		um_unitvc(us_t189,uvc);
		um_ilncir(corners[ipt1],uvc,cpt,nvec,radius,&(nint),pts);
		if( ( nint==2 ) )
			{
			um_vcmnvc(pts[0],corners[ipt1],us_t190);
			um_unitvc(us_t190,uvc1);
			um_vcmnvc(pts[0],corners[ipt2],us_t191);
			um_unitvc(us_t191,uvc2);
			if( ( um_dot(uvc1,uvc2)<0.000000e+000 ) )
				{
				(*icirpts) = ( (*icirpts)+1 );
				um_vctovc(pts[0],cirpts[(*icirpts)-1]);
				}
			else
				{
				um_vcmnvc(pts[1],corners[ipt1],us_t192);
				um_unitvc(us_t192,uvc1);
				um_vcmnvc(pts[1],corners[ipt2],us_t193);
				um_unitvc(us_t193,uvc2);
				if( ( um_dot(uvc1,uvc2)<0.000000e+000 ) )
					{
					(*icirpts) = ( (*icirpts)+1 );
					um_vctovc(pts[1],cirpts[(*icirpts)-1]);
					}
				}
			}
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_arclen()
**       User interaction routine for the creation of 
**			arc length dimensions.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_arclen()
	{
	struct UA_generic_draft	al;
	struct UA_PLOCREC	plocrec;
	int		relation, prev_txt_blk_use, locations, status, j, subtype,
				key, prev_key;
	UM_coord	svec, cpt, ept, cpln_origin, spt, dim_origin, xaxis, yaxis,
				zaxis, nvec;
	UU_REAL	arcang, arad, alen;
	UU_LOGICAL	redo, first, ok;


	uu_denter(UU_STRC,(us," ua_arclen()"));

	subtype = 49;
	first = UU_TRUE;
again:
	ua_init_entity(49,subtype,&(al));
	ua_getcpln(&(al),cpln_origin,xaxis,yaxis,zaxis);
	redo = UU_FALSE;
entity:
	ud_lgeo(UU_TRUE,UD_draftable);
	status = ua_select_ent_subf(106,&(al),1,&(plocrec));
	switch( status )
		{
		case 0:
			{
			uu_dexit;
			return;
			}
		case 2:
			{
			if( ( first==UU_FALSE ) )
				{
				redo = UU_TRUE;
				al.key = prev_key;
				status = uc_retrieve_data(&(al),sizeof(struct 
				    UA_generic_draft	));
				if( ( status==0 ) )
					{
					al.arc_blk_use = 0;
					al.line_blk_use = 0;
					al.arrow_blk_use = 0;
					al.asso_blk_use = 1;
					al.txt_blk_use = ( prev_txt_blk_use-1 );
					j = 1;
					for(;;)
						{
						if( j > 10 ) 	break;
						al.txt_blk[j-1].tangle = UA_text_ang;
us_l194:
						j++ ;
						}
us_l195: 
					;
					goto origin;
					}
				else
					{
					first = UU_TRUE;
					prev_key = 0;
					goto again;
					}
				}
			else
				{
				uu_uerror0(13,36);
				goto entity;
				}
			}
		case 1:
			{
			status = uc_draft_type(al.asso_blk[0].key,&(relation));
			if( ( relation!=2 ) )
				{
				uu_uerror0(13,6);
				goto entity;
				}
			uc_draft_arc(al.asso_blk[0].key,cpt,&(arad),&(arcang),nvec
			    ,spt,ept,&(alen));
			if( ( fabs(um_dot(nvec,zaxis))<UA_FUZZ ) )
				{
				uu_uerror0(13,17);
				goto entity;
				}
			}
		}
origin:
	status = ua_ent_origin_subf(30,&(al));
	if( ( status!=1 ) )
		{
		goto entity;
		}
	al.asso_blk_use = 1;

	ok = ua_text_subf(&(al));
	if( ( !ok ) )
		{
		goto origin;
		}
	prev_txt_blk_use = al.txt_blk_use;
	um_nptpln(al.dim_origin,cpln_origin,zaxis,dim_origin);
	um_vctovc(dim_origin,al.dim_origin);
	um_vctovc(cpt, al.asso_blk[0].location);
	status = ua_arclen_create(&(al));
	if( redo )
		{
		status = ua_update_entity(prev_key,&(al));
		if( ( status!=0 ) )
			{
			ua_create_entity(&(al),&(key));
			}
		}
	else
		{
		ua_create_entity(&(al),&(key));
		}
	uc_display(&(al));
	prev_key = al.key;
	first = UU_FALSE;
	goto again;
	}

/*********************************************************************
**    E_FUNCTION     : UU_REAL	ua_arclen_arcang(cpt, pt, uvc)
**		Determine angle of radius line from cpt to pt from xaxis.
**    PARAMETERS   
**       INPUT  : 
**				cpt				center point
**				pt					radius point
**       OUTPUT :  
**				uvc				unit vector from center to radius
**    RETURNS      : angle (0- 2PI) from xaxis to radial line.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL	ua_arclen_arcang(cpt, pt, uvc)
UM_coord	cpt, pt, uvc;
	{
	UU_REAL	ang;
	UM_coord tmp_vec, tmpx, tmpz;

	uu_denter(UU_STRC,(us,
		"ua_arclen_arcang(cpt=<%g,%g,%g>, pt=<%g,%g,%g>, uvc=%s)",
		cpt[0],cpt[1],cpt[2], pt[0],pt[1],pt[2], "..."));
	um_vcmnvc(pt,cpt,tmp_vec);
	um_unitvc(tmp_vec,uvc);
	tmpx[0] = 1.000000e+000;
	tmpx[1] = 0.000000e+000;
	tmpx[2] = 0.000000e+000;
	tmpz[0] = 0.000000e+000;
	tmpz[1] = 0.000000e+000;
	tmpz[2] = 1.000000e+000;
	ang = um_angle2p(tmpx,uvc,tmpz);
	uu_dexit;
	return(ang);
	}


/*********************************************************************
**    E_FUNCTION     : int	ua_arc_length_symbol(al,corners)
**		Function to create the arc length indication symbol
**		above the dimension text. It is currently done by
**		createing a line block above the dimension text in the
**		shape of an arc.
**    PARAMETERS   
**       INPUT  : 
**          al		Arc length draft record
**	corners		corners of the box containing the text
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_arc_length_symbol(al,corner)
struct UA_generic_draft	*al;
UM_coord corner[4];
{
	UU_REAL		arc_dis;
	UU_REAL		pi = 3.14159;
	UM_coord	cent_pt,down_dir;
	UM_coord	text_len,act_center;
	int		i,n,m;
	uu_denter(UU_STRC,(us,"ua_arc_length_symbol(corner[0]=<%g,%g,%g>)",
		corner[0][0],corner[0][1],corner[0][2]));
	
	um_vcmnvc(corner[0],corner[3],down_dir);
	um_unitvc(down_dir,down_dir);
	um_vcmnvc(corner[2],corner[3],text_len);
	um_vctmsc(text_len,(UU_REAL)0.5,act_center);
	um_vcplvc(corner[3],act_center,act_center);
	n = ( (*al).arc_blk_use+1 );
	(*al).arc_blk_use = n;
	(*al).arc_blk[n-1].subtype = dim_arc;
	(*al).arc_blk[n-1].arc.line_font = UA_dim_line_font;
	(*al).arc_blk[n-1].arc.line_density = UA_dim_line_dens;
	(*al).arc_blk[n-1].arc.color = UA_dim_line_color;
	arc_dis = 0.75 * (*al).char_size;
	um_vctmsc(down_dir,arc_dis,cent_pt);
	um_vcplvc(act_center,cent_pt,(*al).arc_blk[n-1].center_pt);
	(*al).arc_blk[n-1].radius = 1.05 * (*al).char_size;
	(*al).arc_blk[n-1].angles[0] = (*al).txt_blk[0].tangle + (pi / 4);
	(*al).arc_blk[n-1].angles[1] = (*al).txt_blk[0].tangle + (3 * (pi / 4));
	(*al).arc_blk[n-1].num_pts = 2;

	uu_dexit;
}
