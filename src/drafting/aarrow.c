
/*********************************************************************
**    NAME         : arrow.c
**       CONTAINS:
**      		 ua_arrowhead
**      		 
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       aarrow.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:30
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) aarrow.c 4.3 7/7/89 12:32:09 single"};
#else
static char uu_sccsident[]={"@(#) aarrow.c 4.3 7/7/89 12:32:09 double"};
#endif


#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"

us_init_aarrow()
{
}

/*********************************************************************
**    E_FUNCTION     : void	ua_arrowhead(arrow_use_count, 
**					array_of_arrow_blocks, scale_fac)
**       Displays arrowheads.
**    PARAMETERS   
**       INPUT  : 
**          INTEGER  :  arrow_use_count  --  Number of arrowheads.
**          UA_arrow_blk  :  array_of_arrow_blocks  -- Description of
**                                                       arrowheads.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_arrowhead(arrow_use_count,array_of_arrow_blocks, scale_fac)
int		arrow_use_count;
struct UA_arrow_blk	array_of_arrow_blocks[];
UU_REAL	scale_fac;
	{
	int		step;
	int		key;
	int		tcolor;
	int		no_plot;
	int		i;
	int		j;
	int		k;
	int		tstyle;
	int		error;
	UU_REAL	a_wc[13][3];
	UU_REAL	ang;
	UU_REAL	v1[3];
	UU_REAL	v2[3];
	UU_REAL	v3[3];
	UU_REAL	density;
	UU_REAL	rotate_trans[4][3];
	UU_REAL	radius[3];
	UU_REAL	delta;
	UU_REAL	rotate[4][3];
	UU_REAL	a[12][3];
	UU_REAL	ratio;
	UU_REAL	count;
	UU_REAL	rotate_trans2[4][3];
	UU_REAL	a_cc[12][3];
	UU_REAL	location[3];

	uu_denter(UU_STRC,(us,"SAL ua_arrowhead(arrow_use_count=%d,array_of_arrow_blocks=%s, scale_fac=%g)",
		 arrow_use_count, "...",
			scale_fac));
		{
		int		us_t107;
		us_t107 = arrow_use_count;
		i = 1;
		for(;;)
			{
			if( i > us_t107 ) 	break;
			um_mcstoccs(0,array_of_arrow_blocks[i-1].location,location);
				{
				UU_REAL	us_t108[4][3];
				UU_REAL	us_t109[3];
				UU_REAL	us_t110[4][3];
				us_t109[0] = 0.000000e+000;
				us_t109[1] = 0.000000e+000;
				us_t109[2] = 1.000000e+000;
				um_rottf(us_t109,array_of_arrow_blocks[i-1].aangle,us_t108);
				um_disptf(location,us_t110);
				um_tftmtf(us_t108,us_t110,rotate_trans);
				}
			tcolor = array_of_arrow_blocks[i-1].arrow.color;
			tstyle = 1;
			density = array_of_arrow_blocks[i-1].arrow.line_density;
			key = 0;
			error = us_set_dispattr(key,tcolor,tstyle,density,2);
			if( ( array_of_arrow_blocks[i-1].arrow_type==UA_SINGLE_OPEN ) )
				{
				a[0][0] = -1.000000e+000;
				a[0][1] = -1.500000e-001;
				a[0][2] = 0.000000e+000;
				a[1][0] = 0.000000e+000;
				a[1][1] = 0.000000e+000;
				a[1][2] = 0.000000e+000;
				a[2][0] = -1.000000e+000;
				a[2][1] = 1.500000e-001;
				a[2][2] = 0.000000e+000;
				j = 1;
				for(;;)
					{
					if( j > 3 ) 	break;
					um_vctmsc(a[j-1],array_of_arrow_blocks[i-1].size,a[j-1]);
					um_cctmtf(a[j-1],rotate_trans,a[j-1]);
					um_cpltrans(a[j-1],a_wc[j-1]);
us_l111:
					j++ ;
					}
us_l112: 
				;
				error = gpolyline3(3,a_wc);
				}
			else if( ( array_of_arrow_blocks[i-1].arrow_type==UA_SINGLE_CLOSED ) )
				{
				a[0][0] = -1.000000e+000;
				a[0][1] = -1.500000e-001;
				a[0][2] = 0.000000e+000;
				a[1][0] = 0.000000e+000;
				a[1][1] = 0.000000e+000;
				a[1][2] = 0.000000e+000;
				a[2][0] = -1.000000e+000;
				a[2][1] = 1.500000e-001;
				a[2][2] = 0.000000e+000;
				j = 1;
				for(;;)
					{
					if( j > 3 ) 	break;
					um_vctmsc(a[j-1],array_of_arrow_blocks[i-1].size,a[j-1]);
					um_cctmtf(a[j-1],rotate_trans,a[j-1]);
					um_cpltrans(a[j-1],a_wc[j-1]);
us_l113:
					j++ ;
					}
us_l114: 
				;
				um_vctovc(a_wc[0],a_wc[3]);
				error = gpolyline3(4,a_wc);
				}
			else if( ( array_of_arrow_blocks[i-1].arrow_type==UA_SINGLE_FILLED ) )
				{
				if( ( uj_miplotting()==UU_FALSE ) )
					{
					error = gsfillcolor(array_of_arrow_blocks[i-1].arrow.color);
					a[0][0] = -1.000000e+000;
					a[0][1] = -1.500000e-001;
					a[0][2] = 0.000000e+000;
					a[1][0] = 0.000000e+000;
					a[1][1] = 0.000000e+000;
					a[1][2] = 0.000000e+000;
					a[2][0] = -1.000000e+000;
					a[2][1] = 1.500000e-001;
					a[2][2] = 0.000000e+000;
					j = 1;
					for(;;)
						{
						if( j > 3 ) 	break;
						um_vctmsc(a[j-1],array_of_arrow_blocks[i-1].size,a[j-1]);
						um_cctmtf(a[j-1],rotate_trans,a[j-1]);
						um_cpltrans(a[j-1],a_wc[j-1]);
us_l115:
						j++ ;
						}
us_l116: 
					;
					error = gfillarea3(3,a_wc);
					}
				else
					{
					a_cc[0][0] = -1.000000e+000;
					a_cc[0][1] = -1.500000e-001;
					a_cc[0][2] = 0.000000e+000;
					a_cc[1][0] = 0.000000e+000;
					a_cc[1][1] = 0.000000e+000;
					a_cc[1][2] = 0.000000e+000;
					a_cc[2][0] = -1.000000e+000;
					a_cc[2][1] = 1.500000e-001;
					a_cc[2][2] = 0.000000e+000;
					v1[0] = 6.525563e-001;
					v1[1] = 7.577402e-001;
					v1[2] = 0.000000e+000;
					v2[0] = -6.525563e-001;
					v2[1] = 0.000000e+000;
					v2[2] = 0.000000e+000;
					v3[0] = 6.525563e-001;
					v3[1] = -7.577402e-001;
					v3[2] = 0.000000e+000;
					ratio = ( ( array_of_arrow_blocks[i-1].size*scale_fac )*
					    1.297810e+001 );
					if( ( ratio<2.000000e+000 ) )
						{
						ratio = 2.000000e+000;
						}
					delta = ( 1.988809e-001/ratio );
					ratio = ( ratio+1.490000e+000 );
						{
						UU_REAL	us_t119;
						us_t119 = ratio;
						count = 1.000000e+000;
						for(;;)
							{
							if( count > us_t119 ) 	break;
							j = 1;
							for(;;)
								{
								if( j > 3 ) 	break;
								um_vctmsc(a_cc[j-1],array_of_arrow_blocks[i-1].size,
												a[j-1]);
								um_cctmtf(a[j-1],rotate_trans,a[j-1]);
								um_cpltrans(a[j-1],a_wc[j-1]);
us_l120:
								j++ ;
								}
us_l121: 
							;
							um_vctovc(a_wc[0],a_wc[3]);
							error = gpolyline3(4,a_wc);
								{
								UU_REAL	us_t122[3];
								um_vctmsc(v1,delta,us_t122);
								um_vcplvc(a_cc[0],us_t122,a_cc[0]);
								}
								{
								UU_REAL	us_t123[3];
								um_vctmsc(v2,delta,us_t123);
								um_vcplvc(a_cc[1],us_t123,a_cc[1]);
								}
								{
								UU_REAL	us_t124[3];
								um_vctmsc(v3,delta,us_t124);
								um_vcplvc(a_cc[2],us_t124,a_cc[2]);
								}
us_l117:
							count += 1.000000e+000;
							}
us_l118: 
						;
						}
					}
				}
			else if(( array_of_arrow_blocks[i-1].arrow_type==UA_REVERSED_CLOSED ))
				{
				a[0][0] = 0.000000e+000;
				a[0][1] = -1.500000e-001;
				a[0][2] = 0.000000e+000;
				a[1][0] = 0.000000e+000;
				a[1][1] = 1.500000e-001;
				a[1][2] = 0.000000e+000;
				a[2][0] = -1.000000e+000;
				a[2][1] = 0.000000e+000;
				a[2][2] = 0.000000e+000;
				j = 1;
				for(;;)
					{
					if( j > 3 ) 	break;
					um_vctmsc(a[j-1],array_of_arrow_blocks[i-1].size,a[j-1]);
					um_cctmtf(a[j-1],rotate_trans,a[j-1]);
					um_cpltrans(a[j-1],a_wc[j-1]);
us_l125:
					j++ ;
					}
us_l126: 
				;
				um_vctovc(a_wc[1-1],a_wc[4-1]);
				error = gpolyline3(4,a_wc);
				}
			else if( ( array_of_arrow_blocks[i-1].arrow_type==UA_NODE ) )
				{
				ang = 6.283185e-001;
				if( ( uj_miplotting()==UU_FALSE ) )
					{
					error = gsfillcolor(array_of_arrow_blocks[i-1].arrow.color);
					a[1-1][0] = 2.500000e-001;
					a[1-1][1] = 0.000000e+000;
					a[1-1][2] = 0.000000e+000;
					um_vctmsc(a[1-1],array_of_arrow_blocks[i-1].size,a[1-1]);
					j = 2;
					for(;;)
						{
						if( j > 10 ) 	break;
							{
							UU_REAL	us_t129[4][3];
							UU_REAL	us_t130[3];
							UU_REAL	us_t131[4][3];
							us_t130[0] = 0.000000e+000;
							us_t130[1] = 0.000000e+000;
							us_t130[2] = 1.000000e+000;
							um_rottf(us_t130,( ang*( (UU_REAL)( j-1 ) ) ),us_t129);
							um_disptf(array_of_arrow_blocks[i-1].location,us_t131);
							um_tftmtf(us_t129,us_t131,rotate);
							}
						um_cctmtf(a[1-1],rotate,a[j-1]);
us_l127:
						j++ ;
						}
us_l128: 
					;
					um_vcplvc(a[1-1],array_of_arrow_blocks[i-1].location,a[1-1])
						;
					um_vctovc(a[1-1],a[11-1]);
					j = 1;
					for(;;)
						{
						if( j > 11 ) 	break;
						um_cpltrans(a[j-1],a_wc[j-1]);
us_l132:
						j++ ;
						}
us_l133: 
					;
					error = gfillarea3(11,a_wc);
					}
				else
					{
					ratio = ( ( 2.500000e+001*array_of_arrow_blocks[i-1].size )*
					    scale_fac );
					delta = ( 2.500000e-001/ratio );
					ratio = ( ratio+1.490000e+000 );
					a_cc[1-1][0] = 2.500000e-001;
					a_cc[1-1][1] = 0.000000e+000;
					a_cc[1-1][2] = 0.000000e+000;
						{
						UU_REAL	us_t136;
						us_t136 = ratio;
						count = 1.000000e+000;
						for(;;)
							{
							if( count > us_t136 ) 	break;
							um_vctmsc(a_cc[1-1],array_of_arrow_blocks[i-1].size,a[1-1]);
							j = 2;
							for(;;)
								{
								if( j > 10 ) 	break;
									{
									UU_REAL	us_t139[4][3];
									UU_REAL	us_t140[3];
									UU_REAL	us_t141[4][3];
									us_t140[0] = 0.000000e+000;
									us_t140[1] = 0.000000e+000;
									us_t140[2] = 1.000000e+000;
									um_rottf(us_t140,( ang*( (UU_REAL)( j-1 ) ) ),
																us_t139);
									um_disptf(array_of_arrow_blocks[i-1].location,
																us_t141);
									um_tftmtf(us_t139,us_t141,rotate);
									}
								um_cctmtf(a[1-1],rotate,a[j-1]);
us_l137:
								j++ ;
								}
us_l138: 
							;
							um_vcplvc(a[0],array_of_arrow_blocks[i-1].location,a[0]);
							um_vctovc(a[0],a[10]);
							j = 1;
							for(;;)
								{
								if( j > 11 ) 	break;
								um_cpltrans(a[j-1],a_wc[j-1]);
us_l142:
								j++ ;
								}
us_l143: 
							;
							error = gpolyline3(11,a_wc);
							a_cc[1-1][0] = ( a_cc[1-1][0]-delta );
us_l134:
							count += 1.000000e+000;
							}
us_l135: 
						;
						}
					}
				}
			else if( ( array_of_arrow_blocks[i-1].arrow_type==UA_POINT ) )
				{
				a[0][0] = 0.000000e+000;
				a[0][1] = -2.500000e-001;
				a[0][2] = 0.000000e+000;
				a[1][0] = 2.500000e-001;
				a[1][1] = 0.000000e+000;
				a[1][2] = 0.000000e+000;
				a[2][0] = 0.000000e+000;
				a[2][1] = 2.500000e-001;
				a[2][2] = 0.000000e+000;
				a[3][0] = -2.500000e-001;
				a[3][1] = 0.000000e+000;
				a[3][2] = 0.000000e+000;
				j = 1;
				for(;;)
					{
					if( j > 4 ) 	break;
					um_vctmsc(a[j-1],array_of_arrow_blocks[i-1].size,a[j-1]);
					um_cctmtf(a[j-1],rotate_trans,a[j-1]);
					um_cpltrans(a[j-1],a_wc[j-1]);
us_l144:
					j++ ;
					}
us_l145: 
				;
				um_vctovc(a_wc[0],a_wc[4]);
				error = gpolyline3(5,a_wc);
				}
			else if( ( array_of_arrow_blocks[i-1].arrow_type==UA_DOUBLE_CLOSED ) )
				{
				a[0][0] = -1.000000e+000;
				a[0][1] = -1.500000e-001;
				a[0][2] = 0.000000e+000;
				a[1][0] = 0.000000e+000;
				a[1][1] = 0.000000e+000;
				a[1][2] = 0.000000e+000;
				a[2][0] = -1.000000e+000;
				a[2][1] = 1.500000e-001;
				a[2][2] = 0.000000e+000;
				j = 1;
				for(;;)
					{
					if( j > 3 ) 	break;
					um_vctmsc(a[j-1],array_of_arrow_blocks[i-1].size,a[j-1]);
					um_cctmtf(a[j-1],rotate_trans,a[j-1]);
					um_cpltrans(a[j-1],a_wc[j-1]);
us_l146:
					j++ ;
					}
us_l147: 
				;
				um_vctovc(a_wc[0],a_wc[3]);
				error = gpolyline3(4,a_wc);
				a[3][0] = -2.000000e+000;
				a[3][1] = -1.500000e-001;
				a[3][2] = 0.000000e+000;
				a[4][0] = -1.000000e+000;
				a[4][1] = 0.000000e+000;
				a[4][2] = 0.000000e+000;
				a[5][0] = -2.000000e+000;
				a[5][1] = 1.500000e-001;
				a[5][2] = 0.000000e+000;
				j = 1;
				for(;;)
					{
					if( j > 3 ) 	break;
					um_vctmsc(a[( j+3 )-1],array_of_arrow_blocks[i-1].size,a[j -1]);
					um_cctmtf(a[j-1],rotate_trans,a[j-1]);
					um_cpltrans(a[j-1],a_wc[j-1]);
us_l148:
					j++ ;
					}
us_l149: 
				;
				um_vctovc(a_wc[0],a_wc[3]);
				error = gpolyline3(4,a_wc);
				}
			else if( ( array_of_arrow_blocks[i-1].arrow_type==UA_ARCH_CROSS ) )
				{
				a[0][0] = -5.000000e-001;
				a[0][1] = -5.000000e-001;
				a[0][2] = 0.000000e+000;
				a[1][0] = 5.000000e-001;
				a[1][1] = 5.000000e-001;
				a[1][2] = 0.000000e+000;
				j = 1;
				for(;;)
					{
					if( j > 2 ) 	break;
					um_vctmsc(a[j-1],array_of_arrow_blocks[i-1].size,a[j-1]);
					um_cctmtf(a[j-1],rotate_trans,a[j-1]);
					um_cpltrans(a[j-1],a_wc[j-1]);
us_l150:
					j++ ;
					}
us_l151: 
				;
				error = gpolyline3(2,a_wc);
				}
			else if( ( array_of_arrow_blocks[i-1].arrow_type==UA_TILDE ) )
				{
				a[0][0] = -2.750000e-001;
				a[0][1] = -2.500000e-001;
				a[0][2] = 0.000000e+000;
				a[1][0] = -2.500000e-001;
				a[1][1] = -2.700000e-001;
				a[1][2] = 0.000000e+000;
				a[2][0] = -2.250000e-001;
				a[2][1] = -2.800000e-001;
				a[2][2] = 0.000000e+000;
				a[3][0] = -2.000000e-001;
				a[3][1] = -2.700000e-001;
				a[3][2] = 0.000000e+000;
				a[4][0] = -1.750000e-001;
				a[4][1] = -2.600000e-001;
				a[4][2] = 0.000000e+000;
				a[5][0] = -1.500000e-001;
				a[5][1] = -2.500000e-001;
				a[5][2] = 0.000000e+000;
				a[6][0] = 1.500000e-001;
				a[6][1] = 2.500000e-001;
				a[6][2] = 0.000000e+000;
				a[7][0] = 1.750000e-001;
				a[7][1] = 2.600000e-001;
				a[7][2] = 0.000000e+000;
				a[8][0] = 2.000000e-001;
				a[8][1] = 2.700000e-001;
				a[8][2] = 0.000000e+000;
				a[9][0] = 2.250000e-001;
				a[9][1] = 2.800000e-001;
				a[9][2] = 0.000000e+000;
				a[10][0] = 2.500000e-001;
				a[10][1] = 2.700000e-001;
				a[10][2] = 0.000000e+000;
				a[11][0] = 2.750000e-001;
				a[11][1] = 2.500000e-001;
				a[11][2] = 0.000000e+000;
				j = 1;
				for(;;)
					{
					if( j > 12 ) 	break;
					um_vctmsc(a[j-1],array_of_arrow_blocks[i-1].size,a[j-1]);
					um_cctmtf(a[j-1],rotate_trans,a[j-1]);
					um_cpltrans(a[j-1],a_wc[j-1]);
us_l152:
					j++ ;
					}
us_l153: 
				;
				error = gpolyline3(12,a_wc);
				}
			else if( ( array_of_arrow_blocks[i-1].arrow_type==UA_ARC_BACK_FILLED ) )
				{
				if( ( uj_miplotting()==UU_FALSE ) )
					{
					error = gsfillcolor(array_of_arrow_blocks[i-1].arrow.color);
					a[0][0] = -1.0;
					a[0][1] = -0.25;
					a[0][2] = 0.0;
					a[1][0] = 0.0;
					a[1][1] = 0.0;
					a[1][2] = 0.0;
					a[2][0] = -1.0;
					a[2][1] = 0.25;
					a[2][2] = 0.0;
					a[3][0] = -0.93635;
					a[3][1] = 0.22267;
					a[3][2] = 0.0;
					a[4][0] = -0.88020;
					a[4][1] = 0.18349;
					a[4][2] = 0.0;
					a[5][0] = -0.83769;
					a[5][1] = 0.13055;
					a[5][2] = 0.0;
					a[6][0] = -0.80937;
					a[6][1] = 0.06782;
					a[6][2] = 0.0;
					a[7][0] = -0.8000;
					a[7][1] = 0.0;
					a[7][2] = 0.0;
					a[8][0] = -0.80937;
					a[8][1] = -0.06782;
					a[8][2] = 0.0;
					a[9][0] = -0.83769;
					a[9][1] = -0.13055;
					a[9][2] = 0.0;
					a[10][0] = -0.88020;
					a[10][1] = -0.18349;
					a[10][2] = 0.0;
					a[11][0] = -0.93635;
					a[11][1] = -0.22267;
					a[11][2] = 0.0;
					for(j=1;j<=12;j++)
						{
						um_vctmsc(a[j-1],array_of_arrow_blocks[i-1].size,a[j-1]);
						um_cctmtf(a[j-1],rotate_trans,a[j-1]);
						um_cpltrans(a[j-1],a_wc[j-1]);
						}
					um_vctovc(a_wc[1],a_cc[0]);
					um_vctovc(a_wc[1],a_cc[3]);
					for (j=2;j<11;j++)
						{
						um_vctovc(a_wc[j],a_cc[1]);
						um_vctovc(a_wc[j+1],a_cc[2]);
						error = gfillarea3(4,a_cc);
						}
					um_vctovc(a_wc[11],a_cc[1]);
					um_vctovc(a_wc[0],a_cc[2]);
					error = gfillarea3(4,a_cc);
					}
				else
					{
					a_cc[0][0] = -1.0;
					a_cc[0][1] = -0.25;
					a_cc[0][2] = 0.0;
					a_cc[1][0] = 0.0;
					a_cc[1][1] = 0.0;
					a_cc[1][2] = 0.0;
					a_cc[2][0] = -1.0;
					a_cc[2][1] = 0.25;
					a_cc[2][2] = 0.0;
					a_cc[3][0] = -0.93635;
					a_cc[3][1] = 0.22267;
					a_cc[3][2] = 0.0;
					a_cc[4][0] = -0.88020;
					a_cc[4][1] = 0.18349;
					a_cc[4][2] = 0.0;
					a_cc[5][0] = -0.83769;
					a_cc[5][1] = 0.13055;
					a_cc[5][2] = 0.0;
					a_cc[6][0] = -0.80937;
					a_cc[6][1] = 0.06782;
					a_cc[6][2] = 0.0;
					a_cc[7][0] = -0.8000;
					a_cc[7][1] = 0.0;
					a_cc[7][2] = 0.0;
					a_cc[8][0] = -0.80937;
					a_cc[8][1] = -0.06782;
					a_cc[8][2] = 0.0;
					a_cc[9][0] = -0.83769;
					a_cc[9][1] = -0.13055;
					a_cc[9][2] = 0.0;
					a_cc[10][0] = -0.88020;
					a_cc[10][1] = -0.18349;
					a_cc[10][2] = 0.0;
					a_cc[11][0] = -0.93635;
					a_cc[11][1] = -0.22267;
					a_cc[11][2] = 0.0;
					for(j=0;j<12;j++)
						{
						um_vctmsc(a_cc[j],array_of_arrow_blocks[i-1].size,
										a[j]);
						um_cctmtf(a[j],rotate_trans,a[j]);
						um_cpltrans(a[j],a_wc[j]);
						}
					um_vctovc(a_wc[0],a_wc[12]);
					error = gpolyline3(13,a_wc);
					um_vctovc(a_wc[1],a_cc[0]);
					um_vctovc(a_wc[1],a_cc[3]);
					for (j=1;j<6;j++)
						{
						um_vctovc(a_wc[j+1],a_cc[1]);
						um_vctovc(a_wc[j+2],a_cc[2]);
						error = gpolyline3(4,a_cc);
						}
					}
				}
			i++ ;
			}
		}
	uu_dexit;
	}
