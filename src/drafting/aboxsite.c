
/*********************************************************************
**    NAME         : aboxsite.c
**       CONTAINS:
**       		ua_box_site
**     		  ua_box_frame
**    		   ua_box_move
**    		   
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       aboxsite.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:31
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) aboxsite.c 4.2 8/11/89 02:35:34 single"};
#else
static char uu_sccsident[]={"@(#) aboxsite.c 4.2 8/11/89 02:35:34 double"};
#endif

#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"

/*********************************************************************
**    E_FUNCTION     : ua_box_frame(e, corners)
**       Take an entity with strings set and properly position
**       the dimension, tolerances, and appended text strings
**       relative to one another and the entity site.  Find
**       the four corners of box surrounding all text.
**       Find the point to pass the dimension line through.
**    PARAMETERS   
**       INPUT  : 
**          corners- corners of box surrounding all text.
**          e      - generic drafting entity.
**							using:     e.txt_gap
**       OUTPUT :  
**          corners- corners of box surrounding all text with
**                   gap (frame) that takes into account the
**                   attribute of gap from dim line to text. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_box_frame(e, corners)
struct UA_generic_draft	(*e);
UU_REAL	corners[4][3];
	{
	UU_REAL	vec1[3];
	UU_REAL	vec2[3];

	uu_denter(UU_STRC,(us,"SAL ua_box_frame(e=%s, corners=%s)", "...", "..."));
		{
		UU_REAL	us_t73[3];
		UU_REAL	us_t74[3];
		um_vcmnvc(corners[1],corners[0],us_t74);
		um_unitvc(us_t74,us_t73);
		um_vctmsc(us_t73,(*e).txt_gap,vec1);
		}
		{
		UU_REAL	us_t75[3];
		UU_REAL	us_t76[3];
		um_vcmnvc(corners[3],corners[0],us_t76);
		um_unitvc(us_t76,us_t75);
		um_vctmsc(us_t75,(*e).txt_gap,vec2);
		}
		{
		UU_REAL	us_t77[3];
		um_vcmnvc(corners[0],vec1,us_t77);
		um_vcmnvc(us_t77,vec2,corners[0]);
		}
		{
		UU_REAL	us_t78[3];
		um_vcplvc(corners[1],vec1,us_t78);
		um_vcmnvc(us_t78,vec2,corners[1]);
		}
		{
		UU_REAL	us_t79[3];
		um_vcplvc(corners[2],vec1,us_t79);
		um_vcplvc(us_t79,vec2,corners[2]);
		}
		{
		UU_REAL	us_t80[3];
		um_vcmnvc(corners[3],vec1,us_t80);
		um_vcplvc(us_t80,vec2,corners[3]);
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_box_site(e, corners, pt1)
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
ua_box_site(e, corners, pt1)
struct UA_generic_draft	(*e);
UU_REAL	corners[4][3];
UU_REAL	pt1[3];
	{
	int		m_tol_indx;
	int		dblkcnt;
	int		sq1_blk_indx;
	int		mblkcnt;
	int		sq2_blk_indx;
	int		dtxtblk[4];
	int		mtxtblk[4];
	int		i;
	int		d_blk_indx;
	int		k;
	int		d_tol_indx;
	int		j;
	UU_REAL	mainbox[3];
	UU_REAL	dualbox[3];
	UU_REAL	box[3];
	UU_REAL	dtxtsize;
	UU_REAL	overall_y;
	UU_REAL	pt1_cc[3];
	UU_REAL	mdabox[3];
	UU_REAL	mtxtsize;
	UU_REAL	site_offset[3];
	UU_REAL	offset[20][3];
	UU_REAL	mdrbox[3];
	UU_REAL	rotate[4][3];
	UU_REAL	corners_cc[4][3];
	UU_REAL	main_y;
	UU_REAL	dual_y;
	UU_REAL	mdbox[3];
	UU_REAL	main_del_y;
	UU_REAL	dual_del_y;
	UU_REAL	del_x;
	UU_REAL	md_adjust[3];
	UU_REAL	del_y;
	UU_LOGICAL	tol_flg;

	uu_denter(UU_STRC,(us,"SAL ua_box_site(e=%s, corners=%s, pt1=%s)", "...",
				"...", "..."));

	mblkcnt = 0;
	dblkcnt = 0;
	mtxtsize = 0.000000e+000;
	dtxtsize = 0.000000e+000;
	m_tol_indx = 0;
	d_tol_indx = 0;
	del_x = 0.000000e+000;
	del_y = 0.000000e+000;
	tol_flg = UU_FALSE;
	main_del_y = 0.000000e+000;
	dual_del_y = 0.000000e+000;
		{
		int		us_t83;
		us_t83 = (*e).txt_blk_use;
		i = 1;
		for(;;)
			{
			if( i > us_t83 ) 	break;
			ua_text_offset(&((*e)),i,offset[i-1]);
us_l81:
			i++ ;
			}
us_l82: 
		;
		}
		{
		int		us_t86;
		us_t86 = (*e).txt_blk_use;
		i = 1;
		for(;;)
			{
			if( i > us_t86 ) 	break;
			if( ( ( (*e).txt_blk[i-1].subtype==main_txt1 )||( (*e).
			    							txt_blk[i-1].subtype==main_txt2 ) ) )
				{
				if( ( (*e).txt_blk[i-1].subtype==main_txt1 ) )
					{
					if( ( mblkcnt==0 ) )
						{
						mainbox[0] = (*e).txt_blk[i-1].dx;
						mainbox[1] = (*e).txt_blk[i-1].dy;
						mainbox[2] = 0.000000e+000;
						main_y = ( (*e).txt_blk[i-1].dy/2.000000e+000 );
						mtxtsize = ( (*e).txt_blk[i-1].dx/( (UU_REAL)(*e).txt_blk[i
						    -1].char_cnt ) );
						}
					else
						{
						offset[i-1][0] = ( offset[i-1][0]+mainbox[0] );
						if( ( mblkcnt==2 ) )
							{
							(*e).txt_blk[i-1].dx = ( (*e).txt_blk[i-1].dx/1.762000e+000
							    );
							}
						mainbox[0] = ( mainbox[0]+(*e).txt_blk[i-1].dx );
						if( ( mainbox[1]<(*e).txt_blk[i-1].dy ) )
							{
							main_del_y = ( 5.000000e-001*( (*e).txt_blk[i-1].dy-mainbox
							    [1] ) );
							offset[i-1][1] = ( offset[i-1][1]-main_del_y );
							mainbox[1] = (*e).txt_blk[i-1].dy;
							}
						}
					mblkcnt = ( mblkcnt+1 );
					mtxtblk[mblkcnt-1] = i;
					}
				if( ( (*e).txt_blk[i-1].subtype==main_txt2 ) )
					{
					if( ( dblkcnt==0 ) )
						{
						dualbox[0] = (*e).txt_blk[i-1].dx;
						dualbox[1] = (*e).txt_blk[i-1].dy;
						dualbox[2] = 0.000000e+000;
						dual_y = ( (*e).txt_blk[i-1].dy/2.000000e+000 );
						dtxtsize = ( (*e).txt_blk[i-1].dx/( (UU_REAL)(*e).txt_blk[i
						    -1].char_cnt ) );
						}
					else
						{
						offset[i-1][0] = ( offset[i-1][0]+dualbox[0] );
						if( ( dblkcnt==2 ) )
							{
							(*e).txt_blk[i-1].dx = ( (*e).txt_blk[i-1].dx/1.762000e+000
							    );
							}
						dualbox[0] = ( dualbox[0]+(*e).txt_blk[i-1].dx );
						if( ( dualbox[1]<(*e).txt_blk[i-1].dy ) )
							{
							dual_del_y = ( 5.000000e-001*( (*e).txt_blk[i-1].dy-dualbox
							    [1] ) );
							offset[i-1][1] = ( offset[i-1][1]-dual_del_y );
							dualbox[1] = (*e).txt_blk[i-1].dy;
							}
						}
					dblkcnt = ( dblkcnt+1 );
					dtxtblk[dblkcnt-1] = i;
					}
				}
us_l84:
			i++ ;
			}
us_l85: 
		;
		}
	if( ( ( ( ( ( (*e).tol_method==UA_NO_TOL )
	  	 ||( (*e).tol_method==UA_LIMIT_ONE_LINE ) )
	    ||( (*e).tol_method==UA_LIMIT_TWO_LINE ) )
		 ||( (*e).tol_method==UA_LIMIT_LARG_FIRST ) )
		 ||( (*e).tol_method==UA_LIMIT_LARG_BELOW ) ) )
		{
		goto dual;
		}
	else
		{
			{
			int		us_t89;
			us_t89 = (*e).txt_blk_use;
			i = 1;
			for(;;)
				{
				if( i > us_t89 ) 	break;
				if( ( ( (*e).txt_blk[i-1].subtype==tol_txt1 )||( (*e).
				    txt_blk[i-1].subtype==tol_txt2 ) ) )
					{
					switch( (*e).txt_blk[i-1].subtype )
						{
						case tol_txt1:
							{
							del_x = mainbox[0];
							del_y = mainbox[1];
							tol_flg = UU_TRUE;
							m_tol_indx = i;
							}
							break;
						case tol_txt2:
							{
							del_x = dualbox[0];
							del_y = dualbox[1];
							tol_flg = UU_FALSE;
							d_tol_indx = i;
							}
							break;
						}
					switch( (*e).tol_site )
						{
						case UA_TOL_ABOVE:
							{
							offset[i-1][1] = ( offset[i-1][1]+del_y );
							if( ( (*e).txt_blk[i-1].dx>del_x ) )
								{
								box[0] = (*e).txt_blk[i-1].dx;
								box[1] = ( (*e).txt_blk[i-1].dy+del_y );
								box[2] = 0.000000e+000;
								}
							else
								{
								box[0] = del_x;
								box[1] = ( del_y+(*e).txt_blk[i-1].dy );
								box[2] = 0.000000e+000;
								}
							}
							break;
						case UA_TOL_BELOW:
							{
							if( ( tol_flg==UU_TRUE ) )
								{
									{
									int		us_t92;
									us_t92 = mblkcnt;
									j = 1;
									for(;;)
										{
										if( j > us_t92 ) 	break;
										k = mtxtblk[j-1];
										offset[k-1][1] = ( offset[k-1][1]+(*e).txt_blk[i-1].dy );
us_l90:
										j++ ;
										}
us_l91: 
									;
									}
								}
							else
								{
									{
									int		us_t95;
									us_t95 = dblkcnt;
									j = 1;
									for(;;)
										{
										if( j > us_t95 ) 	break;
										k = dtxtblk[j-1];
										offset[k-1][1] = ( offset[k-1][1]+
															(*e).txt_blk[i-1].dy );
us_l93:
										j++ ;
										}
us_l94: 
									;
									}
								}
							if( ( (*e).txt_blk[i-1].dx>del_x ) )
								{
								box[0] = (*e).txt_blk[i-1].dx;
								box[1] = ( (*e).txt_blk[i-1].dy+del_y );
								box[2] = 0.000000e+000;
								}
							else
								{
								box[0] = del_x;
								box[1] = ( (*e).txt_blk[i-1].dy+del_y );
								box[2] = 0.000000e+000;
								}
							}
							break;
						case UA_TOL_CENTERED:
							{
							offset[i-1][0] = ( offset[i-1][0]+del_x );
							if( ( del_y>(*e).txt_blk[i-1].dy ) )
								{
								offset[i-1][1] = ( offset[i-1][1]+
													( ( del_y-(*e).txt_blk[i-1].dy )
													/2.000000e+000 ) );
								}
							else
								{
								offset[i-1][1] = ( offset[i-1][1]-
													( ( (*e).txt_blk[i-1].dy-del_y )
													/2.000000e+000 ) );
								}
							if( ( (*e).txt_blk[i-1].dy>del_y ) )
								{
								box[0] = ( (*e).txt_blk[i-1].dx+del_x );
								box[1] = (*e).txt_blk[i-1].dy;
								box[2] = 0.000000e+000;
								}
							else
								{
								box[0] = ( (*e).txt_blk[i-1].dx+del_x );
								box[1] = del_y;
								box[2] = 0.000000e+000;
								}
							}
							break;
						case UA_TOL_ALIG_TOP:
							{
							offset[i-1][0] = ( offset[i-1][0]+del_x );
							if( ( (*e).txt_blk[i-1].dy<del_y ) )
								{
								offset[i-1][1] = ( offset[i-1][1]+
												( del_y-(*e).txt_blk[i-1].dy ) );
								}
							else
								{
								if( ( tol_flg==UU_TRUE ) )
									{
										{
										int		us_t98;
										us_t98 = mblkcnt;
										j = 1;
										for(;;)
											{
											if( j > us_t98 ) 	break;
											k = mtxtblk[j-1];
											offset[k-1][1] = ( offset[k-1][1]+
																( (*e).txt_blk[i-1].dy-
																(*e).txt_blk[k-1].dy ) );
us_l96:
											j++ ;
											}
us_l97: 
										;
										}
									}
								else
									{
										{
										int		us_t101;
										us_t101 = dblkcnt;
										j = 1;
										for(;;)
											{
											if( j > us_t101 ) 	break;
											k = dtxtblk[j-1];
											offset[k-1][1] = ( offset[k-1][1]+
															( (*e).txt_blk[i-1].dy-
																(*e).txt_blk[k-1].dy ) );
us_l99:
											j++ ;
											}
us_l100: 
										;
										}
									}
								}
							if( ( (*e).txt_blk[i-1].dy>del_y ) )
								{
								box[0] = ( (*e).txt_blk[i-1].dx+del_x );
								box[1] = (*e).txt_blk[i-1].dy;
								box[2] = 0.000000e+000;
								}
							else
								{
								box[0] = ( (*e).txt_blk[i-1].dx+del_x );
								box[1] = del_y;
								box[2] = 0.000000e+000;
								}
							}
							break;
						case UA_TOL_ALIG_BOT:
							{
							offset[i-1][0] = ( offset[i-1][0]+del_x );
							if( ( (*e).txt_blk[i-1].dy>del_y ) )
								{
								box[0] = ( (*e).txt_blk[i-1].dx+del_x );
								box[1] = (*e).txt_blk[i-1].dy;
								box[2] = 0.000000e+000;
								}
							else
								{
								box[0] = ( (*e).txt_blk[i-1].dx+del_x );
								box[1] = del_y;
								box[2] = 0.000000e+000;
								}
							}
							break;
						}
					if( ( tol_flg==UU_TRUE ) )
						{
						um_vctovc(box,mainbox);
						}
					else
						{
						um_vctovc(box,dualbox);
						}
					}
us_l87:
				i++ ;
				}
us_l88: 
			;
			}
		}
dual:
	switch( (*e).dual_format )
		{
		case UA_NO_DUAL:
			{
			um_vctovc(mainbox,mdbox);
			overall_y = main_y;
			}
			break;
		case UA_MAIN_O_DUAL:
			{
			mdbox[1] = ( ( mainbox[1]*1.100000e+000 )+dualbox[1] );
			overall_y = ( dualbox[1]+( 5.000000e-002*mainbox[1] ) );
				{
				int		us_t104;
				us_t104 = mblkcnt;
				i = 1;
				for(;;)
					{
					if( i > us_t104 ) 	break;
					k = mtxtblk[i-1];
					offset[k-1][1] = ( ( ( offset[k-1][1]+dualbox[1] )+( 
					1.000000e-001*mainbox[1] ) )+main_del_y );
us_l102:
					i++ ;
					}
us_l103: 
				;
				}
			if( ( mainbox[0]>dualbox[0] ) )
				{
				mdbox[0] = mainbox[0];
					{
					int		us_t107;
					us_t107 = dblkcnt;
					i = 1;
					for(;;)
						{
						if( i > us_t107 ) 	break;
						k = dtxtblk[i-1];
						offset[k-1][0] = ( offset[k-1][0]+( ( mainbox[0]-dualbox[0]
						    )/2.000000e+000 ) );
us_l105:
						i++ ;
						}
us_l106: ;
					}
				if( ( (*e).tol_method!=0 ) )
					{
					offset[m_tol_indx-1][1] = ( ( ( offset[m_tol_indx-1][1]+
					    dualbox[1] )+( 1.000000e-001*mainbox[1] ) )+main_del_y );
					offset[d_tol_indx-1][0] = ( offset[d_tol_indx-1][0]+( ( 
					mainbox[0]-dualbox[0] )/2.000000e+000 ) );
					}
				}
			else
				{
				mdbox[0] = dualbox[0];
					{
					int		us_t110;
					us_t110 = mblkcnt;
					i = 1;
					for(;;)
						{
						if( i > us_t110 ) 	break;
						k = mtxtblk[i-1];
						offset[k-1][0] = ( offset[k-1][0]+( ( dualbox[0]-mainbox[0]
						    )/2.000000e+000 ) );
us_l108:
						i++ ;
						}
us_l109: 
					;
					}
				if( ( (*e).tol_method!=0 ) )
					{
					offset[d_tol_indx-1][0] = ( offset[d_tol_indx-1][0]+( ( 
					dualbox[0]-mainbox[0] )/2.000000e+000 ) );
					offset[m_tol_indx-1][1] = ( ( ( offset[m_tol_indx-1][1]+
					    dualbox[1] )+( 1.000000e-001*mainbox[1] ) )+main_del_y );
					}
				}
			}
			break;
		case UA_DUAL_O_MAIN:
			{
			mdbox[1] = ( mainbox[1]+( 1.100000e+000*dualbox[1] ) );
			overall_y = ( mainbox[1]+( 5.000000e-002*dualbox[1] ) );
				{
				int		us_t113;
				us_t113 = dblkcnt;
				i = 1;
				for(;;)
					{
					if( i > us_t113 ) 	break;
					k = dtxtblk[i-1];
					offset[k-1][1] = ( ( ( offset[k-1][1]+mainbox[1] )+( 
					1.000000e-001*dualbox[1] ) )+dual_del_y );
us_l111:
					i++ ;
					}
us_l112: 
				;
				}
			if( ( mainbox[0]>dualbox[0] ) )
				{
				mdbox[0] = mainbox[0];
					{
					int		us_t116;
					us_t116 = dblkcnt;
					i = 1;
					for(;;)
						{
						if( i > us_t116 ) 	break;
						k = dtxtblk[i-1];
						offset[k-1][0] = ( offset[k-1][0]+( ( mainbox[0]-dualbox[0]
						    )/2.000000e+000 ) );
us_l114:
						i++ ;
						}
us_l115: 
					;
					}
				if( ( (*e).tol_method!=UA_NO_TOL ) )
					{
					offset[d_tol_indx-1][0] = ( offset[d_tol_indx-1][0]+( ( 
					mainbox[0]-dualbox[0] )/2.000000e+000 ) );
					offset[d_tol_indx-1][1] = ( ( ( offset[d_tol_indx-1][1]+
					    mainbox[1] )+( 1.000000e-001*dualbox[1] ) )+dual_del_y );
					}
				}
			else
				{
				mdbox[0] = dualbox[0];
					{
					int		us_t119;
					us_t119 = mblkcnt;
					i = 1;
					for(;;)
						{
						if( i > us_t119 ) 	break;
						k = mtxtblk[i-1];
						offset[k-1][0] = ( offset[k-1][0]+( ( dualbox[0]-mainbox[0]
						    )/2.000000e+000 ) );
us_l117:
						i++ ;
						}
us_l118: 
					;
					}
				if( ( (*e).tol_method!=UA_NO_TOL ) )
					{
					offset[m_tol_indx-1][0] = ( offset[m_tol_indx-1][0]+( ( 
					dualbox[0]-mainbox[0] )/2.000000e+000 ) );
					offset[d_tol_indx-1][1] = ( ( ( offset[d_tol_indx-1][1]+
					    mainbox[1] )+( 1.000000e-001*dualbox[1] ) )+dual_del_y );
					}
				}
			}
			break;
		case UA_MAIN_DUAL:
			{
			mdbox[0] = ( ( mainbox[0]+mtxtsize )+dualbox[0] );
				{
				int		us_t122;
				us_t122 = dblkcnt;
				i = 1;
				for(;;)
					{
					if( i > us_t122 ) 	break;
					k = dtxtblk[i-1];
					offset[k-1][0] = ( ( offset[k-1][0]+mainbox[0] )+mtxtsize );
us_l120:
					i++ ;
					}
us_l121: 
				;
				}
			if( ( mainbox[1]>dualbox[1] ) )
				{
				mdbox[1] = mainbox[1];
					{
					int		us_t125;
					us_t125 = dblkcnt;
					i = 1;
					for(;;)
						{
						if( i > us_t125 ) 	break;
						k = dtxtblk[i-1];
						offset[k-1][1] = ( offset[k-1][1]+( ( mainbox[1]-dualbox[1]
						    )/2.000000e+000 ) );
us_l123:
						i++ ;
						}
us_l124: 
					;
					}
				if( ( (*e).tol_method!=UA_NO_TOL ) )
					{
					offset[d_tol_indx-1][0] = ( ( offset[d_tol_indx-1][0]+
					    mainbox[0] )+mtxtsize );
					offset[d_tol_indx-1][1] = ( offset[d_tol_indx-1][1]+( ( 
					mainbox[1]-dualbox[1] )/2.000000e+000 ) );
					}
				}
			else
				{
				mdbox[1] = dualbox[1];
					{
					int		us_t128;
					us_t128 = mblkcnt;
					i = 1;
					for(;;)
						{
						if( i > us_t128 ) 	break;
						k = mtxtblk[i-1];
						offset[k-1][1] = ( offset[k-1][1]+( ( dualbox[1]-mainbox[1]
						    )/2.000000e+000 ) );
us_l126:
						i++ ;
						}
us_l127: 
					;
					}
				if( ( (*e).tol_method!=UA_NO_TOL ) )
					{
					offset[m_tol_indx-1][1] = ( offset[m_tol_indx-1][1]+( ( 
					dualbox[1]-mainbox[1] )/2.000000e+000 ) );
					offset[d_tol_indx-1][0] = ( ( offset[d_tol_indx-1][0]+
					    mainbox[0] )+mtxtsize );
					}
				}
			if( ( main_y>dual_y ) )
				{
				overall_y = main_y;
				}
			else
				{
				overall_y = dual_y;
				}
			}
			break;
		case UA_DUAL_MAIN:
			{
			mdbox[0] = ( ( mainbox[0]+mtxtsize )+dualbox[0] );
				{
				int		us_t131;
				us_t131 = mblkcnt;
				i = 1;
				for(;;)
					{
					if( i > us_t131 ) 	break;
					k = mtxtblk[i-1];
					offset[k-1][0] = ( ( offset[k-1][0]+dualbox[0] )+mtxtsize );
us_l129:
					i++ ;
					}
us_l130: 
				;
				}
			if( ( mainbox[1]>dualbox[1] ) )
				{
				mdbox[1] = mainbox[1];
					{
					int		us_t134;
					us_t134 = dblkcnt;
					i = 1;
					for(;;)
						{
						if( i > us_t134 ) 	break;
						k = dtxtblk[i-1];
						offset[k-1][1] = ( offset[k-1][1]+( ( mainbox[1]-dualbox[1]
						    )/2.000000e+000 ) );
us_l132:
						i++ ;
						}
us_l133: 
					;
					}
				if( ( (*e).tol_method!=UA_NO_TOL ) )
					{
					offset[d_tol_indx-1][1] = ( offset[d_tol_indx-1][1]+( ( 
					mainbox[1]-dualbox[1] )/2.000000e+000 ) );
					offset[m_tol_indx-1][0] = ( ( offset[m_tol_indx-1][0]+
					    dualbox[0] )+mtxtsize );
					}
				}
			else
				{
				mdbox[1] = dualbox[1];
					{
					int		us_t137;
					us_t137 = mblkcnt;
					i = 1;
					for(;;)
						{
						if( i > us_t137 ) 	break;
						k = mtxtblk[i-1];
						offset[k-1][1] = ( offset[k-1][1]+( ( dualbox[1]-mainbox[1]
						    )/2.000000e+000 ) );
us_l135:
						i++ ;
						}
us_l136: 
					;
					}
				if( ( (*e).tol_method!=UA_NO_TOL ) )
					{
					offset[m_tol_indx-1][0] = ( ( offset[m_tol_indx-1][0]+
					    dualbox[0] )+mtxtsize );
					offset[m_tol_indx-1][1] = ( offset[m_tol_indx-1][1]+( ( 
					dualbox[1]-mainbox[1] )/2.000000e+000 ) );
					}
				}
			if( ( main_y>dual_y ) )
				{
				overall_y = main_y;
				}
			else
				{
				overall_y = dual_y;
				}
			}
			break;
		case UA_MAIN_O_BDUAL:
			{
			mdbox[1] = ( ( mainbox[1]*1.100000e+000 )+dualbox[1] );
			overall_y = ( dualbox[1]+( 5.000000e-002*mainbox[1] ) );
				{
				int		us_t140;
				us_t140 = mblkcnt;
				i = 1;
				for(;;)
					{
					if( i > us_t140 ) 	break;
					k = mtxtblk[i-1];
					offset[k-1][1] = ( ( ( offset[k-1][1]+dualbox[1] )+( 
					1.000000e-001*mainbox[1] ) )+main_del_y );
us_l138:
					i++ ;
					}
us_l139: 
				;
				}
			if( ( mainbox[0]>dualbox[0] ) )
				{
				mdbox[0] = mainbox[0];
					{
					int		us_t143;
					us_t143 = dblkcnt;
					i = 1;
					for(;;)
						{
						if( i > us_t143 ) 	break;
						k = dtxtblk[i-1];
						offset[k-1][0] = ( offset[k-1][0]+( ( mainbox[0]-dualbox[0]
						    )/2.000000e+000 ) );
us_l141:
						i++ ;
						}
us_l142: 
					;
					}
				if( ( (*e).tol_method!=UA_NO_TOL ) )
					{
					offset[m_tol_indx-1][1] = ( ( ( offset[m_tol_indx-1][1]+
					    dualbox[1] )+( 1.000000e-001*mainbox[1] ) )+main_del_y );
					offset[d_tol_indx-1][0] = ( offset[d_tol_indx-1][0]+( ( 
					mainbox[0]-dualbox[0] )/2.000000e+000 ) );
					}
				}
			else
				{
				mdbox[0] = dualbox[0];
					{
					int		us_t146;
					us_t146 = mblkcnt;
					i = 1;
					for(;;)
						{
						if( i > us_t146 ) 	break;
						k = mtxtblk[i-1];
						offset[k-1][0] = ( offset[k-1][0]+( ( dualbox[0]-mainbox[0]
						    )/2.000000e+000 ) );
us_l144:
						i++ ;
						}
us_l145: 
					;
					}
				if( ( (*e).tol_method!=UA_NO_TOL ) )
					{
					offset[m_tol_indx-1][0] = ( offset[m_tol_indx-1][0]+( ( 
					dualbox[0]-mainbox[0] )/2.000000e+000 ) );
					offset[m_tol_indx-1][1] = ( ( ( offset[m_tol_indx-1][1]+
					    dualbox[1] )+( 1.000000e-001*mainbox[1] ) )+main_del_y );
					}
				}
			}
			break;
		case UA_BDUAL_O_MAIN:
			{
			mdbox[1] = ( mainbox[1]+( 1.100000e+000*dualbox[1] ) );
			overall_y = ( mainbox[1]+( 5.000000e-002*dualbox[1] ) );
				{
				int		us_t149;
				us_t149 = dblkcnt;
				i = 1;
				for(;;)
					{
					if( i > us_t149 ) 	break;
					k = dtxtblk[i-1];
					offset[k-1][1] = ( ( ( offset[k-1][1]+mainbox[1] )+( 
					1.000000e-001*dualbox[1] ) )+dual_del_y );
us_l147:
					i++ ;
					}
us_l148: 
				;
				}
			if( ( mainbox[0]>dualbox[0] ) )
				{
				mdbox[0] = mainbox[0];
					{
					int		us_t152;
					us_t152 = dblkcnt;
					i = 1;
					for(;;)
						{
						if( i > us_t152 ) 	break;
						k = dtxtblk[i-1];
						offset[k-1][0] = ( offset[k-1][0]+( ( mainbox[0]-dualbox[0]
						    )/2.000000e+000 ) );
us_l150:
						i++ ;
						}
us_l151: 
					;
					}
				if( ( (*e).tol_method!=UA_NO_TOL ) )
					{
					offset[d_tol_indx-1][0] = ( offset[d_tol_indx-1][0]+( ( 
					mainbox[0]-dualbox[0] )/2.000000e+000 ) );
					offset[d_tol_indx-1][1] = ( ( ( offset[d_tol_indx-1][1]+
					    mainbox[1] )+( 1.000000e-001*dualbox[1] ) )+dual_del_y );
					}
				}
			else
				{
				mdbox[0] = dualbox[0];
					{
					int		us_t155;
					us_t155 = mblkcnt;
					i = 1;
					for(;;)
						{
						if( i > us_t155 ) 	break;
						k = mtxtblk[i-1];
						offset[k-1][0] = ( offset[k-1][0]+( ( dualbox[0]-mainbox[0]
						    )/2.000000e+000 ) );
us_l153:
						i++ ;
						}
us_l154: 
					;
					}
				if( ( (*e).tol_method!=UA_NO_TOL ) )
					{
					offset[m_tol_indx-1][0] = ( offset[m_tol_indx-1][0]+( ( 
					dualbox[0]-mainbox[0] )/2.000000e+000 ) );
					offset[d_tol_indx-1][1] = ( ( ( offset[d_tol_indx-1][1]+
					    mainbox[1] )+( 1.000000e-001*dualbox[1] ) )+dual_del_y );
					}
				}
			}
			break;
		case UA_MAIN_BDUAL:
			{
			mdbox[0] = ( ( mainbox[0]+mtxtsize )+dualbox[0] );
				{
				int		us_t158;
				us_t158 = dblkcnt;
				i = 1;
				for(;;)
					{
					if( i > us_t158 ) 	break;
					k = dtxtblk[i-1];
					offset[k-1][0] = ( ( offset[k-1][0]+mainbox[0] )+mtxtsize );
us_l156:
					i++ ;
					}
us_l157: 
				;
				}
			if( ( mainbox[1]>dualbox[1] ) )
				{
				mdbox[1] = mainbox[1];
					{
					int		us_t161;
					us_t161 = dblkcnt;
					i = 1;
					for(;;)
						{
						if( i > us_t161 ) 	break;
						k = dtxtblk[i-1];
						offset[k-1][1] = ( offset[k-1][1]+( ( mainbox[1]-dualbox[1]
						    )/2.000000e+000 ) );
us_l159:
						i++ ;
						}
us_l160: 
					;
					}
				if( ( (*e).tol_method!=UA_NO_TOL ) )
					{
					offset[d_tol_indx-1][0] = ( ( offset[d_tol_indx-1][0]+
					    mainbox[0] )+mtxtsize );
					offset[d_tol_indx-1][1] = ( offset[d_tol_indx-1][1]+( ( 
					mainbox[1]-dualbox[1] )/2.000000e+000 ) );
					}
				}
			else
				{
				mdbox[1] = dualbox[1];
					{
					int		us_t164;
					us_t164 = mblkcnt;
					i = 1;
					for(;;)
						{
						if( i > us_t164 ) 	break;
						k = mtxtblk[i-1];
						offset[k-1][1] = ( offset[k-1][1]+( ( dualbox[1]-mainbox[1]
						    )/2.000000e+000 ) );
us_l162:
						i++ ;
						}
us_l163: 
					;
					}
				if( ( (*e).tol_method!=UA_NO_TOL ) )
					{
					offset[m_tol_indx-1][1] = ( offset[m_tol_indx-1][1]+( ( 
					dualbox[1]-mainbox[1] )/2.000000e+000 ) );
					offset[d_tol_indx-1][0] = ( ( offset[d_tol_indx-1][0]+
					    mainbox[0] )+mtxtsize );
					}
				}
			if( ( main_y>dual_y ) )
				{
				overall_y = main_y;
				}
			else
				{
				overall_y = dual_y;
				}
			}
			break;
		case UA_BDUAL_MAIN:
			{
			mdbox[0] = ( ( mainbox[0]+mtxtsize )+dualbox[0] );
				{
				int		us_t167;
				us_t167 = mblkcnt;
				i = 1;
				for(;;)
					{
					if( i > us_t167 ) 	break;
					k = mtxtblk[i-1];
					offset[k-1][0] = ( ( offset[k-1][0]+dualbox[0] )+mtxtsize );
us_l165:
					i++ ;
					}
us_l166: 
				;
				}
			if( ( mainbox[1]>dualbox[1] ) )
				{
				mdbox[1] = mainbox[1];
					{
					int		us_t170;
					us_t170 = dblkcnt;
					i = 1;
					for(;;)
						{
						if( i > us_t170 ) 	break;
						k = dtxtblk[i-1];
						offset[k-1][1] = ( offset[k-1][1]+( ( mainbox[1]-dualbox[1]
						    )/2.000000e+000 ) );
us_l168:
						i++ ;
						}
us_l169: 
					;
					}
				if( ( (*e).tol_method!=UA_NO_TOL ) )
					{
					offset[d_tol_indx-1][1] = ( offset[d_tol_indx-1][1]+( ( 
					mainbox[1]-dualbox[1] )/2.000000e+000 ) );
					offset[m_tol_indx-1][0] = ( ( offset[m_tol_indx-1][0]+
					    dualbox[0] )+mtxtsize );
					}
				}
			else
				{
				mdbox[1] = dualbox[1];
					{
					int		us_t173;
					us_t173 = mblkcnt;
					i = 1;
					for(;;)
						{
						if( i > us_t173 ) 	break;
						k = mtxtblk[i-1];
						offset[k-1][1] = ( offset[k-1][1]+( ( dualbox[1]-mainbox[1]
						    )/2.000000e+000 ) );
us_l171:
						i++ ;
						}
us_l172: 
					;
					}
				if( ( (*e).tol_method!=UA_NO_TOL ) )
					{
					offset[m_tol_indx-1][0] = ( ( offset[m_tol_indx-1][0]+
					    dualbox[0] )+mtxtsize );
					offset[m_tol_indx-1][1] = ( offset[m_tol_indx-1][1]+( ( 
					dualbox[1]-mainbox[1] )/2.000000e+000 ) );
					}
				}
			if( ( main_y>dual_y ) )
				{
				overall_y = main_y;
				}
			else
				{
				overall_y = dual_y;
				}
			}
			break;
		}


	if( ( ( ( ( ( (*e).tol_method==UA_BILAT_TWO_LINE )
					||( (*e).tol_method==UA_UNILAT_ABOVE ) )
	    			||( (*e).tol_method==UA_UNILAT_BELOW ) )
					||( (*e).tol_method==UA_LIMIT_LARG_BELOW ) )
					||( (*e) .tol_method==UA_LIMIT_TWO_LINE ) ) )
		{
		if( ( ( ( ( (*e).dual_format==UA_MAIN_BDUAL )
					||( (*e).dual_format==UA_MAIN_BDUAL ) )
		    		||( (*e).dual_format==UA_MAIN_O_BDUAL ) )
					||( (*e).dual_format==UA_BDUAL_O_MAIN ) ) )
			{
			d_blk_indx = dtxtblk[0];
			um_vctovc(offset[d_blk_indx-1],box);
			mdbox[0] = ( mdbox[0]+( 2.000000e+000*mtxtsize ) );
			switch( (*e).dual_format )
				{
				case UA_MAIN_BDUAL:
					{
						{
						int		us_t176;
						us_t176 = (*e).txt_blk_use;
						j = 1;
						for(;;)
							{
							if( j > us_t176 ) 	break;
							if( ( ( (*e).txt_blk[j-1].subtype==main_txt2 )||( (*e).
							    txt_blk[j-1].subtype==tol_txt2 ) ) )
								{
								offset[j-1][0] = ( offset[j-1][0]+mtxtsize );
								}
us_l174:
							j++ ;
							}
us_l175: 
						;
						}
					}
					break;
				case UA_MAIN_O_BDUAL:
					{
						{
						int		us_t179;
						us_t179 = (*e).txt_blk_use;
						j = 1;
						for(;;)
							{
							if( j > us_t179 ) 	break;
							if( ( ( ( ( (*e).txt_blk[j-1].subtype==main_txt1 )||( (*e).
							    txt_blk[j-1].subtype==tol_txt1 ) )||( (*e).txt_blk[j-1].
							    subtype==main_txt2 ) )||( (*e).txt_blk[j-1].subtype==
							    tol_txt2 ) ) )
								{
								offset[j-1][0] = ( offset[j-1][0]+mtxtsize );
								}
us_l177:
							j++ ;
							}
us_l178: 
						;
						}
					}
					break;
				case UA_BDUAL_O_MAIN:
					{
						{
						int		us_t182;
						us_t182 = (*e).txt_blk_use;
						j = 1;
						for(;;)
							{
							if( j > us_t182 ) 	break;
							if( ( ( ( ( (*e).txt_blk[j-1].subtype==main_txt1 )||( (*e).
							    txt_blk[j-1].subtype==tol_txt1 ) )||( (*e).txt_blk[j-1].
							    subtype==main_txt2 ) )||( (*e).txt_blk[j-1].subtype==
							    tol_txt2 ) ) )
								{
								offset[j-1][0] = ( offset[j-1][0]+mtxtsize );
								}
us_l180:
							j++ ;
							}
us_l181: 
						;
						}
					}
					break;
				case UA_BDUAL_MAIN:
					{
						{
						int		us_t185;
						us_t185 = (*e).txt_blk_use;
						j = 1;
						for(;;)
							{
							if( j > us_t185 ) 	break;
							if( ( ( (*e).txt_blk[j-1].subtype==main_txt1 )||( (*e).
							    txt_blk[j-1].subtype==tol_txt1 ) ) )
								{
								offset[j-1][0] = ( offset[j-1][0]+( 2.000000e+000*mtxtsize )
								    );
								}
							else
								{
								if( ( ( (*e).txt_blk[j-1].subtype==main_txt2 )||( (*e).
								    txt_blk[j-1].subtype==tol_txt2 ) ) )
									{
									offset[j-1][0] = ( offset[j-1][0]+mtxtsize );
									}
								}
us_l183:
							j++ ;
							}
us_l184: 
						;
						}
					}
					break;
				}

			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			sq1_blk_indx = (*e).txt_blk_use;
			(*e).txt_blk[sq1_blk_indx-1].char_cnt = 1;
			strcpy((*e).txt_blk[sq1_blk_indx-1].tstring,"[");
			(*e).txt_blk[sq1_blk_indx-1].tangle = (*e).txt_blk[
			    d_blk_indx-1].tangle;
			(*e).txt_blk[sq1_blk_indx-1].subtype = main_txt2;
			(*e).txt_blk[sq1_blk_indx-1].txt_just = 2;
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			sq2_blk_indx = (*e).txt_blk_use;
			(*e).txt_blk[sq2_blk_indx-1].char_cnt = 1;
			strcpy((*e).txt_blk[sq2_blk_indx-1].tstring,"]");
			(*e).txt_blk[sq2_blk_indx-1].tangle = (*e).txt_blk[
			    d_blk_indx-1].tangle;
			(*e).txt_blk[sq2_blk_indx-1].subtype = main_txt2;
			(*e).txt_blk[sq2_blk_indx-1].txt_just = 2;
			ua_text_offset(&((*e)),sq1_blk_indx,offset[sq1_blk_indx-1]);
			ua_text_offset(&((*e)),sq2_blk_indx,offset[sq2_blk_indx-1]);
			um_vctovc(box,offset[sq1_blk_indx-1]);
			um_vctovc(box,offset[sq2_blk_indx-1]);
			offset[sq2_blk_indx-1][0] = ( ( offset[sq1_blk_indx-1][0]+
			    dualbox[0] )+mtxtsize );
			if( ( ( (*e).tol_method==4 )||( (*e).tol_method==2 ) ) )
				{
				offset[sq1_blk_indx-1][1] = ( offset[sq1_blk_indx-1][1]-( 
				dualbox[1]/4.000000e+000 ) );
				offset[sq2_blk_indx-1][1] = ( offset[sq2_blk_indx-1][1]-( 
				dualbox[1]/4.000000e+000 ) );
				}
			}
		}
	um_vctovc(mdbox,mdrbox);
	md_adjust[0] = 0.000000e+000;
	md_adjust[1] = 0.000000e+000;
	md_adjust[2] = 0.000000e+000;
		{
		int		us_t188;
		us_t188 = (*e).txt_blk_use;
		i = 1;
		for(;;)
			{
			if( i > us_t188 ) 	break;
			if( ( (*e).txt_blk[i-1].subtype==dia_rad_sym ) )
				{
				if( ( (*e).etype==UA_RAD_CEN_DIM ) )
					{
					k = (*e).rad_place;
					}
				else
					{
					k = (*e).dia_place;
					}
				switch( k )
					{
					case UA_ABOVE:
						{
						offset[i-1][1] = ( offset[i-1][1]+( 1.100000e+000*mdbox[1] )
						    );
						if( ( (*e).txt_blk[i-1].dx>mdbox[0] ) )
							{
							mdrbox[0] = (*e).txt_blk[i-1].dx;
							mdrbox[1] = ( (*e).txt_blk[i-1].dy+( 1.100000e+000*mdbox[1]
							    ) );
							mdrbox[2] = 0.000000e+000;
							}
						else
							{
							mdrbox[0] = mdbox[0];
							mdrbox[1] = ( (*e).txt_blk[i-1].dy+( 1.100000e+000*mdbox[1]
							    ) );
							mdrbox[2] = 0.000000e+000;
							}
						um_vctovc(mdrbox,mdbox);
						}
						break;
					case UA_BELOW:
						{
						offset[i-1][1] = ( offset[i-1][1]-( 1.100000e+000*mdbox[1] )
						    );
						if( ( (*e).txt_blk[i-1].dx>mdbox[0] ) )
							{
							mdrbox[0] = (*e).txt_blk[i-1].dx;
							mdrbox[1] = ( (*e).txt_blk[i-1].dy+( 1.100000e+000*mdbox[1]
							    ) );
							mdrbox[2] = 0.000000e+000;
							}
						else
							{
							mdrbox[0] = mdbox[0];
							mdrbox[1] = ( (*e).txt_blk[i-1].dy+( 1.100000e+000*mdbox[1]
							    ) );
							mdrbox[2] = 0.000000e+000;
							}
						um_vctovc(mdrbox,mdbox);
						}
						break;
					case UA_BEFORE:
						{
						if( ( (*e).txt_blk[i-1].dy>mdbox[1] ) )
							{
							mdrbox[0] = ( ( 1.800000e+000*(*e).txt_blk[i-1].dx )+mdbox
							    [0] );
							mdrbox[1] = (*e).txt_blk[i-1].dy;
							mdrbox[2] = 0.000000e+000;
							md_adjust[0] = ( 1.800000e+000*(*e).txt_blk[i-1].dx );
							md_adjust[1] = 0.000000e+000;
							md_adjust[2] = 0.000000e+000;
							}
						else
							{
							mdrbox[0] = ( ( 1.800000e+000*(*e).txt_blk[i-1].dx )+mdbox
							    [0] );
							mdrbox[1] = mdbox[1];
							mdrbox[2] = 0.000000e+000;
							md_adjust[0] = ( 1.800000e+000*(*e).txt_blk[i-1].dx );
							md_adjust[1] = 0.000000e+000;
							md_adjust[2] = 0.000000e+000;
							}
						um_vctovc(mdrbox,mdbox);
						}
						break;
					case UA_AFTER:
						{
						offset[i-1][0] = ( offset[i-1][0]+mdbox[0] );
						if( ( (*e).txt_blk[i-1].dy>mdbox[1] ) )
							{
							mdrbox[0] = ( (*e).txt_blk[i-1].dx+mdbox[0] );
							mdrbox[1] = (*e).txt_blk[i-1].dy;
							mdrbox[2] = 0.000000e+000;
							}
						else
							{
							mdrbox[0] = ( (*e).txt_blk[i-1].dx+mdbox[0] );
							mdrbox[1] = mdbox[1];
							mdrbox[2] = 0.000000e+000;
							}
						um_vctovc(mdrbox,mdbox);
						}
						break;
					}
				}
us_l186:
			i++ ;
			}
us_l187: 
		;
		}
		{
		int		us_t191;
		us_t191 = (*e).txt_blk_use;
		i = 1;
		for(;;)
			{
			if( i > us_t191 ) 	break;
			if( ( ( ( ( (*e).txt_blk[i-1].subtype==main_txt1 )||( (*e).
			    txt_blk[i-1].subtype==main_txt2 ) )||( (*e).txt_blk[i-1].
			    subtype==tol_txt1 ) )||( (*e).txt_blk[i-1].subtype==tol_txt2
			    ) ) )
				{
				um_vcplvc(offset[i-1],md_adjust,offset[i-1]);
				}
us_l189:
			i++ ;
			}
us_l190: 
		;
		}
	um_vctovc(mdbox,mdabox);
	md_adjust[0] = 0.000000e+000;
	md_adjust[1] = 0.000000e+000;
	md_adjust[2] = 0.000000e+000;
		{
		int		us_t194;
		us_t194 = (*e).txt_blk_use;
		i = 1;
		for(;;)
			{
			if( i > us_t194 ) 	break;
			if( ( (*e).txt_blk[i-1].subtype==app_cre_txt ) )
				{
				switch( (*e).appn_text )
					{
					case UA_ABOVE:
						{
						switch( (*e).txt_just )
							{
							case UA_LEFT:
								{
								offset[i-1][1] = ( offset[i-1][1]+
													( 1.100000e+000*mdbox[1] ));
								if( ( (*e).txt_blk[i-1].dx>mdbox[0] ) )
									{
									mdabox[0] = (*e).txt_blk[i-1].dx;
									mdabox[1] = ( (*e).txt_blk[i-1].dy+
													( 1.100000e+000*mdbox[1]) );
									mdabox[2] = 0.000000e+000;
									}
								else
									{
									mdabox[0] = mdbox[0];
									mdabox[1] = ( (*e).txt_blk[i-1].dy+
													( 1.100000e+000*mdbox[1]) );
									mdabox[2] = 0.000000e+000;
									}
								}
								break;
							case UA_RIGHT:
								{
								offset[i-1][1] = ( offset[i-1][1]+
													( 1.100000e+000*mdbox[1] ));
								if( ( (*e).txt_blk[i-1].dx>mdbox[0] ) )
									{
									mdabox[0] = (*e).txt_blk[i-1].dx;
									mdabox[1] = ( (*e).txt_blk[i-1].dy+
													( 1.100000e+000*mdbox[1]) );
									mdabox[2] = 0.000000e+000;
									md_adjust[0] = ( (*e).txt_blk[i-1].dx-mdbox[0] );
									md_adjust[1] = 0.000000e+000;
									md_adjust[2] = 0.000000e+000;
									}
								else
									{
									offset[i-1][0] = ( ( offset[i-1][0]+mdbox[0] )-(*e).txt_blk[
									    i-1].dx );
									mdabox[0] = mdbox[0];
									mdabox[1] = ( (*e).txt_blk[i-1].dy+( 1.100000e+000*mdbox[1]
									    ) );
									mdabox[2] = 0.000000e+000;
									}
								}
								break;
							case UA_CENTER:
								{
								offset[i-1][1] = ( offset[i-1][1]+
														( 1.100000e+000*mdbox[1] ));
								if( ( (*e).txt_blk[i-1].dx>mdbox[0] ) )
									{
									mdabox[0] = (*e).txt_blk[i-1].dx;
									mdabox[1] = ( (*e).txt_blk[i-1].dy+
													( 1.100000e+000*mdbox[1]) );
									mdabox[2] = 0.000000e+000;
									md_adjust[0] = ( ( (*e).txt_blk[i-1].dx-mdbox[0] )/
									    2.000000e+000 );
									md_adjust[1] = 0.000000e+000;
									md_adjust[2] = 0.000000e+000;
									}
								else
									{
									offset[i-1][0] = ( offset[i-1][0]+
														( ( mdbox[0]-(*e).txt_blk[i-1].dx )
															/2.000000e+000 ) );
									mdabox[0] = mdbox[0];
									mdabox[1] = ( (*e).txt_blk[i-1].dy+
														( 1.100000e+000*mdbox[1]) );
									mdabox[2] = 0.000000e+000;
									}
								}
								break;
							}
						overall_y = ( mdabox[1]/2.000000e+000 );
						}
						break;
					case UA_BELOW:
						{
						switch( (*e).txt_just )
							{
							case UA_LEFT:
								{
								md_adjust[0] = 0.000000e+000;
								md_adjust[1] = ( (*e).txt_blk[i-1].dy+
													( 1.000000e-001*mdbox [1] ) );
								md_adjust[2] = 0.000000e+000;
								if( ( (*e).txt_blk[i-1].dx>mdbox[0] ) )
									{
									mdabox[0] = (*e).txt_blk[i-1].dx;
									mdabox[1] = ( (*e).txt_blk[i-1].dy+
													( 1.100000e+000*mdbox[1]) );
									mdabox[2] = 0.000000e+000;
									}
								else
									{
									mdabox[0] = mdbox[0];
									mdabox[1] = ( (*e).txt_blk[i-1].dy+
														( 1.100000e+000*mdbox[1]) );
									mdabox[2] = 0.000000e+000;
									}
								}
								break;
							case UA_RIGHT:
								{
								if( ( (*e).txt_blk[i-1].dx>mdbox[0] ) )
									{
									mdabox[0] = (*e).txt_blk[i-1].dx;
									mdabox[1] = ( (*e).txt_blk[i-1].dy+
													( 1.100000e+000*mdbox[1]) );
									mdabox[2] = 0.000000e+000;
									md_adjust[0] = ( (*e).txt_blk[i-1].dx-mdbox[0] );
									md_adjust[1] = ( (*e).txt_blk[i-1].dy+
														( 1.000000e-001*mdbox [1] ) );
									md_adjust[2] = 0.000000e+000;
									}
								else
									{
									offset[i-1][0] = ( ( offset[i-1][0]+mdbox[0] )-
												(*e).txt_blk[ i-1].dx );
									mdabox[0] = mdbox[0];
									mdabox[1] = ( (*e).txt_blk[i-1].dy+
													( 1.100000e+000*mdbox[1]) );
									mdabox[2] = 0.000000e+000;
									md_adjust[0] = 0.000000e+000;
									md_adjust[1] = ( (*e).txt_blk[i-1].dy+
													( 1.000000e-001*mdbox[1] ) );
									md_adjust[2] = 0.000000e+000;
									}
								}
								break;
							case UA_CENTER:
								{
								if( ( (*e).txt_blk[i-1].dx>mdbox[0] ) )
									{
									mdabox[0] = (*e).txt_blk[i-1].dx;
									mdabox[1] = ( (*e).txt_blk[i-1].dy+
													( 1.100000e+000*mdbox[1]) );
									mdabox[2] = 0.000000e+000;
									md_adjust[0] = ( ( (*e).txt_blk[i-1].dx-mdbox[0] )/
									    						2.000000e+000 );
									md_adjust[1] = ( (*e).txt_blk[i-1].dy+
														( 1.000000e-001*mdbox [1] ) );
									md_adjust[2] = 0.000000e+000;
									}
								else
									{
									offset[i-1][0] = ( offset[i-1][0]+
															( ( mdbox[0]-(*e).txt_blk[i-1].dx )
															/2.000000e+000 ) );
									mdabox[0] = mdbox[0];
									mdabox[1] = ( (*e).txt_blk[i-1].dy+
													( 1.100000e+000*mdbox[1]) );
									mdabox[2] = 0.000000e+000;
									md_adjust[0] = 0.000000e+000;
									md_adjust[1] = ( (*e).txt_blk[i-1].dy+
														( 1.000000e-001*mdbox[1] ) );
									md_adjust[2] = 0.000000e+000;
									}
								}
								break;
							}
						overall_y = ( mdabox[1]/2.000000e+000 );
						}
						break;
					case UA_BEFORE:
						{
						if( ( (*e).txt_blk[i-1].dy>mdbox[1] ) )
							{
							mdabox[0] = ( (*e).txt_blk[i-1].dx+mdbox[0] );
							mdabox[1] = (*e).txt_blk[i-1].dy;
							mdabox[2] = 0.000000e+000;
							md_adjust[0] = (*e).txt_blk[i-1].dx;
							md_adjust[1] = 0.000000e+000;
							md_adjust[2] = 0.000000e+000;
							}
						else
							{
							mdabox[0] = ( (*e).txt_blk[i-1].dx+mdbox[0] );
							mdabox[1] = mdbox[1];
							mdabox[2] = 0.000000e+000;
							md_adjust[0] = (*e).txt_blk[i-1].dx;
							md_adjust[1] = 0.000000e+000;
							md_adjust[2] = 0.000000e+000;
							}
						}
						break;
					case UA_AFTER:
						{
						offset[i-1][0] = ( offset[i-1][0]+mdbox[0] );
						if( ( (*e).txt_blk[i-1].dy>mdbox[1] ) )
							{
							mdabox[0] = ( ( (*e).txt_blk[i-1].dx+mdbox[0] )+mtxtsize );
							mdabox[1] = (*e).txt_blk[i-1].dy;
							mdabox[2] = 0.000000e+000;
							}
						else
							{
							mdabox[0] = ( (*e).txt_blk[i-1].dx+mdbox[0] );
							mdabox[1] = mdbox[1];
							mdabox[2] = 0.000000e+000;
							}
						}
						break;
					}
				}
us_l192:
			i++ ;
			}
us_l193: ;
		}
		{
		int		us_t197;
		us_t197 = (*e).txt_blk_use;
		i = 1;
		for(;;)
			{
			if( i > us_t197 ) 	break;
			if( ( ( ( ( ( (*e).txt_blk[i-1].subtype==main_txt1 )
						||( (*e) .txt_blk[i-1].subtype==main_txt2 ) )
						||( (*e).txt_blk[i-1].  subtype==tol_txt1 ) )
						||( (*e).txt_blk[i-1].subtype== dia_rad_sym ) )
						||( (*e).txt_blk[i-1].subtype==tol_txt2 ) ) )
				{
				um_vcplvc(offset[i-1],md_adjust,offset[i-1]);
				}
us_l195:
			i++ ;
			}
us_l196: 
		;
		}
	switch( (*e).entity_site )
		{
		case UA_TOP_LEFT:
			{
			site_offset[0] = 0.000000e+000;
			site_offset[1] = ( ( -overall_y )*2.000000e+000 );
			site_offset[2] = 0.000000e+000;
			}
			break;
		case UA_TOP_CENTER:
			{
			site_offset[0] = ( ( -mdabox[0] )/2.000000e+000 );
			site_offset[1] = ( ( -overall_y )*2.000000e+000 );
			site_offset[2] = 0.000000e+000;
			}
			break;
		case UA_TOP_RIGHT:
			{
			site_offset[0] = ( -mdabox[0] );
			site_offset[1] = ( ( -overall_y )*2.000000e+000 );
			site_offset[2] = 0.000000e+000;
			}
			break;
		case UA_MIDDLE_LEFT:
			{
			site_offset[0] = 0.000000e+000;
			site_offset[1] = ( -overall_y );
			site_offset[2] = 0.000000e+000;
			}
			break;
		case UA_MIDDLE_CENTER:
			{
			site_offset[0] = ( ( -mdabox[0] )/2.000000e+000 );
			site_offset[1] = ( -overall_y );
			site_offset[2] = 0.000000e+000;
			}
			break;
		case UA_MIDDLE_RIGHT:
			{
			site_offset[0] = ( -mdabox[0] );
			site_offset[1] = ( -overall_y );
			site_offset[2] = 0.000000e+000;
			}
			break;
		case UA_BOTTOM_LEFT:
			{
			}
			break;
		case UA_BOTTOM_CENTER:
			{
			site_offset[0] = ( ( -mdabox[0] )/2.000000e+000 );
			site_offset[1] = 0.000000e+000;
			site_offset[2] = 0.000000e+000;
			}
			break;
		case UA_BOTTOM_RIGHT:
			{
			site_offset[0] = ( -mdabox[0] );
			site_offset[1] = 0.000000e+000;
			site_offset[2] = 0.000000e+000;
			}
			break;
		}
	if( ( main_del_y!=0.000000e+000 ) )
		{
		corners_cc[0][0] = 0.000000e+000;
		corners_cc[0][1] = ( -main_del_y );
		corners_cc[0][2] = 0.000000e+000;
		}
	else
		{
		if( ( dual_del_y!=0.000000e+000 ) )
			{
			corners_cc[0][0] = 0.000000e+000;
			corners_cc[0][1] = ( -dual_del_y );
			corners_cc[0][2] = 0.000000e+000;
			}
		else
			{
			corners_cc[0][0] = 0.000000e+000;
			corners_cc[0][1] = 0.000000e+000;
			corners_cc[0][2] = 0.000000e+000;
			}
		}
	corners_cc[1][0] = mdabox[0];
	corners_cc[1][1] = 0.000000e+000;
	corners_cc[1][2] = 0.000000e+000;
	corners_cc[2][0] = mdabox[0];
	corners_cc[2][1] = mdabox[1];
	corners_cc[2][2] = 0.000000e+000;
	corners_cc[3][0] = 0.000000e+000;
	corners_cc[3][1] = mdabox[1];
	corners_cc[3][2] = 0.000000e+000;
	pt1_cc[0] = ( mdabox[0]/2.000000e+000 );
	pt1_cc[1] = overall_y;
	pt1_cc[2] = 0.000000e+000;
		{
		UU_REAL	us_t198[3];
		us_t198[0] = 0.000000e+000;
		us_t198[1] = 0.000000e+000;
		us_t198[2] = 1.000000e+000;
		um_rottf(us_t198,(*e).txt_blk[0].tangle,rotate);
		}
	i = 1;
	for(;;)
		{
		if( i > 4 ) 	break;
		um_vcplvc(corners_cc[i-1],site_offset,corners_cc[i-1]);
		um_cctmtf(corners_cc[i-1],rotate,corners_cc[i-1]);
		um_ccstomcs(1,corners_cc[i-1],corners[i-1]);
		um_vcplvc(corners[i-1],(*e).dim_origin,corners[i-1]);
us_l199:
		i++ ;
		}
us_l200: 
	;
	um_vcplvc(pt1_cc,site_offset,pt1_cc);
	um_cctmtf(pt1_cc,rotate,pt1_cc);
	um_ccstomcs(1,pt1_cc,pt1);
	um_vcplvc(pt1,(*e).dim_origin,pt1);
		{
		int		us_t203;
		us_t203 = (*e).txt_blk_use;
		i = 1;
		for(;;)
			{
			if( i > us_t203 ) 	break;
			um_vcplvc(offset[i-1],site_offset,offset[i-1]);
			um_cctmtf(offset[i-1],rotate,offset[i-1]);
			um_ccstomcs(1,offset[i-1],(*e).txt_blk[i-1].origin);
us_l201:
			i++ ;
			}
us_l202: 
		;
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_box_move(dimvect, base_pt, base_vec, off_set, e
**										, box, dl_pt)
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
ua_box_move(dimvect, base_pt, base_vec, off_set, e
, box, dl_pt)
UU_REAL	dimvect[3];
UU_REAL	base_pt[3];
UU_REAL	base_vec[3];
UU_REAL	off_set;
struct UA_generic_draft	(*e);
UU_REAL	box[4][3];
UU_REAL	dl_pt[3];
	{
	int		num;
	int		i;
	UU_REAL	move_vect[3];
	UU_REAL	move_dist;
	UU_REAL	ent_vec[3];
	UU_REAL	origin[3];
	UU_REAL	cen_pt[3];
	UU_REAL	temp_extent;
	UU_REAL	dim_minext;
	UU_REAL	dim_maxext;
	UU_REAL	int_pt[3];
	UU_REAL	box_minext;
	UU_REAL	move_vect_grid[3];
	UU_REAL	box_maxext;
	UU_REAL	box_extent;
	UU_REAL	xaxis[3];
	UU_REAL	yaxis[3];
	UU_REAL	move_vect_cent[3];
	UU_REAL	zaxis[3];
	UU_REAL	nvec[3];

	uu_denter(UU_STRC,(us,"SAL ua_box_move(dimvect=<%g,%g,%g>, base_pt=<%g,%g,%g>, base_vec=<%g,%g,%g>, off_set=%g)",
		dimvect[0],dimvect[1],dimvect[2],base_pt[0],base_pt[1],
			base_pt[2], base_vec[0],base_vec[1],base_vec[2],
		off_set));

	if( ( (*e).txt_place==UA_AUTOMATIC ) )
		{
		box_maxext = um_dot(dimvect,box[0]);
		box_minext = box_maxext;
		for(i=2;i<=4;i++)
			{
			temp_extent = um_dot(dimvect,box[i-1]);
			if( ( temp_extent>box_maxext ) )
				{
				box_maxext = temp_extent;
				}
			if( ( temp_extent<box_minext ) )
				{
				box_minext = temp_extent;
				}
			}
		box_extent = ( box_maxext-box_minext );
		dim_minext = um_dot(dimvect,(*e).asso_blk[0].location);
		dim_maxext = um_dot(dimvect,(*e).asso_blk[1].location);
		if(((*e).grid_dist == 0.0)&&((*e).etype==UA_BASELN_DIM))
			{
			if( ( dim_minext>dim_maxext ) )
				um_vctmsc(dimvect,((dim_minext + box_extent )
				    -box_minext-(*e).dim_value),move_vect_cent);
			else
				um_vctmsc(dimvect,(((*e).dim_value-box_extent )
				    -(*e).arrow_size - (*e).txt_gap
				    +dim_minext -box_minext ),move_vect_cent);
			}
		else
			{
			if( ( dim_minext>dim_maxext ) )
				dim_minext = dim_maxext;
			um_vctmsc(dimvect,( ( ( ( (*e).dim_value-box_extent )/
			    2.0 )+dim_minext )-box_minext ),move_vect_cent);
			}
		}
	else
		{
		move_vect_cent[0] = 0.000000e+000;
		move_vect_cent[1] = 0.000000e+000;
		move_vect_cent[2] = 0.000000e+000;
		}
	if( ( (*e).stack_grid==1 ) )
		{
		ua_getcpln(&((*e)),origin,xaxis,yaxis,zaxis);
			{
			UU_REAL	us_t206[3];
			um_cross(dimvect,zaxis,us_t206);
			um_unitvc(us_t206,nvec);
			}
			{
			UU_REAL	us_t207[3];
			UU_REAL	us_t208[3];
			UU_REAL	us_t209[3];
			UU_REAL	us_t210[3];
			UU_REAL	us_t211[3];
			um_vcmnvc(box[1],box[0],us_t209);
			um_vctmsc(us_t209,(UU_REAL) 1.0 / 2.000000e+000,us_t208);
			um_vcplvc(box[0],us_t208,us_t207);
			um_vcmnvc(box[3],box[0],us_t211);
			um_vctmsc(us_t211,(UU_REAL) 1.0 / 2.000000e+000,us_t210);
			um_vcplvc(us_t207,us_t210,cen_pt);
			}
		um_ilnln(cen_pt,nvec,base_pt,base_vec,&(num),int_pt);
		if( ( num==0 ) )
			{
			uu_dexit;
			return;
			}
			{
			UU_REAL	us_t212[3];
			um_vcmnvc(int_pt,dl_pt,us_t212);
			um_unitvc(us_t212,move_vect_grid);
			}
		move_dist = um_dcccc(int_pt,dl_pt);
		um_vctmsc(move_vect_grid,( move_dist-off_set ),
		move_vect_grid);
		}
	else
		{
		move_vect_grid[0] = 0.000000e+000;
		move_vect_grid[1] = 0.000000e+000;
		move_vect_grid[2] = 0.000000e+000;
		}
	um_vcplvc(move_vect_cent,move_vect_grid,move_vect);
	um_vcplvc((*e).dim_origin,move_vect,(*e).dim_origin);
	um_vcplvc(dl_pt,move_vect,dl_pt);
	i = 1;
	for(;;)
		{
		if( i > 4 ) 	break;
		um_vcplvc(box[i-1],move_vect,box[i-1]);
us_l213:
		i++ ;
		}
us_l214: 
	;
	uu_dexit;
	}
