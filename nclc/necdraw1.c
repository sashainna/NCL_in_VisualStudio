/*********************************************************************
**    NAME         :  necdraw1.c
**       CONTAINS:
**			ncl_solid_cutter
**			ncl_process_cutter
**			ncl_process_symcutter
**			ncl_process_holder
**			ncl_process_ipvcutter
**			ncl_process_loadtl
**			ncl_load_cutter_symbol
**    COPYRIGHT 2001 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       necdraw1.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       12/01/15 , 08:24:33
*********************************************************************/

#include "usysdef.h"
#include "lcom.h"
#include "mcrv.h"
#include "mgeom.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "m2dattr.h"
#include "mfort.h"
#include "nclfc.h"
#include "nccs.h"
#include "bsym.h"
#include "nclmplay.h"
#include "nclmodals.h"
#include "view.h"
#include "lipv.h"
#include "lipvmach.h"
#include "xenv1.h"

/*static LtDoublePoint LW_stk_pos;*/

#define PW_CUTTER 716
#define PW_HOLDER 157
#define PW_LATHE 700
#define PW_BLADE 191

/*********************************************************************
**    E_FUNCTION     : ncl_solid_cutter(tend,taxis,nsides,vp,blade)
**			Revolve the cutter in order to create a solid cutter for
**			display.  Uses the global variable 'cutdef.gpt' to obtain
**			the cutter profile and to store the solid cutter.
**		PARAMETERS   
**			INPUT  : 
**				te      = Tool end point.
**				tax     = Tool axis vector.
**				nsides  = Number of sides to generate for solid cutter.
**				vp      = Current viewport.
**          ifl     = UU_TRUE if this part of the cutter should be shaded.
**                    [0] = Cutter, [1] = Shank, [2] = Holder.
**			OUTPUT :  
**          cutdef.gpt   = Cutter solid polygons.
**          cutdef.gnorm = Cutter solid normals.
**		RETURNS      : 
**				none.
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
ncl_solid_cutter(te,tax,nsides,vp,ifl)
UM_coord te;
UM_vector tax;
int nsides;
int vp;
UU_LOGICAL ifl[];
{
	int i,j,inc,inc1,n,k,ns,ns1,npt,ng,ist,ish,*vnpt,nj,ipt;
	UM_coord tend;
	UM_vector vc0,taxis;
	struct UM_circle_rec c[2],c0;
	UN_cutdef_view_struc cdef;
	Gwpoint3 gpt[2][200];
	Gwpoint3 gnorm[2][200];
	Gwpoint3 *vgpt,*vgnorm;
	UM_transf tm1;
	UU_LOGICAL ifirst,redo;
	UU_REAL pl[4],d,um_dsupt(),ang,dang;
/*
.....Initialize routine
*/
	npt = 0;
	cdef.ngeo = 0;
	c0.center[0] = c0.center[1] = c0.center[2] = 0;
	vc0[0] = vc0[1] = vc0[2] = 0.;
	c0.dang = UM_TWOPI;
	um_vctovc(taxis,c0.nvec);
	vnpt = (int *)UU_LIST_ARRAY(&cutdef[vp].view.npt);
	vgpt = (Gwpoint3 *)UU_LIST_ARRAY(&cutdef[vp].view.gpt);
	vgnorm = (Gwpoint3 *)UU_LIST_ARRAY(&cutdef[vp].view.gnorm);
/*
.....Set tool axis based on cutter type
*/
	if (cutdef[vp].cutr[8] > 10)
	{
/*		taxis[0] = 0;
		taxis[1] = 1;
		taxis[2] = 0;*/
		taxis[0] = tax[2];
		taxis[1] = tax[1];
		taxis[2] = -tax[0];
	}
	else
	{
		um_vctovc(tax,taxis);
		um_vctovc(te,tend);
	}
/*
.....Allocate local storage
*/
	ng = 0;
	for (i=0;i<cutdef[vp].view.ngeo;i++)
	{
		if (vnpt[i] > 0) ng += vnpt[i];
	}
	if (ng == 0) goto done;
	uu_list_init(&cdef.npt,sizeof(int),cutdef[vp].view.ngeo*20,
		cutdef[vp].view.ngeo);
	uu_list_init(&cdef.gpt,sizeof(Gwpoint3),ng*20,ng);
	uu_list_init(&cdef.gnorm,sizeof(Gwpoint3),ng*20,ng);
/*
.....If blade cutter, then
.....loop until Color entry is found
.....This is where the shank display starts
*/
	ng = 0;
	ist = 0;
/*
	if (blade)
	{
		for (ist=0;ist<cutdef[vp].view.ngeo;ist++)
		{
			if (ist > 2 && vnpt[ist] <= 0) break;
			uu_list_push(&cdef.npt,&vnpt[ist]);
			for (j=0;j<vnpt[ist];j++)
			{
				uu_list_push(&cdef.gpt,&vgpt[ng]);
				uu_list_push(&cdef.gnorm,&vgnorm[ng]);
				ng++; npt++;
			}
			cdef.ngeo++;
		}
	}
*/
/*
.....Loop thru all points on cutter profile
*/
	ipt = -1;
	for (i=ist;i<cutdef[vp].view.ngeo;i++)
	{
/*
........Initialize loop
*/
		ifirst = UU_TRUE;
		inc = 0;
		inc1 = 1;
/*
........Store color setting
*/
		if (vnpt[i] <= 0)
		{
			uu_list_push(&cdef.npt,&vnpt[i]); cdef.ngeo++; i++;
			ish = -vnpt[i];
			uu_list_push(&cdef.npt,&vnpt[i]); cdef.ngeo++; i++;
			uu_list_push(&cdef.npt,&vnpt[i]); cdef.ngeo++;
			ipt++;
			continue;
		}
/*
........Loop through points in this section
*/
		if (cutdef[vp].cutr[8] > 10) um_vctovc(&vgpt[ng],tend);
		for (j=0;j<vnpt[i];j++)
		{
/*
........Store non-shaded portion of cutter
*/
			if (ish == 0 || !ifl[ipt])
			{
				uu_list_push(&cdef.gpt,&vgpt[ng]);
				uu_list_push(&cdef.gnorm,&vgnorm[ng]);
				npt++;
				ng++;
				if (j == vnpt[i]-1)
				{
					uu_list_push(&cdef.npt,&vnpt[i]);
					cdef.ngeo++;
				}
				continue;
			}
/*
........Store shaded portion of cutter
*/
			if (j > 0 &&
				um_dcccc(&vgpt[ng],&vgpt[ng-1]) < UM_FUZZ)
				ifirst = UU_TRUE;
/*
........Define the circle for this level
*/
			um_ilnpln(tend,taxis,&vgpt[ng],taxis,&n,c[inc].center);
			c[inc].radius = um_dcccc(c[inc].center,&vgpt[ng]);
			if (c[inc].radius < UM_FUZZ)
			{
				um_vctovc(c[inc].center,&(gpt[inc][0]));
				um_vctovc(&vgnorm[ng],&(gnorm[inc][0]));
				ns = 1;
			}
/*
.....Generate points around the circle
*/
			else
			{
				um_vctovc(taxis,c[inc].nvec);
				um_vcmnvc(&vgpt[ng],c[inc].center,c[inc].svec);
				um_unitvc(c[inc].svec,c[inc].svec);
				c[inc].dang = UM_TWOPI;
				ncl_cutter_circle(&c[inc],&(gpt[inc]),nsides);
				ns = nsides;

				redo = UU_TRUE;
				ang = 0;
				dang = c0.dang / (nsides-1);
				for (k=0;k<nsides;k++)
				{
					um_vctovc(&vgnorm[ng],&gnorm[inc][k]);
					um_rotatept(&gnorm[inc][k],taxis,c0.center,ang,redo,tm1);
					ang += dang;
				}
					
/*
				um_ilnpln(tend,taxis,&vgnorm[ng],taxis,&n,c0.center);
				c0.radius = um_dcccc(c0.center,&vgnorm[ng]);
				um_vctovc(c[inc].svec,c0.svec);
				if (c0.radius < UM_FUZZ)
				{
					for (k=0;k<nsides;k++)
						um_vctovc(&vgnorm[ng],&gnorm[inc][k]);
				}
				else
					ncl_cutter_circle(&c0,&gnorm[inc],nsides);
*/
			}
/*
.....Create polygons
*/
			if (!ifirst && (ns > 1 || ns1 > 1))
			{
/*
........Second point is center of circle
*/
				if (ns == 1)
				{
					for (k=0;k<nsides;k++)
					{
						uu_list_push(&cdef.gpt,&gpt[inc1][k]);
						uu_list_push(&cdef.gnorm,taxis);
						npt++;
					}
					uu_list_push(&cdef.npt,&nsides);
					cdef.ngeo++;
				}
/*
........First point is center of circle
*/
				else if (ns1 == 1)
				{
/*
...........Verify that this is a flat area
...........and not the apex of a ball end mill
*/
					um_vctovc(taxis,pl);
					pl[3] = um_dcccc(tend,&(gpt[inc1][0]));
					d = um_dsupt(pl,&(gpt[inc][0]));
					if (fabs(d) < UM_FUZZ)
					{
						for (k=0;k<nsides;k++)
						{
							uu_list_push(&cdef.gpt,&gpt[inc][k]);
							uu_list_push(&cdef.gnorm,taxis);
							npt++;
						}
						uu_list_push(&cdef.npt,&nsides);
						cdef.ngeo++;
					}
/*
...........Display radius of ball end mill
*/
					else
					{
						nj = 3;
						for (k=0;k<nsides-1;k++)
						{
							uu_list_push(&cdef.gpt,&gpt[inc1][0]);
							uu_list_push(&cdef.gnorm,&gnorm[inc1][0]);
							npt++;

							uu_list_push(&cdef.gpt,&gpt[inc][k+1]);
							uu_list_push(&cdef.gnorm,&gnorm[inc][k+1]);
							npt++;
	
							uu_list_push(&cdef.gpt,&gpt[inc][k]);
							uu_list_push(&cdef.gnorm,&gnorm[inc][k]);
							npt++;

							uu_list_push(&cdef.npt,&nj);
							cdef.ngeo++;
						}
					}
				}
/*
........Create polygon on outside of cutter
*/
				else
				{
					nj = 4;
					for (k=0;k<nsides-1;k++)
					{
						uu_list_push(&cdef.gpt,&gpt[inc][k]);
						uu_list_push(&cdef.gnorm,&gnorm[inc][k]);
						npt++;

						uu_list_push(&cdef.gpt,&gpt[inc1][k]);
						uu_list_push(&cdef.gnorm,&gnorm[inc1][k]);
						npt++;

						uu_list_push(&cdef.gpt,&gpt[inc1][k+1]);
						uu_list_push(&cdef.gnorm,&gnorm[inc1][k+1]);
						npt++;
	
						uu_list_push(&cdef.gpt,&gpt[inc][k+1]);
						uu_list_push(&cdef.gnorm,&gnorm[inc][k+1]);
						npt++;
	
						uu_list_push(&cdef.npt,&nj);
						cdef.ngeo++;
					}
				}
			}
			else
				ifirst = UU_FALSE;
			inc = 1 - inc;
			inc1 = 1 - inc1;
			ns1 = ns;
			ng++;
		}
	}
/*
.....Copy temporary storage to cutter definition
*/
	uu_list_free(&cutdef[vp].view.npt);
	uu_list_free(&cutdef[vp].view.gpt);
	uu_list_free(&cutdef[vp].view.gnorm);
	cutdef[vp].view.ngeo = cdef.ngeo;
	cutdef[vp].view.npt = cdef.npt;
	cutdef[vp].view.gpt = cdef.gpt;
	cutdef[vp].view.gnorm = cdef.gnorm;
/*
.....End of routine
*/
done:;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_process_cutter(scan,iclw,rclw,cnv,clist,mdid,
**                                        ncutr,cutr,tlno,tlen)
**			Processes a CUTTER clfile record and determines if needs to
**       be added to the Cutter List.
**    PARAMETERS   
**       INPUT  : 
**				scan    = 0 = Normal playback.
**                    1 = SEQUNC scan.
**                    2 = Bounding box scan.
**                    3 = Cutter scan.
**				iclw    = Integer array from clfile record.
**          rclw    = Real array from clfile record.
**          cnv     = Metric conversion factor.
**          clist   = Tool list.
**          mdid    = 1 = Processed motion record since last Cutter.
**				ncutr   = Number of cutters currently in the list.
**          tlno    = Current tool number from LOADTL command.
**          tlen    = Current tool length from LOADTL command.
**       OUTPUT :  
**				cutr    = Cutter parameters when not scanning for cutters.
**                    When Scan=3 the cutter parameters are stored in the
**                    Cutter List.
**          ncutr   = Updated number of cutters in list when Scan=3.
**    RETURNS      : UU_TRUE if processing for NCLIPV and the new cutter
**                   is processed.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL
	ncl_process_cutter(scan,iclw,rclw,cnv,clist,mdid,ncutr,cutr,tlno,tlen)
int scan;
int iclw[];
UU_REAL rclw[];
UU_REAL cutr[],cnv;
UU_LIST *clist;
int *mdid,*ncutr;
int tlno;
UU_REAL tlen;
{
	int i,ncp,nc,ifl;
	UU_LOGICAL ifnd=UU_FALSE,same;
	UN_cutter_list cdata,*cpt;
	UU_REAL tdiff;
/*
.....Initialize routine
*/
	if (scan != 0 && scan != 1 && scan != 3) goto done;
	ncp = iclw[4];
/*
.....Scanning clfile
.....Store tool data
*/
	if (scan == 3 && (rclw[0] > 0. || rclw[3] != 0.))
	{
/*
........Initialize tool data
*/
		ul_ipv_reset_tool();
		cpt = (UN_cutter_list *) UU_LIST_ARRAY (clist);
		cdata.isn = iclw[0];
		cdata.clrec = iclw[5];
		cdata.cut_color = -1;
		cdata.color[0] = cdata.color[1] = cdata.color[2] = -1;
		cdata.ctype[0] = 1;
		cdata.ctype[1] = cdata.ctype[2] = 0;
		ncl_get_tool_symlib(cdata.symlib);
		cdata.symbol[0][0] = cdata.symbol[1][0] = cdata.symbol[2][0] = '\0';
		cdata.symkey[0] = cdata.symkey[1] = cdata.symkey[2] = 0;
		cdata.toler = LW_default_tool.toler;
		cdata.maxang = LW_default_tool.maxang;
		cdata.trans[0] = cdata.trans[1] = cdata.trans[2] =
			LW_default_tool.translucency;
		cdata.edge[0] = cdata.edge[1] = cdata.edge[2] = LW_default_tool.edge;
		cdata.edge_color[0] = cdata.edge_color[1] = cdata.edge_color[2] =
			LW_default_tool.edge_color;
		cdata.rapid = LW_default_tool.rapid;
		cdata.used = UU_FALSE;
		if (*ncutr > 0 && !cpt[*ncutr-1].used) cpt[*ncutr-1].used = *mdid;
		cdata.shank_clash = LW_default_tool.shank_clash;
		for (i=0;i<4;i++)
		{
			cdata.parms[0][i] = cdata.parms[1][i] = cdata.parms[2][i] = 0.;
		}
		cdata.tlno = tlno; cdata.tlen = tlen; cdata.tlofs = 0.;
/*
........Lathe cutter
*/
		if (rclw[0] == PW_LATHE-10000)
		{
			cdata.type = NCL_CUTTER_LATHE;
			for (i=1;i<ncp;i++) cdata.cutter[i-1] = rclw[i];
			for (i=ncp-1;i<6;i++) cdata.cutter[i] = 0.;
			if (cdata.cutter[2] < .01) cdata.cutter[2] = .01;
			cdata.cutter[0] = cdata.cutter[0] / cnv;
			cdata.cutter[1] = cdata.cutter[1] / cnv;
			cdata.cutter[2] = cdata.cutter[2] / cnv;
			if (cdata.cutter[3] != 0.) cdata.cutter[4] = cdata.cutter[4] / cnv;
			if (LW_tool_limit[2] > 0. && cdata.cutter[0] < LW_tool_limit[2])
				cdata.cutter[0] = LW_tool_limit[2];
			cdata.ncparm = ncp - 1;
		}
/*
........Blade cutter
*/
		else if (rclw[0] < 0.)
		{
			cdata.type = NCL_CUTTER_BLADE;
			for (i=1;i<ncp;i++) cdata.cutter[i-1] = rclw[i];
			for (i=ncp-1;i<6;i++) cdata.cutter[i] = 0.;
			if (cdata.cutter[2] < .01) cdata.cutter[2] = .01;
			cdata.cutter[0] = cdata.cutter[0] / cnv;
			cdata.cutter[1] = cdata.cutter[1] / cnv;
			cdata.cutter[2] = cdata.cutter[2] / cnv;
			cdata.cutter[3] = asin(cdata.cutter[3]) * UM_RADIAN;
			if (LW_tool_limit[2] > 0. && cdata.cutter[0] < LW_tool_limit[2])
				cdata.cutter[0] = LW_tool_limit[2];
			cdata.ncparm = ncp - 1;
		}
/*
........Standard mill cutter
*/
		else
		{
			cdata.type = NCL_CUTTER_MILL;
			for (i=0;i<ncp;i++) cdata.cutter[i] = rclw[i];
			for (i=ncp;i<6;i++) cdata.cutter[i] = 0.;
			if (ncp < 3) ncp = 3;
/*
.....Fix corner radius if rounding made it larger that half
.....cutter diameter - Andrew 3/5/13
*/
			tdiff = 2.*cdata.cutter[1] - cdata.cutter[0];
			if (tdiff > 0. && tdiff < LW_default_tool.toler)
				cdata.cutter[1] = cdata.cutter[0] / 2.;
			cdata.cutter[0] = cdata.cutter[0] / cnv;
			cdata.cutter[1] = cdata.cutter[1] / cnv;
			cdata.cutter[2] = cdata.cutter[2] / cnv;
			if (ncp > 4) cdata.cutter[3] = cdata.cutter[3] / cnv;
			cdata.cutter[4] = cdata.cutter[4] / cnv;
			cdata.ncparm = ncp;
			if (LW_tool_limit[2] > 0. && cdata.cutter[0] < LW_tool_limit[2])
				cdata.cutter[0] = LW_tool_limit[2];
			if (LW_tool_limit[0] > 0. && cdata.cutter[2] < LW_tool_limit[0])
				cdata.cutter[2] = LW_tool_limit[0];
			if (LW_tool_limit[1] > 0. && cdata.cutter[2] > LW_tool_limit[1])
				cdata.cutter[2] = LW_tool_limit[1];
			if (cdata.cutter[2] == 0.) cdata.cutter[2] = cdata.cutter[1];
			if (cdata.cutter[2] == 0.) goto done;
		}
		same = UU_FALSE;
		if (*ncutr != 0)
		{
			same = ncl_cutter_same(cdata.cutter,cdata.ncparm,
				cpt[*ncutr-1].cutter,cpt[*ncutr-1].ncparm);
			same = same && (cdata.tlno == cpt[*ncutr-1].tlno);
		}
		if (!same)
		{
			if (*mdid == 0 && *ncutr != 0)
			{
				if (*ncutr > 1)
				{
					same = ncl_cutter_same(cdata.cutter,cdata.ncparm,
						cpt[*ncutr-2].cutter,cpt[*ncutr-2].ncparm);
					same = same && (cdata.tlno == cpt[*ncutr-2].tlno);
					if (same)
					{
						if (!cpt[*ncutr-1].used)
						{
							*ncutr = *ncutr - 1;
							uu_list_delete(clist,*ncutr,1);
						}
						else
						{
							uu_list_push(clist,&cdata);
							*ncutr = *ncutr + 1;
						}
					}
				}
				if (!same)
				{
					if (!cpt[*ncutr-1].used)
					{
						uu_move_byte(&cdata,&cpt[*ncutr-1],sizeof(UN_cutter_list));
						ifnd = UU_TRUE;
					}
					else
					{
						uu_list_push(clist,&cdata);
						*ncutr = *ncutr + 1;
					}
				}
			}
			else
			{
				uu_list_push(clist,&cdata);
				*ncutr = *ncutr + 1;
			}
			*mdid = 0;
		}
	}
/*
.....Normal processing of clfile
*/
	else if (scan != 3)
	{
/*		if (ncl_cutter_same(rclw,iclw[4],cutr,*ncutr)) goto done;*/
/*
........Determine if this cutter has
........been changed by the user
*/
		ifnd = UU_FALSE;
/*
........Lathe cutter
*/
		if (rclw[0] == PW_LATHE-10000)
		{
			for (i=ncp;i<10;i++) rclw[i] = 0.;
			cutr[0] = rclw[1] * 2. / cnv;
			cutr[1] = 0.;
			cutr[2] = rclw[3] / cnv;
			cutr[3] = cutr[4] = 0.;
			cutr[5] = rclw[5];
			cutr[6] = rclw[2] / cnv;
			cutr[7] = rclw[4];
			cutr[8] = 12;
			if (cutr[7] == 90.) cutr[8] = 11;
			if (cutr[7] == 60.) cutr[8] = 13;
			if (cutr[7] == 0. && cutr[6] == 0.) cutr[8] = 14;
			if (cutr[7] == 0. && cutr[6] != 0.) cutr[8] = 15;
		}
/*
........Blade cutter
*/
		else if (rclw[0] < 0.0)
		{
			cutr[0] = rclw[0];
			cutr[5] = rclw[1] / cnv;
			cutr[6] = rclw[2] / cnv;
			cutr[2] = rclw[3] / cnv;
			cutr[7] = rclw[4];
		}
/*
........Standard cutter
*/
		else if (ncp != 5 && ncp != 6)
		{
			for (i=0;i<ncp;i++) cutr[i] = rclw[i];
			for (i=ncp;i<9;i++) cutr[i] = 0.;
			cutr[0] = cutr[0] / cnv;
			cutr[1] = cutr[1] / cnv;
			cutr[2] = cutr[2] / cnv;
		}
/*
........Barrel cutter
*/
		else
		{
			ifl = 1;
			for (i=0;i<ncp;i++)
			{
				if (rclw[i] < -.0001 || rclw[i] > .0001) nc = i+1;
			}
			barseg(&ifl,rclw,cutr,&nc);
			cutr[0] = cutr[0] / cnv;
			cutr[1] = cutr[1] / cnv;
			cutr[2] = cutr[2] / cnv;
			cutr[4] = cutr[4] / cnv;
			cutr[5] = cutr[5] / cnv;
			cutr[6] = cutr[6] / cnv;
			cutr[9] = cutr[9] / cnv;
		}
	}
/*
.....End of routine
*/
done:;
	return(ifnd);
}

/*********************************************************************
**    E_FUNCTION     : ncl_process_symcutter(scan,iclw,sym,offset,cfl,
**                                           clist,mdid,ncutr,tlno,tlen)
**			Processes a CUTTER/DISPLY clfile record while scanning the clfile
**			and determines if needs to be added to the Cutter List.
**    PARAMETERS   
**       INPUT  : 
**				scan    = 0 = Normal playback.
**                    1 = SEQUNC scan.
**                    2 = Bounding box scan.
**                    3 = Cutter scan.
**				iclw    = Integer array from clfile record.
**          sym     = Symbol name from clfile record.
**          offset  = Symbol Z-depths for lathe cutter from clfile record.
**          cfl     = 2 = Symbol, 3 = Point List, 4 = Tool file.
**          clist   = Tool list.
**          mdid    = 1 = Processed motion record since last Cutter.
**				ncutr   = Number of cutters currently in the list.
**          tlno    = Current tool number from LOADTL command.
**          tlen    = Current tool length from LOADTL command.
**       OUTPUT :  
**          ncutr   = Updated number of cutters in list when Scan=3.
**    RETURNS      : UU_TRUE if the new cutter is processed.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL
	ncl_process_symcutter(scan,iclw,sym,offset,cfl,clist,mdid,ncutr,tlno,tlen)
int scan;
int iclw[];
char *sym;
UU_REAL *offset;
int cfl;
UU_LIST *clist;
int *mdid,*ncutr;
int tlno;
UU_REAL tlen;
{
	int i,stat,flag,type,npts;
	UU_LOGICAL ifnd,same;
	UM_coord *pts;
	UN_cutter_list cdata,*cpt;
	double length;
/*
.....Initialize routine
*/
	if (scan != 0 && scan != 1 && scan != 3) goto done;
	cpt = (UN_cutter_list *) UU_LIST_ARRAY (clist);
	ifnd = UU_FALSE;
/*
.....Scanning clfile
.....Store tool data
*/
	if (scan == 3)
	{
		cdata.type = NCL_CUTTER_MILL;
		if (*ncutr > 0) cdata.type = cpt[*ncutr-1].type;
		cdata.color[0] = cdata.color[1] = cdata.color[2] = -1;
		cdata.cut_color = -1;
		cdata.symkey[0] = cdata.symkey[1] = cdata.symkey[2] = 0;
		cdata.symbol[1][0] = cdata.symbol[2][0] = '\0';
		ncl_get_tool_symlib(cdata.symlib);
		cdata.toler = LW_default_tool.toler;
		cdata.maxang = LW_default_tool.maxang;
		cdata.trans[0] = cdata.trans[1] = cdata.trans[2] =
			LW_default_tool.translucency;
		cdata.edge[0] = cdata.edge[1] = cdata.edge[2] = LW_default_tool.edge;
		cdata.edge_color[0] = cdata.edge_color[1] = cdata.edge_color[2] =
			LW_default_tool.edge_color;
		cdata.rapid = LW_default_tool.rapid;
		cdata.used = UU_TRUE;
		if (*ncutr > 0 && !cpt[*ncutr-1].used) cpt[*ncutr-1].used = *mdid;
		cdata.ctype[1] = cdata.ctype[2] = 0;
		cdata.shank_clash = LW_default_tool.shank_clash;
		for (i=0;i<4;i++) cdata.parms[1][i] = cdata.parms[2][i] = 0.;
		cdata.tlno = tlno; cdata.tlen = tlen; cdata.tlofs = 0.;
/*
........Load symbol
*/
		type = cfl;
		if (cfl == 2)
		{
			flag = 2; if (cdata.type == NCL_CUTTER_MILL) flag = 1;
			stat = ncl_load_cutter_symbol(cdata.symlib,sym,&cdata.symkey[0],flag,
				&type);
			if (stat == UU_SUCCESS)
			{
				if (type == 1 || type == 2) type = 3;
				else type = 2;
			}
/*
........Could not load symbol
........Might be a cutter profile
........(for example, when loading posted file)
*/
			else
			{
				type = 4;
			}
		}
/*
........Load cutter profile
*/
		if (type == 4)
		{
			stat = ncl_load_cutprof(sym,&pts,&npts,&length);
			if (stat != UU_SUCCESS) goto done;
		}
		cdata.ctype[0] = type;
/*
........Display cutter as symbol
*/
		strcpy(cdata.symbol[0],sym);
		cdata.isn = iclw[0];
		cdata.clrec = iclw[5];
		if (*ncutr == 0)
		{
			for (i=0;i<7;i++) cdata.cutter[i] = 0.;
		}
		else
		{
			for (i=0;i<7;i++) cdata.cutter[i] = cpt[*ncutr-1].cutter[i];
		}
		if (cdata.type == NCL_CUTTER_LATHE)
			cdata.cutter[2] = cpt[*ncutr-1].cutter[2];
		if (cdata.type == NCL_CUTTER_BLADE)
			cdata.cutter[0] = cpt[*ncutr-1].cutter[0];
		cdata.ncparm = 0;
		cdata.ctype[1] = cdata.ctype[2] = 0;
		cdata.parms[0][0] = cdata.parms[0][1] = 0.;
		cdata.parms[0][2] = offset[0];
		cdata.parms[0][3] = offset[1];
		same = UU_FALSE;
		if (*ncutr != 0)
		{
			if (strcmp(sym,cpt[*ncutr-1].symbol[0]) == 0 &&
				cpt[*ncutr-1].ncparm == 0) same = UU_TRUE;
		}
		if (!same)
		{
			if (*mdid == 0 && *ncutr != 0)
			{
				if (*ncutr > 1)
				{
					if (strcmp(sym,cpt[*ncutr-2].symbol[0]) == 0 &&
						cpt[*ncutr-2].ncparm == 0) same = UU_TRUE;
					if (same)
					{
						if (!cpt[*ncutr-1].used)
						{
							*ncutr = *ncutr - 1;
							uu_list_delete(clist,*ncutr,1);
						}
						else
						{
							uu_list_push(clist,&cdata);
							*ncutr = *ncutr + 1;
						}
					}
				}
				if (!same)
				{
					ifnd = UU_TRUE;
					if (!cpt[*ncutr-1].used)
						uu_move_byte(&cdata,&cpt[*ncutr-1],sizeof(UN_cutter_list));
					else
					{
						uu_list_push(clist,&cdata);
						*ncutr = *ncutr + 1;
					}
				}
			}
			else
			{
				uu_list_push(clist,&cdata);
				*ncutr = *ncutr + 1;
			}
			*mdid = 0;
		}
	}
/*
.....End of routine
*/
done:;
	return(ifnd);
}

/*********************************************************************
**    E_FUNCTION     : ncl_process_holder(scan,sym,attach,cfl,dfl,clist,ncutr)
**			Processes a CUTTER/DISPLY,SHANK-HOLDER clfile record and
**			determines if needs to be added to the Cutter List.
**    PARAMETERS   
**       INPUT  : 
**				scan    = 0 = Normal playback.
**                    1 = SEQUNC scan.
**                    2 = Bounding box scan.
**                    3 = Cutter scan.
**          sym     = Symbol name from clfile record.
**          attach  = Symbol Attach point from clfile record.
**          cfl     = 2 = Display as symbol, 3 = Pt-List, 4 = Tool file.
**          dfl     = 0 = Shank as cutter, 1 = Shank as holder, 2 = Holder.
**          clist   = Tool list.
**				ncutr   = Number of cutters currently in the list.
**       OUTPUT : none
**    RETURNS      : none
**                   is processed.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_process_holder(scan,sym,attach,cfl,dfl,clist,ncutr)
int scan;
char *sym;
UU_REAL *attach;
int cfl,dfl;
UU_LIST *clist;
int *ncutr;
{
	int stat,flag,type,npts;
	UU_LOGICAL *hptr;
	UU_KEY_ID *kptr,key;
	UU_REAL *aptr;
/*	char *sptr,symlib[22]; */
	char *sptr,symlib[256];
	UM_coord *pts;
	UN_cutter_list *cpt;
	UM_real8 length;
/*
.....Scanning clfile
.....Store tool data
*/
	if (scan == 3)
	{
		if (*ncutr != 0)
		{
			cpt = (UN_cutter_list *) UU_LIST_ARRAY (clist);
/*
........Shank
*/
			if (dfl == 0 || dfl == 1)
			{
				aptr = cpt[*ncutr-1].parms[1];
				sptr = cpt[*ncutr-1].symbol[1];
				kptr = &cpt[*ncutr-1].symkey[1];
				hptr = &cpt[*ncutr-1].ctype[1];
			}
/*
........Holder
*/
			else
			{
				aptr = cpt[*ncutr-1].parms[2];
				sptr = cpt[*ncutr-1].symbol[2];
				kptr = &cpt[*ncutr-1].symkey[2];
				hptr = &cpt[*ncutr-1].ctype[2];
			}
/*
........Load symbol
*/
			type = cfl;
			if (type == 2)
			{
				ncl_get_tool_symlib(symlib);
				flag = 1; if (cpt[*ncutr-1].type == NCL_CUTTER_LATHE) flag = 2;
				stat = ncl_load_cutter_symbol(symlib,sym,&key,flag,&type);
				if (stat == UU_SUCCESS)
				{
					if (type == 1 || type == 2) type = 3;
					else type = 2;
				}
/*
........Could not load symbol
........Might be a cutter profile
........(for example, when loading posted file)
*/
				else
					type = 4;
			}
			else key = 0;
/*
........Load cutter profile
*/
			if (type == 4)
			{
				stat = ncl_load_cutprof(sym,&pts,&npts, &length);
				if (stat != UU_SUCCESS) goto done;
			}
/*
........Store data
*/
			strcpy(sptr,sym);
			*kptr = key;
			aptr[0] = attach[0];
			aptr[1] = attach[1];
			aptr[2] = attach[2];
			aptr[3] = attach[3];
			*hptr = type;
			if (dfl == 0 || dfl == 1) cpt[*ncutr-1].shank_clash = dfl;
		}
	}
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_process_ipvcutter(scan,iclw,cnv,cutr,cfl,clist,
**		                                       cutsyb,shksyb,hldsyb,symkey)
**			Determines if this is an active cutter definition when running
**			NCLIPV and sets up the cutter paramters from the (modified) tool
**			list if it is.
**    PARAMETERS   
**       INPUT  : 
**				scan    = 0 = Normal playback.
**                    1 = SEQUNC scan.
**                    2 = Bounding box scan.
**                    3 = Cutter scan.
**                    4 = Preload lathe cutters onto turret.
**				iclw    = Integer array from clfile record.
**       OUTPUT :  
**          cutr    = Cutter parameters.
**          cfl     = Cutter display type flags.
**          clist   = Tool list.
**          cutsyb  = Cutter symbol name.
**          shksyb  = Cutter symbol name.
**          hldsyb  = Cutter symbol name.
**          symkey  = Unibase keys of cutter, shank, and holder symbols.
**    RETURNS      : UU_TRUE if a new cutter is processed.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_process_ipvcutter(scan,iclw,cutr,cfl,clist,cutsyb,shksyb,
	hldsyb,symkey)
int scan;
int *iclw;
UU_REAL cutr[];
int cfl[];
UU_LIST *clist;
char *cutsyb,*shksyb,*hldsyb;
UU_KEY_ID symkey[];
{
	int i,j,flags[10],idc[3],ifl,ncp;
	UU_LOGICAL ifnd;
	UU_REAL pos[6];
	UN_cutter_list *cpt;
	UN_motseg_cutter cutseg;
	UN_motseg_cutattr cattr;
	UU_REAL tdiff;
/*
.....Initialize routine
*/
	cpt = (UN_cutter_list *)UU_LIST_ARRAY(clist);
	ifnd = UU_FALSE;
/*
.....See if this is an active tool
*/
	for (i=0;i<LW_ntool;i++)
	{
		if (iclw[0] == cpt[i].isn && iclw[5] == cpt[i].clrec)
		{
			if (!cpt[i].used) break;
			ifnd = UU_TRUE;
			for (j=0;j<LW_spindle_nload;j++)
				LW_act_tool[LW_spindle_load[j]] = i;
/*
........Initialize cutter array
*/
			for (j=0;j<20;j++) cutr[j] = 0.;
/*
........Mill cutter
*/
			if (cpt[i].type == NCL_CUTTER_MILL)
			{
				if (cpt[i].ncparm < 5)
				{
/*					if (LW_mach_type == LW_LATHE) cutr[8] = 14;*/
					for (j=0;j<cpt[i].ncparm;j++) cutr[j] = cpt[i].cutter[j];
				}
				else
				{
					ifl = 1;
					ncp = cpt[i].ncparm;
					barseg(&ifl,cpt[i].cutter,cutr,&ncp);
				}
/*
.....Fix corner radius if rounding made it larger that half
.....cutter diameter - Andrew 3/5/13
*/
				tdiff = 2.*cutr[1] - cutr[0];
				if (tdiff > 0. && tdiff < LW_default_tool.toler)
					cutr[1] = cutr[0] / 2.;
			}
/*
........Lathe cutter
*/
			else if (cpt[i].type == NCL_CUTTER_LATHE)
			{
				cutr[0] = cpt[i].cutter[0] * 2.;
				cutr[1] = 0.;
				cutr[2] = cpt[i].cutter[2];
				cutr[3] = cutr[4] = 0.;
				cutr[5] = cpt[i].cutter[4];
				cutr[6] = cpt[i].cutter[1];
				cutr[7] = cpt[i].cutter[3];
				cutr[8] = 12;
				if (cutr[7] == 90.) cutr[8] = 11;
				if (cutr[7] == 60.) cutr[8] = 13;
				if (cutr[7] == 0. && cutr[6] == 0.) cutr[8] = 14;
				if (cutr[7] == 0. && cutr[6] != 0.) cutr[8] = 15;
			}
/*
........Blade cutter
*/
			else if (cpt[i].type == NCL_CUTTER_BLADE)
			{
				cutr[0] = -1.;
				cutr[5] = cpt[i].cutter[0];
				cutr[6] = cpt[i].cutter[1];
				cutr[2] = cpt[i].cutter[2];
				cutr[7] = sin(cpt[i].cutter[3]/UM_RADIAN);
			}
/*
........Cutter display variables
*/
			for (j=0;j<3;j++)
			{
				cfl[j] = cpt[i].ctype[j];
				idc[j] = cfl[j];
				symkey[j] = cpt[i].symkey[j];
			}
			strcpy(cutsyb,cpt[i].symbol[0]);
			strcpy(shksyb,cpt[i].symbol[1]);
			strcpy(hldsyb,cpt[i].symbol[2]);
/*
........Attach points
*/
			cutr[18] = cpt[i].parms[0][2]; cutr[19] = cpt[i].parms[0][3];
			for (j=0;j<4;j++)
			{
				cutr[13+j] = cpt[i].parms[1][j];
				cutr[9+j] = cpt[i].parms[2][j];
			}
			for (j=0;j<20;j++) cutdef[0].cutr[j] = cutr[j];
/*
........Setup cut color
*/
			if (cpt[i].cut_color == -1)
			{
				if (LW_cutcolor_index == -1) ul_ipv_set_colors();
				if (LW_cutcolor_index >= LW_n_cut_colormap)
					LW_cutcolor_index = 0;
				LW_cut_material = LW_cut_colormap[LW_cutcolor_index];
				if (scan == 0) LW_cutcolor_index++;
			}
			else LW_cut_material = cpt[i].cut_color;
/*
........Setup tool color
*/
			if (cpt[i].color[0] == -1)
			{
				if (LW_default_tool.color == -1)
					LW_tool_material = LW_cut_material;
				else
					LW_tool_material = LW_default_tool.color;
			}
			else LW_tool_material = cpt[i].color[0];
/*
........Setup tool tolerance
*/
			LW_toler = cpt[i].toler;
			LW_maxang = cpt[i].maxang;
			LW_translucency[0] = cpt[i].trans[0];
			LW_translucency[1] = cpt[i].trans[1];
			LW_translucency[2] = cpt[i].trans[2];
/*
........Process the updated cutter parameters
*/
			ncl_cutter_get_defattr(&cattr);
			flags[0] = cattr.segfl;
			flags[1] = cattr.mov;
			flags[2] = cattr.shaded[0];
			flags[5] = cattr.shaded[1];
			flags[6] = cattr.shaded[2];
			ncl_cutter_set(cutr,idc,flags,cutsyb,shksyb,hldsyb,symkey);
			ncl_cutter_get(&cutseg,UN_MOTSEG_ACTIVE);
/*
........Draw the tool parts if
........defined as parameters
........in case the user changed them
*/
			ncl_draw_cutter_parts(&cutseg,cfl,0,1);
/*
........Attach tool to turret
........if scanning for turret tools
*/
			if (scan == 4)
			{
				pos[0] = 0.; pos[1] = 0.; pos[2] = 0.;
				pos[3] = 0.; pos[4] = 1.; pos[5] = 0.;
				ul_ipv_cutters(pos,cutr,UU_NULL,UU_NULL,scan);
			}
			break;
		}
	}
/*
.....End of routine
*/
done:;
	return(ifnd);
}

/*********************************************************************
**    E_FUNCTION     : ncl_process_loadtl(scan,clist,mdid,ncutr,tlno,tlen)
**			Stores the tool number and tool length from a LOADTL command
**       with the current cutter if there is no motion programmed after
**       the CUTTER statement and scanning of the clfile is active for
**       cutters is active.
**    PARAMETERS   
**       INPUT  : 
**				scan    = 0 = Normal playback.
**                    1 = SEQUNC scan.
**                    2 = Bounding box scan.
**                    3 = Cutter scan.
**          clist   = Tool list.
**          mdid    = 1 = Processed motion record since last Cutter.
**				ncutr   = Number of cutters currently in the list.
**          tlno    = Current tool number from LOADTL command.
**          tlen    = Current tool length from LOADTL command.
**       OUTPUT :  
**				clist   = Updated tool number & length for current cutter.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_process_loadtl(scan,clist,mdid,ncutr,tlno,tlen)
int scan;
UU_LIST *clist;
int mdid,ncutr;
int tlno;
UU_REAL tlen;
{
	UN_cutter_list *cpt;
/*
.....Initialize routine
*/
	if (scan != 3 || ncutr == 0) goto done;
/*
.....Cutter is not used
.....Store tool data
*/
	cpt = (UN_cutter_list *) UU_LIST_ARRAY (clist);
	if (!mdid)
	{
		cpt[ncutr-1].tlno = tlno;
		cpt[ncutr-1].tlen = tlen;
	}
/*
.....End of routine
*/
done:;
	return;
}
/*********************************************************************
**    E_FUNCTION     : ncl_load_cutter_symbol(symlib,sym,key,flag,type)
**			Loads a symbol to use as either a cutter or holder.
**    PARAMETERS   
**       INPUT  : 
**				symlib  = Symbol library.
**				sym     = Symbol to load.
**          flag    = 1 = Requires surface of revolution.
**                    2 = Requires composite curve.
**       OUTPUT :  
**          key     = Surface of revolution key.
**          type    = 1 = Surface of revolution, 2 = Composite Curve,
**                    0 = Unknown.
**    RETURNS      : UU_SUCCESS if symbol was successfully loaded and
**                   is requested geometry type.  UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_load_cutter_symbol(symlib,sym,key,flag,type)
char *symlib,*sym;
UU_KEY_ID *key;
int flag;
int *type;
{
	int status;
	UU_LOGICAL found;
	struct UB_symbol_rec symrec;
	struct NCL_fixed_databag subent;
/*
.....Initialize routine
*/
	*key = 0;
	status = UU_FAILURE;
/*
.....Is symbol already loaded
*/
	ncl_parse_label(sym,symrec.label,&symrec.subscr);
	ub_get_symmaster_by_name(&symrec,&found,1,1);
/*
.....Master not found
.....Try to load it
*/
	if (!found)
	{
		if (ubi_load_file("local",symlib,sym,UU_NULL,"WHOLESYM",
			&symrec,UX_NPRTERRS) != UU_SUCCESS)
		{
			if (ubi_load_file("system",symlib,sym,UU_NULL,"WHOLESYM",
				&symrec,UX_NPRTERRS) != UU_SUCCESS) goto done;
		}
	}
/*
.....Make sure first entity is
.....surface of revolution
*/
	subent.key = symrec.geom[0];
	if (ncl_retrieve_data_fixed(&subent,sizeof(subent)) != UU_SUCCESS)
		goto done;
	if (subent.rel_num != UM_SOLID_REL)
	{
		if (flag == 1 && subent.rel_num != NCL_REVSURF_REL) goto done;
		else if (flag == 2 && subent.rel_num != UM_COMPCRV_REL) goto done;
	}
	*type = 0;
	if (symrec.no_geom == 1)
	{
		if (subent.rel_num == NCL_REVSURF_REL) *type = 1;
		else if (subent.rel_num == UM_COMPCRV_REL) *type = 2;
	}
	*key = subent.key;
	status = UU_SUCCESS;
/*
.....End of routine
*/
done:;
	return(status);
}
