/*********************************************************************
**
**    NAME         :  zgeomtut.h
**
**       CONTAINS:
**       	DD1 tutorial definition file
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       zgeomtut.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:25
**
*********************************************************************/

#ifndef ZGEOMTUTH


#include "dtuto.h"
#include "ustdio.h"

static UD_TUTO UZ_wirefram = { 0, "zwirefra.tut", 3, NULL };
static UD_TUTO UZ_pinotes 	= { 0, "zinotes.tut", 1, NULL };
static UD_TUTO UZ_pitrmmod = { 0, "zitrmmod.tut", 1, NULL };
static UD_TUTO UZ_points 	= { 0, "zpoints.tut", 1, NULL };
static UD_TUTO UZ_pisglpt 	= { 0,"zisglpt.tut", 1, NULL };
static UD_TUTO UZ_pidelpt 	= { 0, "zidelpt.tut", 1, NULL };
static UD_TUTO UZ_pilinpts = { 0, "zilinpts.tut", 2, NULL };
static UD_TUTO UZ_picirpts = { 0, "zicirpts.tut", 2, NULL };
 static UD_TUTO UZ_piintpts = { 0, "ziintpts.tut", 2, NULL };
static UD_TUTO UZ_lines 	= { 0, "zlines.tut", 1, NULL };
static UD_TUTO UZ_pilin2pt = { 0, "zilin2pt.tut", 1, NULL };
static UD_TUTO UZ_pilincon = { 0, "zilincon.tut", 1, NULL };
static UD_TUTO UZ_pilinpnt = { 0, "zilinpnt.tut", 2, NULL };
static UD_TUTO UZ_pilintt 	= { 0, "zilintt.tut", 2, NULL };
static UD_TUTO UZ_pilinpao = { 0, "zilinpao.tut",2, NULL };
static UD_TUTO UZ_pilinpap = { 0, "zilinpap.tut",2, NULL };
static UD_TUTO UZ_pilinprp = { 0, "zilinprp.tut",2, NULL };
static UD_TUTO UZ_pilinang = { 0, "zilinang.tut",2, NULL };
static UD_TUTO UZ_pilinhvn = { 0, "zilinhvn.tut",2, NULL };
static UD_TUTO UZ_pilinchm = { 0, "zilinchm.tut",2, NULL };
static UD_TUTO UZ_arcs 		= { 0, "zarcs.tut",   1, NULL };
static UD_TUTO UZ_piarc3pt = { 0, "ziarc3pt.tut", 1, NULL };
static UD_TUTO UZ_piarccnr = { 0, "ziarccnr.tut", 2, NULL };
static UD_TUTO UZ_piarcc2p = { 0, "ziarcc2p.tut", 1, NULL };
static UD_TUTO UZ_piarcttr = { 0, "ziarcttr.tut", 2, NULL };
static UD_TUTO UZ_piarcttt = { 0, "ziarcttt.tut", 2, NULL };
static UD_TUTO UZ_piarcsfl = { 0, "ziarcsfl.tut", 2, NULL };
static UD_TUTO UZ_piarcmfl = { 0, "ziarcmfl.tut", 2, NULL };
static UD_TUTO UZ_circles 	= { 0, "zcircles.tut", 1, NULL };
static UD_TUTO UZ_picir3pt = { 0, "zicir3pt.tut", 1, NULL };
static UD_TUTO UZ_picir2di = { 0, "zicir2di.tut", 1, NULL };
static UD_TUTO UZ_picircnr = { 0, "zicircnr.tut", 1, NULL };
static UD_TUTO UZ_picircep = { 0, "zicircep.tut", 1, NULL };
static UD_TUTO UZ_picircnt = { 0, "zicircnt.tut", 2, NULL };
static UD_TUTO UZ_picirttr = { 0, "zicirttr.tut", 2, NULL };
static UD_TUTO UZ_picirttt = { 0, "zicirttt.tut", 2, NULL };
static UD_TUTO UZ_compcurv = { 0, "zcompcur.tut", 2, NULL };
static UD_TUTO UZ_picpcmrg = { 0, "zicpcmrg.tut", 1, NULL };
static UD_TUTO UZ_picpccnl = { 0, "zicpccnl.tut", 1, NULL };
static UD_TUTO UZ_picpcspt = { 0, "zicpcspt.tut", 1, NULL };
static UD_TUTO UZ_picpcdis = { 0, "zicpcdis.tut", 1, NULL };
static UD_TUTO UZ_othrcurv = { 0, "zothrcur.tut", 1, NULL };
static UD_TUTO UZ_piotcell = { 0, "ziotcell.tut", 2, NULL };
static UD_TUTO UZ_piotcrbs = { 0, "ziotcrbs.tut", 2, NULL };
static UD_TUTO UZ_extrusions = { 0, "zextrusions.tut", 1, NULL };
static UD_TUTO UZ_piextlin = { 0, "ziextlin.tut", 2, NULL };
static UD_TUTO UZ_piextrot = { 0, "ziextrot.tut", 2, NULL };
static UD_TUTO UZ_symmgmt 	= { 0, "zsymmgmt.tut", 3, NULL };
static UD_TUTO UZ_pisymcre = { 0, "zisymcre.tut", 4, NULL };
static UD_TUTO UZ_pisymdel = { 0, "zisymdel.tut", 1, NULL };
static UD_TUTO UZ_pisymlst = { 0, "zisymlst.tut", 1, NULL };
static UD_TUTO UZ_pisymlbl = { 0, "zisymlbl.tut", 1, NULL };
static UD_TUTO UZ_pisymlod = { 0, "zisymlod.tut", 1, NULL };
static UD_TUTO UZ_pisymlol = { 0, "zisymlol.tut", 1, NULL };
static UD_TUTO UZ_pisympla = { 0, "zisympla.tut", 3, NULL };
static UD_TUTO UZ_pisymexp = { 0, "zisymexp.tut", 1, NULL };
static UD_TUTO UZ_pisymuld = { 0, "zisymuld.tut", 1, NULL };
static UD_TUTO UZ_pisymsdm = { 0, "zisymsdm.tut", 2, NULL };
static UD_TUTO UZ_pisymssv = { 0, "zisymssv.tut", 1, NULL };
static UD_TUTO UZ_pisymren = { 0, "zisymren.tut", 1, NULL };
static UD_TUTO UZ_pisymarc = { 0, "zisymarc.tut", 1, NULL };
static UD_TUTO UZ_pisymlal = { 0, "zisymlal.tut", 1, NULL };
static UD_TUTO UZ_pisymsdl = { 0, "zisymsdl.tut", 1, NULL };
static UD_TUTO UZ_pisymcrl = { 0, "zisymcrl.tut", 1, NULL };
static UD_TUTO UZ_pisymdsl = { 0, "zisymdsl.tut", 1, NULL };

#define ZGEOMTUTH
#endif
