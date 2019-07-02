/*********************************************************************
**
**    NAME         :  zdrfttut.h
**
**       CONTAINS:
**       	DD1 tutorial definition file
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       zdrfttut.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:24
**
*********************************************************************/

#ifndef ZDRFTTUTH


#include "dtuto.h"
#include "ustdio.h"

static UD_TOPICS UZ_pidftfpttxt[3] =
						    { {"zidftfp1.tut", 2},
								{"zidftfp2.tut", 5},
								{"zidftfp3.tut", 1} } ;

static UD_TUTO UZ_pidftfpt = { 3, "zidftfpt.tut", 1, UZ_pidftfpttxt };

static UD_TUTO UZ_drafting = { 0, "zidrftng.tut", 3, NULL };
static UD_TUTO UZ_zdftlin  = { 0, "zdftlin.tut",  3, NULL };
static UD_TUTO UZ_pidfthor = { 0, "zidfthor.tut", 3, NULL };
static UD_TUTO UZ_pidftcho = { 0, "zidftcho.tut", 3, NULL };
static UD_TUTO UZ_pidftver = { 0, "zidftver.tut", 3, NULL };
static UD_TUTO UZ_pidftcve = { 0, "zidftcve.tut", 3, NULL };
static UD_TUTO UZ_pidftpar = { 0, "zidftpar.tut", 3, NULL };
static UD_TUTO UZ_pidftcpa = { 0, "zidftcpa.tut", 3, NULL };
static UD_TUTO UZ_pidftang = { 0, "zidftang.tut", 4, NULL };
static UD_TUTO UZ_pidftper = { 0, "zidftper.tut", 3, NULL };
static UD_TUTO UZ_pidftthk = { 0, "zidftthk.tut", 3, NULL };
static UD_TUTO UZ_zdftoth  = { 0, "zdftoth.tut",  2, NULL };
static UD_TUTO UZ_pidftarl = { 0, "zidftarl.tut", 3, NULL };
static UD_TUTO UZ_pidftrad = { 0, "zidftrad.tut", 3, NULL };
static UD_TUTO UZ_pidftdia = { 0, "zidftdia.tut", 5, NULL };
static UD_TUTO UZ_pidftnot = { 0, "zidftnot.tut", 2, NULL };
static UD_TUTO UZ_pidftlab = { 0, "zidftlab.tut", 4, NULL };
static UD_TUTO UZ_pidftxht = { 0, "zidftxht.tut", 3, NULL };
static UD_TUTO UZ_pidftaro = { 0, "zidftaro.tut", 1, NULL };
static UD_TUTO UZ_pidrwscl = { 0, "zidrwscl.tut", 4, NULL };
static UD_TUTO UZ_pidftsdv = { 0, "zidftsdv.tut", 1, NULL };
static UD_TUTO UZ_pidftstd = { 0, "zidftstd.tut", 1, NULL };
static UD_TUTO UZ_pidftedt = { 0, "zidftedt.tut", 4, NULL };
static UD_TUTO UZ_drftmodl = { 0, "zdrftmod.tut", 3, NULL };
static UD_TUTO UZ_pidfmtxt = { 0, "zidfmtxt.tut", 4, NULL };
static UD_TUTO UZ_pidfmtol = { 0, "zidfmtol.tut", 3, NULL };
static UD_TUTO UZ_pidfmdim = { 0, "zidfmdim.tut", 3, NULL };
static UD_TUTO UZ_pidfmdil = { 0, "zidfmdil.tut", 1, NULL };
static UD_TUTO UZ_pidfmexl = { 0, "zidfmexl.tut", 1, NULL };
static UD_TUTO UZ_pidfmlel = { 0, "zidfmlel.tut", 1, NULL };
static UD_TUTO UZ_pidfmaro = { 0, "zidfmaro.tut", 1, NULL };
static UD_TUTO UZ_pidfmspc = { 0, "zidfmspc.tut", 1, NULL };

#define ZDRFTTUTH
#endif
