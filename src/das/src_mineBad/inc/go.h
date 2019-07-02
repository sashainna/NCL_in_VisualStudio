/********************************************************************* 
**  NAME:   gkso.h
**
**		Output Type definitions for GKS. 
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       go.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:21
**
**  PARAMETERS   
**      INPUT:  none 
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
/* From ANSI X3H34/83-12R1, the GKS C-language binding */

/****************************************************************/
/*							**NOTICE**											*/
/*		This is a temporary replacement of the original include	*/
/*     file and should not be used for new development.			*/
/*		One should use the smaller include files made from			*/
/*     breaking up the original version of this include file.	*/
/****************************************************************/
#ifndef GKSOH
#include "ustdio.h"
#include "gsegdet.h"
#include "go1.h"
#include "go2.h"
#include "go3.h"
#include "gobas.h"
#include "goseg.h"
#include "gomisc.h"
#include "goatt.h"
#include "gobndl.h"
#include "gofac.h"
#include "gows.h"
#include "goren.h"
#define GKSOH
#endif
