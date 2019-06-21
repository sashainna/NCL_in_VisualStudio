/*********************************************************************
**    NAME         :  nclmodals.h
**       CONTAINS: NCL modal definitions.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       nclmodals.h , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:06:36
*********************************************************************/
/*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!                                                             !!!!!!
!!!!!!   NOTE:                                                     !!!!!!
!!!!!!      Recompile routine NEGLOBAL.C to cause any changes      !!!!!!
!!!!!!      made in this include file to be reflected in the       !!!!!!
!!!!!!      executable.                                            !!!!!!
!!!!!!                                                             !!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/
#include "ulist.h"

#ifndef NCL_MODALS

#ifdef NCL_MPGM

UU_LOGICAL NCL_auto_label = UU_TRUE;
UU_LOGICAL NCL_auto_subscr = UU_FALSE;
UU_LOGICAL NCL_display_label = UU_FALSE;
UU_LOGICAL NCL_display_leader = UU_FALSE;
UU_LOGICAL NCL_edit_mode = UU_FALSE;
UU_LOGICAL NCL_explicit_check = UU_FALSE;
UU_LOGICAL NCL_labeldisp = UU_FALSE;
int NCL_toggle_off =0;
int NCL_alter = 0;
#if UU_COMP == UU_VAXVMS
#ifdef GPX
float      NCL_cmdwind_ury = 10 * (.75/40);
#else
/* float      NCL_cmdwind_ury = 10 * (.75/32); */
float      NCL_cmdwind_ury;
#endif
#endif
#if UU_COMP != UU_VAXVMS
float      NCL_cmdwind_ury;
#endif
UU_LOGICAL NCL_window_background = UU_FALSE;
UU_LOGICAL NCL_cmd_window_mode = UU_FALSE;
/*
.....initial value is *SAME
*/
int NCL_com_mode = 2;
#if UU_COMP==UU_WIN2K
int	       glines = 20;
#else
int	       glines = 10;
#endif

int NCL_macro_outflag, NCL_macro_remval, NCL_macro_outdefault, NCL_macro_modal;

#else

extern UU_LOGICAL NCL_auto_label;
extern UU_LOGICAL NCL_auto_subscr;
extern UU_LOGICAL NCL_display_label;
extern UU_LOGICAL NCL_display_leader;
extern UU_LOGICAL NCL_edit_mode;
extern UU_LOGICAL NCL_explicit_check;
extern float      NCL_cmdwind_ury;
extern int        glines;
extern UU_LOGICAL NCL_window_background;
extern UU_LOGICAL NCL_cmd_window_mode;
extern int NCL_com_mode;
extern UU_LOGICAL NCL_labeldisp;
extern int NCL_toggle_off;
extern int NCL_alter;
extern int NCL_macro_outflag, NCL_macro_remval, NCL_macro_outdefault, NCL_macro_modal;

#endif

#define NCL_MODALS 
#define NCL_LABEL_BIT  1
#define NCL_ALTER_BIT  2
#define NCL_LDR_BIT  4

#endif

