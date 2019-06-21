/**************************************************************
**         NAME: wsntglfunc.h
**         Contains: Opengl function declearation
**
**    COPYRIGHT 2000 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       wsntglfunc.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:19
*********************************************************************/
#ifndef NTGLFUNC_H
#define NTGLFUNC_H

#include <GL/gl.h>
#include <GL/glu.h>

#ifdef WSGL_FIRST
	#define EXT
#else
	#ifdef __cplusplus 
		#define EXT extern "C"
	#else
		#define EXT extern
	#endif
#endif

EXT void uw_glinit();
EXT void uw_glredrawws();
EXT void uw_ntsctogc(POINT *pt);
EXT int uw_ntevent (int *event, int pflag, int xy[2], int cflag);
EXT void uw_glcolor(int color);
EXT void uw_ntsetcursor(int cursorno);
EXT int uw_ntget_event(int *event,int pflag, int *xpos, int*ypos, int cflag);
EXT void uw_glinit_visual();
EXT int uw_glglxinit(int size[2]);
EXT void uw_glgraphsurf();
EXT int uu_init_mpe();
EXT int ud_getuims();
EXT int unicad_();
EXT void uw_gldevtondc(int rast[2], Gfloat ndc[2]);
EXT void uw_glredrawsegrect(int ws, Gnrect *segrect, Gint segno);
EXT void uw_glresize_graphics(int cx, int cy, int disp);
EXT void uw_glset_scissor(int clip[4]);

EXT int nclu_ln_xyz_xyz(int flag);
EXT int nclu_ln_pt_pt();
EXT int current_glctx;
EXT int uz_wnt_callfunc(int num, int flag);
EXT int uz_ntget_dspt(int num, char* descript, int flag);
EXT int uw_ntwrplabel(char *msg);
EXT int ug_save_event();
EXT int ug_reset_event();
EXT int uw_ntgetprm(char *prompt);
EXT void uw_ntsetcom_focus(int focus);
EXT int uw_ntset_cmdstr(char *msg);
EXT int uw_ntgetprlabel(char *msg);
EXT int uw_ntgetprmerr(char *msg);
EXT int uw_ntwrprm(char *pstr);
EXT int uw_ntprmerr(char *estr);
EXT int uz_ntstatus_calls(unsigned int id);

EXT void uw_ntenable_cmd(int flag);
EXT int uw_ntget_cmdstr(char *cmdstr);
EXT int uw_ntfirst_resize_graphics();
EXT int uw_ntcreate_font(int which);
EXT void uw_ntupd_comfont();
EXT void uw_glshd_switch_resize();
EXT int uv_current_screen_name(char *cur_screen);
EXT int uw_ntpaint_pocket(int cx, int cy);
EXT int uw_ntset_maingw(char *cur_screen);
EXT int uw_ntset_gwcapture();
EXT int uw_ntrelease_capture();
#undef EXT
#endif
