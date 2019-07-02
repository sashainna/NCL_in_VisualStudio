/*********************************************************************
**    NAME         :wsntcfunc.h
**				functions calls from C++ to C 
** 
**    CONTAINS     : 
**  
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntcfunc.h , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**			12/01/15 , 10:29:38
*********************************************************************/
#ifndef NCLCFUNC_H
#define NCLCFUNC_H

#include <Windows.h>
#include "udfdata.h"
#include "udforms.h"
#include "ddef.h"
#include "mdcoord.h"
#include "xenv1.h"
#include "dmotif.h"
#include "uims.h"
#include "mfort.h"
#include "mpocket.h"

#ifdef __cplusplus 
	#define EXT extern "C"
#else
	#define EXT extern
#endif

EXT int ud_getpos_from_bararea(int flag, char*fname, char *bar_area, int bar_pos[3], int bar_size[2]);
EXT int udm_read_menu(char *filename, int pos[3], int size[2], int kdis, int kflag,int menutype);
EXT int udm_read_layout(UD_UIMS *duims, char *fullname);
EXT int ud_wrerr(char *tmpstr);
EXT int ud_rpwrmenu(char *func, char *parms, char *name);
EXT int ud_chk_rprd();
EXT int ud_rpwrform(char *key, char *msg1, char *msg2);
EXT int ud_rdform(UD_FSTRUCT *fStruct, UD_FDATA *fData, UD_FSTRUCT *fstruct, UD_FDATA *fdata);
EXT int ud_formstr_format(int typ, UD_DDATA data, int prec, int len,
				char *buf);
EXT int ud_dasin(char ibuf[], UD_DASDATA *dsda, int  dasflag);
EXT int ud_todas(UD_DDATA *data,UD_DASIN *das, int dtyp); 
EXT int ud_ckdata(UD_FSTRUCT fStruct, UD_DASIN *de, int fno);
EXT int ud_delform(UD_FSTRUCT *fStruct);
EXT int ud_get_pickstr(char *prompt, int datatype, char *lstr, int prec, int len, int flag);
EXT int ud_jmpmark(int markparm);
EXT int ud_printmsg(char *msg);
EXT void ud_format_formstr(int typ, int *data, int prec, int len, char* buf);
EXT int ud_int_frm_read(char *str,int stat,char key);
EXT void ud_format_formstr(int typ, int *data, int prec, int len, char *buf);
EXT int ud_ntload_toggle(int kinc, int bindx);
EXT int ud_brower_rprd(char *fnam, int *nc);
EXT int ud_brower_endrprd(char *fnam, int nc);
extern "C" int ud_is_playback();

EXT int ul_break_fname(char *fullname, char* dir, char* fname);
EXT int ul_to_lower(char *str);
EXT int ul_get_full_dir(char *dir, char *fullname);
EXT int ul_modal_check (char *buf, int *ityp, char *ctyp, char *cmsg);
EXT int ul_to_upper(char *ctyp);
EXT int ul_to_reals(UU_REAL rval[2], int *inum, int maxnum, char str[]);
EXT int ul_strip_blanks (char *str, int *size);
EXT void ul_getvalid_fulldir(char*, char*);
EXT int ul_build_full_dir (char* dir, char *subdir, char *fdir);
EXT int ul_remove_quotes(char *str);
EXT int ul_get_base_fname(char *pathname, char *basename, int args[], int options);

EXT int um_ccstomcs(int option, UM_coord ccs, UM_coord  mcs);
EXT int um_load_pocket_drawing(char *title, char*dname, char*fname, char *sysdir, int eflag);
EXT int um_close_pocket_window(UM_pkwin_type type);

EXT char *uu_move_byte(register char* from_ptr, register char* to_ptr, register int length);
EXT char *uu_lsinsrt (char *, int len);

EXT int ux_search_for_path(char *, char*, int);
EXT int ux_mk_chk_syspath(char *path_prefix, char *path, char *filename, 
		char *fenv,  char *ftype, int *modeptr,
		UX_pathname totalpath, int options);
EXT int ux_get_syspath(char *oinpath, char **pathlistptrptr, UX_pathname outpath, 
					   UU_LOGICAL *foundptr, int options);
EXT int ux_create_file(char *, int, char *, char *, char *, 
						char *, int *,	int);
EXT int ux_get_os_filedesc(int, FILE **, int);
EXT int ux_fputs0(char *,FILE *);
EXT int ux_close(int, int);
EXT int ux_strip_quotes(char*);
EXT int ux_delete(char*);

EXT void uw_nterror(char*);
EXT int uw_ntform(UD_FSTRUCT *fstruct, UD_FDATA *fdata);
EXT int uw_ntget_strsize(char *string, int pt, char *fntname, int *wid, int* hgt);
EXT int uw_ntget_strscsize(char *string, int pt, char *fntname, int *wid, int* hgt);
EXT void uw_ntget_avrchr_size(int pt, char *fntname, int *wid, int *hgt);
EXT int uw_save_barpos(UDM_LAYOUT *layout);
EXT int uw_ntresize_graphics(int, int, int);
EXT void uw_getmarea_list(char area_lst[100][80], int *num_area);
EXT int uw_ntadd_menuarea(char *areaname, int dir, int rows, int cols, int width, int height);
EXT int uw_ntdelete_menuarea(char *areaname);
EXT int uw_ntyes_or_no(CWnd* parent, char* msg, char *title);
EXT int uw_ntyesnocancel(CWnd *parent, char* msg, char *title);
EXT void uw_ntget_dirname(CWnd* parent, char *title, char* fnam, int *nc, char *paths, char *path_des);
EXT int uw_ntprmerr(char *msg);

EXT void ud_get_filename(CWnd* parent, char *title, char* filter, char* fnam, int *nc, char *descrip, int open_flag);
EXT int uz_status();
EXT int znu_copyright();
EXT void getver(UM_real8 *ver);
EXT void uu_free(char *chunk_data);
EXT int ncl_get_scalar_value(char *text, double *value);
EXT void uu_lsdel(char * l);
EXT int ux_mk_pthdir(char * path);
#undef EXT
#endif
