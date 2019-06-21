/*********************************************************************
**    NAME         :  m9epocket.c
**       CONTAINS:
**			um_pocket_window()
**			um_load_pocket_drawing()
**			um_load_pocket_drawing2()
**			um_view_pocket_drawing()
**			um_get_pocket_window()
**			um_set_pocket_graphics()
**			um_reset_pocket_graphics()
**			um_is_pocket_window()
**			um_close_pocket_window()
**			um_get_ncl_win()
**			um_minimize_pocket_window()
**			um_dwpocket_reset()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**       m9epocket.c , 26.2
**  DATE AND TIME OF LAST  MODIFICATION
**       05/22/18 , 10:20:25
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "usysg.h"
#include "class.h"
#include "mdcoord.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdattr.h"
#include "mattr.h"
#include "mxxx.h"
#include "mdebug.h"
#include "mdraw.h"
#include "mpocket.h"
#include "view.h"
#include "uims.h"
#include "xenv1.h"
#include "xfsys1.h"

char *UM_pocket_hwnd = 0;

extern int UW_pocket_mode;
extern int UM_swap_ipv;

static int UM_pocket_graphics = UU_FALSE;
static UM_pkwin_type current_sarea = UM_NCL_WINDOW;

void um_set_screen_area();
extern int NCL_animation;

/*********************************************************************
**    E_FUNCTION     : int um_pocket_window(name,type)
**       Creates a Pocket Graphics Window and displays the requested
**			drawing in it.
**    PARAMETERS   
**       INPUT  : 
**				title:	title of the pocket window
**          drawing         Drawing to view.
**          type            Type of pocket window to open, either
**                          UM_DRAWING_WINDOW or UM_IPV_WINDOW.
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS if no error;  UU_FAILURE if error.
**    SIDE EFFECTS : 
**			Opens a separate graphics window.
**    WARNINGS     :
**			Only one window of a given type may be opened
**                      at a time.
*********************************************************************/
int um_pocket_window(title, drawing,type)
char *title;
int *drawing;
UM_pkwin_type type;
{
	int status;
/*
.....Open the Pocket Graphics Window
.....(The drawing is displayed in
.....the window refresh routine)
*/
	status = ud_open_pocket(title, drawing,type);
	uw_gllighting_reset();
/*
.....End of routine
*/
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : um_load_pocket_drawing(title,dname,fname,sysdir)
**			Displays the specified drawing in a Pocket Graphics Window.
**			This drawing could be a drawing file, or graphic file such 
**			as JPEG, GIF or BITMAP file. It will attempt to load the drawing
**			if it is not found in the current Unibase. If can't find drawing or load
**			drawing file, then we will try to load a JPEG/GIF/BITMAP file
**			to display
**			
**    PARAMETERS   
**       INPUT  : 
**				title:	title of the drawing window
**				dname        Drawing name to view.  If a drawing file is
**				             loaded from this routine, then the drawing
**				             will be renamed to 'dname'.
**          fname        Filename to load if the drawing is not found.
**          sysdir       System directory for drawing, if not found in
**                       current directory.
**			eflag:		error message display flag
**					1:  display error message
**					0: don't display error message
**       OUTPUT :  
**          none
**    RETURNS      : UU_FAILURE on failure
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_load_pocket_drawing(title, dname,fname,sysdir,eflag)
char *title, *dname,*fname,*sysdir;
int eflag;
{
	FILE *fptr = NULL;
#if UU_COMP == UU_WIN2K
	int i, status,mode, rep, file_find, type;
	UX_pathname name,filename,fnamea,dir,fullname,tname,odir;

	char basefile[UX_MAX_FILE_LEN], sname[UX_MAX_FILE_LEN], *indx,*strrchr();
#endif
/*
.....Unix systems only support .dw files
*/
#if UU_COMP != UU_WIN2K
	if (um_load_pocket_drawing2(title, dname,fname,sysdir,eflag)==UU_SUCCESS)
		return UU_SUCCESS;
	else
		return UU_FAILURE;
/*
.....Windows NT supports .dw and Graphics Files
*/
#else
	if (um_load_pocket_drawing2(title, dname,fname,sysdir,0)==UU_SUCCESS)
		return UU_SUCCESS;

	file_find = 0;
	ul_break_fname(fname,odir,basefile);
/*
.....Create the JPEG file name
*/
	strcpy(name,basefile);
/*
.....we also need to remove preceding spaces
.....Yurong 3/1/02
*/
	for (i=0; i<strlen(name); i++)
	{
		if (name[i]!=' ') break;
	}
	strcpy(name, &(name[i]));
	for (i=strlen(name); i>0; i--)
	{
		if (name[i-1]==' ')
			name[i-1] = '\0';
		else
			break;
	}
	strcpy(sname, name);
	rep = 0;
	type = 0;
repeat:;
	indx = strrchr(name, '.');
	if (indx!=NULL) *indx = 0;
	if (type==0)
		strcat (name, ".bmp");
	else if (type==1)
		strcat(name, ".JPG");
	else if (type==2)
		strcat(name, ".GIF");
	else
	{
		status = UU_FAILURE;
		return(status);
	}

	strcpy(filename, name);
	if (odir[0]=='\0')
	{
		ul_get_full_dir(".",dir);
		mode = UX_EXISTS|UX_READ;
		status = ux_mk_chk_syspath(UU_NULL,dir,filename,UU_NULL,
				UU_NULL,&mode,fullname,UX_NPRTERRS);
		if (rep == 0)
			strcpy(tname, fullname);
		if (status != UU_SUCCESS || mode == (mode|UX_NEXISTS))
		{
			strcpy(fullname, filename);
			status = ul_open_mod_file2(sysdir, NULL, fullname, 0, &fptr, UU_NULL, UU_NULL);
			if (status != UU_SUCCESS)
			{
/*
.....the drawing name could be upper/lower case
.....try the lower case filename
*/
				if (rep==0)
					ul_to_lower(name);
				else if (rep==1)
					ul_to_upper(name);
				else
				{
					sprintf(fnamea,"Could not find Drawing View File '%s'.",tname);
					if (type>2)
					{
						if (eflag)
							ud_winerror(fnamea);
						status = UU_FAILURE;
						goto done;
					}
					else
					{
						type++;
						strcpy(name,sname);
						rep = 0;
						goto repeat;
					}
				}
				rep++;
				goto repeat;
			}
		}
	}
	else
	{
		mode = UX_EXISTS|UX_READ;
		status = ux_mk_chk_syspath(UU_NULL,UU_NULL,filename,UU_NULL,
					UU_NULL,&mode,fullname,UX_NPRTERRS);
		if (rep == 0)
			strcpy(tname, fullname);
		if (status != UU_SUCCESS || mode == (mode|UX_NEXISTS))
		{
/*
.....the drawing name could be upper/lower case
.....try the lower case filename
*/
			if (rep==0)
				ul_to_lower(name);
			else if (rep==1)
				ul_to_upper(name);
			else
			{
				sprintf(fnamea,"Could not find Drawing/Picture File '%s'.",tname);			
				if (type>2)
				{
					if (eflag)
						ud_winerror(fnamea);
					status = UU_FAILURE;
					goto done;
				}
				else
				{
					type++;
					rep = 0;
					strcpy(name,sname);
					goto repeat;
				}
			}
			rep++;
			goto repeat;
		}
	}
	file_find = 1;
	ul_remove_quotes(fullname);
	status = um_pocket_window(title, &fullname,UM_GRAPHIC_WINDOW);
done:;
	if (status!=UU_SUCCESS)
	{
		if (rep==0)
			ul_to_lower(name);
		else if (rep==1)
			ul_to_upper(name);
		else
		{
			sprintf(fnamea, "Error loading drawing file %s!", tname);
			if (file_find==0)
			{
				if (eflag)
					ud_winerror(fnamea);
			}
			if (type>2)
			{
				status = UU_FAILURE;
				return status;
			}
			else
			{
				type++;
				rep = 0;
				strcpy(name,sname);
				goto repeat;
			}
		}
		rep++;
		goto repeat;
	}
	return(status);
#endif
}
/*********************************************************************
**    E_FUNCTION     : um_load_pocket_drawing2(title,dname,fname,sysdir,flag)
**			Displays the specified drawing in a Pocket Graphics Window.
**			Will attempt to load the drawing if it is not found in the
**			current Unibase.
**    PARAMETERS   
**       INPUT  : 
**				title:	title of the drawing window
**				dname        Drawing name to view.  If a drawing file is
**				             loaded from this routine, then the drawing
**				             will be renamed to 'dname'.
**          fname        Filename to load if the drawing is not found.
**          sysdir       System directory for drawing, if not found in
**                       current directory.
**			flag:		1: display error message
**						0: don't display error message, just return failure
**							if errors.
**       OUTPUT :  
**          none
**    RETURNS      : UU_FAILURE on failure
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_load_pocket_drawing2(title, dname,fname,sysdir,flag)
char *title, *dname,*fname,*sysdir;
int flag;
{
	int i,status,mode,rep;
	UX_pathname filename,fnameu,fnamea,dir,tdir,fullname,tname, ext1;
	UU_KEY_ID key;
	char *p;
	struct UM_drawing_rec drawing;
	FILE *fptr = NULL;
/*
......break filename to base name and directory
*/
	ul_break_fname(fname,dir,filename);
/*
.....Remove preceding and trailing spaces
*/
#if UU_COMP!=UU_WIN2K
	i = strlen(filename);
	ul_strip_blanks(filename,&i);
#else
	for (i=0; i<strlen(filename); i++)
	{
		if (filename[i]!=' ') break;
	}
	strcpy(filename,&filename[i]);
	for (i=strlen(filename); i>0; i--)
	{
		if (filename[i-1]==' ')
			filename[i-1] = '\0';
		else
			break;
	}
#endif
/*
.....See if drawing is already loaded
*/
	if (um_key_from_drawing_name(dname,&key) == UU_SUCCESS)
	{
		drawing.key = key;
	}
/*
.....Verify the drawing file exists
.....Try both the local directory and
.....the provided system directory
.....See if the drawing is already loaded first
*/
	else
	{
		rep = 0;
		strcpy(tname,filename);
		do
		{
/*
........Try local directory first
*/
			strcpy(tdir,dir);
			if (tdir[0]=='\0') ul_get_full_dir(".",tdir);
			mode = UX_EXISTS|UX_READ;
			status = ux_mk_chk_syspath(UU_NULL,tdir,filename,UU_NULL,
				"UM_DRAWING_SUFFIX",&mode,fullname,UX_NPRTERRS);
			if (status == UU_SUCCESS && mode != (mode|UX_NEXISTS)) break;
/*
........Then system directory
*/
			mode = UX_EXISTS|UX_READ;
			tdir[0] = '\0';
			strcpy(tdir,sysdir);
			ul_remove_quotes(tdir);
/*
.....we allow multi-diectory define, so check for file, not just directory
.....
			ul_get_full_dir(tdir,fnamea);
			ux_search_for_path(fnamea,tdir,UX_PRTERRS|UX_NCHK|UX_NQUOTES);
			status = ux_mk_chk_syspath(UU_NULL,tdir,filename,UU_NULL,
				"UM_DRAWING_SUFFIX",&mode,fullname,UX_NPRTERRS);
			if (status == UU_SUCCESS && mode != (mode|UX_NEXISTS)) 
				break;
*/
			status = ul_open_mod_file2(tdir, NULL, filename, 0,  &fptr, UU_NULL, "UM_DRAWING_SUFFIX");
			if (status == UU_SUCCESS)
				break;
/*
........Try upper/lower case filename
*/
			if (rep==0) ul_to_lower(filename);
			else if (rep==1) ul_to_upper(filename);
			rep++;
		} while (rep < 2);
/*
.....Could not find drawing file
*/
		if (rep == 2)
		{
			if (flag == 1)
			{
				sprintf(fnamea,"Could not find Drawing View File '%s'.",tname);
				ud_winerror(fnamea);
			}
			status = UU_FAILURE;
			return(status);
		}
/*
.....Open the drawing file
*/
		ul_remove_quotes(fullname);
		status = um_open_archive_file(UX_READ, fullname, fnameu, fnamea);
		if (status != UU_SUCCESS) goto done;
/*
.....Retrieve the drawing
*/
		status = um_retrieve_drawing(dname,fnameu,fnamea,UU_FALSE);
		if (status!=UU_SUCCESS) goto done;
		if (um_key_from_drawing_name(dname,&drawing.key) == -1)
		{
			status = UU_FAILURE;
			goto done;
		}
	}
/*
.....Load the drawing information
*/
	status = um_get_all_geom(&drawing,sizeof(struct UM_drawing_rec));
	if (status != UU_SUCCESS) goto done;
/*
.....Open the Pocket Window
*/
	status = um_pocket_window(title, (int *)&drawing,UM_DRAWING_WINDOW);
done:;
	if (status!=UU_SUCCESS)
	{
		if (flag==1)
		{
			sprintf(fnamea, "Error loading drawing file %s!", tname);
			ud_winerror(fnamea);
		}
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : um_view_pocket_drawing(drawing)
**			Display the specified drawing in a Pocket Graphics Window.
**    PARAMETERS   
**       INPUT  : 
**          drawing						pointer to drawing entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_view_pocket_drawing(drawing)
struct UM_drawing_rec *drawing;
{
	int status;
	UV_view view,vsave;
	UU_REAL xlen, ylen;
	UU_REAL drw_aspect;
	UM_coord ref_pt;
	UU_REAL x, y, xm, ym;
	UU_KEY_ID drw_view,key;
	Gnrect *gr_area;
/*
.....Mark all objects on the new drawing displayable
*/
	key = ur_get_drwmdl_curdrw();
	if (drawing->key != key)
		um_fordrawing_set_disp(drawing->key, UM_DISPLAYABLE);
/*
.....Activate the single view
*/
	drw_view = ur_get_drwmdl_drwvw();
	uv_getvid(drw_view, &view);
	if (drawing->key == key) uu_move_byte(&view,&vsave,sizeof(UV_view));
/*
.....Set the view extents
.....to the current drawing size
*/
	um_get_drawing_extents(drawing->drwsize, &x, &y, &xm , &ym);
	gr_area = &(UD_duimsdeflt.screen[UD_curlayout.curr_screen].areas[UD_GRAF][0].posn);
	drw_aspect = (gr_area->ur.y - gr_area->ll.y) /
		(gr_area->ur.x - gr_area->ll.x);
/*	drw_aspect = ur_get_drwmdl_aspect();*/
	xlen = x;
	ylen = y/drw_aspect;
	if (ylen > xlen) xlen = ylen;
	xlen *= 1.01;
	um_xyztovc(x/2.0, y/2.0, (UU_REAL) 0.0, ref_pt);
	uv_setrefpt(&view, ref_pt);
	uv_setvaperture(&view, xlen);
	uv_putv(&view);
/*
.....Set the drawing screen &
.....Display the drawing
*/
	uv_chgsc("drw_screen");
	um_fordrawing_display(drawing);
/*
.....After updating Pocket Graphics Window
.....Reset viewing attributes
*/
	if (drawing->key != key)
	{
		um_fordrawing_set_disp(drawing->key, UM_UNDISPLAYABLE);
		um_fordrawing_delseg(drawing);
	}
	else
	{
		uv_putv(&vsave);
	}
	status = UU_SUCCESS;
	ud_updatews(UG_PERFORM);
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int um_set_pocket_graphics()
**       Enables Pocket Graphics.
**    PARAMETERS   
**       INPUT  : 
**          type            Type of pocket window to enable.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : 
**			All graphics displayed after calling this routine will be
**			output to the Pocket Graphics Window
**    WARNINGS     : none
*********************************************************************/
void um_set_pocket_graphics(type)
UM_pkwin_type type;
{
	if (type == UM_DRAWING_WINDOW)
	{
		UM_pocket_graphics = UU_TRUE;
		uw_glset_context(UM_DRAWING_WINDOW,UU_FALSE);
	}
	else if (type == UM_IPV_WINDOW)
	{
		uw_glset_context(UM_IPV_WINDOW,UU_FALSE);
		um_set_screen_area(UM_IPV_WINDOW);
	}
	uw_gllighting_reset();
}

/*********************************************************************
**    E_FUNCTION     : int um_get_pocket_window(type)
**       Returns the window handler for the Pocket Window type.
**    PARAMETERS   
**       INPUT  : 
**          type            Type of pocket window to get handle for.
**       OUTPUT :  
**          none
**    RETURNS      :
**          Handle of Pocket Window or UU_NULL if not active.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
char *um_get_pocket_window(type)
UM_pkwin_type type;
{
	char *win;
	if (type == UM_IPV_WINDOW)
	{
#if UU_COMP == UU_WIN2K
		uw_ntget_pockwin(&win);
#else
		uw_mfget_pockwin(&win);
#endif
	}
	else
		win = UU_NULL;
	return(win);
}

/*********************************************************************
**    E_FUNCTION     : int um_reset_pocket_graphics(type)
**       Disables Pocket Graphics for Drawing windows only.
**    PARAMETERS   
**       INPUT  : 
**          type            Type of pocket window to disable.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_reset_pocket_graphics(type)
UM_pkwin_type type;
{
	if (type == UM_DRAWING_WINDOW)
	{
		UM_pocket_graphics = UU_FALSE;
		uw_glset_context(UM_NCL_WINDOW,UU_FALSE);
	}
	else if (type == UM_IPV_WINDOW)
	{
		uw_glset_context(UM_NCL_WINDOW,UU_FALSE);
		um_set_screen_area(UM_NCL_WINDOW);
	}
	uw_gllighting_reset();
}

/*********************************************************************
**    E_FUNCTION     : int um_is_pocket_graphics()
**       Returns the Pocket Graphics active flag for the Drawing window.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : UU_TRUE if Pocket Graphics is enabled, otherwise
**		               UU_FALSE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_is_pocket_graphics()
{
	return(UM_pocket_graphics);
}

/*********************************************************************
**    E_FUNCTION     : int um_close_pocket_window(type)
**       Closes the Pocket Graphics Window.
**    PARAMETERS   
**       INPUT  : 
**          type            Type of pocket window to disable.
**       OUTPUT :  
**          none
**    RETURNS      : none if Pocket Graphics is enabled, otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_close_pocket_window(type)
UM_pkwin_type type;
{
	ud_close_pocket(type);
	uw_gllighting_reset();
}
/*********************************************************************
**    E_FUNCTION     : um_get_ncl_win()
**       Returns the window handler for the NCL graphics main Window.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      :
**          Handle of NCL graphics main Window or UU_NULL if not active.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
char *um_get_ncl_win()
{
	char *win;
#if UU_COMP == UU_WIN2K
	uw_ntget_mainwin(&win);
#else
	uw_mfget_mainwin(&win);
#endif
	return(win);
}

/*********************************************************************
**    E_FUNCTION     : um_minimize_pocket_window()
**       Minimizes an open pocket window.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_minimize_pocket_window()
{
#if UU_COMP == UU_WIN2K
	uw_ntmin_pockwin(UM_IPV_WINDOW);
#endif
	return;
}

/*********************************************************************
**    E_FUNCTION     : um_dwpocket_reset(type)
**       Reset window after drawing pocket window close
**    PARAMETERS   
**       INPUT  : 
**          type: window type
**       OUTPUT :  
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_dwpocket_reset(type)
int type;
{
	int status,i;
	UV_vport vport;
	if (type == UM_DRAWING_WINDOW)
	{
		ug_setredrwflag(0);
/*
.....Redraw all viewports (not contents though)
.....Because the segments were altered when the
.....screen changed
*/
		if (UW_pocket_mode)
		{
			for (i=0;i<UV_act_screen[0].nvports; i++)
			{
				status = uv_getvpid(UV_act_screen[0].vports[i],&vport);
				uv_drawvp(&vport);
			}
		}
	}
	UW_pocket_mode = UU_FALSE;
}

/*********************************************************************
**    E_FUNCTION     : um_set_screen_area(type)
**       Set the screen area for the window type
**    PARAMETERS   
**       INPUT  : 
**          type            Type of window to set
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : 
**			All graphics displayed after calling this routine will be
**			output to the Pocket Graphics Window
**    WARNINGS     : none
*********************************************************************/
void um_set_screen_area(type)
UM_pkwin_type type;
{
	int ix, iy, gx, gy;
/*
.....the 'gswswindow' window set function inside this function 
.....will cause window to repaint
.....when we doing playback, we don't want the repaint happened
.....when do playback
.....not call here when playback
*/
	if (NCL_animation)
		return;

	current_sarea = type;
#if UU_COMP == UU_WIN2K
	uw_ntget_gwsize(&gx,&gy);
	uw_ntget_ipvsize(&ix,&iy);
#else
	uw_mfget_gwsize(&gx,&gy);
	uw_mfget_ipvsize(&ix,&iy);
#endif
	if (type == UM_IPV_WINDOW)
	{
		if (UM_swap_ipv==0)
			udm_resize_graphics(&UD_duimsdeflt,ix, iy,0);
		else
			udm_resize_graphics(&UD_duimsdeflt,gx, gy,0);
	}
	else
	{
		if (UM_swap_ipv!=0)
			udm_resize_graphics(&UD_duimsdeflt,ix, iy,0);
		else
			udm_resize_graphics(&UD_duimsdeflt,gx, gy,0);
	}
	gswswindow(UD_ksws,&(UD_duimsdeflt.screen[0].wswind));
}

UM_pkwin_type um_get_screen_area()
{
	return current_sarea;
}

