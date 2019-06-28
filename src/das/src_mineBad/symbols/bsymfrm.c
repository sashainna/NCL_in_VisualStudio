/*********************************************************************
**    NAME:  bsymfrm.c
**       CONTAINS:
**             ub_savsym_setname
**             ub_savsym_saveall
**             ub_savsym_libname
**             ub_savsym_checklib
**             ub_browse_symfile
**             ub_browse_symlib
**             ub_browse_list
**             ubi_master_sym_form
**             ubi_sym_instance_form
**             ubi_text_node_form
**             ubu_instance_display_form
**             ubi_reset_instance_vis_form
**             ubi_symnam_form
**             ubi_savsym_form(formfile, sym, savsym)
**             ubi_rensym_form(formfile, sym1, sym2)
**             ubi_breaklib_sym(tempname, templib, bname)
**    COPYRIGHT 1984 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**        bsymfrm.c , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**        12/01/15 , 08:10:13
*********************************************************************/
#include "usysdef.h"
#include "uhep.h"       /* for error system */
#include "dmark.h"              /* for UD_MARK */
#include "udebug.h"
#include "xenv1.h"              /* for UX_PRTERRS */
#include "xfsys1.h"
#include "bsym.h"
#include "udforms.h"
#include "udfdata.h"

#define UB_RUN_TIME_DEFAULT 1
#define UB_NO_RUN_TIME_DEFAULT 0
#define TRACE UU_FALSE  /* for debugging only */
static UD_LIST Ssymbol_list;
static UU_LOGICAL Ssave = UU_FALSE, Smk_dir = UU_FALSE;
static UX_pathname Sfull_path,Sdef_path;
static char Snew_sym[UB_MAX_PATH_LEN] = "";
static char Tnew_sym[UB_MAX_PATH_LEN] = "";
static char Snew_lib[UB_MAX_PATH_LEN] = "symlib_S";
static char Tnew_lib[UB_MAX_PATH_LEN] = "symlib_S";
static UD_LIST Ssymname_list;
void ubi_breaklib_sym();
/*********************************************************************
**    S_FUNCTION     :  ub_savsym_setname(fieldno, val, stat)
**       Method called at list field.  The text in the Save As field
**       will be updated with the symbol name.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added symbol name setting for "Save As" field
.....ASF 7/31/13
*/
UD_FSTAT ub_savsym_setname(fieldno, val, stat)
int *fieldno;
UD_FSTAT stat;
UD_DDATA *val;
{
	if (*fieldno == 0)
	{
		Ssave = UU_TRUE;
		ud_update_answer(5, (int *)val->frmstr);
		strcpy(Tnew_sym,Snew_sym);
	}
	else if (strcmp(Tnew_sym,Snew_sym) != 0)
	{
		Ssave = UU_TRUE;
		strcpy(Tnew_sym,Snew_sym);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  ub_savsym_defpath(fieldno, val, stat)
**       Sets the default path based on the current selection.  The
**       path will either be the system or local directory.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UD_FSTAT ub_savsym_defpath()
{
	int hdrfound;
	UX_pathname tfullname;
	char *list = UU_NULL;
/*
	if (lib_direct == 1)
	{
		ux_get_syspath(UB_libdata_rec.sys_farea,&list,Sdef_path,&hdrfound, 
								UX_NPRTERRS);
	}
	else
	{
		ux_get_syspath(UB_libdata_rec.loc_farea,&list,Sdef_path,&hdrfound, 
								UX_NPRTERRS);
	}
	uu_lsdel(list);
	ul_remove_quotes(Sdef_path);
*/
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  ub_savsym_saveall()
**       Method called for Save All button.  All symbols in the master
**       list will be saved to the current library using the label
**       associated with the master symbol.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : A new directory may be created.
**    WARNINGS     : none
*********************************************************************/
UD_FSTAT ub_savsym_saveall()
{
	int i,max,mode,status;
	char fullname[UB_MAX_PATH_LEN] = "";
	char fext[] = "";
	char sym_name[11] = " ";
	char tbuf[UB_MAX_PATH_LEN+46];
	sprintf(tbuf,"Are you sure you want to save all symbols to %s?",Snew_lib);
	if (ud_yesno2(0,tbuf,"Save All"))
	{
		if (Smk_dir)
		{
			mode= 0755;
			if (ux_mk_dir(Sfull_path, mode, UX_PRTERRS) != UU_SUCCESS)
					goto done;
			Smk_dir = UU_FALSE;
		}
		Ssave = UU_FALSE;
		max = Ssymname_list.num_item;
		for (i=0;i<max;i++)
		{
			strcpy(sym_name,Ssymname_list.item[i]);
			ul_build_full_fname(Sfull_path,sym_name,fext,fullname);
			status = ubi_archive_symmaster(sym_name,fullname);
			if (status != UU_SUCCESS) break;
		}
	}
done:
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  ub_savsym_libname(fieldno, val, stat)
**       Method called at Library edit field.  The text in the Library
**       field will be updated with the new name.  The user will be
**       prompted to create the library if it does not already exist.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form display mask.
**    WARNINGS     : none
*********************************************************************/
UD_FSTAT ub_savsym_libname(fieldno, val, stat)
int *fieldno;
UD_FSTAT stat;
UD_DDATA *val;
{
	int len;
	UU_LOGICAL found;
	UX_pathname tpath;
	char tbuf[UB_MAX_PATH_LEN+47];
	if (strcmp(val->frmstr,Tnew_lib) == 0 || val->frmstr[0] == '\0')
	{
		if (val->frmstr[0] == '\0')
		{
			Tnew_lib[0] = '\0';
			Smk_dir = UU_FALSE;
			Ssave = UU_TRUE;
			strcpy(Sfull_path,Sdef_path);
		}
		return(UD_FLDOK);
	}
	len = strlen(val->frmstr);
	found = ub_savsym_checklib(val->frmstr,tpath,len);
	sprintf(tbuf,"Library %s not found. Do you wish to create it?",val->frmstr);
	if (found || ud_yesno2(0,tbuf,"Create Library?"))
	{
		if (!found) Smk_dir = UU_TRUE;
		else Smk_dir = UU_FALSE;
		Ssave = UU_TRUE;
		strcpy(Sfull_path,tpath);
		strcpy(Tnew_lib,val->frmstr);
	}
	else
	{
		Ssave = UU_FALSE;
		Smk_dir = UU_FALSE;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  ubi_compare_defaultpath(path,lib)
**       Check if path matches the local or system directory.
**    PARAMETERS
**       INPUT  :
**          libname  - path to search for.
**          len      - directory flag: 1 = System
**                                     0 = Local
**       OUTPUT :
**          none
**    RETURNS      : UU_TRUE iff path matches local/system directory.
**    SIDE EFFECTS : Changes the form display mask.
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ubi_compare_defaultpath(path,lib)
char *path;
int lib;
{
	int hdrfound;
	UX_pathname tfullname;
	char *list = UU_NULL;
	if (lib == 1)
	{
		ux_get_syspath(UB_libdata_rec.sys_farea,&list,tfullname,&hdrfound, 
								UX_NPRTERRS);
	}
	else
	{
		ux_get_syspath(UB_libdata_rec.loc_farea,&list,tfullname,&hdrfound, 
								UX_NPRTERRS);
	}
	uu_lsdel(list);
	ul_remove_quotes(tfullname);
	if (ul_compare_upper(tfullname,path) == 0)
		return(UU_TRUE);
	return (UU_FALSE);
}

/*********************************************************************
**    S_FUNCTION     :  ub_savsym_checklib(libname,fullname,len)
**       Method to check if a library already exists in the current
**       parent directory.
**    PARAMETERS
**       INPUT  :
**          libname  - name of library to search for.
**          len      - Length of input library name.
**       OUTPUT :
**          fullname - full path for library
**    RETURNS      : UU_TRUE iff library already exists.
**    SIDE EFFECTS : Changes the form display mask.
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ub_savsym_checklib(libname,fullname,len)
char *libname,*fullname;
int len;
{
	int hdrfound,mode,numint,status;
	UX_pathname dir, fname, tfullname, path_prefix;
	char tlib[UB_MAX_PATH_LEN] = "";
	char *list = UU_NULL;
/*
.....Check if library is the local or system directory.
*/
	if (ubi_compare_defaultpath(libname,0))
	{
		strcpy(fullname,libname);
		return (UU_TRUE);
	}
	if (ubi_compare_defaultpath(libname,1))
	{
		strcpy(fullname,libname);
		return (UU_TRUE);
	}
/*
.....Search for library.
*/
	strcpy(tlib,libname);
	if (tlib[len-1]=='/')
	{
		tlib[len-1] = '\0';
		len = len -1;
	}
	if (len>=2)
	{
		if ((tlib[len-2]=='_')&&(tlib[len-1]=='S'))
		{
			tlib[len-2] = '\0';
		}
	}
	mode = UX_READ | UX_WRITE; 
	ul_break_fname(tlib, dir, fname);
	if (strcmp(UB_libdata_rec.default_area, "local") == 0)
		strcpy(path_prefix, UB_libdata_rec.loc_farea);
	else
		strcpy(path_prefix, UB_libdata_rec.sys_farea);
	if (dir[0]!='\0')
	{
		status = ux_file_inquire(UU_NULL, tlib, UU_NULL, UB_libdata_rec.fenv,
						UB_libdata_rec.ftype, &mode, &hdrfound, tfullname,
						UX_PRTERRS);                                            
	}
	else
	{
		status = ux_file_inquire(path_prefix, tlib, UU_NULL, 
						UB_libdata_rec.fenv, UB_libdata_rec.ftype, &mode, 
						&hdrfound, tfullname, UX_PRTERRS);                                               
	}
	if (status != UU_SUCCESS)
	{
		uu_uerror2(UX_UDOS, 27, UB_libdata_rec.pstring, "uxu_create_lib");
		/* error is: Error in path name to %s library.  (%s). */
		goto done;
	}
	else
		strcpy(fullname,tfullname);
	if (mode != (mode | UX_NEXISTS))  /* then the library exists */
	{
		return (UU_TRUE);
	}
done:
	return (UU_FALSE);
}
/*********************************************************************
**    S_FUNCTION     :  ub_browse_csymfile(fieldno, val, stat)
**       Method called at Symbol browser toggle field.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form display mask.
**    WARNINGS     : none
*********************************************************************/
UD_FSTAT ub_browse_csymfile(fieldno, val, stat)
int *fieldno;
UD_FSTAT stat;
UD_DDATA *val;
{
	UX_pathname symname, symdir, tempstr, dir, fname;
	char paths[UX_MAX_PATH_LEN*20], path_des[UX_MAX_PATH_LEN*20];
	int len;
	UD_DDATA data;
/*
.....Get the material name
*/
	data.frmstr = tempstr;
	ud_get_field(*fieldno-1,data,UU_FALSE);
	strcpy(symdir, tempstr);

	ud_get_field(*fieldno+1,data,UU_FALSE);
	strcpy(symname, tempstr);
	ul_break_fname(symname, dir, fname);
	if (dir[0]=='\0')
	{
		strcpy(dir, symdir);
		len = strlen(dir);
		if ((len>=2)&&(dir[len-2]=='_')&&(dir[len-1]=='S'))
		{
			dir[len-2] = '\0';
			len -= 2;
		}
		if (len!=0)
			strcat(dir, "_S");
		ul_build_full_fname(dir, symname,"sy",symname);
		len = strlen(symname);
	}
	sprintf(paths, "%s;%s", UB_libdata_rec.loc_farea, UB_libdata_rec.sys_farea);
	strcpy(path_des, "Local;System");
	ud_get_filename1("Symbol File", "Symbol File", "*.sy", symname,&len, "Symbol Files (*.sy)", 0, UU_FALSE, paths, path_des);
	if (len>0)
	{
		ul_break_fname(symname, dir, fname);
		len = strlen(dir);
		if ((len>=2)&&(dir[len-2]=='_')&&(dir[len-1]=='S'))
		{
			dir[len-2] = '\0';
			len -= 2;
		}
		ud_update_answer(*fieldno-1, (int *)dir);
		ud_update_answer(*fieldno+1, (int *)fname);
	}
	else
		*fieldno = -1;
	return(UD_FLDOK);
}
/*********************************************************************
**    S_FUNCTION     :  ub_browse_symfile(fieldno, val, stat)
**       Method called at Symbol browser toggle field.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form display mask.
**    WARNINGS     : none
*********************************************************************/
UD_FSTAT ub_browse_symfile(fieldno, val, stat)
int *fieldno;
UD_FSTAT stat;
UD_DDATA *val;
{
	UX_pathname symname, symdir, tempstr, dir, fname;
	char paths[UX_MAX_PATH_LEN*20], path_des[UX_MAX_PATH_LEN*20];
	int len;
	UD_DDATA data;
/*
.....Get the material name
*/
	data.frmstr = tempstr;
	ud_get_field(*fieldno+1,data,UU_FALSE);
	strcpy(symname, tempstr);
	ul_break_fname(symname, dir, fname);
	if (dir[0]=='\0')
	{
		strcpy(dir, symdir);
		ul_build_full_fname(symdir, symname,"sy",symname);
	}
	sprintf(paths, "%s;%s", UB_libdata_rec.loc_farea, UB_libdata_rec.sys_farea);
	strcpy(path_des, "Local;System");
	if (*fieldno==0)
		ud_get_filename1("Symbol File", "Symbol File", "*.sy", symname,&len, "Symbol Files (*.sy)", 1, UU_FALSE, paths, path_des);
	else
		ud_get_filename1("Symbol File", "Symbol File", "*.sy", symname,&len, "Symbol Files (*.sy)", 0, UU_FALSE, paths, path_des);
	if (len>0)
	{
//		ul_break_fname(symname, dir, fname);
		ud_update_answer(*fieldno+1, (int *)symname);
	}
	else
		*fieldno = -1;
	return(UD_FLDOK);
}
/*********************************************************************
**    S_FUNCTION     :  ub_browse_symfile2(fieldno, val, stat)
**       Method called at file browser toggle field.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form display mask.
**    WARNINGS     : none
*********************************************************************/
UD_FSTAT ub_browse_symfile2(fieldno, val, stat)
int *fieldno;
UD_FSTAT stat;
UD_DDATA *val;
{
	UX_pathname symname, symdir, tempstr, dir, fname;
	char paths[UX_MAX_PATH_LEN*20], path_des[UX_MAX_PATH_LEN*20], *p;
	int len;
	UD_DDATA data;
/*
.....Get the material name
*/
	data.frmstr = tempstr;
	ud_get_field(*fieldno-1,data,UU_FALSE);
	strcpy(symdir, tempstr);

	ud_get_field(*fieldno+1,data,UU_FALSE);
	strcpy(symname, tempstr);
	ul_break_fname(symname, dir, fname);
	if (dir[0]=='\0')
	{
		strcpy(dir, symdir);
		len = strlen(dir);
		if ((len>=2)&&(dir[len-2]=='_')&&(dir[len-1]=='S'))
		{
			dir[len-2] = '\0';
			len -= 2;
		}
		if (len!=0)
			strcat(dir, "_S");
		ul_build_full_fname(dir, symname,"sy",symname);
		len = strlen(symname);
	}
	sprintf(paths, "%s;%s", UB_libdata_rec.loc_farea, UB_libdata_rec.sys_farea);
	strcpy(path_des, "Local;System");
	ud_get_filename1("Symbol File", "Symbol File", "*.sy", symname,&len, "Symbol Files (*.sy)", 0, UU_FALSE, paths, path_des);
	if (len>0)
	{
		ux_decompose_path(symname, dir, fname, UX_NQUOTES);
		if (fname[0]!='\0')
		{
			p = rindex(fname,'.');
			if (p != UU_NULL)
				*p = '\0';
			Ssave = UU_TRUE;
			len = strlen(dir);
			strcpy(Sfull_path,dir);
			if ((len>=2)&&(dir[len-2]=='_')&&(dir[len-1]=='S'))
			{
				dir[len-2] = '\0';
				len -= 2;
			}
			ud_update_answer(*fieldno-1, (int *)dir);
			ud_update_answer(*fieldno+1, (int *)fname);
			strcpy(Tnew_lib,dir);
			strcpy(Tnew_sym,fname);
		}
		else
			*fieldno = -1;
	}
	else
		*fieldno = -1;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  ub_browse_symlib(fieldno, val, stat)
**      Method called at library browser toggle field
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form display mask.
**    WARNINGS     : none
*********************************************************************/
UD_FSTAT ub_browse_symlib(fieldno, val, stat)
int *fieldno;
UD_FSTAT stat;
UD_DDATA *val;
{
	UX_pathname symlib, tempstr, dirname, dir, lname;
	char paths[UX_MAX_PATH_LEN*20], path_des[UX_MAX_PATH_LEN*20];
	int len;
	int hdrfound,mode,numint,status;
	UD_DDATA data;
/*
.....Get the material name
*/
	data.frmstr = tempstr;
	ud_get_field(*fieldno+1,data,UU_FALSE);
	strcpy(symlib, tempstr);

	len = strlen(symlib);
	ul_strip_blanks(symlib,&len);

	sprintf(paths, "%s;%s", UB_libdata_rec.loc_farea, UB_libdata_rec.sys_farea);
	strcpy(path_des, "Local;System");
	dirname[0] = '\0';
	if (symlib[0]!='\0')
	{
		strcpy(dirname, symlib);
		ul_break_fname(dirname, dir, lname);
		len = strlen(dirname);
		if ((len>=2)&&(dirname[len-2]=='_')&&(dirname[len-1]=='S'))
		{
			dirname[len-2] = '\0';
		}
		if (dir[0]=='\0')
		{
			status = ux_file_inquire(UB_libdata_rec.loc_farea, dirname, UU_NULL, 
						UB_libdata_rec.fenv, UB_libdata_rec.ftype, &mode, 
						&hdrfound, dirname, UX_PRTERRS);  
			ul_remove_quotes(dirname);
			len = strlen(dirname);
		}
		else
		{
			strcat(dirname, "_S");
			len = strlen(dirname);
		}
	}
	ud_get_dirname1("Symbol Library", "Symbol Library", dirname, &len, paths, path_des);
	if ((len>=2)&&(dirname[len-2]=='_')&&(dirname[len-1]=='S'))
	{
		dirname[len-2] = '\0';
		len -= 2;
	}
	if (len>0)
	{
		ud_update_answer(*fieldno+1, (int *)dirname);
	}
	else
		*fieldno = -1;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  ub_browse_list(fieldno, val, stat)
**       Loads a symbol file via a browser and adds it to the symbol list.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form display mask.
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT ub_browse_list(fieldno, val, stat)
int *fieldno;
UD_FSTAT stat;
UD_DDATA *val;
{
	char path[256];
	char **ub_get_symmaster_name();
/*
.....Load new symbol
*/
	if (ubu_load_symmaster(Ssymbol_list.answer, path) == UU_SUCCESS)
	{
/*
.....we need update the symlist field
*/
		uu_free(Ssymbol_list.item);
		Ssymbol_list.item = (char **)
				ub_get_symmaster_name(&(Ssymbol_list.num_item));
		ud_update_answer(1, (int *)&Ssymbol_list);
	}
/*
.....canceled
*/
	else
		*fieldno = -1;
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION : int ubi_master_sym_form(formfile, master_frm_outptr)
**       Display master symbol form and get results.
**    PARAMETERS   
**       INPUT  : 
**                      formfile                                                character string giving the name of form 
**                                                                                      text file.
**                      master_frm_outptr                       pointer to the data structure for returning 
**                                                                                      form responses; note, some fields already 
**                                                                                      have values.
**       OUTPUT :  
**          master_frm_outptr           pointer to the filled-in data structure for
**                                                                                              for returning symbol instance form 
**                                                                                              responses.
**    RETURNS: UU_SUCCESS if no errors detected, otherwise UB_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS: Assumes defaults are already specified.
*********************************************************************/
int ubi_master_sym_form(formfile, master_frm_outptr)
	char *formfile;
	struct UB_master_sym_frm_rec *master_frm_outptr;
{
	static char tempname[UB_SYMBOL_NAME_LEN_PLUS] = "";
	static char templib[UB_MAX_PATH_LEN] = "symlib";
	static int tmpcmd=0;
	char bname[UB_SYMBOL_NAME_LEN_PLUS], dirname[UB_MAX_PATH_LEN];
	static UU_REAL temporigin[3];
/*
.....added browse button
.....Yurong 
*/
	static int option = 0;
	static int option2 = 0;
	UD_FSTAT uj_noop();
	static int *ans[] = { (int *)&option, (int *)templib, 
							(int *)&option2, (int *)tempname, 
									(int *)temporigin, (int *)&tmpcmd};
	static UD_METHOD methods[6] = {ub_browse_symlib, uj_noop, 
			ub_browse_csymfile, uj_noop, uj_noop, uj_noop};
	static char called[] = {6,6,6,6,6,6};
	int len, status;

	uu_denter(UU_BTRC, (us,"ubi_master_sym_form(%s,%x)",
					formfile,master_frm_outptr));

	strcpy(tempname, master_frm_outptr->name);
	um_vctovc(master_frm_outptr->origin, temporigin);

	status = ud_form1(formfile, ans, ans, methods, called, NULL, NULL);
	if (status == -1) goto failed;
	len = strlen(tempname);
	ul_strip_blanks(tempname,&len);
	ul_break_fname(tempname, dirname, bname);
	len = strlen(dirname);
	if ((len>=2)&&(dirname[len-2]=='_')&&(dirname[len-1]=='S'))
	{
		dirname[len-2] = '\0';
		len -= 2;
	}
	if (len>0)
		strcpy(master_frm_outptr->lib, dirname);
	else
		strcpy(master_frm_outptr->lib, templib);
	strcpy(master_frm_outptr->name, bname);
	master_frm_outptr->cmd = tmpcmd;
	um_vctovc(temporigin, master_frm_outptr->origin);
	uu_dexit;
	return(UU_SUCCESS);
failed: return(UB_FAILURE);
}

/*********************************************************************
**    I_FUNCTION : int ubi_sym_instance_form(formfile, instance_frm_outptr)
**       Display symbol instance form and get results.
**    PARAMETERS   
**       INPUT  : 
**          formfile             character string giving the name of form text
**                               file.
**          instance_frm_outptr  pointer to the data structure for returning 
**          form responses; note, some fields already have values.
**       OUTPUT :  
**          instance_frm_outptr  pointer to the filled-in data structure for
**                               responses.
**    RETURNS: UU_SUCCESS if no errors detected, otherwise UB_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS: Assumes defaults are already specified.
*********************************************************************/
int ubi_sym_instance_form(formfile, instance_frm_outptr)
char *formfile;
struct UB_sym_instance_frm_rec *instance_frm_outptr;
{
	UU_REAL tempscale;
	UU_REAL tempangle;
	int tempcmd;
	UU_LOGICAL cmdreject;
	char **ub_get_symmaster_name();
	int option  = 0;
	int *ans[5];
	UD_FSTAT uj_noop();
	static UD_METHOD methods[5] = {
			ub_browse_list, uj_noop, uj_noop, uj_noop, uj_noop};
	static char called[] = {6,6,6,6,6};
	int status;
/*
.....Initialize routine
*/
	status = UU_SUCCESS; /* assume success */
	UD_MARK(cmdreject, UU_FALSE);
	if (cmdreject) goto done;
/*
.....Get list of defined symbols
*/
	Ssymbol_list.answer =
		(char *)uu_malloc(UB_SYMBOL_NAME_LEN_PLUS * sizeof(char));
	strcpy(Ssymbol_list.answer, instance_frm_outptr->name);
	Ssymbol_list.item = (char **)
		ub_get_symmaster_name(&(Ssymbol_list.num_item));
/*
.....Setup defaults
*/
   ans[0] = (int *)&option;
   ans[1] = (int *)&Ssymbol_list;
   ans[2] = (int *)&tempscale;
	ans[3] = (int *)&tempangle;
	ans[4] = (int *)&tempcmd;
	if (Ssymbol_list.num_item==0)
	{
		Ssymbol_list.num_item = 1;
		Ssymbol_list.item = (char **)uu_malloc(sizeof(char *));
		Ssymbol_list.item[0] =
			(char *)uu_malloc(UB_SYMBOL_NAME_LEN_PLUS*sizeof(char *));
		strcpy(Ssymbol_list.item[0], "    ");
	}
   strcpy(Ssymbol_list.answer, instance_frm_outptr->name);
   tempscale = instance_frm_outptr->scale;
   tempangle = instance_frm_outptr->angle;
	tempcmd = instance_frm_outptr->cmd;
/*
.....Display the form
*/
	status = ud_form1(formfile, ans, ans, methods, called, NULL, NULL);
	if (status == -1) goto failed;
/*
.....Store the form results
.......Setting default library location to null causes issues when trying
.......to create a new symbol after one was placed. No library can be found
*/
//	strcpy(instance_frm_outptr->lib, "");
	strcpy(instance_frm_outptr->name, Ssymbol_list.answer);
	instance_frm_outptr->scale = tempscale;
	instance_frm_outptr->angle = tempangle;
	instance_frm_outptr->cmd = tempcmd;
/*      strcpy(instance_frm_outptr->area,(lib_direct==0) ? "local" : "system");*/
	goto done;
/*
.....Failure
*/
failed:
	status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
/*
.....End of routine
*/
done:;
	ud_free_flist(&Ssymbol_list);
	UD_UNMARK(cmdreject);
	return(status);
}

/*********************************************************************
**    I_FUNCTION : int ubi_text_node_form(formfile, tnode_frm_outptr)
**       Display text node form and get results.
**    PARAMETERS   
**       INPUT  : 
**          formfile            name of file containing the text node form.
**       OUTPUT :  
**          tnode_frm_outptr            pointer to the record containing the
**                                                                              form data given by the user.
**    RETURNS      : UU_SUCCESS if no errors detected, otherwise
**                                               UB_FAILURE is returned.
**    SIDE EFFECTS : none
**    WARNINGS: Assumes defaults are already specified.
*********************************************************************/
int ubi_text_node_form(formfile, tnode_frm_outptr)
	char *formfile;
	struct UB_text_node_frm_rec *tnode_frm_outptr;
{
	static int temptype[20];
	static int tempvisibility[12];
	static int tempcolor[12];
	static UU_REAL tempangle;
	static int *ans[] = {temptype,tempvisibility,tempcolor, (int *)&tempangle};
	int status;

	uu_denter(UU_BTRC, (us,"ubi_text_node_form(%s,%x)", formfile, 
					tnode_frm_outptr));

	temptype[0] = tnode_frm_outptr->type;
	tempvisibility[0] = tnode_frm_outptr->visibility; 
/* NCL */
	tempcolor[0] = tnode_frm_outptr->color;
/* end NCL */
	tempangle = tnode_frm_outptr->angle;

	/* put up the form: */
	status = ud_form(formfile, ans, ans);
	if (status == -1) return UB_FAILURE;

	tnode_frm_outptr->type = temptype[0];
	tnode_frm_outptr->visibility = tempvisibility[0]; 
/* NCL */
	tnode_frm_outptr->color = tempcolor[0];
/* end NCL */
	tnode_frm_outptr->angle = tempangle;

	uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION : int ubu_instance_display_form(formfile)
**       This function sets the display characteristics for instances.
**    PARAMETERS   
**       INPUT  : 
**          formfile            name of file containing the text node form.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no errors detected, otherwise UB_FAILURE 
**                                      is returned.
**    SIDE EFFECTS : none
**    WARNINGS: Assumes defaults are already specified.
*********************************************************************/
int ubu_instance_display_form(formfile)
	char *formfile;
{
	/* NOTE: THE FOLLOWING STATIC VAR'S ARE NEEDED SINCE THEIR GLOBAL INCLUDE
	 * FILE COUNTER PARTS HAVE THE CORRESPONDING VALUES THE REST OF THE SYSTEM 
	 * NEEDS WHEREAS THESE VARIABLES ARE FOR THE ANALOGOUS VALUE IN THE FORM. 
	 * THUS, THE INITIAL VALUES HERE MUST CORROLATE WITH THOSE IN BSYM.H */
	static int snapnodetype = {3};  /* this is a circle */
	static int snapnodecolor = {1}; /* hopefully 1 is white; 8 corresponds to dark red in form */
	static int snapnodevisibility = {0}; /* 0 corresponds to visible */
	static int textnodevisibility = {0}; /* 0 corresponds to "all visible" */
	static int textnodeattr = {0};/* 0 corresponds to "inherited from master";
											 * 1 corresponds to "current attributes"*/
	static int textdefault = {1};   /* 1 corresponds to YES */
	static int temp_reload[8];
	static int temp_snodetype[4];
	static int temp_snodecolor[13];
	static int temp_snodevis[10];
	static int temp_tnodevis[10];
	static int temp_tnodeattr[22];
	static int temp_textdefault[4];
	UU_LOGICAL reload;
	char val[8]; /* need room for "TRUE"/"FALSE" */
	int status = UU_SUCCESS;                        /* return status */
	static int *ans[] = {temp_reload,temp_snodetype,temp_snodecolor,
		temp_snodevis,temp_tnodevis,temp_tnodeattr,temp_textdefault};

	uu_denter(UU_BTRC, (us,"ubu_instance_display_form(%s)", formfile));

	if (ubi_reloadMasters(&reload) != UU_SUCCESS) goto failed;
	temp_reload[0] = (reload) ? 1 : 0; /* 1 for true, 0 for false */
	temp_snodetype[0] = snapnodetype;
/* NCL */
	temp_snodecolor[0] = snapnodecolor;
/* end NCL */
	temp_snodevis[0] = snapnodevisibility;
	temp_tnodevis[0] = textnodevisibility;
	temp_tnodeattr[0] = textnodeattr;
	temp_textdefault[0] = textdefault;

	status = ud_form(formfile, ans, ans);
	if (status == -1) return (UB_FAILURE);

	reload = (temp_reload[0] == 0) ? UU_FALSE : UU_TRUE;
	if (reload)
		strcpy(val, "TRUE");
	else strcpy(val, "FALSE");
	if (ux_modenv("replace", "UB_UPDATE_LOAD_SYMS", val, UX_PRTERRS)
			!= UU_SUCCESS) goto failed;
	UB_snap_node_marker_type = temp_snodetype[0] + 1; 
	/* form output starts at 0, marker types start at 1 */

/* NCL -
	if (ubi_get_form_color(temp_snodecolor[0] + 1, &UB_snap_node_color)
		!= UU_SUCCESS) goto failed;
*/
	UB_snap_node_color = temp_snodecolor[0];
/* end NCL */

	UB_snap_nodes_visible = (temp_snodevis[0] == 0) ? UU_TRUE : UU_FALSE;

	switch(temp_tnodevis[0])
	{
		case 0/* all visible */:
			UB_text_node_visibility = UB_ALL_TEXT_NODES_VISIBLE;
			break;  
		case 1/* only graphic visible */:
			UB_text_node_visibility = UB_ONLY_GRAPHIC_TEXT_NODES;
			break;
		case 2/* none visible */:
			UB_text_node_visibility = UB_NO_TEXT_NODES_VISIBLE;
			break;
		default:
			uu_uerror2(UB_SYMBOL, 57, temp_tnodevis[0], 
				"ubu_instance_display_form");
			/* error is: Illegal text node visibility specification 
			 *      of %d,  (%s) */
			goto failed;
	}
	strcpy(UB_text_node_attr, (temp_tnodeattr[0]==0) ? "inherited" : "current");
	uu_dprint(UU_BTRC, (us," Now UB_TEXT_NODE_ATTR  is %s", UB_text_node_attr));
	UB_text_default = temp_textdefault[0];
			
	snapnodetype = temp_snodetype[0];
/* NCL */
	snapnodecolor = temp_snodecolor[0];
/* end NCL */
	snapnodevisibility = temp_snodevis[0];
	textnodevisibility = temp_tnodevis[0];
	textnodeattr = temp_tnodeattr[0];
	textdefault = temp_textdefault[0];

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION : int ubu_reset_instance_vis_form(formfile, 
**                                                                      snap_nodes_visibleptr, text_node_visibilityptr)
**       This function sets the visibility characteristics of instances.
**    PARAMETERS   
**       INPUT  : 
**          formfile            name of file containing the text node form.
**       OUTPUT :  none.
**                              snap_nodes_visibleptr   Pointer to UU_TRUE iff the snap nodes
**                                                                                              are to be visible.
**                              text_node_visibilityptr Pointer to the visibility characteristics
**                                                                                              of text nodes; one of the following values
**                                                                                              is output: UB_ALL_TEXT_NODE_VISIBLE,
**                                                                                              UB_ONLY_GRAPHIC_TEXT_NODES, 
**                                                                                              UB_NO_TEXT_NODES.
**    RETURNS: UU_SUCCESS if no errors detected, otherwise UB_FAILURE 
**                                      is returned.
**    SIDE EFFECTS : none
**    WARNINGS: Assumes defaults are already specified.
*********************************************************************/
int ubu_reset_instance_vis_form(formfile,snap_nodes_visibleptr,
						text_node_visibilityptr)
	char *formfile;
	int *snap_nodes_visibleptr;
	int *text_node_visibilityptr;
{
	static int snapnodevisibility = {0}; /* 0 corresponds to visible */
	static int textnodevisibility = {0}; /* 0 corresponds to "all visible" */
	static int *ans[] = { &snapnodevisibility, &textnodevisibility };
	int cmdreject, status = UU_SUCCESS;       

	uu_denter(UU_BTRC, (us,"ubu_reset_instance_vis_form(%s,?,?)", formfile));
	UD_MARK(cmdreject, UU_TRUE);
	if (cmdreject) goto failed;
	/* else no command reject */
	status = ud_form("binstvis.frm", ans, ans);
	if (status == -1) goto failed;
	*snap_nodes_visibleptr = (snapnodevisibility == 0) ? UU_TRUE : UU_FALSE;

	switch(textnodevisibility)
	{
		case 0/* all visible */:
			*text_node_visibilityptr = UB_ALL_TEXT_NODES_VISIBLE;
			break;  
		case 1/* only graphic visible */:
			*text_node_visibilityptr = UB_ONLY_GRAPHIC_TEXT_NODES;
			break;
		case 2/* none visible */:
			*text_node_visibilityptr = UB_NO_TEXT_NODES_VISIBLE;
			break;
		default:
			uu_uerror2(UB_SYMBOL, 57, textnodevisibility, 
				"ubu_reset_instance_vus_form");
			/* error is: Illegal text node visibility specification of %d,  (%s) */
			goto failed;
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	UD_UNMARK(cmdreject);
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION : int ubi_symnam_form(formfile, symnam_frm_outptr,libdataptr)
**
**       Display browser to get symbol name, directory, symlib.
**    PARAMETERS   
**       INPUT  : 
**                      banner                                                  character string giving the name of browser 
**                                                                                      banner.
**                      symnam_frm_outptr                       pointer to the data structure for returning 
**                                                                                      form responses; note, some fields already 
**                                                                                      have values.
**       OUTPUT :  
**          symnam_frm_outptr           pointer to the filled-in data structure for
**                                                                                              for returning symbol instance form 
**                                                                                              responses.
**    RETURNS: UU_SUCCESS if no errors detected, otherwise UB_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS: Assumes defaults are already specified.
*********************************************************************/
int ubi_symnam_form(banner, symnam_frm_outptr, libdataptr)
char *banner;
struct UB_symnam_frm_rec *symnam_frm_outptr;
UX_libdata_bag *libdataptr;
{
	char paths[UX_MAX_PATH_LEN*20], path_des[UX_MAX_PATH_LEN*20];
	char templib[UB_MAX_PATH_LEN],bname[UB_SYMBOL_NAME_LEN_PLUS];
	char fname[UB_MAX_PATH_LEN], descrip[UB_MAX_PATH_LEN];
	char *p,ext[UX_SUFFIX_LEN],ext1[UX_SUFFIX_LEN],*ux_getenv();
	int nc,status;
/*
.....Get symbol filename
*/
	fname[0] = '\0'; nc = 0;

	strcpy(paths, "UB_SYS_M_SYMDIR;UB_LOC_M_SYMDIR");
	strcpy(path_des, "System;Local");

	strcpy(descrip, "Symbol Files (");
	strcpy(ext,"*.");
	p = ux_getenv("UB_SYM_SUFFIX");
	if (p != UU_NULL)
	{
		strcpy(ext1,p);
		ul_remove_quotes(ext1);
		strcat(ext,ext1);
	}
	else strcat(ext,"sy");
	strcat(descrip, ext);
	strcat(descrip, ")");

	ud_get_filename1(NULL, banner, ext, fname,&nc, descrip, 1, UU_FALSE, paths, path_des);
/*
.....Break out the directory and filename
*/
	if (strlen(fname) == 0) status = UB_FAILURE;
	else
	{
		status = UU_SUCCESS;
		ubi_breaklib_sym(fname, templib, bname);
		strcpy(symnam_frm_outptr->name, bname);
		strcpy(symnam_frm_outptr->lib, templib);
	}
	return(status);
}

/*********************************************************************
**    I_FUNCTION : int ubi_savsym_form(formfile,sym, savsym)
**       Display form to get symbol name, save symbol name
**    PARAMETERS   
**       INPUT  : 
**         formfile character string giving the name of form text file.
**       OUTPUT : 
**         sym: symbol to be saved
**         savsym: symbol name to save to 
**    RETURNS: none
**    SIDE EFFECTS : none
*********************************************************************/
ubi_savsym_form(formfile, sym, savsym)
char *formfile, *sym, *savsym;
{
	int *ans[7],mode,len,save_all,option,option2,cmdreject;
	UU_LOGICAL first = UU_TRUE;
	UD_FSTAT uj_noop(), ub_browse_csymfile(), ub_savsym_setname(),
		ub_savsym_libname(), ub_savsym_saveall();
	char **ub_get_symmaster_name();
	static char sym_name[11] = " ";
	static char fullname[UB_MAX_PATH_LEN] = "";
	static char errmsg[UB_MAX_PATH_LEN];
	char fext[] = "sy";
	static UD_METHOD methods[7] = {ub_savsym_setname, 
		ub_savsym_saveall, ub_browse_symlib, ub_savsym_libname, 
		ub_browse_symfile2, ub_savsym_setname};
	static char called[] = {6,6,6,6,6,6, 6};
	static int status = UU_SUCCESS;
	UD_MARK(cmdreject, UU_FALSE);
	if (cmdreject) goto done;
	Ssymname_list.answer =
		(char *) uu_malloc(UB_SYMBOL_NAME_LEN_PLUS * sizeof(char));
	strcpy(Ssymname_list.answer, sym_name);
	Ssymname_list.item = (char **)
									ub_get_symmaster_name(&(Ssymname_list.num_item));
	if (Ssymname_list.num_item==0)
	{
		ud_wrerr("No symbols in the UNIBASE");
		goto failed;
	}
	Ssave = UU_TRUE;
	ub_savsym_defpath();
	len = strlen(Snew_lib);
	Smk_dir = !ub_savsym_checklib(Tnew_lib,fullname,len);
	sprintf(errmsg,"Library %s not found. Do you wish to create it?",Snew_lib);
	if (Smk_dir && !ud_yesno2(0,errmsg,"Create Library?"))
	{
		Snew_lib[0] = Tnew_lib[0] = Sfull_path[0] = '\0';
		strcpy(Sfull_path,Sdef_path);
		strcpy(Snew_lib,Sfull_path);
		strcpy(Tnew_lib,Snew_lib);
		Smk_dir = UU_FALSE;
	}
	else
		strcpy(Sfull_path,fullname);
	strcpy(Tnew_lib,fullname);
	option = 0;
	ans[0] = (int *)&Ssymname_list;
	ans[1] = (int *)&save_all;
	ans[2] = (int *)&option;
	ans[3] = (int *)Snew_lib;
	ans[4] = (int *)&option2;
	ans[5] = (int *)Snew_sym;
	status = ud_form1("bsavsym.frm", ans, ans, methods, called, NULL, NULL);
/*
.....Nothing to save if answer[0] == '\0' or Snew_sym[0] == '\0'
*/
	if (status==-1 || Ssymname_list.answer[0]=='\0' || Snew_sym[0]=='\0')
	{
		if (Ssave && status == UU_SUCCESS)
		{
			if (Ssymname_list.answer[0]=='\0')
				ud_wrerr("No master symbol selected.");
			else if (Snew_sym[0]=='\0')
				ud_wrerr("Label required for master symbol.");
		}
		goto failed;
	}
	strcpy(sym, Ssymname_list.answer);
	if (Ssave)
	{
		if (Smk_dir)
		{
			mode= 0755;
			if (ux_mk_dir(Sfull_path, mode, UX_PRTERRS) != UU_SUCCESS)
					goto done;
		}
		ul_build_full_fname(Sfull_path,Snew_sym,fext,savsym);
	}
	else
		goto failed;
	goto done;
failed:
	status = UU_FAILURE;
done:
	Ssave = Smk_dir = UU_FALSE;
	Snew_sym[0]=Tnew_sym[0]='\0';
	ud_free_flist(&Ssymname_list);
	UD_UNMARK(cmdreject);
	return status;
}

/*********************************************************************
**    I_FUNCTION : int ubi_rensym_form(formfile,sym1, sym2)
**       Display form to get symbol name, rename symbol name
**    PARAMETERS   
**       INPUT  : 
**                      formfile                                                character string giving the name of form 
**                                                                                      text file.
**       OUTPUT : sym1: symbol to be renamed
**                                              sym2: symbol name to rename to 
**    RETURNS: none
**    SIDE EFFECTS : none
*********************************************************************/
ubi_rensym_form(formfile, sym1, sym2)
char *formfile, *sym1, *sym2;
{
	static int option;
	static char tempname1[UB_MAX_PATH_LEN] = "";
	static char tempname2[UB_MAX_PATH_LEN] = "";
	UD_FSTAT uj_noop(), ub_browse_symfile();
	static int *ans[] = {(int *)&option, (int *)tempname1, 
									(int *)&option, (int *)tempname2};
	static UD_METHOD methods[4] = {ub_browse_symfile, uj_noop, ub_browse_symfile, uj_noop};
	static char called[] = {6,6,6,6};
	int status;

	strcpy(tempname1, sym1);
	strcpy(tempname2, sym2);
	status = ud_form1(formfile, ans, ans, methods, called, NULL, NULL);
	if (status == -1) return(UB_FAILURE);
	strcpy(sym1, tempname1);
	strcpy(sym2, tempname2);
	return 0;
}       

/*********************************************************************
**    I_FUNCTION : int ubi_breaklib_sym(tempname, templib, bname)
**       Braek a path to symbol library name & symbol name. The library
**                              name is not include possible "_S/", the symbol name not include
**                              possible ".sy".
**    PARAMETERS   
**       INPUT  : 
**                      tempname                                                character string giving a symbol path 
**                      
**       OUTPUT :  
**                      templib: symbol library with path
**                      bname:  symbol name without path
**    RETURNS: none
**    SIDE EFFECTS : none
*********************************************************************/
void ubi_breaklib_sym(tempname, templib, bname)
char *tempname, *templib, *bname;
{
	int numint,args[2];

	ul_break_fname(tempname, templib, bname);
/*
.....remove passible ending '_S/' for symlib name
.....Yurong 9/18/98
*/
	numint = strlen(templib);
	if (numint > 0)
	{
		if (templib[numint-1]=='/')
		{
			templib[numint-1] = '\0';
			numint = numint-1;
		}
	}
#if UU_COMP == UU_VAXVMS
	if (numint >= 3)
	{
		if (templib[numint-3] == '_' && templib[numint-2] == 'S')
		{
			templib[numint-3] = ']';
			templib[numint-2] = '\0';
		}
	}
#else
	if (numint >=2 )
	{
		if ((templib[numint-2]=='_')&&(templib[numint-1]=='S'))
			templib[numint-2] = '\0';
	}
#endif
/*
.....remove ".sy" for symbol name
*/
	args[0] = 0 ; args[1] = 0;
	 ul_get_base_fname(bname,bname,args,UX_NPRTERRS);
/*
.....use default lib if has any
*/
	if (strlen(templib)==0)
	{
		if (UB_libdata_rec.default_firstim == UU_FALSE)
			strcpy(templib, UB_libdata_rec.default_lib);
	}
}
