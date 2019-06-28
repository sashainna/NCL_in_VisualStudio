/*******************************************************************
**    NAME:bumsymc.c
**       CONTAINS:  
**			ub_symbol_name
**			ub_symbol_set_vis
**			ub_symbol_rename
**			ub_symbol_delete
**			ub_simple_symbol
**			ubu_create_sym
**			ubu_get_geom_and_msyms
**			ubu_get_text_node_data
**			ubu_modify_dsfile
**			ubu_view_dsfile
**			symfst
**			symnxt
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       bumsymc.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       12/01/15 , 08:13:16
*********************************************************************/
#include "usysdef.h"		/* for UU_REAL, etc. */
#include "uhep.h"     	/* for error system */
#include "udebug.h"		/* for debugging trace facility */
#include "canbe.h"
#include "dselmask.h"
#include "dmark.h"		/* for UD_MARK */
#include "dasnog.h"		/* for DAS */
#include "mdcpln.h"
#include "mdrel.h"		/* for relation numbers */
#include "mcrv.h"
#include "mattr.h"
#include "mfort.h"
#include "xfsys1.h"		/* for ux calls and UX_NEXISTS */
#include "xenv1.h"		/* for ux calls and UX_PRTERRS */
#include "bsym.h"		
#include	"atext.h"		/* for text	*/
#include	"nclfc.h"
#include	"nclcmd.h"
#include	"nclinp.h"
#include	"nkeywd.h"

#define TRACE UU_FALSE /* for debugging only */
extern  ATXT_FRM	UB_txtattr;

static UU_LOGICAL Ssimple = UU_FALSE;
static int Sngeom,Ssub;
static UU_REAL Spt[3];
static char Sname[UB_SYMBOL_NAME_LEN];
/*
.....Note that if UM_MAXPICK changes, then the array size for keys in
.....declsy.f will need to be increased to match so SYMBOL command can
.....handle all the geometry in the command.
*/
static UU_KEY_ID Skeys[UM_MAXPICK];
static int Ssym_indx = 1, Sinst_indx = 1, Ssym_flag = 1;

/*********************************************************************
**    E_FUNCTION : void ub_symbol_name(symname,isub,key,origin,flag)
**       Determines if a symbol exists. Fortran callable routine.
**    PARAMETERS   
**       INPUT  :
**			         symname  = Name of symbol to see if exists.
**			         isub     = Subscript of symbol.
**       OUTPUT :
**			         key      = Key of symbol or instance when found.
**			         origin   = Origin of symbol instance when flag=2.
**			         flag     = 0 = Symbol nor instance exists.
**                           1 = Symbol exists.
**                           2 = Symbol instance exists.
**    RETURNS   : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ub_symbol_name(symname,isub,key,origin,flag)
UM_f77_str_ptr symname;
UM_int4 *isub,*key;
UM_real8 origin[];
UM_int2 *flag;
{
	int i;
	char *cstr;
	UM_int2 i3;
	UU_LOGICAL found,stilllooking;
	struct UB_symbol_rec sym;
	struct UB_instance_rec inst;
/*
.....Initialize routine
*/
	*flag = 0;
	*key = 0;
	i3 = 3;
	cstr = UM_cstr_of_f77_str(symname);
	strncpy(sym.label,cstr,UB_SYMBOL_NAME_LEN);
	i = UB_SYMBOL_NAME_LEN;
	ul_strip_blanks(sym.label,&i);
	sym.subscr = *isub;
	um_nullvc(origin);
/*
.....Check for symbol name in Unibase
*/
	if (ub_get_symmaster_by_name(&sym,&found, 0,1) != UU_SUCCESS) goto done;
	if (found)
	{
		*flag = 1;
		*key = sym.key;
	}
/*
.....Check for symbol instance
*/
	else
	{
		strcpy(inst.label,sym.label);
		inst.subscr = sym.subscr;
		if (ub_get_symmaster_by_name(&inst,&found, 0,2) != UU_SUCCESS) goto done;
		if (found)
		{
			*flag = 2;
			*key = inst.key;
			ub_get_instance_origin(&inst,origin);
			conpt(origin,&i3);
		}
	}
done:;
	return;
}

/*********************************************************************
**    E_FUNCTION : void ub_symbol_set_vis(nclkey,option)
**       Invisibles/Visibles all symbol instances or all instances
**       associated with a given symbol.
**    PARAMETERS   
**       INPUT  :
**			         option   = 0 = Invisible instances, 1 = Visible instances.
**			         nclkey   = Key of master symbol to modify instances
**                           for or 0 to modify all symbol instances.
**       OUTPUT :
**			         none
**    RETURNS   : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ub_symbol_set_vis(option,nclkey)
UM_int4 *nclkey;
UM_int2 *option;
{
	int i,ivis,nxtuple;
	struct UB_symbol_rec sym;
/*
.....Initialize routine
*/
	ivis = *option;
	if (ivis == 1) ivis = 2;
/*
.....Modify instances of a given symbol
*/
	if (*nclkey != 0)
	{
		sym.key = *nclkey;
		if (ub_retrieve_sym(&sym,sizeof(sym)) != UU_SUCCESS) goto done;
		for (i=0;i<sym.no_inst;i++)
			ncl_sea_ent_blank(ivis,sym.inst[i].inst_key);
	}
/*
.....Modify all symbol instances
*/
	else
	{
		nxtuple = 1;
		while (ur_get_next_data_key(UB_INSTANCE_REL,&nxtuple,&sym.key) == 0)
		{
			ncl_sea_ent_blank(ivis,sym.key);
			nxtuple++;
		}
	}
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    E_FUNCTION : void ub_symbol_rename(nclkey,label,isub,ierr)
**       Invisibles/Visibles all symbol instances or all instances
**       associated with a given symbol.
**    PARAMETERS   
**       INPUT  :
**			         option   = 0 = Invisible instances, 1 = Visible instances.
**			         nclkey   = Key of master symbol to modify instances
**                           for or 0 to modify all symbol instances.
**       OUTPUT :
**			         none
**    RETURNS   : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ub_symbol_rename(nclkey,label,isub,ierr)
UM_int4 *isub,*nclkey;
UM_f77_str_ptr label;
UM_int2 *ierr;
{
	int i;
	UM_int2 flag;
	UM_int4 key;
	UU_REAL origin[3];
	char *cstr;
	struct UB_symbol_rec sym;
/*
.....Initialize routine
*/
	*ierr = 0;
/*
.....Determine if symbol with new label already exists
*/
	ub_symbol_name(label,isub,&key,origin,&flag);
	if (flag != 0) goto failed;
/*
.....Rename symbol
*/
	sym.key = *nclkey;
	if (ub_retrieve_sym(&sym,sizeof(sym)) != UU_SUCCESS) goto done;
	cstr = UM_cstr_of_f77_str(label);
	strncpy(sym.label,cstr,UB_SYMBOL_NAME_LEN);
	i = UB_SYMBOL_NAME_LEN;
	ul_strip_blanks(sym.label,&i);
	sym.subscr = *isub;
	ur_update_data_fixed(&sym);
/*
.....End of routine
*/
done:;
	return;
/*
.....Symbol name is already used
*/
failed:;
	*ierr = 1;
	goto done;
}

/*********************************************************************
**    E_FUNCTION : void ub_symbol_delete(nclkey)
**       Deletes a symbol and all of its associated instances.
**    PARAMETERS   
**       INPUT  :
**			         nclkey   = Key of master symbol to delete or 0 to
**                           delete all symbols.
**       OUTPUT :
**			         none
**    RETURNS   : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ub_symbol_delete(nclkey)
UM_int4 *nclkey;
{
	int i,nxtuple;
	UU_KEY_ID key;
/*
.....Delete single symbol
*/
	if (*nclkey != 0)
	{
		key = *nclkey;
		ubu_del_symmaster(key,UU_FALSE,UU_FALSE);
	}
/*
.....Delete all symbols
*/
	else
	{
		nxtuple = 1;
		while (ur_get_next_data_key(UB_SYMBOL_REL,&nxtuple,&key) == 0)
		{
			ubu_del_symmaster(key,UU_FALSE,UU_FALSE);
			nxtuple++;
		}
	}
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    E_FUNCTION : void ub_simple_symbol(symname,isub,keys,nent,pt,ierr)
**       Create a simple symbol (geometry only).  Fortran callable routine.
**    PARAMETERS   
**       INPUT  :
**			         symname  = Name of symbol to create.
**			         isub     = Subscript of symbol to create.
**			         keys     = Array of geometry keys to include in symbol.
**			         nent     = Number of geometry keys.
**						pt       = Attach point.
**       OUTPUT :
**			         ierr     = Returns non-zero on error.
**    RETURNS   : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ub_simple_symbol(symname,isub,keys,nent,pt,ierr)
UM_f77_str_ptr symname;
UM_int4 *isub,keys[];
UM_int2 *nent;
UM_real8 pt[];
UM_int2 *ierr;
{
	int i,stat;
	char *cstr;
/*
.....Store arguments in global variables
*/
	cstr = UM_cstr_of_f77_str(symname);
	strncpy(Sname,cstr,NCL_MAX_LABEL);
	i = NCL_MAX_LABEL;
	ul_strip_blanks(Sname,&i);
	Ssub = *isub;
	Sngeom = *nent;
	for (i=0;i<Sngeom;i++) Skeys[i] = keys[i];
	Spt[0] = pt[0] ; Spt[1] = pt[1] ; Spt[2] = pt[2];
/*
.....Create symbol
*/
	*ierr = 0;
	Ssimple = UU_TRUE;
	stat = ubu_create_sym();
	Ssimple = UU_FALSE;
	if (stat != UU_SUCCESS) *ierr = 1;
	return;
}
	
/*********************************************************************
**    E_FUNCTION : void ub_simple_symbol(symname,isub,keys,nent,pt,ierr)
**       Create a simple symbol (geometry only).  Fortran callable routine.
**    PARAMETERS   
**       INPUT  :
**			         symname  = Name of symbol to create.
**			         isub     = Subscript of symbol to create.
**			         keys     = Array of geometry keys to include in symbol.
**			         nent     = Number of geometry keys.
**						tlen  = tool length
**						pt       = Attach point.
**       OUTPUT :
**			         ierr     = Returns non-zero on error.
**    RETURNS   : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ubf_create_symbol(symname,isub,keys,nent,pt,tlen,ierr)
UM_f77_str_ptr symname;
UM_int4 *isub,keys[];
UM_int2 *nent;
UM_real8 pt[], *tlen;
UM_int2 *ierr;
{
	int i,stat;
	char *cstr;
	UU_REAL opt[3], spt[3];
	char sname[NCL_MAX_LABEL];
	struct UB_symbol_rec sym;
	struct UM_point_rec ept;
	struct UM_attrdata_rec ptattr;
	struct UB_snap_nod_rec snapnod;
	UU_KEY_ID deleteList[UM_MAXPICK];
	int nbrOnDelList;
	struct UB_master_sym_frm_rec master_sym_frm_out;
	char fullpath[UB_MAX_PATH_LEN], dir[UB_MAX_PATH_LEN],
				bname[UB_MAX_PATH_LEN];
	char *ux_getenv();
	int fmode;
	int dsegid;			/* display segment id */
	int status = UU_SUCCESS;
	int rel_num;
	int sngeom,ssub;
	UU_LOGICAL found;
	NCL_cmdbuf cmdbuf;
	char label[NCL_MAX_LABEL_AND_SUBSCRIPT],sbuf[80];
/*
.....Store arguments in global variables
*/
	cstr = UM_cstr_of_f77_str(symname);
	strncpy(sname,cstr,NCL_MAX_LABEL);
	i = NCL_MAX_LABEL;
	ul_strip_blanks(sname,&i);
	ncl_parse_label(sname, sym.label, &sym.subscr);
	*ierr = 0;
	ssub = *isub;
	sngeom = *nent;
	opt[0] = pt[0] ; opt[1] = pt[1] ; opt[2] = pt[2];
	spt[0] = pt[0] ; spt[1] = pt[1] ; spt[2] = pt[2] + *tlen;
////////////////////////////
/*
.....Delete existing one if it already exists
*/
	if (ub_get_symmaster_by_name(&sym,&found,1,1) == UU_SUCCESS)
	{
		if (found) ubu_del_symmaster(sym.key,UU_FALSE,UU_FALSE);
	}
	sym.no_masters = 0;
	sym.no_inst = 0;
	sym.no_geom = 0;
	sym.no_text_nod = 0;
	sym.no_snap_nod = 0;
	sym.version = -1;

	UB_SETUP_DATA(UB_SYMBOL_REL, &sym, sizeof(struct UB_symbol_rec), status);
	if (status == UB_FAILURE)
		goto failed;
	sym.key = -1; 
/*
.....Create simple symbol
.....Name already furnished
*/
	ncl_parse_label(sname, sym.label, &sym.subscr);
/*
.....Create a symbol
.....Geometry already furnished
*/
	for (i=0;i<sngeom;i++)
	{
		if (ub_add2GeomList(keys[i],&sym,deleteList,&nbrOnDelList) !=
					UU_SUCCESS) goto failed;
	}
/*
.....snag node
*/
	ur_setup_data(UM_POINT_REL, &ept, sizeof(struct UM_point_rec));
	for(i=0; i < 3; i++) ept.pt[i] = spt[i];
	ept.snap_node = UU_TRUE;
	ept.markertype = UB_snap_node_marker_type;	
	if (um_current_default_attr(UM_POINT_REL, &ptattr) != UU_SUCCESS)
		goto failed;		
	ptattr.color = UB_snap_node_color;
	if (um_create_geom(&ept, UM_DEFAULT_TF, &ptattr) != UU_SUCCESS)
		goto failed;
	if (uc_display(&ept) != UU_SUCCESS)
		goto failed;
	snapnod.snap_key = ept.key;
	snapnod.nbr = 1;
	if (ubi_update_app_varlist(&sym, UB_MSNAP_LIST, &snapnod, 1, 1) 
						!= UU_SUCCESS) goto failed;
	if (ubi_reset_marker_type() != UU_SUCCESS)
		goto failed;
/*
.....orig point
*/
	status = ubi_orient_symbol(&sym, opt);
	if (ub_create_symbol_tuple(&sym,UB_DEFAULT_TF,UB_CURRENT_ATTR) !=
				UU_SUCCESS)
		goto failed;
/* delete the master symbol subentities from the DISPLAY */
	for (i=0; i<sym.no_geom; i++)
	{
		ur_retrieve_disp_segid(sym.geom[i], &dsegid);
		if (dsegid >= 0) uv_delsegs(dsegid);
		ur_update_disp_segid(sym.geom[i], -1);
		if (um_retrieve_data_relnum(sym.geom[i],&rel_num) == UU_SUCCESS)
			ncl_randel(sym.geom[i],rel_num);
	}
	ur_retrieve_disp_segid(sym.snap_nod[0].snap_key, &dsegid);
	if (dsegid >= 0) uv_delsegs(dsegid);                                 
	ur_update_disp_segid(sym.snap_nod[0].snap_key, -1); 
	goto done;
failed: *ierr = 1;
done:;
	*ierr = 0;
	return;
}

/*********************************************************************
**    E_FUNCTION : int ubu_create_sym()
**       Create a master symbol.
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT : none.
**    RETURNS   : UU_SUCCESS if no problems encountered, UB_FAILURE
**                otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubu_create_sym()
{
	struct UB_symbol_rec sym,sym1;
	UU_KEY_ID deleteList[UM_MAXPICK];/* list of entities to delete*/
	int nbrOnDelList;
	/******************* MASTER SYMBOL FORM DECLARATIONS *******/
	/* master symbol form output record */
	struct UB_master_sym_frm_rec master_sym_frm_out;
	char fullpath[UB_MAX_PATH_LEN], dir[UB_MAX_PATH_LEN],
				bname[UB_MAX_PATH_LEN];
	char *ux_getenv();
	int fmode;
	int cmdreject;		/* command reject variable, TRUE iff a command reject has
							 * occurred */
	int dsegid;			/* display segment id */
	int i, status = UU_SUCCESS;
	int rel_num;
	UU_LOGICAL found;
	NCL_cmdbuf cmdbuf;
	char label[NCL_MAX_LABEL_AND_SUBSCRIPT],sbuf[80];
	uu_denter(UU_BTRC, (us, "ubu_create_sym()"));

/*
.....Creating simple symbol
.....Delete existing one if it already exists
*/
	if (Ssimple)
	{
		strcpy(sym.label,Sname);
		sym.subscr = Ssub;
		if (ub_get_symmaster_by_name(&sym,&found,1,1) == UU_SUCCESS)
		{
			if (found) ubu_del_symmaster(sym.key,UU_FALSE,UU_FALSE);
		}
	}
	/* initializations for the nbr of text and snap nodes, also used in case
	 * command reject hit */
	sym.no_masters = 0;
	sym.no_inst = 0;
	sym.no_geom = 0;
	sym.no_text_nod = 0;
	sym.no_snap_nod = 0;
	if (Ssimple)
		sym.version = UB_NOLIB;
	else
		sym.version = -1;	/* merely a default value; not to be used */

	if (Ssimple) cmdreject = UU_FALSE;
	else UD_MARK(cmdreject, UU_FALSE);

	if (!cmdreject)	/* then no command reject encountered, go on */
	{
		UB_SETUP_DATA(UB_SYMBOL_REL, &sym, sizeof(struct UB_symbol_rec), status);
		if (status == UB_FAILURE)
			goto failed;
		sym.key = -1; /* this is a flag used to determine whether the master
							* master symbol tuple has been created or not */
/*
.....Create simple symbol
.....Name already furnished
*/
		if (Ssimple)
		{
			strcpy(sym.label,Sname);
			sym.subscr = Ssub;
			sym.path[0] = '\0';
			master_sym_frm_out.cmd = 0;
		}
/*
.....Get master symbol data from form
*/
		else
		{
			if (ubu_get_symmaster_data(&master_sym_frm_out, sym.path) != UU_SUCCESS) 
						goto failed;
		/* note we have an appropriate symbol name and library name; also,
		 * there is no master symbol currently in UNIBASE with the returned
		 * symbol name, and there exists a symbol library with the returned
		 * library name. */
			ncl_parse_label(master_sym_frm_out.name,sym.label,&sym.subscr);

		/* check to see if user will overwrite existing symbol file and ask */
			fmode = UX_EXISTS;
		/* note that work is done only in the local area. make full path */
/*
.....if symlib already has a path, ignore symbol area
.....Yurong changed 9/18/98
*/
			ul_break_fname(master_sym_frm_out.lib, dir, bname);
			if (dir[0]!='\0')
			{
				status = ux_mk_chk_syspath(UU_NULL, master_sym_frm_out.lib,
					master_sym_frm_out.name,"UB_SYM_EXTEN","UB_SYM_SUFFIX",&fmode,
					fullpath, UX_PRTERRS);
			}
			else
			{
				status = ux_mk_chk_syspath("UB_LOC_M_SYMDIR",master_sym_frm_out.lib,
					master_sym_frm_out.name,"UB_SYM_EXTEN","UB_SYM_SUFFIX",&fmode,
					fullpath, UX_PRTERRS);
			}
			if (status!= UU_SUCCESS) goto failed;
			uu_dprint(UU_BTRC,(us,"mode is: %d, for %s", fmode, fullpath));
			if (fmode != (fmode|UX_NEXISTS) && master_sym_frm_out.cmd != 1)
			{	
				if (!ud_yesno(0, uu_uprompt1(UB_SYMBOL,31,fullpath), 
								"Symbol exists"))
					/* %s already exists; okay to overwrite? */
					goto done;
			}
		}

		/* get the geometry and delete the display segments of the entities to 
		 * be in the symbol; note, no master symbols in master symbols yet. */
/*
.....Create simple symbol
.....Geometry already furnished
*/
		if (Ssimple)
		{
			nbrOnDelList = 0;
			for (i=0;i<Sngeom;i++)
			{
				if (ub_add2GeomList(Skeys[i],&sym,deleteList,&nbrOnDelList) !=
					UU_SUCCESS) goto failed;
			}
		}
/*
.....Prompt user for geometry
*/
		else
		{
			if (ubu_get_geom_and_msyms(&sym, deleteList, &nbrOnDelList) != UU_SUCCESS) 					goto failed;
		}
/*
.....Creating a command
.....Act as if the command was entered already
.....and follow same logic as Create Simple Symbol
*/
		if (master_sym_frm_out.cmd == 1)
		{
			sym.no_text_nod = 0;
			sym.no_snap_nod = 0;
			sym.version = UB_NOLIB;
			Spt[0] = master_sym_frm_out.origin[0];
			Spt[1] = master_sym_frm_out.origin[1];
			Spt[2] = master_sym_frm_out.origin[2];
			Ssimple = UU_TRUE;
			ul_to_upper(sym.label);
			strcpy(sym1.label,sym.label);
			sym1.subscr = sym.subscr;
			if (ub_get_symmaster_by_name(&sym1,&found,1,1) == UU_SUCCESS)
			{
				if (found) uc_delete(sym1.key);
			}
		}
		/********* geometry gotten *********/

		/* get any text node data and put text in UNIBASE */
		if (!Ssimple)
			if (ubu_get_text_node_data(&sym) != UU_SUCCESS) goto failed;

		/********** text nodes gotten *******/

		/* get any snap nodes */
		if (!Ssimple)
			if (ubu_get_snap_nodes(&sym) != UU_SUCCESS) goto failed;

		/********** snap nodes gotten ********/
#if (TRACE)
		ub_print_sym(&sym);
#endif
	  	/* Fix the geometry for the master symbol so that it is relativized
	 	 * to the symbol origin and construction plane coordinate axises; 
		 * note, 
		 * 1) we transform all the subentities so that the transforms
		 * 	associated with them are the identity transform;
		 */
/*
.....Create simple symbol
.....Origin is already furnished
*/
		if (Ssimple)
			status = ubi_orient_symbol(&sym, Spt);
		else
			status = ubi_orient_symbol(&sym, master_sym_frm_out.origin);
		if (status != UU_SUCCESS) goto failed;

/*
.....Create simple symbol
.....In active Unibase only (do not save to separate file)
*/
		if (Ssimple)
		{
			if (ub_create_symbol_tuple(&sym,UB_DEFAULT_TF,UB_CURRENT_ATTR) !=
				UU_SUCCESS)
					goto failed;
		}
		else
		{
			if (ubu_mk_master_sym(&sym,master_sym_frm_out.lib,UB_DEFAULT_TF,
						UB_CURRENT_ATTR) != UU_SUCCESS) goto failed;
		}
/*
.....Never enters this logic
*/
/* now delete this geometry completely because whatever information
		 * needed was put into the geometry list already. */
		for (i=0; i<nbrOnDelList; i++)
		{
			/* delete the display segments for these subentities */
			ur_retrieve_disp_segid(deleteList[i], &dsegid);
			if (dsegid >= 0) uv_delsegs(dsegid);											
			ur_update_disp_segid(deleteList[i], -1); 
			if (ur_delete_all(deleteList[i]) != 0)
			{
				if (!Ssimple)
					uu_uerror2(UB_SYMBOL, 16, deleteList[i], "ubu_create_sym");
				/* error is: Can't delete entity record, %d, from UNIBASE  (%s). */
				goto failed;
			}
		}

/*
.....Delete this entity from the ranfile also
.....Since it now belongs the symbol only
.....Bobby  -  11/16/00
*/
		/* delete the master symbol subentities from the DISPLAY */
		for (i=0; i<sym.no_geom; i++)
		{
			/* delete the display segments for these subentities */
			ur_retrieve_disp_segid(sym.geom[i], &dsegid);
			if (dsegid >= 0) uv_delsegs(dsegid);
			ur_update_disp_segid(sym.geom[i], -1);
			if (um_retrieve_data_relnum(sym.geom[i],&rel_num) == UU_SUCCESS)
				ncl_randel(sym.geom[i],rel_num);
		}
		for (i=0; i<sym.no_text_nod; i++)
		{
			/* delete the display segments for these subentities */
			ur_retrieve_disp_segid(sym.text_nod[i].text_key, &dsegid);
			if (dsegid >= 0) uv_delsegs(dsegid);                                 
			ur_update_disp_segid(sym.text_nod[i].text_key, -1); 
		}
		for (i=0; i<sym.no_snap_nod; i++)
		{
			/* delete the display segments for these subentities */
			ur_retrieve_disp_segid(sym.snap_nod[i].snap_key, &dsegid);
			if (dsegid >= 0) uv_delsegs(dsegid);                                 
			ur_update_disp_segid(sym.snap_nod[i].snap_key, -1); 
		}
#if (TRACE)
		ub_print_sym(&sym);
#endif

 		/* last thing to do is to create the optional description text file */
 		/* check the descriptions flag and prompt the user */
 		if ( strcmp(ux_getenv("UB_DESCRIBE",1), "UU_TRUE") == 0 && !Ssimple)
 		{
 			if (!ud_yesno(0, uu_uprompt1(UB_SYMBOL,118,master_sym_frm_out.name), "Question?"))
 				/* Do you want to create a descripton for symbol %s ? */
 				goto done;
 			if (uxu_create_dsfile("UB_LOC_M_SYMDIR",master_sym_frm_out.lib,
 				master_sym_frm_out.name) != UU_SUCCESS)
 				uu_uerror1(UB_SYMBOL,108,master_sym_frm_out.name);
 				/* error mesage is "Unable to create description for symbol %s */
			else
				/*compose and put out a message to the user and wait for "done" */
				uu_uerror1(UB_SYMBOL,109,"dscom");
				/* message is "Use script file dscom to enter the description. " */
				/* not an error, really just a message to guide the user */
 		}
					
	}/* end no command reject */
	else /* command reject */
	{
 		/* if command reject, need to remove ds file also ? */
		if (ubi_msymCmdrejtOrFailCleanup(&sym) != UU_SUCCESS)
				goto done;
	}
/*
.....Output SYMBOL/ command
*/
	if (master_sym_frm_out.cmd == 1 && sym.no_geom > 0)
	{
		Ssimple = UU_FALSE;
		ncl_init_cmdbuf(&cmdbuf);
		ncl_get_label(&sym,label);
		ncl_add_token(&cmdbuf,label,NCL_nocomma);
		ncl_add_token(&cmdbuf,"=SYMBOL/",NCL_nocomma);
		if (Spt[0] != 0. || Spt[1] != 0. || Spt[2] != 0.)
		{
			ncl_add_token(&cmdbuf,NCL_at,NCL_comma);
			ncl_add_token(&cmdbuf,"(",NCL_nocomma);
			ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
			ncl_sprintf(sbuf,Spt,3);
			ncl_add_token(&cmdbuf,sbuf,NCL_nocomma);
			ncl_add_token(&cmdbuf,")",NCL_comma);
		}
		for (i=0; i<sym.no_geom; i++)
		{
			ncl_get_label_with_key(sym.geom[i],label);
			ncl_add_token(&cmdbuf,label,NCL_comma);
		}
		ncl_add_cmdbuf(&cmdbuf);
		ncl_put_in_src(cmdbuf);
	}
	goto done;
failed: status = UB_FAILURE;
	ubi_msymCmdrejtOrFailCleanup(&sym);
	UB_IF_FAILURE_PRINT_IT
done:;
	UD_UNMARK(cmdreject);
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    I_FUNCTION:int ubu_get_geom_and_msyms(symptr,deleteList, nbrOnDelListptr)
**			This function determines the geometry and master symbols that 
**			are to be put in a master symbol.
**    PARAMETERS   
**       INPUT  : 
**				symptr			Pointer to the master symbol entity being created.
**       OUTPUT :  
**				symptr			Pointer to the master symbol entity being created
**									with the geometry and master symbol lists filled in.
**				deleteList		List of key ids of entities to delete from Unibase.
**				nbrOnDelListptr  Pointer to the number of keys on "deleteList".
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubu_get_geom_and_msyms(symptr, deleteList, nbrOnDelListptr)
	struct UB_symbol_rec *symptr;
	UU_KEY_ID deleteList[];
	int *nbrOnDelListptr;
{
	UU_LOGICAL startover;
	UU_KEY_ID key;
	int level, nbrpicks, status = UU_SUCCESS;
	static int first = UU_TRUE;

	uu_denter(UU_BTRC,(us,
	"ubu_get_geom_and_msyms(symptr:%x,deleteList:%x,?)", symptr, deleteList));

	/*	-- set up the select mask for symbol subsystem -- */

	if (first == UU_TRUE)
	{
		uc_build_select_mask(UC_CANBE_ASYMBOL, UD_canbe_asymbol);
		first = UU_FALSE;
	}
	symptr->no_geom = 0;
	symptr->no_masters = 0;
	*nbrOnDelListptr = 0; /* nbr of entities to delete after their subentities 
								  * have been added to the master being created */

	ud_lgeo(UU_TRUE, UD_canbe_asymbol);
	ud_ldas(UD_DASSELECT, UB_SYMBOL, 22, UU_NULL, 100, &nbrpicks, UD_NODEFAULT);
	/* prompt: Pick entities to be put in master symbol. */
	if (nbrpicks <= 0)
		goto done;

	level = 1;	/* picking level */
	startover = UU_TRUE;	/* get all keys for entities picked */	
	while ((status == UU_SUCCESS) 
			&& (ud_gnxt(startover, UU_NULL, &key, level)))
	{
		startover = UU_FALSE;
		if (ub_add2GeomList(key, symptr, deleteList, nbrOnDelListptr)
			!= UU_SUCCESS)	goto failed;
	}/* end while */

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION :int ubu_get_text_node_data(symptr)
**       Get text nodes for a master symbol and put them in UNIBASE.
**    PARAMETERS   
**       INPUT  :
**				symptr		pointer to the master symbol record.
**       OUTPUT :  
**				symptr		pointer to the master symbol with text node
**								data filled in.
**    RETURNS: UU_SUCCESS if no difficulties with forms; UB_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubu_get_text_node_data(symptr)
	struct UB_symbol_rec *symptr;
{
	UU_LOGICAL ud_lyesno();
	UU_LOGICAL more_text_nodes;/* TRUE iff there are (potentially) more text
										 * nodes. */
	/******************** TEXT FORM INITIALIZATION VALUES ***************/
	static struct UB_text_node_frm_rec tnode_frm_out = {0, 0, 1};
			/* output structure for forms and defaults */
	struct UB_text_nod_rec textnod;
	struct UA_txt_rec text;	/* storage for text entity */
	struct UA_txtattr_rec text_attr; /* text attribute bundle */
	char	 textstr[UA_TEXT_BUFSZ];
/**	UU_REAL	loc[3];				 text node position */
    UD_NDCLOCREC loc;

	int i, numint, numlines;
	int status = UU_SUCCESS;
	uu_denter(UU_BTRC,(us,"ubu_get_text_node_data(symptr:%x)",symptr));

	i = 0; /* index of next text node in variable list */
	/* get text nodes */
	more_text_nodes = UU_TRUE;
/*
.....Ensure there is no garbage in textstr before the user gives a string
*/
	sprintf(textstr,"");
	while (more_text_nodes)
	{
		ur_setup_data(UA_TEXT_REL, &text, sizeof(struct UA_txt_rec));

		/* get next text node location, prompt message is: 
		 * Enter text node position. */
		ud_ldas(UD_DASCART,UB_SYMBOL, 23, &loc, 1, &numint, UD_NODEFAULT);

		if (numint <= 0) /* done with text nodes */
			more_text_nodes = UU_FALSE;
		else /* get text, angle, and whether it is a prompt or a label */
		{
			if (ubi_text_node_form("btxtnod.frm", &tnode_frm_out) != UU_SUCCESS)
				goto failed;

			/* the local text infomation overide the global attributes */
			UB_txtattr.color = tnode_frm_out.color;
			UB_txtattr.tangle = tnode_frm_out.angle;

			/* returned types: 0=response optional, 1=response required, 2=label */
			textnod.prompt = (int)(tnode_frm_out.type);

			/* returned visibility: UB_GRAPHIC_TEXT_NODE=0=graphic, 
			 *								UB_NONGRAPHIC_TEXT_NODE=1=nongraphic */
			textnod.visibility = (int)(tnode_frm_out.visibility);
			
			/* get all the user input text strings     */
			/* set the numlines to control how many lines the user can put in the
				text. numlines=0 means no control of input line number  */
			if ((textnod.prompt == 0) || (textnod.prompt == 1))
				/* either optional or required text */
				numlines = 1;
			else /* a label; put in as many lines as desired */
				numlines = 0; 
			ua_get_texts(52,255,textstr,&numint,&numlines);
			if (numint <= 0) 
			{
				/* no text entered, ask if more text nodes are to be given.  */	
				more_text_nodes = ud_lyesno(UB_SYMBOL, 6);
			}
			else /* text given */
			{                                         
				/* set the text record and the text attribute boundle 	  */
				ua_init_txtrec(&text,&text_attr,&UB_txtattr,UU_FALSE);

				strcpy(text.tchar,textstr);
				text.no_tchar = numint; 		/* nbr char's */

				/* create new text entity in UNIBASE */
			
				/***** fillin text attribute bundle *******/
				ur_setup_data(UA_TEXTATTR_REL, &text_attr, 
									sizeof(struct UA_txtattr_rec));

				/* look up correct color index */
/*	
.....Not needed now that colors are handled directly - Andrew 10/08/12
				if (ubi_get_form_color(tnode_frm_out.color, &(text_attr.color))
					!= UU_SUCCESS) goto failed;
*/
				/* get text origin */
				{
					UU_REAL origin[3];
					UU_REAL xaxis[3];
					UU_REAL yaxis[3];
					UU_REAL zaxis[3];
					um_getcpln(origin,xaxis,yaxis,zaxis);
					um_nptpln(&loc,origin,zaxis,text.position);
					/* set dx, dy */
					/*ua_txt_offset(&text,&text_attr,offset_cc);			 */
					/* set text origin and set dx,dy*/
					ua_txt_origin(&text,&text_attr,UU_FALSE);
				}

				if (uc_create_data(&text, UM_DEFAULT_TF, &text_attr) != UU_SUCCESS)
					goto failed;
#if (TRACE)
				ubi_pscroll("MASTER SYMBOL TEXT");
				uc_print(&text);
#endif

				textnod.text_key = text.key;

				/* put the text node record into the variable list */
				if (ubi_update_app_varlist(symptr,UB_MTEXT_LIST,&textnod,
							i+1, 1) != UU_SUCCESS) goto failed;

				if (uc_display(&text) != UU_SUCCESS)
						goto failed;

				i++; /* get index for next text node */
			}/* end text given */
		}
	}/* end while */
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION :int ubi_print_text_attr(attrptr)
**       This function prints text attributes.
**    PARAMETERS   
**       INPUT  :
**				attrptr		Pointer to the text attribute bundle.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no difficulties with forms; UB_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ubi_print_text_attr(attrptr)
	struct UA_txtattr_rec *attrptr;
{
	int status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,"ubi_print_text_attr(attrptr->key:%d)",attrptr->key));

	sprintf(UB_sbuf,"	TEXT ATTRIBUTES FOR TEXT ENTITY %d", attrptr->key);
	ubi_pscroll(UB_sbuf);

	sprintf(UB_sbuf,"	use_count:%d, color:%d, layer:%d, pen:%d, line_style:%d", 
				attrptr->use_count, attrptr->color, attrptr->layer, attrptr->pen, attrptr->line_style);
	ubi_pscroll(UB_sbuf);

	sprintf(UB_sbuf,"	line_width:%g, displayable:%d, selectable:%d",
				attrptr->line_width, attrptr->displayable, attrptr->selectable);
	ubi_pscroll(UB_sbuf);

	sprintf(UB_sbuf,"	font:%d, prec:%d, expn:%g, spacing:%g",
				attrptr->font, attrptr->prec, attrptr->expn, attrptr->spacing);
	ubi_pscroll(UB_sbuf);

	sprintf(UB_sbuf,"	height:%g, up(vector):%g, %g, %g", attrptr->height,
				attrptr->up[0], attrptr->up[1], attrptr->up[2]);
	ubi_pscroll(UB_sbuf);

	sprintf(UB_sbuf,"	plane:%g %g %g, path:%d", attrptr->plane[0],
		attrptr->plane[1], attrptr->plane[2], attrptr->path);
	ubi_pscroll(UB_sbuf);

	sprintf(UB_sbuf,"	align_hor:%d, align_ver:%d", 
				attrptr->align_hor, attrptr->align_ver);
	ubi_pscroll(UB_sbuf);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION: int ubi_msymCmdrejtOrFailCleanup(symptr)
**		This functions cleans up UNIBASE and DIGS if a failure
**		occurs during master symbol creation; in particular,
**		any text nodes and snap nodes created are deleted.
**    PARAMETERS   
**       INPUT  :
**				symptr		pointer to the master symbol record.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no difficulties with forms; UB_FAILURE otherwise.
**    SIDE EFFECTS : Modifies UNIBASE and DIGS.
**    WARNINGS     : none
*********************************************************************/
int ubi_msymCmdrejtOrFailCleanup(symptr)
	struct UB_symbol_rec *symptr;
{
	int i, status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,"ubi_msymCmdrejtOrFailCleanup(symptr:%x)",symptr));
	symptr->no_geom = 0;
	/* delete any text or snap node entities created */
	for (i=0; i<symptr->no_text_nod; i++)
		if (uc_delete(symptr->text_nod[i].text_key) != UU_SUCCESS)
			goto failed;
	symptr->no_text_nod = 0;
	for (i=0; i<symptr->no_snap_nod; i++)
		if (uc_delete(symptr->snap_nod[i].snap_key) != UU_SUCCESS)
			goto failed;
	symptr->no_snap_nod = 0;
	/* reset GKS marker type in case it was changed for snap nodes */
	if (ubi_reset_marker_type() != UU_SUCCESS)
		goto failed;
	if (symptr->key != -1) /* delete the new master */
		if (ubu_del_symmaster(symptr->key, UU_FALSE,UU_TRUE) != UU_SUCCESS)
				goto failed;
	goto done;
failed: status = UB_FAILURE;
	uu_dprint(UU_BTRC,(us,"FAILED ON CLEANUP"));
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}
/*********************************************************************
**    E_FUNCTION : int ubu_modify_dsfile()
**			Create a command file to use to edit a symbol description file.
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT : none.
**    RETURNS   : UU_SUCCESS if no problems encountered, UB_FAILURE
**                otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubu_modify_dsfile()
{
	UX_pathname symname, libname;
	int cmdreject;		/* command reject variable, TRUE iff a command reject has
							 * occurred */
	int numint;			/* number of DAS interactions */
	char *prompt, *uu_uprompt0();
	int status = UU_SUCCESS;
	int file_status;
	char *ux_getenv();

	uu_denter(UU_BTRC, (us, "ubu_modify_dsfile()"));

	UD_MARK(cmdreject, UU_FALSE);
	if (!cmdreject)	/* then no command reject encountered, go on */
	{
 		if ( strcmp(ux_getenv("UB_DESCRIBE",1), "UU_TRUE") == 0)
 		{
			if (ubu_ask_for_symmaster(symname, &numint) != UU_SUCCESS)
				goto failed;
			if (numint <= 0)
				goto done;
			prompt = uu_uprompt0(UB_SYMBOL, 37);
			/* Prompt is: Enter symbol library name. */
			if (uxu_ask_for_lib(&UB_libdata_rec, prompt,libname,&numint,
				&file_status) != UU_SUCCESS) 
				goto failed;
			if (numint <= 0) /* then no library name given */
				goto done;
			if (file_status != UX_FAREA) /* we did not get a directory */
			{
				uu_uerror2(UB_SYMBOL, 49, libname, "ubu_modify_dsfile");
				/* error is: Invalid file area specification: %s  (%s). */
					goto failed;
			}

 			if (uxu_modify_dsfile("UB_LOC_M_SYMDIR",libname,symname) != UU_SUCCESS)
 				uu_uerror1(UB_SYMBOL,111,symname);
 				/* error mesage is "Unable to modify description for symbol %s */
			else
				/*compose and put out a message to the user and wait for "done" */
				uu_uerror1(UB_SYMBOL,112,"modcom");
				/* message is "Use script file modcom to edit the description. " */
				/* not an error, really just a message to guide the user */
 		}
					
	}/* end no command reject */
	else /* command reject */
	{
 		/* if command reject, need to remove connamd file also ? */
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
**    E_FUNCTION : int ubu_view_dsfile()
**			View a symbol description file in a window (no editing).
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT : none.
**    RETURNS   : UU_SUCCESS if no problems encountered, UB_FAILURE
**                otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubu_view_dsfile()
{
	UX_pathname symname, libname;
	int cmdreject;		/* variable TRUE iff a reject has occurred */
	int numint;			/* number of DAS interactions */
	int status = UU_SUCCESS;
	char *prompt, *uu_uprompt0();
	int file_status;
	char *ux_getenv();

	uu_denter(UU_BTRC, (us, "ubu_view_dsfile()"));

	UD_MARK(cmdreject, UU_FALSE);
	if (!cmdreject)	/* then no command reject encountered, go on */
	{
 		if ( strcmp(ux_getenv("UB_DESCRIBE",1), "UU_TRUE") == 0)
 		{
			if (ubu_ask_for_symmaster(symname, &numint) != UU_SUCCESS)
				goto failed;
			if (numint <= 0)
				goto done;
			prompt = uu_uprompt0(UB_SYMBOL, 37);
			/* Prompt is: Enter symbol library name. */
			if (uxu_ask_for_lib(&UB_libdata_rec, prompt,libname,&numint,
				&file_status) != UU_SUCCESS)
				goto failed;
			if (numint <= 0) /* then no library name given */
				goto done;
			if (file_status != UX_FAREA) /* we did not get a directory */
			{
				uu_uerror2(UB_SYMBOL, 49, libname, "ubu_view_dsfile");
				/* error is: Invalid file area specification: %s  (%s). */
					goto failed;
			}

 			if (uxu_view_dsfile("UB_LOC_M_SYMDIR",libname,symname) != UU_SUCCESS)
 				uu_uerror1(UB_SYMBOL,113,symname);
 				/* error mesage is "Unable to show description for symbol %s */
 		}
					
	}/* end no command reject */
	else /* command reject */
	{
 		/* if command reject, need to remove connamd file also ? */
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
**    E_FUNCTION : void symfst
**       Set the symbol search initialization
**    PARAMETERS   
**       INPUT  :none
**       OUTPUT :none
**    RETURNS   : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void symfst()
{
	Ssym_indx = 1;
	Sinst_indx = 1;
	Ssym_flag = 1;
}
/*********************************************************************
**    E_FUNCTION : void symnxt(symname,isub,key,type)
**       get the next symbol name from the unibase.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :
**			         symname  = Name of symbol.
**			         isub     = Subscript of symbol.
**			         key      = Key of symbol or instance.
**			         type     = 1 =  No Symbol nor instance exists.
**                           VSYMBOL=31: Symbol.
**                           VPLACE=32: Symbol instance.
**    RETURNS   : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void symnxt(symname,isub,key,type)
UM_f77_str_ptr symname;
UM_int4 *isub,*key;
UM_int2 *type;
{
	int nxtuple;
	int status,irel;
	int i;
	struct UB_symbol_rec symstruc;
	char *cstr;
	char *p1;
	cstr = UM_cstr_of_f77_str(symname);
	*type = 1;
	status = UU_SUCCESS;
	nxtuple = Ssym_indx;
start:;
	if (Ssym_flag==1)
	{
		irel = UB_SYMBOL_REL;
		nxtuple = Ssym_indx;
	}
	else
	{
		irel = UB_INSTANCE_REL;
		nxtuple = Sinst_indx;
	}
	if ((ur_get_next_data_key(irel, &nxtuple, &(symstruc.key)) == 0))
	{
		if (ur_retrieve_data_fixed(&symstruc) != UU_SUCCESS)
		{
			Ssym_flag++;
			goto start;
		}
		for (i=0,p1=symstruc.label;i<NCL_MAX_LABEL &&*p1!='\0'; i++)
			*cstr++ = *p1++;
		while (i<NCL_MAX_LABEL)
		{
			*cstr++ = ' ';
			i++;
		}
		*isub = symstruc.subscr;
		*key = symstruc.key;
		if (Ssym_flag==1)
		{
			*type = 31;
			Ssym_indx++;
		}
		else
		{
			*type = 32;
			Sinst_indx++;
		}
		goto done;
	}
	else
	{
		Ssym_flag++;
	}
	if (Ssym_flag==2)
		goto start;
done:;
}
