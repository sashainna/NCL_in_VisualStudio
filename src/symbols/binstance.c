/*********************************************************************
**       NAME:  binstance.c
**       CONTAINS:
**           void ub_simple_place(symname,pt,scal,rot,ierr)
**           ub_get_instance_origin()
**           int ubu_instance_sym() 
**           int ubu_make_instances(symptr, sym_inst_frm_outptr)
**           int ubu_fillin_disp_instance(symptr, tfmat, origin, 
**           int ubu_fillin_instance_rec(masterptr, dragging, 
**           int ub_get_instance_tf(origin, scale, angle, tfmat)
**           int ubi_get_instance_geom(masterptr, instanceptr) 
**           int ubi_get_instance_snodes(masterptr, instanceptr)
**           int ubu_get_instance_tnodes(masterptr, tnodeindx, instanceptr, 
**           int ubi_get_textnode_type(txtnodeptr, textreqptr, labelptr)
**           int ubi_setup_textnode(textnodrecptr, textptr, textattrptr)
**           int ubi_create_text_node(masterptr,textptr,textattrptr,
**           ubf_instance_namgen()
**           ubf_instance_label()
**			 ub_get_instance_master()
**           		tnodindx,instanceptr)
**       COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       binstance.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       12/01/15 , 08:08:41
*********************************************************************/
#include "usysdef.h"     /* for UU_REAL, etc. */
#include        "zsysdep.h"
#include "uhep.h"                /* for error system */
#include "udebug.h"       /* for debugging trace facility */
#include "dmark.h"              /* for UD_MARK and UD_UNMARK */
#include "dasnog.h"       /* for UD_RUBBER used in dragging */
#include "drubber.h"            /* for dragging macros */
#include "class.h"              /* for "UC_" data types */
#include "mdrel.h"              /* for define relation numbers */
#include "mdattr.h"       /* for UM_DISPLAYABLE */
#include "mattr.h"      /* for def. of UM_attrdata_rec */
#include "mcrv.h"                /* for UM_point_rec */
#include "mdcpln.h"       /* for construction plane */
#include "mdcoord.h"     /* for UM_zerovec */
#include "mdgenent.h"
#include "mfort.h"
#include "nclfc.h"
#include "nccs.h"
#include "xenv1.h"      /* for UX_PRTERRS */
#include "xfsys1.h"
#include "bsym.h"
#include "atext.h"
#include "nclcmd.h"
#include "nkeywd.h"
#include "nclinp.h"
#include "mdpick.h"
#include "dselmask.h"

/* #include "r1esched.h"  for associativity data types */

#define TRACE UU_FALSE  /* for debugging only */
/* char oldtext[UB_MAX_TEXT_LEN]; */
typedef char    NODETEXT[UA_TEXT_BUFSZ];
static NODETEXT                 *oldtext;
extern  ATXT_FRM        UB_txtattr;
static  int UB_instanceno;                      /* number of instances */
static  UU_REAL Btfmat[4][3];

static UU_LOGICAL Ssimple = UU_FALSE;
static UU_REAL Spt[3],Sscal,Srot;
static char Sname[UB_SYMBOL_NAME_LEN],Sinst_name[UB_SYMBOL_NAME_LEN];
static char Slib[256];
static int Ssub,Sinst_sub;
static UM_int2 Sifl35;

static void S_build_instance_name();

/*********************************************************************
**    E_FUNCTION : ub_simple_place(symname,sub,instname,instsub,pt,scal,rot,
**                                 ierr)
**       Place a symbol.  Fortran callable routine.
**    PARAMETERS   
**       INPUT  :
**         symname  = Name of symbol to place.
**         isub     = Subscript of symbol to place.
**         instname = Name of instance to create.  A name starting with
**                    '[' means that this routine will name the instance
**                    based on the symbol name and number of instances
**                    created.
**         instsub  = Subscript of instance to create.
**         symlib   = Library area which contains symbol.
**         pt       = Attach point.
**         scal     = Scale factor to apply to symbol.
**         rot      = Rotation of symbol in degrees.
**       OUTPUT :
**         instname = Name of created instance.
**         instsub  = Subscript of created instance.
**         ierr     = Returns non-zero on error.
**    RETURNS   : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ub_simple_place(symname,isub,instname,instsub,symlib,pt,scal,rot,ierr)
UM_f77_str_ptr symname,symlib,instname;
UM_int4 *isub,*instsub;
UM_real8 pt[];
UM_real8 *scal;
UM_real8 *rot;
UM_int2 *ierr;
{
	int i,stat;
	char *cstr,label[UB_SYMBOL_NAME_LEN_PLUS];
	UM_int2 idx;
	UU_LOGICAL found;
	struct UB_symbol_rec sym;
	int loaded, count, libstat;
	UX_pathname direc,file,cfname, fullname;
	char *list = NULL,*p,*strchr();
/*
.....Store arguments in global variables
*/
	cstr = UM_cstr_of_f77_str(symname);
	strncpy(Sname,cstr,UB_SYMBOL_NAME_LEN);
	i = UB_SYMBOL_NAME_LEN;
	ul_strip_blanks(Sname,&i);
	Ssub = *isub;

	cstr = UM_cstr_of_f77_str(instname);
	strncpy(Sinst_name,cstr,UB_SYMBOL_NAME_LEN);
	i = UB_SYMBOL_NAME_LEN;
	ul_strip_blanks(Sinst_name,&i);
	Sinst_sub = *instsub;

	cstr = UM_cstr_of_f77_str(symlib);
	strncpy(Slib,cstr,255);
	Slib[255] = '\0';
/*
....allow path (with space inside, so can use ul_strip_blanks
*/
/*	i = 20;
	ul_strip_blanks(Slib,&i);
*/
	for (i=255; i>0; i--)
	{
		if (Slib[i-1]==' ')
			Slib[i-1] = '\0';
		else
			break;
	}
	ul_to_lower(Slib);

	Sscal = *scal;
	UM_ang_exttoint(*rot,Srot);
	Spt[0] = pt[0] ; Spt[1] = pt[1] ; Spt[2] = pt[2];
	idx = 35;
	getifl(&idx,&Sifl35);
	*ierr = 1;
/*
.....Is the symbol already loaded
*/
	strcpy(sym.label,Sname);
	sym.subscr = Ssub;
	if (ub_get_symmaster_by_name(&sym, &found,0,1) != UU_SUCCESS)
		goto done;
/*
.....Master not found
.....Try to load it
*/
	if (!found)
	{
/*
........'PLACE/sym' is case insensitive
........Check all filenames in symbol directory
........to see if any match found
*/
		if (Slib[0]=='\0')
			strcpy(Slib, "symlib");
		count = 0;
		for (count=0;count<2;count++)
		{
			loaded = 0;
			if (count>=2) goto failed;
			if (count == 0 &&
				ubi_mk_libname("local",Slib,direc,&libstat,UX_NPRTERRS) !=
					 UU_SUCCESS) continue;
			else if (count == 1 &&
				ubi_mk_libname("system",Slib,direc,&libstat,UX_NPRTERRS) !=
		 			UU_SUCCESS) continue;
			strcpy(file, "sy");
			ul_get_flist(direc,file,&list,0,(UX_NPRTERRS|UX_NCHK));
			ncl_format_label(Sname,Ssub,label,0);
			ul_build_full_fname(direc, label,"sy",fullname);
			while (ux_nxt_file(&list,cfname,UX_NPRTERRS) == UU_SUCCESS)
			{
				if (ul_compare_upper(cfname,fullname)==0)
				{
					ul_break_fname(cfname,direc,file);
					p = strchr(file,'.'); if (p != UU_NULL) *p = '\0';
					if (ubi_load_file(UU_NULL,direc,file,UU_NULL,"WHOLESYM",
						&sym,UX_NPRTERRS) == UU_SUCCESS)
					{
						loaded = 1;
						break;
					}
				}
			}
			if (loaded==0)
			{
				if (list!=UU_NULL)
				{
					uu_lsdel(list);
					uu_toolmalloc_term();
					list = NULL;
				}
			}
			else break;
		}
	}
	if (i == 2) goto failed;
/*
.....Place the symbol
*/
	*ierr = 0;
	Ssimple = UU_TRUE;
	stat = ubu_instance_sym();
	Ssimple = UU_FALSE;
	if (stat != UU_SUCCESS) *ierr = 1;
	else
	{
		cstr = UM_cstr_of_f77_str(instname);
		strcpy(cstr,Sinst_name);
		for (i=strlen(cstr);i<UB_SYMBOL_NAME_LEN;i++) cstr[i] = ' ';
		*instsub = Sinst_sub;
	}
	goto done;
failed:;
	uu_uerror2(UB_SYMBOL, 25, Sname, "ub_simple_place");
	*ierr = 1;
done:;
	if (list!=UU_NULL)
	{
		uu_lsdel(list);
		uu_toolmalloc_term();
		list = NULL;
	}
	return;
}

/*********************************************************************
**    E_FUNCTION : ub_get_instance_origin(inst,origin)
**       Returns the origin of the symbol instance.
**    PARAMETERS   
**       INPUT  :
**			         inst     = Symbol instance.
**       OUTPUT :
**			         origin   = Origin of symbol instance.
**    RETURNS   : UU_FAILURE when origin could not be obtained.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ub_get_instance_origin(inst,origin)
struct UB_instance_rec *inst;
UU_REAL *origin;
{
	int i,status;
	UU_LOGICAL stilllooking;
	struct UB_symbol_rec sym;
/*
.....Get the master symbol
*/
	status = UU_FAILURE;
	if (ubi_get_master_key_for_inst(inst,&(sym.key)) == UU_SUCCESS)
	{
		if (ub_retrieve_sym(&sym,sizeof(sym)) == UU_SUCCESS)
		{
			i = 0; stilllooking = UU_TRUE;
			while (stilllooking && (i < sym.no_inst))
			{
				if (inst->key == sym.inst[i].inst_key)
					stilllooking = UU_FALSE;
				else
					i++;
			}
			if (stilllooking) goto done;
			um_vctovc(sym.inst[i].tfmat[3],origin);
/*			UM_cc_inttoext(sym.inst[i].tfmat[3],origin);*/
			status = UU_SUCCESS;
		}
	}
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**       E_FUNCTION : int ubu_instance_sym() 
**               This function creates instances of a symbol.
**       PARAMETERS     
**               INPUT  : none. 
**               OUTPUT : none.
**       RETURNS: UU_SUCCESS of no problems encountered, UB_FAILURE otherwise.
**       SIDE EFFECTS: none
**       WARNINGS: none
*********************************************************************/
int ubu_instance_sym( )
{
	struct UB_sym_instance_frm_rec /* instance forms and defaults */
		sym_instance_frm_out; 
	struct UB_symbol_rec sym_rec; /* storage for retrieval of master */
	UU_LOGICAL found; /* UU_TRUE iff the named symbol was found in UNIBASE */
	char fullibpath[UB_MAX_PATH_LEN];
	char fullpath[UB_MAX_PATH_LEN];
	char farea[UB_MAX_PATH_LEN];
	char fname[UB_MAX_PATH_LEN];
	char label[UB_SYMBOL_NAME_LEN_PLUS];
	struct UB_symbol_rec *msym2replace[1]; /* array of pointers */
	int version, libstat, cmdreject; /* command reject flag */
	UU_LOGICAL reload;
	UU_LOGICAL LOADEDFLG;
	int loaded;
	UX_pathname direc,file,cfname, fullname;
	struct UB_symbol_rec newMaster; /* use in reloading master */
	struct UB_symbol_rec *masterptr; /* ultimately this is the pointer to 
												 * the master that will be instanced */
	char *list = UU_NULL;
	int status = UU_SUCCESS;
	uu_denter(UU_BTRC,(us,"ubu_instance_sym()"));
	
	LOADEDFLG = UU_FALSE;
	/* strcpy(oldtext,""); */
	/* set mark for long jump if there is a command reject */
	if (Ssimple) cmdreject = UU_FALSE;
	else UD_MARK(cmdreject, UU_FALSE); 
	if (!cmdreject) /* then no command reject encountered, go on */
	{
		sym_instance_frm_out.name = UB_default_sym_name;
		sym_instance_frm_out.lib = UB_libdata_rec.default_lib;
		sym_instance_frm_out.area = UB_libdata_rec.default_area;
		sym_instance_frm_out.scale = UB_default_scale;
		sym_instance_frm_out.angle = UB_default_angle;
		sym_instance_frm_out.cmd = UB_default_cmd;

		if (Ssimple)
		{
			if (Ssub != 0) sprintf(label,"%s(%d)",Sname,Ssub);
			else strcpy(label,Sname);
			sym_instance_frm_out.name = label;;
			sym_instance_frm_out.lib = Slib;
			sym_instance_frm_out.scale = Sscal;
			sym_instance_frm_out.angle = Srot;
			sym_instance_frm_out.cmd = 0;
		}
		else
		{
			if (ubi_sym_instance_form("binstance.frm", &sym_instance_frm_out)
				!= UU_SUCCESS) goto failed;
		}
		/* form info ok, find master symbol requested */
/*
.....
.....Added by Paul to make new angle and scale values default. 09/25/92
.....FSR 51200.
.....
*/
	UB_default_scale = sym_instance_frm_out.scale;
	UB_default_angle = sym_instance_frm_out.angle;
	UB_default_cmd = sym_instance_frm_out.cmd;
/*****/

		masterptr = &sym_rec; /* get storage for the master to be instanced */
		ncl_parse_label(sym_instance_frm_out.name,masterptr->label,
			&masterptr->subscr);
/*
.....'Place/sym' now accept symbol name case insensitive
*/
		if (ub_get_symmaster_by_name(masterptr, &found, 0,1) != UU_SUCCESS)
			goto failed;
		if (found) /* check to make sure its the right one */
		{
/*
.....Ignore library area now that
.....we use a file browser to load symbol
.....Bobby  -  11/16/00
*/
			if (masterptr->version != UB_NOLIB && masterptr->version == UB_NOLIB)
			{
				if (ubi_reloadMasters(&reload) != UU_SUCCESS) goto failed;

				libstat = UB_NE; /* if the library does not exist print errors */
				if (ubi_mk_libname(sym_instance_frm_out.area,
					sym_instance_frm_out.lib,fullibpath,&libstat,UX_PRTERRS) 
					!= UU_SUCCESS) goto failed;
				if (ux_mk_chk_syspath(UU_NULL,fullibpath,sym_instance_frm_out.name,
					UU_NULL,"UB_SYM_SUFFIX",UU_NULL,fullpath,UX_PRTERRS) 
					!= UU_SUCCESS) goto failed;

				/* compare lib of sym in Unibase to lib of requested sym by
				comparing full pathnames */
				uu_dprint(UU_BTRC,(us,"loaded master path: %s, new path: %s",
								masterptr->path, fullpath));
				if (ux_decompose_path(masterptr->path, farea, fname, UX_PRTERRS)
					!= UU_SUCCESS)
					goto failed;
				if (strcmp(farea,fullibpath) != 0 && !Ssimple)
				{
					/* if same-named symbols but from different libraries,
					ask if should load into Unibase (overwriting the one in there) */
					/* if yes, then go through same code as if autoload was on */
					if (ud_yesno(0, uu_uprompt1(UB_SYMBOL,36,sym_instance_frm_out.name), "Question?"))
					/* Currently loaded master, %s, is from a different library, 
					 * overwrite? */
					{       /* then delete current master and load new one */
						msym2replace[0] = masterptr;
						if (ubu_load_and_update(sym_instance_frm_out.area,
							sym_instance_frm_out.lib, msym2replace, 1, "WHOLESYM", 
							&newMaster) != UU_SUCCESS) goto failed;
						masterptr = &newMaster;
						LOADEDFLG = UU_TRUE;
					}
					else goto done;
				}

				if (reload) /* then try to reload */
				{
					if ( (LOADEDFLG==UU_FALSE) && (strcmp(farea,fullibpath) != 0))
					{       /* then delete current master and load new one */
						msym2replace[0] = masterptr;
						if (ubu_load_and_update(sym_instance_frm_out.area,
							sym_instance_frm_out.lib, msym2replace, 1, "WHOLESYM", 
							&newMaster) != UU_SUCCESS) goto failed;
						masterptr = &newMaster;
					}
					else /* find out if we have a current version of this master */
					{
						if (ux_get_fversion(fullpath, &version, UX_PRTERRS) 
							!= UU_SUCCESS)
						{
							if (!Ssimple)
								uu_uerror3(UB_SYMBOL,78,sym_instance_frm_out.name,
									fullpath,"ubu_instance_sym");
							/* error is:Can't determine version of file for master
							symbol, * %s, at %s (%s). */
							goto failed;
						}
						if (masterptr->version != version) /* then reload */
						{       /* then delete current master and load new one */
							msym2replace[0] = masterptr;
							if (ubu_load_and_update(sym_instance_frm_out.area,
								sym_instance_frm_out.lib, msym2replace, 1, "WHOLESYM",
								&newMaster) != UU_SUCCESS) goto failed;
							masterptr = &newMaster;
						}
					}
				}/* end try to reload */
			}       /* end of if not a library-less master */
		}       /* end of if found */
		else /* master not found; try to load it */
		{
/*
.....'Place/sym' now accept symbol name case insensitive, so we need check 
.....all filenames in symbol directory to see if any match found
*/
			if (sym_instance_frm_out.lib[0]=='\0' || sym_instance_frm_out.lib[0]=='*')
				strcpy(sym_instance_frm_out.lib, "symlib");
			loaded = 0;
			if (masterptr->version != UB_NOLIB)
			{
				if (ubi_mk_libname(sym_instance_frm_out.area,sym_instance_frm_out.lib,direc,&libstat,UX_PRTERRS) != UU_SUCCESS)
					goto failed;
				strcpy(file, "sy");
				status = ul_get_flist(direc,file,&list,0,(UX_NPRTERRS|UX_NCHK));
				if (status==-1) goto failed;
				ul_build_full_fname(direc, sym_instance_frm_out.name,"sy",fullname);
				while (ux_nxt_file(&list,cfname,UX_NPRTERRS) == UU_SUCCESS)
				{
					if (ul_compare_upper(cfname,fullname)==0)
					{
						ul_break_fname(cfname,direc,file);
						if (ubi_load_file(UU_NULL,direc,
								file,UU_NULL,"WHOLESYM",masterptr,
								UX_NPRTERRS) 
								== UU_SUCCESS)
						{
							loaded = 1;
							break;
						}
					}
				}
				if (loaded==0) 
				{
					uu_uerror2(UB_SYMBOL, 25, sym_instance_frm_out.name, "ub_simple_place");
					goto failed;
				}
			}
		}
#if (TRACE)
		ub_print_sym(masterptr);
#endif
		/* now create and display instances */
		if (ubu_make_instances(masterptr, &sym_instance_frm_out) != UU_SUCCESS)
			goto failed;

	}/* end no cmdreject */
	else /* command reject hit */
		goto done;

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	if (list!=UU_NULL)
	{
		uu_lsdel(list);
		uu_toolmalloc_term();
	}
	if (!Ssimple) UD_UNMARK(cmdreject);     /* take mark off long jump stack */
	uu_dexit;
	return(status);
}  

/*********************************************************************
**       E_FUNCTION : int ubu_make_instances(symptr, sym_inst_frm_outptr)
**               This function prompts the user for locations to place an instance
**               of a master symbol, creates and displays the instances.  Note,
**               we assume master symbols have only identity transformations 
**               associated with them; so they are ignored.
**       PARAMETERS     
**               INPUT  : 
**                      symptr          Pointer to the master symbol record to instance.
**                      sym_inst_frm_outptr     Pointer to symbol instance orientation
**                                                                              info.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems encoutered, UB_FAILURE otherwise.
**       SIDE EFFECTS: displays instances and puts them in UNIBASE.
**       WARNINGS: none
*********************************************************************/
int ubu_make_instances(symptr, sym_inst_frm_outptr)
	struct UB_symbol_rec *symptr;/* pointer to the master symbol */
	struct UB_sym_instance_frm_rec *sym_inst_frm_outptr;
{
	UU_REAL tfmat[4][3]; /* transformation for positioning, orienting, and
								 * scaling an instance of a symbol. */
	UU_LOGICAL more_instances;
	UD_NDCLOCREC norg;
	UM_coord origin;
	struct UB_inst_rec instdata;
	UU_KEY_ID dragkey;      /* key of instance being dragging */
	UD_RUBBER draginfo;  /* data structure for dragging */
	UU_LOGICAL dragon;      /* UU_TRUE iff we are dragging a segment */
	int i, dsegid;
	int cmdreject;           /* command reject flag */
	int numint, status = UU_SUCCESS;
	NCL_cmdbuf cmdbuf;
	char sbuf[80],ptlab[64],label[UB_SYMBOL_NAME_LEN_PLUS];
	UU_REAL ang;
	UM_PLOCREC pick;

	uu_denter(UU_BTRC,(us,"ubu_make_instances(symptr:%x,sym_inst_frm_outptr:%x)",
				symptr, sym_inst_frm_outptr));
	/* set mark for long jump if there is a command reject */
	if (Ssimple) cmdreject = UU_FALSE;
	else UD_MARK(cmdreject, UU_FALSE); 
	if (!cmdreject) /* then no command reject encountered, go on */
	{
		/* get transform to orient symbol instance */
		if (ub_get_instance_tf(UM_zerovec, sym_inst_frm_outptr->scale,
				sym_inst_frm_outptr->angle, tfmat) != UU_SUCCESS)  goto failed;

		/* create a symbol instance to DRAG and set it up to drag */
		if (!Ssimple)
		{
			if (ubu_drag_syminstance(symptr, tfmat, &draginfo, &dragkey, 
							&dragon, origin, &dsegid) != UU_SUCCESS) goto failed;
			if (dragon)
			{
			/* now set up to drag instance */
				DRAG_GRAPHICS((&draginfo), dsegid, origin, 1); 
				/*ud_stdrag(draginfo, dsegid, view.cur_ref_pt, vport.xform);*/ 
	
				/* allow dragging of segment */
				DRAG_ON(&(draginfo)); 
				/*ud_ondrag(draginfo);*/ 
			}
		}
		UB_instanceno = 0;
		more_instances = UU_TRUE;
		if (symptr->no_text_nod > 0)
		{
			oldtext = (NODETEXT *)uu_malloc(symptr->no_text_nod*sizeof(NODETEXT));
			for (i=0; i<symptr->no_text_nod;i++)
				oldtext[i][0] = '\0';
		}
		while (more_instances)
		{
			if (Ssimple)
			{
				origin[0] = Spt[0];
				origin[1] = Spt[1];
				origin[2] = Spt[2];
				more_instances = UU_FALSE;
			}
			else
			{
				if (sym_inst_frm_outptr->cmd == 1)
				{
					ud_lgeo(UU_TRUE,UD_ncl_ptpv);
					um_dl_pldas(UD_DASPCKLOC, UB_SYMBOL, 11, &pick, 1, &numint, 1);
					strcpy(ptlab,pick.ploc.label);
				}
				else
				{
					ud_ldas(UD_DASCART, UB_SYMBOL, 11, &norg, 1, &numint,
						UD_NODEFAULT);
					origin[0] = norg.cord[0];
					origin[1] = norg.cord[1];
					origin[2] = norg.cord[2];
				}
				/* message is: Enter location of instance origin. */
				if (numint <= 0) /* then done */
				{
					more_instances = UU_FALSE;
					break;
				}
			}
			/* fill in the rest of the data packet about instance placement */
if (sym_inst_frm_outptr->cmd != 1)
{
}
		   UB_instanceno++;
/*
.....Build and output PLACE/sym command
*/
			if (sym_inst_frm_outptr->cmd == 1)
			{
				ncl_init_cmdbuf(&cmdbuf);
				ncl_add_token(&cmdbuf,NCL_place,NCL_nocomma);
				ncl_get_label(symptr,label);
				ncl_add_token(&cmdbuf,label,NCL_comma);
				ncl_add_token(&cmdbuf,NCL_at,NCL_comma);
				ncl_add_token(&cmdbuf,ptlab,NCL_comma);
				if (sym_inst_frm_outptr->scale != 1.)
				{
					ncl_add_token(&cmdbuf,NCL_scale1,NCL_comma);
					ncl_sprintf(sbuf,&sym_inst_frm_outptr->scale,1);
					ncl_add_token(&cmdbuf,sbuf,NCL_comma);
				}
				if (sym_inst_frm_outptr->angle != 0.)
				{
					ncl_add_token(&cmdbuf,NCL_rotate,NCL_comma);
					UM_ang_inttoext(sym_inst_frm_outptr->angle,ang);
					ncl_sprintf(sbuf,&ang,1);
					ncl_add_token(&cmdbuf,sbuf,NCL_nocomma);
				}
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
				UB_default_cmd = sym_inst_frm_outptr->cmd = 1;
			}
/*
.....Place the symbol
*/
			else
			{
				if (ubu_fillin_disp_instance(symptr,tfmat,origin,UU_FALSE,&instdata)
						!= UU_SUCCESS) goto failed;
			}
		}/* end moreinstances while */

		if (symptr->no_text_nod > 0)
			uu_free(oldtext);

	}/* end no command reject */
	else /* command reject hit */
	{ }

	if (!Ssimple)
	{
		if (dragon) /* get rid of dragging segment */
		{
			/* delete dragging instance */
			/* if (ub_del_syminstance(dragkey) != UU_SUCCESS) goto failed; */
			if (ur_delete_all(dragkey) != UU_SUCCESS) goto failed;
			DRAG_OFF(&draginfo);  
		}

	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	if (!Ssimple) UD_UNMARK(cmdreject);     /* take mark off long jump stack */
	uu_dexit;
	return(status);
}  

/*********************************************************************
**       E_FUNCTION : int ubu_fillin_disp_instance(symptr, tfmat, origin, 
**                                                                                                                      dragging, instdataptr)
**       This function creates and displays an instance of a master symbol.
**       Note, we assume master symbols have only identity transformations 
**       associated with them; so they are ignored.
**       PARAMETERS     
**               INPUT  : 
**                      symptr          Pointer to the master symbol record to instance.
**                      tfmat                   Transformation to orient instances; note, the
**                                                      translation portion of the transformation will
**                                                      be changed according to the placing of each
**                                                      instance, it should have no translation component.
**                      origin          Symbol instance origin.
**                      dragging                UU_TRUE iff the instance created is the dragging instance.
**               OUTPUT : 
**                       instdataptr    Pointer to the instance data packet to be put in the 
**                                                              master symbol.
**       RETURNS: UU_SUCCESS if no problems encoutered, UB_FAILURE otherwise.
**       SIDE EFFECTS: displays instances and puts them in UNIBASE.
**       WARNINGS: none
*********************************************************************/
int ubu_fillin_disp_instance(symptr, tfmat, origin, dragging, instdataptr)
	struct UB_symbol_rec *symptr;/* pointer to the master symbol */
	UU_REAL tfmat[4][3]; /* transformation for positioning, orienting, and
								 * scaling an instance of a symbol. */
	UM_coord origin;
	UU_LOGICAL dragging;
	struct UB_inst_rec *instdataptr;
{
	struct UB_instance_rec instance_rec;
	UU_LOGICAL notify;
	UU_KEY_ID key,tkey;
	UU_REAL instancetf[4][3];/* final transformation for instance. */
	UU_LOGICAL cmdreject; /* UU_TRUE iff there is a command reject */
	int status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,
	"ubu_fillin_disp_instance(symptr:%x,tf:%x,orig:<%g,%g,%g>,dragging:%d,?)"
				,symptr, tfmat, origin[0], origin[1], origin[2], dragging));

	/* get transformation associated with instance location */
	um_disptf(origin, instancetf);

	/* get final transformation for instance */
	um_tftmtf(tfmat, instancetf, instancetf);
 
	/* save transformation used to orient the symbol instance */
	um_tftotf(instancetf, instdataptr->tfmat);
	um_tftotf(instancetf, Btfmat);

	/* get a copy of the master symbol into the instance record */
	if (ubu_fillin_instance_rec(symptr,dragging, &instance_rec, 
				&cmdreject) != UU_SUCCESS) goto failed;

	if (cmdreject) /* a command reject was hit, so quit */
		goto done;

	if (dragging) key = 0;
	else key = symptr->key;
	if (ub_create_instance_tuple(key,&instance_rec, UB_DEFAULT_TF, 
				&UB_symattr_default_bundle, instdataptr) != UU_SUCCESS) goto failed;
	if (dragging)
	{
		ur_update_blanked(instance_rec.key, UU_TRUE);
	}

	notify = UU_FALSE;
	if (ur1_notifyAssocOfUpdates(instance_rec.key, &notify) != UU_SUCCESS)
			goto failed;
	/* transform symbol instance to reflect this transformation */
	if (ub_transform_sym(&instance_rec, instancetf, UU_TRUE) != UU_SUCCESS)  
		goto failed;
	if (ur1_notifyAssocOfUpdates(instance_rec.key, &notify) != UU_SUCCESS)
			goto failed;

#if (TRACE)
	sprintf(UB_sbuf,"from ubu_fillin_disp_instance, instance_rec.key=%d",
							instance_rec.key);
	ubi_pscroll(UB_sbuf);
	ubi_pscroll("after tuple instance creation:");
	if (ub_print_sym(&instance_rec) != UU_SUCCESS)
			goto failed;
	sprintf(UB_sbuf," origin of instance:<%g,%g,%g>",
					origin[0], origin[1], origin[2]);
	ubi_pscroll(UB_sbuf);
	sprintf(UB_sbuf," instance tf:	%g, %g, %g",
		instancetf[0][0], instancetf[0][1], instancetf[0][2]);
	ubi_pscroll(UB_sbuf);
	sprintf(UB_sbuf,"			 %g, %g, %g",
		instancetf[1][0], instancetf[1][1], instancetf[1][2]);
	ubi_pscroll(UB_sbuf);
	sprintf(UB_sbuf,"			 %g, %g, %g",
		instancetf[2][0], instancetf[2][1], instancetf[2][2]);
	ubi_pscroll(UB_sbuf);

#endif
	/* display the symbol instance */
	if (!Ssimple || Sifl35 == 0)
	{
		if (ub_display_sym(&instance_rec) != UU_SUCCESS)
			goto failed;
	}

	instdataptr->inst_key = instance_rec.key;       /* return key of new instance */
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	ur_free_app_data(&instance_rec);                /* return varlist space */
	uu_dexit;
	return(status);
}  

/*********************************************************************
**       E_FUNCTION :int ubu_fillin_instance_rec(masterptr, dragging, 
**                                                      instanceptr, cmdrejectptr)
**               This function fills-in the subentities of a symbol instance
**               from the master symbol pointed to by "masterptr".
**               Note, we assume ALL SUBENTITIES OF A MASTER SYMBOL HAVE IDENTITY
**               TRANSFORMATIONS.
**       PARAMETERS     
**               INPUT  : 
**                       masterptr              pointer to the master symbol.
**                       dragging               UU_TRUE iff the instance filled in is to be dragged.
**                       instanceptr    Pointer to an instance record to be filled-in.
**               OUTPUT :  
**                       instanceptr    Pointer to the filled-in instance record.
**                       cmdrejectptr   Pointer to logical which is UU_TRUE iff a command
**                                                              was hit while in this function.
**       RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**       SIDE EFFECTS: none
**       WARNINGS: none
*********************************************************************/
int ubu_fillin_instance_rec(masterptr,dragging,instanceptr,cmdrejectptr)
	struct UB_symbol_rec *masterptr;
	UU_LOGICAL dragging;
	struct UB_instance_rec *instanceptr;
	UU_LOGICAL *cmdrejectptr;
{
	int nseg,svsub;
	int i, status = UU_SUCCESS;
	UU_LOGICAL found;
	char svname[UB_SYMBOL_NAME_LEN];
	struct UM_srfdatabag ent;
	struct UC_attributedatabag subent_attr;
	UU_REAL etfmat[4][3];

	uu_denter(UU_BTRC,(us,
	"ubu_fillin_instance_rec(mastr>key:%d,draging:%d,instptr:%x,?)",
		masterptr->key, dragging, instanceptr));

	*cmdrejectptr = UU_FALSE;  /* no command reject hit */

	/* setup instance record; note, "no_???" fields are zeroed */
	/* must have this macro on one line for the MASSCOMP */
	UB_SETUP_DATA(UB_INSTANCE_REL, instanceptr, sizeof(struct UB_instance_rec), status);

/*
.....Build instance name
*/
	S_build_instance_name(masterptr,instanceptr,dragging);
/*
.....Creating simple placement
.....Delete existing one if it already exists
*/
	if (Ssimple)
	{
		strcpy(svname,instanceptr->label);
		svsub = instanceptr->subscr;
		if (ub_get_symmaster_by_name(instanceptr,&found,1,2) == UU_SUCCESS)
		{
			if (found) uc_delete(instanceptr->key);
		}
		strcpy(instanceptr->label,svname);
		instanceptr->subscr = svsub;
	}

	instanceptr->key = -1; /* for debugging only */
	if (status != UU_SUCCESS) 
		goto failed;
	/* instanceptr->master_key = masterptr->key; */
	if (ubi_get_instance_geom(masterptr, instanceptr) != UU_SUCCESS)
		goto failed;

	if (!dragging) /* fillin the rest of the symbol instance */
	{
		/* display the geometry in the working viewport as well as fillin the
		 * instance record. The display here is temporary so that multi-line
		 * text nodes can be viewed in relation to the geometry. */
		nseg = gnseg();
		if (Ssimple && Sifl35 == 0)
		{
			gcreateseg(nseg);
			for (i=0; i<instanceptr->no_geom; i++) 
			{
				ent.key = instanceptr->geom[i];
				if (uc_retrieve_data(&ent, sizeof(struct UM_srfdatabag))
					!= UU_SUCCESS)  goto failed;
				/* orient "ent" in instance space */
				if (ncl_tst_un_label(&ent)) strcpy(ent.label,"@UNK");
				if (uc_retrieve_transf(ent.key, etfmat) != UU_SUCCESS)
					goto failed;
				/* get display attributes */
				if (uc_retrieve_attr(ent.key, &subent_attr) != UU_SUCCESS)
					goto failed;
				um_tftmtf(etfmat, Btfmat, etfmat);
				/* set pick id for subentities and draw symbol subentity */
				if (uc_draw(&ent, etfmat, &subent_attr) != UU_SUCCESS)
					goto failed;
			}/* end drawing geometry */
			gcloseseg();
		}

		if (ubi_get_instance_snodes(masterptr, instanceptr) != UU_SUCCESS)
			goto failed;
		if (ubu_get_instance_tnodes(masterptr,0,instanceptr,cmdrejectptr) 
			!= UU_SUCCESS) goto failed;
		if (Ssimple && Sifl35 == 0)
			gdeleteseg(nseg);

		if (*cmdrejectptr) 
		{ /* then command reject hit delete all subentities created for instance*/
			for (i=0; i<instanceptr->no_geom; i++)
				if (uc_delete(instanceptr->geom[i]) != UU_SUCCESS)
					goto failed;
			for (i=0; i<instanceptr->no_snap_nod; i++)
				if (uc_delete(instanceptr->snap_nod[i].snap_key) != UU_SUCCESS)
					goto failed;
			for (i=0; i<instanceptr->no_text_nod; i++)
				if (uc_delete(instanceptr->text_nod[i].text_key) != UU_SUCCESS)
					goto failed;
			instanceptr->no_geom = 0;
			instanceptr->no_snap_nod = 0;
			instanceptr->no_text_nod = 0;
		}
#if (TRACE)
		ubi_pscroll("next instance: (not dragging)");
		if (ub_print_sym(instanceptr) != UU_SUCCESS)
			goto failed;
#endif
	}/* end not dragging */
	else /* we are dragging, so put a marker where the origin is */
	{
		/* make the origin marker a geometry entity, not a snap node */
		struct UM_point_rec pt;
		struct UM_attrdata_rec attr;
		/* remove struct UB_snap_nod_rec snapnod; */

		ur_setup_data(UM_POINT_REL, &pt, sizeof(pt));
		pt.markertype = 4; /* This is a circle */
		pt.snap_node = UU_FALSE; /* this point is a NOT snap node */
		/* now, put the origin marker at the origin in the master symbol's
		 * coordinate space. */
		um_vctovc(UM_zerovec, pt.pt); 
		if (um_current_default_attr(UM_POINT_REL, &attr) != UU_SUCCESS)
			goto failed;
		/* create origin point */
		if (um_create_geom(&pt, UM_DEFAULT_TF, &attr) != 0)
			goto failed;
			/* snapnod.snap_key = pt.key; snapnod.nbr = 0; */
		/* if (ubi_update_app_varlist(instanceptr,UB_ISNAP_LIST,&snapnod,1,1) */
		if (ubi_update_app_varlist(instanceptr,UB_IGEOM_LIST,&pt.key,
			(masterptr->no_geom)+1,1) != UU_SUCCESS) goto failed;
		instanceptr->no_snap_nod = 0;           /* no snap nodes */
		instanceptr->no_text_nod = 0;
#if (TRACE)
		ubi_pscroll("instance to be dragged");
		if (ub_print_sym(instanceptr) != UU_SUCCESS)
			goto failed;
#endif
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}  

/*********************************************************************
**       E_FUNCTION :int ub_get_instance_tf(origin, scale, angle, tfmat)
**                       This function calculates the transformation to orient a
**               symbol instance.
**       PARAMETERS     
**               INPUT  : 
**               origin                 origin of the instance.
**               scale                   scale factor for the instance.
**               angle                   rotation angle about construction plane normal.
**               tfmat                   storage for transformation to be returned.
**               OUTPUT :  
**               tfmat                   transformation to orient instance.
**       RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**       SIDE EFFECTS: none
**       WARNINGS: none
*********************************************************************/
int ub_get_instance_tf(origin, scale, angle, tfmat)
	UU_REAL origin[3];
	UU_REAL scale;
	UU_REAL angle;
	UU_REAL tfmat[4][3];
{
	UU_REAL temptf[4][3];
	UU_REAL tempvec[3];
	int i;
	int status;

	uu_denter(UU_BTRC, (us,"ub_get_instance_tf(%x,scale:%g,angle:%g,%x)",
						origin, scale, angle, tfmat));
	status = UU_SUCCESS; /* assume success */

	/* get transformation that will orient the symbol with respect to the
	 * origin given and the construction plane axises in model coordinates.
	 */
		if (ubi_get_tf_for_new_basis(UB_CPLANE_TO_MODEL, origin, tfmat) 
			!= UU_SUCCESS) goto failed;

	/* if necessary, get rotation transformation for instance. */
	if (angle != 0.0)
	{
		um_rottf(UM_cpln.zaxis, angle, temptf);
		um_tftmtf(tfmat, temptf, tfmat);
	}
	/* concatenate "tfmat" with a scale factor so that the instance is
	 * of the appropriate size.
	 */ 
	for (i=0; i<3; i++)  /* uniformly scale */
		tempvec[i] = scale;
	/* get the scaling transform */
	um_scaletf(tempvec, temptf);
	um_tftmtf(tfmat, temptf, tfmat);
	/* reset last row to make translation correct */
	um_vctovc(origin, tfmat[3]);

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}  

/*********************************************************************
**       I_FUNCTION : int ubi_get_instance_geom(masterptr, instanceptr) 
**               This function puts a copy of the geometry for master symbol
**               into a symbol instance.
**       PARAMETERS     
**               INPUT  : 
**                       masterptr              Pointer to the master symbol to have it's geometry
**                                                               copied.
**                       instanceptr     Pointer to the symbol instance which is to get
**                                                               copies of the master symbol geometry.
**               OUTPUT :  
**                       instanceptr     Pointer to the symbol instance containing the new
**                                                               keys to the newly created geometry.
**       RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**       SIDE EFFECTS: Creates copies of the master symbol geometry in UNIBASE.
**       WARNINGS: none
*********************************************************************/
int ubi_get_instance_geom(masterptr, instanceptr)
	struct UB_symbol_rec *masterptr;
	struct UB_instance_rec *instanceptr;
{
	struct UC_entitydatabag ent;
	struct UC_entitydatabag *copyptr;
	int i, status = UU_SUCCESS;
	UM_int2 i2flg;
	struct UC_attributedatabag attr;

	uu_denter(UU_BTRC, (us,
			"ubi_get_instance_geom(masterptr->key:%d, instanceptr:%x)",
									masterptr->key, instanceptr));

	/* copy geometry and put keys into instance record */
	copyptr = (struct UC_entitydatabag *) 
					uu_malloc(sizeof(struct UC_entitydatabag));
	i2flg = 1;
	stunlb(&i2flg);
	for (i=0; i<masterptr->no_geom; i++)
	{
		ent.key = masterptr->geom[i];
		if (uc_retrieve_data(&ent, sizeof(ent)) != UU_SUCCESS)  
			goto failed;
		if (uc_copy(&ent, copyptr, sizeof(struct UC_entitydatabag)) 
						!= UU_SUCCESS) goto failed;
/*
.....make symbol entity label off
*/
		attr.key = copyptr->key;
		status = ur_retrieve_attr(&attr);
		if (status == 0)
		{
/*
.....the label_on may not be initial
.....just set label_on to 0
*/
/*			attr.label_on = ncl_label_off(attr.label_on); */
			attr.label_on = 0;
			ur_update_attr(&attr);
		}
		if (ubi_update_app_varlist(instanceptr,UB_IGEOM_LIST,copyptr,i+1,1)
				!= UU_SUCCESS) goto failed;

#if (TRACE)
		ubi_pscroll("INSTANCE GEOMETRY SUBENTITY MADE");
		if (uc_print(copyptr) != UU_SUCCESS)
		{
			sprintf(UB_sbuf,"CAN'T PRINT SYMBOL SUBGEOMETRY, KEY:%d, REL_NUM:%d",
						ent.key, ent.rel_num);
			ubi_pscroll(UB_sbuf);
		}
#endif
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_free(copyptr);       /* return the copy buffer */
	stunlb(&i2flg);
	uu_dexit;
	return(status);
}  

/*********************************************************************
**       E_FUNCTION : int ubi_get_instance_snodes(masterptr, instanceptr)
**               This function puts a copy of the snap nodes for master symbol
**               into a symbol instance.
**       PARAMETERS     
**               INPUT  : 
**                       masterptr              Pointer to the master symbol to have it's snap nodes
**                                                               copied.
**                       instanceptr     Pointer to the symbol instance which is to get
**                                                               copies of the master symbol snap nodes.
**               OUTPUT :  
**                       instanceptr     Pointer to the symbol instance containing the new
**                                                               keys to the newly created snap nodes.
**       RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**       SIDE EFFECTS: Creates copies of the master symbol snap nodes in UNIBASE.
**       WARNINGS: none
*********************************************************************/
int ubi_get_instance_snodes(masterptr, instanceptr)
	struct UB_symbol_rec *masterptr;
	struct UB_instance_rec *instanceptr;
{
	struct UM_point_rec ent;
	struct UM_attrdata_rec attr;
	struct UB_snap_nod_rec snapnod;
	int i;
	int status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,"ubi_get_instance_snodes(masterkey:%d,instanceptr:%x)",
										masterptr->key, instanceptr));

	/* copy snap nodes into instance record */
	for (i=0; i<masterptr->no_snap_nod; i++)
	{
		ent.key = masterptr->snap_nod[i].snap_key;
		if (uc_retrieve_data(&ent, sizeof(ent)) != UU_SUCCESS) 
			goto failed;
		if (um_current_default_attr(UM_POINT_REL, &attr) != UU_SUCCESS) 
			goto failed;
		/*
		attr.color = UB_SNAP_NODE_COLOR;
		*/
		/* note, we assume each snap node has the default transform
		 * so we ignore it. */
		if (um_create_geom(&ent, UM_DEFAULT_TF, &attr) != UU_SUCCESS)
				goto failed;
		snapnod.snap_key = ent.key;
		snapnod.nbr = i;
		if (ubi_update_app_varlist(instanceptr,UB_ISNAP_LIST,&snapnod,i+1,1)
				!= UU_SUCCESS) goto failed;
#if (TRACE)
		{
			int k;
			ubi_pscroll("INSTANCE SNAP NODE MADE");
			k = instanceptr->no_snap_nod;
			sprintf(UB_sbuf,
			"instance snap node, %d, from app varlist: key:%d, nbr:%d", k,
				instanceptr->snap_nod[k-1].snap_key,instanceptr->snap_nod[k-1].nbr);
			ubi_pscroll(UB_sbuf);
			if (uc_print(&ent) != UU_SUCCESS)
			{
				sprintf(UB_sbuf,"CAN'T PRINT SYMBOL SNAPNODE, KEY:%d, REL_NUM:%d",
						ent.key, ent.rel_num);
				ubi_pscroll(UB_sbuf);
			}
		}
#endif
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}  

/*********************************************************************
** E_FUNCTION : int ubu_get_instance_tnodes(masterptr, tnodeindx, instanceptr, 
**                                                                                              cmdrejectpr)
**               This function puts a copy of the text nodes for a master symbol
**               into a symbol instance.
**       PARAMETERS     
**               INPUT  : 
**                      masterptr               Pointer to the master symbol to have it's text nodes
**                                                              copied.
**                      tnodeindx               Index of the text node of the master to start with 
**                                                              in filling in the instance's text nodes.
**                      instanceptr             Pointer to the symbol instance which is to get
**                                                              copies of the master symbol text nodes.
**               OUTPUT :  
**                      instanceptr             Pointer to the symbol instance containing the new
**                                                              keys to the newly created text nodes.
**                      cmdrejectptr    Pointer to a logical which UU_TRUE iff a command
**                                                              reject was hit in this function.
**       RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**       SIDE EFFECTS: Creates copies of the master symbol text nodes in UNIBASE.
**       WARNINGS: none
*********************************************************************/
int ubu_get_instance_tnodes(masterptr, tnodeindx, instanceptr, cmdrejectptr)
	struct UB_symbol_rec *masterptr;
	int tnodeindx;
	struct UB_instance_rec *instanceptr;
	UU_LOGICAL *cmdrejectptr;
{
	struct UA_txt_rec text, tmptext;
	struct UA_txtattr_rec textattr;
	UU_LOGICAL textreq;
	UU_LOGICAL label;
	char prompt[UB_MAX_TEXT_LEN];
/*	char tempstr[UB_MAX_TEXT_LEN]; */
	int i, len;
/*	UU_LOGICAL foundblank; */
/*	int nbrblanks; */
	int     num_charno;
	int     dsegid;
	int status = UU_SUCCESS;/* return status; either UU_SUCCESS or UB_FAILURE */
	/* add types for use by das ud_strdef */
	/* UD_STRREC retstr;                                              */

	uu_denter(UU_BTRC,(us,
		"ubu_get_instance_tnodes(masterkey:%d,tnodeindx:%d,instanceptr:%x,?)",
									masterptr->key, tnodeindx,instanceptr));
	/* set mark for long jump if there is a command reject */
	if (Ssimple) *cmdrejectptr = UU_FALSE;
	else UD_MARK(*cmdrejectptr, UU_FALSE);  
	if (!*cmdrejectptr)  /* then no command reject encountered, go on */
	{
		/* copy text nodes into instance record */
		ur_setup_data(UA_TEXTATTR_REL, &textattr, sizeof(textattr));
		for (i=tnodeindx; i<masterptr->no_text_nod; i++)
		{
			if (ubi_setup_textnode(&(masterptr->text_nod[i]), &text, &textattr)
						!= UU_SUCCESS) goto failed;
			if (ubi_get_textnode_type(&(masterptr->text_nod[i]), &textreq, &label)
						!= UU_SUCCESS) goto failed;
			/* textattr.displayable = UM_DISPLAYABLE;        */
			if (!label || Ssimple)          /* then try to get text from user */
			  {
				do                      /* prompt user for input text */
				{
				 if ((UB_instanceno>0)&&(UB_text_default))      /* Let the user edit text */
					{
					 ud_wrprm(text.tchar);          /* put out the prompt first */
					 strcpy(text.tchar,(oldtext+i));                /* get the previous text */
					 text.no_tchar = strlen(text.tchar) + 1;
					 textattr.displayable = UM_DISPLAYABLE; 
					 zbytecp(tmptext,text);
					 uc_create_data(&tmptext,UM_DEFAULT_TF,&textattr);
					 uc_transform(&tmptext,Btfmat,UU_TRUE);
					 ua_edit_txt(&tmptext);
					 /* in ua_edit_txt a text segment will be created which is extra
						 for symbols */
					 ur_retrieve_disp_segid(tmptext.key, &dsegid);
					 if (dsegid >= 0)
						 uv_delsegs(dsegid);
					 ur_delete_all(tmptext.key);
					}
				 else  /* first instance or no default text required */
					{
					 strcpy(prompt,text.tchar);
					 ua_get_prompt_text(prompt,text.tchar,&num_charno);
					 text.no_tchar = strlen(text.tchar) + 1;
					 /*---
					 uc_create_data(&text,UM_DEFAULT_TF,&textattr);
					 uc_transform(&text,Btfmat,UU_TRUE);
					 uc_display(&text);
					 ---*/
					}
				 len = strlen(text.tchar);

/**------------New Text---------------------------------------------**/
					/* changed from UD_DASSTRING to UD_DASSTRINGDEF and from
					text.tchar ( a char * ) to struct retst */
					/*ud_ddas(UD_DASSTRINGDEF, prompt, &retstr, UB_MAX_TEXT_LEN-1, 
							&numint, UD_DEFAULT);
				
					strcpy(text.tchar, retstr.instring); */
					/* note, if command reject is hit here we must delete the 
					 * geometry and snap nodes associated with this instance */

					/* check to see if we got all blanks */
					/*len = strlen(text.tchar);
					strcpy(tempstr, text.tchar);
					for (k=0,foundblank=UU_TRUE,nbrblanks=0;(k<len && foundblank); 
								k++)
						if (tempstr[k] == ' ')
							nbrblanks++;
						else
							foundblank = UU_FALSE;
					if (nbrblanks == len)    then had all blanks */
				 /*     len = 0;                                                                        
					text.no_tchar = len + 1; */
/**-----------------------------------------------------------------------**/
				} while  ((textreq) && (len == 0));

						/* store the text string for next time through */
				strcpy((oldtext+i), text.tchar);
			}
		else                    /* is a lable, just copy it */
			{
			 text.no_tchar = strlen(text.tchar) + 1;
			 /*---
			 uc_create_data(&text,UM_DEFAULT_TF,&textattr);
			 uc_transform(&text,Btfmat,UU_TRUE);
			 uc_display(&text);
			 ---*/
		   }

		if (text.no_tchar > 0) /* user gave us a text string */
		  {
			if (ubi_create_text_node(masterptr,&text, &textattr, i, instanceptr)
							!= UU_SUCCESS) goto failed;
#if (TRACE)
				ubi_pscroll("INSTANCE TEXT NODE MADE");
				if (uc_print(&text) != UU_SUCCESS)
				  {
					sprintf(UB_sbuf,"CAN'T PRINT SYMBOL TEXT, KEY:%d, REL_NUM:%d",
								text.key, text.rel_num);
					ubi_pscroll(UB_sbuf);
				  }
#endif
		     } /* end text len>0 */

		}/* end for each text node */
	}/* end no command reject */

	goto done;
failed: status = UB_FAILURE;
#if (TRACE) 
	UB_IF_FAILURE_PRINT_IT
#endif
done:;
	if (!Ssimple) UD_UNMARK(*cmdrejectptr);
	uu_dexit;
	return(status);
}  
/*********************************************************************
**    I_FUNCTION : int ubi_get_textnode_type(txtnodeptr, textreqptr, labelptr)
**       This function determines whether the function is a label and if not
**                      whether the text to input by the user is required.
**    PARAMETERS   
**       INPUT  : 
**          txtnodeptr          Pointer to the text node record giving the type 
**                                                                      of text.
**       OUTPUT :  
**          textreqptr          Pointer to UU_TRUE or UU_FALSE; UU_TRUE iff
**                                                                      text is required.
**                              labelptr                        Pointer to UU_TRUE or UU_FALSE; UU_TRUE iff
**                                                                      the text node is a label.
**    RETURNS: UU_SUCCESS if no problems encountered; otherwise UB_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubi_get_textnode_type(txtnodeptr, textreqptr, labelptr)
	struct UB_text_nod_rec *txtnodeptr;
	UU_LOGICAL *textreqptr;
	UU_LOGICAL *labelptr;
{
	int status = UU_SUCCESS;/* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_BTRC,(us,"ubi_get_textnode_type(txtnodeptr,?,?)"));

	/* determine the kind of text */
	switch (txtnodeptr->prompt) 
	{
		case 0: /* text optional */
			*textreqptr = UU_FALSE;
			*labelptr = UU_FALSE;
			break;
		case 1: /* text required */
			*textreqptr = UU_TRUE;
			*labelptr = UU_FALSE;
			break;
		case 2: /* label */
			*labelptr = UU_TRUE;
			break;
		default:
			uu_uerror1(UB_SYMBOL, 61, "ubi_get_textnode_type");
			/* error is: Illegal text node type,  (%s). */
			goto failed;
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}       

/*********************************************************************
**    I_FUNCTION :int ubi_setup_textnode(textnodrecptr, textptr, textattrptr)
**       This function retrieves the the text of the text node and initializes
**                      the text attribute bundle.
**    PARAMETERS   
**       INPUT  : 
**          textnodrecptr       Pointer to the text node record of the master symbol.
**       OUTPUT :  
**          textptr                     Pointer to a text entity containing the text
**                                                                      associated with this text node.
**                              textattrptr             Pointer to the text attribute bundle to be used in
**                                                                      in displaying the text node of the instance.
**    RETURNS: UU_SUCCESS if no problems encountered; otherwise, UB_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubi_setup_textnode(textnodrecptr, textptr, textattrptr)
	struct UB_text_nod_rec *textnodrecptr;
	struct UA_txt_rec *textptr;
	struct UA_txtattr_rec *textattrptr;
{
	int status = UU_SUCCESS;/* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_BTRC,(us,
		"ubi_setup_textnode(textnodrecptr,textptr, textattrptr)"));
	textptr->key = textnodrecptr->text_key;
	if (uc_retrieve_data(textptr, sizeof(struct UA_txt_rec)) != UU_SUCCESS) 
		goto failed;

/*--- new text                          ---*/
	/* get text attributes for this text from master */
	/* if (uc_retrieve_attr(textptr->key, textattrptr) != UU_SUCCESS) */
	/*      goto failed; */
	/* uu_dprint(UU_BTRC,(us,"UB_text_node_attr:%s",UB_text_node_attr)); */
/*--- end new text        ---*/

	if (strcmp(UB_text_node_attr, "current") == 0)
	{
	 ua_init_txtrec(textptr,textattrptr,&UB_txtattr,UU_TRUE);

/*--- new text   ---*/
		/* now over ride the font type, precision and size with drafting
		 * specifications */
		/* ua_text_fontnum(&(textattrptr->font)); */
		/* textattrptr->prec = (int) UG_STRING; */
		/* ua_text_precision(&(textattrptr->prec)); */
		/* ua_text_size(&(textattrptr->height)); */
		/* ua_text_spacing(&(textattrptr->exp), &(textattrptr->spacing)); */
	}
	else
		/* get text attributes for this text from master */
		if (uc_retrieve_attr(textptr->key, textattrptr) != UU_SUCCESS)
			goto failed;
/*
.....Ensure that the end of string character is in the right location
*/
	textptr->tchar[textptr->no_tchar-1] = 0;
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}       

/*********************************************************************
**    I_FUNCTION: int ubi_create_text_node(masterptr,textptr,textattrptr,
**                                                                      tnodindx,instanceptr)
**       This function creates a text node for the instance pointed to by
**                      "instanceptr". The text entity is created created here.
**    PARAMETERS   
**       INPUT  : 
**                              masterptr               Pointer to the master symbol that contains the
**                                                                      text node specifications.
**          textptr                     Pointer to the text entity associated with the
**                                                                      text node to be made.
**                              textattrptr             Pointer to the text entity attribute bundle
**                              tnodeindx               Index into the master's text list for this text node.
**                              instanceptr             Pointer to the instance that is to get this text
**                                                                      node.
**       OUTPUT :  
**          instanceptr         Pointer to the instance with a new text node.
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubi_create_text_node(masterptr,textptr, textattrptr, tnodindx, instanceptr)
	struct UB_symbol_rec *masterptr;
	struct UA_txt_rec *textptr;
	struct UA_txtattr_rec *textattrptr;
	int tnodindx;
	struct UB_instance_rec *instanceptr;
{
	struct UB_text_nod_rec textnod;
	int len;
	int status = UU_SUCCESS;/* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_BTRC,(us,
	"ubi_create_text_node(txtptr,txtattrptr,tnodindx:%d,instancptr)",tnodindx));

	len = strlen(textptr->tchar);
	textptr->no_tchar = len+1;               /* number of atoms; note, the 1 gets '\0' */

	/* assign text attributes 
	if (um_assign_text_attr(&textattr) != UU_SUCCESS)
		 goto failed;
	*/
	/* note, we assume each text node has the default transform
	 * so we ignore it. */
	if (uc_create_data(textptr, UM_DEFAULT_TF, textattrptr) != UU_SUCCESS)
		goto failed;
	textnod.text_key = textptr->key;
	textnod.visibility = masterptr->text_nod[tnodindx].visibility;
	textnod.prompt = 2; /* a label */
	textnod.masterindx = tnodindx; /* this is needed in loading new versions
											  * of master symbols */
	if (ubi_update_app_varlist(instanceptr,UB_ITEXT_LIST,&textnod,
					instanceptr->no_text_nod+1,1) != UU_SUCCESS) goto failed;
#if (TRACE)
	ubi_pscroll("INSTANCE TEXT NODE MADE");
	if (uc_print(textptr) != UU_SUCCESS)
	{
		sprintf(UB_sbuf,"CAN'T PRINT SYMBOL TEXT, KEY:%d, REL_NUM:%d",
					textptr->key, textptr->rel_num);
		ubi_pscroll(UB_sbuf);
	}
#endif
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}       

/*********************************************************************
**    E_FUNCTION : ubf_instance_namgen(nclkey,instname,instsub,ier)
**       Generate a name for copied symbol instances.
**    PARAMETERS   
**       INPUT  :
**         nclkey   = Key of instance being copied.
**       OUTPUT :
**         instname = Name of created instance.
**         instsub  = Subscript of created instance.
**         ier      = -1 = Failed to generate label.
**    RETURNS   : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ubf_instance_namgen(nclkey,instname,instsub,ier)
UM_f77_str_ptr instname;
UM_int4 *nclkey,*instsub,*ier;
{
	int i;
	char *cstr;
	struct UB_instance_rec inst;
	struct UB_symbol_rec sym;
/*
.....Initialize routine
*/
	*ier = -1;
/*
.....Retrieve the symbol instance
*/
	inst.key = *nclkey;
	if (ub_retrieve_sym(&inst,sizeof(inst)) == UU_SUCCESS)
	{
/*
.....Get the master symbol for the instance
*/
		if (ubi_get_master_key_for_inst(&inst,&sym.key) == UU_SUCCESS)
		{
			if (ub_retrieve_sym(&sym,sizeof(sym)) == UU_SUCCESS)
			{
/*
.....Label the symbol instance
*/
				UB_instanceno = 0;
				S_build_instance_name(&sym,&inst,UU_FALSE);
				cstr = UM_cstr_of_f77_str(instname);
				strcpy(cstr,inst.label);
				for (i=strlen(cstr);i<MAXSYMLEN;i++) cstr[i] = ' ';
				*instsub = inst.subscr;
				*ier = 0;
			}
		}
	}
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**    E_FUNCTION : ubf_instance_label(nclkey,instname,instsub)
**       Store a name in a symbol instance.
**    PARAMETERS   
**       INPUT  :
**         nclkey   = Key of instance to label.
**         instname = Name to assign instance.
**         instsub  = Subscript to assign instance.
**       OUTPUT : none
**    RETURNS   : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ubf_instance_label(nclkey,instname,instsub)
UM_f77_str_ptr instname;
UM_int4 *nclkey,*instsub;
{
	int i;
	char label[MAXSYMLEN],*cstr;
	struct UB_instance_rec inst;
/*
.....Retrieve the symbol instance
*/
	inst.key = *nclkey;
	if (ub_retrieve_sym(&inst,sizeof(inst)) == UU_SUCCESS)
	{
/*
.....Store new label
*/
		cstr = UM_cstr_of_f77_str(instname);
		strncpy(inst.label,cstr,MAXSYMLEN);
		i = MAXSYMLEN;
		ul_strip_blanks(inst.label,&i);
		inst.subscr = *instsub;
		ur_update_data_fixed(&inst);
	}
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**    E_FUNCTION : S_build_instance_name(masterptr,instanceptr)
**        This function builds the name of an instance.  The name is
**        based on the master symbol name if a name is not provided.
**    PARAMETERS     
**        INPUT  : 
**           masterptr    pointer to the master symbol.
**           instanceptr  Pointer to instance record.
**           dragging     UU_TRUE if this instance is used for dragging.
**        OUTPUT :  
**           instanceptr  Updated instance label.
**       RETURNS: none
**       SIDE EFFECTS: none
**       WARNINGS: none
*********************************************************************/
static void S_build_instance_name(masterptr,instanceptr,dragging)
struct UB_symbol_rec *masterptr;
struct UB_instance_rec *instanceptr;
UU_LOGICAL dragging;
{
	int inc,status;
	UU_LOGICAL found;
	struct UB_instance_rec inst;
/*
.....Create dummy name for dragging
*/
	if (dragging)
	{
		strcpy(instanceptr->label,"@UN");
		instanceptr->subscr = 0;
	}
/*
.....Build the instance name
.....based on the name of the master symbol
.....If a symbol instance is deleted
.....then we need to find the next available name
*/
	else
	{
		if (Sinst_name[0] == '[' || !Ssimple)
		{
			inc = UB_instanceno;
			inst.subscr = masterptr->subscr;
			do
			{
				sprintf(inst.label,"%s_%d",masterptr->label,
					masterptr->no_inst+inc);
				inc++;
				status = ub_get_symmaster_by_name(&inst,&found,1,2);
			} while (status == UU_SUCCESS && found);
			strcpy(Sinst_name,inst.label);
			Sinst_sub = inst.subscr;
		}
/*
.....Use predefined instance name
*/
		strcpy(instanceptr->label,Sinst_name);
		instanceptr->subscr = Sinst_sub;
	}
}

/*********************************************************************
**    E_FUNCTION : ub_get_instance_master(nclkey, symbol)
**       Returns the parent symbol name
**    PARAMETERS   
**       INPUT  :
**			   nclkey     = Symbol instance key.
**       OUTPUT :
**			   symbol   = parent symbol name.
**    RETURNS   : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ub_get_instance_master(nclkey, symbol)
UM_f77_str_ptr symbol;
UM_int4 *nclkey;
{
	int i,status;
	UU_LOGICAL stilllooking;
	struct UB_symbol_rec sym;
	struct UB_instance_rec inst;
	char *p1, *cstr = UM_cstr_of_f77_str(symbol);
		
	*cstr = '\0';
/*
.....Get the master symbol
*/
	status = UU_FAILURE;
	inst.key = *nclkey;
	if (ubi_get_master_key_for_inst(&inst,&(sym.key)) == UU_SUCCESS)
	{
		if (ub_retrieve_sym(&sym, sizeof(sym)) == UU_SUCCESS)
		{
			for (i=0,p1=sym.label;i<NCL_MAX_LABEL &&*p1!='\0'; i++)
				*cstr++ = *p1++;
			while (i<NCL_MAX_LABEL)
			{
				*cstr++ = ' ';
				i++;
			}
			status = UU_SUCCESS;
		}
	}
/*
.....End of routine
*/
done:;
	return(status);
}
