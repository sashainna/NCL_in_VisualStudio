/*********************************************************************
**    NAME         :  reiu.c
**       CONTAINS:
**           ur_init_unibase()
**           ur_init_unibase_stat()
**           ur_unibase_stat()
**           ur_retrieve_unibase_stat()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reiu.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:31
*********************************************************************/

#include	"usysdef.h"
#include	"udebug.h"
#include	"mdrel.h"
#include	"nclver.h"
#include	"rmtuple.h"
#include "riddle.h"
#include	"ribase.h"
#include	"rstack.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "xenv1.h"

/*********************************************************************
**    E_FUNCTION     :  status = ur_init_unibase()
**      initialize unibase
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/* declare the relation control block array */
struct	UR_rcb_rec	UR_rcb[UR_NUM_REL];

/* set aside space for the default path name for save and load */
char	UR_dpn[UX_MAX_PATH_LEN+1];

/* Define other global variables for Unibase */
int UR_active = 0;
UU_KEY_ID	UR_last_mod_mkey;	/* key of last modified master tuple*/
UU_REL_ID	UR_last_mod_rkey;	/* key of last modified tuple			*/
UR_REL_NUM	UR_transf_relnum;	/* what is the transformation relation		*/
long			UR_default_transf;/* has the default matrix been defined		*/
UU_LOGICAL	UR_dd_inited;		/* has data dictionary been initialized	*/
UU_LOGICAL	UR_sav_all;			/* set TRUE to save all tuples				*/
UU_LOGICAL	UR_save_modals;	/* set TRUE to save modals */
UU_LOGICAL	UR_del_mark;		/* set TRUE to mark stack on next delete	*/
UU_LOGICAL	UR_del_stack_enabled;/* set TRUE if del stack enabled			*/
UU_LOGICAL	UR_del_started;	/* TRUE if a logical delete transaction */
										/* is under way */
int			UR_chg_cnt = 0;	/* Unibase change counter - initially 0 */
UU_LOGICAL	UR_changed = UU_FALSE;	/* Unibase change flag - initially FALSE */
UU_LOGICAL	UR_load_env = UU_FALSE;	/* flag whether to read new environment */

/* define the delete stack */
UR_STACK(UR_del_stack,UR_DEL_STK_DEPTH,dstack_element) = {UR_DEL_STK_DEPTH,-1};

ur_init_unibase()
{
	/* local  parameter declarations */
	int	status;			/* status, -1 if error, 0 otherwise		*/
	int	i;					/* an index										*/
	char	*fname;			/* UniDDL file name 							*/
	char	*ux_getenv();
	int	ddl_fd;			/* Uni-DDL file descriptor					*/
	int	atomsize[1],listsize[1];

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**
*/
	uu_denter(UU_RTRC,(us,"ur_init_unibase with %d relations",UR_NUM_REL));

	/* set global unibase variables */
	UR_last_mod_mkey = 0;		/* key of last modified master tuple*/
	UR_last_mod_rkey = 0;		/* key of last modified tuple			*/
	UR_transf_relnum = 0;		/* set to something it can't be	*/
	UR_default_transf	= UU_FALSE; /* no default matrix yet			*/
	UR_dd_inited = UU_FALSE;	/* no data dictionary yet */
	UR_sav_all = UU_TRUE;		/* save all tuples					*/
	UR_save_modals = UU_TRUE;	/* save modals in save file */
	UR_del_mark = UU_TRUE;		/* set mark del stack				*/
	UR_del_stack_enabled = UU_FALSE;	/* disable del stack					*/
	UR_del_started = UU_FALSE;

	/* set default save,load pathname to null */
	UR_dpn[0] = '\000';

	/* set all relations as inactive, status = -1 */
	for(i = 0; i <= UR_MAX_REL; i++) 
	{
		UR_rcb[i].status = -1;
		UR_rcb[i].rel_num = i;
		UR_rcb[i].n_ent = 
		UR_rcb[i].init_ent =
		UR_rcb[i].n_varl = 
		UR_rcb[i].rel_flags = 
		UR_rcb[i].last_accessed_index = 
		UR_rcb[i].last_active_index =
		UR_rcb[i].active_tuple_cnt = 
		UR_rcb[i].tuple_size =
		UR_rcb[i].bmap_size = 0;
		UR_rcb[i].ent_ptr = 0;
		UR_rcb[i].bmap_ptr = 0;
	}
	ur_init_data_dict(0);

	/* initialize the master tuple relation */
	atomsize[0] = sizeof(UU_KEY_ID);
	listsize[0] = 10;
	status = ur_init_data_rel(UR_MTUPLE_REL, "MTID", 2000,
						sizeof(struct UR_MTID_rec) - UR_MTID_BUFSZ,
						1, atomsize, listsize);
 if (UR_active == 0) UR_active = 1;
	uu_dexit ;
	return(status) ;
}

/*********************************************************************
**    E_FUNCTION     :  ur_init_unibase_stat(flag)
**      Initializes Unibase Statistics record.
**    PARAMETERS   
**       INPUT  : 
**          iflag    = UR_STAT_INIT   = Initialize internal Unibase.
**                     UR_STAT_UPDATE = Update stats when loading older Unibase.
**                     UR_STAT_MODIFY = Updated modified stats when saving
**                                      Unibase.
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ur_init_unibase_stat(iflag)
int iflag;
{
	int mm,dd,yy,m,h,status,flag;
	char *p;
	UX_pathname dir,fname,fullname;
	struct UR_unistat_rec unistat;
/*
.....Get Unibase Statistics record
*/
	flag = iflag;
	if (flag == UR_STAT_MODIFY)
	{
		status =  ur_retrieve_unibase_stat(&unistat);
		if (status != 0) flag = UR_STAT_INIT;
	}
/*
.....Initialize structure
*/
	if (flag == UR_STAT_INIT || flag == UR_STAT_UPDATE)
	{
		unistat.key = 0;
		unistat.rel_num = UR_UNISTAT_REL;
/*
........Generating system
*/
		if (flag == UR_STAT_UPDATE)
		{
			sprintf(unistat.system,"NCL V%.3f",NCL_infile_version);
			strcpy(unistat.processor,NCL_machine_type);
		}
		else
		{
			if (NCL_machine[0] == '\0') znu_init_machinetype();
			sprintf(unistat.system,"NCL V%.3f",NCL_version);
			strcpy(unistat.processor,NCL_machine);
		}
/*
........Author, Company
*/
		if (flag == UR_STAT_UPDATE)
		{
			strcpy(unistat.author,"Unknown");
			strcpy(unistat.company,"Unknown");
		}
		else
		{
			p = ux_getenv("NCL_USERNAME",UX_NPRTERRS);
			if (p == UU_NULL)
				p = ux_getenv("USERNAME",UX_NPRTERRS);
			if (p == UU_NULL)
				strcpy(unistat.author,"Unknown");
			else
				strcpy(unistat.author,p);
	
			p = ux_getenv("NCL_COMPANYNAME",UX_NPRTERRS);
			if (p == UU_NULL)
				p = ux_getenv("COMPANYNAME",UX_NPRTERRS);
			if (p == UU_NULL)
				strcpy(unistat.company,"Unknown");
			else
				strcpy(unistat.company,p);
		}
/*
........Filename
*/
		if (flag == UR_STAT_UPDATE || iflag == UR_STAT_MODIFY)
		{
			ul_break_fname(UR_dpn,dir,fname);
			ul_build_full_fname(dir,fname,"u",fullname);
			strcpy(unistat.fname,fullname);
		}
		else
			unistat.fname[0] = '\0';
/*
........Date, Translator
*/
		if (flag == UR_STAT_UPDATE)
		{
			sscanf(NCL_infile_date,"%d/%d/%d,%d:%d",&mm,&dd,&yy,&h,&m);
			if (yy < 100) yy += 1900;
			ul_format_date(mm,dd,yy,h,m,0,unistat.date);
		}
		else
			ul_date_and_time(unistat.date);
		unistat.translator[0] = '\0';
/*
........Modified fields
*/
		if (flag == UR_STAT_INIT) strcpy(unistat.mod_system,"	");
		else unistat.mod_system[0] = '\0';
		unistat.mod_author[0] = '\0';
		unistat.mod_company[0] = unistat.mod_date[0] = '\0';
		unistat.no_notes = 0;
/*
........Create header record
*/
		ur_create_data(&unistat);
	}
/*
.....Update modified fields
*/
	else
	{
/*
........File name
*/
		if (unistat.fname[0] == '\0')
		{
			ul_break_fname(UR_dpn,dir,fname);
			ul_build_full_fname(dir,fname,"u",fullname);
			strcpy(unistat.fname,fullname);
		}
/*
........Creating original Unibase
*/
		if (unistat.mod_system[0] == '	')
			unistat.mod_system[0] = '\0';
/*
........Generating system
*/
		else
		{
			sprintf(unistat.mod_system,"NCL V%.3f",NCL_version);
/*
........Author, Company
*/
			p = ux_getenv("NCL_USERNAME",UX_NPRTERRS);
			if (p == UU_NULL)
				p = ux_getenv("USERNAME",UX_NPRTERRS);
			if (p == UU_NULL)
				strcpy(unistat.mod_author,"Unknown");
			else
				strcpy(unistat.mod_author,p);

			p = ux_getenv("NCL_COMPANYNAME",UX_NPRTERRS);
			if (p == UU_NULL)
				p = ux_getenv("COMPANYNAME",UX_NPRTERRS);
			if (p == UU_NULL)
				strcpy(unistat.mod_company,"Unknown");
			else
				strcpy(unistat.mod_company,p);
/*
........Date
*/
			ul_date_and_time(unistat.mod_date);
		}
/*
........Update Unibase Statistics record
*/
		ur_update_data_fixed(&unistat);
	}
}

/*********************************************************************
**    E_FUNCTION     : ur_unibase_stat(parms)
**          Controls Unibase Statistics form.
**    PARAMETERS   
**       INPUT  : 
**          parms    = "TRUE" = User can modify Unibase Statistics.
**                     Any other value allows the user to modify the
**                     Notes section only.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ur_unibase_stat(parms)
char *parms;
{
	int status,nc;
	UU_LOGICAL cmdreject,changed;
	char *tnotes;
	static char notes[8000];
	struct UR_unistat_rec unistat;
	static struct UR_unistat_rec tstat;
/*
.....Set up form fields
*/
	static int *ans[] = {
		(int *)&(tstat.system),(int *)&(tstat.company),(int *)&(tstat.processor),
		(int *)&(tstat.author),(int *)&(tstat.fname),(int *)&(tstat.date),
		(int *)&(tstat.translator),
		(int *)&(tstat.mod_system),(int *)&(tstat.mod_company),
		(int *)&(tstat.mod_author),(int *)&(tstat.mod_date),
		(int *)&notes};
/*
.....Get Unibase Statistics record
*/
	status = ur_retrieve_unibase_stat(&unistat);
	if (status != 0) goto failed;
	tstat = unistat;
/*
.....Get Unibase Notes
*/
	if (tstat.no_notes == 0)
	{
		notes[0] = '\0';
		tnotes = UU_NULL;
	}
	else
	{
		ur_retrieve_data_varlist(unistat.key,1,notes,1,tstat.no_notes);
		tnotes = uu_malloc(sizeof(char)*(tstat.no_notes+1));
		strcpy(tnotes,notes);
	}
/*
.....Trap Reject Op
*/
	UD_MARK (cmdreject, UU_FALSE);
	if (cmdreject != 0) goto done;
/*
.....Get the Form input
*/
	status = ud_form("unistat.frm",ans,ans);
/*
.....Save statistics if the user modified them
*/
	if (strcmp(parms,"TRUE") == 0 &&
		(strcmp(tstat.system,unistat.system) != 0 ||
		strcmp(tstat.company,unistat.company) != 0 ||
		strcmp(tstat.processor,unistat.processor) != 0 ||
		strcmp(tstat.author,unistat.author) != 0 ||
		strcmp(tstat.fname,unistat.fname) != 0 ||
		strcmp(tstat.date,unistat.date) != 0 ||
		strcmp(tstat.translator,unistat.translator) != 0 ||
		strcmp(tstat.mod_system,unistat.mod_system) != 0 ||
		strcmp(tstat.mod_company,unistat.mod_company) != 0 ||
		strcmp(tstat.mod_author,unistat.mod_author) != 0 ||
		strcmp(tstat.mod_date,unistat.mod_date) != 0))
			ur_update_data_fixed(&tstat);
/*
.....Update notes if they have changed
*/
	changed = UU_FALSE;
	nc = strlen(notes) + 1;
	if (tnotes != UU_NULL)
	{
		if (strcmp(notes,tnotes) != 0) changed = UU_TRUE;
		uu_free(tnotes);
	}
	else if (nc != 1) changed = UU_TRUE;
	if (changed)
			ur_update_data_varlist(tstat.key,1,notes,1,nc);
/*
.....End of routine
*/
done:;
	UD_UNMARK(cmdreject);
	return;
/*
.....Could not find Unibase Statistics record
*/
failed:;
	ud_wrerr("Could not find Unibase Statistics in current Unibase.");
	return;
}

/*********************************************************************
**    E_FUNCTION     : ur_retrieve_unibase_stat(unistat)
**          Returns the Unibase Statistics record.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          unistat   = Unibase Statistics record.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ur_retrieve_unibase_stat(unistat)
struct UR_unistat_rec *unistat;
{
	int status,entnum;
/*
.....Get Unibase Statistics record
*/
	entnum = 1;
	unistat->rel_num = UR_UNISTAT_REL;
	status = ur_get_next_data_key(unistat->rel_num,&entnum,&unistat->key);
	if (status == 0) status = ur_retrieve_data_fixed(unistat);
	return(status);
}
