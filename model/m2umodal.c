/********************************************************************* 
**  NAME: m2umodal.c
**
**		CONTAINS:
**			um_material_modals
**			um_get_material_names
**       umf_get_material_number
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m2umodal.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:50
**
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "uims.h"
#include "lumb.h"
#include "mattr.h"
#include "msrf.h"
#include "mdrel.h"
#include "mrender.h"
#include "mrenddl.h"
#include "gtbldef.h"
#include "mdcoord.h"
#include "udfdata.h"
#include "dselmask.h"
#include "uhep.h"
#include "class.h"
#include "udforms.h"
#include "xfsys1.h"
#include "xenv1.h"

extern int UM_material_reset;
/* Defaults */
extern int um_renmode;					/* Rendering mode */
extern int um_lightmodel;				/* Lighting  model */

int ug_noop();

/* Light keys based on light index */
extern int UM_light_keys[];

static struct UM_mtrlmdl_rec UM_mtrlmdl_tmp;
static char tmp_name[20];
static UU_REAL	tmp_ka, tmp_kd, tmp_ks, tmp_r, tmp_b, tmp_g;
static int tmp_choice, tmp_exp, save_choice;
static int 	material_change[16] = {0,0,0,0,0,0,0,0,
										0,0,0,0,0,0,0,0};
static int 	material_chg = 0;
static void set_frm_mat(choice)
int choice;
{
	char name[20];
	UU_REAL tmp;
	int tmpint;

	strcpy(name, &(UM_mtrlmdl_tmp.name[choice]));
	ud_update_answer(1,(int *)name);

	tmp = UM_mtrlmdl_tmp.ka[choice];
	ud_update_answer(2,(int *)&tmp);

	tmp = UM_mtrlmdl_tmp.kd[choice];
	ud_update_answer(3,(int *)&tmp);

	tmp = UM_mtrlmdl_tmp.ks[choice];
	ud_update_answer(4,(int *)&tmp);

	tmpint = UM_mtrlmdl_tmp.spec_exp[choice];
	ud_update_answer(5,(int *)&tmpint);

	tmp = UM_mtrlmdl_tmp.ks_r[choice];
	ud_update_answer(6,(int *)&tmp);

	tmp = UM_mtrlmdl_tmp.ks_g[choice];
	ud_update_answer(7,(int *)&tmp);

	tmp = UM_mtrlmdl_tmp.ks_b[choice];
	ud_update_answer(8,(int *)&tmp);
}

static void save_tmp_mat(choice)
int choice;
{
	char name[20],str[80];
	UD_DDATA data;
	UU_REAL tmp;
	int i, tmpint;
/*
.....Get the material name
*/
	data.frmstr = name;
	ud_get_field(1,data,UU_FALSE);
	strcpy(&(UM_mtrlmdl_tmp.name[choice]), name);

	data.frmstr = str;
	ud_get_field(2,data,UU_FALSE);
	i = strlen(str);
	ul_strip_blanks(str,&i);
	ul_to_reals(&tmp,&i,1,str);
	UM_mtrlmdl_tmp.ka[choice] = tmp;

	ud_get_field(3,data,UU_FALSE);
	i = strlen(str);
	ul_strip_blanks(str,&i);
	ul_to_reals(&tmp,&i,1,str);
	UM_mtrlmdl_tmp.kd[choice] = tmp;

	ud_get_field(4,data,UU_FALSE);
	i = strlen(str);
	ul_strip_blanks(str,&i);
	ul_to_reals(&tmp,&i,1,str);
	UM_mtrlmdl_tmp.ks[choice] = tmp;

	data.frmstr = str;
	ud_get_field(5,data,UU_FALSE);
	i = strlen(str);
	ul_strip_blanks(str,&i);
	ul_to_number(str,&tmpint);
	UM_mtrlmdl_tmp.spec_exp[choice] = tmpint;

	data.frmstr = str;
	ud_get_field(6,data,UU_FALSE);
	i = strlen(str);
	ul_strip_blanks(str,&i);
	ul_to_reals(&tmp,&i,1,str);
	UM_mtrlmdl_tmp.ks_r[choice] = tmp;

	ud_get_field(7,data,UU_FALSE);
	i = strlen(str);
	ul_strip_blanks(str,&i);
	ul_to_reals(&tmp,&i,1,str);
	UM_mtrlmdl_tmp.ks_g[choice] = tmp;

	ud_get_field(8,data,UU_FALSE);
	i = strlen(str);
	ul_strip_blanks(str,&i);
	ul_to_reals(&tmp,&i,1,str);
	UM_mtrlmdl_tmp.ks_b[choice] = tmp;
}

/*********************************************************************
**    S_FUNCTION     :  um_selmat()
**       Routine to select a list of surfaces.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT um_selmat()
{
	struct UC_entitydatabag e;
	UU_LOGICAL cmdreject;
	int numint,init,material,status;
	struct UM_surfattr_rec srfattr;
	struct UM_agsrf_rec *asrf;
/*
.....Take down form
*/
	ud_form_invis();
/*
.....Trap Reject Op
*/
	UD_MARK (cmdreject, UU_TRUE);
	if (cmdreject != 0) goto done;
/*
.....Set the appropriate selection mask
*/
	ud_lgeo(UU_TRUE,UD_ncl_allsf);
/*
.....Get the next geometry selection
*/
	ud_ldas(UD_DASSELECT,UA_NCL,478,UU_NULL,1,&numint,UD_NODEFAULT);
	if (numint == 0) goto done;
/*
.....Loop through selections
*/
	init = UU_TRUE;
	material = tmp_choice;
	save_tmp_mat(tmp_choice);
	if (UM_mtrlmdl.ka[tmp_choice] != UM_mtrlmdl_tmp.ka[tmp_choice])
	{
		material_chg = 1;
		material_change[tmp_choice] = 1;
	}
	if (UM_mtrlmdl.kd[tmp_choice] != UM_mtrlmdl_tmp.kd[tmp_choice])	
	{
		material_chg = 1;
		material_change[tmp_choice] = 1;
	}
	if (UM_mtrlmdl.ks[tmp_choice] != UM_mtrlmdl_tmp.ks[tmp_choice])	
	{
		material_chg = 1;
		material_change[tmp_choice] = 1;
	}
	if (UM_mtrlmdl.spec_exp[tmp_choice] != UM_mtrlmdl_tmp.spec_exp[tmp_choice])	
	{
		material_chg = 1;
		material_change[tmp_choice] = 1;
	}
	if (UM_mtrlmdl.ks_r[tmp_choice] != UM_mtrlmdl_tmp.ks_r[tmp_choice])	
	{
		material_chg = 1;
		material_change[tmp_choice] = 1;
	}
	if (UM_mtrlmdl.ks_g[tmp_choice] != UM_mtrlmdl_tmp.ks_g[tmp_choice])	
	{
		material_chg = 1;
		material_change[tmp_choice] = 1;
	}
	if (UM_mtrlmdl.ks_b[tmp_choice] != UM_mtrlmdl_tmp.ks_b[tmp_choice])	
	{
		material_chg = 1;
		material_change[tmp_choice] = 1;
	}
	UM_mtrlmdl.ka[tmp_choice] = UM_mtrlmdl_tmp.ka[tmp_choice];
	UM_mtrlmdl.kd[tmp_choice] = UM_mtrlmdl_tmp.kd[tmp_choice];
	UM_mtrlmdl.ks[tmp_choice] = UM_mtrlmdl_tmp.ks[tmp_choice];
	UM_mtrlmdl.spec_exp[tmp_choice] = UM_mtrlmdl_tmp.spec_exp[tmp_choice];
	UM_mtrlmdl.ks_r[tmp_choice] = UM_mtrlmdl_tmp.ks_r[tmp_choice];
	UM_mtrlmdl.ks_g[tmp_choice] = UM_mtrlmdl_tmp.ks_g[tmp_choice];
	UM_mtrlmdl.ks_b[tmp_choice] = UM_mtrlmdl_tmp.ks_b[tmp_choice];
	strcpy(&(UM_mtrlmdl.name[tmp_choice]), &(UM_mtrlmdl_tmp.name[tmp_choice]));

	while(ud_gnxt(init,UU_NULL,&e.key,1))
	{
		init = UU_FALSE;
		uc_retrieve_data(&e,sizeof(e));
/*
.....Update the entities material
*/
		if (material != -1)
		{
			um_set_material(&e, material);
						
			switch (e.rel_num)
			{
			case UM_AGSRF_REL:
				asrf = (struct UM_agsrf_rec *) &e;
				asrf->material = material;
				break;
			case NCL_TRIMSF_REL:
			case UM_RBSPLSRF_REL:
			case NCL_SURF_REL:
			case NCL_MESHSURF_REL:
			case NCL_REVSURF_REL:
			case UM_SOLID_REL:
				status = uc_retrieve_attr(e.key,&srfattr);
				if (status == UU_SUCCESS)
				{
					srfattr.material = material;
					ur_update_attr(&srfattr);
				}
				break;
			default:
				break;
			}

			if (e.rel_num != UM_SOLID_REL) ur_update_data_fixed(&e);
			status = ncl_retrieve_data_fixed(&e);
			uc_display(&e);
		}
	}
/*
.....End of routine
.....Redisplay form
*/
done:;
	ud_unlimit();
	ud_form_vis();
	UD_UNMARK(cmdreject);

	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  static um_typchg(filedno, val, stat)
**       Method called at when  material type
**                      is changed
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
static UD_FSTAT  um_typchg(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....save the data into UM_mtrlmdl_tmp
*/
	save_tmp_mat(save_choice);
/*
.....val->frmint contains choice
*/
	tmp_choice = val->frmint[0];
	set_frm_mat(tmp_choice);
	save_choice = tmp_choice;
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : um_material_modals
**       Sets materials with a form.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
um_material_modals()
{
	int i,status,material;
	struct UC_entitydatabag ent;
	UU_LOGICAL init;
	UU_KEY_ID key;
	static int sel = 0;
	static int *ans[] = { 
		(int *)&tmp_choice, (int *)tmp_name, (int *)&tmp_ka,  (int *)&tmp_kd,  (int *)&tmp_ks,  
		(int *)&tmp_exp, (int *)&tmp_r, (int *)&tmp_g, (int *)&tmp_b, (int *)&sel};
	static UD_METHOD methods[]  = {
			um_typchg, (UD_METHOD)ug_noop, (UD_METHOD)ug_noop, (UD_METHOD)ug_noop,
			(UD_METHOD)ug_noop, (UD_METHOD)ug_noop, (UD_METHOD)ug_noop, 
			(UD_METHOD)ug_noop, (UD_METHOD)ug_noop, um_selmat};
	static char called[] = {6,6,6, 6,6,6,6,6, 6,6};

	tmp_choice = UM_mtrlmdl.index;
	save_choice = tmp_choice;
	strcpy (tmp_name, UM_mtrlmdl.name[tmp_choice]);
	tmp_ka = UM_mtrlmdl.ka[tmp_choice];
	tmp_kd = UM_mtrlmdl.kd[tmp_choice];
	tmp_ks = UM_mtrlmdl.ks[tmp_choice];
	tmp_r = UM_mtrlmdl.ks_r[tmp_choice];
	tmp_g = UM_mtrlmdl.ks_g[tmp_choice];
	tmp_b = UM_mtrlmdl.ks_b[tmp_choice];
	tmp_exp = UM_mtrlmdl.spec_exp[tmp_choice];
	for( i=0; i<16; i++ ) 
	{
		UM_mtrlmdl_tmp.ka[i] = UM_mtrlmdl.ka[i];
		UM_mtrlmdl_tmp.kd[i] = UM_mtrlmdl.kd[i];
		UM_mtrlmdl_tmp.ks[i] = UM_mtrlmdl.ks[i];
		UM_mtrlmdl_tmp.spec_exp[i] = UM_mtrlmdl.spec_exp[i];
		UM_mtrlmdl_tmp.ks_r[i] = UM_mtrlmdl.ks_r[i];
		UM_mtrlmdl_tmp.ks_g[i] = UM_mtrlmdl.ks_g[i];
		UM_mtrlmdl_tmp.ks_b[i] = UM_mtrlmdl.ks_b[i];
		strcpy(&(UM_mtrlmdl_tmp.name[i]), &(UM_mtrlmdl.name[i]));
		material_change[i] = 0;
	}
	status = ud_form1("mmtrlset.frm", ans, ans, methods, called, NULL, NULL);
	if (status==-1)
		return -1;
/*
.....SAVE THE LAST CHNAGED PAGE
*/
	UM_mtrlmdl_tmp.ka[tmp_choice] = tmp_ka;
	UM_mtrlmdl_tmp.kd[tmp_choice] = tmp_kd;
	UM_mtrlmdl_tmp.ks[tmp_choice] = tmp_ks;
	UM_mtrlmdl_tmp.spec_exp[tmp_choice] = tmp_exp;
	UM_mtrlmdl_tmp.ks_r[tmp_choice] = tmp_r;
	UM_mtrlmdl_tmp.ks_g[tmp_choice] = tmp_g;
	UM_mtrlmdl_tmp.ks_b[tmp_choice] = tmp_b;
	strcpy(&(UM_mtrlmdl_tmp.name[tmp_choice]), tmp_name);

	UM_mtrlmdl.index = tmp_choice;

	for( i=0; i<16; i++ ) 
	{
		if (UM_mtrlmdl.ka[i] != UM_mtrlmdl_tmp.ka[i])
		{
			material_chg = 1;
			material_change[i] = 1;
		}
		if (UM_mtrlmdl.kd[i] != UM_mtrlmdl_tmp.kd[i])	
		{
			material_chg = 1;
			material_change[i] = 1;
		}
		if (UM_mtrlmdl.ks[i] != UM_mtrlmdl_tmp.ks[i])	
		{
			material_chg = 1;
			material_change[i] = 1;
		}
		if (UM_mtrlmdl.spec_exp[i] != UM_mtrlmdl_tmp.spec_exp[i])	
		{
			material_chg = 1;
			material_change[i] = 1;
		}
		if (UM_mtrlmdl.ks_r[i] != UM_mtrlmdl_tmp.ks_r[i])	
		{
			material_chg = 1;
			material_change[i] = 1;
		}
		if (UM_mtrlmdl.ks_g[i] != UM_mtrlmdl_tmp.ks_g[i])	
		{
			material_chg = 1;
			material_change[i] = 1;
		}
		if (UM_mtrlmdl.ks_b[i] != UM_mtrlmdl_tmp.ks_b[i])	
		{
			material_chg = 1;
			material_change[i] = 1;
		}

		UM_mtrlmdl.ka[i] = UM_mtrlmdl_tmp.ka[i];
		UM_mtrlmdl.kd[i] = UM_mtrlmdl_tmp.kd[i];
		UM_mtrlmdl.ks[i] = UM_mtrlmdl_tmp.ks[i];
		UM_mtrlmdl.spec_exp[i] = UM_mtrlmdl_tmp.spec_exp[i];
		UM_mtrlmdl.ks_r[i] = UM_mtrlmdl_tmp.ks_r[i];
		UM_mtrlmdl.ks_g[i] = UM_mtrlmdl_tmp.ks_g[i];
		UM_mtrlmdl.ks_b[i] = UM_mtrlmdl_tmp.ks_b[i];
		strcpy(&(UM_mtrlmdl.name[i]), &(UM_mtrlmdl_tmp.name[i]));
	}
/*
.....save the material attributes into ncl_material.mod
*/
	um_savemat_modfile();
/*
.....if the material changed, redisplay all
*/
	if (material_chg)
	{
		init = UU_TRUE;
		while (uv_getobjs(init, &key, UU_FALSE) == UU_SUCCESS)
		{
			init = UU_FALSE;
			ent.key = key;
			if (ncl_retrieve_data_fixed (&ent) == UU_SUCCESS) 
			{
				ncl_retrieve_material(&ent, &material);
				if (material_change[material])
					uc_display(&ent);
			}
		}
		UM_material_reset = 1;
	}
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : um_savemat_modfile
**       Save material setting into a modal file
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
um_savemat_modfile()
{
	int i, stat;
	char msg[256];
	UX_pathname fname;
	FILE *fptr;
/*
.....Initialize routine
*/
	stat = UU_SUCCESS;
/*
.....Open Modals file
*/
	strcpy(fname,"ncl_material.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS","modals",UU_NULL,UU_NULL,fname,
		3,&fptr);
	if (stat != UU_SUCCESS || fptr == UU_NULL) goto done;
/*
.....Store modals
*/
	ux_fputs0("#MATERIAL#\n", fptr);

	for (i=0; i<16; i++)
	{
		if (i != 0) ux_fputs0("\n", fptr);
		sprintf(msg, "/TYPE/ %d\n", i+1);
		ux_fputs0(msg, fptr);
		sprintf(msg, "/NAME/ %s\n", UM_mtrlmdl.name[i]);
		ux_fputs0(msg, fptr);
		sprintf(msg, "/AMBIENT/ %3.4f\n", UM_mtrlmdl.ka[i]);
		ux_fputs0(msg, fptr);
		sprintf(msg, "/DIFFUSE/ %3.4f\n", UM_mtrlmdl.kd[i]);
		ux_fputs0(msg, fptr);
		sprintf(msg, "/SPECULAR/ %3.4f\n", UM_mtrlmdl.ks[i]);
		ux_fputs0(msg, fptr);
		sprintf(msg, "/S_COLOR/ %3.4f, %3.4f, %3.4f\n", UM_mtrlmdl.ks_r[i], 
						UM_mtrlmdl.ks_g[i], UM_mtrlmdl.ks_b[i]);
		ux_fputs0(msg, fptr);
		sprintf(msg, "/EXPONENT/ %3.4f\n", UM_mtrlmdl.spec_exp[i]);
		ux_fputs0(msg, fptr);
	}
	ux_fclose0 (fptr);
done:
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : um_get_material_names
**       Returns the names of the defined materials in a DAS list.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          number    = Number of material names returned.
**    RETURNS      : none
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
char **um_get_material_names(number)
int *number;
{
	int i,len;
	char **names;
/*
.....Allocate memory for material names
*/
	len = 16;
	names = (char **)uu_malloc(len*sizeof(char *));
/*
.....Loop through materials
.....to retrieve their names
*/
	for (i=0;i<len;i++)
	{
		names[i] = (char *)uu_malloc(sizeof(UM_mtrlmdl.name[i]));
		strcpy(names[i],UM_mtrlmdl.name[i]);
	}
/*
.....End of routine
*/
	*number = len;
	return(names);
}

/*********************************************************************
**    E_FUNCTION     : umf_get_material_number(name,nc,number)
**       Determines if the input name is a valid material and returns
**       its number if it is.
**    PARAMETERS   
**       INPUT  : 
**          name      = Name of material to find.
**          nc        = Number of characters in 'name'.
**       OUTPUT :  
**          number    = Number of material if found, -1 if not.
**    RETURNS      : none
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
void umf_get_material_number(name,nc,number)
char *name;
int *nc,*number;
{
	int i,len;
	char cmatrl[80];
/*
.....Initialize routine
*/
	len = 16;
	strncpy(cmatrl,name,*nc);
	cmatrl[*nc] = '\0';
/*
.....Loop through materials
.....to retrieve their names
*/
	for (i=0;i<len;i++)
	{
		if (!ul_compare_upper(cmatrl,UM_mtrlmdl.name[i]))
		{
			*number = i;
			break;
		}
	}
/*
.....End of routine
*/
	return;
}
