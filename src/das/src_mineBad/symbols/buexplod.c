/*********************************************************************
**    NAME:  buexplod.c
**       CONTAINS:
**    		int ubu_explode_syminstance()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       buexplod.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:05
*********************************************************************/
#include "usysdef.h"	/* for UU_REAL, etc. */
#include "uhep.h"    /* for error system */
#include "udebug.h"	/* for debugging trace facility */
#include "dmark.h"	/* for UD_MARK and UD_UNMARK */
#include "class.h"	/* for "UC_" data types */
#include "dasnog.h"	/* for DAS data types */
#include "mdcoord.h" /* for UM_vector, etc */
#include "mdattr.h"	/* for definition of colors and UM_DISPLAYABLE */
#include "mdrel.h"	/* for define relation numbers */
#include "mdpick.h"	/* for UM_PICKENT */
#include "bsym.h"
#include "dselmask.h"	/* for UD_symbol for ud_lgeo call */
#include "mfort.h"   /* for fortran data defines */
#include "nccs.h"    /* for ncl_fixed_databag */
#include "nclfc.h"
#include "nclcmd.h"    /* for ncl_fixed_databag */
#include "nclinp.h"    /* for ncl_fixed_databag */
#include "nkeywd.h"

#define TRACE UU_FALSE	/* for debugging only */

static int Ssub,Snent,Stnent=0,Sflag,Smod,Snents=0;
static UU_LOGICAL Smod_def=UU_FALSE,Slen_exceed=UU_FALSE;
static UU_KEY_ID Skey;
static char Slabel[NCL_MAX_LABEL];

/*********************************************************************
**   E_FUNCTION:  ubf_get_numexplod(nents)
**      This routine outputs the total number of extracted entities.
*********************************************************************/
void ubf_get_numexplod(nents)
UM_int4 *nents;
{
	*nents = Snents;
}

/*********************************************************************
**   E_FUNCTION:  ubf_get_numexplod()
**      This routine resets the total number of extracted entities.
*********************************************************************/
void ubf_reset_numexplod()
{
	Snents = 0;
}

/*********************************************************************
**   E_FUNCTION:  ubf_get_numexplod(nents)
**      This routine sets the total number of extracted entities.
*********************************************************************/
void ubf_set_numexplod(nents)
UM_int4 *nents;
{
	Snents = *nents;
}

/*********************************************************************
**   S_FUNCTION:  S_get_label(cmdstr,label,subsc)
**      This routine gets the label and subscript from the input
**      string and sets subscript and parenthesis flags.
**    PARAMETERS   
**       INPUT  :
**          cmdstr  = command string
**       OUTPUT :
**          label   = label in command
**          subscr  = label subscript
**          subfl   = UU_TRUE if the string is subscripted
**                  = UU_FALSE if it is not
**          parfl   = UU_TRUE if the subscripted has parenthesis
**                  = UU_FALSE if it does not
**    RETURNS: none
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
void S_get_label(cmdstr,label,subsc,subfl,parfl,errfl)
char *cmdstr,*label;
int *subsc,*errfl;
UU_LOGICAL *parfl,*subfl;
{
	int i=0,j,k;
	char buf[80];
	UU_LOGICAL lpar=UU_FALSE,rpar=UU_FALSE;

	*subsc = 0;
	*parfl = UU_FALSE;
	*subfl = UU_FALSE;

	while (i < NCL_MAX_COMLINE && cmdstr[i] != '=' && cmdstr[i] != 0)
	{
		if (cmdstr[i] == '(' || (cmdstr[i] >= '0' && cmdstr[i] <= '9'))
		{
			k = 0;
			j = i;
			if (cmdstr[i] == '(')
			{
				lpar = UU_TRUE;
				j++;
			}
			while (j < NCL_MAX_COMLINE)
			{
				if (cmdstr[j] == ')')
				{
					if (k == 0 || !lpar)
					{
						*errfl = 61;
						return;
					}
					else
					{
						*subsc = atoi(buf);
						*parfl = UU_TRUE;
						*subfl = UU_TRUE;
						return;
					}
				}
				else if (!lpar && (cmdstr[j] == 0 || cmdstr[j] == '='))
					break;
				else if (cmdstr[j] < '0' || cmdstr[j] > '9')
				{
					*errfl = 61;
					return;
				}
				buf[k++] = cmdstr[j++];
			}
			*subsc = atoi(buf);
			*subfl = UU_TRUE;
			return;
		}
		label[i] = cmdstr[i];
		label[++i] = '\0';
	}
}

/*********************************************************************
** E_FUNCTION:ubf_explode_syminstance(nclkey,label,isub,nent,flg,ierr)
**       Fortan callable routine to decompose a symbol instance.  The
**       labeling of the extracted entities follows these rules.
**
**          isub = 0  - 'label' does not have a numeric suffix.
**                      A numeric suffix will be added to the label.
**                      For example, 'abc' will create the labels
**                      'abc1', 'abc2', etc.
**
**                 +  - A subscript has been specified with the label
**                      and will be incremented for each entity.
**
**                 -  - The label has a numeric suffix (which should
**                      be removed from the label prior to calling
**                      this routine) and it will be incremented for
**                      each entity.
**
**    PARAMETERS   
**       INPUT  :
**          nclkey    = Key of symbol instance to decompose.
**          label     = Label of entities created from instance.
**          isub      = Starting subscript of entities created from instance.
**          flg       = 3 if call is from CAM
**                    = 4 if call is for decomposing all instances
**       OUTPUT :
**          nent      = Number of entities extracted from symbol.
**          ierr      = Non-zero if an error occured extracting entities.
**    RETURNS: none
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
void ubf_explode_syminstance(nclkey,label,isub,nent,flg,ierr)
UM_int4 *nclkey;
UM_f77_str_ptr label;
UM_int4 *isub,*nent;
UM_int2 *flg,*ierr;
{
	int stat,i,flag=*flg;
	char *fstr;
/*
.....Initialize routine
*/
	*ierr = 0;
	Skey = *nclkey;
	fstr = UM_cstr_of_f77_str(label);
	strncpy(Slabel,fstr,NCL_MAX_LABEL);
	for (i=0;i<NCL_MAX_LABEL;i++)
	{
		if (Slabel[i] == ' ')
		{
			Slabel[i] = '\0';
			break;
		}
	}
	if (flag == 3)
	{
		if (!Smod_def && Sflag == 2)
		{
			Smod_def = UU_TRUE;
			Smod = (*isub < 0)? -1 : 1;
		}
		if (Sflag != 2) Ssub = *isub;
		else Ssub = Smod*(Stnent + 1);
	}
	else
		Ssub = *isub;
/*
.....Explode symbol
*/
	stat = ubu_explode_syminstance(flag);
	*nent = Snent;
	if (flag == 4) Snents += Snent;
	if (stat == UB_FAILURE) *ierr = 5;
	else if (stat != UU_SUCCESS) *ierr = stat;
}

/*********************************************************************
**    E_FUNCTION:  int ubu_explode_syminstance(iflag)
**       This function explodes a symbol instance.
**    PARAMETERS   
**       INPUT  :
**          flag    - 1 = Use CADD interface.
**                    2 = Use CAM interface.
**                    3 = From CAM command.
**                    4 = ALL instances.
**       OUTPUT : none. 
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubu_explode_syminstance(flag)
int flag;
{
	struct UB_instance_rec sym;
	int cmdreject;	/* command reject flag */
	struct UC_entitydatabag subent;
	struct NCL_fixed_databag *sptr,e;
	UM_PICKENT pickpath;
	int dsegid;
	int i,j;
	int numint=0,numsym,level;
	int dummy=0;
	int status,stat;
	int len,scalsub=1;
	int cmdsub,errfl=0,mod_len=6;
	UM_int2 i2stat;
	char label[NCL_MAX_LABEL_AND_SUBSCRIPT];
	char cmdstr[256],tlabel[256];
	char str[256];
	static char *mods[] = {"PLUS","RESET"};
	NCL_cmdbuf cmdbuf;
	UU_LOGICAL startover,scalvar=UU_FALSE,delfl=UU_FALSE;
	UU_LOGICAL cmd_subfl,cmd_parfl=UU_TRUE;
	UU_LOGICAL init=UU_TRUE;
	UU_KEY_ID key,*keys;
	UU_LIST key_list;

	uu_denter(UU_BTRC, (us, "ubu_explode_syminstance()"));
	status = UU_SUCCESS; /* assume success */
	/* set mark for long jump if there is a command reject */
	UD_MARK(cmdreject, UU_FALSE);	
	if (!cmdreject)	/* then no command reject encountered */
	{
		numsym = 1;
/*
.....Get label for decomposition
*/
		if (flag == 2)
		{
			Sflag = 2;
			Stnent = 0;
			Slen_exceed = UU_FALSE;
			ncl_init_cmdbuf(&cmdbuf);
			stat = ncl_add_name(&cmdbuf,1);
			if (stat == NCL_DONE) 
			{
				ud_wrerr("A label is required.");
				goto error;
			}
			else if (cmdbuf.cur_str[0] == '\0')
				goto failed;
		}
/*
.....Use key from command
*/
		if (flag == 3)
			sym.key = Skey;
/*
.....Get all symbol instances from unibase
*/
		else if (flag == 4)
		{
			uu_list_init(&key_list,sizeof(UU_KEY_ID),10,10);
			delfl = UU_TRUE;
			numsym=0;
			e.key = 0;
			startover = UU_TRUE;
			while (um_getallobjs(startover, &e.key) == 0)
			{
				startover = UU_FALSE;
				ur_retrieve_data_relnum(e.key,&e.rel_num);
				if (e.rel_num == UB_INSTANCE_REL)
				{
					uu_list_push(&key_list,&e.key);
					numsym++; numint++;
				}
			}
			if (numsym > 0) keys = (UU_KEY_ID *)UU_LIST_ARRAY(&key_list);
		}
/*
.....Get picked symbols to decompose
*/
		else
		{
			ud_lgeo(UU_TRUE, UD_symbol);
			ud_ldas(UD_DASSELECT, UB_SYMBOL, 22, UU_NULL, 100, &numint, UD_NODEFAULT);
		/* prompt is: Pick instance to be decomposed. */
			if (numint <= 0)	/* no proper user interactions */
				goto failed;
			uu_list_init(&key_list,sizeof(UU_KEY_ID),10,10);
			delfl = UU_TRUE;
			numsym=0;
			level = 1;  /* picking level */
			startover = UU_TRUE; /* get all keys for entities picked */
			while ((status == UU_SUCCESS)
					&& (ud_gnxt(startover, UU_NULL, &key, level)))
			{
				startover = UU_FALSE;
				uu_list_push(&key_list,&key);
				numsym++; /* number of symbols to decompose */
			}
			if (numsym > 0) keys = (UU_KEY_ID *)UU_LIST_ARRAY(&key_list);
		}
/*
.....Loop through all selected symbols and decompose each one
*/
		for (j=0;j<numsym;j++)
		{
			if (numint > 0)
			{
				sym.key = keys[j];
			}
			if (ub_retrieve_sym(&sym, sizeof(sym)) != UU_SUCCESS)
				goto failed;
			if (sym.rel_num != UB_INSTANCE_REL)
			{
				if (flag < 3)
					uu_uerror2(UB_SYMBOL, 37, sym.rel_num, "ubu_explode_syminstance");
				/* error message: Entity picked has relation type %d and is not a 
				 * symbol instance (%s). */
				goto failed;
			}
/*
.....Get optional # of entities variable and output command.
*/
			if (flag == 2)
			{
				if (j == 0)
				{
					ncl_add_token(&cmdbuf,NCL_decomp,NCL_nocomma);
					len = strlen(cmdbuf.cur_str);
					ncl_get_str(str,673);
					if (strlen(str) > 0) scalvar = UU_TRUE;
					if (scalvar) len += strlen(str);
					S_get_label(&cmdbuf.cur_str,&cmdstr,&cmdsub,&cmd_subfl,&cmd_parfl,&status);
					if (status != 0) goto error;
					if (!cmd_subfl) cmd_parfl = UU_TRUE;
/*
.....Include the label subscript in Stnent so the subscripts based on it
.....will start in the correct location.
*/
					Stnent += cmdsub - 1;
					if (Stnent < 0) Stnent = 0;
					if (numsym > 1) len += mod_len;
				}
				ncl_format_label(sym.label,sym.subscr,label,0);
				len += strlen(label);
/*
.....Output current command if the length will be too long when the
.....current label is added.  Then create a new command and start over
.....at the current label.
*/
				if (len > NCL_MAX_COMLINE)
				{
					if (scalvar) 
					{
						ncl_add_token(&cmdbuf,str,NCL_comma);
						if (Slen_exceed)
							ncl_add_token(&cmdbuf,mods[0],NCL_nocomma);
						else
							ncl_add_token(&cmdbuf,mods[1],NCL_nocomma);
					}
					Slen_exceed = UU_TRUE;
					ncl_add_cmdbuf(&cmdbuf);
					ncl_call(&cmdbuf);
					ncl_reset_cmdbuf();
					ncl_format_label(cmdstr,Stnent+1,tlabel,!cmd_parfl);
					ncl_add_token(&cmdbuf,tlabel,NCL_nocomma);
					ncl_add_token(&cmdbuf,"=",NCL_nocomma);
					ncl_add_token(&cmdbuf,NCL_decomp,NCL_nocomma);
					len = strlen(cmdbuf.cur_str);
					if (scalvar) len += strlen(str);
					len += mod_len;
				}
/*
.....All instances have been added so the commnd is output
*/
				else if (numsym - j == 1)
				{
					ncl_add_token(&cmdbuf,label,NCL_comma);
					if (scalvar && !Slen_exceed)
					{
						ncl_add_token(&cmdbuf,str,NCL_comma);
						if (numsym > 1) ncl_add_token(&cmdbuf,mods[1],NCL_nocomma);
					}
					else if (scalvar)
					{
						ncl_add_token(&cmdbuf,str,NCL_comma);
						ncl_add_token(&cmdbuf,mods[0],NCL_nocomma);
					}
					ncl_add_cmdbuf(&cmdbuf);
					ncl_call(&cmdbuf);
					Sflag = 0;
					Stnent = 0;
					Smod_def = UU_FALSE;
					Slen_exceed = UU_FALSE;
					goto done;
				}
				ncl_add_token(&cmdbuf,label,NCL_comma);
				continue;
			}

			if (ur_retrieve_disp_segid(sym.key, &dsegid) != 0)
			{
				if (flag == 1)
					uu_uerror2(UB_SYMBOL, 38, sym.key, "ubu_explode_syminstance");
				/* error message: Unable to retrieve display segment id for key=%d,
				 * (%s).
				 */
				goto failed;
			}
/*
.....Reset the number of extracted entities
*/
			if (flag != 4 || j == 0) Snent = 0;
			/* now display the geometric subentities */
			for (i=0; i<sym.no_geom; i++)
			{
				subent.key = sym.geom[i];
				if (subent.key == 0) continue;
				if (uc_retrieve_data(&subent, sizeof(subent))
					!= UU_SUCCESS) goto failed;
				if (ur_update_displayable(subent.key, UM_DISPLAYABLE) != 0)
				{
					if (flag == 1)
						uu_uerror3(UB_SYMBOL, 65, subent.key, subent.rel_num,
								"ubu_explode_syminstance");
					/* error is: Can't set displayability for subentity with key:
					 * %d, relation: %d  (%s) */
					goto failed;
				}
				sptr = (struct NCL_fixed_databag *)&subent;
/*
........Label the entity
...........Using original label of sub-entity
*/
				if (!strncmp(sptr->label,"@UN",3)) 
				{
					if (flag == 1)
					{
						status = ncl_label_wf(sptr->rel_num,sptr->label,&sptr->subscr,
							sptr->key,&i2stat);
					}
/*
...........Using provided label and subscript
*/
					else
					{
						status = ncl_label_multiple(Slabel,&Ssub,sptr->label,
							&sptr->subscr);
						if (status != UU_SUCCESS) goto error;
						status = ncl_store_wf2(sptr->key,sptr->rel_num,sptr->label,
							sptr->subscr);
						if (status != UU_SUCCESS) goto failed;
						i2stat = 1;
					}
				}
				else i2stat = -1;
/*
........Store entity
*/
				if (i2stat == 1)
				{
					status = ur_update_data_fixed(sptr);
					if (status == UU_SUCCESS && flag == 1)
						status = ncl_store_wf1(sptr->key);
					if (status != UU_SUCCESS) goto failed;
					Snent++;
				}
/*
........Display entity
*/
				if (uc_display(&subent) != UU_SUCCESS)
					goto failed;
				if (flag == 3 || flag == 4) ncl_srcctl_put(subent.key);
			}
/*
.....Store the total number of extracted entities for subscripting.
.....If a previous call to this routine was with flag = 2, then it
.....is possible the number of extracted entities will be needed for
.....the label subscript when the command is generated.
*/
			if (Sflag == 2) Stnent += Snent;
			if (flag != 4) Snents += Snent;
			/* now delete (used to display) the text nodes */
			for (i=0; i<sym.no_text_nod; i++)
			{
				subent.key = sym.text_nod[i].text_key;
				if (subent.key == 0) continue;
				if (uc_delete(subent.key) != UU_SUCCESS)
					goto failed;
				/*
				if (uc_retrieve_data(&subent, sizeof(subent))
					!= UU_SUCCESS) goto failed;
				if (ur_update_displayable(subent.key, UM_DISPLAYABLE) != 0)
				{
					if (flag == 1)
						uu_uerror3(UB_SYMBOL, 65, subent.key, subent.rel_num,
									"ubu_explode_syminstance");
					 * error is: Can't set displayability for subentity with key:
					 * %d, relation: %d  (%s) *
					goto failed;
				}
				if (uc_display(&subent) != UU_SUCCESS)
					goto failed;
				*/
			}
			/* note, we do not display snap nodes since they should be needed any
			 * longer */
			for (i=0; i<sym.no_snap_nod; i++)
			{
				if (sym.snap_nod[i].snap_key == 0) continue;
				if (uc_delete(sym.snap_nod[i].snap_key) != UU_SUCCESS)
					goto failed;
			}

			
			/* delete instance record from master's list */
			if (ubi_update_inst_data_in_msym(&sym, UU_NULL, UB_UNKNOWN, dummy) 
						!= UU_SUCCESS) goto failed;

			/* delete instance record */
			if (ur_delete_all(sym.key) != 0)
			{
				if (flag == 1)
					uu_uerror2(UB_SYMBOL, 35, sym.key, "ubu_explode_syminstance");
				/* Error message: Unable to delete symbol instance with key=%d (%s). */
				goto failed;
			}

			if (dsegid >= 0) 
				uv_delsegs(dsegid);
		}
	}/* end no command reject */
	else /* command reject hit */
	{}

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
error:
	if (Sflag == 2)
	{
		Stnent = 0;
		Snents = 0;
		Sflag = 0;
		Smod_def = UU_FALSE;
		Slen_exceed = UU_FALSE;
	}
done:;
	if (delfl) uu_list_free(&key_list);
	UD_UNMARK(cmdreject);
	uu_dexit;
	return(status);
}	
