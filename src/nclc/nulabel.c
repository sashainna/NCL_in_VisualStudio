/***********************************************************************
**    NAME         :  nulabel.c
**       CONTAINS: user interface label manipulation routines
**       int nclu_toggle_labels()
**       int nclu_toggle_leader()
**       int nclu_alter_label()
**		 	int nclu_rename_geom()
**       ncl_rename_geom(sav_key, UM_addr_of_f77_str(nclolab), jsub,
**                       UM_addr_of_f77_str(nclnlab), isub, ierr)   
**       ncl_check_name(new_label,flag,flag2)
**		 	int ncl_update_data(e,  nkey, relnum)
**		 	int ncl_update_labl(eptr,str)
**		 	int ncl_set_label_attr(flt,key)
**		 	nclu_toggle_labels1(choice,key)
**		 	nclu_alter_label1(key,abs_del,x,y,z)
**		 	static int S_save_modfile()
**		 	ncl_label_modals()
**		 	void lblini(lbset,ldset)
**
**     NOTE: This file used to contain the following two routines that
**           were removed by M. Gump 10-28-88 due to them being obsolete.
**           They used to be called by zkbpro.itr for "B" and "b".  The 
**           calls were replaced with calls to nclu_toggle_labels.
**       int nclu_display_labels
**       int nclu_erase_labels
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nulabel.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:08
*********************************************************************/
#include <ctype.h>
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dselmask.h"
#include "mdclass.h"
#include "class.h"
#include "mdrel.h"
#include "mlab.h"
#include "mdcoord.h"
#include "mdebug.h"
#include "nclmodals.h"
#include "nclinp.h"
#include "nclfc.h"
#include "nccs.h"
#include "mcrv.h"
#include "mdattr.h"
#include "mattr.h"
#include "mdeval.h"
#include "mfort.h"
#include "mdpick.h"
#include "nclcmd.h"
#include "nkeywd.h"
#include "nclvx.h"
#include "view.h"
#include "modef.h"
#include "lcom.h"
#include "lumb.h"
#include "gsegac.h"

UU_LOGICAL
ncl_label_on(lab)
UU_LOGICAL lab;
	{
	return (lab > 0? lab : (UU_TRUE + (-lab)));
	}
UU_LOGICAL
ncl_label_off(lab)
UU_LOGICAL lab;
	{
	return (lab < 1? lab : (UU_TRUE + (-lab)));
	}

void ncl_rename_geom();
void ncl_check_name();
void ncl_update_labl();

static int pikmode=UD_STRING;
extern char uw_color_name[64][96];
/*********************************************************************
**    E_FUNCTION     : nclu_toggle_labels
**       Turn labels on or off.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_toggle_labels(choice)
int   choice;
{
   int   status;
   int   numint;
   UM_int2 lblst,type;
   UU_LOGICAL  initialize;
   UU_LOGICAL  labels_on;
   struct UC_entitydatabag e;
   struct UC_attributedatabag attr;

   uu_denter(UU_MTRC,(us,"nclu_toggle_labels()"));
	labels_on = NCL_display_label ;
   switch (choice)
	{
		case 1:
			NCL_display_label = UU_TRUE;
			break;
		case 2:
			NCL_display_label = UU_FALSE;
			break;
	}
   
/* pick entities to change label status */
/*
.....Changed mask to UD_ncl_labels;
.....Bobby  -  2/24/94
*/
	ud_lgeo(UU_TRUE, UD_ncl_labels);
	ud_ldas(UD_DASSELECT,/*pick entity*/UM_MODEL,223, UU_NULL,1,
	&numint,UD_NODEFAULT);
	initialize = UU_TRUE;
	savlab();
	if (NCL_display_label) setlbt();
	else setlbf();
	while(ud_gnxt(initialize, UU_NULL, &e.key, 1) == UU_TRUE)
	{
		initialize = UU_FALSE;
		status = uc_retrieve_data(&e, sizeof(e));
/* display  */

		if (ncl_label_type(e.rel_num) == UU_SUCCESS)
		{
         /*  update the label_on flag. kathy */
			attr.key = e.key;
			status = ur_retrieve_attr(&attr);
			if (status == 0)
			{
/*NCL: To preserve handle to modified label location */
/** attr.label_on = NCL_display_label; **/
				switch (choice)
				{
				 case 1:
					ncl_get_type(e.rel_num,&type);
/*
.....Turn leader lines on or off according to the leader line flag
*/
					lblchk(&type,&lblst);
					if(!ncl_get_label_ldr(lblst))
						ncl_set_label_ldr(&attr.label_on,0);
					else
						ncl_set_label_ldr(&attr.label_on,1);
					ncl_set_label_on(&attr.label_on,1);
					break;
				 case 2:
					ncl_set_label_on(&attr.label_on,0);
					break;
				}
				ur_update_attr(&attr);
			}
			uc_display(&e);
		}
	}

/* reset the NCL_display_label global */
	NCL_display_label = labels_on;
	reslab();
	uu_dexit;
}
/*********************************************************************
**    E_FUNCTION     : nclu_toggle_leader
**       Turn leader lines on or off.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_toggle_leader(choice)
int   choice;
{
   int   status;
   int   numint;
   UU_LOGICAL  initialize;
   UU_LOGICAL  ldr_on;
   struct UC_entitydatabag e;
   struct UC_attributedatabag attr;

	ldr_on = NCL_display_leader ;
   switch (choice)
	{
		case 1:
			NCL_display_leader = UU_TRUE;
			break;
		case 2:
			NCL_display_leader = UU_FALSE;
			break;
	}
   
/* pick entities to change label status */
	ud_lgeo(UU_TRUE, UD_ncl_labels);
	ud_ldas(UD_DASSELECT,/*pick entity*/UM_MODEL,223, UU_NULL,1,
	&numint,UD_NODEFAULT);
	initialize = UU_TRUE;
	savldr();
	if (NCL_display_leader) setldt();
	else setldf();
	while(ud_gnxt(initialize, UU_NULL, &e.key, 1) == UU_TRUE)
	{
		initialize = UU_FALSE;
		status = uc_retrieve_data(&e, sizeof(e));
		if (ncl_label_type(e.rel_num) == UU_SUCCESS)
		{
			attr.key = e.key;
			status = ur_retrieve_attr(&attr);
			if (status == 0)
			{
				switch (choice)
				{
				 case 1:
					ncl_set_label_ldr(&attr.label_on,1);
					break;
				 case 2:
					ncl_set_label_ldr(&attr.label_on,0);
					break;
				}
				ur_update_attr(&attr);
			}
			uc_display(&e);
		}
	}

/* reset the NCL_display_label global */
	NCL_display_leader = ldr_on;
	resldr();
	uu_dexit;
}
/*********************************************************************
**    E_FUNCTION     : nclu_alter_label()
**       Alter label location.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_alter_label()
{
	int   status;
	int   numint;
	UM_int2 type,lblst;
	struct NCL_fixed_databag e;
	struct UC_attributedatabag *attp;
	struct UC_attributedatabag attr;
	UD_PLOCREC pick;	/* the pick id and location */
/**	UM_coord altpos; **/
	UM_PLOCREC pent;
    UD_NDCLOCREC altpos;
	UM_coord pt,zpt;
	UM_vector uv,vec,unit;
	UV_vport vvport;
	UV_view view;
	UM_coord pkpt;
	UM_vector vpnorm;
	UU_REAL tol;
	UD_PLOCREC pickent;
	UM_real8 tol8;
	int segno;

	int cmdreject;		 /* command reject flag */
   UM_transf tfmat;

	gettol (&tol8);
	tol = tol8;
	attp = (struct UC_attributedatabag *) &attr;

	uu_denter(UU_MTRC,(us,"nclu_alter_label()"));

	/* save current label settings 
	savlab(); */ 
	/* set current label settings to be true 
	setlbt(); */

	/* limit das to NCL geometry */
/*
.....Changed mask to UD_ncl_labels;
.....Bobby  -  2/24/94
*/
	ud_lgeo(UU_TRUE, UD_ncl_labels);

	/* set mark for long jump if there is a command reject */
	UD_MARK(cmdreject, UU_FALSE); 

	if (!cmdreject)	/* then no command reject encountered, go on */
	{
		/* pick entity to alter label status */
		/* "Pick geometry" - from MISC PROMPTS */
		while (UU_TRUE)
		{
			ud_ldas(UD_DASPCKLOC,UA_NCL,238,&pick,1,&numint,UD_NODEFAULT);
			if (numint < 1)
				break;

			/* retrieve entity */
			status = um_d_pickresolve(&pick,1,&pent);
			e.key = um_get_pickkey(&pent.pent,1);
/*
.....calculate the top of the bounding box of the entity
*/
			segno = pick.ppath.pickpath[0];

			ug_altersegbox(segno,zpt);

			/* user picked wireframe geometry */
			if (ur_retrieve_data_relnum(e.key, &e.rel_num) == UU_SUCCESS &&
				ncl_label_type(e.rel_num) != UU_SUCCESS)
			{
				uu_uerror0(UA_NCL,7);
				continue;
			}

			ncl_retrieve_data_fixed (&e);

			/*  get attribute structure */
			if (ncl_retrieve_ent (&e,attp,tfmat) == UU_SUCCESS)
			{
				/* prompt for new label location */
				/*"Select new label location (DONE for original location)" */
				status = ud_ldas(UD_DASCART,UA_NCL,512,&altpos,1,&numint,UD_NODEFAULT);

				if ((status != 1) || (numint != 1))
				{
/*
.....restore original label location and blacken the previous label box
*/
					
					NCL_alter= 1;
					uc_display(&e);
					NCL_alter= 0;
/*
.....if label display is on and its loaction is altered restore it to defaultlocation
*/
					if (ncl_get_label_on(attp->label_on) && 
						ncl_get_label_alter(attp->label_on))
						ncl_delete_labloc(&attp->label_on);
					ncl_set_label_alter(&attp->label_on,0);
				}
				else
				{
/*
.....Get the current viewport and the corresponding view
.....project the picked location point onto the plane prallel to the view plane
.....and on which the center of the bounding box of the entity lies
*/
					if (altpos.choice == UD_LOCATOR)
					{
						uv_getvpid(UV_act_screen[0].vports[0], &vvport);
						uv_getvid(vvport.cur_view, &view);
						um_unitvc(view.cur_pln_norm,unit);
						um_nptpln(altpos.cord,zpt,unit,altpos.cord);
					}
/*
.....Pick a position on the entity picked for start of leader line
*/
					ud_unlimit();
					do
					{
/*
.....set deafult location for leader line on the entity
*/
						ncl_default_labloc (&e,tfmat, pt);
/*
.....display leader line according to the leader line flag
*/
						ncl_get_type(e.rel_num,&type);
						lblchk(&type,&lblst);
						if (!ncl_get_label_ldr(attp->label_on))
						{
							if(!ncl_get_label_ldr(lblst)) 
								break;
							else 
								if(!ncl_get_label_on(attp->label_on))
									ncl_set_label_ldr(&attp->label_on,1);
						}
						else	
						{
							if(!ncl_get_label_ldr(lblst) )
							{
								if(!ncl_get_label_on(attp->label_on))
								{
									ncl_set_label_ldr(&attp->label_on,0);
									break;
								}
							}
						}
						status = ua_dl_pldas(UD_DASPCKLOC, UA_NCL, 513, &pickent, 1,
						   &numint, 1);
/*
......restore original label location as start of leader line
*/
						ncl_default_labloc (&e,tfmat, pt);
						if (numint<1)
							break;
/*
..... different surface - start new command
*/
						if (e.key != (UU_KEY_ID)pickent.ppath.pickpath[0])
						{
							ud_wrerr("Different entitiy picked.");
							continue;
						}
						else
						{
/*
.....Get point on entity
*/
							if (numint != 0)
							{
								um_vpnorm(pickent.pndc.transform,vpnorm);
/*
.....Project pick locations onto view plane
*/
								gndcw3(&pkpt[0],&pkpt[1],&pkpt[2],
									pickent.pndc.cord[0],pickent.pndc.cord[1],
									pickent.pndc.cord[2]);
/*
.....Project picked point onto entity
*/
								status = ncl_pickptonent(&e,pkpt,vpnorm,tol,pt,uv,vec);
							}
							break;
						}
						
					}while(UU_TRUE);
/*
.....If the altered location is different form the previous location ,
.....blacken the previos labelbox.
*/
					if(um_dcccc(altpos.cord,e.labloc)>UM_FUZZ)
					{
						NCL_alter= 1;
						uc_display(&e);
						NCL_alter= 0;
					}
					ncl_add_labloc(&e,&attp->label_on, &altpos);
					ncl_add_ldrloc(&e,&attp->label_on,pt);
				}
				ur_update_attr(&attr);
				uc_display(&e); /* display */
			}
		}
	}/* end no command reject */

	/* command reject hit */
	else 
	{ }
done:;
	/* reset label setting to original 
	reslab(); */
	UD_UNMARK(cmdreject);	/* take mark off long jump stack */

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_rename_geom
**      To rename CADD geometry.  Can not rename subscripted geometry 
**      or name geometry to be subscripted.
**
** 1-24-91: Enhanced to rename CAM geometry as well.  The state of
**      REDEFINE (in CADD) and CANON (in CAM) is used to determine validity
**      of renaming request.
**	
**    PARAMETERS   
**       INPUT  : 
**          putsrc    =  UU_TRUE if the RENAME command should be placed
**				             in the part program.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : When renaming geometry A to geometry B and B exists,
**                   B will be deleted if REDEFINE is ON, without user
**                   confirmation.
*********************************************************************/
void nclu_rename_geom(putsrc)
UU_LOGICAL putsrc;
	{
	int numint;
	int status, i, relnum, itmp,j,ix;
	int nsub = 0;
   int flag = 0;
   int flag2 = 0;
	UD_STRREC strrec;
	UM_PLOCREC pick;
	char buf[80];  						/* temporary working string */
	UU_KEY_ID   sav_key;
	UM_int2   ierr;                  /* FORTRAN error flag */
	UM_int4   isub=0;                /* subscript */
	UM_int4   jsub;                  /* subscript */
	char old_label[NCL_MAX_LABEL+1];   /* old label */
	char new_label[NCL_MAX_LABEL+1];   /* new label */
	UM_f77_str nclolab, nclnlab; 		/* old and new label pointers */
	struct NCL_fixed_databag e;
	char lnum[20];
	NCL_cmdbuf cmdbuf;

	uu_denter(UU_MTRC,(us,"nclu_rename_geom"));

/*
.....Set-up FORTRAN string pointers to old and new labels.
*/
	UM_init_f77_str(nclolab, old_label, NCL_MAX_LABEL);
	UM_init_f77_str(nclnlab, new_label, NCL_MAX_LABEL);

	status = NCL_OKINPUT;
	while (status != NCL_DONE)
	{
		nsub = 0;
/*
.....Pick the geometry to RENAME
*/
		ud_lgeo(UU_TRUE, UD_ncl_labels);
		status = ua_dl_pldas(UD_DASPCKLOC, UM_MODEL, 322, &pick, 1, &numint, 1);
		if (numint <= 0) 
			goto done;

		e.key = um_get_pickkey(&pick.pent, 1);
		if (ncl_retrieve_data(&e, sizeof(e)) == UU_SUCCESS)
		{
/* 
.....Filter out drafting, dimension, symbols, etc
*/
		if (ncl_label_type(e.rel_num) == UU_SUCCESS)
			{
/*
.....Retrieve original label and store in old_label[].  
.....Also store old key and relation number in sav_key and relnum.
*/
			ncl_get_label_and_subscr(&e, buf, &itmp);
			sprintf(old_label,"%-64s",buf);
			jsub = itmp;
			sav_key = e.key;
			relnum = e.rel_num;
repeat:
/*
.....Initialize string to NULL.
*/
			buf[0] = '\0';
			strrec.instring = buf;
/*
.....Prompt for new label string 'by text' 
*/
/*			status = ud_ldas(UD_DASSTRINGDEF, UA_NCL, 1, &strrec, 80,
						&numint, UD_NODEFAULT);*/
/*
.....User went into 'by pick' mode instead
*/
/*			if (status == UD_DASALTACTION)*/
			{
/*
.....Pick an entity
*/
				ud_dtsel(pikmode,1,1);
				ud_dtsel_str(UU_TRUE);
				status = ua_dl_pldas(UD_DASPCKLOC, UA_NCL, 1, &pick, 1,
                    &numint, 1);
				if (pick.ploc.choice == 1) pikmode = UD_STRING;
				else pikmode = UD_PICK;
/*
.....User picked no geometry, 
.....go back and prompt for geometry to rename.
*/
				if (numint <= 0)
					continue;
/*
.....Extract geometry label from picked entity
*/
				strcpy(buf,pick.ploc.label);
/*				ncl_picktostr(&pick,buf);*/
			}
/*
.....User used the DONE key (or something like that)
.....Go back and prompt for geometry to rename.
*/
/*			else if ((numint <= 0) || (strlen(strrec.instring) == 0))
				continue;*/

/*
.....By this point we have a non-NULL character string.
.....Lets start with checking for subscripted geometry.
*/

			ix = 0;
			for (i=0; i <strlen(buf); i++)
			{
/*
.....Allow for subscripted label
.....Bobby  -  7/16/96
*/
				if (buf[i] == '(')
				{
					ix = i;
					for (j=i;j<strlen(buf);j++)
					{
						if (buf[j] == ')') break;
					}
					if (j != strlen(buf) && j > i+1)
					{
						strncpy(lnum,&buf[i+1],j-i-1);
						lnum[j-i-1] = '\0';
						if (ul_to_number(lnum,&nsub) != UU_SUCCESS)
						{
							uu_uerror0(UQ_CALCERROR, 50);
							goto repeat;
						}
					}
				}
/*
.....Upper-case the string.
*/
				if (islower(buf[i]) != 0)
					buf[i] = toupper(buf[i]);
			}
/*
.....Format string as a label expected by FORTRAN.
*/
			isub = nsub;
	     	buf[i] = '\0';
	     	sprintf(new_label, "%-64s",buf);

         ncl_check_name(new_label, &flag, &flag2);

			if ((flag==1) || (flag2!=0))
			{
				uu_uerror0(UQ_CALCERROR, 50);
            goto repeat;
			}
			if (ix != 0) new_label[ix] = '\0';

/*
.....ncl_rename_geom will check to see if the label,
.....is already in use, if so it will go back to 
.....nclu_rename_geom so that an error message will
.....be displayed and prompt user to make another
.....choice, it will delete the old name
.....from RANFIL and update RANFIL with the new label.
*/

			ncl_rename_geom(&sav_key, UM_addr_of_f77_str(nclolab), &jsub,
					UM_addr_of_f77_str(nclnlab), &isub, &ierr);
   
/*
.....If ierr is equal to 316 then the      
.....new choice for a label is already in use
.....and a new selection needs to be made.
*/
			if (ierr==316)
			{
				uu_uerror0(UM_MODEL,316);
				ierr = '\0';
				goto repeat;
			}
/*  
.....If ierr is equal to 318, then the choices
.....are not of the same type and an error
.....message will be displayed.
*/
			if (ierr==318)
			{
				uu_uerror0(UM_MODEL,/* same type only*/318);
				ierr='\0';
				goto repeat;
			}
/*
.....Put command in source file
.....if in CAM mode
*/
			itmp = strlen(old_label);
			ul_strip_blanks(old_label,&itmp);
			if (putsrc && itmp != 0)
			{
				ncl_init_cmdbuf(&cmdbuf);
				ncl_add_token(&cmdbuf,NCL_rename,NCL_nocomma);
				if (jsub == 0)
					ncl_add_token(&cmdbuf,old_label,NCL_comma);
				else
				{
					itmp = strlen(old_label);
					ul_strip_blanks(old_label,&itmp);
					sprintf(buf,"%s(%d)",old_label,jsub);
					ncl_add_token(&cmdbuf,buf,NCL_comma);
				}
				if (isub == 0)
					ncl_add_token(&cmdbuf,new_label,NCL_nocomma);
				else
				{
					itmp = strlen(new_label);
					ul_strip_blanks(new_label,&itmp);
					sprintf(buf,"%s(%d)",new_label,isub);
					ncl_add_token(&cmdbuf,buf,NCL_comma);
				}
				ncl_add_cmdbuf(&cmdbuf);
				ncl_put_in_src(cmdbuf);
			}
		}
		else
			uu_uerror0(UM_MODEL,/* illegal entity picked*/317);
		}
	}

done:
	ud_dtsel(UD_PICK,1,1);
	ud_dtsel_str(UU_FALSE);
	return;
}




/*********************************************************************
**    E_FUNCTION     : ncl_rename_geom
**      To rename CADD geometry.  Can not rename subscripted geometry
**      or name geometry to be subscripted.
**
** 1-24-91: Enhanced to rename CAM geometry as well.  The state of
**      REDEFINE (in CADD) and CANON (in CAM) is used to determine validity
**      of renaming request.
**
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : When renaming geometry A to geometry B and B exists,
**                   B will be deleted if REDEFINE is ON, without user
**                   confirmation.
*********************************************************************/
void ncl_rename_geom(key, nclolab, jsub, nclnlab, ksub, ierr)
UU_KEY_ID *key;
UM_f77_str_ptr nclolab;
UM_f77_str_ptr nclnlab;
UM_int4 *ksub;
UM_int2 *ierr;
UM_int4 *jsub;

{
   int i,relnum;
   UU_KEY_ID  nkey;                 /* key for the new label*/
   UM_int2   ncltypo,ncltypn;       /* CAM type (equiv with REL_NUM) */
   struct NCL_fixed_databag e;
	struct UC_attributedatabag attr;
   UM_int2 ifnd,ifl41;              /*Flags to store fortran state*/
   char *new_label;
	UM_int4 isub;


/*
.....Check for existing geometry by this name.
.....ifnd=1  -> label used.
.....ifl41=1 -> CANON/ON
*/
	*ierr = 0;
	isub = *ksub;
   chklab(nclnlab, &nkey, &isub, &ifnd, &ifl41);
/*
.....Label used and REDEFINE is NO
*/
   if (ifnd == 1 && ifl41 == 0)
     {
/* 
.....If label is already used
.....the program will return to nclu_rename_geom()
.....where the error message will be displayed.
*/
       *ierr=316;
       goto done;
     }  

   ur_retrieve_data_relnum(*key,&relnum);
   ncl_get_type(relnum,&ncltypo);
   if (nkey != 0)
     {
/*
.....if existing geomtry name used, make sure geometry is of same type.
*/
       e.key = nkey;
       if (uc_retrieve_data(&e, sizeof(e)) == UU_SUCCESS)
         {
/*
.....Convert relation numbers to CAM type
*/
           ncl_get_type(e.rel_num,&ncltypn);
/*
.....Entities are not of the same type,
.....will display an error message in
.....nclu_rename_geom().
*/
           if (ncltypn != ncltypo)
            {
            *ierr=318;
            goto done;
            }
         }
     }

   e.key = *key;
   new_label = UM_cstr_of_f77_str(nclnlab);
   i = strlen(new_label);
/*
.....format the label string
*/
	while (i<NCL_MAX_LABEL) new_label[i++] = ' ';
	new_label[NCL_MAX_LABEL-1] = '\0';
   ncl_update_labl(&e,new_label, isub);
/*
.....Now delete the old label from the RANFIL.
.....The does not affect the UNIBASE.
*/
	if (e.rel_num != UB_INSTANCE_REL)
	{
		randel(nclolab, jsub, &ierr);
/*
.....Proceed with updating RANFIL with new label and the
.....original key.  This will cause previously existing
.....geometry(if any) to be deleted from the Unibase if
.....REDEFINE is on.
*/
		ranstr(nclnlab,&isub,&ncltypo,key,&ierr);
	}


	attr.key = e.key;
	ur_retrieve_attr(&attr);
/*
.....if label display is on display the entity
*/
	if (ncl_get_label_on(attr.label_on))
		{
		uc_retrieve_data(&e, sizeof(e));
		uc_display(&e);
		}

done:

   return;  

}

/*********************************************************************
**    E_FUNCTION     : ncl_check_name(new_label, flag, flag2)
**      Checks the validity of the new given label, and sends a flag
**      back if it is an invalid name so that an error message may be
**      called.  If an error message is called, no changes will be made
**      to the current labels.
**
**    PARAMETERS
**       INPUT  :
**          new_label  -  Label of geometry to check for validity.
**       OUTPUT :
**          flag       -  Returns non-zero if name is invalid.
**          flag2      -  Returns non-zero if name is invalid
**                        (parenthesis check).
**    RETURNS      : none
**    SIDE EFFECTS : none
*********************************************************************/
void ncl_check_name(new_label, flag, flag2)
char *new_label;
int *flag;
int *flag2;
{
	int i = 1;
	UM_int2 inum;
	int nc;
	char lstr[64];
	UM_f77_str label;
/*
.....Flags set to zero to insure that
.....any old flags aren't remaining
*/
	*flag=0;
	*flag2=0;
/*
.....Make sure it is not a vocabulary word
*/
	nc = strlen(new_label);
	strcpy(lstr,new_label);
	ul_strip_blanks(lstr,&nc);
	if (nc <= 6)
	{
		UM_init_f77_str(label,lstr,8);
		voccad(UM_addr_of_f77_str(label),&inum);
		if (inum != 0)
		{
			*flag = 1;
			goto done;
		}
	}
/*
.....A check to make sure that the first element of new_label
.....is a letter and nothing else. If it is not a letter then
.....the flag is set to 1 and will go directly back to nclu_rename_geom
.....with the new flag value so that an error message can be called.
*/

	if (!(((new_label[i]>='A') && (new_label[i]<='Z')) ||
		((new_label[i]>='0') && (new_label[i]<='9'))))
	{
		*flag=1;
	}
/*
.....If the first element of new_label is a letter then
.....a check is performed to make sure that the remaining
.....portion of new_label has only valid characters; letters,
.....numbers, and/or parentheses. If there is an invalid
.....character, then it will set flag to 1 and return to
.....nclu_rename_geom to call an error message.  The while loop
.....is performed until a space is found in new_label.
*/
	else
	{
		while (new_label[i]!=' ')
		{
			if (!((new_label[i]>='A' && new_label[i]<='Z') ||
				(new_label[i]>='0' && new_label[i]<='9') ||
				new_label[i] == '(' || new_label[i]==')' ||
				new_label[i] == '#' || new_label[i] == '_'))
			{
				*flag=1;
				goto done;
			}
/*
.....If a left parenthesis if found then flag2 is incremented
.....by one, if then a right parenthesis if found, flag2 is then
.....decremented by one. Thus, if there is a matching pair of
.....parentheses the net result will be flag2 is equal to 0.
.....But if there is at least one more of either left or right
.....parentheses, then flag2 will not be equal to 0, hence an
.....error message will be called in nclu_rename_geom. A check
.....is also performed to make sure that the parentheses are
.....in the correct order, if not, it is flaged for an error message
.....and returned.
*/
			else
			{
				if (new_label[i]=='(')
				*flag2=*flag2+1;
				if (new_label[i]==')')
				{
					if(*flag2!=1)
					{
						*flag2=1;
						goto done;
					}
					else
						*flag2=*flag2-1;
				}
			}
			i++;
		}
	}
/*
.....End of routine
*/
done:
	return;
}

/*******************************************************
**    E_FUNCTION     : ncl_update_labl
**       Update geometry label.
**    PARAMETERS   
**       INPUT  : 
**          eptr      - Pointer to structure for entity data with key filled in.
**          str       - New label for entity
**          isub      - New subsrcipt for entity
**       OUTPUT :  
**          eptr      - Fixed entity data.
**    RETURNS      : none
**    SIDE EFFECTS : Updates label and subscript of entity in unubase.
**    WARNINGS     : none
*********************************************************************/

void ncl_update_labl(eptr,str, isub)
struct NCL_fixed_databag *eptr;
char *str;
UM_int4 isub;
   {

   ur_retrieve_data_fixed (eptr);
   strcpy (eptr->label,str);
   eptr->subscr = isub;
   ur_update_data_fixed (eptr);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_set_label_attr(flg, key)
**       Set label_on flag according to flg
**    PARAMETERS   
**       INPUT  : 
**          flg      UU_TRUE - turn on label; UU_FALSE - turn off label
**          key      UU_KEY of entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : modifies entity attribute bundle
**    WARNINGS     : none
*********************************************************************/
void ncl_set_label_attr(flg, key)
UU_LOGICAL flg;
UU_KEY_ID key;
	{
	struct UM_attrdata_rec attr;

	/* make sure label is off for copied geo */
	attr.key = key;
	ur_retrieve_attr(&attr);
	if (flg)	/* set label on */
		{
		if (!ncl_get_label_on(attr.label_on))
			{
			ncl_set_label_on(&attr.label_on,1);
			ur_update_attr(&attr);
			}
		}
	else 		/* set label off */
		{
		if (ncl_get_label_on(attr.label_on))
			{
			ncl_set_label_on(&attr.label_on,0);
			ur_update_attr(&attr);
			}
		}
	return;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_toggle_labels1()
**       Turn labels & leader lines on or off.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_toggle_labels1(choice,key,leader)
int   *choice, *key, *leader;
{
   int   status;
   struct UC_entitydatabag e;
   struct UC_attributedatabag attr;
 
    uu_denter(UU_MTRC,(us,"nclu_toggle_labels1()"));
    
  e.key = *key;          
  status = uc_retrieve_data(&e, sizeof(e));
 
	if (ncl_label_type(e.rel_num) == UU_SUCCESS)
  {
     attr.key = e.key;
     status = ur_retrieve_attr(&attr);
     if (status == 0)
     {
	  switch (*choice)
	  {
		 case 1:
			ncl_set_label_on(&attr.label_on,1);
			break;
		 case 2:
			ncl_set_label_on(&attr.label_on,0);
			break;
	  }
	  switch (*leader)
	  {
		 case 0:
			break;
		 case 1:
			ncl_set_label_ldr(&attr.label_on,1);
			break;
		case 2:
			ncl_set_label_ldr(&attr.label_on,0);
			break;
	  }
      ur_update_attr(&attr); 
     }
  uc_display(&e);
  }
  uu_dexit;
}
/*********************************************************************
**    E_FUNCTION     : nclu_alter_label1(key,abs_del,x,y,z,leader,def,x1,y1,z1)
**       Alter label location for the given entity through the 
**                      command *SET/LABEL,ALTER
**    PARAMETERS   
**       INPUT  : 
**                   key - entity key
**                   abs_del - 0 - default, 1-abs. coord, 2 - delta
**                   x,y,z - coord
**					 leader - 0 - default , 1- leader lines on, 2 - off
**					 def - 0 - default location,1 - leader location x1,y1,z1
**					 x1,y1,z1 - new leader location
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
nclu_alter_label1(key,abs_del,x,y,z,leader,def,x1,y1,z1)
int *key,*abs_del,*leader,*def;
double *x,*y,*z,*x1,*y1,*z1;
{
	struct NCL_fixed_databag e;
	struct UC_attributedatabag attr;
	struct UC_attributedatabag *attp;
	UM_coord altpos,curpos,pt;
	int i;
	UM_transf tfmat,invmat;

	uu_denter(UU_MTRC,(us,"nclu_alter_label1()"));

	attp = (struct UC_attributedatabag *) &attr;
	altpos[0] = *x;
	altpos[1] = *y;
	altpos[2] = *z;

	/* save current label settings 
	savlab(); 
	   set current label settings to be true 
	setlbt(); */

	/* retrieve entity */
	e.key = *key;

	if (ur_retrieve_data_relnum(e.key, &e.rel_num) == UU_SUCCESS &&
       ncl_label_type(e.rel_num) != UU_SUCCESS)
	{
		uu_uerror0(UA_NCL,7);
		return(0);
	}

	if (ncl_retrieve_ent (&e,attp,tfmat) == UU_SUCCESS)
	{

	/*  get attribute structure */
		if (*abs_del == 0) /* default */
		{
/*
.....if the label display is on and altered set it to the default location
*/
			if (ncl_get_label_on(attp->label_on) && 
				ncl_get_label_alter(attp->label_on)) 
				ncl_delete_labloc(&attp->label_on);
			/*attp->label_on = 1;*/
			ncl_set_label_alter(&attp->label_on,0);
		}
		else if (*abs_del == 1) /* abs */
		{
				um_cctmtf(altpos,tfmat,altpos);
				ncl_add_labloc(&e,&attp->label_on, altpos);
		}
		else               /* delta */
		{
			if(!(ncl_get_label_on(attp->label_on) && 
				ncl_get_label_alter(attp->label_on))) 
				ncl_get_labcrd(&e,tfmat,curpos);
			else
			{
				ncl_retrieve_labloc(&e,attp->label_on,curpos);
				um_cctmtf(curpos,tfmat,curpos);
			}
			for(i=0; i<3; i++) altpos[i] = altpos[i] + curpos[i];
			um_inverttf(tfmat,invmat);
			um_cctmtf(altpos,invmat,altpos);
			ncl_add_labloc(&e,&attp->label_on, altpos);
		}
/*  
.....get position for leader line start location
*/
		if (*abs_del != 0)
		{
			ncl_default_labloc (&e,tfmat, pt);
			ncl_add_ldrloc(&e,&attp->label_on, pt);
			switch(*leader)
			{
				case 0:
					break;
				case 1:
					ncl_set_label_ldr(&attp->label_on,1);
					break;
				case 2:
					ncl_set_label_ldr(&attp->label_on,0);
					break;
			}
			if(*def)
			{
				pt[0] = *x1;
				pt[1] = *y1;
				pt[2] = *z1;
				um_cctmtf(pt,tfmat,pt);
				ncl_add_ldrloc(&e,&attp->label_on, pt);
			}
		}
		
		ur_update_attr(attp);
		uc_display(&e); /* display */
	}

	/* reset label setting to original 
	reslab(); */

	uu_dexit;
	return(0);
}

/*********************************************************************
**    E_FUNCTION     : S_save_modfile
**       Save the Label properties into modals file.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : UU_FAILURE if could not save modals file,  UU_SUCCESS
**                   otherwise.
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static int S_save_modfile()
{
	int stat;
	char msg[80];
	UX_pathname fname;
	FILE *fptr;
/*
.....Initialize routine
*/
	stat = UU_SUCCESS;
/*
.....Open modals file
*/
	strcpy (fname, "ncl_labels.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS", "modals", UU_NULL, UU_NULL,
					fname, 3, &fptr);
	if ((stat!=UU_SUCCESS)||(fptr==UU_NULL)) goto done;
/*
.....Store playback modals
*/
	ux_fputs0("#LABELS#\n", fptr);
/*
.....LAbels on/off
*/
	if (UW_label_on==1)
		sprintf(msg,"/LABELS/ *ON\n");
	else if (UW_label_on==2)
		sprintf(msg,"/LABELS/ *OFF\n");
	ux_fputs0(msg, fptr);
/*
.....Label Text color
*/
	if (UW_label_clr==0)
		sprintf(msg,"/LABEL_COLOR/ *BLACK\n");
	else if (UW_label_clr==1)
		sprintf(msg,"/LABEL_COLOR/ *WHITE\n");
	else if (UW_label_clr==2)
		sprintf(msg,"/LABEL_COLOR/ *BLUE\n");
	else if (UW_label_clr==3)
		sprintf(msg,"/LABEL_COLOR/ *RED\n");
	else if (UW_label_clr==4)
		sprintf(msg,"/LABEL_COLOR/ *GREEN\n");
	else if (UW_label_clr==5)
		sprintf(msg,"/LABEL_COLOR/ *MAGENTA\n");
	else if (UW_label_clr==6)
		sprintf(msg,"/LABEL_COLOR/ *YELLOW\n");
	else if (UW_label_clr==7)
		sprintf(msg,"/LABEL_COLOR/ *CYAN\n");
	else if (UW_label_clr==8)
		sprintf(msg,"/LABEL_COLOR/ *BROWN\n");
	else if (UW_label_clr==9)
		sprintf(msg,"/LABEL_COLOR/ *TAN\n");
	else if (UW_label_clr==10)
		sprintf(msg,"/LABEL_COLOR/ *LTBLUE\n");
	else if (UW_label_clr==11)
		sprintf(msg,"/LABEL_COLOR/ *SEAGREEN\n");
	else if (UW_label_clr==12)
		sprintf(msg,"/LABEL_COLOR/ *ORANGE\n");
	else if (UW_label_clr==13)
		sprintf(msg,"/LABEL_COLOR/ *PINK\n");
	else if (UW_label_clr==14)
		sprintf(msg,"/LABEL_COLOR/ *PURPLE\n");
	else if (UW_label_clr==15)
		sprintf(msg,"/LABEL_COLOR/ *GREY\n");
	ux_fputs0(msg, fptr);

/*
.....Label Text size
*/
	sprintf(msg,"/FONT_SIZE/ %d, %d\n", UW_label_size[1], UW_label_size[0]);
	ux_fputs0(msg, fptr);
/*
.....Minimal overlap distance
*/
	sprintf(msg,"/OVERLAP_DISTANCE/ %d\n", UW_overlap_dis);
	ux_fputs0(msg, fptr);
/*
.....Label background on/off
*/
	if (UW_bkg_on==1)
		sprintf(msg,"/BACKGROUND/ *ON\n");
	else if (UW_bkg_on==2)
		sprintf(msg,"/BACKGROUND/ *OFF\n");
	ux_fputs0(msg, fptr);

/*
.....Label background color
*/
	if (UW_bkg_clr==0)
		sprintf(msg,"/BACKGROUND_COLOR/  *BLACK\n");
	else if (UW_bkg_clr==1)
		sprintf(msg,"/BACKGROUND_COLOR/ *WHITE\n");
	else if (UW_bkg_clr==2)
		sprintf(msg,"/BACKGROUND_COLOR/ *BLUE\n");
	else if (UW_bkg_clr==3)
		sprintf(msg,"/BACKGROUND_COLOR/ *RED\n");
	else if (UW_bkg_clr==4)
		sprintf(msg,"/BACKGROUND_COLOR/ *GREEN\n");
	else if (UW_bkg_clr==5)
		sprintf(msg,"/BACKGROUND_COLOR/ *MAGENTA\n");
	else if (UW_bkg_clr==6)
		sprintf(msg,"/BACKGROUND_COLOR/ *YELLOW\n");
	else if (UW_bkg_clr==7)
		sprintf(msg,"/BACKGROUND_COLOR/ *CYAN\n");
	else if (UW_bkg_clr==8)
		sprintf(msg,"/BACKGROUND_COLOR/ *BROWN\n");
	else if (UW_bkg_clr==9)
		sprintf(msg,"/BACKGROUND_COLOR/ *TAN\n");
	else if (UW_bkg_clr==10)
		sprintf(msg,"/BACKGROUND_COLOR/ *LTBLUE\n");
	else if (UW_bkg_clr==11)
		sprintf(msg,"/BACKGROUND_COLOR/ *SEAGREEN\n");
	else if (UW_bkg_clr==12)
		sprintf(msg,"/BACKGROUND_COLOR/ *ORANGE\n");
	else if (UW_bkg_clr==13)
		sprintf(msg,"/BACKGROUND_COLOR/ *PINK\n");
	else if (UW_bkg_clr==14)
		sprintf(msg,"/BACKGROUND_COLOR/ *PURPLE\n");
	else if (UW_bkg_clr==15)
		sprintf(msg,"/BACKGROUND_COLOR/ *GREY\n");
	ux_fputs0(msg, fptr);

/*
.....Leader lines on/off
*/
	if (UW_ldr_on==1)
		sprintf(msg,"/LEADER_LINES/  *ON\n");
	else if (UW_ldr_on==2)
		sprintf(msg,"/LEADER_LINES/ *OFF\n");
	ux_fputs0(msg, fptr);

/*
.....Leader line color
*/
	if (UW_ldr_clr==0)
		sprintf(msg,"/LEADER_COLOR/  *BLACK\n");
	else if (UW_ldr_clr==1)
		sprintf(msg,"/LEADER_COLOR/ *WHITE\n");
	else if (UW_ldr_clr==2)
		sprintf(msg,"/LEADER_COLOR/ *BLUE\n");
	else if (UW_ldr_clr==3)
		sprintf(msg,"/LEADER_COLOR/ *RED\n");
	else if (UW_ldr_clr==4)
		sprintf(msg,"/LEADER_COLOR/ *GREEN\n");
	else if (UW_ldr_clr==5)
		sprintf(msg,"/LEADER_COLOR/ *MAGENTA\n");
	else if (UW_ldr_clr==6)
		sprintf(msg,"/LEADER_COLOR/ *YELLOW\n");
	else if (UW_ldr_clr==7)
		sprintf(msg,"/LEADER_COLOR/ *CYAN\n");
	else if (UW_ldr_clr==8)
		sprintf(msg,"/LEADER_COLOR/ *BROWN\n");
	else if (UW_ldr_clr==9)
		sprintf(msg,"/LEADER_COLOR/ *TAN\n");
	else if (UW_ldr_clr==10)
		sprintf(msg,"/LEADER_COLOR/ *LTBLUE\n");
	else if (UW_ldr_clr==11)
		sprintf(msg,"/LEADER_COLOR/ *SEAGREEN\n");
	else if (UW_ldr_clr==12)
		sprintf(msg,"/LEADER_COLOR/ *ORANGE\n");
	else if (UW_ldr_clr==13)
		sprintf(msg,"/LEADER_COLOR/ *PINK\n");
	else if (UW_ldr_clr==14)
		sprintf(msg,"/LEADER_COLOR/ *PURPLE\n");
	else if (UW_ldr_clr==15)
		sprintf(msg,"/LEADER_COLOR/ *GREY\n");
	ux_fputs0(msg, fptr);

/*
.....Leader line arrows on/off
*/
	if (UW_ldr_arrow==1)
		sprintf(msg,"/LEADER_ARROW/  *ON\n");
	else if (UW_ldr_arrow==2)
		sprintf(msg,"/LEADER_ARROW/ *OFF\n");
	ux_fputs0(msg, fptr);

/*
.....Output command
*/
	if (UW_out_cmd==0)
		sprintf(msg,"/OUTPUT_CMD/  *NO\n");
	else if (UW_out_cmd==1)
		sprintf(msg,"/OUTPUT_CMD/ *YES\n");
	ux_fputs0(msg, fptr);

/*
.....Close modals file
*/
	ux_fclose0 (fptr);
/*
.....End of routine
*/
done:
	return(stat);
}

/**************************************************************************
**
**  E_FUNCTION         :  nclu_label_modals()
**     Process the label modals form.
**  PARAMETERS
**      INPUT  :
**          none
**      OUTPUT :
**          none
**
**  RETURNS      :  
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
ncl_label_modals()
{
	int i,*ans[11],status;
	int label,labelclr,backgrnd,backgrndclr,ldrln,ldrlnclr;
	int labelhgt,labelwid,ldrlnarrow,outputcmd,overlapdis;
	static char traverse[] = { 1,1,1,1,1,1,1,1,1,1,1};
	static char display[] = { 1,1,1,1,1,1,1,1,1,1,1};
	NCL_cmdbuf cmdbuf;
	char buf[256];
	char scolor[64][96];
	static char *stoggle[3] = {"IGNORE","ON","OFF"};

	for (i=0; i<64;i++)
	{
		sprintf(scolor[i], "%s", uw_color_name[i]);
	}
/*
...Set default values
*/
	label =UW_label_on;
	if(label ==2)label = 0;
	
	labelclr = UW_label_clr;
	labelhgt = UW_label_size[0];
	labelwid = UW_label_size[1];
	overlapdis = UW_overlap_dis;
	backgrnd =UW_bkg_on;
	if(backgrnd ==2)backgrnd = 0;
	
	backgrndclr=UW_bkg_clr;

	ldrln =UW_ldr_on;
	if(ldrln ==2)ldrln = 0;

	ldrlnclr=UW_ldr_clr;

	ldrlnarrow =UW_ldr_arrow;
	if(ldrlnarrow ==2)ldrlnarrow = 0;

	outputcmd =UW_out_cmd;
		
	ans[0] = (int *)&label;
	ans[1] = (int *)&labelclr;
	ans[2] = (int *)&labelwid;
	ans[3] = (int *)&labelhgt;
	ans[4] = (int *)&overlapdis;
	ans[5] = (int *)&backgrnd;
	ans[6] = (int *)&backgrndclr;
	ans[7] = (int *)&ldrln;
	ans[8] = (int *)&ldrlnclr;
	ans[9] = (int *)&ldrlnarrow;
	ans[10] = (int *)&outputcmd;
	
/*
...Get input from user
*/
	status = ud_form1("labels.frm", ans, ans,UU_NULL,UU_NULL,display,traverse);
	if (status == -1) return (status);

	if(!label)label =2;
	UW_label_on = label;
	labelclr = labelclr;
	if(!backgrnd)backgrnd =2;
	backgrndclr = backgrndclr;
	if(!ldrln)ldrln =2;
	UW_ldr_on = ldrln;
	
	if(!ldrlnarrow)ldrlnarrow =2;
	UW_out_cmd = outputcmd;

	ncl_init_cmdbuf(&cmdbuf);
	if(UW_out_cmd)
		strcpy(buf,"DRAFT/LABEL");
	else
		strcpy(buf,"*DRAFT/LABEL");
	ncl_add_token(&cmdbuf, buf, NCL_comma);

	sprintf(buf,"%s",stoggle[label]);
	ncl_add_token(&cmdbuf,buf,NCL_comma);
	
	if(UW_bkg_on != backgrnd)
	{	
		sprintf(buf,"BOX,%s",stoggle[backgrnd]);
		ncl_add_token(&cmdbuf,buf,NCL_comma);
	}

	if(UW_label_clr != labelclr || UW_bkg_clr != backgrndclr)
	{
		sprintf(buf,"COLOR,%s,%s",scolor[labelclr],scolor[backgrndclr]);
		ncl_add_token(&cmdbuf,buf,NCL_comma);
	}

	if(UW_label_size[0] != labelhgt || UW_label_size[1] != labelwid)
	{
		sprintf(buf,"SIZE,%d,%d",labelwid,labelhgt);
		ncl_add_token(&cmdbuf,buf,NCL_comma);
	}

	if(UW_overlap_dis != overlapdis)
	{
		sprintf(buf,"OFFSET,%d",overlapdis);
		ncl_add_token(&cmdbuf,buf,NCL_comma);
	}
	sprintf(buf,"LEADER,%s",stoggle[ldrln]);
	ncl_add_token(&cmdbuf,buf,NCL_comma);


	if(UW_ldr_clr != ldrlnclr)
	{
		sprintf(buf,"COLOR,%s",scolor[ldrlnclr]);
		ncl_add_token(&cmdbuf,buf,NCL_comma);
	}

	if(UW_ldr_arrow != ldrlnarrow)
	{
		sprintf(buf,"ARROW,%s",stoggle[ldrlnarrow]);
		ncl_add_token(&cmdbuf,buf,NCL_comma);
	}

	ncl_set_cmdmode(UU_TRUE);
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
	S_save_modfile();
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : lblini
**       Set the label flg from the modals file
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          lbset:label flag
**			ldset:leader flag
**    RETURNS      : none
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
void lblini(lbset,ldset)
int *lbset,*ldset;
{
	*lbset = 0;
	*ldset = 0;
	if(UW_label_on==2) *lbset = 0;
	else *lbset = 1;
	if(UW_ldr_on==2) *ldset = 0;
	else *ldset = 1;
}
