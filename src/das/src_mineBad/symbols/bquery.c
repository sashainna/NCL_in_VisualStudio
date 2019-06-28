/*********************************************************************
**    NAME:  bquery.c
**       CONTAINS:
**				ub_query
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       bquery.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:04
*********************************************************************/
#include "usysdef.h"
#include "uhep.h"
#include "udebug.h"
#include "xenv1.h"	/* for UX_PRTERRS */
#include "xfsys1.h"	/* for UX_EXISTS */
#include "mdgenent.h"
#include "mdrel.h"
#include "mcrv.h"
#include "mattr.h"
#include "mdunits.h"
#include "mdcpln.h"
#include "ulist.h"
#include "jquery.h"  /* query macros, etc. */
#include "bsym.h"
#include "atext.h"

/*********************************************************************
**    I_FUNCTION: ub_query(key, list)
**      Build a character array of the data from unibase
**      given an entity key, data array and the extants of
**      of that array.
**
**      This function must be modified much the same as a
**      dispatcher to take into account new entities in Unibase.
**
**      If this function cannot extract the contents for the
**      given key, the function returns the forowsUsedowing strings
**      in the first 3 rows:
**
**          Entity type: ???
**           key . . . . . . . . . . . . (input key)
**           rel_num . . . . . . . . . . (entity relation number)
**
**       Additionally the function returns a status of UU_FAILURE
**       so the calling application has the option of operating
**       on the contents of the data area or not.
**    PARAMETERS
**       INPUT  :
**          key               Unibase key.
**				list					data list
**       OUTPUT :
**    RETURNS      : UU_SUCCESS    if data has been extracted.
**                   UU_FAILURE    if rows < 30, or if maxCols < 70.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ub_query(key, list) 
   UU_KEY_ID  key;
	UU_LIST	  *list;
{
	int ii, rel_num, len, status = UU_SUCCESS; 
	char label[UB_SYMBOL_NAME_LEN_PLUS];
	UM_transf tfmat; 
	struct UB_symattr_rec attr;

	uu_denter(UU_BTRC, (us,"ub_query(key=%d,)", key ));

	/* check for bad input */
 
	/* get all data for each entity */
	if (ur_retrieve_data_relnum(key, &rel_num) != 0) 
	{
		uu_uerror3(UB_SYMBOL, 36, rel_num, key, "ub_query");
		/* error is: Unknown relation type: %d, for key = %d, (%s) */
		goto failed;
	}
	if (ub_get_sym_attr(key, &attr) != UU_SUCCESS)
				goto failed;
 
	/* switch on the relation type - only limited number of queries possible */
	switch(rel_num)
	{
		case UB_INSTANCE_REL:
		{
			UU_REAL um_mag();
			struct UB_instance_rec inst;
			struct UB_symbol_rec master;
			UX_pathname name, libpath, libname, locarea, sysarea, farea, areabuf;
			int i, dif;
			UU_LOGICAL stillooking;
			int found;
			UM_coord origin, cporigin;
			UU_REAL scale;
			UM_vector rotation;
			/* struct UM_text_rec text; */
			struct UA_txt_rec text;
			int nbrKeys;
			UU_KEY_ID *keysList;
			char textbuf[200], tnodevis[40];

			inst.key = key;
   		if (ub_retrieve_sym(&inst, sizeof(inst)) != UU_SUCCESS) goto failed;
			if (ub_get_sym_transf(key, tfmat) != UU_SUCCESS) goto failed;
			/* get master associated with instance */
			if (ubi_get_master_key_for_inst(&inst,&(master.key)) != UU_SUCCESS)
					goto failed;
			/* master.key = inst.master_key; */
   		if (ub_retrieve_sym(&master, sizeof(master)) != UU_SUCCESS) 
							goto failed;
			UJ_PUT0(list, "  ")
			UJ_PUT1(list,"Entity type: %s", "SYMBOL INSTANCE")
			UJ_PUT1(list,"   Unibase key ............... %d", inst.key)
			UJ_PUT1(list,"   rel_num ................... %d", inst.rel_num)
			ncl_format_label(inst.label,inst.subscr,label,0);
			UJ_PUT1(list,"   Label ..................... %s", label)
			ncl_format_label(master.label,master.subscr,label,0);
			UJ_PUT1(list,"   Master Symbol ............. %s", label)
			/* get symbol library path */
			if (ux_decompose_path(master.path,libpath,name,UX_PRTERRS | UX_QUOTES)
					== UU_SUCCESS)
			{
				/* get symbol library name without extension */
				if (ux_get_base_farea(libpath,libname,UX_PRTERRS | UX_NQUOTES)
						!= UU_SUCCESS) goto failed;
				UJ_PUT1(list,"   Library ................... %s", libname)
				/* get file area of library path */
				if (ux_decompose_path(libpath,farea,libname,UX_PRTERRS | UX_QUOTES)
						!= UU_SUCCESS) goto failed;
				areabuf[0] = '\0';
				if (ux_test_prefix("UB_SYS_M_SYMDIR", libpath, &found, 
							UX_PRTERRS | UX_QUOTES) != UU_SUCCESS) goto failed;
				if ((found==UU_SUCCESS) && (strcmp(farea, sysarea) == 0))
					strcpy(areabuf, "System file area");
		
				if (ux_test_prefix("UB_SYS_M_SYMDIR", libpath, &found, 
							UX_PRTERRS | UX_QUOTES) != UU_SUCCESS) goto failed;
				if ((found==UU_SUCCESS) && (strcmp(farea, locarea) == 0))
					strcpy(areabuf, "Local file area");
			
				if (strcmp(areabuf, "") == 0) /* neither area found, print path */
				{
					strcpy(areabuf, "File area:");
					strcat(areabuf, farea);
				}
				else if (strcmp(locarea, sysarea) == 0)
					strcat(areabuf, "(local area = system area)");
			}
			UJ_PUT1(list,"   Number Geometry Entities ... %d", inst.no_geom)
			UJ_PUT1(list,"   Number Text Nodes .......... %d", inst.no_text_nod)
			UJ_PUT1(list,"   Number Snap Nodes .......... %d", inst.no_snap_nod)
			ubi_get_app_spec_assoc_list(inst.key,UB_CONECTOR_REL, UB_CON_INST_FLD 
				/* field in connector record to look for instance key */,
				&nbrKeys, &keysList);
			UJ_PUT1(list,"   Number Associated Connectors %d", nbrKeys)
   	
			UJ_PUT0(list," ")
			UJ_PUT0(list,"- Attribute Data -")
			UJ_PUT1(list,"   layer number............... %d", attr.layer)
			UJ_PUT1(list,"   logical pen ............... %d", attr.pen)
			UJ_PUT1(list,"   linestyle ................. %s", UJ_lstyles[attr.line_style])
			switch (attr.see_tnod)
			{
				case UB_ALL_TEXT_NODES_VISIBLE:
					strcpy(tnodevis, "all text visible");
					break;
				case UB_ONLY_GRAPHIC_TEXT_NODES:
					strcpy(tnodevis, "only graphic text visible");
					break;
				case UB_NO_TEXT_NODES_VISIBLE:
					strcpy(tnodevis, "no text visible");
					break;
			}/* end switch */
         UJ_PUT1(list,"   text visibility: %s", tnodevis)
			if (attr.see_snod)
			{
				UJ_PUT0(list,"   snap node visibility: all visible")
			}
			else 
			{
				UJ_PUT0(list,"   snap node visibility: none visible")
			}
			UJ_PUT0(list," ")

			UJ_PUT0(list,"- Symbol Instance Orientation -")
			i = 0; stillooking = UU_TRUE;
			while (stillooking && (i < master.no_inst))
				if (inst.key == master.inst[i].inst_key)
					stillooking = UU_FALSE;
				else i++;
			if (stillooking) goto failed;

			/* use transformation associated with instance to find instance
			 * origin, rotation, and scale */
			/* TRANSLATION */ 
			um_vctovc(master.inst[i].tfmat[3], origin);
			UM_cc_inttoext(origin, cporigin);
			UJ_PUT4(list,"   Origin %s <%8.4f,%8.4f,%8.4f>",
			UJ_units[UM_cpln.length_unit], cporigin[0], cporigin[1], cporigin[2])
			/* SCALE */
			/* now get scale; take any row or column, magnitude is scale */
			scale = um_mag(tfmat[0]);
			UJ_PUT1(list,"   scale ..................... %g", scale)
			/* ROTATION; can't do; don't have construction plane instance created
			 * on. */
         UJ_PUT0(list," ")

			/* Print text node data */
			UJ_PUT0(list,"- Symbol Instance Text -")
			for (i=0; i<inst.no_text_nod; i++)
			{
				text.key = inst.text_nod[i].text_key;
				/* if (um_get_all_geom(&text, sizeof(text)) != UU_SUCCESS) */
				if (uc_retrieve_data(&text, sizeof(text)) != UU_SUCCESS)
						goto failed;
				/* make sure we don't overrun "textbuf" */
				if ((dif=
						text.no_tchar-(sizeof(textbuf)-sizeof("  (non-graphic)")))>0)
					text.tchar[text.no_tchar-dif] = '\0';
				strcpy(textbuf, text.tchar);
/*
.....Places end of string character in correct location so that garbage values
.....are ignored
*/
				if (master.text_nod[i].prompt == 2) textbuf[text.no_tchar-1] = 0;
				if (inst.text_nod[i].visibility == UB_GRAPHIC_TEXT_NODE)
					strcat(textbuf, "      (graphic)");
				else strcat(textbuf,"      (non-graphic)");
				UJ_PUT1(list,"	%s", textbuf)
			}
			break;
		}
		case UB_SYMBOL_REL:
			goto failed;
	}/* end switch */
	goto done;
failed: status = UB_FAILURE;
   UB_IF_FAILURE_PRINT_IT
done:;
   uu_dexit;
   return(status);
}


