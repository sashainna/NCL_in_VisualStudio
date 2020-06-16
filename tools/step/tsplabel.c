/*********************************************************************
**    NAME         :  tspsupt.c
**       CONTAINS:
**             utp_free_label_list
**					utp_create_step_label
**					utp_define_shape_label
**					utp_get_label_method
**					utp_get_shape_label
**					utp_reset_shape_range
**					utp_set_label_method
**					utp_set_shape_range
**					utp_shape_label_nstack
**					utp_shape_label_push
**					utp_store_debug_label
**					utp_store_step_label
**					utp_valid_shape_range
**             utp_create_copy_label
**             utp_format_label
**    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tsplabel.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       10/27/16 , 15:18:51
*********************************************************************/

#include "mdcoord.h"
#include "mdrel.h"
#include "nccs.h"
#include "nconst.h"
#include "tiges.h"
#include "tigdefs.h"
#include "tstep.h"
#include "udebug.h"
#include "ulist.h"
#include "mlabddl.h"
#include "tzclass.h"

typedef char Stlab[NCL_MAX_LABEL+1];

char Mlabel[80]={""};

#define NREL 12

static void S_validate_label();

static int Shape_sub,Slabel_nrange=0,*Slabel_range;
static int Smethod=1,Ssubfl=1,Sconcat=0;
static char Shape_label[NCL_MAX_LABEL];
static UU_LOGICAL Slabel_stack_init = UU_FALSE;
static UU_LIST Slabel_stack;
static Srel_sub[NREL]={0,0,0,0,0,0,0,0,0,0,0,0};

/*********************************************************************
**    E_FUNCTION     :  utp_free_label_list()
**				Empties the label list to free the memory.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_free_label_list()
{
	if (Slabel_stack_init)
	{
		uu_list_free(&Slabel_stack);
		Slabel_stack_init = UU_FALSE;
	}
	if (Slabel_nrange != 0)
	{
		uu_free(Slabel_range);
		Slabel_nrange = 0;
	}
}
/*********************************************************************
**    E_FUNCTION     :  utp_reset_subs()
**       Reset subscript values for all major entity types.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_reset_subs()
{
	int i;

	for (i=0;i<NREL;i++) Srel_sub[i] = 0;
}
/*********************************************************************
**    E_FUNCTION     :  utp_create_step_label(rel_num,face,recno,label,subscr)
**       Creates a label for an entity to be stored in the Unibase.
**    PARAMETERS   
**       INPUT  : 
**          rel_num  Relation of entity being labeled.
**          recno    Record number from STEP file.
**          face     Face label from STEP file.
**       OUTPUT : none
**          label    Generated label.
**          subscr   Generated subscript.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_create_step_label(rel_num,face,recno,label,subscr)
int rel_num,recno;
char *face,*label;
int *subscr;
{
	int status,method,subfl,inc,sv_inc,i;
	char base[NCL_MAX_LABEL+1],lbuf[NCL_MAX_LABEL+1];
	char *p,*q,*strchr();
/*
.....Initialize routine
*/
	status = 0;
/*
.....Modify labeling method if certain
.....label fields in STEP file are blank
*/
	method = Smethod;
	subfl = Ssubfl;
	if (method == 2 && (face[0] == '\0' || strcmp(face,"NONE") == 0)) method = 1;
	if (method == 1 && Shape_label[0] == '\0') method = 0;
/*
.....Create label using NCL std
*/
again:;
	if (method == 0 || method == 4)
	{
		um_auto_label(rel_num,base);
		switch(rel_num)
		{
		case UM_POINT_REL:
			inc = 0; break;
		case NCL_POINTVEC_REL:
			inc = 1; break;
		case UM_LINE_REL:
			inc = 2; break;
		case NCL_VECTOR_REL:
			inc = 3; break;
		case NCL_PLN_REL:
			inc = 4; break;
		case UM_CIRCLE_REL:
			inc = 5; break;
		case UM_RBSPLCRV_REL:
		case UM_COMPCRV_REL:
			inc = 6; break;
		case UM_RBSPLSRF_REL:
		case NCL_REVSURF_REL:
		case NCL_TRIMSF_REL:
			inc = 7; break;
		case NCL_SHAPE_REL:
			inc = 8; break;
		case NCL_MATRIX_REL:
			inc = 9; break;
		case NCL_PATERN_REL:
			inc = 10; break;
		case UM_SOLID_REL:
			inc = 11; break;
		default:
			inc = 0; break;
		}
		subfl = lab_flag[inc];
		if (subfl) Shape_sub = Srel_sub[inc];
		sv_inc = inc;
	}
/*
.....Create label using Component label
*/
	if (method == 1)
	{
		if (Sconcat == 0 || face[0] == '\0' || strcmp(face,"NONE") == 0)
			strcpy(base,Shape_label);
		else
		{
			sprintf(base,"%s_%s",Shape_label,face);
			Shape_sub = 0;
		}
	}
/*
.....Create label using Face label
*/
	else if (method == 2)
	{
		strcpy(base,face);
		Shape_sub = 0;
	}
/*
.....Create label using Face record number
*/
	else if (method == 3)
	{
		switch (rel_num)
		{
		case UM_POINT_REL:
			strcpy(base,"TPT");
			break;
		case UM_LINE_REL:
			strcpy(base,"TLN");
			break;
		case UM_CIRCLE_REL:
			strcpy(base,"TCI");
			break;
		case NCL_PLN_REL:
			strcpy(base,"TPL");
			break;
		case UM_RBSPLCRV_REL:
		case UM_COMPCRV_REL:
			strcpy(base,"BCV");
			break;
		case UM_RBSPLSRF_REL:
		case NCL_REVSURF_REL:
			strcpy(base,"BSF");
			break;
		case NCL_TRIMSF_REL:
			strcpy(base,"TSF");
			break;
		default:
			method = 0;
			goto again;
		}
		Shape_sub = recno;
	}
/*
.....Create label using Secondary Unibase
*/
/*
	else if (method == 4)
	{
		strcpy(base,"QZ");
		subfl = 1;
	}
*/
/*
.....Break out subscript from label
*/
	if (Shape_sub == 0)
	{
		p = strchr(base,'(');
		if (p != UU_NULL)
		{
			strcpy(lbuf,p+1);
			q = strchr(lbuf,')');
			if (q != UU_NULL)
			{
				*q = '\0';
				if (ul_to_number(lbuf,&i) == UU_SUCCESS)
				{
					*p = '\0';
					Shape_sub = i - 1;
					subfl = 1;
				}
			}
		}
	}
/*
.....Create valid label
*/
	status = 0;
	inc = 0;
	S_validate_label(base);
	do
	{
/*
........Using record numbers
*/
		if (method == 3)
		{
			if (status == 0) strcpy(label,base);
			else sprintf(label,"%s_%d",base,++inc);
			*subscr = Shape_sub;
		}
/*
........Using Subscript
*/
		else if (subfl == 1)
		{
			strcpy(label,base);
			*subscr = ++Shape_sub;
		}
/*
........Without Subscript
*/
		else
		{
			*subscr = 0;
			if (status == 1)
				sprintf(label,"%s%d",base,++Shape_sub);
			else
				strcpy(label,base);
		}
/*
........Check if label exists
*/
		status = uig_label_check(label,*subscr);
	} while (status == 1);
	if (subfl && method == 0) Srel_sub[sv_inc] = Shape_sub;
}

/*********************************************************************
**    E_FUNCTION     :  utp_define_shape_label(recno,label)
**				Finds the SHAPE_REPRESENTATION label in the record tree
**          hierarchy of the SHAPE_DEFINITION.
**    PARAMETERS   
**       INPUT  : 
**          recno    Record number to start scan from.
**          validate UU_TRUE = Make into valid NCL label.
**                   UU_FALSE = Return label as defined.
**       OUTPUT :
**          label    Label of Shape.
**    RETURNS      :
**          UU_TRUE if a component label was found.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL utp_define_shape_label(recno,label,validate)
int recno;
char *label;
UU_LOGICAL validate;
{
	int i,irec;
	UU_LOGICAL found;
	UTPs_step_record *tptr;
/*
.....Default to no Shape label
*/
	strcpy(Shape_label,"");
	Shape_sub = 0;
	found = UU_FALSE;
/*
.....Search for the SHAPE_REPRESENTATION
.....or PRODUCT label
*/
	irec = recno;
	do
	{
		tptr = utp_get_record(irec);
		if (tptr == UU_NULL) break;
		if (tptr->command == SHAPE_REPRESENTATION || 
			tptr->command == PRODUCT_DEFINITION_SHAPE)
		{
/*
........Found the Shape label
*/
			if (tptr->parm[0].type == UTP_STRING)
			{
				if (tptr->parm[0].ptype.str[0] != '\0' &&
					strcmp(tptr->parm[0].ptype.str,"NONE") != 0)
				{
					strcpy(Shape_label,tptr->parm[0].ptype.str);
					found = UU_TRUE;
					break;
				}
			}
		}
/*
........Solid definition
........Get label if it exists
........or create label from record number
*/
		else if (tptr->command == ADVANCED_BREP_SHAPE_REPRESENTATION)
		{
			if (tptr->parm[0].type == UTP_STRING)
			{
				if (tptr->parm[0].ptype.str[0] != '\0' &&
					strcmp(tptr->parm[0].ptype.str,"NONE") != 0)
				{
					strcpy(Shape_label,tptr->parm[0].ptype.str);
					found = UU_TRUE;
				}
				if (Shape_label[0] == '\0' && !validate)
					sprintf(Shape_label,"%d",irec);
				break;
			}
		}
/*
........Search for the next record
*/
		irec = 0;
		for (i=0;i<tptr->nparm;i++)
		{
			if (tptr->parm[i].type == UTP_RECNO)
			{
				irec = tptr->parm[i].ptype.recno;
				break;
			}
		}
	} while (irec != 0);
/*
........Verify the shape label is correct
*/
	strcpy(label,Shape_label);
	if (validate) S_validate_label(label);
	return(found);
}

/*********************************************************************
**    E_FUNCTION     :  utp_get_label_method(method,subfl,concat)
**				Returns the method to used to label entities.
**
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :
**          method   0 = NCL generated labels, 1 = From components,
**                   2 = From faces, 3 = From record numbers,
**                   4 = From existing Unibase.
**          subfl    0 = Don't subscript entities, 1 = Subscript entities.
**          concat   0 = Don't concatenate shape and face labels
**                   (shape_face), 1 = Concatenate labels.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_get_label_method(method,subfl,concat)
int *method,*subfl,*concat;
{
	*method = Smethod;
	*subfl = Ssubfl;
	*concat = Sconcat;
}

/*********************************************************************
**    E_FUNCTION     :  utp_get_shape_label(label,inc)
**				Returns a shape label from the stack.
**
**    PARAMETERS   
**       INPUT  :
**          inc      Label location to retrieve from stack.
**       OUTPUT : none
**    RETURNS      : Label of shape from stack.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
char *utp_get_shape_label(inc)
int inc;
{
	Stlab *sptr;
/*
.....Retrieve requested label
*/
	if (Slabel_stack_init)
	{
		if (inc >= 0 && inc < UU_LIST_LENGTH(&Slabel_stack))
		{
			sptr = (Stlab *)UU_LIST_ARRAY(&Slabel_stack);
			return(sptr[inc]);
		}
	}
	return(UU_NULL);
}

/*********************************************************************
**    E_FUNCTION     :  utp_reset_shape_range()
**				Resets the Shape label active count;
**
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_reset_shape_range()
{
/*
.....Mark All Stack labels as active
*/
	Slabel_nrange = 0;
}

/*********************************************************************
**    E_FUNCTION     :  utp_set_label_method(method,subfl,concat)
**				Defines the method to used to label entities.
**
**    PARAMETERS   
**       INPUT  : 
**          method   0 = NCL generated labels, 1 = From shapes,
**                   2 = From faces, 3 = From existing Unibase.
**          subfl    0 = Don't subscript entities, 1 = Subscript entities.
**          concat   0 = Don't concatenate shape and face labels
**                   (shape_face), 1 = Concatenate labels.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_set_label_method(method,subfl,concat)
int method,subfl,concat;
{
	if (method != -1) Smethod = method;
	if (subfl != -1) Ssubfl = subfl;
	if (concat != -1) Sconcat = concat;
}

/*********************************************************************
**    E_FUNCTION     :  utp_set_shape_range(inc)
**				Marks entries in the Shape label stack as active.
**
**    PARAMETERS   
**       INPUT  : 
**          inc      Label location in stack to mark as active.
**          nent     Number of labels to activate.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_set_shape_range(inc,nent)
int inc,nent;
{
/*
.....Mark Stack label as active
*/
	if (Slabel_stack_init && nent > 0)
	{
		if (Slabel_nrange == 0)
			Slabel_range = (int *)uu_malloc(sizeof(int)*nent);
		Slabel_range[Slabel_nrange++] = inc;
	}
}

/*********************************************************************
**    E_FUNCTION     :  utp_shape_label_nstack()
**				Returns the number of entries on the Shape label stack.
**
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : Number of entries on Shape label stack.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_shape_label_nstack()
{
	if (!Slabel_stack_init)
		return(0);
	else
		return(UU_LIST_LENGTH(&Slabel_stack));
}

/*********************************************************************
**    E_FUNCTION     :  utp_shape_label_push(label)
**				Pushes the provided label onto the stack of defined shapes.
**
**    PARAMETERS   
**       INPUT  : 
**          label    Label of shape to push onto stack.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_shape_label_push(label)
char *label;
{
	int i,iret;
	UU_LOGICAL psh;
	Stlab *sptr;
/*
.....Return on blank string
*/
	if (label[0] == '\0' || strcmp(label,"NONE") == 0) return;
/*
.....Initialize label stack
*/
	if (!Slabel_stack_init)
	{
		uu_list_init(&Slabel_stack,sizeof(char)*(NCL_MAX_LABEL+1),1000,1000);
		Slabel_stack_init = UU_TRUE;
	}
/*
.....Sort entries and
.....Don't push duplicate entities
*/
	psh = UU_TRUE;
	sptr = (Stlab *)UU_LIST_ARRAY(&Slabel_stack);
	for (i=0;i<UU_LIST_LENGTH(&Slabel_stack);i++)
	{
		iret = strcmp(label,sptr[i]);
		if (iret < 0)
		{
			uu_list_insert(&Slabel_stack,i,label);
			psh = UU_FALSE;
			break;
		}
		else if (iret == 0)
		{
			psh = UU_FALSE;
			break;
		}
	}
/*
.....Push label onto end of stack
*/
	if (psh) uu_list_push(&Slabel_stack,label);
}

/*********************************************************************
**    E_FUNCTION     :  utp_store_debug_label(key,ptr,flag,sub)
**				Renames the entity using a generated debug label in the
**          following format where n is the STEP record number defining
**          this entity.
**
**              BCV(n) = Boundary curve.
**              BSF(n) = Base surface.
**              TSF(n) = Trimmed surface.
**    PARAMETERS   
**       INPUT  : 
**          key      Key of entity to rename.
**          ptr      Pointer to STEP record in global array.
**          flag     0 = Boundary curve, 1 = Base surface, 2 = Trimmed surface.
**          sub      0 for initial entity with this label, + for subsequent
**                   entities.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_store_debug_label(key,ptr,flag,sub)
UU_KEY_ID key;
UTPs_step_record *ptr;
int flag;
{
	static char *Slabel[3]={"BCV","BSF","TSF"};
	int status;
	struct NCL_fixed_databag ent;
	struct UC_attributedatabag attr;
/*
.....Get entity
*/
	ent.key = key;
	status = ncl_retrieve_data_fixed(&ent);
	if (status != UU_SUCCESS) return;
/*
.....Generate the label
*/
	if (sub != 0)
		sprintf(ent.label,"%s%s_%d",Slabel[flag],Mlabel,sub);
	else
		sprintf(ent.label,"%s%s",Slabel[flag],Mlabel);
	ent.subscr = ptr->recno;
/*
.....Store entity
.......Set appropriate attributes for debug geometry - ASF 7/2/13.
*/
	ur_update_data_fixed(&ent);
	if (flag < 2)
	{
		attr.key = key;
		ur_retrieve_attr(&attr);
		if (flag == 0) attr.color = UM_CYAN;
		else if (flag == 1) attr.color = UM_YELLOW;
		attr.displayable = UM_DISPLAYABLE;
		ur_update_attr(&attr);
	}
}

/*********************************************************************
**    E_FUNCTION     :  utp_store_step_label(key,face,recno)
**       Creates a valid label for an entity and updates its Unibase
**       record with the new label.
**    PARAMETERS   
**       INPUT  : 
**          key      Key of entity to rename.
**          recno    Record number from STEP file.
**          face     Face label from STEP file.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_store_step_label(key,face,recno)
UU_KEY_ID key;
char *face;
int recno;
{
	int status;
	UU_LOGICAL flag;
	struct NCL_fixed_databag ent;
	struct UC_attributedatabag attr;
/*
.....Get entity
*/
	ent.key = key;
	status = ncl_retrieve_data_fixed(&ent);
	if (status != UU_SUCCESS) return;
	flag = UU_FALSE;
	if (strcmp(ent.label,"@UN") == 0) flag = UU_TRUE;
/*
.....Generate new label
*/
	utp_create_step_label(ent.rel_num,face,recno,ent.label,&ent.subscr);
/*
.....Store entity
*/
	ur_update_data_fixed(&ent);
/*
.....Make entity displayable
.....if originally stored with @UN label
*/
	if (flag)
	{
		attr.key = ent.key;
		ur_retrieve_attr(&attr);
		attr.displayable = UM_DISPLAYABLE;
		ur_update_attr(&attr);
	}
}

/*********************************************************************
**    E_FUNCTION     :  utp_valid_shape_range(label)
**				Determines if a label is part of the select shape label
**          range defined in the Options form.
**
**    PARAMETERS   
**       INPUT  : 
**          label    Label of shape to check for in range.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL utp_valid_shape_range(label)
char *label;
{
	int i;
	Stlab *sptr;
/*
.....Determine if label has been selected by the user
*/
	if (Slabel_nrange == 0)
		return(UU_TRUE);
	else
	{
		sptr = (Stlab *)UU_LIST_ARRAY(&Slabel_stack);
		for (i=0;i<Slabel_nrange;i++)
		{
			if (strcmp(label,sptr[Slabel_range[i]]) == 0)
				return(UU_TRUE);
		}
	}
/*
.....Could not find label in range
*/
	return(UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION     :  utp_create_copy_label(rel_num,label,sub)
**				Create a label for a copied piece of geometry based on
**          the modal settings in nclstep.mod.
**    PARAMETERS   
**       INPUT  : 
**          relnum     Relation number for entity to label.
**       OUTPUT :
**          label      Entity label.
**          sub        Entity subscipt.
**    RETURNS      : UU_SUCCESS if label is generated.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_create_copy_label(relnum,label,sub)
int relnum, *sub;
char *label;
{
	int i,status;
	char lab[80];

	status = UU_FAILURE;
	if ((0 < relnum) && (relnum < 256))
	{
		i = UM_labelmdl.rel[relnum];
		if (UM_labelmdl.issub[i] == 1)
			sprintf(lab, "%s", UM_labelmdl.pfs[i]);
		else
			sprintf(lab, "%s%d", UM_labelmdl.pf[i], UM_labelmdl.next[i]);
		sprintf(label,"%-63s",lab);
		status = UU_SUCCESS;
	}
	if (status == UU_SUCCESS)
	{
		if (UM_labelmdl.issub[UM_labelmdl.rel[relnum]] == 0)
			UM_labelmdl.next[UM_labelmdl.rel[relnum]]++;
		else
			UM_labelmdl.subscr[UM_labelmdl.rel[relnum]]++;
		*sub = UM_labelmdl.subscr[UM_labelmdl.rel[relnum]];
		if (UM_labelmdl.issub[UM_labelmdl.rel[relnum]] == 0) *sub = 0;
	}
	return(status);
}


/*********************************************************************
**    E_FUNCTION     :  S_validate_label(label)
**				Changes all invalid characters in a label to an '_'
**          converts the label to upper case.
**    PARAMETERS   
**       INPUT  : 
**          label    Label to validate.
**       OUTPUT :
**          label    NCL approved label.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_validate_label(label)
char *label;
{
	int i;
	UU_LOGICAL ifl;
	char *strchr();
	static char delims[]={"!@#$%^&*()-=+[]{}\\|'\";:.,<>/?~`"};
/*
.....Convert to NCL approved label
*/
	ifl = UU_FALSE;
	for (i=strlen(label)-1;i>=0;i--)
	{
		if (strchr(delims,label[i]) != UU_NULL) label[i] = '_';
		if (label[i] != ' ' && !ifl)
		{
			label[i+1] = '\0';
			ifl = UU_TRUE;
		}
		else if (ifl && label[i] == ' ') label[i] = '_';
	}
	ul_to_upper(label);
}

/*********************************************************************
**    E_FUNCTION     : utp_format_label(label,isub,str,pflg)
**       Formats the label and optional string into a text string
**       representation.
**    PARAMETERS
**       INPUT  :
**          label            Entity label
**          isub             Entity subscript
**       OUTPUT :
**          str              Text string representation 'lab(sub)'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_format_label(label,isub,str)
char *label,*str;
int isub;
{
	int nc;

	nc = strlen(label);
	ul_strip_blanks(label,&nc);
	if (label[0] == '\0' || strncmp(label,"@UN",3) == 0)
		strcpy(str,"NONE");
	else if (isub <= 0)
		sprintf(str, "%s", label);
	else
	{
		sprintf(str, "%s(%d)", label, isub);
	}
}
