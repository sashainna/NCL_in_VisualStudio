/*********************************************************************
**    NAME         :  nelabel.c
**       CONTAINS:  routines to manipulate labels
**
**       namgen
**       drwlab
**       ncl_unilabel_to_ncllabel
**       ncl_draw_unilabel
**       ncl_get_label
**       nclf_format_label
**       ncl_format_label
**       ncl_get_label_and_subsrc
**       ncl_get_label_with_key
**       ncl_get_label_and_sub_with_key
**       namset
**       ncl_reset_label_counters
**       ncl_draw_label
**       ncl_proj_label_to_drw
**       ncl_get_labcrd
**       dsxlab
**       ncl_default_labloc
**       ncl_getlab
**       ncl_parse_label
**       parslb
**	 		ncl_display_ldr
**	 		ncl_delete_ldrlst
**	 		ncl_lblpt
**			ncl_reset_labloc
**			ncl_label_move(e)
**			ncl_label_overlap(e)
**			ncl_position_labels()
**			ncl_str2lval()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nelabel.c , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**       04/10/18 , 15:18:54
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "umath.h"
#include "uhep.h"
#include "class.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mfort.h"
#include "mdebug.h"
#include "mlab.h"
#include "mcrv.h"
#include "mdeval.h"
#include "mattr.h"
#include "nccs.h"
#include "nclfc.h"
#include "gcolors.h"
#include "ginq.h"
#include "goatt.h"
#include "mdclass.h"
#include "nclmodals.h"
#include "nclvx.h"
#include "lcom.h"
#include "mdattr.h"
#include "zsysdep.h"
#include "view.h"
#include "driver.h"
#include "mdunits.h"
#include "atext.h"
#include "bsym.h"

void ncl_unilabel_to_ncllabel();
void ncl_get_label();
void ncl_format_label();
void ncl_get_label_and_subscr();
void ncl_draw_label();

char blnk[64]="                                                               ";
extern int UR_active;
extern int NCL_dyn_ldr ;
int NCL_ldr_vp =0;
UU_KEY_ID NCL_ldr_vw =0;
extern UU_LIST NCL_ldr_list;
static Gfloat overlapdis;
/*********************************************************************
**    E_FUNCTION     : namgen(nclauto, ncltype, ncllabel, nclsubr)
**       Given the type of NCL entity (NCLTYPE) determine a label
**       (NCLLABEL). If NCLAUTO is 0, the autoindexing name is
**       not incremented (i.e. the same name will be given for 
**       the next call to this routine for that entity type).
**    PARAMETERS   
**       INPUT  : 
**          nclauto                 0 => don't bump name index
**                                  1 => bump name index
**          ncltype                 internal type of entity
**       OUTPUT :  
**          ncllabel                max 64 character label
**                                  (all blanks if any error)
**          nclsubr                 integer*4 for subscript
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void namgen(nclauto, ncltype, ncllabel, nclsubr)
   UM_int2 *nclauto;
   UM_int2 *ncltype;
   UM_int4 *nclsubr;
   UM_f77_str_ptr ncllabel;

   {
   int status;
   int unisubr;
   int rel_num;
   char label[NCL_MAX_LABEL];

   uu_denter(UU_MTRC,(us,"namgen(auto=%d, ncltype=%d, ncllabel=%x)",
      *nclauto, *ncltype, ncllabel));
	
	ncl_ncltyp_to_relnum (*ncltype, &rel_num);
   if (rel_num > 0)
		{
     	 if (*nclauto == 0)
        	status = um_next_auto_label(rel_num, label);
     	 else
       	  	status = um_auto_label(rel_num, label);
    	 if (status == UU_SUCCESS)
			{
			/* If the Auto subscripted geometry naming is in effect update the
			   subscript field in label structure. */
			unisubr = UM_labelmdl.subscr[UM_labelmdl.rel[rel_num]];
			if (UM_labelmdl.issub[UM_labelmdl.rel[rel_num]] == 0) unisubr = 0;
			ncl_unilabel_to_ncllabel(label, unisubr, UM_cstr_of_f77_str(ncllabel),
				nclsubr);
			}
		}
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : drwlab(x, y, z, nclkey)
**       Draw a label at the specified position (X,Y,Z).
**    PARAMETERS   
**       INPUT  : 
**          x,y,z             position to draw label
**          nclkey            key of entity to draw label
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void drwlab(x, y, z, nclkey)
   UM_real8 *x,*y,*z;
   UU_KEY_ID *nclkey;

{
   UM_coord pos;
   char label[100];
   char lab_with_blnk[100];
   struct  NCL_nclattr_rec   attr;  
   int     status;

   uu_denter(UU_MTRC,(us,"drwlab(xyz=(%f,%f,%f), nclkey=%x",
      *x, *y, *z, *nclkey));

   attr.key = *nclkey;
   status = ur_retrieve_attr(&attr);
/*
.....check if label display is on
*/
   if (status == 0 && ncl_get_label_on(attr.label_on))
	{
	   /* if altered, get label location */
		if (ncl_get_label_alter(attr.label_on))
		{
			struct NCL_fixed_databag e;
			e.key = *nclkey;
			ncl_retrieve_data_fixed(&e);
		   ncl_retrieve_labloc(&e,attr.label_on, pos);
		}
		else
		{
			pos[0]=(*x);
			pos[1]=(*y);
			pos[2]=(*z);
		}

      ncl_get_label_with_key(*nclkey, label);
      lab_with_blnk[0] = '\0';
      strcpy(lab_with_blnk," ");
      strcat(lab_with_blnk, label);
      ncl_draw_label(pos, lab_with_blnk, *nclkey);
	}

   uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : ncl_unilabel_to_ncllabel(unilabel, unisubr, 
**                            ncllabel, nclsubr)
**       Convert a UNICAD label (UNILABEL, UNISUBR) to an NCL
**       label (NCLLABEL, NCLSUBR).
**    PARAMETERS   
**       INPUT  : 
**          unilabel                UNICAD label (string, NULL terminated)
**          unisubr                 UNICAD subsrcipt 
**          ncllabel                NCL label (64 character; blank filled)
**          nclsubr                 NCL subscript
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_unilabel_to_ncllabel(unilabel, unisubr, ncllabel, nclsubr)
   char     unilabel[];
   int      unisubr;
   char     ncllabel[64];
   UM_int4  *nclsubr;
   {
   int i;

   uu_denter(UU_MTRC,(us,"ncl_unilabel_to_ncllabel(label=%s, subr=%d)",
      unilabel, unisubr));
   
   for (i=0; i<64; i++) ncllabel[i] = unilabel[i];
   *nclsubr = unisubr;

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_draw_unilabel(eptr)
**       Draw a label for a UNICAD entity. 
**    PARAMETERS   
**       INPUT  : 
**          eptr                Pointer to entity.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_draw_unilabel (eptr, tfmat)
struct UM_point_rec *eptr;
UM_transf tfmat;
{
   struct  UC_attributedatabag attr;  
   char lab_with_blnk[NCL_MAX_LABEL_AND_SUBSCRIPT];
   UM_coord ldrxyz,labxyz;
   int status;

   uu_denter(UU_MTRC,(us,"ncl_draw_unilabel(eptr=%x)",eptr));
   
   if (ncl_label_type(eptr->rel_num) != UU_SUCCESS) return (UU_SUCCESS);

   attr.key = eptr->key;
   status = ur_retrieve_attr(&attr);

/*
..... If label display is on
*/
	if (status == UU_SUCCESS && (ncl_get_label_on(attr.label_on)) && !ncl_tst_un_label(eptr))
	{
/*
..... if altered, get label location
*/
	   if (ncl_get_label_alter(attr.label_on)) 
		{
			ncl_retrieve_labloc (eptr, attr.label_on, labxyz);
/*
......why remove the following line for matrix adjust, added again
......otherwise, the plot file created will plot wrong for label (surface,...)
*/
			if(UM_2d3d_mode == UM_2D)
				um_cctmtf (labxyz,tfmat,labxyz);
/*
.....Display leader line is leader line bit is set
*/
			if(ncl_get_label_ldr(attr.label_on))
			{
				ncl_retrieve_ldrloc (eptr, attr.label_on, ldrxyz);
				if(UM_2d3d_mode == UM_2D)
					um_cctmtf (ldrxyz,tfmat,ldrxyz);
			}

		}
		else 
			status = ncl_default_labloc (eptr,tfmat,labxyz);

		if (status == UU_SUCCESS)
		{
			lab_with_blnk[0] = '\0';
			strcpy(lab_with_blnk," ");
			ncl_get_label(eptr, &lab_with_blnk[1]);
			ncl_draw_label(labxyz, lab_with_blnk, eptr->key);
/*
.....Display leader lines only if label display is on , the label has been
.....altered and the leader lines are on.
*/
			if(ncl_get_label_ldr(attr.label_on) && 
				ncl_get_label_on(attr.label_on) && 
				ncl_get_label_alter(attr.label_on))
			{
				uw_glleaderline(eptr->key,labxyz, ldrxyz, lab_with_blnk);
			}
		}
	}

	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_label(eptr, str)
**       Return the label (including subscript) of the specified
**       entity.
**    PARAMETERS   
**       INPUT  : 
**          eptr              NCL entity
**       OUTPUT :  
**          str               label
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_label(eptr, str)
struct NCL_id_rec *eptr;
char *str;
{
   char label[NCL_MAX_LABEL+1];
   int isub;

   uu_denter(UU_MTRC,(us,"ncl_get_label(key=%x,str=%x)", eptr->key, str));

   ncl_get_label_and_subscr(eptr, label, &isub);
	ncl_format_label(label,isub,str,0);

   uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclf_format_label(label,isub,str,pflg)
**       Fortran callable routine to format label.
**    PARAMETERS   
**       INPUT  : 
**          label            Entity label
**          isub             Entity subscript
**          pflg             Parenethsis flag
**                           0: Use parenthesis around subscript
**                           1: Omit parenthesis around subscript
**       OUTPUT :  
**          str              Text string representation 'lab(sub)'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_format_label(flab,isub,fstr,pflg)
UM_f77_str_ptr flab,fstr;
UM_int4 *isub,*pflg;
{
	int i,nc;
/*
......Label is always define as 64 characters, if use MAXSYMLEN, which is 80
......will cause the memory error and wrong value later
......so change to use 64.
......Yurong
*/
//	char *p,label[MAXSYMLEN];
	char *p,label[64];
/*
.....Get Fortran label
*/
	p = UM_cstr_of_f77_str(flab);
//	nc = MAXSYMLEN;
	nc = 64;
	strncpy(label,p,nc);
	ul_strip_blanks(label,&nc);
/*
.....Format label
*/
	p = UM_cstr_of_f77_str(fstr);
	ncl_format_label(label,*isub,p,*pflg);
/*
.....Store Fortran formatted label
*/
//	for (i=strlen(p);i<MAXSYMLEN;i++) p[i] = ' ';
	for (i=strlen(p);i<64;i++) p[i] = ' ';
}

/*********************************************************************
**    E_FUNCTION     : ncl_format_label(label,isub,str,pflg)
**       Formats the label and optional string into a text string
**       representation.
**    PARAMETERS   
**       INPUT  : 
**          label            Entity label
**          isub             Entity subscript
**          plfg             Parenthesis flag
**                           0: Use parenthesis around subscript
**                           1: Omit parenthesis around subscript
**       OUTPUT :  
**          str              Text string representation 'lab(sub)'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_format_label(label,isub,str,pflg)
char *label,*str;
int isub,pflg;
{
	if (isub == 0)
		sprintf(str, "%s", label);
	else
	{
		if (pflg == 0)	sprintf(str, "%s(%d)", label, isub);
		else sprintf(str, "%s%d", label, isub);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_label_and_subsrc(eptr, str, subscr)
**       Return the label and subscript of the specified
**       entity.
**    PARAMETERS   
**       INPUT  : 
**          eptr              NCL entity
**       OUTPUT :  
**          str               label (without subscript)
**          subsrc            subscript
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_label_and_subscr(eptr, str, subscr)
struct NCL_fixed_databag *eptr;
char *str;
int *subscr;
	{
	int len;
	int i;

	uu_denter(UU_MTRC,(us,"ncl_get_label_and_subscr(key=%x,str=%x)", eptr->key, str));
/*
.....Symbol instances now contain labels
*/
/*
	if (eptr->rel_num==UB_INSTANCE_REL)
	{
	    *subscr = 0;
		str[0] = '\0';
		return;
	}
*/
	len = strlen(eptr->label);
	if (len > NCL_MAX_LABEL)
		{
		len = NCL_MAX_LABEL;
		strncpy(str, eptr->label, len-1);
		str[len-1] = '\0';
		}
	else
	{
		strncpy(str, eptr->label, NCL_MAX_LABEL);
		str[NCL_MAX_LABEL] = '\0';
	}
	for (i=0; i<len; i++)
		{
		if (str[i] == ' ')
			{
			str[i] = '\0';
			break;
			}
		}
/*   *subscr = 0; */
/*   if (ncl_geom_type(eptr->rel_num) == UU_SUCCESS) *subscr = eptr->subscr; */
    *subscr = eptr->subscr;

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ncl_get_label_with_key(key, str)
**       Return the label (including subscript) of the specified
**       entity.
**    PARAMETERS   
**       INPUT  : 
**          key               entity key
**       OUTPUT :  
**          str               label
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_label_with_key(key, str)
   UU_KEY_ID key;
   char *str;

   {
   int status;
   int subscr;

   uu_denter(UU_MTRC,(us,"ncl_get_label_with_key(key=%d,str=%x)", key, str));

   status = ncl_get_label_and_sub_with_key(key, str, &subscr);
   if (status == UU_SUCCESS)
      {
      if (subscr == 0)
         sprintf(str, "%s", str);
      else
         sprintf(str, "%s(%d)", str, subscr);
      }

   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_get_label_and_sub_with_key(key, str, subscr)
**       Return the label and subscript) of the specified
**       entity.
**    PARAMETERS   
**       INPUT  : 
**          key               entity key
**       OUTPUT :  
**          str               label
**          sub               subscript
**    RETURNS      : UU_SUCCESS iff no error, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_label_and_sub_with_key(key, str, subscr)
   UU_KEY_ID key;
   char *str;
   int *subscr;

   {
   int rel_num, status;
   struct NCL_fixed_databag e;
   UU_LOGICAL ncl_legal_relation();

   uu_denter(UU_MTRC,(us,"ncl_get_label_and_sub_with_key(key=%d,str=%x)",
          key, str));

   status = UU_FAILURE;
   ur_retrieve_data_relnum(key, &rel_num);
   if (ncl_legal_relation(rel_num))
      {
      e.key = key;
      if (ur_retrieve_data_fixed (&e) == 0)
         {
         ncl_get_label_and_subscr(&e, str, subscr);
         status = UU_SUCCESS;
         }
      }
   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : namset(ncltype, nclincr)
**       Given the type of NCL entity (NCLTYPE) automatically 
**       generates a name starting at nclincr. 
**    PARAMETERS   
**       INPUT  : 
**          ncltype                 internal type of entity
**          nclincr                  starting increment for geometry name 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void namset(ncltype, nclincr)
   UM_int2 *ncltype;
   UM_int4 *nclincr;

   {
   int rel_num;

   uu_denter(UU_MTRC,(us,"namset(ncltype=%d, nclincr=%d)",
      *ncltype, *nclincr));

	ncl_ncltyp_to_relnum (*ncltype, &rel_num);
   if (rel_num > 0)
      {
      UM_labelmdl.next[UM_labelmdl.rel[rel_num]]= *nclincr;
      }
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_reset_labels()
**       Reset label structure
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_reset_labels()
{
	um_reset_labels();
}

/*********************************************************************
**    E_FUNCTION     : ncl_draw_label(pos, label)
**       Call DIGS to write (in hardware text) label in position pos.
**    PARAMETERS   
**       INPUT  : 
**          pos	       position (MCS) to draw label
**          label      string for label
**			keyid:     key of entity of label
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_draw_label(pos, label, keyid)
   UM_coord pos;
   char     *label;
	int keyid;
   {
	char lab[20];
   Gtxfp      *textptr, oldtextype, newtextype;
   UU_REAL    char_height, char_space;
	
    uu_denter(UU_MTRC,(us,"ncl_draw_label()"));

	/* save current text type */
	textptr = gqtextfp();
	oldtextype.font = textptr->font;
	oldtextype.prec = textptr->prec;

	/* set new text type */
	newtextype.font = 1;
	newtextype.prec = UG_STRING; 
	gstextfp(&newtextype);

	/* set the character height */
    char_height = 0.15;
	gscharheight(char_height);

	/* set the character space in case */
    /* ug_gksstli.curprats.txbundl.space has some large value */
	/* corrects extrema zoom with labels on problem */
    char_space = 0.;
	gscharspace(char_space); 

	/* set the character color */
/*
.....should use UW_label_clr
.....Yurong
*/
/*	gstextcolor(UG_C_WHITE);*/
/*
.....the label color can be set to Black now
*/
	/*if (UW_label_clr==0)
	{
		attr.key = keyid;
		ur_retrieve_attr(&attr);
		gstextcolor(attr.color);
	}
	else*/
	gstextcolor(UW_label_clr);
	
/*
.....Labels preceded with a '#' are drawing labels
.....Remove the '#' prior to displaying label
.....Bobby  -  2/28/94
*/
	if (label[0] == '#') gtext(pos, &label[1]);
	else if (label[0] == ' ' && label[1] == '#')
	{
		lab[0] = ' ';
		strcpy(&lab[1],&label[2]);
		gtext(pos,lab);
	}
	else gtext(pos, label);
	/* set the text type back */
	gstextfp(&oldtextype);
	uu_dexit;
	}


/*********************************************************************
**    E_FUNCTION     : ncl_proj_label_to_drw(eptr,pptr,drwmat,drwmat,
**						    vrefpt,vpnorm)
**       Copies the label and subscript of an entity to the entity
**		projected to a drawing, and if the label is turned on, then
**		projects the label to the drawing.
**    PARAMETERS   
**       INPUT  : 
**			eptr   = Original geometry entity.
**			pptr   = Projected geometry entity.
**			attr   = Attribute record pointer.
**			drwmat = Drawing transformation matrix.
**			vrefpt = Reference point of viewing plane.
**			vpnorm = Normal of viewing plane.
**       OUTPUT :  
**          pptr.label    = Label from eptr.
**			pptr.subscr   = Subscript from eptr.
**			attr.label_on = New label position index, if label is
**			                positioned at a specific location.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_proj_label_to_drw (eptr,pptr,attr,drwmat,vrefpt,vpnorm)
struct UM_point_rec *eptr,*pptr;
struct UC_attributedatabag *attr;
UM_transf drwmat;
UM_coord vrefpt;
UM_vector vpnorm;
{
	int iret,status;
	UM_transf tfmat;
	UM_real8 sfpt[12],pts[40][3];
	UM_real4 u4,v4;
	UM_int4 key[2];
	UM_int2 npts,maxpts,n;
	UM_coord lpt;
/*
.....Do not transfer @UN labels
*/
	if (strncmp(eptr->label,"@UN",3) == 0)
	{
		strcpy(pptr->label,"#");
		attr->label_on = 0;
		return (UU_SUCCESS);
	}
/*
.....Copy label and subscript to projected geometry
*/
	pptr->label[0] = '#';
	strncpy(&(pptr->label[1]),eptr->label, NCL_MAX_LABEL);
	pptr->subscr = eptr->subscr;
/*
.....Geometry which creates polylines
.....requires special logic for default
.....label position
*/
	iret = 1;
/*
.....check if the label display is on and if the label has not been altered
*/
	if (ncl_get_label_on(attr->label_on) && 
		(!ncl_get_label_alter(attr->label_on))&& 
		pptr->rel_num == UM_POLYLINE_REL) 
	{
		iret = 0;
/*
.....set the label altered flag
*/
		ncl_set_label_alter(&attr->label_on,1);

		switch (uc_super_class(eptr->rel_num))
		{
			case UM_CURVE_CLASS:
			case UM_SURFACE_CLASS:
			case UM_SOLID_CLASS:
				status = uc_retrieve_transf (eptr->key,tfmat);
				if (status == UU_SUCCESS) 
					status = ncl_default_labloc (eptr,tfmat,lpt);
				break;
			default:
				switch (eptr->rel_num)
				{
					case NCL_SURF_REL:
						u4 = v4 = 1.;
						key[0] = eptr->key;
						srfevl (key,&u4,&v4,sfpt);
						um_vctovc (sfpt,lpt);
						break;
					case NCL_PLN_REL:
						n = 1;
						maxpts = 40;
						pleval(&eptr->key,&maxpts,&npts,pts,&n);
						lpt[0] = pts[npts/2][0];
						lpt[1] = pts[npts/2][1];
						lpt[2] = pts[npts/2][2];
						break;
					default:
						attr->label_on = 0;
				}
		}
	}
/*
.....Label is turned on at a specific location
.....Project this location to the drawing
*/
	if (ncl_get_label_alter(attr->label_on))
	{
		if (iret == 1) ncl_retrieve_labloc (eptr, attr->label_on,lpt);
		um_nptpln (lpt,vrefpt,vpnorm,lpt);
		um_cctmtf(lpt,drwmat,lpt);
/*
... aak 09-apr-1998: replaced this call: update copy rather than original
		ncl_add_labloc(eptr,&inc,lpt);
*/
		um_vctovc (lpt, pptr->labloc);
		if(ncl_get_label_on(attr->label_on) || !ncl_get_label_alter(attr->label_on))
		{
			ncl_set_label_on(&attr->label_on,1);
			ncl_set_label_alter(&attr->label_on,1);
			/*attr->label_on = (attr->label_on < 0)? -2 : 2;*/
		}
	}

	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_labcrd(eptr,tfmat,labxyz)
**       Get the label coord.  for a UNICAD entity. 
**    PARAMETERS   
**       INPUT  : 
**          eptr                Pointer to entity.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_labcrd(eptr, tfmat,labxyz)
   struct UM_point_rec *eptr;
   UM_transf tfmat;
   UM_coord labxyz;

   {
   int status;
   UM_param u, v;
   struct UM_evcrvout evout;
   struct UM_evsrfout evsrf;

   switch (uc_super_class(eptr->rel_num))
   {
		case UM_POINT_CLASS:
			um_cctmtf(eptr->pt,tfmat,labxyz);
			status = UU_SUCCESS;
			break;
		case UM_CURVE_CLASS:
			uc_init_evcrvout(eptr, &evout);
			u=.5;
			uc_evcrv(UM_POINT,u,eptr,tfmat,&evout);
			um_vctovc(evout.cp,labxyz);
			status = UU_SUCCESS;
			break;
		case UM_SURFACE_CLASS:
			if (eptr->rel_num == NCL_TRIMSF_REL)
			{
				struct NCL_trimsf_rec *trimsf;
				struct NCL_fixed_databag bncv;
				UU_REAL uvm[4];

				trimsf = (struct NCL_trimsf_rec *) eptr;
				bncv.key = trimsf->uv_key;
				status = ncl_retrieve_data_fixed (&bncv);
				if (status == 0)
				{
					uvm[0] = trimsf->u_min;   uvm[1] = trimsf->u_max;
					uvm[2] = trimsf->v_min;   uvm[3] = trimsf->v_max;
					uc_init_evcrvout(&bncv, &evout);
					um_ev7_uvcrv (UM_POINT,&bncv,uvm,(UU_REAL).5,&evout);
					bncv.key = trimsf->bs_key;
					ncl_retrieve_data_fixed (&bncv);
					uc_init_evsrfout(&bncv, &evsrf);
					uc_evsrf (UM_POINT,evout.cp[0],evout.cp[1],&bncv,tfmat,&evsrf);
					um_vctovc (evsrf.sp, labxyz);
				}
			}
			else
			{
				uc_init_evsrfout(eptr, &evsrf);
				u = 1.;
				v = 1.;
				uc_evsrf (UM_POINT, u, v, eptr, tfmat, &evsrf);
				um_vctovc (evsrf.sp, labxyz);
				status = UU_SUCCESS;
			}
			break;
    	default:
			status = UU_FAILURE;
			break;
	}
}
/*********************************************************************
**    E_FUNCTION     : dsxlab(pos,label, keyid)
**       Display label (text) at specified position (fortran callable) 
**    PARAMETERS   
**       INPUT  : 
**          pos         x,y,z position of the 1-st character.
**          label       character string terminated with null.
**			keyid:     key of entity of label
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void dsxlab (pos,label,nc, keyid)
  UM_coord pos;
  UM_f77_str_ptr label;
  UM_int4 *nc, *keyid;
  {
   char *cstr, buff[80];

   cstr = UM_cstr_of_f77_str(label);
   strncpy (buff,cstr,*nc); 
   buff[*nc] = '\0';
   ncl_draw_label (pos,buff, *keyid);
  }
/*********************************************************************
**    E_FUNCTION     : ncl_tst_un_label(eptr)
**       Test if label is @UN marking subentities only. Use this function
**       everywhere when checking @UN label to avoid problems when label
**       is longer then 3 characters. 
**    PARAMETERS   
**       INPUT  : 
**          eptr       pointer to labeled entity
**       OUTPUT :  
**          none
**    RETURNS      : 1 = @UN label, 0 = other string
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_tst_un_label(eptr)
struct NCL_id_rec *eptr;
{
	int i, status;
	
	status = 0;
/*
.....vp 2/24/98
.....only labeled entity is tested for valid label
*/
	if (ncl_legal_relation(eptr->rel_num))
	{
		i = strlen(eptr->label);
		if(strncmp(eptr->label,"@UN",3) == 0)
		{
/*
.....set the entities with a @UN label to NEVER DISPLAYABLE.
*/
			status = ur_update_displayable(eptr->key, UM_NEVERDISPLAYABLE);
			status = 1;
		}
/*
.....make sure that if label is longer then 3 characters, there
.....are all spaces (like "@UN    ", but not "@UNDEFINED")
*/
		if (i > 3 && strncmp(&eptr->label[3],blnk,i-3)) status = 0;
	}

	return(status);
} 
/*********************************************************************
**    E_FUNCTION     : ncl_default_labloc (eptr,tfmat, labxyz)
**       Defines default label location for an entity
**    PARAMETERS   
**       INPUT  : 
**          eptr        - Pointer to entity.
**          tfmat       - transformation
**       OUTPUT :  
**          labxyz      - label location
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_default_labloc (eptr,tfmat, labxyz)
struct NCL_id_rec *eptr;
UM_transf tfmat;
UM_coord labxyz;
{
   int class, status,do_transform = 0;
   UM_param u, v;
   UM_int2 mdsys, mm;
   struct UM_evcrvout evout;
   struct UM_evsrfout evsrf;
	struct UA_txt_rec *anote;
	struct UA_txtattr_rec attr;
	struct UB_instance_rec *inst;
/*
.....Make sure it is a valid geometry type
*/
   if (ncl_label_type(eptr->rel_num) != UU_SUCCESS || 
		ncl_tst_un_label (eptr) ) return (UU_FAILURE);
/* 
.....vp 1.23.97 for some reason netsurface is class 1, so lets
.....fake it to surface class here
*/
	switch (eptr->rel_num)
	{
/*
		case UM_POLYLINE_REL:
			class = UM_POINT_CLASS;
			break;
*/
		case NCL_NETSF_REL:
			class = UM_SURFACE_CLASS;
			break;
/*
.....If it is a Shape the label will be displayed in a shape routine so
.....goto done. JLS 9/21/98
.....Make sure that the status is equal to UU_FAILURE, otherwise the
.....label will also be displayed at the (0,0) location.  JLS 5/11/99
*/
		case NCL_SHAPE_REL:
			status = UU_FAILURE;
			goto done;
			break;
		default:
			class = uc_super_class(eptr->rel_num);
			break;
	}

   status = UU_SUCCESS;

	switch (class)
	{
		case UM_POINT_CLASS:
			do_transform = 1;
/*
...MX point is in transl column  vp 18-jan-95
*/
			if (eptr->rel_num == NCL_MATRIX_REL)
			{
				struct NCL_matrix_rec *mx; 
				UM_int2 iflag=1;
				mx = (struct NCL_matrix_rec *) eptr;
				dstmat(mx->mat,&mx->key,tfmat,labxyz,&iflag);
			}
/*
...Ignore patern  vp 18-jan-95
*/
			else if (eptr->rel_num == NCL_PATERN_REL) return (UU_FAILURE);
/*
...The true point location 
*/
			else
			{
				struct UM_point_rec *ppt;
				ppt = (struct UM_point_rec *)eptr;
				um_vctovc (ppt->pt,labxyz);
			}
			break;

		case UM_CURVE_CLASS:
			uc_init_evcrvout(eptr, &evout);
			u=.5;
			uc_evcrv(UM_POINT,u,eptr,tfmat,&evout);
			um_vctovc(evout.cp,labxyz);
			break;

		case UM_SURFACE_CLASS:
			if (eptr->rel_num == NCL_TRIMSF_REL)
			{
				struct NCL_trimsf_rec *trimsf;
				struct NCL_fixed_databag bncv;
				UU_REAL uvm[4];

				trimsf = (struct NCL_trimsf_rec *) eptr;
				bncv.key = trimsf->uv_key;
				status = ncl_retrieve_data_fixed (&bncv);
				if (status == 0) 
				{
					uvm[0] = trimsf->u_min;   uvm[1] = trimsf->u_max;
					uvm[2] = trimsf->v_min;   uvm[3] = trimsf->v_max;
					uc_init_evcrvout(&bncv, &evout);
					um_ev7_uvcrv (UM_POINT,&bncv,uvm,(UU_REAL).5,&evout);
					bncv.key = trimsf->bs_key;
					ncl_retrieve_data_fixed (&bncv);
					uc_init_evsrfout(&bncv, &evsrf);
					uc_evsrf (UM_POINT,evout.cp[0],evout.cp[1],&bncv,tfmat,&evsrf);
					um_vctovc (evsrf.sp, labxyz);
				}
			}
			else
			{
				gtmsmm (&mdsys, &mm);
				u = v = 1.;
/*
.....vp 1.23.97 place label for net surface at the first subsurface
.....default position. Using 'tfm' we assume that net surface id
.....matrix 'tfmat' is unit mx, if it is not (in future) need
.....one extra translation of evaluated point.
*/
				if (eptr->rel_num == NCL_NETSF_REL)
				{
					struct NCL_netsf_rec *srf;
					struct NCL_fixed_databag e;
					struct UC_attributedatabag att;
					UM_transf tfm;
					srf = (struct NCL_netsf_rec *) eptr;
					e.key = srf->netkey[0];
					ncl_retrieve_ent (&e,&att,tfm);
					uc_init_evsrfout(&e, &evsrf);
					uc_evsrf (UM_POINT, u, v, &e, tfm, &evsrf);
					do_transform = 1;
				}
				else
				{
					uc_init_evsrfout(eptr, &evsrf);
					uc_evsrf (UM_POINT, u, v, eptr, tfmat, &evsrf);
				}

				um_vctovc (evsrf.sp, labxyz);
				stmsmm (&mdsys, &mm);
			}
			break;

		case UM_TEXT_CLASS:
			anote = (struct UA_txt_rec *)eptr;
			if (anote->no_displst == 0)
			{
				status = uc_retrieve_attr(anote->key,&attr);
				if (status != UU_SUCCESS) goto done;
				ua_calc_text_attach(anote,&attr,8,labxyz);
			}
			else
				um_vctovc(&anote->displst[anote->no_displst*6-12],
					labxyz);
			break;

		case UM_SYMBOL_CLASS:
			if (eptr->rel_num == UB_INSTANCE_REL)
			{
				inst = (struct UB_instance_rec *)eptr;
				ub_get_instance_origin(inst,labxyz);
			}
			else
			{
				status = UU_FAILURE;
			}
			break;

		case UM_SOLID_CLASS:
			ncl_solid_labloc(eptr,labxyz);
			break;

		default:
			status = UU_FAILURE;
			break;
	}

	if (status == UU_SUCCESS && do_transform)
		um_cctmtf (labxyz,tfmat,labxyz);
done:;

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_getlab (nclkey, len, label)
**       Display label (text) at specified position (fortran callable) 
**    PARAMETERS
**       INPUT  :
**          nclkey    - key of entity.
**          len       - length of output array.
**       OUTPUT :
**          label     - label of entity, space filled.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_getlab (nclkey, len, label)
  UM_int4 *nclkey;
  UM_int2 *len;
  UM_f77_str_ptr label;
  {
   int i;
   char *cstr;
   UU_KEY_ID key;

   key = *nclkey;
   cstr = UM_cstr_of_f77_str(label);
   ncl_get_label_with_key(key, cstr);
   i = strlen(cstr);
   while (i<*len) cstr[i++] = ' ';
   return;
  }

/*********************************************************************
**    E_FUNCTION     : ncl_parse_label(label,name,isub)
**       Parse label to extract the label and the subscript.  Label is
**       in the standard format of 'lab(sub)'.
**    PARAMETERS
**       INPUT  :
**          label     - Label with optional subscript to parse.
**       OUTPUT :
**          name      - Label of entity.
**          isub      - Subscript of entity.
**    RETURNS      : UU_FAILURE if label is not formatted correctly.
**                   UU_SUCCESS when successfully parsed.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_parse_label(label,name,isub)
char *label,*name;
int *isub;
{
	int status;
	int i,n,inc;
	char buf[80];
/*
.....Initialize routine
*/
	status = UU_FAILURE;
	n = strlen(label);
	inc = 0;
	*isub = -1;
/*
.....Store label and subscript
*/
	for (i=0;i<n;i++)
	{
		if (label[i] != '(')
		{
			name[inc++] = label[i];
			name[inc] = '\0';
		}
/*
........Break out subscript
*/
		else
		{
			name[inc] = '\0';
			inc = 0;
			*isub = 0;
			do
			{
				i++;
				if (label[i] == ')')
				{
					buf[inc] = '\0';
					*isub = atoi(buf);
					if (*isub > 0) status = UU_SUCCESS;
					else *isub = 0;
					break;
				}
				else
				{
					buf[inc++] = label[i];
				}
			} while (i < n);
		}
	}
/*
.....End of routine
*/
	if (*isub == -1)
	{
		*isub = 0;
		status = UU_SUCCESS;
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : parslb(nam,subsc)
**      Parse label to extract the label word, the subscript, and the 
**      ending number. Note that the string is assumed to have at most
**      NCL_MAX_LABEL characters.
**    PARAMETERS
**       INPUT  :
**          nam      - label to parse
**       OUTPUT :
**          nam      - parsed label
**          subsc    - subscript flag
**    RETURNS      : iret - subscript value
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UM_int2 parslb(nam,subsc)
UM_f77_str_ptr nam;
UM_int4 *subsc;
{
	UM_int2 iret;
	char *lab;
	int i,j,k,n;

	lab = UM_cstr_of_f77_str(nam);
	n = strlen(lab);
	if (n > NCL_MAX_LABEL) n = NCL_MAX_LABEL;
	for (i = 0; i < n; i++)
	{
		if (lab[i] >= '0' && lab[i] <= '9')
		{
			iret = 0;
			for (j = i; j < n; j++)
			{
				k = lab[j] - 48;
				if (k < 0 || k > 9) break;
				iret = 10*iret + k;
			}

			lab[i] = '\0';
			*subsc = 0;
			return (iret);
		}
	}
/*
..... set up subscripting if the label ended with a letter
*/
	*subsc = 1;

	return (0);
}
/*********************************************************************
**    FUNCTION     : 	ncl_lblpt(labpt,ldrpos,index)
**			find the nearest point on the label box to the leader line 
**			start position
**    PARAMETERS
**       INPUT  :
**			 	labpt		 : array of points on the box
**				ldrpos		 : leader line start position
**			OUTPUT :
**				index		 : index to the closest pt in the array
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_lblpt(labpt,ldrpos,index)
UM_coord labpt[8],ldrpos;
int *index;
{
	int i;
	UU_REAL dist=0,min=0;

	for(i=0;i<8;i++)
	{
		dist = um_dcccc(labpt[i],ldrpos);
		if(i==0)
		{
			min = dist;
			*index = 0;
		}
		else if(dist<min)
		{
			min = dist;
			*index = i;
		}
	}
	return 0;
}


/*********************************************************************
**    E_FUNCTION     : ncl_delete_ldrlst(key)
**       Remove key form the leader line list 
**    PARAMETERS   
**       INPUT  : 
**			key				key of entity to remove from NCL_ldr_list
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_delete_ldrlst(key)
UU_KEY_ID key;
{
	int nkey,i;
	UU_KEY_ID *keys;
/*
.....add this entity to the list of entities with leader lines displayed.
*/
	nkey = NCL_ldr_list.cur_cnt;
	keys = (UU_KEY_ID *) UU_LIST_ARRAY (&NCL_ldr_list);

   for(i=0;i<nkey;i++)
	{
		if(keys[i]==key)
		{	
			uu_list_delete(&NCL_ldr_list,i,1);
			break;
		}
	}
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : ncl_display_ldr(vport)
**       ReDisplay leader lines for entities in NCL_ldr_lst
**    PARAMETERS   
**       INPUT  : 
**			vport	:	viewport
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_display_ldr(vport)
UV_vport		*vport;
{
	int status,nkey,i;
	UU_KEY_ID *keys;
	struct NCL_fixed_databag e;
	struct  UC_attributedatabag   attr;  
	UV_view view;

	uv_getvid(vport->cur_view, &view);
/*
.....chk if view was modified
*/
	if(um_dcccc(view.cur_pln_norm,view.sav_pln_norm)>UM_FUZZ ||
		fabs(view.cur_aperture-view.sav_aperture)>UM_FUZZ ||
		um_dcccc(view.sav_up_vect,view.sav_up_vect)>UM_FUZZ ||
		view.modified||
		NCL_dyn_ldr)
	{


/*
.....add this entity to the list of entities with leader lines displayed.
*/
		nkey = NCL_ldr_list.cur_cnt;
		keys = (UU_KEY_ID *) UU_LIST_ARRAY (&NCL_ldr_list);

		if(nkey)
		{
			uv_drw_active_vp();
			NCL_ldr_vp = vport->xform;
			NCL_ldr_vw = view.key;
		}
		else 
			return 0;
		for(i=0;i<nkey;i++)
		{
			attr.key = keys[i];
			status = ur_retrieve_attr(&attr);
			if(ncl_get_label_ldr(attr.label_on) && 
				ncl_get_label_on(attr.label_on) && 
				ncl_get_label_alter(attr.label_on))
			{
				e.key = keys[i];
				ncl_retrieve_data_fixed(&e);
				uc_display(&e); /* display */
			}
		}
		uv_drw_active_vp();
	}
	
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : getkey2(f77label, nclkey)
**       Get the key of a entity from the entity label
**		
**    PARAMETERS
**       INPUT  :
**          f77label  - label of entity, space filled.
**       OUTPUT :
**          nclkey    - key of entity.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void getkey(f77label, nclkey)
UM_f77_str_ptr f77label;
UM_int4 *nclkey;
{
	void getkey2();
	if (UR_active == 1)
	{
		getkeyf(f77label, nclkey);
	}
	else
	{
		getkey2(f77label, nclkey);
	}
}

/*********************************************************************
**    E_FUNCTION     : getkey2(f77label, nclkey)
**       Get the key of a entity (used for second unibase, which not store
**			name in the NCL name list
**    PARAMETERS
**       INPUT  :
**          f77label  - label of entity, space filled.
**       OUTPUT :
**          nclkey    - key of entity.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void getkey2(f77label, nclkey)
UM_f77_str_ptr f77label;
UM_int4 *nclkey;
{
	UU_KEY_ID key;
	UU_LOGICAL init;
	int	i; 
	char str[100];
	char *clabel;

	clabel = UM_cstr_of_f77_str(f77label);
/*
.....remove spaces, The last non-space character is considered the last char
.....of the label
*/
	for (i=0; i<strlen (clabel); i++)
	{
		if (clabel[i]==' ') 
		{
			break;
		}
	}
	clabel[i] = '\0';
	init = UU_TRUE;
	while (uv_getobjs(init, &key, UU_FALSE) == UU_SUCCESS)
	{
		init = UU_FALSE;
		ncl_get_label_with_key(key, str);
		if (strcmp(clabel, str)==0)
		{
			*nclkey = key;
			return;
		}
	}
	*nclkey = 0;
}

/*********************************************************************
**    E_FUNCTION     : ncl_reset_labloc
**       reset all labels to their default location
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
int ncl_reset_labloc()
{
	struct  UC_attributedatabag   attr;
	UU_KEY_ID key;
	UU_LOGICAL init;
	struct NCL_fixed_databag e;
	int rel_num;
	UM_transf tfmat;
	
	if (ud_yesno(0, "Current label positions will be changed to default. Continue?",
				"Reset Label Position"))
	{
		init = UU_TRUE;
/*
.....get all entities whose label display flag is on and check for overlap
*/
		while (uv_getobjs(init, &key, UU_FALSE) == UU_SUCCESS)
		{
			init = UU_FALSE;
			if(um_retrieve_data_relnum(key, &rel_num)!=UU_SUCCESS) continue;
			if(!um_islabel_relation(rel_num)) continue;
			e.key = key;
			ncl_retrieve_data_fixed(&e);
			attr.key = key;
			if(ur_retrieve_attr(&attr)!=UU_SUCCESS) continue;
			if(uc_retrieve_transf (e.key,tfmat)!=UU_SUCCESS) continue;
			if(ncl_get_label_on(attr.label_on) && 
				ncl_get_label_alter(attr.label_on))
			{
				ncl_set_label_alter(&attr.label_on,0);
				ur_update_attr(&attr);
				uc_display(&e); /* display */
			}	
		}
		uz_repaint(0);
	}
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : ncl_position_labels
**       position all labels to avoid overlap
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : status 0: no overlap
**									-1: minimal overlap
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
int ncl_position_labels()
{
	struct  UC_attributedatabag   attr;
	UU_KEY_ID key;
	UU_LOGICAL init;
	struct NCL_fixed_databag e;
	int rel_num;
	UV_vport vport;

	if (uvu_pickvp(&vport) != UU_SUCCESS)
		return -1;
	init = UU_TRUE;
/*
.....get all entities whose label display flag is on and check for overlap
*/
	overlapdis =0;
	uw_gloverlapdis(&overlapdis);
	while (uv_getobjs(init, &key, UU_FALSE) == UU_SUCCESS)
	{
		init = UU_FALSE;
		if(um_retrieve_data_relnum(key, &rel_num)!=UU_SUCCESS) continue;
		if(!um_islabel_relation(rel_num)) continue;
		attr.key = key;
		if(ur_retrieve_attr(&attr)!=UU_SUCCESS) continue;
		if( ncl_get_label_on(attr.label_on))
		{
			gsnormtran(vport.xform);
   			e.key = key;
   			ncl_retrieve_data_fixed(&e);
   			if(ncl_label_overlap(&e)== UU_TRUE)
			{
				ncl_label_move(&e);
			}
		}	
	}
	gsnormtran(0);
	uz_repaint(0);
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : ncl_label_overlap(e)
**       check if this label overlaps any other displayed label
**    PARAMETERS
**       INPUT  :
**       e      :      entity ptr
**       OUTPUT :
**          none
**    RETURNS      : status 1: overlap
**									 0: no overlap
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_label_overlap(e)
struct NCL_fixed_databag *e;
{
	struct  UC_attributedatabag   attr, attr2;
	UU_KEY_ID key;
	UU_LOGICAL init,flag=UU_FALSE;
	int rel_num,status;
	UM_coord labpos1,ll1,ur1,labpos2,ll2,ur2;
	Gfloat wid,hgt;
	struct NCL_fixed_databag e2;
	UU_REAL tol;
	UM_real8 tol8;
	UM_transf tfmat,tfmat2;
	char lab_with_blnk[100];

	gettol (&tol8);
	tol = tol8;
	ncl_retrieve_data_fixed(e);
	attr.key = e->key;
	status = ur_retrieve_attr(&attr);
	status = uc_retrieve_transf (e->key,tfmat);
	if (ncl_get_label_alter(attr.label_on))
		ncl_retrieve_labloc(e,attr.label_on,labpos1);
	else
		status = ncl_default_labloc ((struct NCL_id_rec *)e,tfmat,labpos1);
	gwndc3(&ll1[0],&ll1[1],&ll1[2],labpos1[0],labpos1[1],labpos1[2]);
	um_vctovc(ll1,ur1);
	lab_with_blnk[0] = '\0';
	strcpy(lab_with_blnk," ");
	ncl_get_label((struct NCL_id_rec *)e, &lab_with_blnk[1]);
	uw_gllabelsz(lab_with_blnk,&wid,&hgt);
	ur1[0] += wid;
	ur1[1] += hgt;

	init = UU_TRUE;
/*
.....get all entities whose label display flag is on and check for overlap
*/
	while (uv_getobjs(init, &key, UU_FALSE) == UU_SUCCESS)
	{
		init = UU_FALSE;
		if(key==e->key)continue;
		if(um_retrieve_data_relnum(key, &rel_num)!=UU_SUCCESS) continue;
		if(!um_islabel_relation(rel_num)) continue;
		attr2.key = key;
		status = ur_retrieve_attr(&attr2);
		if(status!=UU_SUCCESS) continue;
		if(ncl_get_label_on(attr2.label_on))
		{
			e2.key = key;
			ncl_retrieve_data_fixed(&e2);
			status = uc_retrieve_transf (e2.key,tfmat2);
			if (ncl_get_label_alter(attr2.label_on))
				ncl_retrieve_labloc(&e2,attr2.label_on,labpos2);
			else
				status = ncl_default_labloc ((struct NCL_id_rec *)&e2,tfmat2,labpos2);
			gwndc3(&ll2[0],&ll2[1],&ll2[2],labpos2[0],labpos2[1],labpos2[2]);
			um_vctovc(ll2,ur2);
			lab_with_blnk[0] = '\0';
			strcpy(lab_with_blnk," ");
			ncl_get_label((struct NCL_id_rec *)&e2, &lab_with_blnk[1]);
			uw_gllabelsz(lab_with_blnk,&wid,&hgt);
			ur2[0] += wid;
			ur2[1] += hgt;
			if(ll1[0] > ur2[0]+tol+overlapdis || ur1[0] < ll2[0]-tol-overlapdis ||
			ll1[1] > ur2[1]+tol+overlapdis || ur1[1] < ll2[1]-tol-overlapdis) continue;
			else
			{
				flag = UU_TRUE;
				break;
			}
		}
	}
	return(flag);
}

/*********************************************************************
**    E_FUNCTION     : ncl_label_move(e)
**       move the label till no overlap
**    PARAMETERS
**       INPUT  :
**       e      :      entity ptr
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_label_move(e)
struct NCL_fixed_databag *e;
{
	struct  UC_attributedatabag   attr;
	UU_LOGICAL init;
	int step,status,i;
	UM_coord pt,vec,rotvec,newpos,labpos,labpt,ll;
	Gfloat wid,hgt;
	UM_angle ang;
	UM_transf tf;
	UM_transf tfmat;
	char lab_with_blnk[100];

	ncl_retrieve_data_fixed(e);
	attr.key = e->key;
	status = ur_retrieve_attr(&attr);
	status = uc_retrieve_transf (e->key,tfmat);
	if (ncl_get_label_alter(attr.label_on))
		ncl_retrieve_labloc(e,attr.label_on,labpos);
	else
		status = ncl_default_labloc ((struct NCL_id_rec *)e,tfmat,labpos);
	gwndc3(&ll[0],&ll[1],&ll[2],labpos[0],labpos[1],labpos[2]);
	lab_with_blnk[0] = '\0';
	strcpy(lab_with_blnk," ");
	ncl_get_label((struct NCL_id_rec *)&e, &lab_with_blnk[1]);
	uw_gllabelsz(lab_with_blnk,&wid,&hgt);

	init = UU_TRUE;
	step =1;
	ang = UM_PI /8;
	rotvec[0]=rotvec[1] =0;
	rotvec [2] =1;
	ncl_default_labloc ((struct NCL_id_rec *)e,tfmat, pt);
	ncl_add_ldrloc(e,&attr.label_on,pt);
	ur_update_attr(&attr);
/*
.....keep moving the label till  it doesnot overlap with any other label
*/
	while(UU_TRUE)
	{
		vec[0] = 1;
		vec[1] = vec[2] = 0;
		
		for(i=0;i<16;i++)
		{
			um_translate_point(ll,hgt*.5*step,vec,newpos);
			gndcw3(&labpt[0],&labpt[1],&labpt[2],newpos[0],newpos[1],newpos[2]);
			ncl_add_labloc(e, &attr.label_on, labpt);
			ur_update_attr(&attr);
			if(!ncl_label_overlap(e)) goto done;
			um_rottf(rotvec,ang,tf);
			um_vctmtf(vec,tf,vec);
		}
		step++;
	}
done:
	uc_display(e);
	return (0);
}


/*********************************************************************
**    E_FUNCTION     : nclf_getlabel(key, str, subscr)
**       Return the label and subscript) of the specified
**       entity. Fortran callable
**    PARAMETERS   
**       INPUT  : 
**          key               entity key
**       OUTPUT :  
**          str               label, fill the trailing empty spaces to 64 chars
**          sub               subscript
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_getlabel(key, str, subscr)
UU_KEY_ID *key;
char *str;
int *subscr;
{
	int i, rel_num;
	struct NCL_fixed_databag e;
	UU_LOGICAL ncl_legal_relation();
	char lab[NCL_MAX_LABEL+1];

	*subscr = 0;
	for (i=0; i<NCL_MAX_LABEL; i++)
		str[i] = ' ';
	ur_retrieve_data_relnum(*key, &rel_num);
	if (ncl_legal_relation(rel_num))
	{
		e.key = *key;
		if (ur_retrieve_data_fixed (&e) == 0)
		{
			ncl_get_label_and_subscr(&e, lab, subscr);
			strncpy(str, lab, strlen(lab));
			return;
		}
	}
}
/***********************************************************************
c
c   SUBROUTINE:  ncl_getlabel_sub (rstring, label, sub)
c
c   FUNCTION:  This routine get a character label (up to 64chars) and a sub-num
c				from a input string with label (and subnum), such suf(2)
c
c   INPUT:  rstring    character string:  input full label.
c
c   OUTPUT: label    up to 64 chars.
c           sub: sub number of the label.
c
c***********************************************************************/
void ncl_getlabel_sub (rstring, label, sub)
char *rstring, *label;
int *sub;
{
	int i,j, lnum, snum, nc;
	char tmpbuf[65], substr[20];

	if (rstring==NULL)
	{
		*sub = 0;
		if (label!=NULL)
			label[0] = '\0';
		return;
	}
	nc = strlen(rstring);
/*
......Convert character data to text string
*/
	lnum = 0;
	snum = 0;
	j = 0;
	for (i=0; i<nc; i++)
	{
		if ((rstring[i]!='(')&&(lnum==0))
			tmpbuf[i] = rstring[i];
		else if (rstring[i]=='(')
		{
			tmpbuf[i] = ' ';
			lnum = i;
		}
		else if (rstring[i]==')')
		{
			tmpbuf[i] = ' ';
			snum = j;
			break;
		}
		else if ((lnum>0)&&(snum==0))
		{
			substr[j] = rstring[i];
			tmpbuf[i] = ' ';
			j = j+1;
		}
		else
		{
			tmpbuf[i] = ' ';
		}
	}
	substr[j] = '\0';
	if (lnum>0)
		tmpbuf[lnum] = '\0';
	else
		tmpbuf[i] = '\0';

	strcpy(label, tmpbuf);
	*sub = (short) atoi(substr);
}

