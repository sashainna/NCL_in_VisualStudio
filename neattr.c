/*********************************************************************
**    NAME         :  neattr.c
**       CONTAINS:  routines to manipulate NCL attribute bundles
**         ncl_init_attr_rec()
**         ncl_init_surf_attr()
**         ncl_nclattr_to_uniattr()
**         upattr()
**         ncl_get_entity_color()
**         ncl_set_color_attr()
**         ncl_update_color_()
**         ncl_get_attrmdl_color()
**         ncl_init_attrmdl_color()
**         ncl_set_geo_color()
**         ncl_color_modals()
**         ncl_set_geo_color()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       neattr.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:23
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "umath.h"
#include "uhep.h"
#include "class.h"
#include "mfort.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mattr.h"
#include "mdattr.h"
#include "mdebug.h"
#include "mxxx.h"
#include "lcom.h"
#include "lumb.h"

#include "msrf.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nclver.h"
#include "ncolorddl.h"
#include "mpocket.h"

extern char uw_color_name[64][96];
extern int uw_color_table[64][3];
extern int UM_def_shaded,UM_def_lucency;

static int NCL_model_color = -1;
extern int UR_restore_clr;
/*********************************************************************
**    E_FUNCTION     : ncl_init_attr_rec(key, attr, type)
**       Inititalize the attribute bundle (ATTR) for the specified
**			NCL entity (KEY).
**    PARAMETERS   
**       INPUT  : 
**          key					UNIBASE key of NCL entity
**       OUTPUT :  
**          attr					attribute bundle is initialized
**										and associated with the entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_init_attr_rec(key, attr, type)
UU_KEY_ID key;
struct NCL_nclattr_rec *attr;
int   type;
	{
	UM_int2  itype, lblst;
 	int color;

	uu_denter(UU_MTRC,(us,"ncl_init_attr_rec()"));

	/*MILLS: added to support initalization of attribute bundle for tools.
			 See nclc/neinit.c -rah */
	ncl_uni_init_attr_rec(key, attr, 0);

	/* check to see if the label flag for the geometry is set. If so, set 
	   label_on to on. kathy */

	itype=type;
	lblchk(&itype,&lblst);
	attr->label_on = lblst;

	if (NCL_model_color == -1)
 	   ncl_get_entity_color(key, &color);
 	else
	   color = NCL_model_color;

	attr->color = color;

   /* Create NCL point, line and circle attribute as CADD geo attribute */
   if ((type == NCLI_LINE) || (type == NCLI_POINT) || (type == NCLI_CIRCLE))
		attr->rel_num = UM_ATTR_REL;
   if (type == NCLI_MATRIX) attr->line_style = 3;

	ur_update_attr(attr);

	uu_dexit;
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     : ncl_init_surf_attr(key,attr,type)
**      Initializes Surface and solid attribute record and associates
**      it with the given entity.
**    PARAMETERS   
**       INPUT  :
**          key         Key of entity to initialize attributes for.
**       OUTPUT :  
**          attr        attribute bundle
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_init_surf_attr(key,attr,type)
UU_KEY_ID key;
struct UM_surfattr_rec *attr;
int type;
{
	UM_int2 itype,lblst;
	int color,ecolor,ecolor1,status;
/*
.....Initialize standard attributes
*/
	uu_fill_bytes(attr,0,sizeof(struct UM_surfattr_rec));
	ncl_uni_init_attr_rec(key,attr,UU_FALSE);
	attr->rel_num = UM_SURFATTR_REL;
/*
.....Initialize label display
*/
	itype = type;
	lblchk(&itype,&lblst);
	attr->label_on = lblst;
/*
.....Initialize color
*/
	if (NCL_model_color == -1)
		ncl_get_entity_color(key,&color);
	else
		color = NCL_model_color;
	attr->color = color;
/*
.....Initialize surface attributes
*/
	attr->material = UM_srfattr.material;
	attr->numupaths = UM_srfattr.numupaths;
	attr->numvpaths = UM_srfattr.numvpaths;
	attr->ptsperucrv = UM_srfattr.ptsperucrv;
	attr->ptspervcrv = UM_srfattr.ptspervcrv;
	attr->shaded = UM_srfattr.shaded;
	attr->lucency = UM_srfattr.lucency;

	ecolor1 = -1;
	ecolor = UM_srfattr.edge_color;
	if (UM_srfattr.edge_disp == 1)
	{
		if (ecolor == -1)
			ecolor1 = 64;
		else
			ecolor1 = ecolor;
	}
	attr->ecolor = ecolor1;
/*
.....Update attribute record
*/
	status = ur_update_attr(attr);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_nclattr_to_uniattr(nclattr, uniattr)
**       Initialize a UNICAD modelling attribute bundle from an
**			NCL attribute bundle.
**    PARAMETERS   
**       INPUT  : 
**          nclattr				pointer to NCL attribute bundle
**       OUTPUT :  
**          uniattr				pointer to UNICAD modeler attribute bundle
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_nclattr_to_uniattr(nclattr, uniattr)
	struct NCL_nclattr_rec *nclattr;
	struct UM_attrdata_rec *uniattr;

	{

	uu_denter(UU_MTRC,(us,"ncl_nclattr_to_uniattr(nclattr=%x, uniattr=%x)",
		nclattr, uniattr));

	uniattr->key = -1;
	uniattr->rel_num = UM_ATTR_REL;
	uniattr->use_count = 0;
	uniattr->color = nclattr->color;
	uniattr->layer = nclattr->layer;
	uniattr->pen = nclattr->pen;
	uniattr->line_style = nclattr->line_style;
	uniattr->line_weight = nclattr->line_weight;
	uniattr->line_width = nclattr->line_width;
	uniattr->displayable = nclattr->displayable;
	uniattr->selectable = nclattr->selectable;
	uniattr->label_on = nclattr->label_on;
	/* MPE 26 now maintains blanked in MTID rec *
	uniattr->blanked = nclattr->blanked;
	*/

	uu_dexit;
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     : upattr(oldkey,newkey)
**       Update the attributes of NCL entity newkey with the attributes of
**       entity oldkey.
**    PARAMETERS   
**       INPUT  : 
**          oldkey       - key of entity with atts to use
**          newkey       - key of entity to update atts
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int upattr(oldkey,newkey)
   UM_int4 *oldkey, *newkey;

   {

   struct UC_attributedatabag nclattr;
   struct NCL_fixed_databag sf;
   UU_KEY_ID okey, nkey;
   UU_LOGICAL blanked;
   int status;
   int rel_num;

   uu_denter(UU_MTRC,(us,"upattr()"));

   okey = *oldkey;
   nkey = *newkey;
   nclattr.key = okey;
   status = ur_retrieve_attr(&nclattr);
   if (status == UU_SUCCESS)
      {
      nclattr.key = nkey;
      status = ur_update_attr(&nclattr);
      if (status == UU_SUCCESS)
         {
         ur_retrieve_blanked(okey, &blanked);
         ur_update_blanked(nkey, blanked);
         }
      }
   /*RAH: end preserve of display attributes of surfaces */

   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_get_entity_color(key, color)
**       Return the default color for NCL entity.
**    PARAMETERS   
**       INPUT  : 
**          key       - key of entity to return color for
**       OUTPUT :  
**          color     - color of this entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_entity_color(key, color)
   UU_KEY_ID key;
   int *color;
   {

   int rel_num, status;

   uu_denter(UU_MTRC,(us,"ncl_get_entity_color()"));
/*
.....return the default color for the entity form the color modal variable
*/
   status = UU_SUCCESS;
   *color = UM_YELLOW;
   if (ur_retrieve_data_relnum(key, &rel_num) != 0)
      status = UU_FAILURE;
   else
	switch (rel_num)
	{
		case NCL_POINT_REL:
		case UM_POINT_REL:
			*color = UL_color_mod.point;
			break;
		case NCL_VECTOR_REL:
			*color = UL_color_mod.vector;
			break;
		case NCL_POINTVEC_REL:
			*color = UL_color_mod.pntvec;
			break;
		case NCL_LINE_REL:
		case UM_LINE_REL:
			*color = UL_color_mod.line;
			break;
		case NCL_PLN_REL:
			*color = UL_color_mod.plane;
			break;
		case NCL_CIRCLE_REL:
		case UM_CIRCLE_REL:
			*color = UL_color_mod.circle;
			break;
		case UM_RBSPLCRV_REL:
			*color = UL_color_mod.spline;
			break;
		case NCL_CURVE_REL:
			*color = UL_color_mod.nclcv;
			break;
		case UM_UVCVONSF_REL:
			*color = UL_color_mod.sspline;
			break;
		case UM_COMPCRV_REL:
			*color = UL_color_mod.comp;
			break;
		case NCL_SURF_REL:
			*color = UL_color_mod.nclsf;
			break;
		case NCL_REVSURF_REL:
			*color = UL_color_mod.rvsf;
			break;
		case NCL_MESHSURF_REL:
		case NCL_QUILTSURF_REL:
		case NCL_NETSF_REL:
			*color = UL_color_mod.netsf;
			break;
		case UM_RBSPLSRF_REL:
			*color = UL_color_mod.nurbs;
			break;
		case NCL_TRIMSF_REL:
			*color = UL_color_mod.trimsf;
			break;
		case NCL_SHAPE_REL:
			*color = UL_color_mod.shape;
			break;
		case NCL_MATRIX_REL:
            *color = UL_color_mod.matrix;
			break;
		case NCL_PATERN_REL:
            *color = UL_color_mod.patern;
			break;
		case UM_SOLID_REL:
            *color = UL_color_mod.solid;
			break;
		default:
            status = UU_FAILURE;
            break;
         }

   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_set_color_attr (color)
**       Handle the return from the change color menu
**    PARAMETERS   
**       INPUT  : 
**          color       - index of menu choice
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : sets NCL_model_color
**    WARNINGS     : none
*********************************************************************/
int
ncl_set_color_attr(color)
   int color;
   {

   int status;

   uu_denter(UU_MTRC,(us,"ncl_set_color_attr()"));

   status = UU_SUCCESS;

   NCL_model_color = color;
   if (color == -1)
   {
      status = ur_put_attrmdl_color(UM_YELLOW);
   }
   else
      {
      status = ur_put_attrmdl_color(color);
      }

   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_update_color (key, color)
**       Update color of this entity.
**    PARAMETERS   
**       INPUT  : 
**          key          - key of entity
**          color        - color  -1: default, others defined in wsgl.h
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_update_color(key,color)
   UU_KEY_ID key;
   int color;
   {

   int lcolor, status;

   uu_denter(UU_MTRC,(us,"ncl_update_color_()"));

   status = UU_SUCCESS;

   lcolor = color;
   if (color == -1)
      ncl_get_entity_color(key, &lcolor);

   status = ur_update_color(key,lcolor);

   uu_dexit;
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_def_color (key)
**       Update color of this entity to current NCL color or default.
**    PARAMETERS   
**       INPUT  : 
**          key          - key of entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_def_color(key)
   UU_KEY_ID key;
   {

   int lcolor, status;

   uu_denter(UU_MTRC,(us,"ncl_def_color_()"));

   if (NCL_model_color == -1)
       ncl_get_entity_color(key, &lcolor);
    else
      lcolor = NCL_model_color;

   status = ur_update_color(key,lcolor);

   uu_dexit;
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_get_attrmdl_color();
**       Return current model color
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : current model color
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_attrmdl_color()
   {

   uu_denter(UU_MTRC,(us,"ncl_get_attrmdl_color_()"));
   uu_dexit;

   return(NCL_model_color);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_attrmdl_color();
**       Initialize the current model color.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : current model color
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_attrmdl_color()
   {

   uu_denter(UU_MTRC,(us,"ncl_init_attrmdl_color_()"));

   NCL_model_color = -1;
   
   uu_dexit;

   return(0);
   }
/*********************************************************************
**    E_FUNCTION     : S_save_modfile
**       Save the default colors into modals file.
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
	int i, stat;
	char msg[80];
	FILE *fptr;
	UX_pathname fname;
	char slcolor[65][96];
/*
.....Initialize routine
*/
	strcpy(slcolor[0], "*AUTO");
	for (i=0; i<64;i++)
	{
		sprintf(slcolor[i+1], "*%s", uw_color_name[i]);
	}
	stat = UU_SUCCESS;
/*
.....Open modals file
*/
	strcpy (fname, "ncl_colors.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS", "modals", UU_NULL, UU_NULL,
					fname, 3, &fptr);
	if ((stat!=UU_SUCCESS)||(fptr==UU_NULL)) goto done;
/*
.....Store color modals
*/
	ux_fputs0("#GEO_COLORS#\n", fptr);

	sprintf(msg,"/POINT/ %s\n",slcolor[UL_color_mod.point+1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/LINE/ %s\n",slcolor[UL_color_mod.line+1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/CIRCLE/ %s\n",slcolor[UL_color_mod.circle+1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/PLANE/ %s\n",slcolor[UL_color_mod.plane+1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/VECTOR/ %s\n",slcolor[UL_color_mod.vector+1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/PNTVEC/ %s\n",slcolor[UL_color_mod.pntvec+1]);
	ux_fputs0(msg, fptr);
	
	sprintf(msg,"/MATRIX/ %s\n",slcolor[UL_color_mod.matrix+1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/PATERN/ %s\n",slcolor[UL_color_mod.patern+1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/SHAPE/ %s\n",slcolor[UL_color_mod.shape+1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/NCL_CV/ %s\n",slcolor[UL_color_mod.nclcv+1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/SPLINE/ %s\n",slcolor[UL_color_mod.spline+1]);
	ux_fputs0(msg, fptr);
	
	sprintf(msg,"/SSPLINE/ %s\n",slcolor[UL_color_mod.sspline+1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/COMP_CV/ %s\n",slcolor[UL_color_mod.comp+1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/NCL_SURF/ %s\n",slcolor[UL_color_mod.nclsf+1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/NURBS_SURF/ %s\n",slcolor[UL_color_mod.nurbs+1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/TRIM_SURF/ %s\n",slcolor[UL_color_mod.trimsf+1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/REV_SURF/ %s\n",slcolor[UL_color_mod.rvsf+1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/NET_SURF/ %s\n",slcolor[UL_color_mod.netsf+1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/SOLID/ %s\n",slcolor[UL_color_mod.solid+1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/MAXIS/ %s\n",slcolor[UL_color_mod.maxis+1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/WAXIS/ %s\n",slcolor[UL_color_mod.waxis+1]);
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
**  E_FUNCTION         :  ncl_color_modals()
**     Process the geometry color modals form.
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
ncl_color_modals()
{
	int i,*ans[22],status;
	int orig[21],chg[21];
	int outputcmd;
	static char traverse[] = { 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
	static char display[] =	 { 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
	NCL_cmdbuf cmdbuf;
	char buf[256];
	char scolor[65][96];
	static char *svocab[21] = {"POINT","LINE","CIRCLE","PLANE","VECTOR",
		"PNTVEC","MATRIX","PATERN","SHAPE","CURVE,0","CURVE,1","CURVE,2",
		"CURVE,3","SURF,0","SURF,1","SURF,2","SURF,3","SURF,4","SOLID",
		"MAXIS","WAXIS"};

	strcpy(scolor[0], "DEFALT");
	for (i=0; i<64;i++)
	{
		sprintf(scolor[i+1], "%s", uw_color_name[i]);
	}
/*
...Set default values
*/
	orig[0] = chg[0] = UL_color_mod.point; 
	orig[1] = chg[1] = UL_color_mod.line; 
	orig[2] = chg[2] = UL_color_mod.circle; 
	orig[3] = chg[3] = UL_color_mod.plane; 
	orig[4] = chg[4] = UL_color_mod.vector; 
	orig[5] = chg[5] = UL_color_mod.pntvec;
	orig[6] = chg[6] = UL_color_mod.matrix; 
	orig[7] = chg[7] = UL_color_mod.patern; 
	orig[8] = chg[8] = UL_color_mod.shape; 
	orig[9] = chg[9] = UL_color_mod.nclcv; 
	orig[10] = chg[10] = UL_color_mod.spline; 
	orig[11] = chg[11] = UL_color_mod.comp; 
	orig[12] = chg[12] = UL_color_mod.sspline;
	orig[13] = chg[13] = UL_color_mod.nclsf; 
	orig[14] = chg[14] = UL_color_mod.nurbs; 
	orig[15] = chg[15] = UL_color_mod.trimsf; 
	orig[16] = chg[16] = UL_color_mod.rvsf; 
	orig[17] = chg[17] = UL_color_mod.netsf; 
	orig[18] = chg[18] = UL_color_mod.solid; 
	orig[19] = chg[19] = UL_color_mod.maxis; 
	orig[20] = chg[20] = UL_color_mod.waxis; 

	outputcmd = 0;
	for(i=0;i<21;i++)
		ans[i] = (int *)&chg[i];
	
	ans[21] = (int *)&outputcmd;
	
/*
...Get input from user
*/
	status = ud_form1("chgclr.frm", ans, ans,UU_NULL,UU_NULL,display,traverse);
	if (status == -1) return (status);

	for(i=0;i<21;i++)
	{
		if(orig[i] != chg[i])
		{	
			ncl_init_cmdbuf(&cmdbuf);
			if(outputcmd)
				strcpy(buf,"DRAFT/DEFALT");
			else
				strcpy(buf,"*DRAFT/DEFALT");
			ncl_add_token(&cmdbuf, buf, NCL_comma);
			sprintf(buf,"%s",svocab[i]);
			ncl_add_token(&cmdbuf,buf,NCL_comma);
			sprintf(buf,"COLOR=%s",scolor[chg[i]+1]);
			ncl_add_token(&cmdbuf,buf,NCL_comma);
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	}
	S_save_modfile();
	return(status);
}
/*********************************************************************
**    E_FUNCTION     : ncl_set_geo_color(gtype,stype,newcolor)
**       Set the default color for geometry types 
**    PARAMETERS   
**       INPUT  : 
**          gtype: geometry type
**			stype: subtype for surf and curve
**			newcolor: new color for geom type 
**       OUTPUT :  
**          none
**    RETURNS      : none
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
void ncl_set_geo_color(gtype,stype,newcolor)
int *gtype,*stype,*newcolor;
{
	struct UM_coordsys_rec cpln;
	struct UM_attrdata_rec attr;

	switch(*gtype)
	{
		case 3:/*point*/
			/*Set default color if AUTO*/
			if(*newcolor==-1)*newcolor=UM_DARKRED;
			UL_color_mod.point = *newcolor;
			break;
		case 4:/*vector*/
			/*Set default color if AUTO*/
			if(*newcolor==-1)*newcolor=UM_DARKRED;
			UL_color_mod.vector = *newcolor;
			break;
		case 5:/*line*/
			/*Set default color if AUTO*/
			if(*newcolor==-1)*newcolor=UM_DARKGREEN;
			UL_color_mod.line = *newcolor;
			break;
		case 6:/*plane*/
			/*Set default color if AUTO*/
			if(*newcolor==-1)*newcolor=UM_DARKGREEN;
			UL_color_mod.plane = *newcolor;
			break;
		case 7:/*circle*/
			/*Set default color if AUTO*/
			if(*newcolor==-1)*newcolor=UM_BLACK;
			UL_color_mod.circle = *newcolor;
			break;
		case 8:/*curve*/
			switch(*stype)
			{
				case 0:/*ncl curve*/
					/*Set default color if AUTO*/
					if(*newcolor==-1)*newcolor=UM_CYAN;
					UL_color_mod.nclcv = *newcolor;
					break;
				case 1:/*spline*/
					/*Set default color if AUTO*/
					if(*newcolor==-1)*newcolor=UM_CYAN;
					UL_color_mod.spline = *newcolor;
					break;
				case 2:/*comp*/
					/*Set default color if AUTO*/
					if(*newcolor==-1)*newcolor=UM_YELLOW;
					UL_color_mod.comp = *newcolor;
					break;
				case 3:/*sspline*/
					/*Set default color if AUTO*/
					if(*newcolor==-1)*newcolor=UM_BLUE;
					UL_color_mod.sspline = *newcolor;
					break;
			}
			break;
		case 9:/*surf*/
			switch(*stype)
			{
				case 0:/*NCL surf*/
					/*Set default color if AUTO*/
					if(*newcolor==-1)*newcolor=UM_DARKBLUE;
					UL_color_mod.nclsf = *newcolor;
					break;
				case 1:/*NURBS*/
					/*Set default color if AUTO*/
					if(*newcolor==-1)*newcolor=UM_DARKBLUE;
					UL_color_mod.nurbs = *newcolor;
					break;
				case 2:/*trim sf*/
					/*Set default color if AUTO*/
					if(*newcolor==-1)*newcolor=UM_YELLOW;
					UL_color_mod.trimsf = *newcolor;
					break;
				case 3:/*rev surf*/
					/*Set default color if AUTO*/
					if(*newcolor==-1)*newcolor=UM_DARKBLUE;
					UL_color_mod.rvsf = *newcolor;
					break;
				case 4:/*net surf*/
					/*Set default color if AUTO*/
					if(*newcolor==-1)*newcolor=UM_DARKBLUE;
					UL_color_mod.netsf = *newcolor;
					break;
			}
			break;
		case 10:/*matrix*/
			/*Set default color if AUTO*/
			if(*newcolor==-1)*newcolor=UM_DARKBLUE;
			UL_color_mod.matrix = *newcolor;
			break;
		case 18:/*shape*/
			/*Set default color if AUTO*/
			if(*newcolor==-1)*newcolor=UM_DARKBLUE;
			UL_color_mod.shape = *newcolor;
			break;
		case 20:/*patern*/
			/*Set default color if AUTO*/
			if(*newcolor==-1)*newcolor=UM_YELLOW;
			UL_color_mod.patern = *newcolor;
			break;
		case 21:/*pntvec*/
			/*Set default color if AUTO*/
			if(*newcolor==-1)*newcolor=UM_DARKRED;
			UL_color_mod.pntvec = *newcolor;
			break;
		case 33:/*solid*/
			/*Set default color if AUTO*/
			if(*newcolor==-1)*newcolor=UM_LIGHTGREEN;
			UL_color_mod.solid = *newcolor;
			break;
		case -1:/*maxis*/
		case -2:/*waxis*/
			if (*gtype == -1)
			{
				if(*newcolor==-1)*newcolor=UM_YELLOW;
				UL_color_mod.maxis = *newcolor;
				cpln.key = um_get_modaxis_key();
			}
			else
			{
				if(*newcolor==-1)*newcolor=UM_CYAN;
				UL_color_mod.waxis = *newcolor;
				cpln.key = ur_get_dispattr_cpln_key();
			}
			if (cpln.key == 0) break;
			uc_retrieve_attr(cpln.key,&attr);
			attr.color = *newcolor;
			ur_update_attr(&attr);
			uc_retrieve_data(&cpln,sizeof(cpln));
			uc_display(&cpln);
			break;
		default:
			break;
	}
}
