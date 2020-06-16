/*********************************************************************
**    NAME         :  tspmattr.c
**       CONTAINS:
**             utp_create_unique_layer
**             utp_free_attr_lists
**             utp_get_start_layer
**             utp_get_layer_flag
**					utp_get_attr_flag
**					utp_get_marker
**					utp_in_attr_color
**					utp_in_attr_line_style
**					utp_in_attr_rec
**             utp_in_marker_type
**					utp_init_attr_recs
**					utp_init_attr_stack
**					utp_init_surfattr
**             utp_merge_custom_color
**             utp_out_attributes
**             utp_out_push_attr
**					utp_presentation_layer
**					utp_set_attr
**					utp_set_attr_flag
**					utp_set_default_attr
**             utp_set_layer_flag
**             utp_set_start_layer
**					utp_store_attr
**					utp_store_custom_color
**					utp_store_layers
**					utp_store_surfattr
**					utp_styled_item
**    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tspmattr.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       10/27/16 , 15:19:55
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "tstep.h"
#include "mcrv.h"
#include "mdattr.h"
#include "mdrel.h"
#include "msrf.h"
#include "mxxx.h"
#include "nccs.h"
#include "ncl.h"
#include "nconst.h"
#include "class.h"
#include "udebug.h"

extern int UIG_color_iges;

typedef struct
{
	int *recno;
	int layer;
	int nrec;
	int maxrec;
	char label[NCL_MAX_LABEL];
	char desc[80];
} Slayer_struc;

typedef struct
{
	int recno;
	int attr;
} Sattr_struc;

typedef struct
{
	int recno;
	int marker;
} Smarker_struc;

typedef struct
{
	int recno;
	int rel_num;
	int color;
	int marker;
} Scolor_struc;

static int Slayer=1,Scur_layer=1,Sstart_layer=1;
static int Sncolor=UIG_STDCOLOR,Sncust_color=0,Smarker=1;
static int Srecno;
static UU_LOGICAL Slayinit=UU_FALSE,Sattinit=UU_FALSE,Sattr_flag=UU_FALSE;
static UU_LOGICAL Smarkinit=UU_FALSE,Scolorinit=UU_FALSE;
static UU_LOGICAL Slayact=UU_FALSE,Sunique_layers=UU_FALSE;
static struct UC_attributedatabag Sattr,Sattr_shell;
static UU_LIST Slay_list,Sattr_list,Satrec_list,Smarker_list,Scolor_list;
static int S_color_cust_table[UIG_MAXCOLOR-UIG_STDCOLOR][3];
static char S_color_cust_name[UIG_MAXCOLOR-UIG_STDCOLOR][96];
static char Sbuf[81020],Sbuf1[81020];
static char Sdeflab[]={"NONE"};
static char *Scolor_name[]={"black", "white", "blue", "red", "green", "magenta",
	"yellow", "cyan"};


static void S_init_attr();
static void S_reset_attr();
static void S_store_layer_rec();

/*********************************************************************
**    E_FUNCTION     :  utp_create_unique_layer(recno)
**				Creates a new unique layer for the current shape.
**    PARAMETERS   
**       INPUT  : 
**          ptr    Record to be associated with layer.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_create_unique_layer(ptr)
UTPs_step_record *ptr;
{
	int i,j;
	Slayer_struc *lptr,layer;
	UTPs_step_record *tptr,*lab_ptr,*sptr;
	char label[80],*p;
	UU_LOGICAL labeled;
	static int Scomponent;
	
	if (!Sunique_layers)	return;
/*
.....Search for label and shape representation.
.......Currently it is assumed that the label will be defined in the
.......parmeters of a SHAPE_REPRESENTATION and the shape will be defined
.......in the parameters of a ADVANCED_BREP_SHAPE_REPRESENTATION.
*/
	lab_ptr = sptr = tptr = UU_NULL;
	for (i=0;i<ptr->nparm;i++)
	{
		if (ptr->parm[i].type == UTP_RECNO)
		{
			tptr = utp_get_record(ptr->parm[i].ptype.recno);
			if (tptr->command == SHAPE_REPRESENTATION)
				lab_ptr = tptr;
			else if (tptr->command == ADVANCED_BREP_SHAPE_REPRESENTATION)
				sptr = tptr;
		}
	}
/*
.....Search for label if a shape was found.
.......Do nothing and return if no shape found.
*/
	if (sptr != UU_NULL)
	{
		strcpy(label,"Component");
		labeled = UU_FALSE;
		if (lab_ptr != UU_NULL)
		{
			for (i=0;i<lab_ptr->nparm;i++)
			{
				if (lab_ptr->parm[i].type == UTP_STRING)
				{
//					if (strcmp(lab_ptr->parm[i].ptype.str,"") != 0 && 
//						strcmp(lab_ptr->parm[i].ptype.str," ") != 0)
					if (lab_ptr->parm[i].ptype.str[0] != '\0' && 
						lab_ptr->parm[i].ptype.str[0] != ' ')
					{
						strcpy(label,lab_ptr->parm[i].ptype.str);
						labeled = UU_TRUE;
						break;
					}
				}
			}
/*
.....Make sure the first character of the label is upper case.
*/
			p = label;
			if (isalpha(*p) && islower(*p)) *p = toupper(*p);
		}
	}
	else return;
	if (!Slayinit)
	{
		uu_list_init(&Slay_list,sizeof(Slayer_struc),100,100);
		Slayinit = UU_TRUE;
		Scur_layer = Sstart_layer;
		Scomponent = 1;
	}
/*
.....Check if layer is already in stack.
*/
	else
	{
		lptr = (Slayer_struc *)UU_LIST_ARRAY(&Slay_list);
		for (i=0;i<UU_LIST_LENGTH(&Slay_list);i++)
		{
			for (j=0;j<lptr[i].nrec;j++)
			{
				if (sptr->recno == lptr[i].recno[j])
				{
					return;
				}
			}
		}
	}
	layer.layer = Scur_layer;
	Slayer = Scur_layer - 1;
	sprintf(layer.label,"%d",Scur_layer);
	if (labeled) sprintf(layer.desc,"%s",label);
	else sprintf(layer.desc,"%s %d",label,Scomponent++);
	Scur_layer++;
	layer.nrec = layer.maxrec = 1;
	layer.recno = (int *)uu_malloc(sizeof(int));
	layer.recno[0] = sptr->recno;
	uu_list_push(&Slay_list,&layer);
}

/*********************************************************************
**    E_FUNCTION     :  utp_free_attr_list()
**				Empties the attribute lists and frees the memory.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_free_attr_lists()
{
	if (Slayinit)
	{
		uu_list_free(&Slay_list);
		Slayinit = UU_FALSE;
		Slayer = 1;
	}
	if (Sattinit)
	{
		uu_list_free(&Sattr_list);
		uu_list_free(&Satrec_list);
		Sattinit = UU_FALSE;
	}
	if (Smarkinit)
	{
		uu_list_free(&Smarker_list);
		Smarkinit = UU_FALSE;
	}
	if (Scolorinit)
	{
		uu_list_free(&Scolor_list);
		Scolorinit = UU_FALSE;
	}
}

/*********************************************************************
**    E_FUNCTION     :  utp_get_start_layer()
**       Returns the starting layer number defined in nclstep.mod.
**
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      :	Starting layer number.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_get_start_layer()
{
	return(Sstart_layer);
}

/*********************************************************************
**    E_FUNCTION     :  utp_get_layer_flag()
**				Gets the flag that denotes whether each component will be
**          stored on a uniquie layer.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL utp_get_layer_flag()
{
	return(Sunique_layers);
}

/*********************************************************************
**    E_FUNCTION     :  utp_get_attr_flag(flag)
**				Determines if the attribute definition loop is currently
**          active.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : UU_TRUE if attribute definition loop is active.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL utp_get_attr_flag()
{
	return(Sattr_flag);
}

/*********************************************************************
**    E_FUNCTION     :  utp_get_marker(recno)
**				Returns the defined marker type for the specified record.
**    PARAMETERS   
**       INPUT  :
**          recno    = Record number of point to return marker for.
**       OUTPUT : none
**    RETURNS      : Predefined marker type.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_get_marker(recno)
int recno;
{
	int marker,i;
	Smarker_struc *mptr;
/*
.....Search for requested marker type
*/
	marker = 2;
	if (Smarkinit)
	{
		mptr = (Smarker_struc *)UU_LIST_ARRAY(&Smarker_list);
		for (i=0;i<UU_LIST_LENGTH(&Smarker_list);i++)
		{
			if (recno == mptr[i].recno)
			{
				marker = mptr[i].marker;
				break;
			}
		}
	}
/*
.....End of routine
*/
	return(marker);
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_attr_color(ptr)
**				Processes the COLOUR_RGB and DRAUGHTING_PRE_DEFINED_COLOUR
**          records and stores the defined color in the color table and
**          active attribute bundle.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Step record to process.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_in_attr_color(ptr)
UTPs_step_record *ptr;
{
	int i,rgb[3],inc;
	char label[NCL_MAX_LABEL];
/*
.....Get color name
*/
	if (ptr->parm[0].type == UTP_STRING)
	{
		inc = 1;
		strcpy(label,ptr->parm[0].ptype.str);
		ul_to_upper(label);
	}
	else
	{
		inc = 0;
		label[0] = '\0';
	}
/*
.....DRAUGHTING_PRE_DEFINED_COLOUR
.....Based on color label
*/
	if (ptr->command == DRAUGHTING_PRE_DEFINED_COLOUR)
	{
		inc = S_match_color_label(label);
		if (inc == -1) inc = 15;
	}
/*
.....COLOUR_RGB
.....Get RGB values
*/
	else
	{
		for (i=inc;i<ptr->nparm;i++)
		{
			if (ptr->parm[i].type != UTP_REAL) goto done;
			rgb[i-inc] = ptr->parm[i].ptype.value * 255 + .5;
		}
/*
.....Find closest match
*/
		inc = S_match_color(rgb);
/*
.....Color is not in table
.....Place it in the color table
*/
		if (inc < 0)
		{
			if (Sncolor < UIG_MAXCOLOR)
			{
				if (label[0] != '\0' && !S_color_name_exists(label))
					strcpy(uw_color_name[Sncolor],label);
				else
					sprintf(uw_color_name[Sncolor],"STEP_COLOR_%d",Sncolor);
				uw_color_table[Sncolor][0] = rgb[0];
				uw_color_table[Sncolor][1] = rgb[1];
				uw_color_table[Sncolor][2] = rgb[2];
				inc = Sncolor++;
			}
/*
........Color table is full
*/
			else
				inc = -(inc-1);
		}
	}
/*
.....Store color index in active attribute bundle
*/
	Sattr.color = inc;
done:;
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_attr_line_style(ptr)
**				Processes the DRAUGHTING_PRE_DEFINED_CURVE_FONT record
**          and stores the defined line style in the active attribute
**          bundle.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Step record to process.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_in_attr_line_style(ptr)
UTPs_step_record *ptr;
{
	int i;
	char label[NCL_MAX_LABEL];
	int nstyle=3;
	static char *lstyle[]={"CONTINUOUS","CHAIN","DASHED"};
	static int linc[]={0,0,1};
/*
.....Get line style name
*/
	if (ptr->parm[0].type == UTP_STRING)
	{
		strcpy(label,ptr->parm[0].ptype.str);
		ul_to_upper(label);
/*
.....See if it matches any predefined line styles
*/
		for (i=0;i<nstyle;i++)
		{
			if (strcmp(label,lstyle[i]) == 0)
			{
				Sattr.line_style = linc[i];
				break;
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_attr_rec(ptr)
**				Processes the following records.
**
**                   FILL_AREA_STYLE
**                   FILL_AREA_STYLE_COLOUR
**                   PRESENTATION_STYLE_ASSIGNMENT
**                   SURFACE_SIDE_STYLE
**                   SURFACE_STYLE_FILL_AREA
**                   SURFACE_STYLE_USAGE
**    PARAMETERS   
**       INPUT  : 
**          ptr      Step record to process.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_in_attr_rec(ptr)
UTPs_step_record *ptr;
{
	int i,rel;
/*
.....Process all sub-attribute records
*/
	for (i=0;i<ptr->nparm;i++)
	{
		if (ptr->parm[i].type == UTP_RECNO)
			utp_in_dispat_recno(ptr->parm[i].ptype.recno,&rel);
	}
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_marker_type(ptr)
**				Processes the PRE_DEFINED_MARKER record and stores the
**          defined marker type in the active attribute
**          bundle.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Step record to process.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_in_marker_type(ptr)
UTPs_step_record *ptr;
{
	int i;
	char label[NCL_MAX_LABEL];
	int nmarker=13;
	Smarker_struc marker;
	static char *mtype[]={"PLUS","+", "ASTERISK","STAR","*", "CIRCLE","O",
		"CROSS","X", "TRIANGLE", "DIAMOND", "SQUARE", "DOT"};
	static int minc[]={2,2, 3,3,3, 4,4, 5,5, 6, 7, 8, 10};
/*
.....Get marker type name
*/
	if (ptr->parm[0].type == UTP_STRING)
	{
		strcpy(label,ptr->parm[0].ptype.str);
		ul_to_upper(label);
/*
.....See if it matches any predefined markers
*/
		for (i=0;i<nmarker;i++)
		{
			if (strcmp(label,mtype[i]) == 0)
			{
				if (!Smarkinit)
				{
					uu_list_init(&Smarker_list,sizeof(Smarker_struc),100,100);
					Smarkinit = UU_TRUE;
				}
				marker.marker = minc[i];
				marker.recno = Srecno;
				uu_list_push(&Smarker_list,&marker);
				break;
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     :  utp_init_attr_recs()
**				Initializes the attribute records stack.  Typically called
**          at the start of a root geometry definition when creating a
**          STEP file.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_init_attr_recs()
{
	int i,rel;
/*
.....Define the default attributes
*/
	S_reset_attr(&Sattr);
/*
.....Initialize attribute stack
*/
	if (Sattinit) uu_list_free(&Satrec_list);
	uu_list_init(&Satrec_list,sizeof(Sattr_struc),2000,1000);
}

/*********************************************************************
**    E_FUNCTION     :  utp_init_attr_stack()
**				Initializes the attribute stack using the default attributes.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_init_attr_stack()
{
	int i,rel;
/*
.....Define the default attributes
*/
	S_reset_attr(&Sattr);
/*
.....Initialize attribute stack
*/
	if (Sattinit)
	{
		uu_list_free(&Sattr_list);
		uu_list_free(&Satrec_list);
	}
	uu_list_init(&Sattr_list,sizeof(struct UC_attributedatabag),200,100);
	uu_list_init(&Satrec_list,sizeof(Sattr_struc),2000,1000);
	Sattinit = UU_TRUE;
	uu_list_push(&Sattr_list,&Sattr);
}

/*********************************************************************
**    E_FUNCTION     :  utp_init_surfattr(attr,color)
**				Initializes a surface attribute record.
**    PARAMETERS   
**       INPUT  : 
**          color    Color of entity.
**          ecolor   Edge color of entity.
**          edisp    Edge display flag of entity.
**          shaded   UU_TRUE:  Display as shaded.
**                   UU_FALSE: Display as wireframe.
**          edgefl   UU_TRUE:  Use input edge attributes.
**                   UU_FALSE: Use default edge attributes.
**       OUTPUT :
**          attr     Attribute bundle to initialize.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_init_surfattr(attr,color,ecolor,edisp,shaded,edgefl)
struct UM_surfattr_rec *attr;
int color,ecolor,edisp;
UU_LOGICAL shaded,edgefl;
{
/*
.....Initialize the attribute record
*/
	S_init_attr(attr,color);
	attr->rel_num = UM_SURFATTR_REL;
	attr->material = 0;
	attr->numupaths = attr->numvpaths = 5;
	attr->ptsperucrv = attr->ptspervcrv = 0;
   if (edgefl) 
	{
		if (edisp) attr->ecolor = ecolor;
		else attr->ecolor = -1;
	}
	else attr->ecolor = 0;
   attr->shaded = shaded;
   attr->lucency = UIG_lucency;
	return;
}

/*********************************************************************
**    E_FUNCTION     :  utp_merge_custom_color()
**          Append the user defined custom colors into the current
**          current color table.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :    none
**    RETURNS      : none
**    SIDE EFFECTS :
**          Updates the global color table.
**    WARNINGS     : none
*********************************************************************/
void utp_merge_custom_color()
{
	int i,j;
	double dx,dy,dz;	
/*
.....Main loop to go through modals file colors
*/
	for (i=0; i<Sncust_color && Sncolor<UIG_MAXCOLOR; i++)
	{
/*
........Look for a matching color
........in the color table
*/
		for (j=0;j<Sncolor;j++)
		{
			dx = 1.0*S_color_cust_table[i][0] - 1.0*uw_color_table[j][0];
			dy = 1.0*S_color_cust_table[i][1] - 1.0*uw_color_table[j][1];
			dz = 1.0*S_color_cust_table[i][2] - 1.0*uw_color_table[j][2];
			if ((abs(dx)<=1.0)&&(abs(dy)<=1.0)&&(abs(dz)<=1.0))
				break;
		}
/*
......No matching entry
......Append modals color to color table
*/
		if (j == Sncolor)
		{
			strcpy(uw_color_name[Sncolor], S_color_cust_name[i]);
			uw_color_table[Sncolor][0] = S_color_cust_table[i][0];
			uw_color_table[Sncolor][1] = S_color_cust_table[i][1];
			uw_color_table[Sncolor][2] = S_color_cust_table[i][2];
			Sncolor++;
		}
	}
}

/*********************************************************************
**    E_FUNCTION :  utp_out_attributes(flag)
**			Outputs the attributes for the records contained in a shell
**       or geometry set to the STEP file.
**    PARAMETERS   
**       INPUT  :
**          flag   = UU_TRUE = Output attribute record for shell.
**                   UU_FALSE = Output attribute record for individual
**                   surfaces.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_out_attributes(flag)
UU_LOGICAL flag;
{
	int i,j,inc,ipt,nattr,nrec,*recs,irec,urec,ncolor,master,jrec,linc,nlay;
	int ien,ist,ofl;
	UU_LOGICAL layer_init;
	char tbuf[20];
	UU_REAL rgb[3];
	struct UC_attributedatabag *aptr;
	Sattr_struc *sptr;
	Scolor_struc *colors,sattr;
	struct UM_layer_rec *lptr;
	UU_LIST layer;

	static char *mtype[]={"dot", "+", "*", "O", "X", "triangle", "diamond",
		"square"};
	static int minc[]={0,1,2,3,4,5,6,7,3,0,7};
/*
.....Attributes are only output with STEP AP214
*/
	recs = UU_NULL;
	layer_init = UU_FALSE;
	if (!UTP_step_214) goto done;
/*
.....Initialize routine
*/
	nattr = UU_LIST_LENGTH(&Satrec_list);
	if (nattr == 0) goto done;
	sptr = (Sattr_struc *)UU_LIST_ARRAY(&Satrec_list);
	aptr = (struct UC_attributedatabag *)UU_LIST_ARRAY(&Sattr_list);
	recs = (int *)uu_malloc(sizeof(int)*nattr);
/*
.....Find appropriate layer if outputting a shell
*/
	S_load_layers(&layer,&nlay);
	lptr = (struct UM_layer_rec *)UU_LIST_ARRAY(&layer);
	layer_init = UU_TRUE;
	ist = 0; ien = nlay;
	if (flag)
	{
		inc = sptr[nattr-1].attr;
		for (i=0;i<nlay;i++)
		{
			if (aptr[inc].layer == lptr[i].num)
			{
				ist = i;
				ien = i + 1;
			}
		}
	}
/*
.....Loop through all defined layers
*/
	for (linc=ist;linc<ien;linc++)
	{
		nrec = 0;
		master = -1;
/*
........Loop through attribute records
........The last record contains the
........record number of the shell
*/
		for (i=nattr-1;i>=0;i--)
		{
			inc = sptr[i].attr;
			if (aptr[inc].layer != lptr[linc].num && !flag) continue;
			if (i == nattr-1)
			{
				if (flag) master = inc;
				else continue;
			}
			else if (inc == master) continue;
/*
...........Get color record
*/
			if (Scolorinit)
			{
				colors = (Scolor_struc *)UU_LIST_ARRAY(&Scolor_list);
				ncolor = UU_LIST_LENGTH(&Scolor_list);
				ipt = -1;
				for (j=0;j<ncolor;j++)
				{
					if (aptr[inc].rel_num == colors[j].rel_num &&
						aptr[inc].color == colors[j].color &&
						aptr[inc].pen == colors[j].marker)
					{
						ipt = j;
						break;
					}
				}
			}
			else
			{
				uu_list_init(&Scolor_list,sizeof(Scolor_struc),UIG_MAXCOLOR,
					UIG_MAXCOLOR);
				Scolorinit = UU_TRUE;
				ipt = -1;
			}
			if (ipt == -1)
			{
				sattr.recno = 0;
				sattr.rel_num = aptr[inc].rel_num;
				sattr.color = aptr[inc].color;
				sattr.marker = aptr[inc].pen;
				uu_list_push(&Scolor_list,&sattr);
				colors = (Scolor_struc *)UU_LIST_ARRAY(&Scolor_list);
				ncolor = UU_LIST_LENGTH(&Scolor_list);
				ipt = ncolor - 1;
			}
/*
........Output color records
*/
			if (colors[ipt].recno == 0)
			{
				if (colors[ipt].color < 8)
					sprintf(Sbuf,"DRAUGHTING_PRE_DEFINED_COLOUR('%s');",
						Scolor_name[colors[ipt].color]);
				else
				{
					for (j=0;j<3;j++)
					{
						rgb[j] = uw_color_table[colors[ipt].color][j];
						rgb[j] = rgb[j] / 255.;
					}
					sprintf(Sbuf,"COLOUR_RGB('',%.6f,%.6f,%.6f);",rgb[0],rgb[1],
						rgb[2]);
				}
				irec = utp_out_record(Sbuf);
/*
........Surface attributes
*/
				if (colors[ipt].rel_num == NCL_SURF_REL)
				{
					sprintf(Sbuf,"FILL_AREA_STYLE_COLOUR('',#%d);",irec);
					irec = utp_out_record(Sbuf);

					sprintf(Sbuf,"FILL_AREA_STYLE('',(#%d));",irec);
					irec = utp_out_record(Sbuf);

					sprintf(Sbuf,"SURFACE_STYLE_FILL_AREA(#%d);",irec);
					irec = utp_out_record(Sbuf);

					sprintf(Sbuf,"SURFACE_SIDE_STYLE('',(#%d));",irec);
					irec = utp_out_record(Sbuf);

					sprintf(Sbuf,"SURFACE_STYLE_USAGE(.BOTH.,#%d);",irec);
					irec = utp_out_record(Sbuf);
				}
/*
........Curve attributes
*/
				else if (colors[ipt].rel_num == NCL_CURVE_REL)
				{
					jrec = utp_out_record(
						"DRAUGHTING_PRE_DEFINED_CURVE_FONT('continuous');");

					sprintf(Sbuf,
						"CURVE_STYLE('',#%d,POSITIVE_LENGTH_MEASURE(1.),#%d);",
						jrec,irec);
					irec = utp_out_record(Sbuf);
				}
/*
........Point attributes
*/
				else if (colors[ipt].rel_num == NCL_POINT_REL)
				{
					sprintf(Sbuf,"PRE_DEFINED_MARKER('%s');",
						mtype[minc[aptr[inc].pen-1]]);
					jrec = utp_out_record(Sbuf);

					sprintf(Sbuf,
						"POINT_STYLE('',#%d,POSITIVE_LENGTH_MEASURE(1.),#%d);",
						jrec,irec);
					irec = utp_out_record(Sbuf);
				}

				sprintf(Sbuf,"PRESENTATION_STYLE_ASSIGNMENT((#%d));",irec);
				irec = utp_out_record(Sbuf);

				colors[ipt].recno = irec;
			}

			sprintf(Sbuf,"STYLED_ITEM('%s',(#%d),#%d);",Sdeflab,colors[ipt].recno,
				sptr[i].recno);
			recs[nrec++] = utp_out_record(Sbuf);
		}
/*
.....Output MECHANICAL_DESIGN-- record
*/
		if (nrec > 0)
		{
			sprintf(Sbuf,"MECHANICAL_DESIGN_GEOMETRIC_PRESENTATION_REPRESENTATION('%s'",
				Sdeflab);
			ofl = 0;
			utp_out_multiple_record(Sbuf,&ofl,&irec);
			utp_get_identity_rec(&irec,&urec);
			for (i=0;i<nrec;i++)
			{
				if (i==0)
					sprintf(tbuf,",(#%d",recs[i]);
				else
					sprintf(tbuf,",#%d",recs[i]);
				utp_out_multiple_record(tbuf,&ofl,&irec);
			}
			sprintf(tbuf,"),#%d);",urec);
			ofl = 2;
			utp_out_multiple_record(tbuf,&ofl,&irec);
/*
.....Output PRESENTATION_LAYER--- record
*/
			sprintf(Sbuf,"PRESENTATION_LAYER_ASSIGNMENT('%d','%s'",lptr[linc].num,
				lptr[linc].name);
			ofl = 0;
			utp_out_multiple_record(Sbuf,&ofl,&irec);
			for (i=0;i<nrec;i++)
			{
				if (i==0)
					sprintf(tbuf,",(#%d",recs[i]);
				else
					sprintf(tbuf,",#%d",recs[i]);
				utp_out_multiple_record(tbuf,&ofl,&irec);
			}
			sprintf(tbuf,"),#%d);",urec);
			ofl = 2;
			utp_out_multiple_record(tbuf,&ofl,&irec);
		}
	}
/*
.....End of routine
*/
done:;
	if (recs != UU_NULL) uu_free(recs);
//wrong, should pass in pointer Yurong
//	if (layer_init) uu_list_free(layer);
	if (layer_init) uu_list_free(&layer);
}

/*********************************************************************
**    E_FUNCTION     :  utp_out_push_attr(irec,attr,style)
**				Pushes an attribute bundle and its associated record onto
**          their lists.
**    PARAMETERS   
**       INPUT  : 
**          recno    Record number to associate with attributes.
**          attr     Attribute bundle to store in list.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_out_push_attr(recno,attr)
int recno;
struct UC_attributedatabag *attr;
{
	int i,inc,rel;
	Sattr_struc atrec,*sptr;
	Slayer_struc *lptr;
	struct UC_attributedatabag *aptr;
/*
.....Don't store STEP attributes
*/
	if (!UTP_step_214) goto done;
/*
.....See if attribute bundle is on stack
*/
	aptr = (struct UC_attributedatabag *)UU_LIST_ARRAY(&Sattr_list);
	for (i=0;i<UU_LIST_LENGTH(&Sattr_list);i++)
	{
		if (S_match_attr(attr,&aptr[i])) break;
	}
/*
.....Push new attribute bundle onto stack
*/
	if (i == UU_LIST_LENGTH(&Sattr_list))
		uu_list_push(&Sattr_list,attr);
/*
.....Associate attribute bundle with STEP record
*/
	atrec.recno = recno;
	atrec.attr = i;
	uu_list_push(&Satrec_list,&atrec);
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    E_FUNCTION     :  utp_presentation_layer(ptr)
**				Processes the PRESENTATION_LAYER_ASSIGNMENT record.
**				Initializes a surface attribute record.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Step record to process.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_presentation_layer(ptr)
UTPs_step_record *ptr;
{
	int i,j,rel,*rec,stat;
	Slayer_struc *lptr,layer;
	UTPs_step_record *tptr;
/*
.....Ignore the layer settings in STEP file if each component will
.....be given a unique layer number.
*/
	if (Sunique_layers) goto done;
/*
.....Initialize layer stack and
.....create default Layer 1
*/
	if (!Slayinit)
	{
		uu_list_init(&Slay_list,sizeof(Slayer_struc),100,100);
		Slayinit = UU_TRUE;
		layer.layer = 1;
		strcpy(layer.label,"1");
		strcpy(layer.desc,"default");
		layer.nrec = layer.maxrec = 0;
		layer.recno = UU_NULL;
		uu_list_push(&Slay_list,&layer);
		Slayer = 0;
	}
/*
.....Named layer
*/
	if (ptr->parm[0].type == UTP_STRING)
	{
/*
........See if it is already on the stack
*/
		lptr = (Slayer_struc *)UU_LIST_ARRAY(&Slay_list);
		for (i=0;i<UU_LIST_LENGTH(&Slay_list);i++)
		{
			if (strcmp(ptr->parm[0].ptype.str,lptr[i].label) == 0)
			{
				Slayer = i;
				break;
			}
		}
/*
........Layer is not on stack
........Add it
*/
		if (i == UU_LIST_LENGTH(&Slay_list))
		{
			if (ptr->parm[0].type == UTP_STRING)
			{
				strcpy(layer.label,ptr->parm[0].ptype.str);
				stat = ul_to_number(layer.label,&layer.layer);
				if (stat != UU_SUCCESS) layer.layer = i + 2;
			}
			else
			{
				layer.layer = i + 2;
				sprintf(layer.label,"%d",layer.layer);
			}
			if (ptr->parm[1].type == UTP_STRING)
				strcpy(layer.desc,ptr->parm[1].ptype.str);
			else
				sprintf(layer.desc,"Layer %d",layer.layer);
			layer.maxrec = ptr->nparm - 2;
			layer.nrec = 0;
			layer.recno = (int *)uu_malloc(layer.maxrec*sizeof(int));
			for (j=0;j<layer.maxrec;j++) layer.recno[j] = 0;
			uu_list_push(&Slay_list,&layer);
			Slayer = i;
		}
	}
/*
.....Unnamed layer
.....Use default layer
*/
	else
		Slayer = 0;
/*
.....Dispatch associated attribute records
*/
	Slayact = UU_TRUE;
	lptr = (Slayer_struc *)UU_LIST_ARRAY(&Slay_list);
	for (i=2;i<ptr->nparm;i++)
	{
		if (ptr->parm[i].type == UTP_RECNO)
		{
			tptr = utp_get_record(ptr->parm[i].ptype.recno);
			if (tptr->command == STYLED_ITEM)
			{
				S_store_layer_rec(&lptr[Slayer],
					tptr->parm[tptr->nparm-1].ptype.recno);
				utp_in_dispat(tptr,&rel);
			}
			else 
				S_store_layer_rec(&lptr[Slayer],ptr->parm[i].ptype.recno);
		}
	}
	Slayact = UU_FALSE;
/*
.....End of routine
*/
done:;
	ptr->used = -1;
	return;
}

/*********************************************************************
**    E_FUNCTION     :  utp_set_attr(ptr,flag)
**				Finds the associated attribute bundle for this record and
**          sets it as the default.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Step record to associate with attribute bundle.
**          flag     UU_TRUE = This is a shell level attribute.  Save
**                   it as the default.
**                   UU_FALSE = This is a face attribute, use the
**                   shell level attribute if one is not found.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_set_attr(ptr,flag)
UTPs_step_record *ptr;
UU_LOGICAL flag;
{
	int i,j,nattr;
	Sattr_struc *satt;
	Slayer_struc *lptr;
	struct UC_attributedatabag *aptr;
/*
.....Initialize to default attribute bundle
*/
if (ptr->recno==268 || ptr->recno == 2386)
{
	i = 0;
}
	satt = (Sattr_struc *)UU_LIST_ARRAY(&Satrec_list);
	aptr = (struct UC_attributedatabag *)UU_LIST_ARRAY(&Sattr_list);
/*
.....Find the associated attribute record
*/
	nattr = UU_LIST_LENGTH(&Satrec_list);
	for (i=0;i<nattr;i++)
	{
		if (ptr->recno == satt[i].recno)
		{
			Sattr = aptr[satt[i].attr];
			if (flag) Sattr_shell = Sattr;
			break;
		}
	}
/*
.....Attribute not found
.....Use the default attribute
*/
	if (i == nattr)
	{
		Sattr = Sattr_shell;
	}
/*
.....Find the associated layer record
*/
	lptr = (Slayer_struc *)UU_LIST_ARRAY(&Slay_list);
	for (i=0;i<UU_LIST_LENGTH(&Slay_list);i++)
	{
		for (j=0;j<lptr[i].nrec;j++)
		{
			if (ptr->recno == lptr[i].recno[j])
			{
				Slayer = lptr[i].layer;
				break;
			}
		}
	}
	Sattr.layer = Slayer;
	return;
}

/*********************************************************************
**    E_FUNCTION     :  utp_set_attr_flag(flag)
**				Sets the flag that determines if the attribute definition
**          loop is currently active.
**    PARAMETERS   
**       INPUT  : 
**          ptr      UU_TRUE if attribute definition loop is active.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_set_attr_flag(flag)
UU_LOGICAL flag;
{
	Sattr_flag = UU_TRUE;
}

/*********************************************************************
**    E_FUNCTION     :  utp_set_default_attr()
**				Sets the default attribute bundle as current.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_set_default_attr()
{
	struct UC_attributedatabag *aptr;
/*
.....Initialize to default attribute bundle
*/
	aptr = (struct UC_attributedatabag *)UU_LIST_ARRAY(&Sattr_list);
	Sattr = Sattr_shell = aptr[0];
}

/*********************************************************************
**    E_FUNCTION     :  utp_set_layer_flag(flag)
**				Sets the flag that denotes whether each component will be
**          stored on a uniquie layer.
**    PARAMETERS   
**       INPUT  :
**           flag    UU_TRUE: Each shape will have a unique layer
**							         generated.
**                   UU_FALSE: Use the layer given in STEP file or
**                             the default if none given.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_set_layer_flag(flag)
UU_LOGICAL flag;
{
	Sunique_layers = flag;
}

/*********************************************************************
**    E_FUNCTION     :  utp_set_start_layer(layer)	
**       Stores the starting layer number defined in nclstep.mod.
**
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      :	Starting layer number.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_set_start_layer(layer)
int layer;
{
	/*Scur_layer =*/ Sstart_layer = layer;
}

/*********************************************************************
**    E_FUNCTION     :  utp_store_attr(key,color)
**				Stores an attribute record in the Unibase.
**    PARAMETERS   
**       INPUT  : 
**          key      Key of entity to define attribute bundle for.
**          color    Color of entity.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_store_attr(key,color)
UU_KEY_ID key;
int color;
{
	struct UC_attributedatabag attr;
	struct NCL_fixed_databag ent;
/*
.....Get the attribute record
*/
	ent.key = attr.key = key;
	ur_retrieve_attr(&attr);
	ncl_retrieve_data_fixed(&ent);
/*
.....Upate the attribute record
*/
	S_init_attr(&attr,color);
	if (strcmp(ent.label,"@UN") == 0)
		attr.displayable = UM_NEVERDISPLAYABLE;
/*
.....Store the attribute record
*/
	ur_update_attr(&attr);
	return;
}

/*********************************************************************
**    E_FUNCTION     :  utp_store_custom_color(name,color)
**				Stores a custom color from the ncl_color.mod file into
**          the local custom color array.
**    PARAMETERS   
**       INPUT  : 
**          name     Name of color to store.
**          color    RGB values to store with name.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_store_custom_color(name,color)
char *name;
int color[];
{
	if (Sncust_color == UIG_MAXCOLOR-UIG_STDCOLOR) return;
	strcpy(S_color_cust_name[Sncust_color],name);
	S_color_cust_table[Sncust_color][0] = color[0];
	S_color_cust_table[Sncust_color][1] = color[1];
	S_color_cust_table[Sncust_color][2] = color[2];
	Sncust_color++;
}

/*********************************************************************
**    E_FUNCTION     :  utp_store_layers()
**				Stores all layer definitions in the Unibase.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_store_layers()
{
	int i,entnum;
	Slayer_struc *lptr;
	struct UM_layer_rec newlayer;
/*
.....Loop through and store all defined layers
*/
	lptr = (Slayer_struc *)UU_LIST_ARRAY(&Slay_list);
	entnum = 0;
	for (i=0;i<UU_LIST_LENGTH(&Slay_list);i++)
	{
		entnum++;
		newlayer.key = 0;
		newlayer.rel_num = UM_LAYER_REL;
		newlayer.num = lptr[i].layer;
		strcpy(newlayer.name,lptr[i].desc);
		newlayer.displayable = UU_TRUE;
		newlayer.selectable  = UU_TRUE;
		newlayer.no_layers = 0;
		newlayer.layers = UU_NULL;
		ur_create_tuple(newlayer.rel_num, &entnum, &newlayer);
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     :  utp_store_surfattr(key,color)
**				Stores a surface attribute record in the Unibase.
**    PARAMETERS   
**       INPUT  : 
**          key      Key of entity to define attribute bundle for.
**          color    Color of entity.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_store_surfattr(key,color)
UU_KEY_ID key;
int color;
{
	struct UM_surfattr_rec attr;
/*
.....Get the attribute record
*/
	attr.key = key;
	ur_retrieve_attr(&attr);
/*
.....Initialize the attribute record
*/
	utp_init_surfattr(&attr,color,0,0,UU_TRUE,UU_FALSE);
/*
.....Upate the attribute record
*/
	ur_update_attr(&attr);
	return;
}

/*********************************************************************
**    E_FUNCTION     :  utp_styled_item(ptr)
**				Processes the OVER_RIDING_STYLED_ITEM and STYLED_ITEM records.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Step record to process.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_styled_item(ptr)
UTPs_step_record *ptr;
{
	int i,inc,rel,recno;
	Sattr_struc atrec,*sptr;
	Slayer_struc *lptr;
	struct UC_attributedatabag *aptr;
/*
.....Don't use STEP attributes
*/
	if (!UIG_color_iges) goto done;
/*
.....Check the syntax
*/
	inc = ptr->nparm - 1;
	if (ptr->command == OVER_RIDING_STYLED_ITEM) inc = ptr->nparm - 2;
	if (ptr->parm[inc].type != UTP_RECNO) goto done;
	Srecno = recno = ptr->parm[inc].ptype.recno;
/*
.....See if this record's attributes are already stored
*/
	sptr = (Sattr_struc *)UU_LIST_ARRAY(&Satrec_list);
	for (i=0;i<UU_LIST_LENGTH(&Satrec_list);i++)
	{
		if (recno == sptr[i].recno) goto done;
	}
/*
.....Define default attributes
*/
	S_reset_attr(&Sattr);
if (recno==268 || recno==2386)
{
	i = 0;
}
/*
.....Define all attributes for entity
*/
//	for (i=1;i<ptr->nparm;i++)
	for (i=1;i<inc;i++)
	{
		if (i != inc && ptr->parm[i].type == UTP_RECNO)
			utp_in_dispat_recno(ptr->parm[i].ptype.recno,&rel);
	}
/*
.....See if attribute bundle is on stack
*/
	aptr = (struct UC_attributedatabag *)UU_LIST_ARRAY(&Sattr_list);
	for (i=0;i<UU_LIST_LENGTH(&Sattr_list);i++)
	{
		if (S_match_attr(&Sattr,&aptr[i])) break;
	}
/*
.....Push new attribute bundle onto stack
*/
	if (i == UU_LIST_LENGTH(&Sattr_list))
		uu_list_push(&Sattr_list,&Sattr);
/*
.....Associate attribute bundle with STEP record
*/
	atrec.recno = recno;
	atrec.attr = i;
	uu_list_push(&Satrec_list,&atrec);
/*
.....Update Layer stack if necessary
*/
	if (Slayact)
	{
		lptr = (Slayer_struc *)UU_LIST_ARRAY(&Slay_list);
		S_store_layer_rec(&lptr[Slayer],recno);
	}
/*
.....End of routine
*/
done:;
	Srecno = 0;
	return;
}

/*********************************************************************
**    I_FUNCTION     :  S_color_name_exists(label)
**				Determines if a color name is already in the color table.
**    PARAMETERS   
**       INPUT  : 
**          label    Color name to check.
**       OUTPUT : none
**    RETURNS      : UU_TRUE if 'label' already exists in color table.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_color_name_exists(label)
char *label;
{
	int i;
/*
.....See if color name already exists
.....in color table
*/
	for (i=0;i<Sncolor;i++)
	{
		if (stricmp(uw_color_name[i],label)==0)
			return UU_TRUE;
	}
	return UU_FALSE;
}

/*********************************************************************
**    I_FUNCTION     :  S_init_attr(attr,color)
**				Initializes the basic attributes of an entity.
**    PARAMETERS   
**       INPUT  : 
**          color    Color of entity.
**       OUTPUT :
**          attr     Initialized attribute bundle.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_init_attr(attr,color)
struct UC_attributedatabag *attr;
int color;
{
/*
.....Initialize the attribute record
*/
	attr->rel_num = UM_ATTR_REL;
	if (Sattr.color == -1) attr->color = color;
	else attr->color = Sattr.color;
	attr->layer = Sattr.layer;
	attr->pen = Sattr.pen;
	attr->line_style = Sattr.line_style;
	attr->line_weight = Sattr.line_weight;
	attr->displayable = Sattr.displayable;
	attr->selectable = Sattr.selectable;
	attr->use_count = 0;
	attr->label_on = Sattr.label_on;
	return;
}

/*********************************************************************
**    I_FUNCTION     :  S_match_attr(attr1,attr2)
**				Determines if two attribute bundles are the same.
**    PARAMETERS   
**       INPUT  : 
**          attr1     1st attribute bundle to compare.
**          attr2     2nd attribute bundle to compare.
**       OUTPUT : none
**    RETURNS      :
**          UU_TRUE if the attribute bundles match.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_match_attr(attr1,attr2)
struct UC_attributedatabag *attr1,*attr2;
{
/*
.....Determine if attribute bundles match
*/
	if (attr1->color == attr2->color && attr1->layer == attr2->layer &&
		attr1->pen == attr2->pen && attr1->line_style == attr2->line_style &&
		attr1->line_weight == attr2->line_weight &&
		attr1->displayable == attr2->displayable &&
		attr1->selectable == attr2->selectable &&
		attr1->label_on == attr2->label_on &&
		attr1->rel_num == attr2->rel_num) return(UU_TRUE);
	return(UU_FALSE);
}

/*********************************************************************
**    I_FUNCTION     :  S_match_color(rgb)
**				Matches RGB values with an existing color in the color
**          tables.
**    PARAMETERS   
**       INPUT  : 
**          rgb      RGB values of color to match.
**       OUTPUT : none
**    RETURNS      :
**          A positive value returns the exact match index into the
**          color table.  A negative value references a closest match
**          index+1 into the color table.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_match_color(rgb)
int rgb[3];
{
	int i,dx,dy,dz,ddx,ddy,ddz,dsav,d,icol;
/*
.....Find the closest match
*/
	icol = UIG_STDCOLOR-1;
	ddx = ddy = ddz = dsav = 100000;
	for (i=0;i<Sncolor;i++)
	{
		dx = rgb[0]-uw_color_table[i][0];
		dy = rgb[1]-uw_color_table[i][1];
		dz = rgb[2]-uw_color_table[i][2];
		d = dx*dx + dy*dy + dz*dz;
		if (d < dsav)
		{
			ddx = abs(dx); ddy = abs(dy); ddz = abs(dz);
			dsav = d;
			icol = i;
		}
	}
	if (ddx > 1 || ddy > 1 || ddz > 1) icol = -(icol+1);
/*
.....End of routine
*/
	return(icol);
}

/*********************************************************************
**    I_FUNCTION     :  S_match_color_label(label)
**				Matches a color name an existing color in the color
**          tables.
**    PARAMETERS   
**       INPUT  : 
**          label     Name of color to match.
**       OUTPUT : none
**    RETURNS      :
**          A positive value returns the exact match index into the
**          color table.  -1 is returned if no match is found.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_match_color_label(label)
char *label;
{
	int i;
/*
.....Find the color in the color table
*/
	for (i=0;i<Sncolor;i++)
	{
		if (strcmp(uw_color_name[i],label) == 0 ||
			(i == 5 && strcmp(label,"MAGENTA") == 0)) return(i);
	}
	return(-1);
}

/*********************************************************************
**    I_FUNCTION     :  S_reset_attr(attr)
**				Initializes the basic attributes of an entity.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :
**          attr     Initialized attribute bundle.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_reset_attr(attr)
struct UC_attributedatabag *attr;
{
/*
.....Initialize the attribute record
*/
	attr->rel_num = UM_ATTR_REL;
	attr->color = -1;
	attr->layer = Slayer;
	attr->pen = 1;
	attr->line_style = 0;
	attr->line_weight = 1;
	attr->displayable = UM_DISPLAYABLE;
	attr->selectable = 1;
	attr->use_count = 0;
	attr->label_on = 0;
	return;
}

/*********************************************************************
**    I_FUNCTION     :  S_store_layer_rec(lptr,recno)
**				Stores a record number in the layer definition stack.
**    PARAMETERS   
**       INPUT  :
**          lptr     Pointer to entry in layer definition stack to
**                   store record number.
**          recno    Record number to store.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_store_layer_rec(lptr,recno)
Slayer_struc *lptr;
int recno;
{
	int i,*rec;
/*
.....Store record number in layer stack
*/
	if (lptr->nrec == lptr->maxrec)
	{
		rec = (int *)uu_malloc((lptr->maxrec+10)*sizeof(int));
		for (i=0;i<lptr->maxrec;i++) rec[i] = lptr->recno[i];
		uu_free(lptr->recno); lptr->recno = rec;
		lptr->maxrec += 10;
	}
/*
.....Store record number in layer stack
*/
	lptr->recno[lptr->nrec++] = recno;
}

/*********************************************************************
**   I_FUNCTION: S_load_layers(laylst,nlay)
**      Load all layer definitions into a list.  The list will be
**      created by this routine.  This routine is borrowed from
**      'umu_load_layers'.
**   PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          laylst = List array of defined layers.
**          nlay   = Number of layers in list.
**   RETURNS: UU_SUCCESS if all goes well, UU_FAILURE otherwise.
**   SIDE EFFECTS:
**          The calling routine needs to free the list.
**   WARNINGS: none
*********************************************************************/
static int S_load_layers(laylst,nlay)
UU_LIST *laylst;
int *nlay;
{
	int status,entnum,lstnum,i;
	struct UM_layer_rec layer,*sptr;
/*
.....Create a new layer list
*/
	uu_list_init(laylst,sizeof(struct UM_layer_rec),100,100);
	if (laylst->data == UU_NULL) return(UU_FAILURE);
	sptr = (struct UM_layer_rec *)UU_LIST_ARRAY(laylst);
	*nlay = 0;
	entnum = 0;
	lstnum = 0;
/*
.....Get all of the layers and
.....place them into the list
*/
	layer.rel_num = UM_LAYER_REL;
	status = 0;
	while (status == 0)
	{
		entnum++;
		status = ur_get_next_tuple_index(layer.rel_num,&entnum);
		if (status == 0)
		{
			ur_retrieve_tuple(layer.rel_num,entnum,&layer);
			if (layer.num < lstnum)
			{
				for (i=0;i<*nlay;i++)
				{
					if (layer.num < sptr[i].num)
					{
						uu_list_insert(laylst,i,&layer);
						sptr = (struct UM_layer_rec *)UU_LIST_ARRAY(laylst);
						break;
					}
				}
			}
			else
			{
				uu_list_push(laylst,&layer);
				sptr = (struct UM_layer_rec *)UU_LIST_ARRAY(laylst);
			}
			if (layer.num > lstnum) lstnum = layer.num;
			*nlay = *nlay + 1;
		}
	}
/*
.....End of routine
*/
	return(UU_SUCCESS);
}
