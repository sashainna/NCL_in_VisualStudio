/**************************************************************
**         NAME: nshape.c
**         Contains:
**             ncl_getshp_f2d3d
**             shapcv
**             nclu_shape_mod_display
**             nclu_post_word_add
**             nclu_sh_line
**             nclu_sh_circle
**             num2char
**             nclu_disp_shape
**             ncl_modattr_shape
**             ncl_post_add
**             ncl_post_delete
**             ncl_evolve_shape
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nshape.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:03
*********************************************************************/

#include <math.h>
#include "class.h"
#include "dasnog.h"
#include "dselmask.h"
#include "gobas.h"
#include "mfort.h"
#include "mdrel.h"
#include "mdpick.h"
#include "mxxx.h"
#include "mdebug.h"
#include "mdraw.h"
#include "mdcoord.h"
#include "mattr.h"
#include "mcrv.h"
#include "msrf.h"
#include "mdclass.h"
#include "modef.h"
#include "mdeval.h"
#include "nccs.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclvx.h"
#include "usysdef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "uhep.h"
#include "umath.h"
#include "ulist.h"
#include "ustdio.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "driver.h"
#include "mgeom.h"

UU_LOGICAL  ud_gnxt();

#define MAX2 6
extern int shape_display_mode;
static int cfl =-1;
static int numvpaths=5;
static int numupaths=5;
static int ptspervcrv=0;
static int ptsperucrv=0;
static int add_delete=0;

/*********************************************************************
**    FUNCTION     ncl_getshp_f2d3d
**       Retrieve Shape's f2d3d flag
**    PARAMETERS
**       INPUT  :
**          nclkey: key of Shape
**          
**       OUTPUT :
**          f2d3d: Shape's flag display mode flag (2D/3D)
**    RETURNS      None
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_getshp_f2d3d(nclkey, f2d3d)
UU_KEY_ID *nclkey;
UM_int2 *f2d3d;
{
	struct NCL_shape_rec shape;
	
/*
.....Get the information about the shape.
*/
	shape.key=*nclkey;
	ncl_retrieve_data(&shape,sizeof(struct NCL_shape_rec));
	*f2d3d = shape.f2d3d;
}

/*********************************************************************
**    FUNCTION :    shapcv (crvkey,shpkey)
**       Creates a 3-D display representation of a shape.
**    PARAMETERS
**       INPUT  :
**          crvkey 		Key of composite curve defining shape.
**          shpkey 		Key of actual shape.
**          
**       OUTPUT :
**          none
**    RETURNS      None
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
shapcv (crvkey,shpkey)
UU_KEY_ID *crvkey;
UU_KEY_ID *shpkey; 
{
	UM_tessellation tess;
	UD_NDCLOCREC axis_pt;        
	UM_vector axis_vec;
	UM_angle start_ang, end_ang;
	struct NCL_fixed_databag crv;
	struct UM_rbsplsrf_rec srf;
	struct UM_surfattr_rec shattr;
	struct NCL_shape_rec shape;  
	int status;
	UM_transf tfmat;

	uu_denter(UU_MTRC,(us,"shapcv"));

/*
.....Get the information about the shape.
*/
	shape.key=*shpkey;
	ur_retrieve_data_relnum(shape.key,&shape.rel_num);
	ncl_retrieve_data(&shape,sizeof(struct NCL_shape_rec));
/*
.....Set crv.key to the composite curve key and get all necessary info.
.....Set tfmat to the identity matrix.
*/
	crv.key=*crvkey;
	ncl_retrieve_data_fixed(&crv);
	um_identtf(tfmat);
/*
.....The axis vector will be set at <1,0,0>, the axis point coordinate
.....will be (0,0,0), starting angle is 0, and ending angle is 2PI.
*/

	axis_vec[0] = 1;   
	axis_vec[1] = 0;
	axis_vec[2] = 0;
	axis_pt.cord[0] = 0;
	axis_pt.cord[1] = 0;
	axis_pt.cord[2] = 0;
	start_ang = 0.0;
	end_ang = UM_TWOPI;
	
	srf.key = 0;
/*
.....ncl_revsrf will great the points for the surface structure.
*/
	status = ncl_revsrf(&axis_pt, axis_vec, start_ang, end_ang, &crv, &srf);
	if (status == UU_SUCCESS)
	{
/*
.....For now setting the color of the srf to the modified color 
.....Make the displst for shape. 
*/
		um_get_disp_attr(shape.key,&shattr);
/*
.....Make sure the correct number of uv curves and
.....the correct number of ptspercrv. If the static
.....variables are not equal to the global variables
.....then the the shape is being modified and we want
.....to use the modified values JLS 9/30/99
*/
		if (numupaths != UM_srfattr.numupaths)
			shattr.numupaths = numupaths;
		if (numvpaths != UM_srfattr.numvpaths)
			shattr.numvpaths = numvpaths;
		if (ptsperucrv!= UM_srfattr.ptsperucrv)
			shattr.ptsperucrv = ptsperucrv;
		if (ptspervcrv!= UM_srfattr.ptspervcrv)
			shattr.ptspervcrv = ptspervcrv;
/*
.....If cfl is equal to a -1, then if shape.f2d3d is equal to
.....2, user wants shading, if equal to 1, user wants a wireframe
.....shape, and if f2d3d is equal to 0, then a two demensional
.....shape is desired.
*/
		if (cfl == -1)
		{
			if(shape.f2d3d==2)
				shattr.shaded =1;
			else
				shattr.shaded =0;
		}
/*
.....Set the shape color back to its default color.
*/
		ur_update_attr(&shattr);
/*
.....srf shade attr should same as shape
.....Yurong 5/5/99
*/
		ncl_setent_shaded(&srf, shattr.shaded);
		if (shattr.shaded)
		{
			ncl_setent_lucency(&srf, shattr.lucency);
			um_init_tess(&tess);
			status = ncl_get_surflist (TESSELLATION_LIST,&shape,&tess);
			if (status==0)
			{
				ncl_store_surflist (TESSELLATION_LIST, &srf, &tess);			
			}
			um_free_tess (&tess);
		}
		nclu_disp_shape(&srf,tfmat,&shattr);
/*
.....Need to retrieve the information about srf, this will get the displst.
.....Didn't want the surface to be an actual surface, or to have it
.....saved in the unibase, so erase it.
*/
		ncl_retrieve_data(&srf,sizeof (struct UM_rbsplsrf_rec));
		shape.displst=srf.displst;
		shape.no_displst=(4*srf.no_displst);
		ur_update_data_varlist(shape.key,2,shape.displst,1,shape.no_displst);
/*
.....added for tesslst
.....Yurong 5/5/99
*/
		if (shattr.shaded)
		{
			um_init_tess(&tess);
			ncl_get_surflist (TESSELLATION_LIST,&srf,&tess);
			ncl_store_surflist (TESSELLATION_LIST, &shape, &tess);
			um_free_tess (&tess);
		}
		ur_delete_all(srf.key);
		ncl_randel (srf.key,srf.rel_num);
	}
	else
		uu_outputerr("Error creating B-spline Surface");
/*
.....Reset NCL_skey for later use by ncl_displst.
.....Then delete all the information regarding
.....the curve, it is no longer needed.
*/
	ncl_displst_init(2,shape.key);
	ur_delete_all(crv.key);
	ncl_randel (crv.key,crv.rel_num);

	uu_dexit;
	return(0);
}

/*********************************************************************
**    FUNCTION :    nclu_shape_mod_display()
**       Modifies the display parameters of a shape (2D/3D, etc.).
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      None
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_shape_mod_display()
{
	int status;
	UD_FSTAT ncl_modattr_sh();
	static int *ans[] = {&cfl,
								&numvpaths,
								&numupaths,
								&ptspervcrv,
								&ptsperucrv, };
	static UD_METHOD methods[] = { ncl_modattr_sh, NULL,
											 NULL, NULL, NULL};
	static char called[] = {6, 6, 6, 6, 6 };
	static char traverse[] = {1, 0, 0, 0, 0 };
	struct NCL_shape_rec shape;
	struct UM_surfattr_rec shattr;
	int nsels;
	UM_PLOCREC pick;
	UU_LOGICAL cont;
	int jmpflag;

   uu_denter(UU_MTRC,(us,"nclu_shape_display_3D"));
	UD_MARK(jmpflag, UU_FALSE);
	if (jmpflag == 0)
	{
/*
.....Set the current values to the current
.....default values.
*/ 
		numvpaths = UM_srfattr.numvpaths;
		numupaths = UM_srfattr.numupaths;
		ptspervcrv = UM_srfattr.ptspervcrv;
		ptsperucrv = UM_srfattr.ptsperucrv;
		cfl = shape_display_mode;
/*
.....If the display mode has been set to either
.....wireframe or shaded, make sure the fields
.....for numupaths, numvpaths, ptsperucrv, and
.....ptspervcrv are undimmed. JLS 9/30/99
*/
		if (cfl>=1)
		{
			traverse[0] = 1;
			traverse[1] = 1;
			traverse[2] = 1;
			traverse[3] = 1;
			traverse[4] = 1;
		}
/*
.....Find out if display in 2-d, wireframe or shaded.
.....0 indicates 2-D and 1 indicates wireframe and 2 indicates 
.....shaded.
.....Moved the call to ud_form up here so that the form is
.....displayed once but you can choose multiple shapes to
.....modify.  Also changed the form to have a call back
.....function for the purpose of dimming and undimming
.....the 4 fields that are just for wireframe or shaded
.....shapes. JLS 9/30/99
*/
		status = ud_form1("shapattr.frm",ans,ans,methods,called,NULL,traverse);
		if (status==-1)
			goto done;
		while (UU_TRUE)
   		{
      	cont = UU_TRUE;
/*
.....Get the shape that will be displayed modified.
*/
			ud_lgeo(UU_TRUE, UD_ncl_sh);
      	um_dl_pldas(UD_DASPCKLOC, UM_MODEL, 26, &pick, 1, &nsels, 1);
/*
.....If no selection, quit.
*/
			if (nsels <= 0) goto done;

/*
.....Get the key from the pick record.
*/
			shape.key = um_get_pickkey(&pick.pent, 1);
/*
.....Don't think this call is necessary.  JLS
			ncl_getshp_f2d3d(&(shape.key), &val);
.....Moved this up above.
			ud_form("shapattr.frm",ans,ans);
*/
/*
.....Retrieve the current information.
.....And display the shape.
*/
			ncl_retrieve_data(&shape,sizeof(struct NCL_shape_rec));
			shattr.key = shape.key;
			ur_retrieve_attr(&shattr);
/*
.....we need update f2d3d flag
.....Yurong
.....Since the addition of the new option for the display
.....mode button, both cfl =1 and cfl =2 mean that the
.....shape will be displayed in 3 dimensions so for either
.....of those situations set shape.f2d3d to 1.  JLS 9/30/99
         shape.f2d3d = cfl;
*/
			if (cfl >1)
			{
				shape.f2d3d = 2;
				shattr.shaded = 1;
			}
			else if (cfl ==1)
			{
				shape.f2d3d = 1;
				shattr.shaded =0;
			}
			else
			{
				shape.f2d3d = 0;
				shattr.shaded =0;
			}
			ur_update_data_fixed(&shape);
			ur_update_attr(&shattr);
      	uc_display(&shape);
   	}
	}
done:;
	jmpflag = 0;
	UD_UNMARK(jmpflag);
	traverse[0] = 1;
	traverse[1] = 0;
	traverse[2] = 0;
	traverse[3] = 0;
	traverse[4] = 0;
	return(0);
}

/*********************************************************************
**    FUNCTION :    nclu_post_word_add()
**       This routine prompts user to pick a shape, it will then display
**       the post-processor commands associated with that portion of the
**       shape and the user may then either add or delete postprocessor
**       commands in the shape.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      None
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_post_word_add()
{
#define PPLEN 80
	int status, len;
	static int *ans[4];
	int i;
	UD_LIST postprocessor_word;
	static char post_word[PPLEN] = " ";
	static char shape_label[PPLEN]="";
	struct NCL_shape_rec shape;
	int nsels,nents,nc;
	UM_PLOCREC pick;
	UM_PICKENT data;
	char *ppwd[20];
	char post_command[PPLEN];
	UM_f77_str pmand;
   UM_f77_str mand[20];
	UM_int2 error_flag;
	UM_int4 key;
   char fstr[20][70];
	int icnt,j,comma_flag;
	int space_counter;
	int modal2,modal;
	char idlabel[10];
	UD_FSTAT ncl_post_add();
	UD_FSTAT ncl_post_delete();
	static UD_METHOD methods[]= {NULL, NULL,
											ncl_post_add, ncl_post_delete};
	static char called[] = {6,6,6,6,6};
	static char traverse[] = {1,1,1,1,1};
	static char display[] = {1,1,1,1,1};
	

	uu_denter(UU_MTRC,(us,"nclu_post_word_add()"));

/*
.....Initializing error_flag andthe f77 strings.
*/
	error_flag=0;
	for(i=0;i<20;i++) UM_init_f77_str(mand[i],fstr[i],70);
	UM_init_f77_str(pmand,post_command,PPLEN);

/*
.....Get the shape that will have the postprocessor words added to it.
*/
	icnt=0;
/*
.....Limit picks to shapes only.  JLS/9/30/99
*/
again:;
	ud_lgeo(UU_TRUE, UD_ncl_sh);
	um_dl_pldas(UD_DASPCKLOC, UA_NCL, 492, &pick, 1, &nsels,1);
/*
.....If no selection, quit.
*/
	if (nsels<=0) goto done;
/*
.....Get the key from pick, and get data related to the shape.
.....Also get the pickid so that we know where to put the post-
.....processor command.
*/
	um_d_pickresolve(&pick,2,&data);
	shape.key=um_get_pickkey(&pick.pent,1);
	key= um_get_pickkey(&data,2);
	ur_retrieve_data_relnum(shape.key, &shape.rel_num);
/*
.....Make sure that the last entity is not picked
.....Since LATHE/FINISH does not allow post commands
.....after this entity
*/
	nshent(&shape.key,&nents);
	if (key == nents)
	{
		uu_uerror0(UA_NCL,18);
		goto again;
	}
/*
.....Need to find out if there are any post processor commands
.....already in the definition of the shape, and if so, those
.....commands need to be displayed in the list portion of the
.....ppword.frm.
*/
repeat:;
	fndpwd(&shape.key,&key,UM_addr_of_f77_str(mand[0]));
/*
.....Get the label of the shape so that it can be displayed at the 
.....top of the form, then place shape into shape_label.
*/
	ur_retrieve_data_fixed(&shape);

	strncpy(shape_label,shape.label, NCL_MAX_LABEL);
	nc = strlen(shape_label);
	ul_strip_blanks(shape_label,&nc);
	strcat(shape_label,"/");
/*
.....Attach the pick id number to the label
.....that will be displayed in the form.
*/
	sprintf(idlabel,"id-%d",key);
	strcat(shape_label,idlabel);
/*
.....First add commas to the command statement.
*/
	for(i=0;i<20;i++)
	{
		for(j=20;j<62;j=j+7)
			if(fstr[i][j+1]!=' '&& fstr[i][j+1]!='\0') fstr[i][j]=',';
	}
/*
.....Now eliminate the extra spaces that are in the command statement.
*/
	for(i=0;i<20;i++)
	{
		space_counter=0;
		for(j=0;j<70;j++)
		{
         if(fstr[i][j]==' ') 
			{
				if(j==70-1) 
				{
					fstr[i][j]='\0';
					fstr[i][j-space_counter]=fstr[i][j];
					if (fstr[i][j-(space_counter+1)]==',')
							fstr[i][j-(space_counter+1)]='\0';
				}
            space_counter=space_counter+1;
			} 
         else
			{
            fstr[i][j-space_counter]=fstr[i][j];
				if (space_counter!=0) fstr[i][j]=' ';
			}
		}
	}
/*
.....Eliminate the extra comma at the end if it exists.
*/
	for (i=0;i<20;i++)
	{
		comma_flag=0;
		for (j=70-1;j>0&& comma_flag==0;j--)
		{
			if(fstr[i][j]==','&& comma_flag==0)
			{
				fstr[i][j]='\0';
				comma_flag=1;
			}
			else if (fstr[i][j]!=' '&&fstr[i][j]!='\0') comma_flag=1;
		}
	}
/*
.....Assign to ppwd the postprocessor commands if they exist.
*/
	for(i=0;i<20;i++) ppwd[i]=fstr[i];
/*
.....Assign the ppwd array to postprocessor_word.item 
*/
	postprocessor_word.item=(char **)ppwd;
	postprocessor_word.num_item=20;
	len = strlen(post_word);
	postprocessor_word.answer = (char *) uu_malloc(len * sizeof(char));
	strcpy(postprocessor_word.answer, post_word);
/*
.....ans holds the current choice that is displayed in the postprocessor
.....form, ans[0] has the label of the choosen shape and ans[1] holds
.....the postprocessor command.
*/ 
	modal=0;
	modal2=0;

	ans[0] = (int *)&shape_label;
	ans[1] = (int *)&postprocessor_word;
	ans[2] = (int *)&modal;
	ans[3] = (int *)&modal2;
/*
.....Display the Postprocessor Word form to get the postprocessor command to
.....be added to the shape. Then copy it into post_command.
*/
	for(i=0;i<PPLEN;i++) post_command[i]='\0';
	
/*
.....add_delete is a static variable that is set in the call back
.....functions.  IF add_delete is still equal to 0 after returning
.....from the form, then the user did nothing, if add_delete is
.....is equal to 1, the user wants to add a command, and if it is
.....equal to 2, the user wishes to delete a command.  This way
.....the user may continuously add or delete commands for that
.....particular segment of the shape without having to exit the
.....form and start from the beginning.  JLS 9/30/99
*/
	add_delete =0;
	status = ud_form1("ppword.frm",ans, ans, methods, called, display,traverse);
	if (status==-1)
		return(0);
	strcpy(post_command, postprocessor_word.answer);
	
	if(add_delete==1)
	{
/*
.....Now call ppword so that the new command may be added to the 
.....shape array and saved for future use.
*/
		ppword(&shape.key,UM_addr_of_f77_str(pmand),&key);
   	if(error_flag==1)
			uu_uerror0(UM_MODEL, 322);
		goto repeat;
	}
	else if (add_delete ==2)
	{
/*
.....Call pwddel to delete the command from the shape array
*/
		pwddel(&shape.key,&key,UM_addr_of_f77_str(pmand),&error_flag);
   	if(error_flag==1)
			uu_uerror0(UM_MODEL, 322);
		goto repeat;
	}
	else
		goto again;

done:;
	return(0);
}

/*********************************************************************
**    FUNCTION :    nclu_sh_line(start,end,line_key,pick,shkey)
**       Creates a line record which will be used in generating the
**       3-D display of a shape.
**    PARAMETERS
**       INPUT  :
**          start			Starting point of line.
**          end			Ending point of line.
**       OUTPUT :
**          line_key		Key of line element created.
**    RETURNS      None
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_sh_line(start,end,line_key,pick,shkey)

UM_real8 *start;
UM_real8 *end;
UM_int4 *line_key;
UM_int2 *pick;
UM_int4 *shkey;

{
	struct UM_line_rec line;
	struct UM_attrdata_rec attr;
	struct UM_surfattr_rec shattr;
	UM_transf tfmat;
	int i, ival;
	UM_int2 ifl,val;
	int newkey;

	newkey=*pick;
/*
.....Set tfmat to the identity matrix.
*/
	um_identtf(tfmat);
	ur_setup_data(NCL_LINE_REL,&line,sizeof(line));
	line.key=0;
	ncl_create_entity(&line,NCL_LINE_REL);
	line.rel_num=UM_LINE_REL;
/*
.....Set the start and the end points of the line.
*/
   for (i=0;i<3; i++) 
	{
		line.spt[i]=start[i];
		line.ept[i]=end[i];
	}
	ur_update_data(&line);
/*
.....Set the pick id and call uc_draw so that the id will
.....be set appropriately.
*/
	ifl = 344;
	getifl(&ifl,&val);
	ival = val;
	if (!ival)
	{
/*
.....Make sure that attributes are the same as the shape's 
.....attributes.  JLS 5/17/99
*/
		shattr.key = *shkey;
		ur_retrieve_attr(&shattr);
		um_get_disp_attr(line.key,&attr);
		attr.color=shattr.color; 
		attr.layer = shattr.layer;
		attr.pen = shattr.pen;
		attr.line_style = shattr.line_style;
		attr.line_weight = shattr.line_weight;
		attr.line_width = shattr.line_width;
   	ur_update_attr(&attr);
		gspickid(newkey);
		uc_draw(&line,tfmat,&attr);
		gspickid(newkey);
	}
	*line_key=line.key;
	return(0);
}

/*********************************************************************
**    FUNCTION :    nclu_sh_circle(radius,dang,center,start,end,circle_key,
**                     pick,shkey)
**       Creates a circle record which will be used in generating the
**       3-D display of a shape.
**    PARAMETERS
**       INPUT  :
**          radius		Radius of circle.
**          dang			Delta angle of circle.
**          center		Center point of circle.
**          start			Ignored.
**          end			Starting point of circle.
**          pick			Pick id of circle.
**          shkey			Shape key.
**       OUTPUT :
**          circle_key	Key of circle entity created.
**    RETURNS      None
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_sh_circle(radius,dang,center,start,end,circle_key,pick,shkey)

UU_REAL *radius;
UU_REAL *dang;
UU_REAL *center;
UU_REAL *start;
UU_REAL *end;
UM_int4 *circle_key;
UM_int2 *pick;
UM_int4 *shkey;

{
	struct UM_circle_rec circle;
	struct UM_attrdata_rec attr;
	struct UM_surfattr_rec shattr;
	UM_transf tfmat;
	int i, ival;
	UM_int2 ifl,val;
	int newkey;

	newkey=*pick;
/*
.....Set tfmat to the identity matrix.
*/
	um_identtf(tfmat);
	ur_setup_data(NCL_CIRCLE_REL, &circle,sizeof(circle));
	circle.key=0;
	ncl_create_entity(&circle,NCL_CIRCLE_REL);
/*
.....Set the radius and the angle.
*/
	circle.radius=*radius;
	circle.dang=*dang;
/*
.....Set the normal vector. If the value of dang is negative, then the 
.....value of nvec[2] is 1 and change dang to be positive.
*/
	circle.nvec[0]=0;
	circle.nvec[1]=0;
	if (circle.dang<=0)
	{
		circle.nvec[2]=1;
		circle.dang=-circle.dang;
	}
	else
		circle.nvec[2]=-1;
/*
.....Set the circle center and the starting vector. The starting 
.....vector is found by subtracting the center from the end point 
.....and then multiplying it by the inverse of the radius.
*/

	for(i=0;i<3;i++)
	{
		circle.center[i]=center[i];
		circle.svec[i]=(1.0/circle.radius)*(end[i]-center[i]);
	}


	ur_update_data(&circle);
/*
.....Set the pick id number and call uc_draw to have 
.....the pick id be effective.
*/
	ifl = 344;
	getifl(&ifl,&val);
	ival = val;
	if (!ival)
	{
/*
.....Make sure the attributes are the same as the shape's
.....attributes.  JLS 5/17/99
*/
		shattr.key = *shkey;
		ur_retrieve_attr(&shattr);
		um_get_disp_attr(circle.key,&attr);
		attr.color=shattr.color; 
		attr.layer = shattr.layer;
		attr.pen = shattr.pen;
		attr.line_style = shattr.line_style;
		attr.line_weight = shattr.line_weight;
		attr.line_width = shattr.line_width;
   	ur_update_attr(&attr);
		gspickid(newkey);
		uc_draw(&circle,tfmat,&attr);
		gspickid(newkey);
	}
	*circle_key=circle.key;
	return(0);
}

/*********************************************************************
**    FUNCTION :    num2char(num,newchar)
**       Convert a real number to a character string.  Fortran callable.
**    PARAMETERS
**       INPUT  :
**          num		Number.
**       OUTPUT :
**          newchar	Character string.
**    RETURNS      None
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
num2char(num,newchar)
UU_REAL *num;
UM_f77_str_ptr newchar;
{
	double number;
   char *newnumber;

   uu_denter(UU_MTRC,(us, "num2char"));

	newnumber = UM_cstr_of_f77_str(newchar);

	number = *num;
	ncl_sprintf(newnumber, &number,1);

	return(0);
}

/*********************************************************************
**    FUNCTION :    nclu_disp_shape(eptr, tfmat, attrptr)
**       Displays a shape using a 3-D representation.
**    PARAMETERS
**       INPUT  :
**          eptr		Shape entity to display.
**				tfmat		Transformation matrix.
**				attrptr	Attribute record.
**       OUTPUT :
**          none
**    RETURNS      None
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_disp_shape(eptr, tfmat, attrptr)
struct UM_rbsplsrf_rec *eptr;
UM_transf tfmat;
struct UM_surfattr_rec *attrptr;
{
   UU_LIST cvpts;
   UU_REAL u, v, du, dv, tol, *ptcv, ur[2], *lstp;
   int status, i, j, k, n, nu, nv, nup, nvp, ifl,lstix, ndisp;
   UM_int2 mdsys, mm, idx;
	int shaded ;

   uu_denter(UU_MTRC,(us,"nclu_disp_shape(key=%x, tfmat=%x, attrptr=%x)",
      eptr->key, tfmat, attrptr));

   status = UU_SUCCESS;
   um_set_disp_attr(attrptr);
	idx = 175;
	getsc (&idx,&tol);
/*
..... If surface has a display list, use it & return, otherwise initialize it.
*/
   ifl = 1;
   if (eptr->rel_num == UM_RBSPLSRF_REL)
   {
     lstix = 6;
     ndisp = eptr->no_displst;
     lstp  = eptr->displst;
/*
.....get shaded attributes
.....Yurong
*/
      shaded = attrptr->shaded;
   }
   else
   {
     ifl = 0;
     lstix = 0;
     ndisp = 0;
   }

	if (shaded==1)
	{
		status = ncl_display_shaded_surf(eptr->key);
		if (status == UU_SUCCESS)
			return status;
	}
	if (!ncl_get_wireframe())
		return 0;

	if (ndisp > 0 && ncl_displst_OK(eptr->key,lstp,tol,0))
   {
      status = ncl_displst_display (lstp,tfmat);
/*
.....added for hidden line remove
.....Yurong 
*/
		if (ncl_is_hidline())
			ncl_display_hid_surf(eptr->key);
      return (status);
   }

   if (lstix) ncl_displst_init (lstix, eptr->key);

   gtmsmm (&mdsys, &mm);
   ur[0] = 0.;
   ur[1] = 1.;
/*
.....Set the number of u and v curves and the number of
.....points per curve.
*/
   nu  = attrptr->numupaths;
   nv  = attrptr->numvpaths;
   nup = attrptr->ptsperucrv;
   nvp = attrptr->ptspervcrv; 

   k   = (nup > nvp)? nup: nvp;
   k   = (k < 50)? 50: k;
   uu_list_init (&cvpts, sizeof(UM_coord), k, k);

   du = 1./(nu-1);
   dv = 1./(nv-1);
/*
...vp 14-jul-94 changed both loops for points so now u-line is created
...by u-point increament and v-line by v-point increament.
*/
   for (i=0, u=0.; i<nu; i++, u=i*du)
   {
       cvpts.cur_cnt = 0;
       if (nup > 0 || ifl == 0)

           n = ncl_evolve1_crv_on_srf (eptr,tfmat,u,2,nup,&cvpts);
       else
           n = ncl_evolve_crv_on_srf (eptr,tfmat,u,ur,2,tol,&cvpts,UU_NULL,UU_NULL);
       ptcv = (UU_REAL *) UU_LIST_ARRAY (&cvpts);
       for (j=0, k=0; j<n; j++, k+=3)
           glina3 (&ptcv[k], &ptcv[k+1], &ptcv[k+2]);

       gdraw();
   }

   for (i=0, v=0.; i<nv; i++, v=i*dv)
   {
       cvpts.cur_cnt = 0;
       if (nvp > 0 || ifl == 0)
           n = ncl_evolve1_crv_on_srf (eptr,tfmat,v,1,nvp,&cvpts);
       else
           n = ncl_evolve_crv_on_srf (eptr,tfmat,v,ur,1,tol,&cvpts,UU_NULL,UU_NULL);

       ptcv = (UU_REAL *) UU_LIST_ARRAY (&cvpts);
       for (j=0, k=0; j<n; j++, k+=3)
          glina3 (&ptcv[k], &ptcv[k+1], &ptcv[k+2]);

       gdraw();
   }
/*
..... Complete display list.
*/
   status = ncl_displst_finish(tol,0);


   stmsmm (&mdsys, &mm);
   uu_list_free (&cvpts);
   ncl_conv_sfseg_reset();

   uu_dexit;
   return (status);
  }

/*********************************************************************
**    FUNCTION :    ncl_modattr_sh(fieldno, val, stat)
**       Callback routine for the Display Mode: field on the Shape
**       Attributes form.  Either dims or undims the numupaths,
**       numvpaths, ptsperucrv, and ptspervcrv fields.
**    PARAMETERS
**       INPUT  :
**          fieldno	Field number of Display mode.
**				val		Toggle value for Display mode.
**				stat		Not used.
**       OUTPUT :
**          none
**    RETURNS      None
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UD_FSTAT ncl_modattr_sh(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;

{
/*
.....User selected either wireframe or shaded shapes
.....so undim the fields.
*/
	if(cfl>0)
	{
		ud_set_traverse_mask(1,1);
		ud_set_traverse_mask(2,1);
		ud_set_traverse_mask(3,1);
		ud_set_traverse_mask(4,1);
	}
/*
.....User select 2-D shapes, so dim the fields.
*/
	else
	{
		ud_set_traverse_mask(1,0);
		ud_set_traverse_mask(2,0);
		ud_set_traverse_mask(3,0);
		ud_set_traverse_mask(4,0);
	}
	return(UD_FLDOK);
}
/*********************************************************************
**    FUNCTION :    ncl_post_add(fieldno,val,stat)
**       Callback function for the Add (Post word) Button.
**    PARAMETERS
**       INPUT  :
**          fieldno	Field number of Add button.
**				val		Ignored.
**				stat		Not used.
**       OUTPUT :
**          none
**    RETURNS      None
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UD_FSTAT ncl_post_add(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{

	add_delete =1;
	ud_mfclose_form(1);
	return(UD_FLDOK);
}

/*********************************************************************
**    FUNCTION :    ncl_post_delete(fieldno,val,stat)
**       Callback function for the Delete (Post word) Button.
**    PARAMETERS
**       INPUT  :
**          fieldno	Field number of Delete button.
**				val		Ignored.
**				stat		Not used.
**       OUTPUT :
**          none
**    RETURNS      None
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UD_FSTAT ncl_post_delete(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	add_delete =2;
	ud_mfclose_form(1);
	return(UD_FLDOK);
}

/*********************************************************************
**    FUNCTION :    ncl_evolve_shape(eptr,tfmat,tol,cvpts)
**       Evolves a shape into a set of points.
**    PARAMETERS
**       INPUT  :
**          shape    Pointer to shape record.
**				tfmat		Transformation matrix.
**				tol      Tolerance to use when evolving shape.
**       OUTPUT :
**          cvpts    List of 2-D points generated on the shape.  The list
**                   should be initialized prior to calling this routine.
**
**          npts     Number of points in list.
**    RETURNS :
**          Number of points generated on shape.  0 = Failure.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_evolve_shape(shape,tfmat,tol,cvpts)
struct NCL_shape_rec *shape;
UM_transf tfmat;
UU_REAL tol;
UU_LIST *cvpts;
{
	int nent,inc,kinc,npt,n;
	UU_LOGICAL found;
	UU_REAL *buf,cosa,deltang;
	UM_coord spt;
	UM_int2 *ibuf;
	UM_real4 *rbuf;
	struct UM_circle_rec c;
	Gwpoint3 gpt[100];
/*
.....Initialize routine
*/
	npt = 0;
/*
.....Load the shape data
*/
	nent = shape->no_shapwd;
	buf = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*nent);
	if (buf == UU_NULL) goto failed;
	ibuf = (UM_int2 *)buf;
	if (ur_retrieve_data_varlist(shape->key,1,buf,1,nent) != UU_SUCCESS)
		goto failed;
/*
.....Find the first line
.....This will be the starting point of the shape
*/
	found = UU_FALSE;
	for (inc=7;inc<nent;inc++)
	{
		kinc = inc * 4 + 3;
		if (ibuf[kinc] == 5)
		{
			found = UU_TRUE;
			gpt[0].x = buf[inc+1]; gpt[0].y = buf[inc+2]; gpt[0].z = 0.;
			uu_list_push(cvpts,&gpt[0]);
			npt++;
			spt[0] = buf[inc+1]; spt[1] = buf[inc+2]; spt[2] = 0.;
			break;
		}
	}
	if (!found) goto failed;
	inc = inc + 3;
/*
.....Loop through rest of entities
*/
	while (inc < nent)
	{
		kinc = inc * 4 + 3;
/*
.....Line
*/
		if (ibuf[kinc] == 5)
		{
			gpt[0].x = buf[inc+1]; gpt[0].y = buf[inc+2]; gpt[0].z = 0.;
			uu_list_push(cvpts,&gpt[0]);
			npt++;
			spt[0] = buf[inc+1]; spt[1] = buf[inc+2]; spt[2] = 0.;
			inc = inc + 3;
		}
/*
.....Circle
*/
		else if (ibuf[kinc] == 7)
		{
/*
........Define circle record
*/
			c.center[0] = buf[inc+3]; c.center[1] = buf[inc+4]; c.center[2] = 0.;
			c.nvec[0] = c.nvec[1] = 0.; c.nvec[2] = 1.;
			c.svec[0] = spt[0] - c.center[0]; c.svec[1] = spt[1] - c.center[1];
			c.svec[2] = 0.;
			um_unitvc(c.svec,c.svec);
			c.radius = buf[inc+5];
			rbuf = (UM_real4 *)&buf[inc+6];
			c.dang = rbuf[1] - rbuf[0];
/*
........Get number of points to generate
*/
			cosa = (c.radius-tol) / (c.radius);
			if (cosa > 1.) cosa = 1.;
			deltang = fabs(acos(cosa));
			if (deltang <= 0.) n = 9;
			else n = (int)(c.dang / deltang);
			if ( n < 9) n = 9;
			else if (n > 100) n = 100;
/*
........Generate points along the arc
*/
			ncl_cutter_circle(&c,gpt,n);
			uu_list_push_multiple(cvpts,n-1,&gpt[1]);
			npt = npt + n - 1;
			spt[0] = gpt[n-1].x; spt[1] = gpt[n-1].y;
			inc = inc + 7;
		}
/*
.....Post-processor word
*/
		else
		{
			inc = inc + ibuf[kinc-1] + 1;
		}
	}
	goto done;
/*
.....Could not evaluate shape
*/
failed:;
	npt = 0;
/*
.....End of routine
*/
done:;
	if (buf != UU_NULL) uu_free(buf);
	return(npt);
}
