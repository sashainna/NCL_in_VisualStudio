/*********************************************************************
**    NAME         :  nuln.c 
**       CONTAINS: User interface routines for line creation.
**			nclu_ln_pt_pt()
**			nclu_ln_xyz_xyz()
**			nclu_ln_tanto_ci_ci()
**			nclu_ln_parlel_ln()
**			nclu_ln_plane()
**			nclu_ln_pt_parlel_ln()
**			nclu_ln_pt_perpto_ln()
**			nclu_ln_pt_atangl_ln()
**			nclu_ln_pt_tanto_cv()
**			nclu_ln_intof_pl_pl()
**			nclu_ln_ci_atangl_ln()
**			nclu_ln_fwd()
**			nclu_ln_xy_axis()
**			nclu_ln_offset()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nuln.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:08
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "dmark.h"
#include "class.h"
#include "mdrel.h"
#include "mdpick.h"
#include "modef.h"
#include "mdcoord.h"
#include "mdclass.h"
#include "mcrv.h"
#include "mdeval.h"
#include "mattr.h"
#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"

/*********************************************************************
**    E_FUNCTION     : nclu_ln_pt_pt()
**       description : Create a line definition by selection
**                     of a combination of points and point-vectors.
**                     If user selects a point_vector first and 
**                     hits done, the line will be created from the
**                     head of the point-vector to the tail.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ln_pt_pt()

{
	NCL_cmdbuf cmdbuf;
	int status, reltyp;

	uu_denter(UU_MTRC,(us,"nclu_ln_pt_pt"));

	while (UU_TRUE)
	{
/*
.....Initialize the command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....Check to see if auto_label is on, if not, prompt user for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf,1);

/*
......Put LINE/ in the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_ln, NCL_nocomma);
/*
.....Prompt user for starting point and add it to the 
.....command.
*/
		status = ncl_add_label_rel(UD_DASPCKLOC, &cmdbuf, 30,
            UD_ncl_ptpv, &reltyp);
/*
.....If done was selected, exit.
*/
		if (status !=NCL_OKINPUT) goto done;
/*
......Prompt user for ending point. Add to command.
*/
	 	status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 31, UD_ncl_ptpv);
/*
.....If the first selection was a point-vector and the user hit
.....done for the second selection, make a line out of the point-vector.
*/
      if (reltyp == NCL_POINTVEC_REL && status == NCL_NOINPUT)
			status = NCL_OKINPUT;

/*
.....Process the command.
*/
		if ((status == NCL_OKINPUT))
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}

	}
done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_ln_xyz_xyz()
**    PARAMETERS   
**       INPUT  : 
**          Option: This routine creates a line definition
**                  by having the user pick the z value
**                  and then the xy location. It now is also
**                  the routine that handles line definition
**                  that was previously defined in routine
**                  nclu_ln_xy_xy. Additionally if option is
**                  equal to 2, make lines connected to one
**                  another.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ln_xyz_xyz(option)
int option;
{
	NCL_cmdbuf cmdbuf;
	int status;
	UD_RUBBER rubber;			/* rubber band control block */
/*
.....Adding new variables.  JLS 2/10/99
*/
	UM_coord xyzpt;
	char str[256];
	int get_z, j,nxy,i,len;
	UM_length zval;
	UD_NDCLOCREC tmp;
	char zstr[65];
	 

	uu_denter(UU_MTRC,(us,"nclu_ln_xyz_xyz"));

/*
.....Initialize rubberband.
*/
	RUBBER_LINE(&rubber);
/*
.....Option is equal to 0 then user got here thru the XY button
.....and the z value is 0.
*/
	if(option!=1)
	{
		get_z=0;
		nxy = 2;
		zval=0.0;
	}
	else
	{
		get_z=1;
		nxy = 3;
	}
	while (UU_TRUE)
	{
/*
.....Initialize the command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....Check to see if auto label in on and if not, prompt user for label.
*/	
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
/*
.....Put LINE/ into the command
*/	
		status = ncl_add_token(&cmdbuf, NCL_ln, NCL_nocomma);
/*
.....If user selected the XYZ button, ask for z coordinate
.....first and then pick location of x and y.
*/
		if (get_z==1)
		{
/*
.....we need default zval, if not junk value to display
.....in prompt line
.....Yurong 2/22/00
*/
			zval=0.0;
			status = ncl_get_z(&zval, zstr);
			get_z=0;
/*
.....Done was selected, exit
*/
         if (status!= NCL_OKINPUT) goto done;
		}

/*
.....Get the x,y portion of the first point
*/
		status =ncl_get_coord(&tmp);
/*
.....If user hit done and it is the XYZ option go back and ask for
.....z again, if it is XY option, exit.
*/
		if(status!=NCL_OKINPUT)
		{
			if(option==1)
				get_z=1;
			else
				goto done;
		}
		else
		{
        	for(j=0; j<3; j++) xyzpt[j] = tmp.cord[j];
        	if (option == 1) xyzpt[2] = zval;
/*
.....remove preceeding spaces
*/
			i = 0; len = strlen(tmp.label);
			while (tmp.label[i]==' ') i++;
			if (tmp.label[i]!='\0')
			{
				strcpy(str, &(tmp.label[i]));
				if (option == 1)
				{
/*
.....add or replace the z value
*/
					ncl_replace_cordstr(str, zstr, 3);
				}
			}
			else
			{
				ncl_cctostr(nxy, xyzpt, str);
			}
/*
.....Put the coordinates into the command.
*/
        	ncl_add_token(&cmdbuf,str,NCL_comma);
		}
/*
.....Turn on the rubberband.
.....If we are in CONNECTED, this is where to come back to start the
.....rubberbanding and to prompt user for end point coordinates.
*/
	
		while (status == NCL_OKINPUT)
		{
			RUBBER_ON(&rubber);
ask_again:;
/*
.....Get the ending points xy coordinates.
*/
			status=ncl_get_coord(&tmp);
			if(status!=NCL_OKINPUT && (option==1))
			{
/*
.....User selected done so ask for a new z value.
.....Turn off rubberbanding, otherwise it will start in the wrong place.
*/
				ud_endrub(&rubber);
				status=ncl_get_z(&zval, zstr);
				get_z=0;
/*
.....If done was selected, exit. Otherwise go back and ask for xy
*/
         	if (status != NCL_OKINPUT)
					goto done;
				else
					goto ask_again;
			}
			else if (status==NCL_OKINPUT)
			{
				for(j=0; j<3; j++) xyzpt[j] = tmp.cord[j];
				if (option == 1) xyzpt[2] = zval;
/*
.....remove preceeding spaces
*/
				i = 0; len = strlen(tmp.label);
				while (tmp.label[i]==' ') i++;
				if (tmp.label[i]!='\0')
				{
					strcpy(str, &(tmp.label[i]));
					if (option == 1)
					{
/*
.....add or replace the z value
*/
						ncl_replace_cordstr(str, zstr, 3);
					}
				}
				else
				{
					ncl_cctostr(nxy, xyzpt, str);
				}
/*
.....Add coordinates to command
*/
	           	status =ncl_add_token(&cmdbuf,str,NCL_comma);
        	}
/*
.....End rubberband.
*/
			ud_endrub(&rubber);
	
/*
.....Process command.
*/
			if ((status == NCL_OKINPUT))
			{
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
/*
.....Set status equal to NCL_DONE, if we are doing connected lines, status
.....will be set to NCL_OKINPUT, so that the WHILE (STATUS == NCL_OKINPUT)
.....loop won't be exited and the user will continue to select end points.
*/
				status = NCL_DONE;
/*
.....User selected CONNECTED so make the end point of this line the 
.....start point of the next line.
*/
				if(option==2)
				{
/*
.....1. Initalize command buffer.
.....2. Check if we need to prompt user for label.
.....3. Put LINE/ into command.
.....4. Put the old endpoint in as the new start point.
.....5. Save location for rubberbanding purposes.
.....6. Go back and ask for end point.
*/
					ncl_init_cmdbuf(&cmdbuf);
					if(!NCL_auto_label) ncl_add_name(&cmdbuf,1);
					ncl_add_token(&cmdbuf,NCL_ln,NCL_nocomma);
					ncl_add_token(&cmdbuf,str,NCL_comma);
					status = NCL_OKINPUT;
				}
			}
		}
	}
/*
.....Make sure that Save_Rubberband is unset and rubberbanding is done
*/

done:;
	RUBBER_OFF(&rubber);
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_ln_tanto_ci_ci()
**       description : Creates a line that is tangent to two circles.
**                     Has been changed drastically to get rid of the
**                     modifiers.  The line will be tangent to the 
**                     circle on the side of the circle where it was 
**                     picked.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ln_tanto_ci_ci()
{
	NCL_cmdbuf cmdbuf;
   UM_PLOCREC pick[2];                /* entity pick information */
   int numint,i;                        /* number of DAS entries returned */
   struct UM_line_rec l;              /* line entity being constructed */
	struct UM_circle_rec um_c1, um_c2; /* Needed to get point and nvec to define plane*/
	struct UM_circle_rec neweptr;      /* Circle2 projected int to plane of Circle1.*/
   struct UC_entitydatabag *c1;       /* first curve picked */
   UM_transf tfmat1;                  /* transformation for first curve */
   struct UC_entitydatabag *c2;       /* second curve picked */
   UM_transf tfmat2;                  /* transformation for second curve */
   int status;
	UM_vector direction;
	UU_REAL xmark;
	UU_REAL negxmark;
	UU_REAL ymark;
	UU_REAL negymark;
	UU_REAL temp[3];
	char str[80];
	UM_vector vpnorm;

   uu_denter( UU_MTRC,(us,"nclu_ln_tanto_ci_ci()"));

   ud_lgeo(UU_TRUE, UD_vircurves);

   c1 = (struct UC_entitydatabag *) uu_malloc(sizeof(struct UC_entitydatabag));
   c2 = (struct UC_entitydatabag *) uu_malloc(sizeof(struct UC_entitydatabag));

   while (UU_TRUE)
	{
/*
.....Initialize command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....Check to see if auto_label is on and if not prompt user for label.
*/
		if (!NCL_auto_label) status = ncl_add_name(&cmdbuf,1);
/*
.....Put LINE/ into the command.
*/
		status = ncl_add_token (&cmdbuf, NCL_ln, NCL_nocomma);
/*
.....Indicate that only circles may be selected.
*/

		ud_lgeo(UU_TRUE, UD_ncl_ci);
/*
.....Select first circle.
*/
      um_dl_pldas(UD_DASPCKLOC, UA_NCL, 57, &pick[0], 1, &numint, 2);
/*
.....If done was hit,exit.
*/
      if (numint <= 0) goto done;
/*
.....Get the key of the circle selected.
*/
      c1->key = um_get_pickkey(&pick[0].pent, 2);

/*
.....Again restrict selection to circles.      
*/
		ud_lgeo(UU_TRUE, UD_ncl_ci);
/*
.....Select second circle.
*/
      um_dl_pldas (UD_DASPCKLOC, UA_NCL,58, &pick[1], 1, &numint, 2);
/*
.....If done was hit, go back and ask for first circle.
*/
      if (numint <= 0) goto repeat;
/*
.....Get the key to the second circle.
*/
      c2->key = um_get_pickkey(&pick[1].pent, 2);

/*
.....Make sure that the same circle wasn't selected, if it display error
.....message.
*/
      if (c1->key == c2->key)
        uu_uerror0( UM_MODEL,60);
      else
		{
/*
.....Get the information about the first circle.
*/
         status = uc_retrieve_data(c1, sizeof(struct UC_entitydatabag));
         if (status != UU_SUCCESS) goto repeat;
         status = uc_retrieve_transf(c1->key, tfmat1);
/*
.....Now get all the information concerning the first circle, this is
.....needed to get the point and the normal vector for defining the plane
.....that the circle is in.  The point and normal vector will be used to
.....Project the second circle into the same plane as the first circle.
.....We will also use the center point information for determining 
.....wether to put LEFT or RIGHT into the command.
*/
			um_c1.key=c1->key;
			ur_retrieve_data(&um_c1, sizeof(struct UM_circle_rec));
         if (status != UU_SUCCESS) goto repeat;
/*
.....Now get information about the second circle.
*/
			um_c2.key=c2->key;
			ur_retrieve_data(&um_c2, sizeof(struct UM_circle_rec));
/*
.....Project second circle into the same plane as first circle, the
.....newly created circle will be used to create the line.
*/
			um_proj3_circ_to_plane(&um_c2,um_c1.center,um_c1.nvec,&neweptr);
			ur_create_data(&neweptr);
			c2->key=neweptr.key;
         status = uc_retrieve_data(c2, sizeof(struct UC_entitydatabag));
         if (status != UU_SUCCESS) goto repeat;
         status = uc_retrieve_transf(c2->key, tfmat2);
         if (status != UU_SUCCESS) goto repeat;
/*
.....Calling um_c2_tt will get the endpoints of the line that
.....will be tangent to the two circles, we will use those endpoints
.....to determine whether LEFT or RIGHT should be used as a modifier.
*/
         status =um_c2_tt(c1,  &pick[0].ploc, c2,  &pick[1].ploc, &l);
/*
.....If the second circle picked has a larger radius, we want
.....to reverse the endpts of the line
*/
			if (um_c1.radius<neweptr.radius)
			{
				for(i=0;i<3;i++) temp[i]=l.spt[i];
				for(i=0;i<3;i++) l.spt[i]=l.ept[i];
				for(i=0;i<3;i++) l.ept[i]=temp[i];
			}
/*
.....If this all worked out okay, YEAH! if not display an error message.
*/
         if (status != UU_SUCCESS)
			{
            uu_uerror0(/* unable to calculate line tangent to two curves */
            UM_MODEL, 281);
			}
         else
			{
/*
.....Transform through modsys and refsys if necessary.
*/
		      um_vpnorm(pick[0].ploc.transform, vpnorm);
      		ncl_proj_to_wp (um_c1.center,vpnorm,um_c1.center);
      		ncl_proj_to_wp (neweptr.center,vpnorm,neweptr.center);
      		ncl_proj_to_wp (l.spt,vpnorm,l.spt);
      		ncl_proj_to_wp (l.ept,vpnorm,l.ept);
/*
.....direction is the vector to indicate which direction the second 
.....circle is in.  Create the vector by subtracting the first
.....Circles center point from the projected circle's center.
.....Then make it a unit vector so we can then scale it to be the
.....length of the first circles radius.
*/
				direction[0]=neweptr.center[0]-um_c1.center[0];
				direction[1]=neweptr.center[1]-um_c1.center[1];
				direction[2]=0.0;
				um_unitvc(direction,direction);
 				um_vctmsc(direction,um_c1.radius,direction);
/*
.....The following 4 variables will be used to determine whether 
.....LEFT or RIGHT should be used.   These 4 values make up the
.....coordinates for points on the circle that create an inscribed
.....Square.  The comparisons will determine which side of the square's
.....diagonal the line will be on, thus determining whether LEFT or 
.....Right should be used.
*/
				xmark = um_c1.center[0] + direction[0];
				negxmark = um_c1.center[0] - direction[0];
				ymark = um_c1.center[1] + direction[1];
				negymark = um_c1.center[1] - direction[1];
/*
.....Case 1. Relative to the first circle's center point, the second
.....circle's center point is in the first quadrant.
*/
				
				if ((um_c1.center[0]<neweptr.center[0])&&(um_c1.center[1]<neweptr.center[1]))
				{
					if((l.ept[0]>negxmark) && (l.ept[1] < ymark))
						ncl_add_token(&cmdbuf, NCL_right, NCL_comma);	
					else
						ncl_add_token(&cmdbuf, NCL_left, NCL_comma);
				}
/*
.....Case 2. Relative to the first circle's center point, the second
.....circle's center point is in the second quadrant.
*/
				else if ((um_c1.center[0]<neweptr.center[0])&&(um_c1.center[1]>neweptr.center[1]))
				{
					if ((l.ept[0]>negxmark) && (l.ept[1]>ymark))
						ncl_add_token(&cmdbuf, NCL_left, NCL_comma);
					else
						ncl_add_token(&cmdbuf, NCL_right, NCL_comma);	
				}
/*
.....Case 3. Relative to the first circle's center point, the second
.....circle's center point is in the third quadrant.
*/
				else if ((um_c1.center[0]>neweptr.center[0])&&(um_c1.center[1]>neweptr.center[1]))
				{
					if((l.ept[0]<negxmark)&&(l.ept[1]>ymark))
						ncl_add_token(&cmdbuf, NCL_right, NCL_comma);	
					else
						ncl_add_token(&cmdbuf, NCL_left, NCL_comma);
				}
/*
.....Case 4. Relative to the first circle's center point, the second
.....circle's center point is in the forth quadrant.
*/
				else if ((um_c1.center[0]>neweptr.center[0])&&(um_c1.center[1]<neweptr.center[1]))
				{
					if((l.ept[0]>xmark)&&(l.ept[1]>negymark))
						ncl_add_token(&cmdbuf, NCL_right, NCL_comma);	
					else
						ncl_add_token(&cmdbuf, NCL_left, NCL_comma);
				}
/*
.....Case 5. The centers lie on the same vertical line.
*/
				else if (um_c1.center[0]==neweptr.center[0])
				{
					if (direction[1]>0) 
					{
						if(l.ept[0]>um_c1.center[0])
							ncl_add_token(&cmdbuf, NCL_right, NCL_comma);	
						else
							ncl_add_token(&cmdbuf, NCL_left, NCL_comma);
					}
					else
					{
						if(l.ept[0]<um_c1.center[0])
							ncl_add_token(&cmdbuf, NCL_right, NCL_comma);	
						else
							ncl_add_token(&cmdbuf, NCL_left, NCL_comma);
					}
				}
/*
.....Case 6. The centers lie on the same horizontal line.
*/
				else
				{
					if (direction[0]>0)
					{
						if (l.ept[1]<um_c1.center[1])
							ncl_add_token(&cmdbuf, NCL_right, NCL_comma);	
						else
							ncl_add_token(&cmdbuf, NCL_left, NCL_comma);
					}
					else
					{
						if(l.ept[1]>um_c1.center[1])
							ncl_add_token(&cmdbuf, NCL_right, NCL_comma);	
						else
							ncl_add_token(&cmdbuf, NCL_left, NCL_comma);
					}
				}
/*
.....Now add TANTO, and the first circles label to the command.
*/
				ncl_add_token(&cmdbuf,NCL_tanto,NCL_comma);
				ncl_get_label(&um_c1,str);
				ncl_add_token(&cmdbuf,str,NCL_comma);
/*
.....Make the vector, direction have the same length as the
.....radius of the second circle.
*/
				um_unitvc(direction,direction);
 				um_vctmsc(direction,neweptr.radius,direction);
/*
.....Calculate the values that make up the coordinates of the second
.....circle's inscribed square.
*/
				xmark = neweptr.center[0] - direction[0];
				negxmark = neweptr.center[0] + direction[0];
				ymark = neweptr.center[1] - direction[1];
				negymark = neweptr.center[1] + direction[1];
/*
.....Case 1. Second circle's center is in first quadrant relative to first
.....circle's center.
*/
				if ((um_c1.center[0]<neweptr.center[0])&&(um_c1.center[1]<neweptr.center[1]))
				{
					if((l.spt[0]<negxmark)&&(l.spt[1]>ymark))
						ncl_add_token(&cmdbuf, NCL_left, NCL_comma);
					else
						ncl_add_token(&cmdbuf, NCL_right, NCL_comma);	
				}
/*
.....Case 2. Second circle's center is in second quadrant relative to first
.....circle's center.
*/
				else if ((um_c1.center[0]<neweptr.center[0])&&(um_c1.center[1]>neweptr.center[1]))
				{
					if ((l.spt[0]<negxmark) && (l.spt[1]<ymark))
						ncl_add_token(&cmdbuf, NCL_right, NCL_comma);
					else
						ncl_add_token(&cmdbuf, NCL_left, NCL_comma);	
				}
/*
.....Case 3. Second circle's center is in third quadrant relative to first
.....circle's center.
*/
				else if ((um_c1.center[0]>neweptr.center[0])&&(um_c1.center[1]>neweptr.center[1]))
				{
					if((l.spt[0]<xmark)&&(l.spt[1]>negymark))
						ncl_add_token(&cmdbuf, NCL_right, NCL_comma);	
					else
						ncl_add_token(&cmdbuf, NCL_left, NCL_comma);
				}
/*
.....Case 4. Second circle's center is in forth quadrant relative to first
.....circle's center.
*/
				else if ((um_c1.center[0]>neweptr.center[0])&&(um_c1.center[1]<neweptr.center[1]))
				{
					if((l.spt[0]>negxmark)&&(l.spt[1]>ymark))
						ncl_add_token(&cmdbuf, NCL_right, NCL_comma);	
					else
						ncl_add_token(&cmdbuf, NCL_left, NCL_comma);
				}
/*
.....Case 5. The centers lie on the same vertical line.
*/
				else if (um_c1.center[0]==neweptr.center[0])
				{
					if (direction[1]>0) 
					{
						if(l.spt[0]>um_c1.center[0])
							ncl_add_token(&cmdbuf, NCL_right, NCL_comma);	
						else
							ncl_add_token(&cmdbuf, NCL_left, NCL_comma);
					}
					else
					{
						if(l.spt[0]<um_c1.center[0])
							ncl_add_token(&cmdbuf, NCL_right, NCL_comma);	
						else
							ncl_add_token(&cmdbuf, NCL_left, NCL_comma);
					}
				}
/*
.....Case 6. The centers lie on the same horizontal line.
*/
				else
				{
					if (direction[0]>0)
					{
						if (l.spt[1]<um_c1.center[1])
							ncl_add_token(&cmdbuf, NCL_right, NCL_comma);	
						else
							ncl_add_token(&cmdbuf, NCL_left, NCL_comma);
					}
					else
					{
						if(l.spt[1]>um_c1.center[1])
							ncl_add_token(&cmdbuf, NCL_right, NCL_comma);	
						else
							ncl_add_token(&cmdbuf, NCL_left, NCL_comma);
					}
				}
/*
.....Put TANTO, and the second circle's label into the command.
*/
				ncl_add_token(&cmdbuf, NCL_tanto, NCL_comma);
				ncl_get_label(&um_c2,str);
				ncl_add_token(&cmdbuf, str, NCL_nocomma);
/*
.....Delete all information regarding the projected circle.
*/
				ur_delete_all(neweptr.key);
				ncl_randel(neweptr.key,neweptr.rel_num);
/*
.....Process the command.
*/
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
				
			}
		}
repeat:;
	}
done:;

   uu_free(c1);
   uu_free(c2);
   uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_ln_plane()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ln_plane()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ln_plane"));

	while (UU_TRUE)
	{
/*
.....Initialize command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....Check to see if auto name is on, if not ask for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
/*
.....Put LINE/ into the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_ln, NCL_nocomma);
/*
.....Prompt user for a plane.
*/
      status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 219, UD_ncl_pl);

/*
.....If plane was selected, process the command, otherwise exit.
*/
		if ((status == NCL_OKINPUT))
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
		else
			goto done;
	}

done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_ln_pt_parlel_ln()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ln_pt_parlel_ln()

{
	NCL_cmdbuf cmdbuf;
	int status;
	char str[256];

	uu_denter(UU_MTRC,(us,"nclu_ln_pt_parlel_ln"));

	while (UU_TRUE)
	{
/*
.....Initialize command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....Check to see if auto label is on, if not prompt for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf,1);
/*
.....Put LINE/ into the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_ln, NCL_nocomma);
/*
.....prompt user for a line, this will actually be added to the
.....command buffer later, but ask for it here. JLS 6/3/99
*/
		status = ncl_get_dlabel(UD_DASPCKLOC, &str, 50, UD_ncl_pvln);
		if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user for either a point-vector or a point.
*/
		while (status == NCL_OKINPUT)
		{
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 49, UD_ncl_ptpv);

/*
.....If appropriate selection was made, process the command, if not
.....go back and to first prompt.
*/
			if ((status == NCL_OKINPUT))
			{
/*
.....Put PARLEL,LNxx into command.
*/
				status = ncl_add_token(&cmdbuf, NCL_parlel, NCL_comma);
				ncl_add_token(&cmdbuf, &str, NCL_comma);
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
/*
.....Initialize the command buffer, prompt for label, if needed.
.....Then put LN into the new command. 
*/
				ncl_init_cmdbuf(&cmdbuf);
				status = NCL_OKINPUT;
				if (!NCL_auto_label)
					status = ncl_add_name(&cmdbuf,1);
				status = ncl_add_token(&cmdbuf, NCL_ln, NCL_nocomma);
			}
		}
	}

done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_ln_pt_perpto_ln()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ln_pt_perpto_ln()

{
	NCL_cmdbuf cmdbuf;
	int status;
	int reltyp;
	char str[256];
	UM_PLOCREC pick;
	int numint;
	UU_KEY_ID key;

	uu_denter(UU_MTRC,(us,"nclu_ln_pt_perpto_ln"));

	while (UU_TRUE)
	{
/*
.....Initialize command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....Check to see if auto_label is on.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf,1);
/*
.....Put LINE/ into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_ln, NCL_nocomma);
/*
.....Prompt user for a line or a curve. It will be put in the command
.....buffer a little later.
*/
		ud_lgeo(UU_TRUE,UD_ncl_allcvlnpv);
		status = ua_dl_pldas(UD_DASPCKLOC, UA_NCL,52, &pick, 1,
                  &numint, 1);
		if (numint <=0) goto done;
/*
.....Use the key to find out what the relation number is.
*/
		status = NCL_OKINPUT;
      if(pick.pent.key[0] != 0 || pick.pent.key[1] != 0)
      {
         key = um_get_pickkey(&pick.pent, 1);
         um_retrieve_data_relnum(key, &reltyp);
      }
/*
.....If it wasn't a line or a point-vector, prompt user for a point
.....or a point-vector.
*/
		if((reltyp!=2)&&(reltyp!=98))
			status = ncl_get_dlabel(UD_DASPCKLOC, &str, 34, UD_ncl_ptpv);

/*
.....Prompt user for a point or point-vector.
*/
		while (status == NCL_OKINPUT)
		{
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 51, UD_ncl_ptpv);
/*
.....Place PERPTO,(ln/cv) [,pt/pv] into command.
*/
			if ((status == NCL_OKINPUT))
			{
				ncl_add_token(&cmdbuf, NCL_perpto, NCL_comma);
/*
.....Added the curve or line to the buffer.
*/
				ncl_add_token(&cmdbuf,pick.ploc.label,NCL_comma);
/*
.....If it was a curve, add the point or point-vector to the buffer.
*/
				if((reltyp!=2)&&(reltyp!=98))
					ncl_add_token(&cmdbuf,str,NCL_nocomma);

/*
.....Process command.
*/
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
/*
.....Initialize the new command, prompt for label if neccessary,
.....and put LN in to the command.
*/
				ncl_init_cmdbuf(&cmdbuf);
				status = NCL_OKINPUT;
				if (!NCL_auto_label)
					status = ncl_add_name(&cmdbuf,1);
				status = ncl_add_token(&cmdbuf, NCL_ln, NCL_nocomma);
			}
		}
	}

done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_ln_pt_atangl_ln()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ln_pt_atangl_ln()
{
	NCL_cmdbuf cmdbuf;
	int status,first,numint,i,len;
	char str[256],ln[256],pt[256];
	UD_SCA_VALUE angle;
	UU_REAL anyreal;

	uu_denter(UU_MTRC,(us,"nclu_ln_pt_atangl_ln"));

	while (UU_TRUE)
	{
/*
.....Initialize command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....Check to see if auto label is on and if not, prompt user for label.
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf,1);
		if (status == NCL_DONE) goto done;
		first = UU_TRUE;
/*
.....Get the line
*/
/*		status = ncl_get_dlabel(UD_DASPCKLOC, &ln, 46, UD_ncl_pvln);*/
		status = ncl_get_modifier(ln,NCL_WHICH_LINE);
		if (status != NCL_OKINPUT) break;
		while (UU_TRUE)
		{
/*
.....If not autolabel, then get label
*/
			if (!first)
			{
				status = NCL_OKINPUT;
				if (!NCL_auto_label)
					status = ncl_add_name(&cmdbuf,1);
				if (status == NCL_DONE) goto done;
			}
/*
.....Get the point
*/
			status = ncl_get_dlabel(UD_DASPCKLOC, &pt, 60, UD_ncl_ptpv);
			if (status == NCL_DONE || status == NCL_NOINPUT) break;
			while (UU_TRUE)
			{
/*
.....If not autolabel, then get label
*/
				if (!first)
				{
					status = NCL_OKINPUT;
					if (!NCL_auto_label)
						status = ncl_add_name(&cmdbuf,1);
					if (status == NCL_DONE) goto done;
				}
				first = UU_FALSE;
/*
.....Get angle
*/
				ud_ldas(UD_SCAVAL,UA_NCL, 61, &angle, 1, &numint, UD_NODEFAULT);
				if (numint <= 0) break;
				strcpy(str,angle.label);
/*
.....Place line definition into source file
*/
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_token(&cmdbuf, NCL_ln, NCL_nocomma);
				ncl_add_token(&cmdbuf,pt,NCL_comma);
				ncl_add_token(&cmdbuf,NCL_atangl,NCL_comma);
				ncl_add_token(&cmdbuf,str,NCL_comma);
				ncl_add_token(&cmdbuf,ln,NCL_nocomma);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
			}
			first = UU_TRUE;
		}
	}
done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_ln_pt_tanto_cv()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ln_pt_tanto_cv()

{
	NCL_cmdbuf cmdbuf;
	UU_KEY_ID key;
	int status,reltyp,first,numint,cvfl;
	char ldir[10],pt[80];
	struct UM_circle_rec c1;
	struct UM_point_rec p1;
/*	struct UM_line_rec l1;*/
	UM_transf tfmat1;
	UM_PLOCREC pick,pick1;
	UM_vector v1,v2;
	UM_coord ppt;
	UU_REAL ang;

	while (UU_TRUE)
	{
/*
.....Initialize command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....Check to see if auto label is on and if not, prompt user for label.
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf,1);
		if (status == NCL_DONE) goto done;
		first = UU_TRUE;
/*
.....Get the curve/circle
*/
		ud_lgeo(UU_TRUE,UD_ncl_cicv);
		status = ua_dl_pldas(UD_DASPCKLOC, UA_NCL, 36, &pick, 1, &numint, 2);
		if (numint <= 0) goto done;
/*
........Determine if curve or circle
*/
		status = NCL_OKINPUT;
		if (pick.pent.key[0] != 0 || pick.pent.key[1] != 0)
		{
			key = um_get_pickkey(&pick.pent, 1);
			um_retrieve_data_relnum(key,&reltyp);
		}
/*
...........Curve
...........Get a near point
*/
		if (reltyp != UM_CIRCLE_REL)
		{
			cvfl = UU_TRUE;
			status = ncl_get_dlabel(UD_DASPCKLOC, &pt, 37, UD_ncl_ptpv);
			if (status != NCL_OKINPUT) goto done;
		}
/*
..........Circle
..........Retrieve the circle information
*/
		else
		{
			cvfl = UU_FALSE;
			c1.key = key;
			status = ur_retrieve_data(&c1,sizeof(struct UM_circle_rec));
			status = uc_retrieve_transf(c1.key,tfmat1);
		}
/*
......Loop for the point, get modifier, and
......output command
*/
		while (UU_TRUE)
		{
/*
........If not autolabel, then get label
*/
			if (!first)
			{
				status = NCL_OKINPUT;
				if (!NCL_auto_label)
					status = ncl_add_name(&cmdbuf,1);
				if (status == NCL_DONE) goto done;
			}
			first = UU_FALSE;
/*
........Get the point
*/
			ud_lgeo(UU_TRUE,UD_ncl_ptpv);
			status = ua_dl_pldas(UD_DASPCKLOC, UA_NCL, 60, &pick1, 1, &numint, 2);
			if (numint <= 0) break;
			p1.key = um_get_pickkey(&pick1.pent, 1);
			status = ur_retrieve_data(&p1,sizeof(struct UM_point_rec));
/*
........Determine right or left if circle
...........First get line definition
*/
			if (!cvfl)
			{
				status = um_projploctopln(&pick.ploc,c1.center,c1.nvec,ppt);
/*				status = um_c2_pt_cir(&c1,tfmat1,p1.pt,&pick.ploc,&l1);*/
				if (status != UU_SUCCESS) break;
/*
...........Calculate angular differences
...........between CI/CE and LN/ENDPT
...........in reference to PT
...........This is used to generate LEFT or RIGHT
*/
				um_vcmnvc(c1.center,p1.pt,v1);
				um_unitvc(v1,v1);
				um_vcmnvc(ppt/*l1.ept*/,p1.pt,v2);
				um_unitvc(v2,v2);
				if (c1.nvec[0]+c1.nvec[1]+c1.nvec[2] < 0.)
					um_vctmsc(c1.nvec,-1.,c1.nvec);
				ang = um_angle2p(v1,v2,c1.nvec);
				if (ang > UM_PI) strcpy(ldir,NCL_right);
				else strcpy(ldir,NCL_left);
			}
/*
........Place line definition into source file
*/
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_token(&cmdbuf, NCL_ln, NCL_nocomma);
			ncl_add_token(&cmdbuf,pick1.ploc.label,NCL_comma);
			if (!cvfl) ncl_add_token(&cmdbuf,ldir,NCL_comma);
			ncl_add_token(&cmdbuf,NCL_tanto,NCL_comma);
			if (cvfl)
			{
				ncl_add_token(&cmdbuf,pick.ploc.label,NCL_comma);
				ncl_add_token(&cmdbuf,pt,NCL_nocomma);
			}
			else
				ncl_add_token(&cmdbuf,pick.ploc.label,NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
		first = UU_TRUE;
	}
done:;
}

/*********************************************************************
**    E_FUNCTION     : nclu_ln_intof_pl_pl()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ln_intof_pl_pl()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ln_intof_pl_pl"));

	while (UU_TRUE)
	{
/*
.....Initialize command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....Check to see if auto label is on, if not prompt for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf,1);
/*
.....Put LINE/ into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_ln, NCL_nocomma);
/*
.....Put INTOF, into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_intof, NCL_comma);
/*
.....Prompt user for plane.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 42, UD_ncl_pl);
/*
.....If done was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user for second plane.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 43, UD_ncl_pl);

/*
.....Process command if both planes were entered, if not go back
.....to first prompt.
*/
		if ((status == NCL_OKINPUT))
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}

	}

done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_ln_ci_atangl_ln()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ln_ci_atangl_ln()

{
	NCL_cmdbuf cmdbuf;
	int status;
	int numint;
	UM_PLOCREC pick;
	struct UM_circle_rec circle;
	char str[256];
	char strmod[256];
	char str3[256];
	UM_coord pt;
	UM_vector v1;
	UM_vector vpnorm;

	uu_denter(UU_MTRC,(us,"nclu_ln_ci_atangl_ln"));

	while (UU_TRUE)
	{
/*
.....Initialize command buffer.
*/
prompt1:;
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....Check to see if auto label is on, if not prompt user for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf,1);
/*
.....Put LINE/ into command, prompt user for the line that then new
.....line with be angled to, this will be put in the command buffer later.
*/
		status = ncl_add_token(&cmdbuf, NCL_ln, NCL_nocomma);
		status = ncl_get_dlabel(UD_DASPCKLOC, &str3, 46, UD_ncl_pvln);
/*
.....If no line was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Indicate that only circles may be selected.
*/
prompt2:
      ud_lgeo(UU_TRUE, UD_ncl_ci);
/*
.....Select circle.
*/
      um_dl_pldas(UD_DASPCKLOC, UA_NCL, 44, &pick, 1, &numint, 2);
/*
.....If done was hit, go back to the prompt asking for a line.
*/
      if (numint <= 0) goto prompt1;
/*
.....Get the key of the circle selected and get info on that circle.
*/
      circle.key = um_get_pickkey(&pick.pent, 2);
		ur_retrieve_data(&circle,sizeof(struct UM_circle_rec));
/*
.....Find out the coordinates of the point picked
*/
		um_new_nptent(circle.key,&pick.ploc,pt);
/*
.....Project to working plane down vp normal.
*/
		um_vpnorm(pick.ploc.transform, vpnorm);
		ncl_proj_to_wp (circle.center,vpnorm,circle.center);
		ncl_proj_to_wp (pt,vpnorm,pt);
/*
.....Determine which modifier to use. This depends on
.....where the picked point was relative to the center of
.....the circle.
*/
		um_vcmnvc (pt,circle.center,v1);
		um_unitvc(v1,v1);
		if (v1[0] > 0.7071)
			strcpy (strmod,NCL_xlarge);
		else if (v1[0] < -0.7071)
			strcpy(strmod,NCL_xsmall);
		else if (v1[1] > 0.0)
			strcpy (strmod,NCL_ylarge);
		else
			strcpy (strmod,NCL_ysmall);
/*
.....Add modifier, circle label, and ATANGL to the command.
*/
		status=ncl_add_token(&cmdbuf,strmod,NCL_comma);
		ncl_get_label(&circle,str);
		status=ncl_add_token(&cmdbuf,str,NCL_comma);
		status = ncl_add_token(&cmdbuf, NCL_atangl, NCL_comma);
/*
.....Prompt for angle.
*/
		while (status == NCL_OKINPUT)
		{
			status = ncl_add_angle(&cmdbuf, 45);
			if (status != NCL_OKINPUT)
			{
/*
.....The user hit done, initialize the command buffer, prompt user
.....for a label if necessary, put LINE/ into the command and go 
.....back and ask for a new circle.
*/
				ncl_init_cmdbuf(&cmdbuf);
				status = NCL_OKINPUT;
				if (!NCL_auto_label)
					status = ncl_add_name(&cmdbuf,1);
				status = ncl_add_token(&cmdbuf, NCL_ln, NCL_nocomma);
				goto prompt2;
			}
	
/*
.....Put line in the command buffer.
*/
			ncl_add_token(&cmdbuf, &str3, NCL_comma);
	
/*
.....Process command or go back to first prompt.
*/
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
/*
.....Initialize the command buffer, prompt user for label, if necessary.
.....Put LINE/modifier,CI,atangl, into the command buffer and go back up
.....and ask for a new angle.
*/
			ncl_init_cmdbuf(&cmdbuf);
			if (!NCL_auto_label)
				status = ncl_add_name(&cmdbuf,1);
			status = ncl_add_token(&cmdbuf, NCL_ln, NCL_nocomma);
			status = ncl_add_token(&cmdbuf,strmod,NCL_comma);
			ncl_get_label(&circle,str);
			status = ncl_add_token(&cmdbuf,str,NCL_comma);
			status = ncl_add_token(&cmdbuf, NCL_atangl, NCL_comma);
		}
	}

done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_ln_fwd()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ln_fwd()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ln_fwd"));

/*
.....Initialize command buffer.
*/
	ncl_init_cmdbuf(&cmdbuf);
	status = NCL_OKINPUT;
/*
.....Check to see if auto label is on and if not prompt for label.
*/
	if (!NCL_auto_label)
		status = ncl_add_name(&cmdbuf,1);
/*
.....Put LINE/FWD into command.
*/
	status = ncl_add_token(&cmdbuf, NCL_ln, NCL_nocomma);
	status = ncl_add_token(&cmdbuf, NCL_fwd, NCL_comma);
/*
.....Process command.
*/
	if ((status == NCL_OKINPUT))
	{
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_ln_xy_axis()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ln_xy_axis(choice)
int choice;
{
	NCL_cmdbuf cmdbuf;
	int status;
	int flag;

	uu_denter(UU_MTRC,(us,"nclu_ln_xy_axis"));

	flag=0;
	while (UU_TRUE)
	{
/*
.....Initialize command bufferf.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....If auto label is not on, prompt user for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf,1);
/*
.....Put LINE/ into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_ln, NCL_nocomma);

		switch (choice)
		{
/*
.....If it is x-axis, put XAXIS, into command.
*/
			case 1:
				status = ncl_add_token(&cmdbuf, NCL_xaxis, NCL_comma);
				break;
			case 2:
/*
.....If it is y-axis, put YAXIS, into command.
*/
				status = ncl_add_token(&cmdbuf, NCL_yaxis, NCL_comma);
				break;
		}
/*
.....Prompt user for length.
*/
		status = ncl_add_length(&cmdbuf, 59);
/*
.....If done was selected and it was the first time here, make
.....a line out of the axis.  Set the flag to 1 so that if
.....the user hits done another time, it will not make another
.....line out of the axis, but exit out of the routine.
*/
		if(status!=NCL_OKINPUT) 
		{
			if(flag==0)
				flag=1;
			else
				goto done;
		}
/*
.....Process the command.
*/
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}

done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_ln_offset()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ln_offset()

{
	NCL_cmdbuf cmdbuf;
	int status, save_loc;
	char str[256];
	int markval=0;

	uu_denter(UU_MTRC,(us,"nclu_ln_offset"));

	save_loc = UD_locint;
	UD_locint = UD_STRING;

	UD_MARK(markval,UU_FALSE);
	if (markval != 0)
	{
		UD_locint = save_loc;
		UD_UNMARK (markval);
		return;
	}
	while (UU_TRUE)
	{
/*
.....Initialize command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....Check to see if auto label is on, if not prompt for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf,1);
/*
.....Put LINE/ into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_ln, NCL_nocomma);
/*
.....Prompt user for point-vector or line, and put it in the command 
.....buffer.
*/
		status = ncl_get_dlabel(UD_DASPCKLOC, &str, 47, UD_ncl_pvln);
/*
.....If done was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Put the chosen line into the buffer.
*/
		ncl_add_token(&cmdbuf, &str, NCL_comma);
/*
.....While status is equal NCL_OKINPUT ask for the direction vector.
*/
		while (status == NCL_OKINPUT)
		{
/*
.....not use vector because vector can't allow (0,0,0)
*/
/*			status = ncl_add_vector(&cmdbuf, 48, 3); */
			status = ncl_add_coord(&cmdbuf, 48, 3);
/*
.....Process command if all the proper information
.....was given, otherwise go back to first prompt.
*/

			if ((status == NCL_OKINPUT))
			{
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
/*
.....Initialize the command buffer, prompt user for label if need,
.....and put LINE/ln, into the command buffer, then go back and ask
.....for a new vector.
*/
				ncl_init_cmdbuf(&cmdbuf);
				status = NCL_OKINPUT;
				if (!NCL_auto_label)
					status = ncl_add_name(&cmdbuf,1);
				ncl_add_token(&cmdbuf, NCL_ln, NCL_nocomma);
				ncl_add_token(&cmdbuf, &str, NCL_comma);
			}
		}
	}

done:;
	UD_locint = save_loc;
	UD_UNMARK (markval);
	uu_dexit;
}

/******************************************************************
**        
**         FUNCTION:  nclu_ln_parlel_ln
**
**         PURPOSE:   User interface to create a line parlel to 
**                    another at a given distance.
**
********************************************************************/
void nclu_ln_parlel_ln()
{
	NCL_cmdbuf cmdbuf;
   UM_PLOCREC pick;              /* pick information */
   int numint, rel_num;                   /* number of DAS entries returned */
   struct UM_line_rec e;         /* entity picked */
/*   struct UM_line_rec l;*/         /* new line entity */
	struct NCL_nclpv_rec ptvec;   /* point_vector */
   int status,i;
	char str[256];
	UU_KEY_ID key;
	UU_REAL slope, line_y,line_x;
	UU_REAL a,b,fa,fb;
	UM_coord pickpnt;
	int modifier;
	UM_vector vpnorm;

   uu_denter( UU_MTRC,(us,"nclu_ln_parlel_ln"));

	while (UU_TRUE)
	{
/*
.....Initialize command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....Check to see if auto_label is on and if not prompt user for label.
*/
		if (!NCL_auto_label) status = ncl_add_name(&cmdbuf,1);

/*
.....Put LINE/PARLEL, into the command.
*/
		status=ncl_add_token(&cmdbuf,NCL_ln,NCL_nocomma);
		status=ncl_add_token(&cmdbuf,NCL_parlel,NCL_comma);
/*
.....Now get the line or point-vector to be parallel to.
*/

		ud_lgeo(UU_TRUE, UD_ncl_pvln);
		um_dl_pldas(UD_DASPCKLOC, UM_MODEL, 137, &pick, 1, &numint, 2);
/*
.....If done was selected, exit.
*/
		if (numint <= 0) goto done;
		
/*
.....Convert pick.ploc to cartesian coordinates.
*/
		um_ploctocc(&pick.ploc,pickpnt);
/*
.....Retrieve the key and the relation number of the picked entity.
*/
		key = um_get_pickkey(&pick.pent, 2);
		um_retrieve_data_relnum(key, &rel_num);
/*
.....If it was a line, get the information about that line.
*/
		if (rel_num==UM_LINE_REL)
		{
			e.key=key;
			um_get_all_geom(&e, sizeof(e));
/*
.....Put the original lines label into the command.
*/
			ncl_get_label(&e,str);
			status = ncl_add_token(&cmdbuf,str,NCL_comma);
		}
		else
		{
/*
.....If it is a point -vector, get the information about the ptvec.
*/
			ptvec.key=key;
			ur_retrieve_data(&ptvec,sizeof(struct NCL_nclpv_rec));
/*
.....Copy the label into str and then add to command.
*/
			ncl_get_label(&ptvec,str);
			status = ncl_add_token(&cmdbuf,str,NCL_comma);
/*
.....Determine the start and end points.
*/
			for(i=0;i<3;i++) e.spt[i]=ptvec.pt[i];
			for(i=0;i<3;i++) e.ept[i]=(ptvec.pt[i]+ptvec.ve[i]);
		}
/*
.....Transform through modsys and refsys if necessary.
*/
		um_vpnorm(pick.ploc.transform, vpnorm);
		ncl_proj_to_wp (e.spt,vpnorm,e.spt);
		ncl_proj_to_wp (e.ept,vpnorm,e.ept);
		ncl_proj_to_wp (pickpnt,vpnorm,pickpnt);
/*
.....Determine modifier. 
*/
		a = e.spt[0] - e.ept[0]; fa = fabs (a);
		b = e.spt[1] - e.ept[1]; fb = fabs (b);
/*
..... degenerate case - line is invisible when projected on Z-plane
..... determine modifier by the direction from spt to pick location
*/
		if (fa < UM_FUZZ && fb < UM_FUZZ)
		{
			a = pickpnt[0]-e.spt[0];
			b = pickpnt[1]-e.spt[1];
			if (fabs(a) >= fabs(b)) 
			{
				if (a < 0.)
				{
					ncl_add_token(&cmdbuf,NCL_xsmall,NCL_comma);
					modifier = 2;
				}
				else
				{
					ncl_add_token(&cmdbuf,NCL_xlarge,NCL_comma);
					modifier = 3;
				}
			}
			else
			{
				if (b < 0.) 
				{
					ncl_add_token(&cmdbuf,NCL_ysmall,NCL_comma);
					modifier = 0;
				}
				else
				{
					ncl_add_token(&cmdbuf,NCL_ylarge,NCL_comma);
					modifier = 1;
				}
			}
		}
		else if (fa >= fb)
		{
/*
.....The line is more horizontal. Create the equation of the line to
.....determine the y value at the x value of the picked location.
.....If the line y is greater than the picked y then the modifier is 
.....ysmall, otherwise it is ylarge.
*/
			slope = b/a;
			line_y = (slope*(pickpnt[0]-e.spt[0])) + e.spt[1];
/*
.....modifier will be used to determine the appropriate Modifier
.....down below.
*/
			if (line_y>pickpnt[1])
			{
				ncl_add_token(&cmdbuf,NCL_ysmall,NCL_comma);
				modifier = 0;
			}
			else
			{
				ncl_add_token(&cmdbuf,NCL_ylarge,NCL_comma);
				modifier = 1;
			}
		}
		else 
/*
..... line is more vertical - proceed similarly
*/
		{
			slope = a/b;
			line_x = (slope*(pickpnt[1]-e.spt[1])) + e.spt[0];

			if (line_x>pickpnt[0])
			{
				ncl_add_token(&cmdbuf,NCL_xsmall,NCL_comma);
				modifier = 2;
			}
			else
			{
				ncl_add_token(&cmdbuf,NCL_xlarge,NCL_comma);
				modifier = 3;
			}
		}
/*
.....Get the distance
*/
		while (status == NCL_OKINPUT)
		{
			status =	ncl_add_length(&cmdbuf, 54);
/*
.....Process the command. If no distance was choosen, return
.....to the beginning of the routine and prompt for a new line.
*/
			if (status == NCL_OKINPUT)
			{
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
/*
.....Initialize a new command, this is when the user wants
.....to continue making parallel lines at different distances
.....from the first line picked.
*/
				ncl_init_cmdbuf(&cmdbuf);
				status = NCL_OKINPUT;
/*
.....if auto label is off, prompt user for label of new line.
*/
				if (!NCL_auto_label) status = ncl_add_name(&cmdbuf,1);
/*
.....Put LINE/PARLEL into the new command.
*/
				status=ncl_add_token(&cmdbuf,NCL_ln,NCL_nocomma);
				status=ncl_add_token(&cmdbuf,NCL_parlel,NCL_comma);
/*
.....Put the line label backing to the command.
*/
				status = ncl_add_token(&cmdbuf,str,NCL_comma);
/*
....Put the proper modifier into the command
*/
				if (modifier ==0)
					ncl_add_token(&cmdbuf,NCL_ysmall,NCL_comma);
				else if (modifier ==1)
					ncl_add_token(&cmdbuf,NCL_ylarge,NCL_comma);
				else if (modifier ==2)
					ncl_add_token(&cmdbuf,NCL_xsmall,NCL_comma);
				else 
					ncl_add_token(&cmdbuf,NCL_xlarge,NCL_comma);
			}
		}
	}
done:;
	uu_dexit;
}
