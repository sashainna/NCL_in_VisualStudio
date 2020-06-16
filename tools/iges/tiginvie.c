/*********************************************************************
**    NAME         :  tiginvie.c
**       CONTAINS:
**					uig_in_view
**					uig_in_drawing
**                  uig_default_drawing_view()
**                  uig_set_default_drawing_view()
**                  uig_vctovc(v1, v2)
**                  uig_remove_model()
**                  uig_in_drawing_planssoc()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tiginvie.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:47
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "mcrv.h"
#include "mdattr.h"
#include "view.h"
#include "udebug.h"

extern int MAX_PARA_REC;
UU_REAL  UIG_drawing_size = 43.18;

UU_LOGICAL drw_flag = UU_FALSE;
int default_drawing_ptr = 99999;
int drw_ptr = 0;
int drawing_id= 1;
UU_REAL drw_org[3] = {0.0,0.0,0.0};
UU_REAL drw_scale = 1.0;
UU_REAL drw_s[4][3], drw_v[4][3], drw_t[4][3];

/* cpp: add new data structure for view storage */
struct drw_view_data {
	int view_indx;
	UU_REAL drw_s[4][3];
	UU_REAL drw_v[4][3];
	UU_REAL drw_t[4][3];
	UU_REAL drw_org[3];
	UU_REAL drw_scale;
	};

/*********************************************************************
**    I_FUNCTION     :  uig_in_view(gblk,dblk,pblk,key)
**				Create a view record from an iges view.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_in_view(gblk,dblk,pblk,key)
	struct global_rec *gblk;				/* global record sturcture */
	struct dir_rec *dblk;					/* directory record */
	struct IG_igesvie_rec *pblk;			/* iges line parameter record */
	UU_KEY_ID *key;
	{
	char p_buff[80];
	int i,j;
	UU_REAL t[12];
	UU_KEY_ID e_key;
	UU_REAL vec[3], vecx[3] ,vecy[3] ,vecz[3], origin[3], scale[3], tfmat[4][3];
	UU_REAL modx[3], mody[3], modz[3], modorg[3], drawing_org[3];

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_in_view"));

	uig_get_trans(dblk->matrix_ptr,t);
	if(no_of_views == 0)
		{
		uig_map_view(dblk,pblk,t,&e_key);
		view_keys[no_of_views][0] = current_dir_number;
		view_keys[no_of_views][1] = e_key;
		no_of_views++;
		*key = e_key;
		}
	else
		{
		for(i=0;i<no_of_views;i++)
			{
			if(current_dir_number == view_keys[i][0])
				{
				*key = view_keys[i][1];
				goto draw;
				}
			}
		uig_map_view(dblk,pblk,t,&e_key);
		view_keys[no_of_views][0] = current_dir_number;
		view_keys[no_of_views][1] = e_key;
		if (no_of_views < MAX_VIEWS) no_of_views++;
		*key = e_key;
		}


	/* check if creating drawing view */ 

draw:;
	if(drw_flag == UU_TRUE)
		{	
		drw_scale = pblk->scale;
		if(drw_scale < 1.0e-6) drw_scale = 1.0;
		scale[2] = scale[1] = scale[0] = drw_scale;
		um_scaletf(scale, drw_s);
		for(i=0;i<3;i++)
			{
			modx[i] = mody[i] = modz[i] = modorg[i] = 0.0;
			}
		modx[0] = mody[1] = modz[2] = 1.0;
		for(i=1;i<5;i++)
			{
			uig_trans_comp(t,i,vec);
			switch(i)
				{
				case 1:
					uig_vctovc(vecx, vec);
					uu_dprint(UU_MTRC,(us," vx = (%g %g %g)", vecx[0],vecx[1],vecx[2]));
					break;
				case 2:
					uig_vctovc(vecy, vec);
					uu_dprint(UU_MTRC,(us," vy = (%g %g %g)", vecy[0],vecy[1],vecy[2]));
					break;
				case 3:
					uig_vctovc(vecz, vec);
					uu_dprint(UU_MTRC,(us," vz = (%g %g %g)", vecz[0],vecz[1],vecz[2]));
					break;
				case 4:
					uig_vctovc(origin, vec);
					uu_dprint(UU_MTRC,(us," origin 1 = (%g %g %g)", origin[0],origin[1],origin[2]));
					um_vctmsc(origin, unit_scale, origin);
					uu_dprint(UU_MTRC,(us," origin 2 = (%g %g %g)", origin[0],origin[1],origin[2]));
					break;
				}
			}
		um_chgcstf(modorg, modx, mody, modz, modorg, vecx, vecy, vecz, drw_v);
		um_disptf(origin, tfmat);
		um_tftmtf(drw_v, tfmat, drw_v);
		um_disptf(drw_org, drw_t);
		uu_dprint(UU_MTRC,(us,"scaling transformation :"));
		for(i=0;i<4;i++)
			uu_dprint(UU_MTRC,(us,"row %d: %g %g %g",i, drw_s[i][0], drw_s[i][1], drw_s[i][2]));
		uu_dprint(UU_MTRC,(us,"viewing transformation :"));
		for(i=0;i<4;i++)
			uu_dprint(UU_MTRC,(us,"row %d: %g %g %g",i, drw_v[i][0], drw_v[i][1], drw_v[i][2]));
		uu_dprint(UU_MTRC,(us,"drawing translation transformation :"));
		for(i=0;i<4;i++)
			uu_dprint(UU_MTRC,(us,"row %d: %g %g %g",i, drw_t[i][0], drw_t[i][1], drw_t[i][2]));
		}

	uu_dexit;
	}
/*********************************************************************
**    I_FUNCTION     :  uig_in_drawing(gblk,dblk,pblk,key)
**				Create a drawing view from an iges drawing.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_in_drawing(gblk,dblk,pblk,key)
	struct global_rec *gblk;				/* global record sturcture */
	struct dir_rec *dblk;					/* directory record */
	struct IG_igesdrw_rec *pblk;			/* iges drawing record */
	UU_KEY_ID *key;
	{
	char p_buff[80];
	UU_REAL t[12];
	UU_KEY_ID e_key, lkey, *p;
	int i,j,k,irec,jrec,itype,status,num,drawing_dir, num_grp, num_views;
	char *c;
	char *uu_toolmalloc();
	struct dir_rec ldir;
	struct dir_rec mdir;
	int ii, jj, kk, ll;
/*
.....Changed 'vdata' to memory list pointer
.....to allow for unlimited views
.....Bobby  -  6/7/94
*/
	struct drw_view_data *vdata;    /* cpp: temporary view data storage */
	UU_LOGICAL found;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_in_drawing"));

	num_views = pblk->no_v_tab;
	if (num_views < 1)
	{
		sprintf (p_buff, "(DREC = %d) Drawing contains no views, not translated.\n",
	               dblk->drec_num);
		uig_error(p_buff);
		*key = 0;
		goto fexit;
	}
	/* first create a default drawing view */
	uig_default_drawing_view();

	/* remove all non-drawing entities */
	uig_remove_model();

	/* cpp: modified DRAWING processing loop 
			First translate and store all views associated with the drawing.
			Then process each entity according to the view list. These change
			increases the speed of drawing translation.
	*/

	/* overall view loop */
	drawing_dir = current_dir_number;
	drw_flag = UU_TRUE;
	c = uu_toolmalloc(MAX_PARA_REC);
/*
.....Allocate memory for view data
.....Bobby  -  6/7/94
*/
	vdata = (struct drw_view_data *)uu_toolmalloc(sizeof(struct drw_view_data) *
		num_views);

   uig_str_out("Converting an IGES Drawing\n",UU_TRUE);

	uu_dprint(UU_MTRC,(us,"num views = %d", num_views));
	uu_dprint(UU_MTRC,(us,"drw_ptr = %d", drw_ptr));

	/* for each view in the drawing create viewing entities and store data */
	for(j=0;j<num_views;j++)
		{
		irec = pblk->v_tab[j].v_key;
		uu_dprint(UU_MTRC,(us,"irec = %d", irec));
		status = uig_get_data(irec,0,&ldir,c);
		current_dir_number = irec;
		vdata[j].view_indx = irec;
		vdata[j].drw_org[0] = drw_org[0] = pblk->v_tab[j].x_org*unit_scale;
		vdata[j].drw_org[1] = drw_org[1] = pblk->v_tab[j].y_org*unit_scale;
		vdata[j].drw_org[2] = drw_org[2] = 0.0;
		uig_in_view(gblk, &ldir, c, &lkey);
		vdata[j].drw_scale = drw_scale;
		for(jj=0;jj<4;jj++)
			{
			for(kk=0;kk<3;kk++)
				{
				vdata[j].drw_s[jj][kk] = drw_s[jj][kk];
				vdata[j].drw_v[jj][kk] = drw_v[jj][kk];
				vdata[j].drw_t[jj][kk] = drw_t[jj][kk];
				}
			}
		update_counts(10, irec);
		}

	 /* restore first view to globals */

	 irec = vdata[0].view_indx;
	 drw_scale = vdata[0].drw_scale;
	for(jj=0;jj<4;jj++)
		{
		for(kk=0;kk<3;kk++)
			{
			drw_s[jj][kk] = vdata[0].drw_s[jj][kk];
			drw_v[jj][kk] = vdata[0].drw_v[jj][kk];
			drw_t[jj][kk] = vdata[0].drw_t[jj][kk];
			}
		}

	/* loop through all entities again for the drawing */

   for(jrec = 1;jrec < (sect_ptr[2]+1);jrec++,jrec++)
      {
	  if(jrec == drawing_dir) continue;
      status = uig_get_data(jrec,0,&ldir,c);

	  if(status == 0)
		{
		if(  	ldir.sub_swt == 1 						||
			(ldir.sub_swt == 2 && ldir.view_ptr == 0) 	||
			(ldir.rel_type == DRAW )					||
			(ldir.rel_type == GTRAN )					||
			(ldir.rel_type == VIEW )) continue;

        if( uig_check_range(&ldir) == UU_TRUE)
           {
           lkey = -1;
	  		current_dir_number = jrec;
	       init_label(ldir.rel_type, jrec);
	       check_color(&ldir);	
	  		uu_dprint(UU_MTRC,(us,"irec = %d view_ptr = %d", irec, ldir.view_ptr));

			if(ldir.rel_type == PLNASSOC) 
				{
				uig_in_drawing_plnassoc(num_views, vdata, &gblk, &ldir, c, &lkey);
		   		update_counts(10, jrec);
				}
			else
				{
				if(ldir.view_ptr == 0)
					{
					for(jj=0;jj< num_views; jj++)
						{
						irec = vdata[jj].view_indx;
						drw_scale = vdata[jj].drw_scale;
						for(ll=0;ll<4;ll++)
							{
							for(kk=0;kk<3;kk++)
								{
								drw_s[ll][kk] = vdata[jj].drw_s[ll][kk];
								drw_v[ll][kk] = vdata[jj].drw_v[ll][kk];
								drw_t[ll][kk] = vdata[jj].drw_t[ll][kk];
								}
							}
			   			uig_in_dispat(&gblk,&ldir,c,&lkey); 
						}
		   			update_counts(lkey, jrec);
					}
				else
					{
					found = UU_FALSE;
					for(jj=0;jj< num_views; jj++)
						{
						if(ldir.view_ptr == vdata[jj].view_indx)
							{
							found = UU_TRUE;
							irec = vdata[jj].view_indx;
							drw_scale = vdata[jj].drw_scale;
							for(ll=0;ll<4;ll++)
								{
								for(kk=0;kk<3;kk++)
									{
									drw_s[ll][kk] = vdata[jj].drw_s[ll][kk];
									drw_v[ll][kk] = vdata[jj].drw_v[ll][kk];
									drw_t[ll][kk] = vdata[jj].drw_t[ll][kk];
									}
								}
							}
						}
					if(!found)
						{
						char *c1;
						c1 = uu_toolmalloc(MAX_PARA_REC);
						status = uig_get_data(ldir.view_ptr,0,&mdir,c1);
						if(mdir.rel_type == VIEWVS) 
							{
							struct IG_viewvs_rec *pview;
							UU_KEY_ID *p1;
							int ijk, num_v, v_irec;
							pview = (struct IG_viewvs_rec *) c1;
							p1 = pview->cid;
							num_v = pview->num_views;
							for(ijk=0;ijk<num_v;ijk++)
								{
								v_irec = p1[ijk];
								for(jj=0;jj< num_views; jj++)
									{
									if(v_irec == vdata[jj].view_indx)
										{
										irec = vdata[jj].view_indx;
										drw_scale = vdata[jj].drw_scale;
										for(ll=0;ll<4;ll++)
											{
											for(kk=0;kk<3;kk++)
												{
												drw_s[ll][kk] = vdata[jj].drw_s[ll][kk];
												drw_v[ll][kk] = vdata[jj].drw_v[ll][kk];
												drw_t[ll][kk] = vdata[jj].drw_t[ll][kk];
												}
											}
										/*
			   							uig_in_dispat(&gblk,&ldir,c,&lkey); 
										*/
										uu_dprint(UU_MTRC,(us,"view key = %d", v_irec));
										}
									}
								}
							}
						uu_toolfree(c1);
		   				update_counts(10, jrec);
						}
					else
						{
						if(ldir.sub_swt == 0)
							{
			   				uig_in_dispat(&gblk,&ldir,c,&lkey); 
		   					update_counts(10, jrec);
							}
						}
					}
	            }
			}
         }
      }

	uig_set_default_drawing_view();
	p = pblk->cid;
	num = pblk->no_cid;
	for(i=0;i<num;i++)
		{
		irec = p[i];
		status = uig_get_data(irec,0,&ldir,c);
		current_dir_number = irec;
		if(status == 0)
			{
	  		init_label(ldir.rel_type, irec);
	   		check_color(&ldir);	
			switch(ldir.rel_type)
				{
				case GLINE:
					uig_in_line(gblk,&ldir,c,&lkey);
					break;
				case GPOINT:
					uig_in_point(gblk,&ldir,c,&lkey);
					break;
				case GARC:
					uig_in_arc(gblk,&ldir,c,&lkey);
					break;
				case GCONIC:
					uig_in_conic(gblk,&ldir,c,&lkey);
					break;
				case GCOMPOSITE:
					uig_in_comp(gblk,&ldir,c,&lkey);
					break;
				case GGROUP:
					uig_in_group(gblk,&ldir,c,&lkey);
					break;
				case GPOLY:
					uig_in_poly2(gblk,&ldir,c,&lkey);
					break;
				case GPOLY3D:
					uig_in_poly3(gblk,&ldir,c,&lkey);
					break;
				case GPOLY6D:
					uig_in_poly6(gblk,&ldir,c,&lkey);
					break;
				case GSPLINE:
					uig_in_spline(gblk,&ldir,c,&lkey);
					break;
				case GRSPLINE:
					uig_in_rspline(gblk,&ldir,c,&lkey);
					break;
				case GNOTE:
					uig_in_gnote(gblk,&ldir,c,&lkey);
					break;
				case GANGDIM:
					uig_in_angdim(gblk,&ldir,c,&lkey);
					break;
				case GDIADIM:
					uig_in_diadim(gblk,&ldir,c,&lkey);
					break;
				case GLEADER:
					uig_in_leader(gblk,&ldir,c,&lkey);
					break;
				case GLINDIM:
					uig_in_lindim(gblk,&ldir,c,&lkey);
					break;
				case GRADIM:
					uig_in_radim(gblk,&ldir,c,&lkey);
					break;
				case GLABEL:
					uig_in_label(gblk,&ldir,c,&lkey);
					break;
				default:
					break;
				}
			}
		  	update_counts(lkey, irec);
		}

		/* re-initilize drawing parameters */

		drw_flag = UU_FALSE;
		uu_toolfree(c);
		uu_toolfree(vdata);
fexit:;

	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     :  uig_default_drawing_view()
**				Create a default Drawing view .
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_default_drawing_view()
	{
	UU_KEY_ID e_key;
	UU_REAL vecx[3] ,vecy[3] ,vecz[3], origin[3];
	int i,j,status;
	char p_buff[80];

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_default_drawing_view"));

	default_drawing_ptr++;
	for(i=0;i<3;i++)
		{
		vecx[i] = vecy[i]= vecz[i]= origin[i] = 0.0;
		}
	vecx[0] = vecy[1] = vecz[2] = 1.0;
	origin[0] = 21.59;
	origin[1] = 13.97;
	sprintf(p_buff,"DRAWING%d", drawing_id);
	drawing_id++;
	status = uv_create_view(p_buff, UV_USERDEF_VIEW, UV_PARALLEL,
				(UU_REAL) 0.0, origin, vecy, vecz, UIG_drawing_size, UV_NOCLIP,
				(UU_REAL) 10000.0, (UU_REAL) -10000.0, &e_key);

	view_keys[no_of_views][0] = default_drawing_ptr;
	view_keys[no_of_views][1] = e_key;
	no_of_views++;

	drw_ptr = default_drawing_ptr;
	uu_dprint(UU_MTRC,(us,"default view = %d mtid = %x", drw_ptr, e_key));

	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     :  uig_set_default_drawing_view()
**				Set the default Drawing view into the view globals .
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_set_default_drawing_view()
	{
	int i,j,status;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_set_default_drawing_view"));

	drw_org[0] = drw_org[1] = drw_org[2] = 0.0;
	drw_scale = 1.0;
	for(i=0;i<4;i++)
		for(j=0;j<3;j++)
			drw_v[i][j] = drw_s[i][j] = drw_t[i][j] = 0.0;
	drw_t[0][0] = drw_t[1][1] = drw_t[2][2] = 1.0;
	drw_v[0][0] = drw_v[1][1] = drw_v[2][2] = 1.0;
	drw_s[0][0] = drw_s[1][1] = drw_s[2][2] = 1.0;
	drw_ptr = default_drawing_ptr;
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     :  uig_vctovc(v1, v2)
**				copy v2 to v1.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_vctovc(v1, v2)
	UU_REAL v1[3], v2[3];
	{
	int i;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/


	for(i=0;i<3;i++)
		v1[i] = v2[i];
	}

/*********************************************************************
**    I_FUNCTION     :  uig_remove_model()
**				Remove non-drawing entities from Unibase 
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_remove_model()
	{
	int i;
	UU_KEY_ID next_tupleid, key, view_key;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	 next_tupleid = 1;

	/* query Unibase for next key */
	while (ur_get_next_key(&next_tupleid, &key) > -1)
		{
		next_tupleid++;
		ur_retrieve_view_key(key, &view_key);
		if (view_key == 0) ur_force_delete_all(key);
		}

	}

/*********************************************************************
**    I_FUNCTION     :  uig_in_drawing_planssoc()
**			Translate a planar association into the current drawing
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_in_drawing_plnassoc(num_views, vdata, gblk, ldir, pblk, lkey)
	int			num_views;
	struct drw_view_data vdata[20];    /* cpp: temporary view data storage */
	struct global_rec *gblk;				/* global record sturcture */
	struct dir_rec *ldir;					/* directory record */
	struct IG_plnassoc_rec *pblk;				/* iges view assoc  record */
	UU_KEY_ID *lkey;

	{
	UU_KEY_ID l_key, *p;
	int i,j,jj,k,ll,kk,jrec,irec,status,num;
	char *c;
	char *uu_toolmalloc();
	struct dir_rec l_dir;
	struct dir_rec mdir;
	UU_LOGICAL found;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_in_drawing_plnassoc"));

	c = uu_toolmalloc(MAX_PARA_REC);

	p = pblk->cid;
	num = pblk->num;
	for(i=0;i<num;i++)
		{
		jrec = p[i];
		status = uig_get_data(jrec,0,&l_dir,c);
		if(status == 0)
			{
	        if( uig_check_range(&l_dir) == UU_TRUE)
	           {
	           l_key = -1;
	  			init_label(l_dir.rel_type, jrec);
	   			check_color(&l_dir);	
		  		uu_dprint(UU_MTRC,(us,"jrec = %d view_ptr = %d", jrec, l_dir.view_ptr));
				if(l_dir.view_ptr == 0)
					{
					for(jj=0;jj< num_views; jj++)
						{
						irec = vdata[jj].view_indx;
						drw_scale = vdata[jj].drw_scale;
						for(ll=0;ll<4;ll++)
							{
							for(kk=0;kk<3;kk++)
								{
								drw_s[ll][kk] = vdata[jj].drw_s[ll][kk];
								drw_v[ll][kk] = vdata[jj].drw_v[ll][kk];
								drw_t[ll][kk] = vdata[jj].drw_t[ll][kk];
								}
							}	
			   			uig_in_dispat(&gblk,&l_dir,c,&l_key); 
						}
		   			update_counts(l_key, jrec);
					}
				else
					{
					found = UU_FALSE;
					for(jj=0;jj< num_views; jj++)
						{
						if(l_dir.view_ptr == vdata[jj].view_indx)
							{
							found = UU_TRUE;
							irec = vdata[jj].view_indx;
							drw_scale = vdata[jj].drw_scale;
							for(ll=0;ll<4;ll++)
								{
								for(kk=0;kk<3;kk++)
									{
									drw_s[ll][kk] = vdata[jj].drw_s[ll][kk];
									drw_v[ll][kk] = vdata[jj].drw_v[ll][kk];
									drw_t[ll][kk] = vdata[jj].drw_t[ll][kk];
									}
								}
							}
						}
					if(!found)
						{
						char *c1;
						c1 = uu_toolmalloc(MAX_PARA_REC);
						status = uig_get_data(l_dir.view_ptr,0,&mdir,c1);
						uu_dprint(UU_MTRC,(us,"relation type = %d", mdir.rel_type));
						if(mdir.rel_type == VIEWVS) 
							{
							struct IG_viewvs_rec *pview;
							UU_KEY_ID *p1;
							int ijk, num_v, v_irec;
							pview = (struct IG_viewvs_rec *) c1;
							p1 = pview->cid;
							num_v = pview->num_views;
							for(ijk=0;ijk<num_v;ijk++)
								{
								v_irec = p1[ijk];
								for(jj=0;jj< num_views; jj++)
									{
									if(v_irec == vdata[jj].view_indx)
										{
										irec = vdata[jj].view_indx;
										drw_scale = vdata[jj].drw_scale;
										for(ll=0;ll<4;ll++)
											{
											for(kk=0;kk<3;kk++)
												{
												drw_s[ll][kk] = vdata[jj].drw_s[ll][kk];
												drw_v[ll][kk] = vdata[jj].drw_v[ll][kk];
												drw_t[ll][kk] = vdata[jj].drw_t[ll][kk];
												}
											}
			   							uig_in_dispat(&gblk,&l_dir,c,&l_key); 
										}
									}
								}
							}
						uu_toolfree(c1);
		   				update_counts(l_key, jrec);
						}
					else
						{
			   			uig_in_dispat(&gblk,&l_dir,c,&l_key); 
		   				update_counts(l_key, jrec);
						}
					}
				}
			}
		}

	*lkey = l_key;
	uu_toolfree(c);
	uu_dexit;

	}
