/*********************************************************************
**    NAME         :  viewddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 14:29:47  2015
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct UV_viewdef_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	name[15];
	int	vtype;
	UU_LOGICAL	can_save;
	UU_LOGICAL	modified;
	int	projection;
	UU_REAL	sav_ref_pt[3];
	UU_REAL	sav_pln_norm[3];
	UU_REAL	sav_up_vect[3];
	UU_REAL	sav_eye_dist;
	UU_REAL	sav_aperture;
	UU_REAL	sav_front_clip;
	UU_REAL	sav_back_clip;
	UU_LOGICAL	sav_do_clip;
	UU_REAL	cur_ref_pt[3];
	UU_REAL	cur_pln_norm[3];
	UU_REAL	cur_up_vect[3];
	UU_REAL	cur_eye_dist;
	UU_REAL	cur_aperture;
	UU_REAL	cur_front_clip;
	UU_REAL	cur_back_clip;
	UU_LOGICAL	cur_do_clip;
};

struct UV_vport_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	name[15];
	int	xform;
	UU_REAL	llf[3];
	UU_REAL	urb[3];
	UU_KEY_ID	cur_view;
	int	disp_prio;
	int	input_prio;
	UU_LOGICAL	disp_all;
	int	bord_seg;
	UU_LOGICAL	aperture_on;
	UU_LOGICAL	v_axis_on;
	UU_LOGICAL	name_on;
	UU_LOGICAL	bord_on;
	int	nverts;
	UU_REAL	vertices[60];
	int	grid_seg;
	UU_LOGICAL	grid_on;
	UU_LOGICAL	motion;
	int	disp_mode;
	UU_LOGICAL	wireframe;
};

struct UV_screen_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	name[15];
	int	active;
	int	wstation;
	int	nvports;
	UU_KEY_ID	vports[20];
};
