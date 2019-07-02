/*********************************************************************
**    NAME         :  wssgpD.h
**    CONTAINS:
**			Declarations of structures, errors, and functions used by
**			the DEV_GP1 package.
**   
**    MODULE NAME AND RELEASE LEVEL 
**       wssgpD.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:22
**
**		NOTES: THE FOLLOWING ARE THE ONLY CHANGES THAT UNICAD HAS
**			MADE TO THIS FILE:
**			1. CHANGED TO NAME OF THE FILE FROM "dev_gp1.h" TO "wssgpD.h"
**			2. ADDED THIS HEADER.
**
** Copyright (c) 1986 by Sun Microsystems, Inc.
**	@(#)dev_gp1.h 1.1 86/09/12 SMI
**
*********************************************************************/

typedef struct {
    float	x, y, z;
} Point3df;	/* a 3D floating-point point */ 

typedef struct {
    float	x, y;
} Point2df;	/* a 2D floating-point point */ 

typedef struct {
    int		x, y, z;
} Point3di;	/* a 3D integer point */ 

typedef struct {
    int		x, y;
} Point2di;	/* a 2D integer point */ 

typedef float	Matrix3df[4][4];

typedef float	Matrix2df[3][2];

typedef struct _GP1 	*GP1_handle;

/* pixwin is not using the GP1 */
#define DEV_GP1_ERR_NO_GP1		1 

/* can't allocate a context block */
#define DEV_GP1_ERR_NO_CONTEXT_BLK	2	

/* can't allocate a texture block */
#define DEV_GP1_ERR_NO_TEXTURE_BLK	3	

/* malloc failed */
#define DEV_GP1_ERR_NO_MEMORY		4	

/* an ioctl call failed */
#define DEV_GP1_ERR_IOCTL		5	




extern	GP1_handle	Dev_gp1_create();
extern	void		Dev_gp1_destroy();
extern	void		Dev_gp1_win_change();
extern	void		Dev_gp1_reset_to_defaults();
extern	void		Dev_gp1_flush();
extern	void		Dev_gp1_clear();
extern	void		Dev_gp1_setvp_3d();
extern	void		Dev_gp1_setvp_2d();
extern	void		Dev_gp1_set_color();
extern	void		Dev_gp1_set_write_mask();
extern	void		Dev_gp1_set_clip_planes();
extern	void		Dev_gp1_set_hidden_surf();
extern	void		Dev_gp1_set_rop_mode();
extern	void		Dev_gp1_set_z_buffer_depth();
extern	void		Dev_gp1_set_matrix_3d();
extern	void		Dev_gp1_set_matrix_2d();
extern	void		Dev_gp1_select_matrix();
extern	void		Dev_gp1_matrix_mul_2d();
extern	void		Dev_gp1_matrix_mul_3d();
extern	void		Dev_gp1_get_matrix_2d();
extern	void		Dev_gp1_get_matrix_3d();
extern	int		Dev_gp1_polygon_3df();
extern	int		Dev_gp1_polygon_2df();
extern	void		Dev_gp1_set_texture_org_screen();
extern	void		Dev_gp1_set_texture_org_2df();
extern	void		Dev_gp1_set_texture_org_3df();
extern	int		Dev_gp1_set_polygon_texture();
extern	void		Dev_gp1_print();
extern	void		Dev_gp1_move_2df();
extern	void		Dev_gp1_draw_2df();
extern	void		Dev_gp1_move_3df();
extern	void		Dev_gp1_draw_3df();
extern	void		Dev_gp1_set_line_texture();
extern	void		Dev_gp1_set_line_width();

