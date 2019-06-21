/*********************************************************************
**    NAME         :  nclclip.c
**       CONTAINS:  Routines for calculating boolean operations on polygons
**
**        ncl_polygon_clip
**        ncl_init_polygon
**        ncl_free_polygon
**        ncl_box_test
**        ncl_rotate_test
**        ncl_build_lmt
**        ncl_free_io_table
**        ncl_free_lmt
**        ncl_insert_bound
**        ncl_bound_list
**        ncl_add_to_sbtable
**        ncl_build_sbt
**        ncl_free_sbtable
**        ncl_add_edge_to_aet
**        ncl_add_intersection
**        ncl_add_sorted_edge
**        ncl_build_io_table
**        ncl_count_contours
**        ncl_add_left
**        ncl_merge_left
**        ncl_add_right 
**        ncl_merge_right
**        ncl_add_local_min
**        ncl_add_vertex
**
**    COPYRIGHT 2002 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nclclip.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:22
*********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <float.h>
#include <math.h>
#include "mgeom.h"
#include "nclclip.h"

#define ABOVE 0
#define BELOW 1

#define COADJ .998778699
#define SIADJ .049407601

typedef enum
{
	CLIP,
	SUBJ
} ncl_polygon_type;

typedef enum                   /* Edge intersection classes         */
{
	NUL,                         /* Empty non-intersection            */
	EMAX,                        /* External maximum                  */
	ELEFTI,                      /* External left intermediate        */
	TOPEGDE,                     /* Top edge                          */
	ERIGHTI,                     /* External right intermediate       */
	RIGHTEDGE,                   /* Right edge                        */
	IMINMAX,                     /* Internal maximum and minimum      */
	IMIN,                        /* Internal minimum                  */
	EMIN,                        /* External minimum                  */
	EMINMAX,                     /* External maximum and minimum      */
	LEFTEDGE,                    /* Left edge                         */
	ILEFTI,                      /* Internal left intermediate        */
	BOTEDGE,                     /* Bottom edge                       */
	IRIGHTI,                     /* Internal right intermediate       */
	IMAX,                        /* Internal maximum                  */
	FUL                          /* Full non-intersection             */
} ncl_vertex_type;

typedef enum	/* Edge state */
{
	NON,
	HEAD,
	TAIL
} ncl_edge_state;

typedef struct vtxx
{
	UM_2Dcoord pt; 		        /* vertex point */
	struct vtxx *next;	        /* pointer to next vertex in list */
} ncl_vertex_node;

typedef struct pgnn
{
	int active;                  /* Active flag / vertex count */
	int hole;                    /* Hole / external contour flag */
	ncl_vertex_node *v[2];       /* Left and right vertex list ptrs */
	struct pgnn *next;           /* Pointer to next contour */
	struct pgnn *proxy;          /* Pointer to actual structure used */
} ncl_polygon_node;

typedef struct edgg
{
	UM_2Dcoord vertex;           /* contour vertex data */
	UM_2Dcoord bot;              /* Edge lower (x, y) coordinate */
	UM_2Dcoord top;              /* Edge upper (x, y) coordinate */
	UU_REAL xbot;                /* Scanbeam bottom x coordinate */
	UU_REAL xtop;                /* Scanbeam top x coordinate */
	UU_REAL delx;                /* Change in x for a unit y increase */
	ncl_polygon_type type;       /* CLIP or SUBJ */
	int bundle[2][2];            /* Bundle edge flags */
	int side[2];                 /* Left / right indicators */
	ncl_edge_state state[2];     /* Edge state */
	ncl_polygon_node *outp[2];   /* Output polygon */
	struct edgg  *prev;          /* Previous edge in the AET */
	struct edgg  *next;          /* Next edge in the AET */
	struct edgg  *pred;          /* Edge connected at the lower end */
	struct edgg  *succ;          /* Edge connected at the upper end */
	struct edgg  *next_bound;    /* Pointer to next bound in LMT */
} ncl_edge_node;

typedef struct lmtt             /* Local minima table */
{
	UU_REAL y;                   /* Y coordinate at local minimum */
	ncl_edge_node *first_bound;  /* Pointer to bound list */
	struct lmtt *next;           /* Pointer to next local minimum */
} ncl_lmt;

/*
.....The scanbeam table is a
.....simple linked list that stores
.....the Y-values of the vertices in ascending order
*/
typedef struct sbtt             /* Scanbeam table */
{
	UU_REAL y;                   /* Scanbeam node y value */
	struct sbtt *lower;          /* Pointer to nodes with lower y */
	struct sbtt *higher;         /* Pointer to nodes with higher y */
} ncl_sb_table;

typedef struct iott             /* Intersection table */
{
	ncl_edge_node *ie[2];        /* Intersecting edge pair */
	UM_2Dcoord point;            /* Point of intersection */
	struct iott *next;           /* The next intersection table node */
} ncl_io_table;

typedef struct sted             /* Sorted edge table */
{
	ncl_edge_node *edge;         /* Pointer to AET edge */
	UU_REAL xbot;                /* Scanbeam bottom x coordinate */
	UU_REAL xtop;                /* Scanbeam top x coordinate */
	UU_REAL delx;                /* Change in x for a unit y increase */
	struct sted *prev;           /* Previous edge in sorted list */
} ncl_sorted_edge;

/* Horizontal edge state transitions within scanbeam boundary */
static int next_h_state[3][6] =
{
  /*      ABOVE    BELOW   CROSS */
  /*       L  R    L  R    L  R */  
  /* 0 */ {1, 2,   2, 1,   0, 0},
  /* 1 */ {0, 0,   0, 0,   2, 2},
  /* 2 */ {0, 0,   0, 0,   1, 1}
};

int *c_use,*s_use;
UU_LOGICAL UN_clip_debug=UU_FALSE;
UU_KEY_ID UN_sfkey;

static ncl_edge_node *edgem[2];
static int edge_malloc[2];
static UU_LOGICAL Srotate=UU_FALSE;

static void S_debug_lmt();
static void S_debug_sbtable();
static void S_debug_edge_node();
static void S_debug_outpoly();

/*********************************************************************
**    E_FUNCTION     : ncl_free_edgelst()
**       Free the polygon structure
**    PARAMETERS
**       INPUT  :
**                p     - polygon
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_free_edgelst()
{
	int i;
	for (i = 0; i < 2; i++)
	{
		UU_FREE (edgem[i]);
		edge_malloc[i] = 0;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_init_edgelst(npt)
**       Initialize the polygon structure
**    PARAMETERS
**       INPUT  :
**                p     - polygon
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_init_edgelst(npt)
int npt;
{
	int i,size;

	size = (npt > 99)? npt: 100;
	edge_malloc[0] = edge_malloc[1] = 0;

	for (i = 0; i < 2; i++)
	{
		edgem[i] = (ncl_edge_node *) uu_malloc (size * sizeof(ncl_edge_node));
		if (edgem[i] != UU_NULL)
		edge_malloc[i] = size;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_free_polygon(p)
**       Free the polygon structure
**    PARAMETERS
**       INPUT  :
**                p     - polygon
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_free_polygon(p)
ncl_polygon *p;
{
   UU_FREE(p->np);
   UU_FREE(p->box);
   UU_LIST_FREE(p->contour);
   p->num_contours = 0;
}

/*********************************************************************
**    E_FUNCTION     : void ncl_reset_polygon(p)
**       Reset the polygon structure
**    PARAMETERS
**       INPUT  :
**                p     - polygon
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_reset_polygon(p)
ncl_polygon *p;
{
   UU_FREE(p->np);
   UU_FREE(p->box);
   UU_LIST_EMPTY(p->contour);
   p->num_contours = 0;
}

/*********************************************************************
**    E_FUNCTION     : ncl_init_polygon(p,npt)
**       Initialize the polygon structure
**    PARAMETERS
**       INPUT  :
**                p     - polygon
**       OUTPUT : none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_init_polygon(p,npt)
ncl_polygon *p;
int npt;
{
	int status = UU_SUCCESS;

	p->num_contours = -1;
	p->np = (int *)UU_NULL;
	p->box = (UM_2box *)UU_NULL;
	if (npt < 1)
		p->contour = NULLST;
	else
	{
		p->contour = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
		if (p->contour == NULLST)
			status = UU_FAILURE;
		else
		{
			uu_list_init (p->contour,sizeof(UM_2Dcoord),npt,npt);
			if (p->contour->data == UU_NULL)
				status = UU_FAILURE;
		}
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_box_test(subj,clip,op)
**       Mark nonusable contours by testing min-max boxes
**    PARAMETERS
**       INPUT  :
**                subj     - subject polygon
**                clip     - clip polygon
**       OUTPUT :
**                c_use    - usable flags for clip polygon
**                s_use    - usable flags for subject polygon (only for intof)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_box_test(subj,clip,op)
ncl_polygon *subj,*clip;
ncl_polygon_op op;
{
	int s, c, *o_table, overlap;

	o_table = (int *) 
		uu_malloc (subj->num_contours * clip->num_contours * sizeof(int));
	if (!o_table) return;
/* 
..... Find overlaps between subject's and clip's bounding boxes
*/
	for (s = 0; s < subj->num_contours; s++)
	{
		for (c = 0; c < clip->num_contours; c++)
			o_table[c * subj->num_contours + s] =
				((subj->box[s].xmax >= clip->box[c].xmin) &&
				(subj->box[s].xmin <= clip->box[c].xmax) &&
				(subj->box[s].ymax >= clip->box[c].ymin) &&
				(subj->box[s].ymin <= clip->box[c].ymax));
	}
/*
..... For each clip contour, search for any subject contour overlaps
*/
	for (c = 0; c < clip->num_contours; c++)
	{
		overlap = 0;
		for (s = 0; (!overlap) && (s < subj->num_contours); s++)
			overlap = o_table[c * subj->num_contours + s];
		c_use[c] = overlap;
	}  

	if (op == NCL_INTOF)
	{  
/*
..... For each subject contour, search for any clip contour overlaps
*/
		for (s = 0; s < subj->num_contours; s++)
		{
			overlap = 0;
			for (c = 0; (!overlap) && (c < clip->num_contours); c++)
				overlap = o_table[c * subj->num_contours + s];
			s_use[s] = overlap;
		}  
	}

	UU_FREE(o_table);
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_rotate_test(subj,clip,tol)
**       Logic has problems if there are coincident horizontal lines
**       on the subject and clip polygons.  Need to rotate the polygons
**       a bit if there are.
**    PARAMETERS
**       INPUT  :
**                subj     - subject polygon
**                clip     - clip polygon
**                tol      - Tolerance.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : Sets the Srotate flag.
**    WARNINGS     : none
*********************************************************************/
static void ncl_rotate_test(subj,clip,tol)
ncl_polygon *subj,*clip;
UU_REAL tol;
{
	int c,d,nx,ny,i,j,i1,j1;
	UM_2Dcoord *vtx,*vty;
	ncl_polygon *p1,*p2;
/*
.....Initialize routine
*/
	Srotate = UU_FALSE;
	if (UN_clip_debug)
	{
		Srotate = UU_FALSE;
	}
/*
.....Only perform check if a single polygon
....with less than 5 sides
*/
   if ((subj->num_contours > 1 || subj->np[0] > 4) &&
      (clip->num_contours > 1 || clip->np[0] > 4)) return;
/*
.....Optimize search
*/
	if (UU_LIST_LENGTH(subj->contour) < UU_LIST_LENGTH(clip->contour))
	{
		p1 = subj; p2 = clip;
	}
	else
	{
		p1 = clip; p2 = subj;
	}
/*
.....Determine if there are any coincident lines
*/
	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (p1->contour);
	for (c = 0; c < p1->num_contours; c++, vtx += nx)
	{
		nx = abs(p1->np[c]);
		for (i=0; i<nx; i++)
		{
			i1 = PREV_INDEX(i,nx);
			if (fabs(vtx[i1][1]-vtx[i][1]) <= tol)
			{
				vty = (UM_2Dcoord *) UU_LIST_ARRAY (p2->contour);
				for (d = 0; d < p2->num_contours; d++, vty += ny)
				{
					ny = abs(p2->np[d]);
					for (j=0; j<ny; j++)
					{
						i1 = PREV_INDEX(i,nx);
						j1 = PREV_INDEX(j,ny);
						if (fabs(vty[j1][1]-vtx[i][1]) <= tol &&
							fabs(vty[j][1]-vtx[i][1]) <= tol &&
							((vty[j1][0]-tol <= vtx[i1][0] &&
							vty[j][0]+tol >= vtx[i1][0]) ||
							(vty[j1][0]-tol <= vtx[i][0] && vty[j][0]+tol >= vtx[i][0])
						|| (vty[j][0]-tol <= vtx[i1][0] &&
							vty[j1][0]+tol >= vtx[i1][0])
						||  (vty[j][0]-tol <= vtx[i][0] &&
							vty[j1][0]+tol >= vtx[i][0])))
						{
							Srotate = UU_TRUE;
							return;
						}
					}
				}
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_insert_bound(b,e)
**       
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_insert_bound(b,e)
ncl_edge_node **b,*e;
{
	ncl_edge_node *current;

/*
..... Do primary sort on the x field, secondary sort on the delx field
*/
	if (!(*b))
		*b = e; /* Link node e to the tail of the list */
	else
	{
		if (e[0].bot[0] < (*b)[0].bot[0] ||
			(e[0].bot[0] == (*b)[0].bot[0] && e[0].delx < (*b)[0].delx))
		{
			current = *b;
			*b = e;
			(*b)->next_bound = current;
		}
		else
			ncl_insert_bound(&((*b)->next_bound), e);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_bound_list(lmt,y)
**       
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static ncl_edge_node **ncl_bound_list(lmt,y)
ncl_lmt **lmt;
UU_REAL y;
{
	ncl_lmt *current;

	if (!(*lmt))
	{
		(*lmt) = (ncl_lmt *) uu_malloc (sizeof(ncl_lmt));
		if (!(*lmt)) return (UU_NULL);
		(*lmt)->y = y;
		(*lmt)->first_bound = UU_NULL;
		(*lmt)->next = UU_NULL;
		return (&((*lmt)->first_bound));
	}

	if (y < (*lmt)->y)
	{
/*
..... Insert a new LMT node before the current node
*/
		current = *lmt;
		(*lmt) = (ncl_lmt *) uu_malloc (sizeof(ncl_lmt));
		if (!(*lmt)) return (UU_NULL);
		(*lmt)->y = y;
		(*lmt)->first_bound = UU_NULL;
		(*lmt)->next = current;
		return (&((*lmt)->first_bound));
	}
	else if (y > (*lmt)->y)
		return (ncl_bound_list(&((*lmt)->next), y));
	else
		return (&((*lmt)->first_bound));
}

/*********************************************************************
**    E_FUNCTION     : ncl_add_to_sbtable(entries,sbtable,y)
**       Builds a scanbeam table based on Y low to high values.
**       Y-values that are the same are not added to the table (?).
**       
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_add_to_sbtable(entries,sbtable,y)
int *entries;
ncl_sb_table **sbtable;
UU_REAL y;
{
	if (!*sbtable)
	{
/*
.....Add a new tree node here
*/
		(*sbtable) = (ncl_sb_table *) uu_malloc (sizeof(ncl_sb_table));
		if (!*sbtable) return;
		(*sbtable)->y = y;
		(*sbtable)->lower = UU_NULL;
		(*sbtable)->higher = UU_NULL;
		(*entries)++;
	}
	else
	{
		if (y < (*sbtable)->y)
			ncl_add_to_sbtable(entries,&((*sbtable)->lower),y);
		else if (y > (*sbtable)->y)
			ncl_add_to_sbtable(entries,&((*sbtable)->higher),y);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_build_lmt(lmt,sbtable,sbt_entries,p,type,op,
**                                                         edges)
**       Description.
**    PARAMETERS
**       INPUT  :
**                p        -  polygon
**                op       -  clipping operation:
**                            NCL_DIFF, NCL_INTOF, or NCL_UNION
**                type     -  CLIP or SUBJ
**       OUTPUT :
**                
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_build_lmt(lmt,sbtable,sbt_entries,p,type,op,iedges)
ncl_lmt **lmt;
ncl_sb_table **sbtable;
int *sbt_entries;
ncl_polygon *p;
ncl_polygon_type type;
ncl_polygon_op op;
ncl_edge_node **iedges;
{
	int c, i, min, max, num_edges, v, nv,iv;
	int total_vertices = 0, e_index = 0;
	ncl_edge_node *e,*edges;
	int *use;
	UM_2Dcoord *vtx;

/*
.....Only use contours that have
.....been designated as used
.....(with Intersection and Differences only)
.....(all contours used with Union)
*/
	use = (type == CLIP)? c_use: s_use;
	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (p->contour);

/*
.....Count usable vertices
.....Drop any that are in the middle
.....of a straight Y-line
*/
	for (c = 0; c < p->num_contours; c++, vtx += nv)
	{
		nv = abs(p->np[c]);
		if (!use || use[c] > 0)
		{
			for (i = 0; i < nv; i++)
			{
				if (vtx[PREV_INDEX(i, nv)][1] != vtx[i][1] ||
						vtx[NEXT_INDEX(i, nv)][1] != vtx[i][1])
					total_vertices++;
			}
		}
	}
/*
.....Allocate an edge node
.....for each vertex
*/
	if (edge_malloc[type] > 0)
	{
		if (edge_malloc[type] < total_vertices)
		{
			UU_FREE (edgem[type]);
			edgem[type] = (ncl_edge_node *) 
				uu_malloc (total_vertices * sizeof(ncl_edge_node));
			edge_malloc[type] = total_vertices;
		}
		edges = edgem[type];
	}
	else
		edges = (ncl_edge_node *)
			uu_malloc (total_vertices * sizeof(ncl_edge_node));

	if (!edges) return;
	*iedges = edges;
/*
.....Store vertices in edge node and scanbeam table
.....Use same logic as when counting edges
*/
	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (p->contour);
	for (c = 0; c < p->num_contours; c++, vtx += nv)
	{
		nv = abs(p->np[c]);
		if (!use || use[c] > 0)
		{
			iv = 0;
			for (i = 0; i < nv; i++)
			{
				if (vtx[PREV_INDEX(i, nv)][1] != vtx[i][1] ||
						vtx[NEXT_INDEX(i, nv)][1] != vtx[i][1])
				{
					if (Srotate)
					{
						edges[iv].vertex[0] = vtx[i][0]*COADJ - vtx[i][1]*SIADJ;
						edges[iv].vertex[1] = vtx[i][1]*COADJ + vtx[i][0]*SIADJ;
					}
					else
					{
						edges[iv].vertex[0] = vtx[i][0];
						edges[iv].vertex[1] = vtx[i][1];
					}
/*
..... Add vertex to the scanbeam table
*/
/*					ncl_add_to_sbtable(sbt_entries,sbtable,vtx[i][1]);*/
					ncl_add_to_sbtable(sbt_entries,sbtable,edges[iv].vertex[1]);
					iv++;
				}
			}
/*
..... contour forward pass
*/
			for (min = 0; min < iv; min++)
			{
/*
..... If a forward local minimum
*/
				if (edges[PREV_INDEX(min, iv)].vertex[1] >= edges[min].vertex[1] &&
					edges[NEXT_INDEX(min, iv)].vertex[1] > edges[min].vertex[1])
				{
/*
..... Search for the next local maximum
*/
					num_edges = 1;
					max = NEXT_INDEX(min, iv);
					while (edges[NEXT_INDEX(max, iv)].vertex[1] > 
													edges[max].vertex[1])
					{
						num_edges++;
						max = NEXT_INDEX(max, iv);
					}
/*
..... Build the next edge list
*/
					e = &edges[e_index];
					e_index += num_edges;
					v = min;
					e[0].state[BELOW] = NON;
					e[0].bundle[BELOW][CLIP] = UU_FALSE;
					e[0].bundle[BELOW][SUBJ] = UU_FALSE;
					for (i = 0; i < num_edges; i++)
					{
						e[i].xbot = edges[v].vertex[0];
						e[i].bot[0] = edges[v].vertex[0];
						e[i].bot[1] = edges[v].vertex[1];

						v = NEXT_INDEX(v, iv);

						e[i].top[0] = edges[v].vertex[0];
						e[i].top[1] = edges[v].vertex[1];
						e[i].delx = (edges[v].vertex[0] - e[i].bot[0]) /
													(e[i].top[1] - e[i].bot[1]);
						e[i].type = type;
						e[i].outp[ABOVE] = UU_NULL;
						e[i].outp[BELOW] = UU_NULL;
						e[i].next = UU_NULL;
						e[i].prev = UU_NULL;
						e[i].succ = ((num_edges > 1) && (i < (num_edges - 1)))?
							&(e[i + 1]) : UU_NULL;
						e[i].pred = 
							((num_edges > 1) && (i > 0))? &(e[i - 1]): UU_NULL;
						e[i].next_bound = UU_NULL;
						e[i].side[CLIP] = (op == NCL_DIFF) ? 1: 0;
						e[i].side[SUBJ] = 0;
					}
					ncl_insert_bound(ncl_bound_list(lmt, edges[min].vertex[1]), e);
				}
			}
/*
..... contour reverse pass
*/
			for (min = 0; min < iv; min++)
			{
/*
..... If a reverse local minimum
*/
				if (edges[PREV_INDEX(min, iv)].vertex[1] > edges[min].vertex[1] &&
					edges[NEXT_INDEX(min, iv)].vertex[1] >= edges[min].vertex[1])
        		{
/*
..... Search for the previous local maximum
*/
					num_edges = 1;
					max = PREV_INDEX(min, iv);
					while (edges[PREV_INDEX(max, iv)].vertex[1] >
													edges[max].vertex[1])
					{
						num_edges++;
						max = PREV_INDEX(max, iv);
					}
/*
..... Build the previous edge list
*/
					e = &edges[e_index];
					e_index+= num_edges;
					v = min;
					e[0].state[BELOW] = NON;
					e[0].bundle[BELOW][CLIP] = UU_FALSE;
					e[0].bundle[BELOW][SUBJ] = UU_FALSE;
					for (i = 0; i < num_edges; i++)
					{
						e[i].xbot = edges[v].vertex[0];
						e[i].bot[0] = edges[v].vertex[0];
						e[i].bot[1] = edges[v].vertex[1];

						v = PREV_INDEX(v, iv);

						e[i].top[0] = edges[v].vertex[0];
						e[i].top[1] = edges[v].vertex[1];
						e[i].delx = (edges[v].vertex[0] - e[i].bot[0]) /
													(e[i].top[1] - e[i].bot[1]);
						e[i].type = type;
						e[i].outp[ABOVE] = UU_NULL;
						e[i].outp[BELOW] = UU_NULL;
						e[i].next = UU_NULL;
						e[i].prev = UU_NULL;
						e[i].succ = ((num_edges > 1) && (i < (num_edges - 1)))?
							&(e[i + 1]): UU_NULL;
						e[i].pred = 
							((num_edges > 1) && (i > 0))? &(e[i - 1]): UU_NULL;
						e[i].next_bound = UU_NULL;
						e[i].side[CLIP] = (op == NCL_DIFF) ? 1 : 0;
						e[i].side[SUBJ] = 0;
					}
					ncl_insert_bound(ncl_bound_list(lmt, edges[min].vertex[1]), e);
				}
			}
		}
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_free_io_table(iot)
**       Free the ncl_io_table linked list.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_free_io_table(iot)
ncl_io_table **iot;
{
	ncl_io_table *itn;

	while (*iot)
	{
		itn = (*iot)->next;
		UU_FREE(*iot);
		*iot = itn;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_free_lmt(lmt)
**       Free the lmt structure.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_free_lmt(lmt)
ncl_lmt **lmt;
{
	ncl_lmt *lmtn;

	while (*lmt)
	{
		lmtn = (*lmt)->next;
		UU_FREE(*lmt);
		*lmt = lmtn;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_build_sbt(entries,sbt,sbtable)
**       Description.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_build_sbt(entries,sbt,sbtable)
int *entries;
UU_REAL *sbt;
ncl_sb_table *sbtable;
{
	if (sbtable->lower)
		ncl_build_sbt(entries, sbt, sbtable->lower);
	sbt[*entries] = sbtable->y;
	(*entries)++;
	if (sbtable->higher)
		ncl_build_sbt(entries, sbt, sbtable->higher);
}

/*********************************************************************
**    E_FUNCTION     : ncl_free_sbtable(sbtable)
**       Free the ScanBeam table structure.
**    PARAMETERS
**       INPUT  :
**                   sbtable
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_free_sbtable(sbtable)
ncl_sb_table **sbtable;
{
	if (*sbtable)
	{
		ncl_free_sbtable(&((*sbtable)->lower));
		ncl_free_sbtable(&((*sbtable)->higher));
		UU_FREE(*sbtable);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_add_edge_to_aet(aet,edge,prev)
**       
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_add_edge_to_aet(aet,edge,prev)
ncl_edge_node **aet,*edge,*prev;
{
	if (!(*aet))
	{
		*aet = edge;
		edge->prev = prev;
		edge->next = UU_NULL;
		return;
	}
/*
..... Do primary sort on the xbot field, secondary sort on the delx field
*/
	if (edge->xbot < (*aet)->xbot ||
		(edge->xbot == (*aet)->xbot && edge->delx < (*aet)->delx))
	{
		edge->prev = prev;
		edge->next = *aet;
		(*aet)->prev = edge;
		*aet = edge;
	}
	else
		ncl_add_edge_to_aet(&((*aet)->next), edge, *aet);
}

/*********************************************************************
**    E_FUNCTION     : ncl_add_intersection(iot,edge0,edge1,x,y)
**       Description.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_add_intersection(iot,edge0,edge1,x,y)
ncl_io_table **iot;
ncl_edge_node *edge0,*edge1;
UU_REAL x,y;
{
	ncl_io_table *current_node;

	if (!(*iot))
	{
/*
..... Append a new node to the tail of the list
*/
		*iot = (ncl_io_table *) uu_malloc (sizeof(ncl_io_table));
		if (!(*iot)) return;
		(*iot)->ie[0] = edge0;
		(*iot)->ie[1] = edge1;
		(*iot)->point[0] = x;
		(*iot)->point[1] = y;
		(*iot)->next = UU_NULL;
	}
	else if ((*iot)->point[1] > y)
	{
/*
..... Insert a new node mid-list
*/
		current_node = *iot;
		*iot = (ncl_io_table *) uu_malloc (sizeof(ncl_io_table));
		if (!(*iot)) return;
		(*iot)->ie[0] = edge0;
		(*iot)->ie[1] = edge1;
		(*iot)->point[0] = x;
		(*iot)->point[1] = y;
		(*iot)->next = current_node;
    }
	else
		ncl_add_intersection(&((*iot)->next), edge0, edge1, x, y);
}

/*********************************************************************
**    E_FUNCTION     : ncl_add_sorted_edge(st,it,edge,dy)
**       Description.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_add_sorted_edge(st,it,edge,dy)
ncl_sorted_edge **st;
ncl_io_table **it;
ncl_edge_node *edge;
UU_REAL dy;
{
	ncl_sorted_edge *existing_node;
	UU_REAL den, r, x, y;

	if (!(*st))
	{
/*
..... Append edge onto the tail end of the Sorted Edge Table
*/
		(*st) = (ncl_sorted_edge *) uu_malloc (sizeof(ncl_sorted_edge));
		if (!(*st)) return;
		(*st)->edge = edge;
		(*st)->xbot = edge->xbot;
		(*st)->xtop = edge->xtop;
		(*st)->delx = edge->delx;
		(*st)->prev = UU_NULL;
	}
	else
	{
		den = ((*st)->xtop - (*st)->xbot) - (edge->xtop - edge->xbot);
		if ((edge->xtop >= (*st)->xtop) || (edge->delx == (*st)->delx) || 
			(fabs(den) <= UM_DFUZZ))
		{
/*
..... if no intersection - insert edge here (before the Sorted Edge Table edge)
*/
			existing_node = *st;
			(*st) = (ncl_sorted_edge *)
				uu_malloc (sizeof(ncl_sorted_edge));
			if (!(*st)) return;
			(*st)->edge = edge;
			(*st)->xbot = edge->xbot;
			(*st)->xtop = edge->xtop;
			(*st)->delx = edge->delx;
			(*st)->prev = existing_node;
		}
		else
		{
/*
..... Compute intersection between new edge and Sorted Edge Table edge
*/
			r = (edge->xbot - (*st)->xbot) / den;
			x = (*st)->xbot + r * ((*st)->xtop - (*st)->xbot);
			y = r * dy;

/*
..... Insert the edge pointers and the intersection point in the IT
*/
			ncl_add_intersection(it, (*st)->edge, edge, x, y);

			ncl_add_sorted_edge(&((*st)->prev), it, edge, dy);
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_build_io_table(it,aet,dy)
**       Description.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_build_io_table(it,aet,dy)
ncl_io_table **it; 
ncl_edge_node *aet; 
UU_REAL dy;
{
	ncl_sorted_edge *st, *stp;
	ncl_edge_node *edge;

/*
..... Build intersection table for the current scanbeam
*/
	ncl_free_io_table(it);
	st = UU_NULL;
/*
..... Process each AET edge
*/
	for (edge = aet; edge != UU_NULL; edge = edge->next)
	{
		if ((edge->state[ABOVE] == HEAD) || edge->bundle[ABOVE][CLIP] ||
			edge->bundle[ABOVE][SUBJ])
		ncl_add_sorted_edge(&st, it, edge, dy);
	}
/*
..... Free the sorted edge table
*/
	while (st)
	{
		stp = st->prev;
		UU_FREE(st);
		st = stp;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_count_contours(pol,npt,tolsq,itsk)
**       Count the number of contours in the output.
**    PARAMETERS
**       INPUT  :
**                 pol   - inner polygon structure
**                 itsk  - omit holes iff 1
**                 tolsq - squared tolerance
**       OUTPUT :
**                 npt  - total number of vertices
**    RETURNS      : number of (valid) contours
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_count_contours(pol,npt,tolsq,itsk)
ncl_polygon_node *pol;
int *npt,itsk;
UU_REAL tolsq;
{
	int nc, nv;
	ncl_vertex_node *v, *nextv;
	UU_REAL area = 0., areamin = 20.*tolsq;
	ncl_vertex_node *v0;
	char sbuf[80];

	for (nc = 0; pol; pol = pol->next)
	{
		if (pol->active)
		{
/*
..... Count the vertices in the current contour
*/
			nv = 0; area = 0.;
			if (itsk == 0 || !pol->proxy->hole || UN_sfkey == 19305)
			{
				for (v0 = v = pol->proxy->v[0]; v; v = v->next)
				{
					if (pol->proxy->hole && nv > 0 && v->next)
						area += um_triangle_signed_area(v0,v,v->next);
					nv++;
				}

				if (pol->proxy->hole) area = fabs(area);
				if (nv <= 5)
				{
					if (pol->proxy->hole && area >= areamin)
					{
						UU_REAL a = 0.,d;
						for (v = v0; v->next; v = v->next)
						{
							d = UM_SQDIS_2D(v->pt,v->next->pt);
							if (d > a) a = d;
						}
						if (area*area < a*tolsq) area = 0.;
					}
					else if (itsk == 1)
					{
						int n = 1;
						for (v = v0; v->next; v = v->next)
						{
							if (UM_SQDIS_2D(v->pt,v->next->pt) > tolsq) n++;
						}
						if (n < 3) nv = n;
					}
				}
			}
			if (nv < 3 || (pol->proxy->hole && area < areamin))
			{
/*
..... Invalid contour: just free the memory
*/
				for (v = pol->proxy->v[0]; v; v = nextv)
				{
					nextv = v->next;
					UU_FREE(v);
				}
if (UN_clip_debug) NclxDbgPstr("ncl_count_contours active = 0");
				pol->active = 0;
			}
/*
..... Record valid vertex counts in the active field	
*/
			else
			{
if (UN_clip_debug)
{
	sprintf(sbuf,"nv active = %d",nv);
	NclxDbgPstr(sbuf);
}
				pol->active = nv;
				nc++;
				(*npt) += nv;
			}
		}
	}

	return (nc);
}

/*********************************************************************
**    E_FUNCTION     : ncl_add_left(p,x,y)
**       Description.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_add_left(p,x,y)
ncl_polygon_node *p;
UU_REAL x,y;
{
	ncl_vertex_node *nv;

	if (!p) return;
	nv = (ncl_vertex_node *) uu_malloc(sizeof(ncl_vertex_node));
	if (!nv) return;
	nv->pt[0] = x;
	nv->pt[1] = y;
/*
..... Add vertex nv to the left end of the polygon's vertex list
*/
	nv->next = p->proxy->v[0];
/*
..... Update proxy->[0] to point to nv
*/
	p->proxy->v[0] = nv;
}

/*********************************************************************
**    E_FUNCTION     : ncl_merge_left(p,q,list)
**       Description.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_merge_left(p,q,list)
ncl_polygon_node *p,*q,*list;
{
	ncl_polygon_node *target;
	if (!p || !q) return;
/*
..... Label contour as hole
*/
	q->proxy->hole = UU_TRUE;

	if (p->proxy != q->proxy)
	{
/*
..... Assign p's vertex list to the left end of q's list
*/
		p->proxy->v[1]->next = q->proxy->v[0];
		q->proxy->v[0] = p->proxy->v[0];
/*
..... Redirect any p->proxy references to q->proxy
*/
		for (target = p->proxy; list; list = list->next)
		{
			if (list->proxy == target)
			{
if (UN_clip_debug) NclxDbgPstr("merge left active = 0");
				list->active = UU_FALSE;
				list->proxy = q->proxy;
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_add_right (p,x,y)
**       Description.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_add_right (p,x,y)
ncl_polygon_node *p;
UU_REAL x,y;
{
	ncl_vertex_node *nv;

	if (!p) return;
	nv = (ncl_vertex_node *) uu_malloc(sizeof(ncl_vertex_node));
	if (!nv) return;
	nv->pt[0] = x;
	nv->pt[1] = y;
	nv->next = UU_NULL;
/*
..... Add vertex nv to the right end of the polygon's vertex list
*/
	p->proxy->v[1]->next = nv;
/*
..... Update proxy->[1] to point to nv
*/
	p->proxy->v[1] = nv;
}

/*********************************************************************
**    E_FUNCTION     : ncl_merge_right(p,q,list)
**       Description.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_merge_right(p,q,list)
ncl_polygon_node *p,*q,*list;
{
	ncl_polygon_node *target;
	if (!p || !q) return;
/*
..... Label contour as external
*/
	q->proxy->hole = UU_FALSE;

	if (p->proxy != q->proxy)
	{
/*
..... Assign p's vertex list to the right end of q's list
*/
		q->proxy->v[1]->next = p->proxy->v[0];
		q->proxy->v[1] = p->proxy->v[1];
/*
..... Redirect any p->proxy references to q->proxy
*/
		for (target = p->proxy; list; list = list->next)
		{
			if (list->proxy == target)
			{
if (UN_clip_debug) NclxDbgPstr("merge right active = 0");
				list->active = UU_FALSE;
				list->proxy = q->proxy;
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_add_local_min(p,edge,x,y)
**       Description.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_add_local_min(p,edge,x,y)
ncl_polygon_node **p;
ncl_edge_node *edge;
UU_REAL x,y;
{
	ncl_polygon_node *cur_min;
	ncl_vertex_node *nv;

	cur_min = *p;

	*p = (ncl_polygon_node *) uu_malloc (sizeof(ncl_polygon_node));
	if (!(*p)) return;
	nv = (ncl_vertex_node *) uu_malloc (sizeof(ncl_vertex_node));
	if (!nv) return;

	nv->pt[0] = x;
	nv->pt[1] = y;
	nv->next = UU_NULL;
/*
..... Initialise proxy to point to p itself
*/
	(*p)->proxy = (*p);
	(*p)->active = UU_TRUE;
	(*p)->hole = UU_FALSE;
	(*p)->next = cur_min;

	(*p)->v[0] = nv;
	(*p)->v[1] = nv;

	edge->outp[ABOVE] = *p;
}

/*********************************************************************
**    E_FUNCTION     : ncl_add_vertex(vt,x,y)
**       Description.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_add_vertex(vt,x,y)
ncl_vertex_node **vt; 
UU_REAL x,y;
{
	if (!(*vt))
	{
		*vt = (ncl_vertex_node *) uu_malloc(sizeof(ncl_vertex_node));
		if (!(*vt)) return;
		(*vt)->pt[0] = x;
		(*vt)->pt[1] = y;
		(*vt)->next = UU_NULL;
	}
	else
		ncl_add_vertex(&((*vt)->next), x, y);
}

/*********************************************************************
**    E_FUNCTION     : ncl_polygon_clip(op,subj,clip,result)
**       Perform a binary operation (union,difference,intersection) on
**       2D polygons.
**    PARAMETERS
**       INPUT  :
**                op       - clipping type:
**                           NCL_DIFF, NCL_INTOF, or NCL_UNION
**                subj     - subject polygon
**                clip     - clip polygon
**       OUTPUT :
**                result   - result polygon
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_polygon_clip(op,subj,clip,result,tol,tolsq)
ncl_polygon_op op;
ncl_polygon *subj,*clip,*result;
UU_REAL tol,tolsq;
{
	ncl_io_table *iot = UU_NULL, *intersect;
	ncl_lmt *lmt = UU_NULL, *local_min;
	ncl_sb_table *sbtable = UU_NULL;
	ncl_edge_node *edge,*prev_edge,*next_edge,*succ_edge,*e0,*e1;
	ncl_edge_node *aet = UU_NULL, *c_edges = UU_NULL, *s_edges = UU_NULL;
	ncl_polygon_node *out_poly = UU_NULL,*cf = UU_NULL, *p, *q, *poly, *npoly;
	ncl_vertex_node *vtx, *nv;
	int horiz[2], in[2], exists[2], par[2];
	int c, contributing, scanbeam = 0, sbt_entries = 0;
	int vclass,bl,br,tl,tr,npt,cnt,nc;
	UU_REAL *sbt = UU_NULL;
	UU_REAL xbot,xtop,px,ybot,ytop,dy,xi,yi;
	UM_2Dcoord vt0,vtmp;
	int itsk = 0;
	UU_REAL eps = 0.1*tol;
	char sbuf[80];

#if 0
	NclxDbgPstr("ERASE/ALL");
	ncl_debug_polygon(UU_NULL,subj,4); /* GREEN */
	ncl_debug_polygon(UU_NULL,clip,2); /* BLUE */
#endif
	if (op == NCL_CONTOUR)
	{
		itsk = 1; op = NCL_UNION;
	}

	par[0] = par[1] = 0;
/*
..... Test for trivial null result cases
*/
	if (((subj->num_contours == 0) && (clip->num_contours == 0))
   || ((subj->num_contours == 0) && ((op == NCL_INTOF) || (op == NCL_DIFF)))
   || ((clip->num_contours == 0) &&  (op == NCL_INTOF)))
		return(-1);
/*
..... Flag nonusable contours
*/
	c_use = s_use = UU_NULL;
	if (((op == NCL_INTOF) || (op == NCL_DIFF))
			&& (subj->num_contours > 0) && (clip->num_contours > 0))
	{
		c_use = (int *) uu_malloc (clip->num_contours * sizeof(int));
		if (!c_use) return(-1);
		if (op == NCL_INTOF)
		{
			s_use = (int *) uu_malloc (subj->num_contours * sizeof(int));
			if (!s_use) { UU_FREE (c_use); return(-1); }
		}
		ncl_box_test (subj, clip, op);
	}
/*
.....Test for overlapping horizontal lines
.....This algorithm does not handle them correctly
.....(does not join contours)
.....So we rotate the contours to get rid of
.....the overlapping horizontal segments
*/
	ncl_rotate_test(subj,clip,eps);
/*
.....Build LMT
.....The s_edges and c_edges edge node arrays
.....Are just a bucket area for all defined edges
.....The lmt array contains the pointers to the edges
.....that are referenced later on
*/
	if (subj->num_contours > 0)
		ncl_build_lmt(&lmt, &sbtable, &sbt_entries, subj, SUBJ, op, &s_edges);
	if (clip->num_contours > 0)
		ncl_build_lmt(&lmt, &sbtable, &sbt_entries, clip, CLIP, op, &c_edges);
	if (UN_clip_debug)
	{
		NclxDbgPstr("");
		S_debug_sbtable(sbtable,sbt_entries);
	}
/*
..... Return a null result if no contours contribute
*/
	if (lmt == UU_NULL)
	{
		ncl_free_lmt (&lmt);
		UU_FREE (c_use); UU_FREE (s_use);
		if (edge_malloc[CLIP] == 0) UU_FREE(c_edges);
		if (edge_malloc[SUBJ] == 0) UU_FREE(s_edges);
		return (-1);
	}
	sbt = (UU_REAL *) uu_malloc (sbt_entries * sizeof(UU_REAL));
	if (!sbt) goto Done;
	ncl_build_sbt(&scanbeam,sbt,sbtable);
	scanbeam = 0;
	ncl_free_sbtable(&sbtable);

	if (itsk == 0 && (subj == result || clip == result))
	{
		UU_FREE(result->np);
		UU_FREE(result->box);
		result->num_contours = 0;
		if (result->contour && result->contour->item_size == sizeof (UM_2Dcoord)) 
			result->contour->cur_cnt = 0;
		else
			return (-1);
	}
	if (op == NCL_DIFF) par[CLIP] = 1;

	local_min = lmt;
/*
..... Process each scanbeam
*/
	while (scanbeam < sbt_entries)
	{
/*
..... Set ybot and ytop to the bottom and top of the scanbeam
*/
		ybot = sbt[scanbeam++];
		if (scanbeam < sbt_entries)
		{
			ytop = sbt[scanbeam];
			dy = ytop - ybot;
		}
/*
..... If Local Minima Table node corresponding to ybot exists
*/
		if (local_min)
		{
			if (local_min->y == ybot)
			{
/*
..... Add edges starting at this local minimum to the Active Edge Table
*/
				if (UN_clip_debug) S_debug_lmt(local_min);
				for (edge = local_min->first_bound; edge; edge = edge->next_bound)
				{
					ncl_add_edge_to_aet(&aet,edge,(ncl_edge_node *)UU_NULL);
					if (UN_clip_debug) S_debug_edge_node(edge);
				}
				local_min = local_min->next;
			}
		}
		px = -DBL_MAX; /* initialize "previous x" value */

		e0 = aet;
		e1 = aet;
/*
..... Set up bundle fields of first edge
*/
		aet->bundle[ABOVE][aet->type] = (aet->top[1] != ybot);
		aet->bundle[ABOVE][1 - aet->type] = UU_FALSE;
		aet->state[ABOVE] = NON;

		for (next_edge = aet->next; next_edge; next_edge = next_edge->next)
		{
/*
..... Set up bundle fields of next edge
*/
			next_edge->bundle[ABOVE][next_edge->type] = 
				(next_edge->top[1] != ybot);
			next_edge->bundle[ABOVE][1 - next_edge->type] = UU_FALSE;
			next_edge->state[ABOVE] = NON;
/*
..... Bundle edges above the scanbeam boundary if they coincide
*/
			if (next_edge->bundle[ABOVE][next_edge->type])
			{
				if (fabs(e0->xbot - next_edge->xbot) < eps &&
					fabs(e0->delx - next_edge->delx) < eps &&
					e0->top[1] != ybot)
				{
					next_edge->bundle[ABOVE][next_edge->type] ^= 
						e0->bundle[ABOVE][next_edge->type];
					next_edge->bundle[ABOVE][1 - next_edge->type] = 
						e0->bundle[ABOVE][1 - next_edge->type];
					next_edge->state[ABOVE] = HEAD;
					e0->bundle[ABOVE][CLIP] = UU_FALSE;
					e0->bundle[ABOVE][SUBJ] = UU_FALSE;
					e0->state[ABOVE] = TAIL;
				}
				e0 = next_edge;
			}
		}
    
		horiz[CLIP] = 0;
		horiz[SUBJ] = 0;
/*
..... Process each edge at this scanbeam boundary
*/
		for (edge = aet; edge; edge = edge->next)
		{
			exists[CLIP] = edge->bundle[ABOVE][CLIP] + 
							(edge->bundle[BELOW][CLIP] << 1);
			exists[SUBJ] = edge->bundle[ABOVE][SUBJ] + 
							(edge->bundle[BELOW][SUBJ] << 1);

			if (exists[CLIP] || exists[SUBJ])
			{
/*
..... Set bundle side
*/
				edge->side[CLIP] = par[CLIP];
				edge->side[SUBJ] = par[SUBJ];
/*
..... Determine contributing status and quadrant occupancies
*/
				switch (op)
				{
					case NCL_DIFF: case NCL_INTOF:
contributing = (exists[CLIP] && (par[SUBJ] || horiz[SUBJ])) ||
		(exists[SUBJ] && (par[CLIP] || horiz[CLIP])) ||
		(exists[CLIP] && exists[SUBJ] && (par[CLIP] == par[SUBJ]));
						br = (par[CLIP]) && (par[SUBJ]);
						bl = (par[CLIP] ^ edge->bundle[ABOVE][CLIP]) &&
								(par[SUBJ] ^ edge->bundle[ABOVE][SUBJ]);
						tr = (par[CLIP] ^ (horiz[CLIP] != 0)) &&
							(par[SUBJ] ^ (horiz[SUBJ]!=0)); 
			tl = (par[CLIP] ^ (horiz[CLIP] != 0) ^ edge->bundle[BELOW][CLIP]) &&
					(par[SUBJ] ^ (horiz[SUBJ]!=0) ^ edge->bundle[BELOW][SUBJ]);
						break;
					case NCL_UNION:
contributing = (exists[CLIP] && (!par[SUBJ] || horiz[SUBJ])) ||
		(exists[SUBJ] && (!par[CLIP] || horiz[CLIP])) ||
		(exists[CLIP] && exists[SUBJ] && (par[CLIP] == par[SUBJ]));
						br = (par[CLIP]) || (par[SUBJ]);
						bl = (par[CLIP] ^ edge->bundle[ABOVE][CLIP]) ||
							(par[SUBJ] ^ edge->bundle[ABOVE][SUBJ]);
						tr = (par[CLIP] ^ (horiz[CLIP]!=0)) ||
							(par[SUBJ] ^ (horiz[SUBJ]!=0));
			tl = (par[CLIP] ^ (horiz[CLIP]!=0) ^ edge->bundle[BELOW][CLIP]) || 
					(par[SUBJ] ^ (horiz[SUBJ]!=0) ^ edge->bundle[BELOW][SUBJ]);
if (UN_clip_debug)
{
	sprintf(sbuf,"Edge = %8.5f,%8.5f",edge->vertex[0],edge->vertex[1]);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"Contributing = %d",contributing);
	NclxDbgPstr(sbuf);
}
						break;
				}
/*
..... Update parity
*/
				par[CLIP] ^= edge->bundle[ABOVE][CLIP];
				par[SUBJ] ^= edge->bundle[ABOVE][SUBJ];
/*
..... Update horizontal state
*/
				if (exists[CLIP]) horiz[CLIP] =
					next_h_state[horiz[CLIP]][((exists[CLIP] - 1) << 1) + par[CLIP]];
				if (exists[SUBJ]) horiz[SUBJ] =
					next_h_state[horiz[SUBJ]][((exists[SUBJ] - 1) << 1) + par[SUBJ]];

				vclass = tr + (tl << 1) + (br << 2) + (bl << 3);

				if (contributing)
				{
					xbot = edge->xbot;
					switch (vclass)
					{
						case EMIN: case IMIN:
							ncl_add_local_min(&out_poly, edge, xbot, ybot);
							px = xbot;
							cf = edge->outp[ABOVE];
							break;
						case ERIGHTI:
							if (xbot != px)
							{
								ncl_add_right(cf, xbot, ybot);
								px = xbot;
							}
							edge->outp[ABOVE] = cf;
							cf = UU_NULL;
							break;
						case ELEFTI:
							ncl_add_left(edge->outp[BELOW], xbot, ybot);
							px = xbot;
							cf = edge->outp[BELOW];
							break;
						case EMAX:
							if (xbot != px)
							{
								ncl_add_left(cf, xbot, ybot);
								px = xbot;
							}
							ncl_merge_right(cf, edge->outp[BELOW], out_poly);
							cf = UU_NULL;
							break;
						case ILEFTI:
							if (xbot != px)
							{
								ncl_add_left(cf, xbot, ybot);
								px = xbot;
							}
							edge->outp[ABOVE] = cf;
							cf = UU_NULL;
							break;
						case IRIGHTI:
							ncl_add_right(edge->outp[BELOW], xbot, ybot);
							px = xbot;
							cf = edge->outp[BELOW];
							edge->outp[BELOW] = UU_NULL;
							break;
						case IMAX:
							if (xbot != px)
							{
								ncl_add_right(cf, xbot, ybot);
								px = xbot;
							}
							ncl_merge_left(cf,edge->outp[BELOW],out_poly);
							cf = UU_NULL;
							edge->outp[BELOW] = UU_NULL;
							break;
						case IMINMAX:
							if (xbot != px)
							{
								ncl_add_right(cf, xbot, ybot);
								px = xbot;
							}
							ncl_merge_left(cf,edge->outp[BELOW],out_poly);
							edge->outp[BELOW] = UU_NULL;
							ncl_add_local_min(&out_poly,edge, xbot,ybot);
							cf = edge->outp[ABOVE];
							break;
						case EMINMAX:
							if (xbot != px)
							{
								ncl_add_left(cf, xbot, ybot);
								px = xbot;
							}
							ncl_merge_right(cf,edge->outp[BELOW],out_poly);
							edge->outp[BELOW] = UU_NULL;
							ncl_add_local_min(&out_poly,edge,xbot,ybot);
							cf = edge->outp[ABOVE];
							break;
						case LEFTEDGE:
							if (edge->bot[1] == ybot)
							{
								ncl_add_left(edge->outp[BELOW],xbot,ybot);
							}
							edge->outp[ABOVE] = edge->outp[BELOW];
							px = xbot;
							break;
						case RIGHTEDGE:
							if (edge->bot[1] == ybot)
							{
								ncl_add_right(edge->outp[BELOW],xbot,ybot);
							}
							edge->outp[ABOVE] = edge->outp[BELOW];
							px = xbot;
							break;
						default:
							break;
					}
				} /* End of contributing conditional */
			} /* End of edge exists conditional */
		} /* End of AET loop */
/*
..... Delete terminating edges from the AET, otherwise compute xtop
*/
		for (edge = aet; edge; edge = edge->next)
		{
			if (edge->top[1] == ybot)
			{
				prev_edge = edge->prev;
				next_edge = edge->next;
				if (prev_edge)
					prev_edge->next = next_edge;
				else
					aet = next_edge;
				if (next_edge)
					next_edge->prev = prev_edge;
/*
..... Copy bundle head state to the adjacent tail edge if required
*/
				if ((edge->state[BELOW] == HEAD) && prev_edge)
				{
					if (prev_edge->state[BELOW] == TAIL)
					{
						prev_edge->outp[BELOW] = edge->outp[BELOW];
						prev_edge->state[BELOW] = NON;
						if (prev_edge->prev && prev_edge->prev->state[BELOW] == TAIL)
							prev_edge->state[BELOW] = HEAD;
					}
				}
			}
			else
			{
				if (edge->top[1] == ytop)
					edge->xtop = edge->top[0];
				else
					edge->xtop = edge->bot[0] + edge->delx * (ytop - edge->bot[1]);
			}
		}
/*
.... SCANBEAM INTERIOR PROCESSING
*/
		if (scanbeam < sbt_entries)
		{
			ncl_build_io_table(&iot, aet, dy);
/*
..... Process each node in the intersection table
*/
			for (intersect = iot; intersect; intersect = intersect->next)
			{
				e0 = intersect->ie[0];
				e1 = intersect->ie[1];

/*
..... Only generate output for contributing intersections
*/
				if ((e0->bundle[ABOVE][CLIP] || e0->bundle[ABOVE][SUBJ]) &&
					(e1->bundle[ABOVE][CLIP] || e1->bundle[ABOVE][SUBJ]))
				{
					p = e0->outp[ABOVE];
					q = e1->outp[ABOVE];
					xi = intersect->point[0];
					yi = intersect->point[1] + ybot;
#if 0
					if (UN_clip_debug && Ntm == 56)
					{
						sprintf(sbuf,"Intersect = %12.5f,%12.5f",xi,yi);
						NclxDbgPstr(sbuf);
					}
#endif
 
					in[CLIP] = (e0->bundle[ABOVE][CLIP] && !e0->side[CLIP]) ||
								(e1->bundle[ABOVE][CLIP] &&  e1->side[CLIP]) ||
								(!e0->bundle[ABOVE][CLIP] &&
								!e1->bundle[ABOVE][CLIP] &&
								e0->side[CLIP] && e1->side[CLIP]);
					in[SUBJ] = (e0->bundle[ABOVE][SUBJ] && !e0->side[SUBJ]) ||
								(e1->bundle[ABOVE][SUBJ] &&  e1->side[SUBJ]) ||
								(!e0->bundle[ABOVE][SUBJ] &&
								!e1->bundle[ABOVE][SUBJ] &&
								e0->side[SUBJ] && e1->side[SUBJ]);
       
/*
..... Determine quadrant occupancies
*/
					switch (op)
					{
						case NCL_DIFF: case NCL_INTOF:
							tr = (in[CLIP]) && (in[SUBJ]);
							tl = (in[CLIP] ^ e1->bundle[ABOVE][CLIP]) &&
									(in[SUBJ] ^ e1->bundle[ABOVE][SUBJ]);
							br = (in[CLIP] ^ e0->bundle[ABOVE][CLIP]) &&
									(in[SUBJ] ^ e0->bundle[ABOVE][SUBJ]);
			bl = (in[CLIP] ^ e1->bundle[ABOVE][CLIP] ^ e0->bundle[ABOVE][CLIP]) &&
					(in[SUBJ] ^ e1->bundle[ABOVE][SUBJ] ^ e0->bundle[ABOVE][SUBJ]);
							break;
						case NCL_UNION:
							tr = (in[CLIP]) || (in[SUBJ]);
							tl = (in[CLIP] ^ e1->bundle[ABOVE][CLIP]) ||
									(in[SUBJ] ^ e1->bundle[ABOVE][SUBJ]);
							br = (in[CLIP] ^ e0->bundle[ABOVE][CLIP]) ||
									(in[SUBJ] ^ e0->bundle[ABOVE][SUBJ]);
			bl = (in[CLIP] ^ e1->bundle[ABOVE][CLIP] ^ e0->bundle[ABOVE][CLIP]) ||
					(in[SUBJ] ^ e1->bundle[ABOVE][SUBJ] ^ e0->bundle[ABOVE][SUBJ]);
							break;
					}
	  
					vclass = tr + (tl << 1) + (br << 2) + (bl << 3);

					switch (vclass)
					{
						case EMIN:
							ncl_add_local_min(&out_poly, e0, xi, yi);
							e1->outp[ABOVE] = e0->outp[ABOVE];
							break;
						case ERIGHTI:
							if (p)
							{
								ncl_add_right(p, xi, yi);
								e1->outp[ABOVE] = p;
								e0->outp[ABOVE] = UU_NULL;
							}
							break;
						case ELEFTI:
							if (q)
							{
								ncl_add_left(q, xi, yi);
								e0->outp[ABOVE] = q;
								e1->outp[ABOVE] = UU_NULL;
							}
							break;
						case EMAX:
							if (p && q)
							{
								ncl_add_left(p, xi, yi);
								ncl_merge_right(p, q, out_poly);
								e0->outp[ABOVE] = UU_NULL;
								e1->outp[ABOVE] = UU_NULL;
							}
							break;
						case IMIN:
							ncl_add_local_min(&out_poly, e0, xi, yi);
							e1->outp[ABOVE] = e0->outp[ABOVE];
							break;
						case ILEFTI:
							if (p)
							{
								ncl_add_left(p, xi, yi);
								e1->outp[ABOVE] = p;
								e0->outp[ABOVE] = UU_NULL;
							}
							break;
						case IRIGHTI:
							if (q)
							{
								ncl_add_right(q, xi, yi);
								e0->outp[ABOVE] = q;
								e1->outp[ABOVE] = UU_NULL;
							}
							break;
						case IMAX:
							if (p && q)
							{
								ncl_add_right(p, xi, yi);
								ncl_merge_left(p, q, out_poly);
								e0->outp[ABOVE] = UU_NULL;
								e1->outp[ABOVE] = UU_NULL;
							}
							break;
						case IMINMAX:
							if (p && q)
							{
								ncl_add_right(p, xi, yi);
								ncl_merge_left(p, q, out_poly);
								ncl_add_local_min(&out_poly, e0, xi, yi);
								e1->outp[ABOVE] = e0->outp[ABOVE];
							}
							break;
						case EMINMAX:
							if (p && q)
							{
								ncl_add_left(p, xi, yi);
								ncl_merge_right(p, q, out_poly);
								ncl_add_local_min(&out_poly, e0, xi, yi);
								e1->outp[ABOVE] = e0->outp[ABOVE];
							}
							break;
						default:
							break;
					} /* End of switch */
				} /* End of contributing intersection conditional */
/*
..... Swap bundle sides in response to edge crossing
*/
				if (e0->bundle[ABOVE][CLIP])
					e1->side[CLIP] = !e1->side[CLIP];
				if (e1->bundle[ABOVE][CLIP])
					e0->side[CLIP] = !e0->side[CLIP];
				if (e0->bundle[ABOVE][SUBJ])
					e1->side[SUBJ] = !e1->side[SUBJ];
				if (e1->bundle[ABOVE][SUBJ])
					e0->side[SUBJ] = !e0->side[SUBJ];
/*
..... Swap e0 and e1 bundles in the AET
*/
				prev_edge = e0->prev;
				next_edge = e1->next;
				if (next_edge)
				next_edge->prev = e0;

				if (e0->state[ABOVE] == HEAD)
				{
					while (UU_TRUE)
					{
						prev_edge = prev_edge->prev;
						if (!prev_edge || prev_edge->state[ABOVE] != TAIL)
							break;
					}
				}
				if (!prev_edge)
				{
					aet->prev = e1;
					e1->next = aet;
					aet = e0->next;
				}
				else
				{
					prev_edge->next->prev = e1;
					e1->next = prev_edge->next;
					prev_edge->next = e0->next;
				}
				if (e0->next) e0->next->prev = prev_edge;
				if (e1->next) e1->next->prev = e1;
				e0->next = next_edge;
			} /* End of IT loop*/
/*
..... Prepare for next scanbeam
*/
			for (edge = aet; edge; edge = next_edge)
			{
				next_edge = edge->next;
				succ_edge = edge->succ;

				if ((edge->top[1] == ytop) && succ_edge)
				{
/*
..... Replace AET edge by its successor
*/
					succ_edge->outp[BELOW] = edge->outp[ABOVE];
					succ_edge->state[BELOW] = edge->state[ABOVE];
					succ_edge->bundle[BELOW][CLIP] = edge->bundle[ABOVE][CLIP];
					succ_edge->bundle[BELOW][SUBJ] = edge->bundle[ABOVE][SUBJ];
					prev_edge = edge->prev;
					if (prev_edge)
						prev_edge->next = succ_edge;
					else
						aet = succ_edge;
					if (next_edge)
						next_edge->prev = succ_edge;
					succ_edge->prev = prev_edge;
					succ_edge->next = next_edge;
				}
				else
				{
/*
..... Update this edge
*/
					edge->outp[BELOW] = edge->outp[ABOVE];
					edge->state[BELOW] = edge->state[ABOVE];
					edge->bundle[BELOW][CLIP] = edge->bundle[ABOVE][CLIP];
					edge->bundle[BELOW][SUBJ] = edge->bundle[ABOVE][SUBJ];
					edge->xbot = edge->xtop;
				}
				edge->outp[ABOVE] = UU_NULL;
			}
		}
if (UN_clip_debug) S_debug_outpoly(out_poly,tolsq,itsk);
	} /* end of scanbeam processing */
/*
..... Generate result polygon from out_poly
*/
		npt = 0;
		nc = ncl_count_contours(out_poly,&npt,tolsq,itsk);

		if (nc > 0)
		{
			if (itsk == 1 && (subj == result || clip == result))
			{
				UU_FREE(result->np);
				UU_FREE(result->box);
				result->num_contours = 0;
				if (result->contour) 
					result->contour->cur_cnt = 0;
				else
					goto Done;
			}
			result->num_contours = nc;
			result->np = (int *) uu_malloc (result->num_contours * sizeof(int));
			if (op != NCL_UNION) result->box = (UM_2box *)
				uu_malloc (result->num_contours * sizeof(UM_2box));
			if (!result->contour)
			{
				result->contour = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
				uu_list_init (result->contour,sizeof(UM_2Dcoord),npt,npt);
			}
			if (!result->np || !(op == NCL_UNION || result->box) || 
				!result->contour || !result->contour->data)
			{
				ncl_free_polygon (result);
				goto Done;
			}
			c = 0;
			for (poly = out_poly; poly; poly = npoly)
			{
				npoly = poly->next;
				if (poly->active)
				{
					cnt = result->contour->cur_cnt; npt = 1;
					vtx = poly->proxy->v[0];
					if (Srotate)
					{
						vtmp[0] = vtx->pt[0]*COADJ + vtx->pt[1]*SIADJ;
						vtmp[1] = vtx->pt[1]*COADJ - vtx->pt[0]*SIADJ;
						vtx->pt[0] = vtmp[0];
						vtx->pt[1] = vtmp[1];
					}
					xbot = xtop = vt0[0] = vtx->pt[0];
					ybot = ytop = vt0[1] = vtx->pt[1];
					uu_list_push (result->contour,vtx->pt);
					for (vtx = vtx->next; vtx; vtx = nv)
					{
						nv = vtx->next;
						if (Srotate)
						{
							vtmp[0] = vtx->pt[0]*COADJ + vtx->pt[1]*SIADJ;
							vtmp[1] = vtx->pt[1]*COADJ - vtx->pt[0]*SIADJ;
							vtx->pt[0] = vtmp[0];
							vtx->pt[1] = vtmp[1];
						}
						if (op != NCL_UNION)
						{
							if (vtx->pt[0] < xbot) xbot = vtx->pt[0];
							if (vtx->pt[1] < ybot) ybot = vtx->pt[1];
							if (vtx->pt[0] > xtop) xtop = vtx->pt[0];
							if (vtx->pt[1] > ytop) ytop = vtx->pt[1];
						}
						if (UM_SQDIS_2D(vt0,vtx->pt) > tolsq)
						{
							uu_list_push (result->contour,vtx->pt);
							npt++;
							vt0[0] = vtx->pt[0];
							vt0[1] = vtx->pt[1];
						}
						UU_FREE(vtx);
					}
					if (npt < 3)
					{
						result->contour->cur_cnt = cnt;
						result->num_contours--;
						continue;
					}
					result->np[c] = (!poly->proxy->hole)? npt: -npt;
					if (op != NCL_UNION)
					{
						result->box[c].xmin = xbot;
						result->box[c].ymin = ybot;
						result->box[c].xmax = xtop;
						result->box[c].ymax = ytop;
					}
					c++;
				}
				UU_FREE(poly);
			}
			if (result->num_contours == 0) UU_FREE(result->np);
		}
#if 0
		ncl_debug_polygon(UU_NULL,result,5); /* MAGENTA */
#endif

Done:;
	UU_FREE (c_use); UU_FREE (s_use);
	ncl_free_io_table(&iot);
	ncl_free_lmt (&lmt);
	if (edge_malloc[CLIP] == 0) UU_FREE(c_edges);
	if (edge_malloc[SUBJ] == 0) UU_FREE(s_edges);
	UU_FREE(sbt);
	if (result->num_contours > 0 && result->contour->cur_cnt >= 3)
	{
		if (nc <= 0 && itsk == 1 && (subj == result || clip == result))
			return (-1);
		return (0);
	}
	else
		return (-1);
}

/*********************************************************************
**    E_FUNCTION     : ncl_valid_polygon(poly,tol)
**       Determines if a polygon is valid for clipping.
**    PARAMETERS
**       INPUT  :
**          pol    - Polygon to check.
**          tol    - Tolerance for determining if polygon is valid.
**       OUTPUT : none
**
**    RETURNS      : UU_TRUE if a valid polygon.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_valid_polygon(pol,tol)
ncl_polygon *pol;
UU_REAL tol;
{
	int i,j;
	UU_LOGICAL xf,yf;
	UM_2Dcoord *pts;
/*
.....Loop through polygon points
.....and determine if a valid polygon is formed
.....Based on X distance and Y distance of points
*/
	pts = (UM_2Dcoord *)UU_LIST_ARRAY(pol->contour);
	for (i=0;i<pol->num_contours;i++)
	{
		xf = UU_FALSE; yf = UU_FALSE;
		if (pol->np[i] > 0)
		{
			for (j=1;j<pol->np[i];j++)
			{
				if (fabs(pts[j][0]-pts[0][0]) > tol*2) xf = UU_TRUE;
				if (fabs(pts[j][1]-pts[0][1]) > tol*2) yf = UU_TRUE;
				if (xf && yf)
				{
/*
char sbuf[80];
sprintf(sbuf,"Pt0 = %12.4f,%12.4f  Pt1 = %12.4f,%12.4f",pts[0][0],pts[0][1],
	pts[j][0],pts[j][1]);
NclxDbgPstr(sbuf);
*/
					return(UU_TRUE);
				}
			}
		}
		pts = pts + abs(pol->np[i]);
	}
	return(UU_FALSE);
}

/*********************************************************************
**    I_FUNCTION     : S_debug_lmt(lmt)
**       Print the 'ncl_lmt' structure.
**    PARAMETERS
**       INPUT  :
**          lmt    = Local minima table
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_debug_lmt(lmt)
ncl_lmt *lmt;
{
	char sbuf[80];
	NclxDbgPstr("");
	sprintf(sbuf,"Lmt Y = %8.5f   Edge = %x",lmt->y,lmt->first_bound);
	NclxDbgPstr(sbuf);
}

/*********************************************************************
**    I_FUNCTION     : S_debug_sbtable(sbtable,sbt_entries)
**       Print the 'ncl_sb_table' structure.
**    PARAMETERS
**       INPUT  :
**          sbt         = Scanbeam table
**          sbt_entries = Scanbeam table
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_debug_sbtable(sbtable)
ncl_sb_table *sbtable;
{
	char sbuf[80];

	if (sbtable->lower != UU_NULL)
		S_debug_sbtable(sbtable->lower);

	sprintf(sbuf,"Sbtable Y = %8.5f",sbtable->y);
	NclxDbgPstr(sbuf);

	if (sbtable->higher != UU_NULL)
		S_debug_sbtable(sbtable->higher);
}

/*********************************************************************
**    I_FUNCTION     : S_debug_edge_node(edge)
**       Print the linked 'ncl_edge_node' structures.
**    PARAMETERS
**       INPUT  :
**          edge   = Edge node
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_debug_edge_node(edge)
ncl_edge_node *edge;
{
	char sbuf[80];

	NclxDbgPstr("***********");
	if (edge->type == SUBJ) NclxDbgPstr("Subject Edge");
	else NclxDbgPstr("Clip Edge");
	NclxDbgPstr("***********");

	sprintf(sbuf,"Edge = %x",edge);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"Vertex = %8.5f,%8.5f",edge->vertex[0],edge->vertex[1]);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"Bottom = %8.5f,%8.5f",edge->bot[0],edge->bot[1]);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"Top = %8.5f,%8.5f",edge->top[0],edge->top[1]);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"Delx = %8.5f",edge->delx);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"Polygon Type = %d",edge->type);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"Bundle = %d,%d, %d,%d",edge->bundle[0][0],edge->bundle[0][1],
		edge->bundle[1][0],edge->bundle[1][1]);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"Side = %d,%d",edge->side[0],edge->side[1]);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"State = %d,%d",edge->state[0],edge->state[1]);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"Polygon = %x,%x",edge->outp[0],edge->outp[1]);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"Predecessor = %x",edge->pred);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"Successor = %x",edge->succ);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"Next Bound = %x",edge->next_bound);
	NclxDbgPstr(sbuf);
}

/*********************************************************************
**    I_FUNCTION     : S_debug_outpoly(out_poly,tolsq,itsk)
if (UN_clip_debug) S_debug_outpoly(out_poly,tolsq,itsk);
**       Print the linked 'ncl_edge_node' structures.
**    PARAMETERS
**       INPUT  :
**          edge   = Edge node
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_debug_outpoly(out_poly,tolsq,itsk)
ncl_polygon_node *out_poly;
UU_REAL tolsq;
int itsk;
{
	int nc,npt,nv;
	ncl_polygon_node *pol;
	ncl_vertex_node *v,*v0;
	char sbuf[80];

	pol = out_poly;
	nc = npt = 0;
	while (pol)
	{
		if (pol->active)
		{
/*
..... Count the vertices in the current contour
*/
			nv = 0;
			for (v0 = v = pol->proxy->v[0]; v; v = v->next) nv++;
			sprintf(sbuf,"pol->active = %d   hole = %d",nv,pol->proxy->hole);
			NclxDbgPstr(sbuf);
			npt += nv;
			nc++;
		}
		pol = pol->next;
	}
	sprintf(sbuf,"  Contours = %d   Npt = %d",nc,npt);
	NclxDbgPstr(sbuf);
}
