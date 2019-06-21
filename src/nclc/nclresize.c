/*********************************************************************
**    NAME         :  nclresize.c
**       CONTAINS:  Routines for rearranging a set of 2D-boxes
**
**    COPYRIGHT 2005 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nclresize.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:22
*********************************************************************/
#include "usysdef.h"
#include "uminmax.h"
#include "mgeom.h"

#define HORZ 1
#define VERT 2
#define WHOLE 3

typedef struct
{
	UU_REAL xmin;
	UU_REAL xmax;
	UU_REAL ymin;
	UU_REAL ymax;
	int num;
} VW_2box;

static UU_LIST cxlst,dxlst;

/*********************************************************************
*********************************************************************/
static void ncl_add_box (box,amb)
VW_2box *box,*amb;
{
	VW_2box boxi;
	UU_REAL tol = 0.01;

	boxi.xmin = MAX2 (box->xmin,amb->xmin); 
	boxi.ymin = MAX2 (box->ymin,amb->ymin);
	boxi.xmax = MIN2 (box->xmax,amb->xmax); 
	boxi.ymax = MIN2 (box->ymax,amb->ymax);
	boxi.num = box->num;

	if (boxi.xmax >= boxi.xmin+tol && boxi.ymax >= boxi.ymin+tol)
		uu_list_push (&dxlst,&boxi);
}

/*********************************************************************
*********************************************************************/
static void ncl_remove_box (bxlst,i)
UU_LIST *bxlst;
int i;
{
	VW_2box *box;

	box = (VW_2box *) UU_LIST_ARRAY (bxlst);
	box[i].xmin = box[i].ymin = box[i].xmax = box[i].ymax = 0;
	uu_list_push (&dxlst,&box[i]);
	uu_list_delete (bxlst,i,1);
}

/*********************************************************************
*********************************************************************/
static int ncl_find_ll (box,n,amb,ax,ay)
int n;
VW_2box *box,*amb;
UU_REAL ax,ay;
{
	int i,ll;
	UU_REAL dx,dy,d,di;

	ll = 0;
	d = 1.e+20;
/*
..... compare squared distances from box centers to the lower-left corner
*/
	for (i = 0; i < n; i++)
	{
		dx = (box[i].xmin + box[i].xmax - 2.*amb->xmin) / ax;
		dy = (box[i].ymin + box[i].ymax - 2.*amb->ymin) / ay;
		di = dx*dx + dy*dy;
		if (di < d)
		{
			d = di; ll = i;
		}
	}

	return (ll);
}

/*********************************************************************
*********************************************************************/
static int ncl_long_box (bxlst,cxlst,amb,ax,ay,bxi)
UU_LIST *bxlst,*cxlst;
VW_2box *amb,*bxi;
UU_REAL ax,ay;
{
	int i,j,iret,mside,pside;
	int n = bxlst->cur_cnt;
	VW_2box *box;
	UU_REAL a0,a1,b0,b1,dx,dy,zi,zj;

	cxlst->cur_cnt = 0;
	if (n < 2) return (0);

	box = (VW_2box *) UU_LIST_ARRAY (bxlst);
	bxi->xmin = amb->xmin; bxi->ymin = amb->ymin;
	bxi->xmax = amb->xmax; bxi->ymax = amb->ymax;

	for (i = iret = 0; i < n && iret == 0; i++)
	{
		if (box[i].xmax > amb->xmax - 0.1*ax && box[i].xmin < amb->xmin + 0.1*ax)
		{
			iret = HORZ;
			a0 = amb->ymin + 0.01*ay;
			a1 = box[i].ymin - 0.01*ay;
			b0 = box[i].ymax + 0.01*ay;
			b1 = amb->ymax - 0.01*ay;
			zi = box[i].ymin + box[i].ymax;

			for (j = mside = pside = 0; (j < n) && (mside + pside < 2); j++)
			{
				if (j == i) continue;
				zj = box[j].ymin + box[j].ymax;
				if (box[j].ymax > a0 && box[j].ymin < a1 && zj < zi) mside = 1;
				if (box[j].ymax > b0 && box[j].ymin < b1 && zj >= zi) pside = 1;
			}
			if (mside == 0 && pside == 0)
			{
				iret = WHOLE;				
				bxi->num = box[i].num;
			}
			else
			{
				if (pside == 1) bxi->ymax = box[i].ymax;
				if (mside == 1) bxi->ymin = box[i].ymin;
				bxi->num = box[i].num;

				uu_list_delete (bxlst,i,1);
				n--;
				box = (VW_2box *) UU_LIST_ARRAY (bxlst);
				for (j = 0; j < n; j++)
				{
					zj = box[j].ymin + box[j].ymax;
					if (zj < zi)
					{
						uu_list_push (cxlst,&box[j]);
						uu_list_delete (bxlst,j,1);
						j--; n--;
						box = (VW_2box *) UU_LIST_ARRAY (bxlst);
					}
				}
			}
		}
		if (box[i].ymax > amb->ymax - 0.1*ay && box[i].ymin < amb->ymin + 0.1*ay)
		{
			iret = VERT;
			a0 = amb->xmin + 0.01*ax;
			a1 = box[i].xmin - 0.01*ax;
			b0 = box[i].xmax + 0.01*ax;
			b1 = amb->xmax - 0.01*ax;
			zi = box[i].xmin + box[i].xmax;

			for (j = mside = pside = 0; (j < n) && (mside + pside < 2); j++)
			{
				if (j == i) continue;
				zj = box[j].xmin + box[j].xmax;
				if (box[j].xmax > a0 && box[j].xmin < a1 && zj < zi) mside = 1;
				if (box[j].xmax > b0 && box[j].xmin < b1 && zj >= zi) pside = 1;
			}
			if (mside == 0 && pside == 0)
			{
				iret = WHOLE;				
				bxi->num = box[i].num;
			}
			else
			{
				if (pside == 1) bxi->xmax = box[i].xmax;
				if (mside == 1) bxi->xmin = box[i].xmin;
				bxi->num = box[i].num;
				uu_list_delete (bxlst,i,1);
				n--;
				box = (VW_2box *) UU_LIST_ARRAY (bxlst);
				for (j = 0; j < n; j++)
				{
					zj = box[j].xmin + box[j].xmax;
					if (zj < zi)
					{
						uu_list_push (cxlst,&box[j]);
						uu_list_delete (bxlst,j,1);
						j--; n--;
						box = (VW_2box *) UU_LIST_ARRAY (bxlst);
					}
				}
			}
		}
	}
	if (iret == WHOLE)
	{
		uu_list_delete (bxlst,i,1);
		n--;
		for (j = 0; j < n; j++) ncl_remove_box (bxlst,j);
	}

	return (iret);
}

/*********************************************************************
*********************************************************************/
static void ncl_resolve(bxlst,cxlst,amb,ax,ay)
UU_LIST *bxlst,*cxlst;
VW_2box *amb;
UU_REAL ax,ay;
{
	int i,m,M;
	int n = bxlst->cur_cnt;
	VW_2box *box;
	UU_REAL x0,y0,x1,y1,a,A,r0,r1,r;


	cxlst->cur_cnt = 0;
	if (n < 2) return;

	box = (VW_2box *) UU_LIST_ARRAY (bxlst);
	r0 = 1; r1 = 0;
		
	for (i = 0; i < n; i++)
	{
		r = 0;
		x0 = MAX2 (box[i].xmin,amb->xmin); 
		y0 = MAX2 (box[i].ymin,amb->ymin);
		x1 = MIN2 (box[i].xmax,amb->xmax); 
		y1 = MIN2 (box[i].ymax,amb->ymax);
		if (x1 > x0 + 0.01*ax && y1 > y0 + 0.01*ay)
		{
			a = (x1 - x0)*(y1 - y0);
			A = (box[i].xmax - box[i].xmin)*(box[i].ymax - box[i].ymin);
			r = a/A;
		}
		if (r < r0) r0 = r;
		if (r > r1) r1 = r;
	}

	if (r1 < 0.4) return;

	M = (n + 1)/3;
	if (r0 > 0.99*r1)
	{
		for (i = 0; i < n; i++)
		{
			uu_list_push (cxlst,&box[i]);
			m = cxlst->cur_cnt;
			if (m >= M)
			{
				uu_list_delete (bxlst,0,m);
				return;
			}
		}
	}
	else
	{
		if (r0 < 0.4) r0 = 0.4;
		r = (2.*r1 + r0)/3.;
		for (i = 0; i < n; i++)
		{
			x0 = MAX2 (box[i].xmin,amb->xmin); 
			y0 = MAX2 (box[i].ymin,amb->ymin);
			x1 = MIN2 (box[i].xmax,amb->xmax); 
			y1 = MIN2 (box[i].ymax,amb->ymax);
			if (x1 > x0 + 0.01*ax && y1 > y0 + 0.01*ay)
			{
				a = (x1 - x0)*(y1 - y0);
				A = (box[i].xmax - box[i].xmin)*(box[i].ymax - box[i].ymin);
				if  (a >= r*A)
				{
					uu_list_push (cxlst,&box[i]);
					uu_list_delete (bxlst,i,1);
					i--; n--;
					box = (VW_2box *) UU_LIST_ARRAY (bxlst);
				}
			}
		}
	}

	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_check_box_ins (boxa,boxb,tol)
**       Checks if one of two boxes is inside another
**    PARAMETERS
**       INPUT  :
**          boxa, boxb - the 2D-boxes
**          tol 
**       OUTPUT :
**          none
**    RETURNS      :  1 - A is inside B, 
**                   -1 - B is inside A
**                    0 - neither
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_check_box_ins (boxa,boxb,tol)
VW_2box *boxa,*boxb;
UU_REAL tol;
{
	UU_REAL dx0,dx1,dy0,dy1;

	dx0 = boxa->xmin - boxb->xmin;
	dx1 = boxa->xmax - boxb->xmax;
	dy0 = boxa->ymin - boxb->ymin;
	dy1 = boxa->ymax - boxb->ymax;

	if (dx0 > -tol && dx1 < tol && dy0 > -tol && dy1 < tol) return (1);
	if (dx1 > -tol && dx0 < tol && dy1 > -tol && dy0 < tol) return (-1);
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_isect_boxes0 (boxa,boxb,tol)
**       Checks if one of two boxes intersect
**    PARAMETERS
**       INPUT  :
**          boxa, boxb - the 2D-boxes
**          tol 
**       OUTPUT :
**          none
**    RETURNS      :  1 - A intersects B, 
**                    0 - else
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_isect_boxes0 (boxa,boxb,tol)
VW_2box *boxa,*boxb;
UU_REAL tol;
{
	UU_REAL xxa[2],yya[2],xxb[2],yyb[2];

	xxa[0] = boxa->xmin; xxa[1] = boxa->xmax;
	xxb[0] = boxb->xmin; xxb[1] = boxb->xmax;

	yya[0] = boxa->ymin; yya[1] = boxa->ymax;
	yyb[0] = boxb->ymin; yyb[1] = boxb->ymax;

	return (um_isect_boxes (xxa,yya,xxb,yyb,tol));
}

/*********************************************************************
*********************************************************************/
static UU_REAL ncl_boxes_area (bxlst,ax,ay)
UU_LIST *bxlst;
UU_REAL ax,ay;
{
	int i,n;
	VW_2box *box;
	UU_REAL A,dx,dy,area;

	n = bxlst->cur_cnt;
	box = (VW_2box *) UU_LIST_ARRAY (bxlst);

	A = ax*ay;
	if (A < 0.0001)
	{
		area = 1.; return (area);
	}

	for (i = 0, area = 0; i < n; i++, box++)
	{
		dx = box->xmax - box->xmin;
		dy = box->ymax - box->ymin;

		area += (dx*dy); 
	}
	return (area/A);
}

/*********************************************************************
*********************************************************************/
static int ncl_small_shifts (bxlst,amb,ax,ay,gap)
UU_LIST *bxlst;
VW_2box *amb;
UU_REAL ax,ay,gap;
{
	int i,j,k,n;
	VW_2box *box;
	UU_REAL d,c0,c1,mgap,mgap0,tol;
	UU_LOGICAL llm,lrm,lbm,ltm,lambx0,lambx1,lamby0,lamby1;


	if (gap < 0.000001)
	{
		uu_list_push_list (bxlst,&dxlst);
		return (0);
	}

	n = bxlst->cur_cnt;
	box = (VW_2box *) UU_LIST_ARRAY (bxlst);

	tol = 0.08;
	for (i = 0; i < n; i++)
	{
		if (fabs(box[i].xmin - amb->xmin) < tol)
			box[i].xmin = amb->xmin;
	}
	for (i = 0; i < n; i++)
	{
		if (fabs(box[i].ymin - amb->ymin) < tol)
			box[i].ymin = amb->ymin;
	}
	for (i = 0; i < n; i++)
	{
		if (fabs(box[i].xmax - amb->xmax) < tol)
			box[i].xmax = amb->xmax;
	}
	for (i = 0; i < n; i++)
	{
		if (fabs(box[i].ymax - amb->ymax) < tol)
			box[i].ymax = amb->ymax;
	}

/*
..... adjust matching pairs until maximal gap is small
*/
	tol = 0.01;
	mgap0 = 1.;
	for (k = 0; k < 10; k++)
	{
		mgap = 0.;

		for (i = 0; i < n; i++)
		{
			llm = lambx0 = (fabs(box[i].xmin - amb->xmin) < tol);
			lrm = lambx1 = (fabs(box[i].xmax - amb->xmax) < tol);
			lbm = lamby0 = (fabs(box[i].ymin - amb->ymin) < tol);
			ltm = lamby1 = (fabs(box[i].ymax - amb->ymax) < tol);

			for (j = 0; j < n; j++)
			{
				if (j == i) continue;

				c0 = MAX2 (box[i].ymin,box[j].ymin);
				c1 = MIN2 (box[i].ymax,box[j].ymax);
				if (!lambx0)
				{
/*
..... match and maybe adjust the i-th box on the left
*/
					d = fabs(box[j].xmax - box[i].xmin);
					if (d < tol && c1 >= c0 + 4.*tol)
					{
						llm = UU_TRUE;
						if (d > mgap) mgap = d;
						if (d > 0)
						{
							d = (box[j].xmax + box[i].xmin)/2.;
							box[j].xmax = box[i].xmin = d;
						}
					}
				}
				if (!lambx1)
				{
/*
..... match and maybe adjust the i-th box on the right
*/
					d = fabs(box[j].xmin - box[i].xmax);
					if (d < tol && c1 >= c0 + 4.*tol)
					{
						lrm = UU_TRUE;
						if (d > mgap) mgap = d;
						if (d > 0)
						{
							d = (box[i].xmax + box[j].xmin)/2.;
							box[j].xmin = box[i].xmax = d;
						}
					}
				}

				c0 = MAX2 (box[i].xmin,box[j].xmin);
				c1 = MIN2 (box[i].xmax,box[j].xmax);
				if (!lamby0)
				{
/*
..... match and maybe adjust the i-th box on the bottom
*/
					d = fabs(box[j].ymax - box[i].ymin);
					if (d < tol && c1 >= c0 + 4.*tol)
					{
						lbm = UU_TRUE;
						if (d > mgap) mgap = d;
						if (d > 0)
						{
							d = (box[j].ymax + box[i].ymin)/2.;
							box[j].ymax = box[i].ymin = d;
						}
					}
				}
				if (!lamby1)
				{
/*
..... match and maybe adjust the i-th box on the top
*/
					d = fabs(box[j].ymin - box[i].ymax);
					if (d < tol && c1 >= c0 + 4.*tol)
					{
						ltm = UU_TRUE;
						if (d > mgap) mgap = d;
						if (d > 0)
						{
							d = (box[i].ymax + box[j].ymin)/2.;
							box[j].ymin = box[i].ymax = d;
						}
					}
				}
			}
			if (!llm || !lrm || !lbm || !ltm) return (-1);
		}

		if (mgap < 0.0001 || (k > 0 && mgap > mgap0)) break;
		mgap0 = mgap;
	}

	if (mgap < 0.008)
	{
		uu_list_push_list (&dxlst,bxlst);
		return (0);
	}

	return (-1);
}

/*********************************************************************
*********************************************************************/
static int ncl_expand_right (bxlst,amb,ax,ay,bxi,ishape)
UU_LIST *bxlst;
VW_2box *amb,*bxi;
UU_REAL ax,ay;
int *ishape;
{
	int i,n,found,iret;
	VW_2box *box;
	UU_REAL dx,dy,tol;

	tol = 0.08;

	iret = 0;
	found = 1;
	while (found && iret != WHOLE)
	{
		n = bxlst->cur_cnt;
		box = (VW_2box *) UU_LIST_ARRAY (bxlst);
		for (i = found = 0; i < n && found == 0; i++)
		{
			if (fabs(bxi->xmax - box[i].xmin) < tol &&
				fabs(bxi->ymax - box[i].ymax) < tol &&
				fabs(bxi->ymin - box[i].ymin) < tol)
			{
				found = 1;
				if (fabs(amb->xmax - box[i].xmax) < tol)
				{
					box[i].xmax = amb->xmax;
					iret = WHOLE;
				}
				else
					iret = 1;
/*
..... adjust the matching boxes to each other
*/
				dx = (bxi->xmax + box[i].xmin)/2;
				bxi->xmax = box[i].xmin = dx;
/*
..... add the old expanded box bxi to the final list, and replace it
..... by the extending box
*/
				ncl_add_box (bxi,amb);
				bxi->xmin = dx;
				bxi->xmax = box[i].xmax;
				bxi->num = box[i].num;
				uu_list_delete (bxlst,i,1);
			}
		}
	}
	if (iret >= 1)
	{
		dx = (bxi->xmax - amb->xmin)/ax;
		dy = (bxi->ymax - amb->ymin)/ay;
		if (dx > dy)
			*ishape = HORZ;
		else if (dx < dy)
			*ishape = VERT;
	}
	return (iret);
}

/*********************************************************************
*********************************************************************/
static int ncl_expand_up (bxlst,amb,ax,ay,bxi,ishape)
UU_LIST *bxlst;
VW_2box *amb,*bxi;
UU_REAL ax,ay;
int *ishape;
{
	int i,n,found,iret;
	VW_2box *box;
	UU_REAL dx,dy,tol;

	tol = 0.08;

	iret = 0;
	found = 1;
	while (found && iret != WHOLE)
	{
		n = bxlst->cur_cnt;
		box = (VW_2box *) UU_LIST_ARRAY (bxlst);
		for (i = found = 0; i < n && found == 0; i++)
		{
			if (fabs(bxi->ymax - box[i].ymin) < tol &&
				fabs(bxi->xmax - box[i].xmax) < tol &&
				fabs(bxi->xmin - box[i].xmin) < tol)
			{
				found = 1;
				if (fabs(amb->ymax - box[i].ymax) < tol)
				{
					box[i].ymax = amb->ymax;
					iret = WHOLE;
				}
				else
					iret = 1;
/*
..... adjust the matching boxes to each other
*/
				dy = (bxi->ymax + box[i].ymin)/2;
				bxi->ymax = box[i].ymin = dy;
/*
..... add the old expanded box bxi to the final list, and replace it
..... by the extending box
*/
				ncl_add_box (bxi,amb);
				bxi->ymin = dy;
				bxi->ymax = box[i].ymax;
				bxi->num = box[i].num;
				uu_list_delete (bxlst,i,1);
			}
		}
	}
	if (iret >= 1)
	{
		dx = (bxi->xmax - amb->xmin)/ax;
		dy = (bxi->ymax - amb->ymin)/ay;
		if (dx > dy)
			*ishape = HORZ;
		else if (dx < dy)
			*ishape = VERT;
	}
	return (iret);
}

/*********************************************************************
*********************************************************************/
static void ncl_resize1 (bxlst,amb,ax,ay)
UU_LIST *bxlst;
VW_2box *amb;
UU_REAL ax,ay;
{
	int i,j,n,iret,ishape;
	VW_2box *box,bxi,amb1;
	UU_REAL a0,a1,b0,b1,ax1,ay1,area,gap,dx,dy,tol;
	UU_LIST cxlst;

	tol = 0.01;
	n = bxlst->cur_cnt;
	box = (VW_2box *) UU_LIST_ARRAY (bxlst);
	uu_list_init (&cxlst,sizeof(VW_2box),0,10);

	for (i = 0; i < n; i++)
	{
		iret = ncl_isect_boxes0 (&box[i],amb,tol);
		if (iret > 0)
		{
			box[i].xmin = MAX2 (box[i].xmin,amb->xmin);
			box[i].ymin = MAX2 (box[i].ymin,amb->ymin);
			box[i].xmax = MIN2 (box[i].xmax,amb->xmax);
			box[i].ymax = MIN2 (box[i].ymax,amb->ymax);
			dx = box[i].xmax - box[i].xmin;
			dy = box[i].ymax - box[i].ymin;
			if (dx < 0.08 || dy < 0.08) iret = 0;
		}
		if (iret == 0)
		{
			ncl_remove_box (bxlst,i);
			i--; n--;
			box = (VW_2box *) UU_LIST_ARRAY (bxlst);
		}
	}
	if (n < 1) return;

	if (n == 1)
	{
		box[0].xmin = amb->xmin;
		box[0].ymin = amb->ymin;
		box[0].xmax = amb->xmax;
		box[0].ymax = amb->ymax;
		ncl_add_box (&box[0],amb);
	}
	else if (n == 2)
	{
		i = ncl_find_ll (box,n,amb,ax,ay);
		j = 1-i;
		box[i].xmin = amb->xmin;
		box[i].ymin = amb->ymin;

		iret = ncl_check_box_ins(&box[j],&box[i],tol);
		if (iret == 1)
		{
			ncl_remove_box (bxlst,j);
			ncl_resize1 (bxlst,amb,ax,ay);
		}
		else if (iret == -1)
		{
			ncl_remove_box (bxlst,i);
			ncl_resize1 (bxlst,amb,ax,ay);
		}
		else
		{
			amb1.xmax = amb->xmax;
			amb1.ymax = amb->ymax;

			if ((box[i].xmax >= box[j].xmax) || (box[i].ymax < box[j].ymax &&
				(box[i].xmax - amb->xmin)/ax >= (box[i].ymax - amb->ymin)/ay))
			{
	/* horizontal */
				if (box[i].ymax < box[j].ymin)
					b1 = (box[i].ymax + box[j].ymin)/2.;
				else
					b1 = box[i].ymax;
				box[i].xmax = amb->xmax;
				box[i].ymax = b1;
				amb1.xmin = amb->xmin;
				amb1.ymin = b1;
				ax1 = ax;
				ay1 = amb1.ymax - amb1.ymin;
				amb->ymax = b1;
				ay = amb->ymax - amb->ymin; 
			}
			else
			{
	/* vertical */
				if (box[i].xmax < box[j].xmin)
					a1 = (box[i].xmax + box[j].xmin)/2.;
				else
					a1 = box[i].xmax;
				box[i].xmax = a1;
				box[i].ymax = amb->ymax;
				amb1.xmin = a1;
				amb1.ymin = amb->ymin;
				ay1 = ay;
				ax1 = amb1.xmax - amb1.xmin; 
				amb->xmax = a1;
				ax = amb->xmax - amb->xmin; 
			}

			ncl_add_box (&box[i],amb);
			uu_list_delete (bxlst,i,1);
			ncl_resize1 (bxlst,&amb1,ax1,ay1);
		}
	}
	else if (n > 2)
	{
		iret = ncl_long_box (bxlst,&cxlst,amb,ax,ay,&bxi);
		box = (VW_2box *) UU_LIST_ARRAY (bxlst);
		if (iret == HORZ)
		{
			a0 = amb->ymin;
			a1 = bxi.ymin;
			b0 = bxi.ymax;
			b1 = amb->ymax;

			ncl_add_box (&bxi,amb);

			if (cxlst.cur_cnt > 0)
			{
				amb->ymin = a0; amb->ymax = a1;
				ay = amb->ymax - amb->ymin; 
				ncl_resize1 (&cxlst,amb,ax,ay);
			}

			amb->ymin = b0; amb->ymax = b1;
			ay = amb->ymax - amb->ymin; 
			ncl_resize1 (bxlst,amb,ax,ay);
		}
		else if (iret == VERT)
		{
			a0 = amb->xmin;
			a1 = bxi.xmin;
			b0 = bxi.xmax;
			b1 = amb->xmax;

			ncl_add_box (&bxi,amb);

			if (cxlst.cur_cnt > 0)
			{
				amb->xmin = a0; amb->xmax = a1;
				ax = amb->xmax - amb->xmin; 
				ncl_resize1 (&cxlst,amb,ax,ay);
			}

			amb->xmin = b0; amb->xmax = b1;
			ax = amb->xmax - amb->xmin;
			ncl_resize1 (bxlst,amb,ax,ay);
		}
		else if (iret == WHOLE)
		{
/* one box covers everything, the rest are removed from bxlst */
			ncl_add_box (&bxi,amb);
		}
		else
		{
			area = ncl_boxes_area (bxlst,ax,ay);
			gap = fabs(1.-area)/n;
			if (gap < 0.004)
			{
				iret = ncl_small_shifts (bxlst,amb,ax,ay,gap);
				if (iret == 0) return;
			}

			i = ncl_find_ll (box,n,amb,ax,ay);
			bxi.xmax = a0 = box[i].xmax;
			bxi.ymax = b0 = box[i].ymax;
			bxi.xmin = amb->xmin;
			bxi.ymin = amb->ymin;
			bxi.num = box[i].num;
			uu_list_delete (bxlst,i,1);

			ishape = VERT;
			if ((a0 - amb->xmin)/ax >= (b0 - amb->ymin)/ay) ishape = HORZ;
			iret = 0;
			if (ishape == HORZ)
			{
				iret = ncl_expand_right (bxlst,amb,ax,ay,&bxi,&ishape);
				if (iret == 0)
					iret = ncl_expand_up (bxlst,amb,ax,ay,&bxi,&ishape);
			}
			else
			{
				iret = ncl_expand_up (bxlst,amb,ax,ay,&bxi,&ishape);
				if (iret == 0)
					iret = ncl_expand_right (bxlst,amb,ax,ay,&bxi,&ishape);
			}
			a0 = bxi.xmax;
			b0 = bxi.ymax;

			if (ishape == HORZ)
			{
				if (iret != WHOLE)
				{
					amb1.xmin = a0;
					amb1.xmax = amb->xmax;
					amb1.ymin = amb->ymin;
					amb1.ymax = b0;
					ax1 = amb1.xmax - amb1.xmin; 
					ay1 = amb1.ymax - amb1.ymin;
					ncl_resolve(bxlst,&cxlst,&amb1,ax1,ay1);
					if (cxlst.cur_cnt > 0)
					{
						ncl_add_box (&bxi,amb);
						ncl_resize1(&cxlst,&amb1,ax1,ay1);
					}
				}
				else
				{
					bxi.xmax = amb->xmax;
					ncl_add_box (&bxi,amb);
				}

				amb->ymin = b0;
				ay = amb->ymax - amb->ymin; 
				ncl_resize1 (bxlst,amb,ax,ay);
			}
			else
			{
				if (iret != WHOLE)
				{
					amb1.xmin = amb->xmin;
					amb1.xmax = a0;
					amb1.ymin = b0;
					amb1.ymax = amb->ymax;
					ax1 = amb1.xmax - amb1.xmin; 
					ay1 = amb1.ymax - amb1.ymin;
					ncl_resolve(bxlst,&cxlst,&amb1,ax1,ay1);
					if (cxlst.cur_cnt > 0)
					{
						ncl_add_box (&bxi,amb);
						ncl_resize1(&cxlst,&amb1,ax1,ay1);
					}
				}
				else
				{
					bxi.ymax = amb->ymax;
					ncl_add_box (&bxi,amb);
				}

				amb->xmin = a0;
				ax = amb->xmax - amb->xmin; 
				ncl_resize1 (bxlst,amb,ax,ay);
			}
		}
	}
	uu_list_free (&cxlst);

}

/*********************************************************************
**    E_FUNCTION: ncl_pokcmp(e1,e2)
**       Comparison routine for the sort algorithm (uu_qsort).  
**    PARAMETERS
**       INPUT  :
**          e1     - first element to be compared 
**          e2     - second element
**       OUTPUT :
**    RETURNS      :  -1 if e1 < e2
**                     0 if e1 = e2
**                     1 if e1 > e2
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_vbxcmp(e1,e2)
VW_2box *e1,*e2;
{
	if (e1->num > e2->num) return(1);
	else if (e1->num < e2->num) return(-1);

	return(0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_fix_boxes (llf,urf,nf)
**       Rearrange 2D-boxes
**    PARAMETERS   
**       INPUT  : 
**          llf - lower-left corners
**          urf - upper-right corners
**          nf  - number of (active) boxes
**       OUTPUT :  
**          llf - lower-left corners
**          urf - upper-right corners
**          nf  - number of (active) boxes
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_fix_boxes (llf,urf,nf)
UU_REAL llf[][2], urf[][2];
int nf;
{
	UU_LIST bxlst;
	UU_REAL ax,ay,tol;
	VW_2box box;
	VW_2box *bx;
	int i,n;
	int ncl_vbxcmp();

	uu_list_init (&bxlst,sizeof(VW_2box),10,10);
	uu_list_init (&dxlst,sizeof(VW_2box),10,10);

	n = nf;
	ax = ay = 1;
	tol = 0.01;

	for (i = 0; i < n; i++)
	{
		box.xmin = MAX2(llf[i][0],0);
		box.ymin = MAX2(llf[i][1],0);
		box.xmax = MIN2(urf[i][0],1);
		box.ymax = MIN2(urf[i][1],1);
		box.num = i;
		if (box.xmax < box.xmin+tol || box.ymax < box.ymin+tol)
		{
			box.xmin = box.ymin = box.xmax = box.ymax = 0;
			uu_list_push (&dxlst,&box);
		}
		else
			uu_list_push (&bxlst,&box);
	}

	box.xmin = 0; box.ymin = 0; 
	box.xmax = 1; box.ymax = 1; 

	ncl_resize1 (&bxlst,&box,ax,ay);

	uu_list_sort (&dxlst,ncl_vbxcmp);
	n = dxlst.cur_cnt;
	if (n > 0)
	{
		bx = (VW_2box *) UU_LIST_ARRAY (&dxlst);
		for (i = 0; i < n; i++)
		{
			llf[i][0] = bx[i].xmin;
			llf[i][1] = bx[i].ymin;
			urf[i][0] = bx[i].xmax;
			urf[i][1] = bx[i].ymax;
		}
	}

	uu_list_free (&bxlst);
	uu_list_free (&dxlst);
}
