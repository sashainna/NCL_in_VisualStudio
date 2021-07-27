/*********************************************************************
**    NAME         : nebucket.c
**       Funtions to bucket the triangles and segments.
**
**			 ncl_coord_equal()
**			 ncl_trian_equal()
**			 ncl_segment_equal()
**			 ncl_points_contain_point()
**			 ncl_ptlist_contain_point()
**			 ncl_sfbucket_init()
**		     ncl_sfbucket_contain_trian()
**			 ncl_sfbucket_add_trian()
**			 ncl_sfbucket_create()
**			 ncl_sfbucket_free()
**			 ncl_sfbucket_getbox_trianlst()
**			 ncl_sfbucket_getpt_trianlst()
**			 ncl_sgbucket_init()
**		     ncl_sgbucket_contain_seg()
**			 ncl_sgbucket_add_seg()
**			 ncl_sgbucket_create()
**			 ncl_sgbucket_free()
**			 ncl_sgbucket_getbox_seglst()
**
**    COPYRIGHT 2010 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nebucket.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:25
*********************************************************************/
#include "nccs.h"
#include "mgeom.h"
#include "uminmax.h"
#include "nclwaterln.h"

typedef struct
{
	int		nBuckets;      /*number of bucket*/
	int		nCols;         /*number of cols*/
	UU_LIST grid;  	       /*bucket list*/
	UU_REAL cellSize;      /*bucket size*/
	UU_REAL xmin;          /*bottom left x*/
	UU_REAL ymin;          /*bottom left y*/
} ncl_Buckets;

static ncl_Buckets SFBuckets; /*triangles buckets*/
static ncl_Buckets SGBuckets; /*segment buckets*/

/**********************************************************************
**    E_FUNCTION     : floor_int(val)
**       Get the integer value of input real value.
**    PARAMETERS
**       INPUT  :
**          val		- real value
**       OUTPUT :
**          none
**    RETURNS      :
**       integer value 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
static int floor_int(val)
UU_REAL val;
{    
	int i = (int)val;    
	return (val < 0.0f && val != i ? i - 1 : i); 
}   

/**********************************************************************
**    E_FUNCTION     : ncl_coord_equal(pt1,pt2)
**       Compare two points if equal or not.
**    PARAMETERS
**       INPUT  :
**          pt1,pt2	- points to compare
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS if equal, UU_FAILURE if not
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
static UU_LOGICAL ncl_coord_equal(pt1,pt2)
UM_coord pt1,pt2;
{
	return (UM_SQDIS(pt1,pt2) < UM_DFUZZ * UM_DFUZZ);
}

/**********************************************************************
**    E_FUNCTION     : ncl_trian_equal(tri1,tri2)
**       Compare two traingles if equal or not.
**    PARAMETERS
**       INPUT  :
**          tri1,tri2	- traingles to compare
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS if equal, UU_FAILURE if not
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
static UU_LOGICAL ncl_trian_equal(tri1,tri2)
UM_trian *tri1,*tri2;
{
	return (ncl_coord_equal(tri1->p1,tri2->p1) && 
		    ncl_coord_equal(tri1->p2,tri2->p2) &&
		    ncl_coord_equal(tri1->p3,tri2->p3));
}

/**********************************************************************
**    E_FUNCTION     : ncl_segment_equal(seg1,seg2)
**       Compare two segments if equal or not.
**    PARAMETERS
**       INPUT  :
**          tri1,tri2	- traingles to compare
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS if equal, UU_FAILURE if not
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
static UU_LOGICAL ncl_segment_equal(seg1,seg2)
UM_segment *seg1,*seg2;
{
	return (ncl_coord_equal(seg1->p1,seg2->p1) &&
		    ncl_coord_equal(seg1->p2,seg2->p2)) ||
		   (ncl_coord_equal(seg1->p1,seg2->p2) &&
		    ncl_coord_equal(seg1->p2,seg2->p1));
}

/**********************************************************************
**    E_FUNCTION     : ncl_points_contain_point(npts,pts,pt)
**       Check if point(pt) is contained in points(pts).
**    PARAMETERS
**       INPUT  :
**          npts	- number of points
**          npts	- the points
**			pt		- the point to check
**       OUTPUT :
**          none
**    RETURNS      :
**          UU_SUCCESS if equal, UU_FAILURE if not
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
UU_LOGICAL ncl_points_contain_point(npts,pts,pt)
int npts;
UM_coord pts[],pt;
{
	int i;
	for (i = 0; i < npts; i++)
		if (ncl_coord_equal(pts[i],pt))
			return UU_TRUE;

	return UU_FALSE;
}

/**********************************************************************
**    E_FUNCTION     : ncl_ptlist_contain_point(ptlist,pt)
**       Check if point(pt) is contained in points list(ptlist).
**    PARAMETERS
**       INPUT  :
**          ptlist	- the points list
**			pt		- the point to check
**       OUTPUT :
**          none
**    RETURNS      :
**          UU_SUCCESS if equal, UU_FAILURE if not
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
UU_LOGICAL ncl_ptlist_contain_point(ptlist,pt)
UU_LIST *ptlist;
UM_coord pt;
{
	int i,npts;
	UM_coord *pts;

	npts = UU_LIST_LENGTH(ptlist);
	pts = (UM_coord*) UU_LIST_ARRAY(ptlist);
	for (i = npts-1; i >= 1; i--)
	{
		if (ncl_coord_equal(pts[i],pt))
			return UU_TRUE;
	}

	return UU_FALSE;
}

/**********************************************************************
**    E_FUNCTION     : ncl_sfbucket_init(box,size)
**       Allocate memory and initialize lists used for triangles Buckets.
**    PARAMETERS
**       INPUT  :
**          box		- bounding box
**			size	- bucket size
**       OUTPUT :
**          none
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_sfbucket_init(box,size)
UM_2box box;
UU_REAL size;
{
	int i,j,nBuckets,nCols,nRows;
	UU_LIST tesslst;

	nCols = (box.xmax - box.xmin)/size;
	nRows = (box.ymax - box.ymin)/size;
	nBuckets = nCols * nRows;

	SFBuckets.nBuckets = nBuckets;
	SFBuckets.nCols = nCols;
	SFBuckets.cellSize = size;
	SFBuckets.xmin = box.xmin;
	SFBuckets.ymin = box.ymin;
	uu_list_init(&SFBuckets.grid, sizeof(UU_LIST), nBuckets+1, nBuckets+1);

	for (i = 0; i < nBuckets; i++)
	{
	   uu_list_init(&tesslst, sizeof(UM_trian), 100, 100);
	   uu_list_push(&SFBuckets.grid, &tesslst);
	}
}

/**********************************************************************
**    E_FUNCTION     : ncl_sfbucket_contain_trian(trilst, trian)
**       Check if trian is contained in the bucket with trilst.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
UU_LOGICAL ncl_sfbucket_contain_trian(trilst,trian)
UU_LIST *trilst;
UM_trian *trian;
{
	int i,npts;
	UM_trian *ptri;
		
	npts = trilst->cur_cnt;
    ptri = (UM_trian*) UU_LIST_ARRAY (trilst);
	for (i = 0; i < npts; i++,ptri++)
	{
		if (ncl_trian_equal(ptri,trian))
			return UU_TRUE;
	}

	return UU_FALSE;
}

/**********************************************************************
**    E_FUNCTION     : ncl_sfbucket_add_trian(trian)
**       Added trian to the sfbuckets
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_sfbucket_add_trian(trian)
UM_trian *trian;
{
	int i,j,isX,ieX,isY,ieY,ihash,nhigh,nwidth;
	UU_REAL cellsize;
	UU_LIST trianlst;
	UU_LIST *grid;
	
	nwidth = SFBuckets.nCols;
	nhigh = SFBuckets.nBuckets / nwidth;
	cellsize = SFBuckets.cellSize;

	isX = (int) floor_int((MIN3(trian->p1[0],trian->p2[0],trian->p3[0]) -
						   SFBuckets.xmin)/cellsize);
	if (isX < 0) isX = 0;
	if (isX >= nwidth) isX = nwidth - 1;
	ieX = (int) floor_int((MAX3(trian->p1[0],trian->p2[0],trian->p3[0])-
						   SFBuckets.xmin)/cellsize);
	if (ieX < 0) ieX = 0;
	if (ieX >= nwidth) ieX = nwidth - 1;
	isY = (int) floor_int((MIN3(trian->p1[1],trian->p2[1],trian->p3[1])-
						   SFBuckets.ymin)/cellsize);
	if (isY < 0) isY = 0;
	if (isY >= nhigh) isY = nhigh - 1;
	ieY = (int) floor_int((MAX3(trian->p1[1],trian->p2[1],trian->p3[1])-
							SFBuckets.ymin)/cellsize);
	if (ieY < 0) ieY = 0;
	if (ieY >= nhigh) 	
		ieY = nhigh - 1;

	grid = (UU_LIST *) UU_LIST_ARRAY (&SFBuckets.grid);			

	for (i = isX; i <= ieX; ++i)
	{
		for (j = isY; j <= ieY; ++j)
		{
			ihash = i + j * nwidth;		
/*
.....Check if trian already contained in grid[ihash]
*/
			if (grid[ihash].cur_cnt > 0)
			{
				if (ncl_sfbucket_contain_trian(&grid[ihash], trian))
					continue;
			}
	        uu_list_push(&grid[ihash], trian);
		}
	}
}

/**********************************************************************
**    E_FUNCTION     : ncl_sfbucket_create(sfnum,sff,bbox,size)
**       create surface tess bucket
**    PARAMETERS
**       INPUT  :
**          sfnum	-surfaces number
**			sff		-surfaces data
**			box		-bounding box
**			size	-bucket size
**       OUTPUT :
**          none
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_sfbucket_create(sfnum,sff,bbox,size)
int sfnum;
NCL_waterline_surf *sff;
UM_2box bbox;
UU_REAL size;
{
	int i,j,npts;
	UM_trian *ptri;
/*
.....Initialize bucket
*/
	ncl_sfbucket_init(bbox,size);
/*
.....Put sff tess tianglist into buckets
.......Added check for null key so invalid data was not accessed.
.......Some surface records will not be initialized by previous
.......routines if they can't be tesselated - ASF 7/12/13.
*/
	for (i = 0; i < sfnum; i++)
	{
		if (sff[i].key == 0 || sff[i].trianlist == UU_NULL) continue;
		npts = sff[i].trianlist->cur_cnt;
		ptri = (UM_trian *)UU_LIST_ARRAY(sff[i].trianlist);
		for (j = 0; j < npts; j++, ptri++)
			ncl_sfbucket_add_trian(ptri);
	}
}

/**********************************************************************
**    E_FUNCTION     :  ncl_sfbucket_free()
**       Free allocate memory and initialize lists used for Buckets..
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_sfbucket_free()
{
	int i,nBuckets;
	UU_LIST *grid;
		
	nBuckets = SFBuckets.nBuckets;
	grid = (UU_LIST *) UU_LIST_ARRAY (&SFBuckets.grid);		

	for (i = 0; i < nBuckets; i++)
	   uu_list_free (&grid[i]);
}

/**********************************************************************
**    E_FUNCTION     : ncl_bucket_getbox_trianlst(bbox,trianlst)
**       Get the tessellation trangles given the bounding box
**    PARAMETERS
**       INPUT  :
**			bbox		- bounding box
**       OUTPUT :
**          trianlst	- pointer to the point triangles list 
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_sfbucket_getbox_trianlst(bbox,trianlst,ldel)
UM_2box bbox;
UU_LIST *trianlst;
UU_LOGICAL ldel;
{
	int i,j,k,ihash,isX,ieX,isY,ieY,ncnt,nhigh,nwidth,status;
	UU_REAL xmin,ymin,cellsize;
	UU_LIST *grid,containlst;
	UM_trian *ptri;

	nwidth = SFBuckets.nCols;
	nhigh = SFBuckets.nBuckets / nwidth;
	cellsize = SFBuckets.cellSize;
	xmin = SFBuckets.xmin;
	ymin = SFBuckets.ymin;

	isX = (int) floor_int((bbox.xmin - xmin)/cellsize);
	if (isX < 0) isX = 0;
	if (isX >= nwidth) isX = nwidth - 1;
	ieX = (int) floor_int((bbox.xmax - xmin)/cellsize);
	if (ieX < 0) ieX = 0;
	if (ieX >= nwidth) ieX = nwidth - 1;
	isY = (int) floor_int((bbox.ymin - ymin)/cellsize);
	if (isY < 0) isY = 0;
	if (isY >= nhigh) isY = nhigh - 1;
	ieY = (int) floor_int((bbox.ymax - ymin)/cellsize);
	if (ieY < 0) ieY = 0;
	if (ieY >= nhigh) ieY = nhigh - 1;

	grid = (UU_LIST *) UU_LIST_ARRAY (&SFBuckets.grid);	

	for (i = isX; i <= ieX; ++i)
	{
		for (j = isY; j <= ieY; ++j)
		{
			ihash = i + j * nwidth;
			ncnt = grid[ihash].cur_cnt;
			if (ncnt > 0)
				uu_list_push_list (trianlst, &grid[ihash]);	
		}
	}

/*
.....Delete duplicate triangles
*/
	if (ldel)
	{
		ncnt = trianlst->cur_cnt;
		if (ncnt > 0)
		{
			uu_list_init(&containlst, sizeof(UM_trian), ncnt, ncnt);
			uu_list_push_list (&containlst, trianlst);
			UU_LIST_EMPTY (trianlst);

			ptri = (UM_trian *) UU_LIST_ARRAY (&containlst);
			for (k = 0; k < ncnt; k++,ptri++)
			{
				if (trianlst->cur_cnt > 0 && ncl_sfbucket_contain_trian(trianlst, ptri))
					continue;
				uu_list_push(trianlst, ptri);
			}

			if (containlst.data)
				uu_list_free(&containlst);
		}
	}
}

/**********************************************************************
**    E_FUNCTION     : ncl_bucket_getpt_trianlst(pt,radius,trianlst)
**       Get the tessellation trangles given point and radius
**    PARAMETERS
**       INPUT  :
**			pt			- point
**			radius		-radius
**       OUTPUT :
**          trianlst	- pointer to the point triangles list 
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_sfbucket_getpt_trianlst(pt,radius,trianlst,ldel)
UM_coord pt;
UU_REAL radius;
UU_LIST *trianlst;
UU_LOGICAL ldel;
{
	UM_2box bbox;
	bbox.xmin = pt[0] - radius;
	bbox.xmax = pt[0] + radius;
	bbox.ymin = pt[1] - radius;
	bbox.ymax = pt[1] + radius;

	ncl_sfbucket_getbox_trianlst(bbox,trianlst,ldel);
}

/**********************************************************************
**    E_FUNCTION     : ncl_sgbucket_init(box,cellsize)
**       Allocate memory and initialize lists used for segment Buckets.
**    PARAMETERS
**       INPUT  :
**          box		- bounding box
**			size	- bucket size
**       OUTPUT :
**          none
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_sgbucket_init(box,size)
UM_2box box;
UU_REAL size;
{
	int i,j,nBuckets,nCols,nRows;
	UU_LIST seglst;

	nCols = ceil((box.xmax - box.xmin)/size) + 1;
	nRows = ceil((box.ymax - box.ymin)/size) + 1;
	nBuckets = nCols * nRows;

	SGBuckets.nBuckets = nBuckets;
	SGBuckets.nCols = nCols;
	SGBuckets.cellSize = size;
	SGBuckets.xmin = box.xmin;
	SGBuckets.ymin = box.ymin;
	uu_list_init(&SGBuckets.grid, sizeof(UU_LIST), nBuckets+1, nBuckets+1);

	for (i = 0; i < nBuckets; i++)
	{
	   uu_list_init(&seglst, sizeof(UM_segment), 100, 100);
	   uu_list_push(&SGBuckets.grid, &seglst);
	}
}

/**********************************************************************
**    E_FUNCTION     : ncl_sgbucket_add_seg(seg)
**       Added segment to the sgbucket
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_sgbucket_add_seg(seg)
UM_segment *seg;
{
	int i,j,isX,ieX,isY,ieY,ihash,nhigh,nwidth;
	UU_REAL size;
	UU_LIST trianlst;
	UU_LIST *grid;
	
	nwidth = SGBuckets.nCols;
	nhigh = SGBuckets.nBuckets / nwidth;
	size = SGBuckets.cellSize;

	isX = (int)floor_int((MIN2(seg->p1[0],seg->p2[0])-SGBuckets.xmin)/size);
	if (isX < 0) isX = 0;
	if (isX >= nwidth) isX = nwidth - 1;
	ieX = (int)floor_int((MAX2(seg->p1[0],seg->p2[0])-SGBuckets.xmin)/size);	
	if (ieX < isX) ieX = isX;
	if (ieX >= nwidth) ieX = nwidth - 1;
	isY = (int)floor_int((MIN2(seg->p1[1],seg->p2[1])-SGBuckets.ymin)/size);
	if (isY < 0) isY = 0;
	if (isY >= nhigh) isY = nhigh - 1;
	ieY = (int)floor_int((MAX2(seg->p1[1],seg->p2[1])-SGBuckets.ymin)/size);
	if (ieY < isY) ieY = isY;
	if (ieY >= nhigh) ieY = nhigh - 1;

	grid = (UU_LIST *) UU_LIST_ARRAY (&SGBuckets.grid);			

	for (i = isX; i <= ieX; ++i)
	{
		for (j = isY; j <= ieY; ++j)
		{
			ihash = i + j * nwidth;		
/*
.....Check if the seg already contained in grid[ihash]
*/
			if (grid[ihash].cur_cnt > 0)
			{
				if (ncl_sgbucket_contain_seg(&grid[ihash], seg))
					continue;
			}
	        uu_list_push(&grid[ihash], seg);
		}
	}
}

/**********************************************************************
**    E_FUNCTION     : ncl_sgbucket_create(bound,size)
**       create segement bucket
**    PARAMETERS
**       INPUT  :
**			bound	-segments list
**			size	-bucket size
**       OUTPUT :
**          none
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_sgbucket_create(bound,size)
UU_LIST *bound;
UU_REAL size;
{
	int i,j,npts,nsegs;
	UM_coord *pts;
	UM_segment seg,*psegs;
	UU_LIST seglst;
	UM_2box bbox;
	UU_REAL minmax[4];
	
	npts = bound->cur_cnt;
	pts = (UM_coord *) UU_LIST_ARRAY (bound);
/*
.....Get the bbox
*/
	minmax[0] = 0;
	minmax[1] = 0;
	minmax[2] = 0;
	minmax[3] = 0;
	um_bound_2box(pts,npts,minmax);
	bbox.xmin = minmax[0];
	bbox.xmax = minmax[1];
	bbox.ymin = minmax[2];
	bbox.ymax = minmax[3];
/*
.....Initialize bucket
*/
	ncl_sgbucket_init(bbox,size);
/*
.....Put boundary into seglst
*/				
	uu_list_init (&seglst,sizeof (UM_segment),200,200);
	for (i = 0; i < npts-1; i++)
	{
		um_vctovc(pts[i],seg.p1);
		um_vctovc(pts[i+1],seg.p2);
		uu_list_push(&seglst,&seg);
	}
/*
.....Store seglst into sgbucket
*/	
	nsegs = seglst.cur_cnt;
	psegs = (UM_segment *)UU_LIST_ARRAY(&seglst);
	for (j = 0; j < nsegs; j++, psegs++)
		ncl_sgbucket_add_seg(psegs);
}

/**********************************************************************
**    E_FUNCTION     : ncl_sgbucket_contain_trian(trilst, trian)
**       Check if trian is contained in the bucket with trilst.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
UU_LOGICAL ncl_sgbucket_contain_seg(seglst, pseg)
UU_LIST *seglst;
UM_segment *pseg;
{
	int i,npts;
	UM_segment *segs;
		
	npts = seglst->cur_cnt;
    segs = (UM_segment*) UU_LIST_ARRAY (seglst);
	for (i = 0; i < npts; i++,segs++)
	{
		if (ncl_segment_equal(pseg,segs))
			return UU_TRUE;
	}

	return UU_FALSE;
}

/**********************************************************************
**    E_FUNCTION     : ncl_sgbucket_getbox_seglst(bbox,trianlst)
**       Get the segments given the bounding box
**    PARAMETERS
**       INPUT  :
**			bbox		- bounding box
**       OUTPUT :
**          seglst  	- pointer to the point triangles list 
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_sgbucket_getbox_seglst(bbox,seglst)
UM_2box bbox;
UU_LIST *seglst;
{
	int i,j,k,ihash,isX,ieX,isY,ieY,ncnt,nhigh,nwidth,status;
	UU_REAL xmin,ymin,cellsize;
	UU_LIST *grid,containlst;
	UM_segment *pseg;

	nwidth = SGBuckets.nCols;
	nhigh = SGBuckets.nBuckets / nwidth;
	cellsize = SGBuckets.cellSize;
	xmin = SGBuckets.xmin;
	ymin = SGBuckets.ymin;

	isX = (int) floor_int((bbox.xmin - xmin)/cellsize);
	if (isX < 0) isX = 0;
	if (isX >= nwidth) isX = nwidth - 1;
	ieX = (int) floor_int((bbox.xmax - xmin)/cellsize);
	if (ieX < 0) ieX = 0;
	if (ieX >= nwidth) ieX = nwidth - 1;
	isY = (int) floor_int((bbox.ymin - ymin)/cellsize);
	if (isY < 0) isY = 0;
	if (isY >= nhigh) isY = nhigh - 1;
	ieY = (int) floor_int((bbox.ymax - ymin)/cellsize);
	if (ieY < 0) ieY = 0;
	if (ieY >= nhigh) ieY = nhigh - 1;

	grid = (UU_LIST *) UU_LIST_ARRAY (&SGBuckets.grid);	

	for (i = isX; i <= ieX; ++i)
	{
		for (j = isY; j <= ieY; ++j)
		{
			ihash = i + j * nwidth;
			ncnt = grid[ihash].cur_cnt;
			if (ncnt > 0)
				uu_list_push_list (seglst, &grid[ihash]);
		}
	}
}

/**********************************************************************
**    E_FUNCTION     :  ncl_sgbucket_free()
**       Free allocate memory and initialize lists used for Buckets..
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_sgbucket_free()
{
	int i,nBuckets;
	UU_LIST *grid;
		
	nBuckets = SGBuckets.nBuckets;
	grid = (UU_LIST *) UU_LIST_ARRAY (&SGBuckets.grid);		

	for (i = 0; i < nBuckets; i++)
	   uu_list_free (&grid[i]);
}
