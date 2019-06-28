/*********************************************************************
**    NAME         :  gout3.c -- More DIGS user callable output fctns.
**       CONTAINS:
**
**    gfillareanorm3
**    ug_flnm3
**    ug_dflareanm3
**
**    gshadearea
**    ug_shade
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       gout3.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:22
*********************************************************************/

#define UG_TRUE 1
#define UG_FALSE 0
#define logical int
#include "zsysdep.h"
#include "umath.h"
#include <stdio.h>
#include "gtbl.h"
#include "g.h"
#include "gerror.h"
#include "ginq.h"
#include "gvlib.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "udebug.h"
#include "gsegac.h"
#include "gmat4.h"

Gerror ug_chkrect(),ug_chkrect3(),ug_chkpoints(),ug_chkpoints3();
extern int ug_viwseg;

typedef struct { int id; Gws ws; int n; Gwpoint3 *points; Gwpoint3 *norms; }
	Stflarea;
typedef struct { 
   int id;                 /* Digs opcode */
   Gws ws;                 /* Workstation id */
   int n;                  /* Number of verticies for this polygon */
   Gwpoint3 *points;       /* Array of verticies */
   Gwpoint3 *norms;        /* Array of normals */
} Stflarea3;

void ug_flnm3();

/********************************************************************* 
**  E_FUNCTION:  Gerror gfillareanorm3(n,points,norms) -- 
**    3D fill area with normals
**      3D fill area is drawn. The outline is specified by points. The normal
**      at each vertex is specified by norms.  The number of verticies is n. 
**      The appearance of the fill area is governed by the rendering attributes.
**  PARAMETERS   
**      INPUT:  Gint n            -- number of verticies.
**              Gwpoint3 points[] -- points specifying the outline of the
**                                   fill area region.
**              Gwpoint3 norms[]   -- normal at each vertex.
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gfillareanorm3(n,points,norms)
/*$ INPUT */
Gint n;
Gwpoint3 points[];
Gwpoint3 norms[];
{
   Gerror irtn;
   int i;

#define ZILCH(a,b) ( fabs((a)-(b)) < 1.0e-6 ? 1 : 0 )

   uu_denter(UU_GTRC,(us,
         "gfillareanorm3(%d,p[0..2]=%f %f %f, %f %f %f, %f %f %f)",
         n,points[0].x,points[0].y,points[0].z,
           points[1].x,points[1].y,points[1].z,
           points[2].x,points[2].y,points[2].z));

   irtn=NCL_NO_ERROR;

   if ((n<3)||(n>=(UG_lismaxsiz-1)/2)) {     /* bad number of verticies */
      ug_errorhand(ENPOINTS,"gfillarea3",&n); irtn=ENPOINTS;
   }

   for( i=0; i<n-1; i++ ) {
      if( ZILCH(points[i].x,points[i+1].x) && 
          ZILCH(points[i].y,points[i+1].y) &&
          ZILCH(points[i].z,points[i+1].z) ) {

         uu_dprint(-1,(us,"ERROR. gfillareanorm3 - illegal polygon"));
         irtn = ENPOINTS;
      }
   }

   if( ZILCH(points[0].x,points[n-1].x) && 
       ZILCH(points[0].y,points[n-1].y) &&
       ZILCH(points[0].z,points[n-1].z) ) {

      uu_dprint(UU_GTRC,(us,"illegal polygon"));
      irtn = ENPOINTS;
   }

   if (irtn==NCL_NO_ERROR) {

      ug_flnm3(n,points,norms);
   }

   uu_dexit;
   return(irtn);
}


/*********************************************************************
**    I_FUNCTION   :  ug_flnm3(n,points,norms) -- 
**    Call workstation 3D fill area with normals.
**    PARAMETERS   
**      INPUT:  Gint n            -- number of verticies.
**              Gwpoint3 points[] -- points specifying the outline of the
**                                   fill area region.
**              Gwpoint3 norms[]   -- normal at each vertex.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_flnm3(n,points,norms)
Gint n;
Gwpoint3 points[];
Gwpoint3 norms[];
{
   struct { int id; 
            Gws ws; 
            int n; 
            Gwpoint3 *points; 
            Gwpoint3 *norms;
          } prms;
   int i;

   uu_denter(UU_GTRC,(us,"ug_flnm3(n=%d)",n));

   if (ug_find.find==UG_FALSE) {
      /* Call workstation */
      prms.n=n;
      prms.id=UG_DFLAREANM3;
      prms.points=points;
      prms.norms=norms;
      ug_wkout(&prms,i=sizeof(prms)/sizeof(int));
   }
   else {         /* See if fill area is picked  */
      /* See if point ug_find.x,y is near a vertex or inside fill area */
      ug_closepolygon(n,points);
   }

   uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     : ug_dflareanm3(prms, reply)
**       
**    PARAMETERS   
**       INPUT  : 
**          prms     
**          reply 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dflareanm3(prms, reply)
Stflarea3 *prms;
int reply[];
{

   UG_segstli *sp;
   struct { 
      int id;                 /* Digs opcode */
      Gws ws;                 /* Workstation id */
      int n;                  /* Number of verticies for this polygon */
      Gwpoint3 *points;       /* Array of verticies */
   } fla3;

   uu_denter(UU_GTRC,(us,"ug_dflareanm3( n = %d )",prms->n));

   if( ug_hidsurf == 0 ) {       /* No hidden-surface removal */

      /* Call workstation's fill area 3D entry. */
      if (ug_find.find==UG_FALSE) {
         fla3.id     = UG_DFLAREA3;
         fla3.ws     = prms->ws;
         fla3.n      = prms->n;
         fla3.points = prms->points;
         ug_wkout(&fla3,sizeof(prms)/sizeof(int));
      }
      else {         /* See if fill area is picked  */
         /* See if point ug_find.x,y is near a vertex or inside fill area */
         ug_closepolygon(prms->n,prms->points);
      }

      /* Expand the ndc box... */
      if ((ug_viwseg>=0)&&(ug_ndcseg>0)) {
         sp=ug_segac(ug_gksstli.opnseg);
         if (ug_find.find==UG_FALSE) ug_boxexpfa3(sp,prms->points,prms->n);
      }
   }
   uu_dexit;
}


/*********************************************************************
**    I_FUNCTION     :  ug_dnoflareanm3(prms, reply)
**       This is a simulation routine which draws the polygon
**       outline and vertex normals instead of the shaded polygon.
**    PARAMETERS   
**       INPUT  : 
**          prms->n           Number of verticies.
**          prms->points      Verticies of fillarea.
**          prms->norms       Surface normal at each vertex.
**       OUTPUT :  
**          reply
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : Only displays outline and normals.
*********************************************************************/
void ug_dnoflareanm3(prms, reply)
Stflarea *prms;
int reply[];
{
   int i;
   Gwpoint3 points[2];
   
   uu_denter(UU_GITRC,(us,"ug_dnoflareanm3(%d)", prms->n));

   /* Draw the polygon outline */
   ug_polyln3(prms->n, prms->points);

   points[0].x = prms->points[0].x;
   points[0].y = prms->points[0].y;
   points[0].z = prms->points[0].z;
   points[1].x = prms->points[prms->n-1].x;
   points[1].y = prms->points[prms->n-1].y;
   points[1].z = prms->points[prms->n-1].z;
   ug_polyln3(2, points);

   /* Draw each of the normals */
   for(i=0; i<prms->n; i++) {

      /* Base of normal at the vertex */
      points[0].x = prms->points[i].x;
      points[0].y = prms->points[i].y;
      points[0].z = prms->points[i].z;

      points[1].x = points[0].x + prms->norms[i].x;
      points[1].y = points[0].y + prms->norms[i].y;
      points[1].z = points[0].z + prms->norms[i].z;

      ug_polyln3(2, points);
   }

   uu_dexit;
}
/********************************************************************* 
**  E_FUNCTION:  Gerror gshadearea (n,points,norms) -- 
**    3D shade area
**      3D area is shaded. The outline is specified by points. The normal
**      at each vertex is specified by norms.  The number of verticies is n. 
**      The appearance of the fill area is governed by the rendering attributes.
**  PARAMETERS   
**      INPUT:  Gint n            -- number of verticies.
**              Gwpoint3 points[] -- points specifying the outline of the
**                                   fill area region.
**              Gwpoint3 norms[]   -- normal at each vertex.
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gshadearea (n,points,norms,type)
Gint n,type;
Gwpoint3 points[];
Gwpoint3 norms[];
{
   Gerror irtn;
   UG_segstli *sp;

   irtn=NCL_NO_ERROR;

/*
...bad number of verticies
*/
   if ( n < 3  || 2*n >= 10000 )
	{
      ug_errorhand(ENPOINTS,"gfillarea3",&n);
		irtn=ENPOINTS;
   }

   if (irtn == NCL_NO_ERROR)
   {
       ug_shade (n,points,norms,type);
       if (ug_gksos.sysstate==UG_SGOP)
       {
          ug_nshad3 (ug_segac(ug_gksstli.opnseg)->seglist,n,points,norms,type);
/* 
... update xforms bits in seg header
*/
          sp=ug_segac(ug_gksstli.opnseg);
          ug_updxforms(sp);
       }
   }

   uu_dexit;
   return(irtn);
}
/*********************************************************************
**    I_FUNCTION   :  ug_shade (n,points,norms) -- 
**    Call workstation 3D shade area with normals.
**    PARAMETERS   
**      INPUT:  Gint n            -- number of verticies.
**              Gwpoint3 points[] -- points specifying the outline of the
**                                   fill area region.
**              Gwpoint3 norms[]   -- normal at each vertex.
**       OUTPUT :  
**    RETURNS      : 0
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_shade (n,points,norms,type)
Gint n;
Gwpoint3 points[];
Gwpoint3 norms[];
{
   struct { int id; 
            Gws ws; 
            int n; 
			int type;
            Gwpoint3 *points; 
            Gwpoint3 *norms;
          } prms;
   int i;

   uu_denter(UU_GTRC,(us,"ug_flnm3(n=%d)",n));

   if (ug_find.find==UG_FALSE) {
      /* Call workstation */
      prms.n=n;
      prms.id = UG_DSHADEAREA;
      prms.points=points;
      prms.norms=norms;
      prms.type=type;
      ug_wkout(&prms,i=sizeof(prms)/sizeof(int));
   }
   else {         /* See if fill area is picked  */
      /* See if point ug_find.x,y is near a vertex or inside fill area */
      ug_closepolygon(n,points);
   }

   uu_dexit;
	return (0);
}
