/*******************************************************************
**    NAME         :  igesmesh.f
**       CONTAINS: routines to handle NCL mesh surfaces.
**       uig_map_splinesrf(dblk,igesin,t,key)
**       ncl_put_meshpatch(meshpatch, patch)
**       ncl_p_meshpatch(meshpatch)
**       ncl_p83_meshsurf(surf)
**       int ptmhed(buf, nclkey)
**       int ptmpat(nclkey, ipatch, buf)
**    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       igesmesh.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:43
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "msrf.h"
#include "mdattr.h"
#include "tigsupp.h"
#include "mdrel.h"
#include "udebug.h"
#include "usysdef.h"
#include "mxxx.h" 
#include "rbase.h"
#include "nccs.h"
#include "ncl.h"
#include "umath.h"
#include "uhep.h"
#include "mdrel.h"
#include "modef.h"
#include "mcrv.h"
#include "mdebug.h"
#include "mdgenent.h"
#include "mfort.h"

#define NCLI_MESHSURF 26

int ixa[16] = {0,12,24,36,3,15,27,39,6,18,30,42,9,21,33,45};
extern UU_KEY_ID *tig_unlabeled_keys;
extern int tig_unlabeled;

void ncl_put_meshpatch();

/*********************************************************************
**    E_SUBROUTINE     : subroutine igesmesh 
**          prepare a mesh surface from patch data of iges file. 
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
 
void uig_map_splinesrf(dblk,igesin,t,key)
	struct dir_rec *dblk;            /* IGES p-spline surface dir. rec */
	struct IG_igesplsf_rec *igesin;  /* IGES p-spline surface par. rec */
	UU_REAL t[12];
	UU_KEY_ID *key;
	  {
	  UU_REAL a[3][17];    /* note: second dimension indexed 1..16 */ 
      UU_REAL p2[17][3];   /* note: first  dimension indexed 1..16 */
      UU_REAL w[48];
	  UU_REAL u, u2, u3, v, v2, v3;
	  UU_REAL pt1[3];
	  int hed[10];
      int m, n, i, j, k, ju, jv, cx;
	  int ipat, npats, jwx;
      UU_KEY_ID srfkey;

      /*    get row & col sizes   */

      m = igesin->no_u_seg;
      n = igesin->no_v_seg;
      npats=m*n;

      /*    set up header word    */

      hed[1]=NCLI_MESHSURF;
      hed[2]=npats;
      hed[3]=m;
      hed[4]=n;
      hed[5]=0;
      hed[6]=0;

      /*    save surface header   */

      ptmhed(dblk, igesin, hed, &srfkey);

      /*    write out dummy last patch to allocate mem   */

      for (i=0;i<48;i++) w[i] = 0.; /* fix occasional reserved op fault on vax */
      ptmpat(srfkey, npats, w);

	  cx = 0;
	  ipat = 0;
      for (ju=0; ju<m; ju++)
        { 
        u=igesin->tu[ju+1] - igesin->tu[ju];
        u2=u*u;
        u3=u2*u;
 
        for (jv=0; jv<n; jv++)
          { 
          v=igesin->tv[jv+1] - igesin->tv[jv];
          v2=v*v;
          v3=v2*v;
 
          for (j=0; j<3; j++)
            {
            for (k=1; k<=16; k++)
			  {
              a[j][k] = igesin->patc[cx];
			  cx = cx+1;
			  };
 
            a[j][ 2]=a[j][ 2]*u;
            a[j][ 3]=a[j][ 3]*u2;
            a[j][ 4]=a[j][ 4]*u3;
            a[j][ 5]=a[j][ 5]*v;
            a[j][ 6]=a[j][ 6]*u*v;	
            a[j][ 7]=a[j][ 7]*u2*v;	
            a[j][ 8]=a[j][ 8]*u3*v;
            a[j][ 9]=a[j][ 9]*v2;
            a[j][10]=a[j][10]*u*v2;
            a[j][11]=a[j][11]*u2*v2;
            a[j][12]=a[j][12]*u3*v2;
            a[j][13]=a[j][13]*v3;
            a[j][14]=a[j][14]*u*v3;
            a[j][15]=a[j][15]*u2*v3;
            a[j][16]=a[j][16]*u3*v3;
 
            p2[ 1][j]=a[j][1];
            p2[ 2][j]=a[j][1]+a[j][2]/3.;
            p2[ 3][j]=a[j][1]+a[j][2]*2./3.+a[j][3]/3.;
            p2[ 4][j]=a[j][1]+a[j][2]+a[j][3]+a[j][4];
            p2[ 5][j]=a[j][1]+a[j][5]/3.;
            p2[ 6][j]=p2[2][j]+a[j][5]/3.+a[j][6]/9.;
            p2[ 7][j]=p2[3][j]+a[j][5]/3.+a[j][6]*2./9.+a[j][7]/9.;
            p2[ 8][j]=p2[4][j]+a[j][5]/3.+a[j][6]/3.+a[j][7]/3.+a[j][8]/3.;
            p2[ 9][j]=a[j][1]+a[j][5]*2./3.+a[j][9]/3.;
            p2[10][j]=p2[2][j]+a[j][5]*2./3.+a[j][6]*2./9.+a[j][9]/3.+a[j][10]/9.;
            p2[11][j]=p2[3][j]+a[j][5]*2./3.+a[j][6]*4./9.+a[j][7]*2./9.
                      +a[j][9]/3.+a[j][10]*2./9.+a[j][11]/9.;
            p2[12][j]=p2[4][j]+a[j][5]*2./3.+a[j][6]*2./3.+a[j][7]*2./3.
                      +a[j][8]*2./3.+a[j][9]/3.+a[j][10]/3.+a[j][11]/3.+a[j][12]/3.;	
            p2[13][j]=a[j][1]+a[j][5]+a[j][9]+a[j][13];
            p2[14][j]=p2[2][j]+a[j][5]+a[j][6]/3.+a[j][9]+a[j][10]/3.
                      +a[j][13]+a[j][14]/3.;
            p2[15][j]=p2[3][j]+a[j][5]+a[j][6]*2./3.+a[j][7]/3.+a[j][9]
                      +a[j][10]*2./3.+a[j][11]/3.+a[j][13]+a[j][14]*2./3.+a[j][15]/3.;
            p2[16][j]=p2[4][j]+a[j][5]+a[j][6]+a[j][7]+a[j][8]+a[j][9]+a[j][10]
                      +a[j][11]+a[j][12]+a[j][13]+a[j][14]+a[j][15]+a[j][16];
            };
 
          uig_tran_coor(p2[1], t, pt1);
          um_vctmsc(pt1,unit_scale,w);

          for (i=2; i<=16; i++)
            {
            uig_tran_coor(p2[i], t, pt1);
            um_vctmsc(pt1,unit_scale,pt1);
            jwx = ixa[i-1];
            um_vcmnvc(pt1, w, &w[jwx]);
            };
 
          /*    save patch data   */

          ipat=jv*m+ju+1;
          ptmpat(srfkey, ipat, w);

          };

        cx = cx + 48;
        };
      *key = srfkey;
      }


/*********************************************************************
**    E_FUNCTION     : int ptmhed(dblk, buf, nclkey)
**       Create an NCL mesh surface entity in UNIBASE using the given
**       mesh surface header data (buf) and return the UNIBASE key of
**       the newly created mesh surface entity.
**    PARAMETERS
**       INPUT  :
**          dblk              IGES directory record
**          buf               mesh surface header data
**       OUTPUT :
**          nclkey            UNIBASE key of created entity
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ptmhed(dblk, igesin, buf, nclkey)
   struct dir_rec *dblk;
   struct IG_igesplsf_rec *igesin;
   int buf[];
   UU_KEY_ID *nclkey;

   {
   int status;
   struct NCL_meshsf_rec surf;
	int found;
	int uig_match_meshsurf();

   uu_denter(UU_MTRC,(us,"ptmhed(buf=%x, nclkey=%x)",buf, nclkey));

   /* setup unibase storage area */

   status = ur_setup_data(NCL_MESHSURF_REL, &surf, sizeof(struct NCL_meshsf_rec));

   surf.surf_type = buf[1];
   surf.no_mpatch = 0;
   surf.m = buf[3];
   surf.n = buf[4];
   surf.rev_normal = UU_FALSE;
   surf.offset = buf[5];
   surf.offdist = buf[6];
   surf.rldnu = -1;
   surf.swapuv = 0;
   surf.no_displst = 0;
   surf.displst = NULL;
   surf.no_tesslst = 0;
   surf.tesslst = NULL;
   surf.no_boxlst = 0;
   surf.boxlst = NULL;

   /* create unibase record */

   uig_update_attr(dblk);
   create_label(dblk, igesin->no_prop, igesin->prop, surf.label, &surf.subscr);

   status = ncl_create_entity(&surf, dblk->view_ptr);
	if (label_type == 8)
	{
/*
.....Label matching. Determine if exact match.
*/
		uig_exact_match(&surf,uig_match_meshsurf);
	}

   *nclkey = surf.key;

   if (status != UU_SUCCESS) uu_dprint(-1,(us,"ptmhed returned FAILURE"));
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ptmpat(nclkey, ipatch, buf)
**       Either append or replace a patch (IPATCH) in an NCL
**       mesh surface (NCLKEY) with the data in BUF.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of mesh surface
**          ipatch               number of patch
**          buf                  patch data
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ptmpat(nclkey, ipatch, buf)
   UU_KEY_ID nclkey;
   int ipatch;
   UU_REAL buf[];

   {
   int status;
   UU_KEY_ID key;
   int patch;
   int rel_num;
   struct NCL_meshsf_rec surf;
   struct NCL_mpatch_rec meshpatch;

   uu_denter(UU_MTRC,(us,"ptmpat(key=%x, ipatch=%d, buf=%x)", nclkey,
      ipatch, buf));

   key = nclkey;
   patch = ipatch;
   status = UU_SUCCESS;
   if (ur_retrieve_data_relnum(key, &rel_num) != 0)
      status = UU_FAILURE;
   else if (rel_num != NCL_MESHSURF_REL)
      status = UU_FAILURE;
   else
      {
      surf.key = key;
      if (ur_retrieve_data_fixed(&surf) != 0)
         status = UU_FAILURE;
      else
         {
         ncl_put_meshpatch(buf, &meshpatch);
         if (ur_update_data_varlist(surf.key, 1, &meshpatch, patch, 1) != 0)
            status = UU_FAILURE;
         }
      }

   uu_dexit;
   return (status);
   }


/*********************************************************************
**    E_FUNCTION     : ncl_put_meshpatch(meshpatch, patch)
**       Move an NCLI mesh surface patch into a mesh surface patch.
**    PARAMETERS   
**       INPUT  : 
**          meshpatch         NCLI representation of a mesh surface
**                            patch
**       OUTPUT :  
**          patch             mesh surface patch
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_put_meshpatch(meshpatch, patch)
   UU_REAL meshpatch[];
   struct NCL_mpatch_rec *patch;

   {
   int i,j,k;

   uu_denter(UU_MTRC,(us,"ncl_put_meshpatch(meshpatch=%x,patch=%x)",
      meshpatch, patch));
/*   ncl_p_meshpatch(meshpatch); */
   for (k=0; k<3; k++)
	  {
      patch->pt[k] = meshpatch[k];
	  };
   k = 3;
   for (j=0; j<15; j++) 
      {
      for (i=0; i<3; i++,k++) patch->delta[j][i] = meshpatch[k];
      }
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_p_meshpatch(meshpatch)
**       Print an NCLI mesh surface patch record.
**    PARAMETERS   
**       INPUT  : 
**          meshpatch            NCLI mesh surface patch record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
ncl_p_meshpatch(meshpatch)
   struct NCLI_meshpatch_rec *meshpatch;

   {
   int i;

   uu_denter(UU_MTRC,(us,"ncl_p_meshpatch(meshpatch=%x)",meshpatch));

   um_pscroll("NCLI_meshpatch_rec");
   sprintf(UM_sbuf, "  point = (%g,%g,%g)", meshpatch->point[0],
      meshpatch->point[1], meshpatch->point[2]);
   um_pscroll(UM_sbuf);
   for (i=0; i<15; i++)
      {
      sprintf(UM_sbuf, "  delta[%d] = (%g,%g,%g)", i, meshpatch->delta[i][0],
         meshpatch->delta[i][1], meshpatch->delta[i][2]);
      um_pscroll(UM_sbuf);
      }

   uu_dexit;
   }
*/

/*********************************************************************
**    E_FUNCTION     : ncl_p83_meshsurf(surf)
**       Print an NCL mesh surface record.
**    PARAMETERS   
**       INPUT  : 
**          surf                 an NCL mesh record (UNIBASE format)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
ncl_p83_meshsurf(surf)
   struct NCL_meshsf_rec *surf;

   {
   int i,j;

   uu_denter(UU_MTRC,(us,"ncl_p83_meshsurf(key=%x)", surf->key));

   sprintf(UM_sbuf,"NCL MESHSURF: %x", surf->key);
   um_pscroll(UM_sbuf);

   sprintf(UM_sbuf, "label %s",surf->label);
   um_pscroll(UM_sbuf);

   um_p_ary(UM_PINT,"surf_type", 1, &surf->surf_type);
   um_p_ary(UM_PINT,"m", 1, &surf->m);
   um_p_ary(UM_PINT,"n", 1, &surf->n);

   for (i=0; i<surf->no_mpatch; i++)
      {
      sprintf(UM_sbuf, "  PATCH %d", i);
      um_pscroll(UM_sbuf);
      sprintf(UM_sbuf, "    point=(%g,%g,%g)", surf->mpatch[i].pt[0],
         surf->mpatch[i].pt[1], surf->mpatch[i].pt[2]);
      um_pscroll(UM_sbuf);
      for (j=0; j<15; j++)
         {
         sprintf(UM_sbuf, "    delta(%d)=(%g,%g,%g)", j,
            surf->mpatch[i].delta[j][0], surf->mpatch[i].delta[j][1],
            surf->mpatch[i].delta[j][2]);
         um_pscroll(UM_sbuf);
         }
      }

   uu_dexit;
   }
*/
