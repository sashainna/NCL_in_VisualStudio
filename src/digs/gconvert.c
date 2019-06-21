/*********************************************************************
**    NAME         :  gconvert.c	- precision checking routines
**								written to convert single/double on iris.
**       CONTAINS:
**		Getpoints3(pts[],len)  
**		Getpoints(pts[],len)
**		convert_seg(dstr,sstr)
**
**			NOTES:	This file exists to facilitate iris precision fix.
**
** 
**    MODULE NAME AND RELEASE LEVEL
**       gconvert.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:18
*********************************************************************/
#include "zsysdep.h"
#include "gtbl.h"
#include "gobas.h"
#include "gsegop.h"
#include "gviw.h"
#include "gconvert.h"
#include "udebug.h"

/*********************************************************************
**    I_FUNCTION     :  Getpoints3(pts,len)  
**			Checks if pts are double precision, if yes - return pts; if no -
**			copies values to new structure and return new double precision pts.
**    PARAMETERS   
**       INPUT  : Segpoint3 pts[] -- array of 3d point structures.
**						len -- number of points
**       OUTPUT : 
**    RETURNS      : array of Gfloat points
**    WARNINGS     : none
*********************************************************************/
Gwpoint3 *Getpoints3(pts,len)
Segpoint3 pts[];
int len;
{
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
	if (len > 0)
		{	Gwpoint3 *a;
			int i;

			a = (Gwpoint3 *)uu_toolmalloc(len*sizeof(Gwpoint3));
			for (i=0; i<len; i++)
			{
				ug_ItoGpoint3(a[i],pts[i]);
			}
			return(a);
		}
   else  /* return address other than NULL - never used because len = 0 */
		return((Gwpoint3 *)pts);
#else
	return(pts);
#endif
}

/*********************************************************************
**    I_FUNCTION     :  Getpoints(pts,len)  
**			Checks if pts are double precision, if yes - return pts; if no -
**			copies values to new structure and return new double precision pts.
**    PARAMETERS   
**       INPUT  : Segpoint pts[] -- array of 2d point structures.
**						len -- number of points
**       OUTPUT : 
**    RETURNS      : array of Gfloat points
**    WARNINGS     : none
*********************************************************************/
Gwpoint *Getpoints(pts,len)
Segpoint pts[];
int len;
{
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
	if (len > 0)
		{	Gwpoint *a;
			int i;

			a = (Gwpoint *)uu_toolmalloc(len*sizeof(Gwpoint));
			for (i=0; i<len; i++)
			{
				ug_ItoGpoint(a[i],pts[i]);
			}
			return(a);
		}
   else  /* return address other than NULL - never used because len = 0 */
		return((Gwpoint *)pts);
#else
	return(pts);
#endif
}

/*********************************************************************
**    I_FUNCTION     :  convert_seg(dstr,sstr)  
**			Converts single precision segment structure to double precision.
**    PARAMETERS   
**       INPUT  : int **dstr; double precision segment structure.
**						int *sstr; -- pointer to single precision structure. 
**       OUTPUT : 
**    RETURNS      : 
**    WARNINGS     : none
*********************************************************************/
convert_seg(dstr,sstr)
Gint **dstr,*sstr;
{
	int opcode,len,i;

	opcode=(*(UG_plylna3op *)sstr).elttype;

	switch (opcode) {
   case UG_NOOP: break;
   case UG_PAGEOP: break;
   case UG_PLYLNA3OP:
			{
			static UG_segplylna3op local;
			local.elttype = opcode;
			len=(*(UG_plylna3op *)sstr).len;
			local.len = len;
			for (i=0; i<len; i++)
			{
				ug_ItoGpoint3(local.pts[i], (*(UG_plylna3op *)sstr).pts[i]);
			}
			*dstr = (int *)&local;
			}
			break;
    case UG_PLYLNA2OP: 
			{
			static UG_segplylna2op local;
		 	len=(*(UG_plylna2op *)sstr).len;
			local.len = len;
			for (i=0; i<len; i++)
			{
				ug_ItoGpoint(local.pts[i], (*(UG_plylna2op *)sstr).pts[i]);
			}
			*dstr = (int *)&local;
			}
			break;
	 case UG_PLYLNRASOP:
			*dstr = (int *)sstr;
			break;
    case UG_PLYMKA3OP:
			{	static UG_segplymka3op local;
				len=(*(UG_plymka3op *)sstr).len;
				(*(UG_segplymka3op *)dstr).len = len;
				for (i=0; i<len; i++)
				{
					ug_ItoGpoint3((*(UG_segplymka3op *)dstr).pts[i],
									(*(UG_plymka3op *)sstr).pts[i]);
				}
				*dstr = (int *)&local;
			}
		break;
    case UG_PLYMKA2OP:
			{	static UG_segplymka2op local;
				len=(*(UG_plymka2op *)sstr).len;
				(*(UG_segplymka2op *)dstr).len = len;
				for (i=0; i<len; i++);
				{
					ug_ItoGpoint((*(UG_segplymka2op *)dstr).pts[i],
									(*(UG_plymka2op *)sstr).pts[i]);
				}
				*dstr = (int *)&local;
			}
			break;
	 case UG_PLYMKRASOP:
			*dstr = (int *)sstr;
			break;
/*
    case UG_TEXTOP:
			ug_ItoGpoint3((*(UG_segtextop *)dstr).position,(*(UG_textop *)sstr).position);
			*dstr = (int *)&local;
			break;
	 case UG_TEXTRASOP:
			*dstr = *sstr;
			break;
	 case UG_FLAREA3OP:
			len=(*(UG_flarea3op *)sstr).len;
			for (i=0; i<len; i++)
			{
				ug_ItoGpoint3((*(UG_segflarea3op *)dstr).pts[i],
									(*(UG_flarea3op *)sstr).pts[i]);
			}
			*dstr = (int *)&local;
			break;
	 case UG_FLAREAOP:
			len=(*(UG_flareaop *)sstr).len;
			for (i=0; i<len; i++)
			{
				ug_ItoGpoint((*(UG_segflareaop *)dstr).pts[i],
									(*(UG_flareaop *)sstr).pts[i]);
			}
			*dstr = (int *)&local;
			break;
	 case UG_FLAREARASOP:
			*dstr = *sstr;
			break;
	 case UG_CELLOP:	
			ug_ItoGpoint((*(UG_segcellop *)dstr).rect.ll,(*(UG_cellop *)sstr).rect.ll);
			ug_ItoGpoint((*(UG_segcellop *)dstr).rect.ur,(*(UG_cellop *)sstr).rect.ur);
			break;
			zbytecp((*(UG_cellrasop *)cmd).dims,(*(UG_cellop *)cmd).dims);
			*dstr = (int *)&local;
			break;
    case UG_PROCOP: 
			break;
    case UG_CALLOP: 
         break;
	case UG_SNTRANOP:
			break;
	case UG_MTRANOP: 
		{	Gtran a;

			ug_ItoGcopy(a,(*(UG_mtranop *)cmd).xf);
			ug_smodxf(a,(*(UG_mtranop *)cmd).type);
		}
			*dstr = (int *)&local;
			break;
	case UG_LMTRANOP: 
		{	Gtran a;

			ug_ItoGcopy(a,(*(UG_lmtranop *)cmd).xf);
			ug_slmodxf(a,(*(UG_mtranop *)cmd).type);
		}
			*dstr = (int *)&local;
			break;
	case UG_DFATSOP:  break;
   case UG_LNCOLROP: 
			 break;
   case UG_MKCOLROP: 
			 break;
   case UG_TXCOLROP: 
			 break;
   case UG_LSTYLOP: 
		{	Glntype b;
			ug_ItoGlntype(b,&(*(UG_lstylop *)cmd).ls);
			ug_nlstl(newlist,&b);
		}
			*dstr = (int *)&local;
			break;
   case UG_LWIDOP: 
			break;
   case UG_PENOP:  
			break;
   case UG_FONTOP: 
			break;
   case UG_CHHGTOP:
		ug_nchhgt(newlist,(*(UG_chhgtop *)cmd).height*rasscl);
		break;
	case UG_CHEXPOP: 
			ug_nchexp(newlist,(Gfloat)(*(UG_chexpop *)cmd).exp); break;
   case UG_CHPLANEOP: 
			ug_nchpl(newlist,(Gfloat)(*(UG_chplaneop *)cmd).txpvc.x,
				(Gfloat)(*(UG_chplaneop *)cmd).txpvc.y,
				(Gfloat)(*(UG_chplaneop *)cmd).txpvc.z);
       	break;
   case UG_CHUP3OP: 
			ug_nchup3(newlist,temp3=Getpoints3(&(*(UG_chup3op *)cmd).upvec,1));
        break;
   case UG_CHUP2OP:
			ug_nchup(newlist,
				temp=Getpoints(&(*(UG_chup2op *)cmd).upvec,1));
        break;
   case UG_CHPATHOP: ug_nchpa(newlist,(*(UG_chpathop *)cmd).path); break;
   case UG_CHSPACEOP: 
			ug_nchsp(newlist,(Gfloat)(*(UG_chspaceop *)cmd).spacing); 
			break;
   case UG_CHJUSTOP: ug_nchj(newlist,&(*(UG_chjustop *)cmd).align); break;
   case UG_SYMBOLOP: ug_nmksy(newlist,(*(UG_symbolop *)cmd).type); break;
   case UG_PICKIDOP: ug_npkid(newlist,(*(UG_pickidop *)cmd).pid); break;
			break;
	case UG_FACOLROP: ug_nfacolr(newlist,(*(UG_facolorop *)cmd).color); break;
	case UG_EDGEFLAGOP: ug_sedgeflag((*(UG_edgeflagop *)cmd).f); break;
*/
    default:/* fprintf(ug_gksos.erfile,"ug_rasstr2 illegal opcode=%d\n",opcode);*/
         break;
    };                             /* case */
}
