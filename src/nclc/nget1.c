/*********************************************************************
**    NAME         :  nget1.c
**       CONTAINS: data retrieval routines for mapping UNIBASE entities
**                   to the data representation for NCL; these routines
**                   are FORTRAN callable and hence follow the standard
**                   FORTRAN/C interface conventions
**       int gtgeo(nclkey, buf)
**       int iscmpc (nclkey, iflag)
**       int gtccnm (nclkey, nents)
**       int gtcent (nclkey, ix, iunit, buf, ietype, ierr)
**       void gtcccv (nclkey, ix, keyout,ierr)
**       int gtshap (buf, nclkey)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nget1.c , 25.1
**     DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:09:02
*********************************************************************/
#include "usysdef.h"

#include "mcrv.h"
#include "mdrel.h"
#include "mfort.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclx.h"
#include "nclxmdl.h"
#include "umath.h"
#include "ycom.h"

extern int NCLX_internal_geom;

/*********************************************************************
**    E_FUNCTION     : int gtgeo(nclkey, buf)
**       Retrieve the NCL  geometric representation for any of the
**       legal NCL entities.
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the entity
**       OUTPUT :
**          buf               buffer to place the data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int gtgeo(nclkey, buf)
UM_int4 *nclkey;
UM_real8 buf[];
{
   int status;
	UU_KEY_ID key = *nclkey;
/*
.....Internal OpenNCL geometry
*/
	if (NCLX_internal_geom)
	{
		status = NclxMdlRetrieveGeo(key,buf);
		return(status);
	}
	else
	{
		status = gtgeo1(nclkey, buf);
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int gtpnnp(nclkey, npts, ntyp)
**       Return the type & number of points in a Patern record.
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the entity
**       OUTPUT :
**          npts              number of points in patern.
**          ntyp              type of patern (1 - PT, 2 - PV)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int gtpnnp(nclkey, npts, ntyp)
UU_KEY_ID *nclkey;
UM_int2 *npts;
int *ntyp;
{
   int status;
	NCLX_mdl_patern *pat;
/*
.....Internal OpenNCL geometry
*/
	if (NCLX_internal_geom)
	{
		status = NclxMdlFindPatern(*nclkey,&pat);
		if (status == NCLX_SUCCESS)
		{
			*npts = pat->npts;
			if (pat->pntype == NCLX_MDL_POINT) *ntyp = 1;
			else *ntyp = 2;
		}
	}
/*
.....Standard NCL geometry
*/
	else
	{
		status = gtpnnp1(nclkey, npts, ntyp);
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int gtpnpt(buf, ntyp, nclkey, ipoint)
**       Return the type & requested point from a Patern record.
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the entity
**       OUTPUT :
**          buf               Requested point.
**          ntyp              type of patern (1 - PT, 2 - PV)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int gtpnpt(buf, ntyp, nclkey, ipoint)
UM_real8 buf[];
UU_KEY_ID *nclkey;
UM_int2 *ipoint;
int *ntyp;
{
   int status,i,ist,np;
	NCLX_mdl_patern *pat;
/*
.....Internal OpenNCL geometry
*/
	if (NCLX_internal_geom)
	{
		status = NclxMdlFindPatern(*nclkey,&pat);
		if (status == NCLX_SUCCESS)
		{
			if (*ipoint < 1 || *ipoint > pat->npts) return(NCLX_FAILURE);
			if (pat->pntype == NCLX_MDL_POINT)
			{
				*ntyp = 1;
				np = 3;
			}
			else
			{
				*ntyp = 2;
				np = 6;
			}
			ist = (*ipoint-1) * np;
			for (i=ist;i<ist+np;i++) buf[i-ist] = pat->pts[i];
		}
	}
/*
.....Standard NCL geometry
*/
	else
	{
		status = gtpnpt1(buf, ntyp, nclkey, ipoint);
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int iscmpc (nclkey, iflag)
**       Determine if an entity is a composite curve.
**    PARAMETERS   
**       INPUT  : 
**          nclkey     - key of entity
**       OUTPUT :  
**          iflag      - 1 if entity is composite curve, 0 otherwise.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int iscmpc (nclkey, iflag)
UM_int4 *nclkey;
UM_int2 *iflag;
{
	int status;
	NCLX_mdl_struct *geo;
/*
.....Internal OpenNCL geometry
*/
	if (NCLX_internal_geom)
	{
		iflag = 0;
		status = NclxMdlFindGeo(*nclkey,&geo);
		if (status == NCLX_SUCCESS && geo->relnum == NCLX_MDL_COMPOSITE) 
			*iflag = 1;
	}
	else
	{
		status = iscmpc1(nclkey, iflag);
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int gtccnm (nclkey, nents)
**       Return the number of elements in a composite curve.
**    PARAMETERS   
**       INPUT  : 
**          nclkey     - key of entity
**       OUTPUT :  
**          nents      - number of elements
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int gtccnm (nclkey, nents)
UM_int4 *nclkey;
UM_int2 *nents;
{
   int status;
	NCLX_mdl_struct *geo;
	NCLX_mdl_composite *cv;
/*
.....Internal OpenNCL geometry
*/
	if (NCLX_internal_geom)
	{
		*nents = 0;
		status = NclxMdlFindGeo(*nclkey,&geo);
		if (status == NCLX_SUCCESS && geo->relnum == NCLX_MDL_COMPOSITE) 
		{
			cv = (NCLX_mdl_composite *)geo;
			*nents = cv->ncurve;
		}
	}
	else
	{
		status = gtccnm1(nclkey, nents);
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int gtcent (nclkey, ix, buf, ietype, ierr)
**       Return data of element of composite curve for pocket.
**    PARAMETERS   
**       INPUT  : 
**          nclkey     - key of composite curve.
**          ix         - index of element
**       OUTPUT :  
**          keyout     - key of this entity
**          buf        - canonical data if line or circle.
**          ietype     - NCL type
**          ierr       - 0 if no error, non-zero if error
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int gtcent (nclkey, ix, keyout, buf, ietype, ierr)
UM_int4 *nclkey, *keyout;
UM_int2 *ix, *ietype, *ierr;
UM_real8 buf[];
{
   
	int status, lix, i;
	UU_REAL dang;
	UM_transf rotmat;
	UM_vector svec,v1;
	NCLX_mdl_struct *geo,*dat;
	NCLX_mdl_cmp_entity *ent;
	NCLX_mdl_line *ln;
	NCLX_mdl_circle *ci;
	NCLX_mdl_composite *cv;
/*
.....Internal OpenNCL geometry
*/
	if (NCLX_internal_geom)
	{
/*
........Initialize routine
*/
		*ierr = 0;
		status = NclxMdlFindGeo(*nclkey,&geo);
		if (status == NCLX_SUCCESS && geo->relnum == NCLX_MDL_COMPOSITE) 
		{
			lix = *ix;
			cv = (NCLX_mdl_composite *)geo;
			if (lix < 1 || lix > cv->ncurve) goto failed;
/*
........Get composite entity
*/
			ent = (NCLX_mdl_cmp_entity *)&cv->cvid[lix-1];
			dat = (NCLX_mdl_struct *)ent->curve;
			switch (dat->relnum)
			{
/*
...........Line
*/
			case NCLX_MDL_LINE:
				ln = (NCLX_mdl_line *)dat;
				if (ent->reverse)
				{
					um_vctovc(ln->ept,buf);
					um_vctovc(ln->spt,&buf[3]);
				}
				else
				{
					um_vctovc(ln->spt,buf);
					um_vctovc(ln->ept,&buf[3]);
				}
				*ietype = NCLG_LINE;
				break;
/*
...........Circle
*/
			case NCLX_MDL_CIRCLE:
				ci = (NCLX_mdl_circle *)dat;
				if (ent->reverse)
				{
					um_rotlntf(ci->center, ci->nvec, ci->dang, rotmat);
					um_vctmtf(ci->svec, rotmat, svec);
					dang = -ci->dang;
				}
				else
				{
					um_vctovc(ci->svec,svec);
					dang = ci->dang;
				}
				um_rotlntf(ci->center, ci->nvec, dang, rotmat);
				um_vctmtf(svec, rotmat, v1);
				for (i=0;i<3;i++)
				{
					buf[i] = (ci->svec[i]*ci->radius+ci->center[i]);
					buf[i+3] = ci->center[i];
					buf[i+6] = (v1[i]*ci->radius+ci->center[i]);
					buf[i+9] = ci->nvec[i];
				}
				buf[12] = ci->radius;
				buf[13] = dang;
				*ietype = NCLI_CIRCLE;
				break;
/*
...........Curve
*/
			case NCLX_MDL_CURVE:
			case NCLX_MDL_BSPLINE:
				if (ent->reverse) *keyout = -dat->key;
				else *keyout = dat->key;
				*ietype = NCLG_CURVE;
				break;
/*
...........Unsupported entity
*/
			default:
				goto failed;
			}
		}
		goto done;
failed:;
		*ierr = 1;
		status = UU_FAILURE;
		goto done;
	}
	else
	{
		status = gtcent1 (nclkey, ix, keyout, buf, ietype, ierr);
	}
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int gtshap (buf, nclkey)
**       Return shape.
**    PARAMETERS   
**       INPUT  : 
**          nclkey     - key of shape.
**       OUTPUT :  
**          buf        - canonical data of shape.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int gtshap (buf,nclkey)
UM_int4 *nclkey;
UM_real8 buf[];
{
   
   int status, ix, i,j, stfl,nw;
	UU_REAL st[4],en[4],tl,vx,vy;
	UM_int2 ityp,i2;
	UM_real8 r8;
	NCLX_mdl_struct *geo;
	NCLX_mdl_shape *sh;
	union
	{
		UM_real8 asn;
		UM_real4 qsn[2];
		UM_int2 ksn[4];
		UM_int4 jsn[2];
	} val;
#define CCLW 60
#define CLW 60
#define IN 652
#define OUT 653
#define LEFT 8
#define RIGHT 24
/*
.....Internal OpenNCL geometry
*/
	if (NCLX_internal_geom)
	{
/*
........Initialize routine
*/
		status = NclxMdlFindGeo(*nclkey,&geo);
		if (status == NCLX_SUCCESS && geo->relnum == NCLX_MDL_SHAPE) 
		{
			ix = 7;
			sh = (NCLX_mdl_shape *)geo;
			stfl = 0;
/*
........Get shape entity
*/
			for (i=0;i<sh->nents;i++)
			{
				switch(sh->shid[i].type)
				{
/*
...........Line
*/
				case NCLX_S_ENDPT:
					val.ksn[0] = 0; val.ksn[1] = 0; val.ksn[2] = 2; val.ksn[3] = 5;
					buf[ix] = val.asn; ix++;
					buf[ix] = sh->shid[i].pt[0]; ix++;
					buf[ix] = sh->shid[i].pt[1]; ix++;
					if (stfl <= 2)
					{
						st[stfl] = sh->shid[i].pt[0];
						st[stfl+1] = sh->shid[i].pt[1];
						stfl = stfl + 2;
					}
					en[0] = en[2];
					en[1] = en[3];
					en[2] = sh->shid[i].pt[0];
					en[3] = sh->shid[i].pt[1];
					break;
/*
...........Circle
*/
				case NCLX_S_ARC:
					val.ksn[0] = 0;
					if (sh->shid[i].arc.dir == NCLX_P_CLW) val.ksn[1] = -1;
					else val.ksn[1] = 1;
					val.ksn[2] = 6; val.ksn[3] = 7;
					buf[ix] = val.asn; ix++;
					buf[ix] = sh->shid[i].pt[0]; ix++;
					buf[ix] = sh->shid[i].pt[1]; ix++;
					buf[ix] = sh->shid[i].arc.cen[0]; ix++;
					buf[ix] = sh->shid[i].arc.cen[1]; ix++;
					buf[ix] = sh->shid[i].arc.rad; ix++;
					val.qsn[0] = sh->shid[i].arc.sang;
					val.qsn[1] = sh->shid[i].arc.eang;
					buf[ix] = val.asn; ix++;
					if (stfl <= 2)
					{
						st[stfl] = sh->shid[i].pt[0];
						st[stfl+1] = sh->shid[i].pt[1];
						stfl = stfl + 2;
					}
					en[0] = en[2];
					en[1] = en[3];
					en[2] = sh->shid[i].pt[0];
					en[3] = sh->shid[i].pt[1];
					break;
/*
...........Direction
*/
				case NCLX_S_DIRECTION:
					if (sh->shid[i].dir == NCLX_P_CLW) val.ksn[0] = CLW;
					else val.ksn[0] = CCLW;
					val.ksn[1] = 0; val.ksn[2] = 0; val.ksn[3] = 19;
					buf[ix] = val.asn; ix++;
					break;
/*
...........Post-processor command
*/
				case NCLX_S_VOCAB:
					val.ksn[0] = sh->shid[i].vocab;
					val.ksn[1] = 0;
					nw = sh->shid[i].value - 1;
					val.ksn[2] = nw;
					val.ksn[3] = 2000;
					buf[ix] = val.asn; ix++;
					for (j=0;j<nw;j++)
					{
						i++;
						if (sh->shid[i].type == NCLX_S_VOCAB) ityp = 0;
						else ityp = 1;
						i2 = sh->shid[i].vocab; r8 = sh->shid[i].value;
						clspwd(&buf[ix],&ityp,&i2,&r8); ix++;
					}
					buf[ix] = 0.; ix++;
					break;
				}
			}
/*
........Set up Shape header
*/
			if (sh->side == NCLX_P_IN) val.ksn[0] = IN;
			else val.ksn[0] = OUT;
			if (sh->dir == NCLX_P_LEFT) val.ksn[1] = LEFT;
			else val.ksn[1] = RIGHT;
			val.ksn[2] = ix;
			val.ksn[3] = 0;
			buf[0] = val.asn;
/*
...........Starting limit plane
*/
			vx = st[2] - st[0];
			vy = st[3] - st[1];
			tl = sqrt(vx*vx + vy*vy);
			buf[1] = -(vx / tl);
			buf[2] = vy / tl;
			buf[3] = buf[1]*st[0] + buf[2]*st[1];
/*
...........Ending limit plane
*/
			vx = en[0] - en[2];
			vy = en[1] - en[3];
			tl = sqrt(vx*vx + vy*vy);
			buf[4] = -(vx / tl);
			buf[5] = vy / tl;
			buf[6] = buf[4]*en[2] + buf[5]*en[3];
		}
	}
	else
	{
		status = gtshap1 (buf, nclkey);
	}
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : void gtcccv (nclkey, ix, keyout,ierr)
**       Return a composite component's key for pocket. If the component
**       should be reversed, the key is multiplied by -1.
**    PARAMETERS   
**       INPUT  : 
**          nclkey     - key of composite curve.
**          ix         - index of element
**       OUTPUT :  
**          keyout     - key of this entity, negative if reversed
**          ierr       - 0 if no error, non-zero if error
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gtcccv (nclkey, ix, keyout,ierr)
UM_int4 *nclkey, *keyout;
UM_int2 *ix, *ierr;
{
	int status,lix;
	struct UM_compcrv_rec e;
	struct UM_cid_rec cid;
	NCLX_mdl_cmp_entity *ent;
	NCLX_mdl_composite *cv;
	NCLX_mdl_struct *geo,*dat;

	lix = *ix;
/*
.....Internal OpenNCL geometry
*/
	if (NCLX_internal_geom)
	{
		status = NclxMdlFindGeo(*nclkey,&geo);
		if (status == 0 && geo->relnum == NCLX_MDL_COMPOSITE) 
		{
			cv = (NCLX_mdl_composite *)geo;
			if (lix < 1 || lix > cv->ncurve) goto failed;

			ent = (NCLX_mdl_cmp_entity *)&cv->cvid[lix-1];
			dat = (NCLX_mdl_struct *)ent->curve;
			if (ent->reverse)
				*keyout = -dat->key;
			else
				*keyout = dat->key;

			return;
		}
	}
	else
	{
		e.key = *nclkey;
		if (ur_retrieve_data_fixed (&e) == 0)
		{
			if (e.rel_num == UM_COMPCRV_REL)
			{
				if (lix < 1 || lix > e.no_cid) goto failed;

				if (ur_retrieve_data_varlist (e.key, 1, &cid, lix, 1) == 0)
				{
					if (cid.reverse)
						*keyout = -cid.crvid;
					else
						*keyout = cid.crvid;

					return;
				}
			}
		}
	}

failed:
	*ierr = 1;
	return;
}
