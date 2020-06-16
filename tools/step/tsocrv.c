/*********************************************************************
**    NAME         :  tsocrv.c
**       CONTAINS:
**         utp_out_compcrv
**         utp_out_trimcv
**			  utp_out_curve
**         utp_out_line
**         utp_out_circle
**         utp_out_conic
**         utp_out_bspline
**    COPYRIGHT 2014 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			tsocrv.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			10/27/16 , 15:00:38
*********************************************************************/

#include "ustdio.h"
#include "usysdef.h"
#include "udebug.h"
#include "nccs.h"
#include "mcrv.h"
#include "msrf.h"
#include "mdeval.h"
#include "mdrel.h"
#include "mgeom.h"
#include "modef.h"
#include "msol.h"
#include "ncl.h"
#include "nconst.h"
#include "tstep.h"
#include "tiges.h"
#include "tioconv.h"
#include "mdgenent.h"
#include "nclver.h"
#include "xenv1.h"
#include "riddle.h"
#include <time.h>

/*
.....can not define Sdeflab as such, in this way, Sdeflab can not re-assigned
.....it will cause error later
*/
//static char Sbuf[81920],*Sdeflab={"NONE"}; 
static char Sbuf[81920];
static char Sdeflab[] = {"NONE"};

extern UM_int2 NCL_ubas_unit;

/*********************************************************************
**    E_FUNCTION :  utp_out_compcrv(crv,flag)
**			Output a Composite curve entity to the STEP file as a
**       COMPOSITE_CURVE or EDGE_LOOP record.
**    PARAMETERS   
**       INPUT  : 
**          crv     = Composite curve entity to output.
**          flag    = UU_TRUE = Composite curve is a trimmed surface
**                    boundary, output an EDGE_LOOP command.
**                    UU_FALSE = Composite curve is output as a
**                    standalone wireframe entity (COMPOSITE_CURVE).
**       OUTPUT : none
**    RETURNS      :
**          Record number of COMPOSITE_CURVE or EDGE_LOOP record
**          output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_compcrv(crv,flag)
struct UM_compcrv_rec *crv;
UU_LOGICAL flag;
{
	int i,recno,ncid,status,*recs,nrec,irec,ofl;
	char tbuf[20],cbuf[20],label[NCL_MAX_LABEL_AND_SUBSCRIPT];
	UM_coord spt,ept;
	struct UM_crvdatabag ent;
	struct UM_cid_rec *cids;
/*
.....Initialize routine
*/
	cids = UU_NULL;
	recs = UU_NULL;
	recno = 0;
	ncid = nrec = 0;
/*
.....Get curve components
*/
	ncid = crv->no_cid;
	cids = (struct UM_cid_rec *)uu_malloc(sizeof(struct UM_cid_rec)*ncid);
	if (cids == UU_NULL) goto done;
	recs = (int *)uu_malloc(sizeof(int)*ncid);
	if (recs == UU_NULL) goto done;
	ur_retrieve_data_varlist(crv->key,1,cids,1,ncid);
/*
.....Set closed flag
*/
	if (!flag)
		strcpy(cbuf,".CONTINUOUS.");
/*
.....Loop through the curves
*/
	for (i=0;i<ncid;i++)
	{
/*
........Convert a Curve
*/
		if (flag)
		{
			ent.key = cids[i].crvid;
			status = ncl_retrieve_data_fixed(&ent);
			if (status != UU_SUCCESS) continue;
			irec = utp_out_curve(&ent,spt,ept,cids[i].reverse,UU_TRUE);
		}
		else
		{
			irec = utp_out_trimcv(cids[i].crvid);
			utp_get_logical_str(!(cids[i].reverse),tbuf);
			sprintf(Sbuf,"COMPOSITE_CURVE_SEGMENT(%s,%s,#%d);",cbuf,tbuf,irec);
			irec = utp_out_record(Sbuf);
		}
		if (irec != 0)
			recs[nrec++] = irec;
	}
/*
.....Output EDGE_LOOP/COMPOSITE_CURVE command
*/
	if (nrec > 0)
	{
		utp_format_label(crv->label,crv->subscr,label);
		if (flag)
			sprintf(Sbuf,"EDGE_LOOP('%s',(",label);
		else
			sprintf(Sbuf,"COMPOSITE_CURVE('%s',(",label);
		ofl = 0;
		utp_out_multiple_record(Sbuf,&ofl,&recno);
		for (i=0;i<nrec;i++)
		{
			if (i != 0) utp_out_multiple_record(",",&ofl,&recno);
			sprintf(tbuf,"#%d",recs[i]);
			utp_out_multiple_record(tbuf,&ofl,&recno);
		}
		if (ofl)
			strcpy(tbuf,"));");
		else
			strcpy(tbuf,"),.U.);");
		ofl = 2;
		utp_out_multiple_record(tbuf,&ofl,&recno);
	}
/*
.....End of routine
*/
done:;
	if (cids != UU_NULL) uu_free(cids);
	if (recs != UU_NULL) uu_free(recs);
	return(recno);
}

/*********************************************************************
**    E_FUNCTION :  utp_out_trimcv(key)
**			Output a Trimmed Curve entity to the STEP file.
**    PARAMETERS   
**       INPUT  : 
**          key      = Key of curve to output.
**       OUTPUT : none
**    RETURNS      :
**          Record number of trimmed curve (TRIMMMED_CURVE) output to
**          STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_trimcv(key)
UU_KEY_ID key;
{
	int recno,irec,jrec,krec,type;
	UU_LOGICAL sense;
	char tbuf[20],label[NCL_MAX_LABEL_AND_SUBSCRIPT],tbuf1[80],tbuf2[80];
	UU_REAL vals[2];
	UM_coord spt,ept;
	UM_transf tfmat;
	struct UM_line_rec *ln;
	struct UM_circle_rec *ci;
	struct UM_rbsplcrv_rec *cv;
	struct UM_crvdatabag ent;
/*
.....Initialize routine
*/
	recno = 0;
	vals[0] = vals[1] = 0.;
/*
.....Determine type of geometry to output
*/
	ent.key = key;
	ncl_retrieve_data_fixed(&ent);
	switch (ent.rel_num)
	{
/*
........Convert a line
*/
	case UM_LINE_REL:
		ln = (struct UM_line_rec *)&ent;
		irec = utp_out_line(ln);
		um_vctovc(ln->spt,spt);
		um_vctovc(ln->ept,ept);
		sense = UU_TRUE;
		type = 1;
		break;
/*
........Convert a circle
*/
	case UM_CIRCLE_REL:
		ci = (struct UM_circle_rec *)&ent;
		irec = utp_out_circle(ci);
		uc_retrieve_transf(ci->key,tfmat);
		um_get_endpts(ci,tfmat,spt,ept);
		vals[0] = 0.; vals[1] = ci->dang;
		sense = ci->dang > 0.;
		break;
/*
........Convert a curve
*/
	case UM_RBSPLCRV_REL:
		cv = (struct UM_rbsplcrv_rec *)&ent;
		irec = utp_out_bspline(cv,UU_FALSE);
		uc_retrieve_transf(cv->key,tfmat);
		um_get_endpts(cv,tfmat,spt,ept);
		vals[0] = cv->t0; vals[1] = cv->t1;
		sense = UU_TRUE;
		break;
/*
........Unrecognized curve type
*/
	default:
		recno = 0;
		break;
	}
/*
.....Output end point records
*/
	if (irec == 0) goto done;
	jrec = utp_out_point_ct(spt,Sdeflab,0);
	krec = utp_out_point_ct(ept,Sdeflab,0);
/*
.....Output TRIMMED_CURVE record
*/
	utp_format_label(ent.label,ent.subscr,label);
	utp_get_logical_str(sense,tbuf);
	if (type == 1)
		sprintf(Sbuf,"TRIMMED_CURVE('%s',#%d,(#%d),(#%d),%s,.CARTESIAN.);",label,
			irec,jrec,krec,tbuf);
	else
	{
		ul_format_numbers(&vals[0],1,STEP_ACY,UU_TRUE,tbuf1);
		ul_format_numbers(&vals[1],1,STEP_ACY,UU_TRUE,tbuf2);
		sprintf(Sbuf,"TRIMMED_CURVE('%s',#%d,(PARAMETER_VALUE(%s),#%d),(PARAMETER_VALUE(%s),#%d),%s,.PARAMETER.);",
			label,irec,tbuf1,jrec,tbuf2,krec,tbuf);
	}
	recno = utp_out_record(Sbuf);
/*
.....End of routine
*/
done:;
	return(recno);
}

/*********************************************************************
**    E_FUNCTION :  utp_out_curve(curve,spt,ept,reverse,flag)
**			Output a wireframe curve entity to the STEP file.  The curve
**       can be a spline, circle, line, composite curve, etc.
**    PARAMETERS   
**       INPUT  : 
**          curve   = Curve entity to output.
**       OUTPUT :
**          spt     = Starting point of curve.
**          ept     = Ending point of curve.
**          reverse = UU_TRUE = This curve is reversed.
**          flag    = UU_TRUE = This is part of a composite curve and the 
**                    EDGE_CURVE and ORIENTED_EDGE commands should be
**                    output.  UU_FALSE = Output curve only.
**    RETURNS      :
**          Record number of the (boundary) curve output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_curve(curve,spt,ept,reverse,flag)
struct UM_crvdatabag *curve;
UM_coord spt,ept;
UU_LOGICAL reverse,flag;
{
	int recno,irec[2],jrec,status;
	UU_REAL u;
	char tbuf[20];
	UM_transf tfmat;
	struct UM_evcrvout evout;
/*
.....Initialize routine
*/
	recno = 0;
/*
.....Output the outer boundary curve
*/
	switch (curve->rel_num)
	{
	case UM_LINE_REL:
		jrec = utp_out_line(curve);
		break;
	case UM_CIRCLE_REL:
		jrec = utp_out_circle(curve);
		break;
	case UM_CONIC_REL:
		jrec = utp_out_conic(curve);
		break;
	case UM_COMPCRV_REL:
		jrec = utp_out_compcrv(curve,UU_TRUE);
		break;
	case UM_RBSPLCRV_REL:
		jrec = utp_out_bspline(curve,UU_TRUE);
		break;
	}
/*
.....Calculate start and end points
*/
	uc_retrieve_transf(curve->key,tfmat);
	uc_init_evcrvout(curve,&evout);
	u = 0.; uc_evcrv(UM_POINT,u,curve,tfmat,&evout); um_vctovc(evout.cp,spt);
	u = 1.; uc_evcrv(UM_POINT,u,curve,tfmat,&evout); um_vctovc(evout.cp,ept);
/*
.....Output Curve records
........Start and end points
*/
	if (jrec != 0 && curve->rel_num != UM_COMPCRV_REL)
	{
		irec[0] = utp_out_point_vx(spt,Sdeflab,0);
		irec[1] = utp_out_point_vx(ept,Sdeflab,0);
/*
........EDGE_CURVE
*/
		utp_get_logical_str(UU_TRUE,tbuf);
		sprintf(Sbuf,"EDGE_CURVE('%s',#%d,#%d,#%d,%s);",Sdeflab,irec[0],irec[1],
			jrec,tbuf);
		jrec = utp_out_record(Sbuf);
/*
........ORIENTED_EDGE
*/
		utp_get_logical_str(!reverse,tbuf);
//		utp_get_logical_str(UU_TRUE,tbuf);
		sprintf(Sbuf,"ORIENTED_EDGE('%s',*,*,#%d,%s);",Sdeflab,jrec,tbuf);
		recno = utp_out_record(Sbuf);
	}
	else
		recno = jrec;
/*
.....End of routine
*/
done:;
	return(recno);
}

/*********************************************************************
**    E_FUNCTION :  utp_out_line(line)
**			Output a Line entity to the STEP file.
**    PARAMETERS   
**       INPUT  : 
**          line    = Line entity to output.
**       OUTPUT : none
**    RETURNS      :
**          Record number of line (LINE) output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_line(line)
struct UM_line_rec *line;
{
	int i,recno,irec[2];
	char label[NCL_MAX_LABEL_AND_SUBSCRIPT];
	UM_vector vec;
/*
.....Initialize routine
*/
	recno = 0;
/*
.....Store start point
*/
	irec[0] = utp_out_point_ct(line->spt,Sdeflab,0);
/*
.....Calculate and store vector
*/
	um_vcmnvc(line->ept,line->spt,vec);
	irec[1] = utp_out_vector(vec,Sdeflab,0);
/*
.....Output line record
*/
	utp_format_label(line->label,line->subscr,label);
	sprintf(Sbuf,"LINE('%s',#%d,#%d);",label,irec[0],irec[1]);
	recno = utp_out_record(Sbuf);
/*
.....End of routine
*/
done:;
	return(recno);
}

/*********************************************************************
**    E_FUNCTION :  utp_out_circle(circle)
**			Output a Circle entity to the STEP file.
**    PARAMETERS   
**       INPUT  : 
**          circle   = Circle entity to output.
**       OUTPUT : none
**    RETURNS      :
**          Record number of circle (CIRCLE) output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_circle(circle)
struct UM_circle_rec *circle;
{
	int recno,irec;
	char label[NCL_MAX_LABEL_AND_SUBSCRIPT],tbuf[80];
	UU_REAL rnum;
/*
.....Initialize routine
*/
	recno = 0;
/*
.....Store center point, normale vector, & starting vector
*/
	irec = utp_out_ptaxis(circle->center,circle->nvec,circle->svec);
/*
.....Output circle record
*/
	utp_format_label(circle->label,circle->subscr,label);
	UIO_LEN_INTTOEXT(circle->radius,rnum);
	ul_format_numbers(&rnum,1,STEP_ACY,UU_TRUE,tbuf);
	sprintf(Sbuf,"CIRCLE('%s',#%d,%s);",label,irec,tbuf);
	recno = utp_out_record(Sbuf);
/*
.....End of routine
*/
done:;
	return(recno);
}

/*********************************************************************
**    E_FUNCTION :  utp_out_conic(conic)
**			Output a Conic entity to the STEP file.
**    PARAMETERS   
**       INPUT  : 
**          conic    = Conic entity to output.
**       OUTPUT : none
**    RETURNS      :
**          Record number of conic (CONIC) output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_conic(conic)
struct UM_conic_rec *conic;
{
	int recno,irec;
	char label[NCL_MAX_LABEL_AND_SUBSCRIPT],tbuf[80];
	UU_REAL rnum[2];
/*
.....Initialize routine
*/
	recno = 0;
	if (conic->type != UM_ELLIPSE) goto done;
/*
.....Output center & axes
*/
	irec = utp_out_ptaxis(conic->tfmat[3],conic->tfmat[2],conic->tfmat[0]);
/*
.....Output conic record
*/
	utp_format_label(conic->label,conic->subscr,label);
	UIO_LEN_INTTOEXT(conic->invariants[0],rnum[0]);
	UIO_LEN_INTTOEXT(conic->invariants[1],rnum[1]);
	ul_format_numbers(rnum,2,STEP_ACY,UU_TRUE,tbuf);
	sprintf(Sbuf,"ELLIPSE('%s',#%d,%s);",label,irec,tbuf);
	recno = utp_out_record(Sbuf);
/*
.....End of routine
*/
done:;
	return(recno);
}

/*********************************************************************
**    E_FUNCTION :  utp_out_bspline(crv,flag)
**			Output an Rational B-spline curve entity to the STEP file.
**    PARAMETERS   
**       INPUT  : 
**          crv      = Curve entity to output.
**          flag     = UU_TRUE = Untrim (regenerate) curve prior to output.
**       OUTPUT :
**          crv      = Updated curve entity if it was trimmed.
**    RETURNS      :
**          Record number of curve (B_SPLINE_CURVE_WITH_KNOTS) output to
**          STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_bspline(crv,flag)
struct UM_rbsplcrv_rec *crv;
UU_LOGICAL flag;
{
	int i,m,n,recno,irec,*recs,npt,degu,nwgt,ofl;
	UU_LOGICAL wgtfl;
	UU_REAL *knots,*wgts;
	char label[NCL_MAX_LABEL_AND_SUBSCRIPT],tbuf[512],ubuf[20],vbuf[20];
	UM_coord *pts;
/*
.....Initialize routine
*/
	recno = 0;
	recs = UU_NULL;
	pts = UU_NULL;
	knots = UU_NULL;
	wgts = UU_NULL;
/*
.....Untrim curve by regenerating it
.....if necessary
*/
	if (flag && (crv->t0 != 0. || crv->t1 != 1.))
	{
		int npts,status;
		UU_REAL tol;
		UM_transf tfmat;
		UM_coord *pts;
		UM_vector *vs;
 		struct NCL_crvgen_rec *ptve, *cpt;
		struct UM_rbsplcrv_rec rbsp;
		UU_LIST cvpts,tangs;

		uc_retrieve_transf (crv->key,tfmat);
/*
........Evolve points on the curve
*/
		gettol(&tol);
		uu_list_init(&cvpts,sizeof(UM_coord),200,200);
		uu_list_init(&tangs,sizeof(UM_coord),200,200);
		npts = ncl_evolve_curve(crv,tfmat,tol,&cvpts,&tangs,UU_NULL,0);
		if (npts > 1)
		{
/*
........Build new curve
*/
			pts = (UM_coord *) UU_LIST_ARRAY (&cvpts);
			vs = (UM_coord *) UU_LIST_ARRAY (&tangs);

 			cpt = (struct NCL_crvgen_rec *) 
				uu_toolmalloc ((npts+1)*sizeof(struct NCL_crvgen_rec));
 			ptve = cpt;
 			for (i=0; i<npts; i++)
			{
				ncl_init_seg(ptve);
				ptve->x = pts[i][0];
				ptve->y = pts[i][1];
				ptve->z = pts[i][2];
				if (um_mag(vs[i]) > 99000.)
				{
					ptve->a = vs[i][0]; ptve->b = vs[i][1]; ptve->c = vs[i][2];
					ptve->inv = 1;
				}
	 			ptve++;
			} 
			status = ncl_interp_rbsp (npts, cpt, 0, &rbsp);
			if (status == UU_SUCCESS)
				uig_move(&rbsp,crv,
					sizeof(struct UM_rbsplcrv_rec)-UM_RBSPLCRV_BUFSZ);
			uu_toolfree(cpt);
			uu_list_free (&cvpts);
			uu_list_free (&tangs);
		}
	}
/*
.....Get Control Points
*/
	npt = crv->no_pt;
	pts = (UM_coord *)uu_malloc(sizeof(UM_coord)*npt);
	if (pts == UU_NULL) goto done;
	ur_retrieve_data_varlist(crv->key,2,pts,1,npt);

	n = npt; if (crv->no_t > n) n = crv->no_t;
	recs = (int *)uu_malloc(sizeof(int)*n);
	if (recs == UU_NULL) goto done;
/*
.....Get weights
*/
	nwgt = crv->no_wt;
	wgtfl = UU_FALSE;
	if (nwgt > 0)
	{
		wgts = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*nwgt);
		if (wgts == UU_NULL) goto done;
		ur_retrieve_data_varlist(crv->key,3,wgts,1,nwgt);
		for  (i=0;i<nwgt;i++)
		{
			if (wgts[i] != 1.)
			{
				wgtfl = UU_TRUE;
				break;
			}
		}
	}
/*
.....Output Control points
*/
	for (i=0;i<npt;i++)
		recs[i] = utp_out_point_ct(pts[i],Sdeflab,0);
/*
.....Setup bspline record
*/
	utp_format_label(crv->label,crv->subscr,label);
	degu = crv->k - 1;
	if (wgtfl)
		sprintf(Sbuf,"BOUNDED_CURVE() B_SPLINE_CURVE(%d",degu);
	else
		sprintf(Sbuf,"B_SPLINE_CURVE_WITH_KNOTS('%s',%d",label,degu);
	ofl = 0;
	utp_out_multiple_record(Sbuf,&ofl,&recno);
/*
........Append control point record numbers
*/
	for (i=0;i<npt;i++)
	{
		if (i==0)
			sprintf(tbuf,",(#%d",recs[i]);
		else
			sprintf(tbuf,",#%d",recs[i]);
		utp_out_multiple_record(tbuf,&ofl,&recno);
	}
	utp_out_multiple_record(")",&ofl,&recno);
/*
........Append flags
*/
	utp_get_logical_str(crv->closdinu,ubuf);
	utp_get_logical_str(UU_FALSE,vbuf);
	sprintf(tbuf,",.UNSPECIFIED.,%s,%s",ubuf,vbuf);
	utp_out_multiple_record(tbuf,&ofl,&recno);
/*
........Weights are specified
........Add surface command
*/
	if (wgtfl)
		utp_out_multiple_record(") B_SPLINE_CURVE_WITH_KNOTS(",&ofl,&recno);
/*
........Add knot values
*/
	npt = crv->no_t;
	knots = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*npt);
	if (knots == UU_NULL) goto done;
	ur_retrieve_data_varlist(crv->key,1,knots,1,npt);
/*
...........Count multiplicities
*/
	n = 1;
	m = 0;
	for (i=1;i<npt;i++)
	{
		if (knots[i] == knots[i-1]) n++;
		else
		{
			recs[m++] = n;
			n = 1;
		}
	}
	recs[m++] = n;
/*
...........Append multiplicities
*/
	if (!wgtfl) utp_out_multiple_record(",",&ofl,&recno);
	sprintf(tbuf,"(%d",recs[0]);
	utp_out_multiple_record(tbuf,&ofl,&recno);
	for (i=1;i<m;i++)
	{
		sprintf(tbuf,",%d",recs[i]);
		utp_out_multiple_record(tbuf,&ofl,&recno);
	}
	utp_out_multiple_record(")",&ofl,&recno);
/*
...........Append knot values
*/
	n = 0;
	for (i=0;i<m;i++)
	{
		utp_out_multiple_record(",",&ofl,&recno);
		if (i == 0) utp_out_multiple_record(",(",&ofl,&recno);
		ul_format_numbers(&knots[n],1,STEP_ACY,UU_TRUE,tbuf);
		utp_out_multiple_record(tbuf,&ofl,&recno);
		n += recs[i];
	}
	utp_out_multiple_record(")",&ofl,&recno);
/*
.....Append final flag
*/
	utp_out_multiple_record(",.UNSPECIFIED)",&ofl,&recno);
/*
........Append weights
*/
	if (wgtfl)
	{
		utp_out_multiple_record(
			"GEOMETRIC_REPRESENTATION_ITEM() RATIONAL_B_SPLINE_CURVE ((",
			&ofl,&recno);
		m = 0;
		ul_format_numbers(wgts,nwgt,STEP_ACY,UU_TRUE,tbuf);
		utp_out_multiple_record(tbuf,&ofl,&recno);
		utp_out_multiple_record(")) REPRESENTATION_ITEM(''))",&ofl,&recno);
	}
/*
.....Output bspline record
*/
	ofl = 2;
	utp_out_multiple_record(";",&ofl,&recno);
/*
.....End of routine
*/
done:;
	if (pts != UU_NULL) uu_free(pts);
	if (knots != UU_NULL) uu_free(knots);
	if (recs != UU_NULL) uu_free(recs);
	if (wgts != UU_NULL) uu_free(wgts);
	return(recno);
}
