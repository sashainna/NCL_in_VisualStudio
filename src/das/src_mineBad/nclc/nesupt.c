/*********************************************************************
**    NAME         :  nesupt.c
**       CONTAINS:
**       int dspent(nclkey, ncltype)
**       void dsprdw (nclkey,ietype)
**       puterm(buf)
**       nevdsp(nclkey)
**       sftype1(nclkey,ietype)
**       cvtype1(nclkey,ietype)
**       int evstup1(nclkey,isrf)
**       int ncl_cv_revers (ix,nkey)
**       int uevcrv(u,isrf,p,v)
**       int uevsrf(u,v,isrf,sv)
**       int wfcvln(nclkey, tol, sum, ierr)
**       int ncl_del_key(nclkey)
**       int isitwf1(nclkey,iret)
**       UU_LOGICAL ncl_setver
**       int wfstrg(nclkey,buf)
**       int ncl_label_wf(rel_num,label,key,status)
**       int ncl_label_cam(rel_num,label,key,status)
**       ncl_label_multiple()
**       int ncl_store_wf1(key)
**       int flopnv(ival,jval)
**       int ncl_ncltyp_to_relnum(type,rel_num)
**       int clnwf (keyin, rmx, keyout)
**       int ncl_copy_geom1 (e1, e2)
**       int ncl_copy_geom (e1, e2, bagsize)
**       ncl_fmtreal (n, a, str)
**       ncevolv(ix, tol, points, tangs)
**       ncevolvF(ix, tol, points, tangs)
**       ncl_fix_evol(itsk,npts,tol,points,tangs,uptr)
**       ncevolve2d(isf,told,maxpts,pts)
**		 ncl_compuvcv_to_xyzcv()
**		 ncl_uvcv_to_xyzcv()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nesupt.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       11/22/17 , 10:40:43
*********************************************************************/

#include <errno.h>
#include <ctype.h>
#include "usysdef.h"
#include "udebug.h"
#include "umath.h"
#include "uhep.h"
#include "usysg.h"
#include "ustdio.h"
#include "dinput.h"
#include "dasnog.h"
#include "dmark.h"
#include "class.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mdattr.h"
#include "mattrddl.h"
#include "mfort.h"
#include "mdebug.h"
#include "mdgenent.h"
#include "mdeval.h"
#include "mdunits.h"
#include "mcrv.h"
#include "msrf.h"
#include "mdclass.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclmodals.h"
#include "mdpick.h"
#include "dselmask.h"
#include "nclinp.h"
#include "mlab.h"
#include "nccs.h"
#include "nclvx.h"

static struct NCL_fixed_databag evalent[4];
static struct UM_evcrvout evout[4];
static struct UM_evsrfout evsrf[4];
static UM_transf evtfmat[4];
static UM_transf *evtfptr[4];

extern UU_LOGICAL ncl_where;
extern int NCL_ubcopy, NCL_copy_assoc;
extern UU_LOGICAL NCL_allcurve;

/*********************************************************************
**    E_FUNCTION     : int dspent(nclkey, ncltype)
**       Call the viewing subsystem to display an NCCS entity having
**       the specified UNIBASE key (NCLKEY).
**    PARAMETERS   
**       INPUT  : 
**          nclkey               UNIBASE key of entity to display
**          ncltype              NCCS type of entity (not used)
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
dspent(nclkey, ncltype)
   UM_int4 *nclkey;
   UM_int2 *ncltype;
   {
   int status = UU_FAILURE;
   UM_int2 num;
   struct NCL_fixed_databag e;

   uu_denter(UU_MTRC, (us,"dspent(nclkey=%x, ncltype=%d)",
      *nclkey, *ncltype));
   e.key = *nclkey;
	if (ur_retrieve_data_relnum(e.key, &e.rel_num) == 0)
	{
		status = wf_geom_type(e.rel_num);
		if (status == UU_SUCCESS)
			status = ncl_retrieve_data(&e, sizeof(e));
		if (status == UU_SUCCESS) status = uv_disp_entity(&e); 

		if (status == UU_SUCCESS)
		{
			ncl_gtssnum(nclkey,&num);
			if (num > 0) ncl_redisp_ss (&e);
		}
	}
failed:
   uu_dexit;
/*   uw_newline_flush();*/
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : void dsprdw (nclkey,ietype)
**       Erase display and tess lists, redisplay entity
**    PARAMETERS   
**       INPUT  : 
**          nclkey               UNIBASE key of entity to display
**          ietype               NCCS type of entity
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void dsprdw (nclkey,ietype)
UM_int4 *nclkey;
UM_int2 *ietype;
{
	int status = UU_FAILURE;
	struct NCL_fixed_databag e;
   
	e.key = *nclkey;
	status = ncl_retrieve_data_fixed (&e);
	if (status == UU_SUCCESS)
	{
		status = wf_geom_type (e.rel_num);
		if (status == UU_SUCCESS)
		{
			if (*ietype == NCLI_SURF)
				ncl_sfdisp_delete (&e);
			else
				ncl_wfdisp_delete (&e);

//			e.key = *nclkey;
			ncl_retrieve_data_fixed (&e);
			uv_disp_entity(&e);
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : puterm(buf)
**       Write a message (BUF of maximum length 80) to the error 
**       message area.
**    PARAMETERS   
**       INPUT  : 
**          buf                     character buffer (no terminator)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void puterm(buf)
   UM_f77_str_ptr buf;

   {
   int i;
   char msg[256];
   int buflen;
   UM_int2 ifl302 = 302;
   UM_int2 val302;
   char *cstr;

   uu_denter(UU_MTRC,(us,"puterm(buf=%x)", buf));

   cstr = UM_cstr_of_f77_str(buf);

   getifl(&ifl302, &val302);
   buflen = 79 + val302 * 20;
   for (i=buflen; i>0; i--) if (cstr[i] != ' ') break;
   buflen = i+1;
   for (i=0; i<buflen; i++) msg[i] = cstr[i];
   msg[buflen] = '\0';
   ud_prmerr(msg);

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : int nevdsp(nclkey)
**       Set NEVERDISPLAY attribute of the entity with key nclkey.
**    PARAMETERS   
**       INPUT  : 
**          nclkey               UNIBASE key of entity to set NEVERDISPLAY
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
nevdsp(nclkey)
   UM_int4 *nclkey;

   {
   int status;
   UU_KEY_ID key;

   uu_denter(UU_MTRC, (us,"nevdsp(nclkey=%x", *nclkey));

   key = *nclkey;
   status = ur_update_displayable(key, UM_NEVERDISPLAYABLE);

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int sftype1(nclkey,ietype)
**       Return NCLI surface type
**    PARAMETERS   
**       INPUT  : 
**          nclkey       UNIBASE key of surface
**       OUTPUT :  
**          ietype       NCLI type of surface
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
sftype1(nclkey,ietype)
   UM_int4 *nclkey;
   UM_int2 *ietype;
   {
   int status;
   UU_KEY_ID key;
   int rel_num;

   uu_denter(UU_MTRC, (us,"sftype1(nclkey=%x", *nclkey));

   key = *nclkey;
   if (ur_retrieve_data_relnum(key, &rel_num) != 0)
      status = UU_FAILURE;
   else
      {
      status = UU_SUCCESS;
      switch (rel_num)
         {
         case NCL_SURF_REL:
            *ietype = NCLI_REGSURF;
            break;
         case NCL_MESHSURF_REL:
            *ietype = NCLI_MESHSURF;
            break;
         case NCL_QUILTSURF_REL:
            *ietype = NCLI_QUILTSURF;
            break;
         case NCL_NETSF_REL:
            *ietype = NCLI_NETSF;
            break;
         case NCL_EVALSF_REL:
            *ietype = NCLI_EVALSF;
            break;
         case UM_RBSPLSRF_REL:
            *ietype = NCLI_RBSF;
            break;
         case NCL_TRIMSF_REL:
         /* ietype set to CAD relation number for now */
            *ietype = NCL_TRIMSF_REL;
            break;
         case NCL_REVSURF_REL:
            *ietype = NCL_REVSURF_REL;
            break;
         default:
            *ietype = 0;
            status = UU_FAILURE;
            break;
         }
      }

   uu_dexit;

   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int cvtype1(nclkey,ietype)
**       Return Unicad curve type
**    PARAMETERS   
**       INPUT  : 
**          nclkey       UNIBASE key of curve
**       OUTPUT :  
**          ietype       Relation number of curve
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
cvtype1(nclkey,ietype)
   UM_int4 *nclkey;
   UM_int2 *ietype;
   {
   int status;
   UU_KEY_ID key;
   int rel_num;

   uu_denter(UU_MTRC, (us,"sftype1(nclkey=%x", *nclkey));

   key = *nclkey;
   if (ur_retrieve_data_relnum(key, &rel_num) != 0)
      status = UU_FAILURE;
   else
      {
      status = UU_SUCCESS;
      switch (rel_num)
         {
         case UM_LINE_REL: case NCL_LINE_REL:
            *ietype = NCL_LINE_REL;
            break;
         case UM_CIRCLE_REL: case NCL_CIRCLE_REL:
            *ietype = NCL_CIRCLE_REL;
            break;
         case NCL_CURVE_REL:
            *ietype = NCLI_CURVE;
            break;
         case UM_COMPCRV_REL:
            *ietype = UM_COMPCRV_REL;
            break;
         case UM_UVCVONSF_REL:
            *ietype = UM_UVCVONSF_REL;
            break;
         case UM_RBSPLCRV_REL:
            *ietype = UM_RBSPLCRV_REL;
            break;
         case NCL_EVALCV_REL:
            *ietype = NCL_EVALCV_REL;
            break;
         case UA_TEXT_REL:
            *ietype = UA_TEXT_REL;
            break;
         default:
            *ietype = 0;
            status = UU_FAILURE;
            break;
         }
      }

   uu_dexit;

   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int evstup1(nclkey, isrf)
**       Get the entity with key nclkey into global structure evalent and call
**       um_setup_evaluator to set up global evaluation record evout.
**       Call this routine before calling uevcrv() or uevsrf() with a
**       new entity.
**    PARAMETERS   
**       INPUT  : 
**          nclkey               UNIBASE key of entity to set up.
**          isrf                 Index into evalent structure.
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
evstup1 (nclkey, isrf)
   UM_int4 *nclkey;
   UM_int2 *isrf;
   {
   int ix, iclass, status;

   uu_denter(UU_MTRC, (us,"evstup1(nclkey=%x)", *nclkey));

   ix = *isrf -1;
/*   if (*nclkey == evalent[ix].key) goto done; */
   evalent[ix].key = *nclkey;
   status = ncl_retrieve_data (&evalent[ix], sizeof(struct NCL_fixed_databag));
   status = um_get_transformation(evalent[ix].key, evtfmat[ix]);
   evtfptr[ix] = UM_DEFAULT_TF;
   if (!um_is_idmat(evtfmat[ix])) evtfptr[ix] = &evtfmat[ix];  
   iclass = uc_super_class(evalent[ix].rel_num);
   if (iclass ==  UM_CURVE_CLASS)
      status = uc_init_evcrvout (&evalent[ix], &evout[ix]);
   else if (iclass ==  UM_SURFACE_CLASS)
      status = uc_init_evsrfout (&evalent[ix], &evsrf[ix]);
   else
      status = UU_FAILURE;
  
   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_cv_revers (ix,nkey)
**       Replace the curve currently stored as the ix entity with
**       its reversed copy. Used for the PROFIL command.
**    PARAMETERS   
**       INPUT  : 
**          ix                   Index into evalent structure.
**       OUTPUT :  
**          nkey                 key of new curve
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cv_revers (ix,nkey)
int ix;
UU_KEY_ID *nkey;
{
	int iclass,isze,status;
	struct NCL_fixed_databag crv1;
	struct UM_rbsplcrv_rec *rcrv;
	UU_LOGICAL ldelsp;
	UM_int2 lfl_77;

	ldelsp = UU_FALSE;
	status = UU_FAILURE;
		
	iclass = uc_super_class(evalent[ix].rel_num);
	if (iclass != UM_CURVE_CLASS) return (status);

	if (evalent[ix].rel_num == NCL_CURVE_REL)
	{
		isze = um_curve_size (&evalent[ix]);
		ldelsp = (isze > 0);
		if (!ldelsp) return (status);
		um_allocate_curve (&rcrv,isze); 
		status = um_rbcrv_frmnclcrv (&evalent[ix],rcrv);
		if (status != UU_SUCCESS) goto done;
	}
					
	lfl_77 = 1;
	stunlb (&lfl_77);
	crv1.key = NULLKEY;
	if (ldelsp)
		status = ncl_copy_rbsp1 (rcrv,&crv1);
	else
	{
		isze = sizeof(struct NCL_fixed_databag);
		status = uc_copy (&evalent[ix],&crv1,isze);
	}
	stunlb (&lfl_77);

	if (crv1.key > NULLKEY) *nkey = crv1.key;
	if (status == UU_SUCCESS)
	{
		evalent[ix].key = crv1.key;
		status = ncl_retrieve_data (&evalent[ix],isze);
		if (status == UU_SUCCESS)
			status = uc_init_evcrvout (&evalent[ix], &evout[ix]);
		if (status == UU_SUCCESS)
			status = uc_reverse_curve (&evalent[ix]);
	}

done:
	if (ldelsp) uu_toolfree (rcrv);
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int uevcrv(u,isrf,p,v)
**       Call um_evcrv() to return the point and first derivative
**       (i.e. slope) at parameter u of unicad curve currently in evalent.
**    PARAMETERS   
**       INPUT  : 
**          u                    parameter of curve to evaluate
**          isrf                 index into evalent structure
**       OUTPUT :  
**          p                    point at u
**          v                    first derivative at u
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uevcrv(u,isrf,p,v)
   UM_real8 *u;
   UM_int2 *isrf;
   UM_real8 p[3];
   UM_real8 v[3];

   {
   int ix, status, i;
   UM_param uu;

   uu_denter(UU_MTRC, (us,"uevcrv(u=%f)", *u));

   uu = *u;
   if (uu < 0.) uu = 0.;
   if (uu > 1.) uu = 1.;
   ix = *isrf -1;
   status = uc_evcrv(UM_FRSTDERIV, uu, &evalent[ix], evtfptr[ix], &evout[ix]);
   for (i=0; i<3; i++)
      {
      p[i] = evout[ix].cp[i];
      v[i] = evout[ix].dcdu[i];
      }
   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : int uevsrf(u,v,isrf,sv)
**       Call um_evsrf() to return the point and first derivative
**       (i.e. slope) at parameters u & v of unicad surface currently in
**       evalent.
**    PARAMETERS   
**       INPUT  : 
**          u                    u parameter of surface to evaluate
**          v                    v parameter of surface to evaluate
**          isrf                 Index into evalent structure.
**       OUTPUT :  
**          sv[0:2]              point at u,v
**          sv[3:5]              first derivative in u
**          sv[6:8]              first derivative in v
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uevsrf (u,v,isrf,sv)
   UM_real8 *u;
   UM_real8 *v;
   UM_int2 *isrf;
   UM_real8 sv[9];

   {
   int ix, status, i;
   UM_param uu, vv;
/* ## */
#ifdef debug
    struct tbuffer {         
       int proc_user_time;   
       int prc_system_time;  
       int child_user_time;  
       int child_system_time;
       };                    
    struct tbuffer buffer;   
   static FILE fd;
   static int first = 1;
#endif
/* ## */
   uu_denter(UU_MTRC, (us,"uevsrf(u=%f,v=%f)", *u, *v));

   uu = *u;
   vv = *v;
   if (uu < 0.) uu = 0.;
   if (uu > 1.) uu = 1.;
   if (vv < 0.) vv = 0.;
   if (vv > 1.) vv = 1.;
   ix = *isrf -1;
/* ## */
#ifdef debug
    times (&buffer);          
    i = buffer.proc_user_time;
#endif
/* ## */
   status = uc_evsrf(UM_FRSTDERIV, uu, vv, &evalent[ix], evtfptr[ix], 
                      &evsrf[ix]);
/* ## */
#ifdef debug
    times (&buffer);              
    uu = buffer.proc_user_time - i;
    uu = uu / 100.;
    if (first) {
       first = 0;
       fd = fopen ("debug","w");
    }
    fprintf (fd, "%12.4f\n",uu);
#endif
/* ## */
   for (i=0; i<3; i++)
      {
      sv[i] = evsrf[ix].sp[i];
      sv[i+3] = evsrf[ix].dsdu[i];
      sv[i+6] = evsrf[ix].dsdv[i];
      }
   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : int wfcvln (nclkey, tol, sum, ierr)
**       Calculate the length of a wire frame curve.
**    PARAMETERS   
**       INPUT  : 
**          nclkey               UNIBASE key of curve.
**          tol                  tolerance to use in calc.
**       OUTPUT :  
**          sum                  length of curve
**          ierr                 = 0 success
**                               = 1 error
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
wfcvln (nclkey, tol, sum, ierr)
UM_int4 *nclkey;
UM_real8 *tol, *sum;
UM_int2 *ierr;
{
	struct NCL_fixed_databag e;
	int status;
	UU_REAL toler,sc169,length,um_getpolylen();
	UM_int2 idx, mm,isc,ifl264,ifl;
	UM_coord pt0;
	UM_param u, du;
	int i, n,np;
	struct UM_evcrvout evout;
	UM_transf tfmat,*tf;
	struct UM_crvdatabag crv;
	UU_LIST points,tvc;
	UM_coord *p;
/*
.....Initialize routine
*/
	*ierr = 1;
	*sum = 0.;
	status = UU_FAILURE;
/*
.....Use tolerance scheme for V9.0 and above
*/
	isc = 169;
	getsc(&isc,&sc169);
	if (sc169 > 8.999)
	{
		ifl = 264;
		getifl(&ifl,&ifl264);
		toler = *tol * .1;
		if (ifl264 == 1) toler = toler / 25.4;
		toler = (toler >= 1.e-4) ? toler : 1.e-4;

		crv.key = *nclkey;
		status = ncl_retrieve_data_fixed(&crv);

		if(status != UU_SUCCESS) return (UU_FAILURE);

		uu_list_init (&points, sizeof(UM_coord), 200, 200);
		uu_list_init (&tvc,  sizeof(UM_coord), 200, 200);

		if(ncl_itsa_compcrv(&crv))
		{
			np = ncl_evolve_composite_curve(&crv,toler,&points,&tvc,UU_NULL,0);
		}
		else
		{
			tf = &tfmat;
			status = ncl_trimsrf_get_tf (&crv,&tf);
			if(!status)
				np = ncl_evolve_curve (&crv,tf,toler,&points,&tvc,UU_NULL,0);
		}

		if (status != UU_SUCCESS) goto Done;

		np = UU_LIST_LENGTH(&points);
		if(np <= 1) 
		{
			status = UU_FAILURE;
			goto Done;
		}

		p  = (UM_coord *)UU_LIST_ARRAY(&points);

		length = um_getpolylen (np,p);
		*sum = length;
		*ierr = 0;
Done:;
		uu_list_free (&points);
		uu_list_free (&tvc);
	}
/*
.....Use number of points method
.....for previous versions
*/
	else
	{
		e.key = *nclkey;
		if (ncl_retrieve_data (&e, sizeof(e)) == UU_SUCCESS)
		{
			status = um_get_transformation(e.key, tfmat);
			uc_init_evcrvout (&e, &evout);
			u = 0.;
			status = uc_evcrv(UM_POINT, u, &e, tfmat, &evout);
			um_vctovc(evout.cp,pt0);
			n = 100;
			du = n;
			du = 1./du;
			for (i=0, u=du; i<n && status == UU_SUCCESS; i++, u+=du)
			{
				if (u>1.) u = 1.;
				status = uc_evcrv(UM_POINT, u, &e, tfmat, &evout);
				*sum += UM_DCCCC(pt0,evout.cp);
				um_vctovc(evout.cp,pt0);
			}
		}
	}
	if (status == UU_SUCCESS)
	{
		*ierr = 0;
		idx = 264;
		getifl (&idx, &mm);
		if (mm == 1) *sum *= 25.4;
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_del_evkey (nclkey)
**       Zero saved key of entity in evalent if it is equal to key of entity
**       currently being deleted.
**    PARAMETERS   
**       INPUT  : 
**          nclkey               key of entity being deleted
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_del_evkey (nclkey)
   UU_KEY_ID nclkey;
   {
   int i;

   for (i=0; i<3; i++) 
      if (evalent[i].key == nclkey) evalent[i].key = 0;

   return(0);
   }

/*********************************************************************
**    E_FUNCTION     : int isitwf1(nclkey,iret)
**       Determine if an entity is a wireframe or ncl entity.
**    PARAMETERS   
**       INPUT  : 
**          nclkey               Key of entity.
**       OUTPUT :  
**          iret                 0 if NCL, 1 if wf.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
isitwf1(nclkey,iret)
   UM_int4 *nclkey;
   UM_int2 *iret;

   {
   int status;
   int rel_num;
   UU_KEY_ID key;

   uu_denter(UU_MTRC, (us,"isitwf1(nclkey=%d)", *nclkey));

   status = UU_FAILURE;
   key = *nclkey;
   if (ur_retrieve_data_relnum(key, &rel_num) == 0)
      {
      status = UU_SUCCESS;
      *iret = 1;
      if (rel_num != NCL_EVALCV_REL && rel_num != NCL_EVALSF_REL)
         {
         if (ncl_geom_type(rel_num) == UU_SUCCESS) *iret = 0;
         }
      }

   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL ncl_setver (nver)
**       Determine if a version flag is active.
**    PARAMETERS   
**       INPUT  : 
**          nver   version number to check. for example, nver=95 means:
**                 check the currently set version number for NCL95 or older;
**                 ncl_setver(95) is true iff lv95 is true in mover.f
**       OUTPUT :  none
**    RETURNS      : 
**       UU_TRUE iff the version flag is active
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_setver (nver)
int nver;
{
	UM_int2 idx = 169;
	UM_real8 ver;
	int lver;

	getsc(&idx,&ver);
	lver = (int)(10.*ver + 0.5);

	return (lver <= nver);
}

/*********************************************************************
**    E_FUNCTION     : int wfstrg(nclkey,buf)
**       Return a string describing a wireframe entity.
**    PARAMETERS   
**       INPUT  : 
**          nclkey               Key of entity.
**       OUTPUT :  
**          buf                  Description of entity.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
wfstrg(nclkey,buf)
   UM_int4 *nclkey;
   UM_f77_str_ptr buf;

   {
   int status;
   int rel_num;
   UU_KEY_ID key;
   char *cstr;
   struct NCL_trimsf_rec tsf;
   struct UM_polyline_rec pcv;

   uu_denter(UU_MTRC, (us,"wfstrg(nclkey=%d)", *nclkey));

   status = UU_FAILURE;
   cstr = UM_cstr_of_f77_str(buf);
   key = *nclkey;
   if (ur_retrieve_data_relnum(key, &rel_num) == 0)
      {
      status = UU_SUCCESS;
      switch (rel_num)
         {
         case UM_POINT_REL:
            strcpy(cstr,"Wire Frame Point");
            break;
         case UM_LINE_REL:
            strcpy(cstr,"Wire Frame Line");
            break;
         case UM_CIRCLE_REL:
            strcpy(cstr,"Wire Frame Circle");
            break;
         case UM_CONIC_REL:
            strcpy(cstr,"Wire Frame Conic");
            break;
         case UM_COMPCRV_REL:
            strcpy(cstr,"Wire Frame Composite Curve");
            break;
         case UM_RBSPLCRV_REL:
            strcpy(cstr,"Wire Frame Rational B-Spline");
            break;
         case UM_AGCRV_REL:
            strcpy(cstr,"Wire Frame Curve");
            break;
         case UM_AGSRF_REL:
            strcpy(cstr,"Wire Frame Surface");
            break;
         case UM_AGSHELL_REL:
            strcpy(cstr,"Wire Frame Shell");
            break;
         case NCL_EVALCV_REL:
            strcpy(cstr,"PDES Evaluated Curve");
            break;
         case NCL_EVALSF_REL:
            strcpy(cstr,"PDES Evaluated Surface");
            break;
         case UM_RBSPLSRF_REL:
            strcpy(cstr,"Rational B-Spline Surface");
            break;
         case UM_UVCVONSF_REL:
            strcpy(cstr,"UV Curve on Surface");
            break;
         case UM_POLYLINE_REL:
			pcv.key = key;
			ncl_retrieve_data_fixed(&pcv);
			sprintf(cstr,"%d point(s) ", pcv.no_pt);
            strcat(cstr,"Polyline");
            break;
         case NCL_TRIMSF_REL:
			tsf.key = key;
			ncl_retrieve_data_fixed(&tsf);
            strcpy(cstr,"Trimmed Surface");
			if (tsf.drive_type == 0) strcat(cstr," (Base)");
			else strcat(cstr," (Face)");
            break;
         default:
            strcpy(cstr,"Wire Frame Unknown Entity");
            break;
         }
      }

   cstr[strlen(cstr)] = ' ';

   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_label_wf(rel_num,label,isub,key,nclstatus)
**       Create a label for wf entity.
**    PARAMETERS   
**       INPUT  : 
**          rel_num              rel_num of entity.
**          key 	             unibase key of entity used if called from ur_lp02b.
**          nclstatus            status of store in ranfile used if called from ur_lp02b.
**       OUTPUT :  
**          label                Generated label for entity.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS :
**       Label is entered in ranfile.
**    WARNINGS     : none
*********************************************************************/
int
ncl_label_wf(rel_num,label, isub, key, nclstatus)
   int rel_num;
   char *label;
   int *isub;
   int key;
   UM_int2 *nclstatus;

   {
   int status, i;
   UM_f77_str ncllabel;
   char str[NCL_MAX_LABEL+1];
   UM_int2 idst;
   UM_int4 nclkey,i4sub;
   UU_LOGICAL llab;

   uu_denter(UU_MTRC, (us,"ncl_label_wf(rel_num=%d)", rel_num));
/*
.....In 2D Wireframe
.....Do not label geometry
.....Bobby  -  2/28/94
*/
 if (UM_2d3d_mode == UM_2D)
 {
  status = UU_SUCCESS;
  strcpy(label,"#");
  *isub = 0;
  return(status);
 }

   nclkey = key;

/*
... If ncl_where=UU_TRUE this routine was called from ur_lp02b when merging
... a unibase. vp 4/25/94 added llab flag to support 'get' from unibase
*/
   llab  = ncl_where || (NCL_ubcopy == 1);

   if (llab)
      strncpy (str, label, NCL_MAX_LABEL);
   i4sub = *isub;

   UM_init_f77_str(ncllabel, str, NCL_MAX_LABEL);
   status = UU_SUCCESS;

   ncl_get_type (rel_num, &idst);
   

/* 
...If name modal form is set to subscript for CAM reset the 
...CAD geometry label to prefix.
........removed as of 24-apr-95 by vp to allow subscripts in CADD
*/
/*   n = UM_labelmdl.rel[rel_num];
   if (UM_labelmdl.issub[n] == 1)
      {
      UM_labelmdl.issub[n]=0;
      changed = UU_TRUE;
      }  */
   if (NCL_allcurve) idst = 8;
   labwf(&idst,UM_addr_of_f77_str(ncllabel),&i4sub, &nclkey, &llab, nclstatus);
/*   if (changed == UU_TRUE)
      {
      UM_labelmdl.issub[n]=1;
      changed = UU_FALSE;
      }
*/
   for (i=0;i<NCL_MAX_LABEL-1;i++) label[i] = str[i];
   label[NCL_MAX_LABEL-1] = '\0';
   *isub = i4sub;

   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_label_cam(rel_num,label,ncsub,nclkey,nclstatus)
**       Create a label for cam entity.
**       This routine is called from ur_lp02b when merging a unibase in the case
**       when the entity name is previously defined and Auto rename option is
**       selected.
**    PARAMETERS   
**       INPUT  : 
**          rel_num              rel_num of entity.
**          key 	             unibase key of entity used if called from ur_lp02b.
**          nclstatus            status of store in ranfile used if called from ur_lp02b.
**       OUTPUT :  
**          label                Generated label for entity.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS :
**       Label is entered in ranfile.
**    WARNINGS     : none
*********************************************************************/
int
ncl_label_cam(rel_num,label,ncsub,key,nclstatus)
   int rel_num;
   char *label;
   UM_int4 *ncsub;
   UM_int4 key;
   UM_int2 *nclstatus;

   {
   int status, i;
   UM_f77_str ncllabel;
   char str[NCL_MAX_LABEL];
   UM_int2 idst;
   UM_int4 nclkey;

   uu_denter(UU_MTRC, (us,"ncl_label_cam(rel_num=%d)", rel_num));

	nclkey = key;
	strncpy(str,label,NCL_MAX_LABEL);
   UM_init_f77_str(ncllabel, str, NCL_MAX_LABEL);
   status = UU_SUCCESS;

	ncl_get_type(rel_num,&idst);
   	labcam(&idst, UM_addr_of_f77_str(ncllabel), ncsub, &nclkey, nclstatus);
   	for (i=0;i<NCL_MAX_LABEL-1;i++) label[i] = str[i];
   	label[NCL_MAX_LABEL-1] = '\0';

   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_label_multiple(inlab,insub,outlab,outsub)
**       Generates labels for multiple entities created from
**       a single command (i.e. DECOMP, etc.).  The labeling follows
**       the following rules.
**
**         insub = 0  - 'label' does not have a numeric suffix.
**                      A numeric suffix will be added to the label.
**                      For example, 'abc' will create the labels
**                      'abc1', 'abc2', etc.
**
**                 +  - A subscript has been specified with the label
**                      and will be incremented for each entity.
**
**                 -  - The label has a numeric suffix (which should
**                      be removed from the label prior to calling
**                      this routine) and it will be incremented for
**                      each entity.
**    PARAMETERS   
**       INPUT  : 
**          inlab     = Base label of entity.
**          insub     = Base subscript/suffix of entity.
**       OUTPUT :  
**          insub     = Incremented subscript/suffix of entity.
**          outlab    = Actual label of entity.
**          outsub    = Actual subscript of entity.
**    RETURNS      : 
**       UU_SUCCESS if no error, a valid NCL error number if an error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_label_multiple(inlab,insub,outlab,outsub)
char *inlab,*outlab;
int *insub,*outsub;
{
	int status,i;
	UM_int2 redef,found;
	UM_int4 isub4;
	UU_KEY_ID nclkey;
	char label[NCL_MAX_LABEL];
	UM_f77_str flab;;
/*
.....Generate prefix style label
*/
	if (*insub == 0) *insub = 1;
	if (*insub < 0)
	{
		i = 0 - (*insub);
		sprintf(label,"%s%d",inlab,i);
		*outsub = 0;
		*insub =  *insub - 1;
	}
/*
.....Generate subscript style label
*/
	else
	{
		strcpy(label,inlab);
		*outsub = *insub;
		*insub = *insub + 1;
	}
/*
.....See if label already exists
*/
	strcpy(outlab,label);
	UM_init_f77_str(flab,label,NCL_MAX_LABEL);
	for (i=strlen(label);i<NCL_MAX_LABEL;i++) label[i] = ' ';
	isub4 = *outsub;
	chklab(flab,&nclkey,&isub4,&found,&redef);
/*
.....Label exists
.....Delete the existing entity of CANON/ON
.....Otherwise return error
*/
	status = UU_SUCCESS;
	if (found == 1)
	{
		if (redef == 1)
		{
			status = uc_delete(nclkey);
			if (status != UU_SUCCESS) status = 89;
		}
		else
		{
			status = 384;
			goto done;
		}
	}
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_store_wf1(key)
**       Store a wf entity label in the ranfile. Variables required
**       must have been previously set (perhaps by a call to labwf).
**    PARAMETERS   
**       INPUT  : 
**          key                  key of entity.
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_store_wf1(key)
   UU_KEY_ID key;

   {
   UM_int4 nclkey;
   int status;

   uu_denter(UU_MTRC, (us,"ncl_store_wf1(key=%d)", key));

   status = UU_SUCCESS;
/*
.....Don't store drawing entities in ranfile
.....Bobby  -  2/28/94
*/
 if (UM_2d3d_mode == UM_3D)
  {
   nclkey = key;
   strwf1(&nclkey);
  }

   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : int flopnv(ival,jval)
**       Convert fortran file open errors to VMS values.
**    PARAMETERS   
**       INPUT  : 
**          ival        Error to be converted
**       OUTPUT :  
**          jval        Converted error
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
flopnv (ival, jval)
   UM_int2 *ival;
   UM_int2 *jval;

   {
   int status;
   int i,j;

   uu_denter(UU_MTRC, (us,"flopnv(ival=%d)",*ival));
   status = UU_SUCCESS;
   i = j = *ival;

#if UU_COMP == UU_SUN
   switch (i)
      {
      case 0:          /* No error */
         j = 0;
         break;
		case 1017:
      case 117:        /* New file exists. (No VMS value - use SUN value) */
         j = 117;
         break;
		case 1018:
      case 118:        /* No file */
         j = 29;
         break;
      default:
         j = 30;
         status = UU_FAILURE;
         break;
      }
#endif
#if UU_COMP == UU_CIM
   switch (i)
      {
      case 0:          /* No error */
         j = 0;
         break;
      case 669:        /* No file */
         j = 29;
         break;
      case 668:        /* New file exists. (No VMS value - use SUN value) */
         j = 117;
         break;
      default:
         j = 30;
         status = UU_FAILURE;
         break;
      }
#endif
#if UU_COMP == UU_IRIS4D
   switch (i)
      {
      case 0:          /* No error */
         j = 0;
         break;
      case 2:          /* No file */
         j = 29;
         break;
/*
.....Addded for RS6000. Paul
.....11/25/91
*/
#ifdef UU_RS6000
      case 6:
         j = 29;
         break;
      case 107:
         j = 117;
         break;
#endif

      case 126:        /* New file exists. (No VMS value - use SUN value) */
         j = 117;
         break;
      default:
         j = 30;
         status = UU_FAILURE;
         break;
      }
#endif
#if UU_COMP == UU_HPUX
   switch (i)
      {
      case 0:          /* No error */
         j = 0;
         break;
      case 908:          /* No file */
         j = 29;
         break;
      case 918:        /* New file exists. (No VMS value - use SUN value) */
         j = 117;
         break;
      default:
         j = 30;
         status = UU_FAILURE;
         break;
      }
#endif
   *jval=j;
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_ncltyp_to_relnum(type,rel_num)
**       Determine the rel_num form relation ncltype of entity.
**    PARAMETERS   
**       INPUT  : 
**          type:                ncltype of entity
**       OUTPUT :  
**          rel_num              Relation number of entity.
**    RETURNS      : 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_ncltyp_to_relnum(type,rel_num)
int *rel_num;
UM_int2 type;

{
   switch (type)
      {
      case NCLI_POINT:
         *rel_num = NCL_POINT_REL;
         break;
      case NCLI_VECTOR:
         *rel_num = NCL_VECTOR_REL;
         break;
      case NCLI_LINE:
         *rel_num = NCL_LINE_REL;
         break;
      case NCLI_CIRCLE:
         *rel_num = NCL_CIRCLE_REL;
         break;
      case NCLI_CURVE:
      case NCLI_UVCVONSF:
         *rel_num = NCL_CURVE_REL;
         break;
      case NCLI_PLANE:
         *rel_num = NCL_PLN_REL;
         break;
      case NCLI_MATRIX:
         *rel_num = NCL_MATRIX_REL;
         break;
      case NCLI_SURF:
      case NCLI_REGSURF:
      case NCLI_MESHSURF:
      case NCLI_QUILTSURF:
      case NCLI_NETSF:              /*all surfaces have same label */
         *rel_num = NCL_SURF_REL;
         break;
      case NCLI_PATERN:
         *rel_num = NCL_PATERN_REL;
         break;
      case NCLI_SHAPE:
         *rel_num = NCL_SHAPE_REL;
         break;
      case NCLI_POINTVEC:
         *rel_num = NCL_POINTVEC_REL;
         break;
      case NCLI_DATAST:
         *rel_num = NCL_DATAST_REL;
         break;
      case NCLI_TEXTVAR:
         *rel_num = NCL_TEXTVAR_REL;
         break;
      case NCLI_NOTE:
         *rel_num = UA_TEXT_REL;
         break;
		case NCLI_SYMBOL:
			*rel_num = UB_SYMBOL_REL;
			break;
		case NCLI_INSTANCE:
			*rel_num = UB_INSTANCE_REL;
			break;
		case NCLI_SOLID:
			*rel_num = UM_SOLID_REL;
			break;
      default:
         *rel_num = -1;
         break;
      }
	return (0);
}
/*********************************************************************
**    E_FUNCTION     : int clnwf (nclkey, rmx, entkey)
**       Clone a wf entity
**    PARAMETERS   
**       INPUT  : 
**          keyin      - key of entity
**          rmx        - NCL matrix
**       OUTPUT :
**          keyout     - key of copied & transformed entity
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
clnwf (keyin, rmx, keyout)
   UM_int4 *keyin, *keyout;
   UM_real8 rmx[12];

   {
   int status, i, j, irmx, itsk;
   UM_transf tfmat;
   struct UC_entitydatabag e1,e2,*eptr;
   UU_REAL det;

   uu_denter(UU_MTRC,(us,"clnwf (key=%x, rmx=%x)", keyin, rmx));

   status = UU_FAILURE;

	itsk = *keyout;
   *keyout = 0;

   for (i=0, irmx=0; i<3; i++) 
      {
      for (j=0; j<4; j++) tfmat[j][i] = rmx[irmx++];
      }
   
   e1.key = *keyin;
/*
.....initialized they key to zero to avoid UMR in um_create_geom
*/
   e2.key = 0;
/*
.....'uc_retrieve_data' must be used instead of
.....'ncl_retrieve_data' so that a clean copy of the entity
.....is retrieved (ncl_retrieve_data returns with pointers
.....to the entity's variable lists in the Unibase)
.....This caused a problem with translating annotation text
.....Bobby  -  8/22/07
*/
   if (uc_retrieve_data(&e1, sizeof(e1)) == 0)
	{
		if (itsk == 3) 
			eptr = &e1;
		else 
		{
			eptr = &e2; 
			uc_copy(&e1, eptr, sizeof(e1)); 
		}
/*
.....Do not copy xyzbylst when doing clone/move
*/
		if (eptr->rel_num == NCL_TRIMSF_REL && !um_is_idmat(tfmat))		
			status = ncl_lst_delete(WHOLE_BOUNDARY_LIST,&eptr->key);

		if (uc_transform(eptr, tfmat, UU_TRUE) == 0)
		{
			*keyout = eptr->key;
			status = UU_SUCCESS;
		}
/*
.....check if tfmat mirror matrix
*/
		um_determinant(tfmat, &det);
		if (det < 0.)
		{
/*
......we need do somthing about mirror matrix
*/
			ncl_solid_conv(eptr->key);
		}
/*
.....Clone/Move of Symbol Instances
.....Will not be displayed in 'dspent'
.....Since they are not considered wireframe geometry
.....So they must be displayed here
*/
		if (eptr->rel_num == UB_INSTANCE_REL)
		{
			uv_disp_entity(eptr);
		}
	}

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_copy_geom1 (e1, e2)
**       Copy the fixed part of a NCL geometric entity.
**    PARAMETERS   
**       INPUT  : 
**          e1       - pointer to entity to copy. (only key need be valid)
**       OUTPUT :
**          e2       - pointer to copied entity.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_copy_geom1 (e1, e2)
   struct NCL_fixed_databag *e1, *e2;
   {
   int status, ncltyp;
   struct UM_transf_rec tran;
   UU_KEY_ID key;
   UM_int2 i2;
   struct  UC_attributedatabag attr1, attr2;
   uu_denter(UU_MTRC,(us,"ncl_copy_geom1 (key=%x)", e1->key));

   status = UU_FAILURE;

   key = e1->key;

     tran.key = key;
     tran.rel_num = UM_TRANSFORM_REL;
     if (ur_retrieve_transf(&tran) == 0)
       {
       if (ncl_get_type(e2->rel_num, &i2) == UU_SUCCESS)
         {
         ncltyp = i2;
			if (!NCL_copy_assoc) ncl_reset_assoc (e2);
         if (ncl_create_entity (e2, ncltyp) == UU_SUCCESS)
           {
           tran.key = e2->key;
           if (ur_update_transf(&tran) == 0) status = UU_SUCCESS;
            uc_retrieve_attr(e1->key, &attr1);
            uc_retrieve_attr(e2->key, &attr2);
            attr2.layer       = attr1.layer;
            attr2.color       = attr1.color;
            attr2.pen         = attr1.pen;
            attr2.line_style  = attr1.line_style;
            attr2.line_weight = attr1.line_weight;
            attr2.line_width  = attr1.line_width;
            if (e1->label[0] != '@') attr2.displayable = attr1.displayable;
            attr2.selectable  = attr1.selectable;
            attr2.label_on    = attr1.label_on;
            ur_update_attr(&attr2);
           }
         }
       }
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_copy_geom (e1, e2, bagsize)
**       Copy the fixed part of a NCL geometric entity.
**    PARAMETERS   
**       INPUT  : 
**          e1       - pointer to entity to copy. (only key need be valid)
**       OUTPUT :
**          e2       - pointer to copied entity.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_copy_geom(e1, e2, bagsize)
   struct NCL_fixed_databag *e1, *e2;
   int bagsize;
   {
   int status;
   UU_KEY_ID key;
   uu_denter(UU_MTRC,(us,"ncl_copy_geom (key=%x)", e1->key));

   status = UU_FAILURE;

   key = e1->key;
   e2->key = key;
   if (ur_retrieve_data_fixed (e2) == UU_SUCCESS)
	   status = ncl_copy_geom1 (e1,e2);

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_fmtreal (n, a, str)
**       Convert an array of reals into a string "a,b,c,...".
**    PARAMETERS   
**       INPUT  : 
**          n        - number of reals.
**          a        - array of reals.
**       OUTPUT :  
**          str      - string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_fmtreal (n, a, str)
int n;
UU_REAL *a;
char *str;
{
/*
	int i, j, l;
	char *s1;
.....ncl_sprintf formats reals to the accuracy and alignment
.....that the user chooses, so replace all this code by calling
.....ncl_sprintf instead. JLS 2/14/00
	s1 = str;
	for (i=0;i<n;i++)
	{
		sprintf(s1,"%f",a[i]);
		l = strlen(s1);
		for (j=0; j<5 && s1[l-1] == '0'; j++) l-- ;
		s1[l] = ',';
		s1 = s1+l+1;
	}
	s1[-1] = '\0';
*/
	ncl_sprintf(str,a,n);

	return (0);
}
 
/*********************************************************************
**    E_FUNCTION     : int isitag(nclkey,iret)
**       Determine if an entity is a wireframe or ncl entity.
**    PARAMETERS  
**       INPUT  : 
**          nclkey               Key of entity.
**       OUTPUT :  
**          iret                 0 if not AG, 1 if AG.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none  
**    WARNINGS     : none
*********************************************************************/
int
isitag(nclkey,iret)
   UM_int4 *nclkey;
   UM_int2 *iret;
 
   {
   int status;
   int rel_num;
   UU_KEY_ID key;
 
   uu_denter(UU_MTRC, (us,"isitwf(nclkey=%d)", *nclkey));
 
   status = UU_FAILURE;
   key = *nclkey;
   if (ur_retrieve_data_relnum(key, &rel_num) == 0)
      {
      status = UU_SUCCESS;
      *iret = 0;
         if (rel_num == UM_AGCRV_REL ||
             rel_num == UM_AGSRF_REL ||
             rel_num == UM_AGSHELL_REL) *iret = 1;
      }
   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_transform_pn (eptr,tranfmat,store)
**   Transform a pointvector by applying the specified 4X3 transformation
**   and update UNIBASE iff store == UU_TRUE.
**    PARAMETERS
**       INPUT  :
**    eptr          pointer to the entity to be transformed
**          tranfmat      the 4x3 transformation matrix
**    store     TRUE iff UNIBASE is to be updated here.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_transform_pn (eptr,tranfmat,store)
 struct NCL_patern_rec *eptr;
 UM_transf    tranfmat;
 UU_LOGICAL store;

 {
  int i,m,j,npts;

  uu_denter(UU_MTRC,(us,"ncl_transform_pn(key:%d,tfmat:%x,store:%d)",
     eptr->key, tranfmat,store));

  m    = 3 * eptr->pntype;
  npts = eptr->no_patpnt / m;
  j    = 0;
  for (i=0; i<npts; i++)
     {
/*
.....Get actual point data
*/
      um_cctmtf(&eptr->patpnt[j], tranfmat, &eptr->patpnt[j]);
      if (eptr->pntype == 2)
         um_vctmtf(&eptr->patpnt[j+3], tranfmat, &eptr->patpnt[j+3]);

      j  = j + m;
     }
/*
...do not need update since patpnt is a pointer where we just 
...replaced the contents
...
 if (store)
  um_update_geom(eptr, UM_DEFAULT_TF); */
 uu_dexit;
 return (UU_SUCCESS);
 }

/*********************************************************************
**    E_FUNCTION     : int ncl_transform_nclcv (eptr,tranfmat,store)
**   Transform NCL curve by applying the specified 4X3 transformation
**   and update UNIBASE if store == UU_TRUE.
**    PARAMETERS
**       INPUT  :
**    eptr          pointer to the entity to be transformed
**          tranfmat      the 4x3 transformation matrix
**    store     TRUE iff UNIBASE is to be updated here.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_transform_nclcv (eptr,tranfmat,store)
 struct NCL_curve_rec *eptr;
 UM_transf    tranfmat;
 UU_LOGICAL store;

 {
  struct NCL_segment_rec seg;
  int i,status;
  UU_KEY_ID key;

  uu_denter(UU_MTRC,(us,"ncl_transform_nclcv(key:%d,tfmat:%x,store:%d)",
     eptr->key, tranfmat,store));

  key  = eptr->key;
  for (i=1; i<=eptr->no_segment; i++)
     {
      if (ur_retrieve_data_varlist (key, 2, &seg, i, 1) != 0)
          status = UU_FAILURE;
      else status = ncl_transf_cvseg (&seg, tranfmat);
      if (status == UU_SUCCESS && store)
          if (ur_update_data_varlist (key, 2, &seg, i, 1) != 0)
             status = UU_FAILURE;
     } 
  uu_dexit;
  return (status);
 }

/*********************************************************************
**    E_FUNCTION     : int ncl_transf_cvseg (eptr,tfmat)
**   Transform NCL curve segment by applying the specified transformation
**    PARAMETERS
**       INPUT  :
**    eptr          pointer to the entity to be transformed
**    tfmat         the 4x3 transformation matrix
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_transf_cvseg (seg,tfmat)
  struct NCL_segment_rec *seg;
  UM_transf tfmat;
  {
   int j, status;
   UM_vector vec;

   uu_denter(UU_MTRC,(us,"ncl_transf_cvseg()"));

   status = UU_SUCCESS;

   um_cctmtf(seg->point,tfmat,seg->point);
   for (j=0; j<3; j++) vec[j] = seg->delta[j];
   um_vctmtf(vec,tfmat,vec);
   for (j=0; j<3; j++) seg->delta[j] = (float)vec[j];

   uu_dexit;
   return (status);
  }

/*********************************************************************
**    E_FUNCTION     : int ncevolv(ix, tol, points, tangs)
**    C wrapper for calling ncl_evolve_curve() from Fortran or C
**    PARAMETERS
**       INPUT  :
**    ix         curve ptr
**    tol        chord height tolerance
**       OUTPUT :
**    points        list of evolved points
**    tangs	        list of tangent vectors
**    RETURNS      : number of evaluated pts 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncevolv(ix, tol, points, tangs)
	int ix;
	UU_REAL tol;
	UU_LIST *points, *tangs;
{
	int n1,status;
	UU_LIST uptr;

	uu_list_init (points, sizeof(UM_coord), 100, 100);
	uu_list_init (tangs,  sizeof(UM_coord), 100, 100);
	uu_list_init (&uptr,  sizeof(UM_coord), 100, 100);

	n1 = ncl_evolve_curve (&evalent[ix],evtfptr[ix],tol,points,tangs,&uptr,1);
	status = ncl_fix_evol (1,&evalent[ix],evtfptr[ix],&evout[ix],&n1,tol,
		points,tangs,&uptr);

	uu_list_free (&uptr);

	if (status == UU_SUCCESS)
		return (n1);
	else
		return (UU_FAILURE);
}

/*********************************************************************
**    E_FUNCTION     : int ncevolvF(ix, tol, points, tangs)
**    C wrapper for calling ncl_evolve_curve() from Fortran - to
**    create an NCL curve
**    PARAMETERS
**       INPUT  :
**    ix         curve ptr
**    tol        chord height tolerance
**       OUTPUT :
**    points        list of evolved points
**    tangs	        list of tangent vectors
**    RETURNS      : number of evaluated pts 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncevolvF(ix, tol, points, tangs)
int ix;
UU_REAL tol;
UU_LIST *points, *tangs;
{
	UU_LIST uptr;
	int i,j,n1,status;
	UM_coord *pts,*uu;
	UM_vector *vs, w0,w1;
	UU_REAL co,dd,du;
	UU_REAL eps = 25.*tol*tol;
	UM_int2 idx = 169;
	UM_real8 ver;

	uu_list_init (points, sizeof(UM_coord), 100, 100);
	uu_list_init (tangs,  sizeof(UM_coord), 100, 100);
	uu_list_init (&uptr,  sizeof(UM_coord), 100, 100);

	n1 = ncl_evolve_curve (&evalent[ix],evtfptr[ix],tol,points,tangs,&uptr,1);

	getsc(&idx,&ver);

	if (ver > 9.450)
	{
/*
..... weed out short segments that are on a line with the previous or next
.....
..... This interferes with creating sharp cornered splines but works good
..... for NCL curves, which is the reason for this routine.
*/
		pts = (UM_coord *) UU_LIST_ARRAY(points);
		vs = (UM_vector *) UU_LIST_ARRAY(tangs);
		uu = (UM_coord *) UU_LIST_ARRAY(&uptr);

		for (i = 0; i < n1-1; i++)
		{
			j = -1;
			du = uu[i+1][0] - uu[i][0];
			if (du < 0.001)
			{
				dd = UM_SQDIS (pts[i+1],pts[i]);
				if (dd < eps)
				{
					um_unitvc (vs[i],w0); um_unitvc (vs[i+1],w1); 
					co = um_dot (w0, w1);
					if (co > 0.99995)
					{
						if (i == n1-2)
							j = i;
						else
							j = i+1;
					}
					else if (i > 0)
					{
						um_unitvc (vs[i-1],w0); um_unitvc (vs[i],w1); 
						co = um_dot (w0, w1);
						if (co > 0.99995)
							j = i;
					}
					if (j >= 0 && j < n1)
					{
						uu_list_delete (points,j,1);
						uu_list_delete (tangs,j,1);
						uu_list_delete (&uptr,j,1);
						pts = (UM_coord *) UU_LIST_ARRAY(points);
						vs = (UM_vector *) UU_LIST_ARRAY(tangs);
						uu = (UM_coord *) UU_LIST_ARRAY(&uptr);
						i--; n1--;
					}
				}
			}
		}
	}

	status = ncl_fix_evol (1,&evalent[ix],evtfptr[ix],&evout[ix],&n1,tol,
		points,tangs,&uptr);

	uu_list_free (&uptr);

	if (status == UU_SUCCESS)
		return (n1);
	else
		return (UU_FAILURE);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_fix_evol(itsk,crv,cvmat,crvout,npts,
**                                      tol, points, tangs,uptr)
**    Fix the points/tangents data to use for curve fitting. 
**    If a segment between a point and its next is big, we insert 2 points 
**    (with tangents) close to the ends of the segment.
**    PARAMETERS
**       INPUT  :
**        itsk     - 1 = Add points only add ends of straight segments.
**                   2 = Add multiple points along straight segments
**                       (used for projections)
**        crv      - pointer to curve entity
**        cvmat    - transformation matrix
**        crvout   - (pointer to) curve evaluator
**        npts     - number of points
**        tol      - chord height tolerance
**        points   - list of points
**        tangs	 - list of tangent vectors
**        uptr	    - list of u-values
**       OUTPUT :
**        npts              -   new number of points 
**        points,tangs,uptr -   changed lists
**
**    RETURNS      : UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_fix_evol(itsk,crv,cvmat,crvout,npts, tol, points, tangs,uptr)
struct NCL_fixed_databag *crv;
UM_transf cvmat;
struct UM_evcrvout *crvout;
int itsk,*npts;
UU_REAL tol;
UU_LIST *points,*tangs,*uptr;
{
	int i,j, nn,n1,status;
	UM_coord *pts,*uu,uvp;
	UM_vector *vs, w0,w1,vv0;
	UU_REAL co,beta,ang,dd,du,d1;
	struct UM_rbsplsrf_rec surf;
	struct UM_uvcvonsf_rec *uvcv;
	UM_transf sfmat;
/*
.....Initialize routine
*/
	n1 = *npts;
	uvp[1] = uvp[2] = 0.;
	pts = (UM_coord *) UU_LIST_ARRAY(points);
	vs = (UM_vector *) UU_LIST_ARRAY(tangs);
	uu = (UM_coord *) UU_LIST_ARRAY(uptr);
/*
.....Get base surface if UV curve on surface
*/
	if (crv->rel_num == UM_UVCVONSF_REL)
	{
		uvcv = (struct UM_uvcvonsf_rec *)crv;
		surf.key = uvcv->bskey;
		status = ncl_retrieve_data_fixed(&surf);
		if (status == UU_SUCCESS) uc_retrieve_transf(surf.key,sfmat);
		if (status != UU_SUCCESS) return(status);
	}
/*
..... the change below is for QAR 92198 - to ensure a flat ending stretch in
..... a closed curve is evolved with enough points
*/
	if (itsk == 2 && UM_DCCCC(pts[0],pts[n1-1]) < tol)
		um_vctovc(vs[0],vs[n1-1]);

	for (i = 0; i < n1-1; i++)
	{
		du = uu[i+1][0] - uu[i][0];
		if (du < tol || (itsk != 1 && du < 0.02)) continue;
		um_vcmnvc (pts[i+1],pts[i],vv0);
		dd = um_mag (vv0);
		if (dd < 10.*tol) continue;
		for (j = 0; j < 3; j++) vv0[j] /= dd;
		d1 = UM_MAG (vs[i+1]);
		um_unitvc (vs[i],w0); um_unitvc (vs[i+1],w1); 
		co = um_dot (w0, w1);
/*
..... if alpha is the angle between the two vectors w0 and w1, then
..... (dd/2 * tan(alpha/4)) is an estimated deviation for this segment
*/                
		beta = atan2 (2.*tol,dd);
		ang = acos(co);
		if ((co > 0.99995 || ang <= 4.*beta) && (itsk == 1 || dd < tol*100.))
		{
			if (d1 > 99000.) um_vctovc (w1,vs[i+1]);
			continue;
		}

		nn = (int)floor (dd/tol + 0.5);
		co = um_dot(vv0,w0);
/*
..... if the next point is along the current tangent vector and the
..... next tangent vector is at an angle from the previous one, we
..... guess a corner, and delineate it if itsk=1
..... In the test case there was no corner with co=0.99875
*/
		if (itsk == 1)
		{
			uvp[0] = uu[i][0] + 0.5*du;
			status = uc_evcrv(UM_FRSTDERIV,uvp[0],crv,cvmat,crvout);
			if (status != UU_SUCCESS) return (UU_FAILURE);
			n1++;
			uu_list_insert (points,i+1,crvout->cp);
			um_unitvc (crvout->dcdu,w0);
			um_vctmsc (w0,100000.,w0);
			uu_list_insert (tangs,i+1,w0);
			uu_list_insert (uptr,i+1,uvp);
			i--;
		}
		else
		{
			nn = nn/100 + 1;
			if (nn > 1)
			{
				du /= nn;
				for (j = 0; j < nn-1; j++)
				{
					uvp[0] = uu[i][0] + du; 
					status = uc_evcrv(UM_FRSTDERIV,uvp[0],crv,cvmat,crvout);
					if (status != UU_SUCCESS) return (UU_FAILURE);
					i++; n1++;
					uu_list_insert (points,i,crvout->cp);
					uu_list_insert (tangs,i,crvout->dcdu);
					uu_list_insert (uptr,i,uvp);
					uu = (UM_coord *) UU_LIST_ARRAY(uptr);
				}
			}
		}
		pts = (UM_coord *) UU_LIST_ARRAY(points);
		vs = (UM_vector *) UU_LIST_ARRAY(tangs);
		uu = (UM_coord *) UU_LIST_ARRAY(uptr);
	}

	*npts = n1;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : int ncevolve2d(isf, told, maxpts, pts)
**    C wrapper for calling ncl_evolve_curve() from Fortran's shapre
**    PARAMETERS
**       INPUT  :
**    isf	     curve ptr
**    told	     chord height tolerance
**       OUTPUT :
**    pts            point buffer ptr allocated in F77
**    RETURNS      : number of evaluated pts req'd to meet told
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ncevolve2d(isf, told, maxpts, pts)
	UM_int2 *isf;		/* curve ptr index         */
	UM_real8 *told;	/* chord height tolerance  */
	UM_int2 *maxpts;	/* number of points allocated*/
	UM_real8 *pts;		/* points array allocated*/
{

   /*
   ** pts (storage for point array is passed in after allocation 
   ** from shapre)
   ** max size of point array is passed in via maxpts
   ** npts (actual points evaluated) is returned
   */

	int ipts,n1,npts,ix,j,i;
	UU_LIST cvpoint;
	UM_real8  *pbuf;/*  point buffer in UU_LIST format */
	UM_real8  d, x0, x1;

	UM_2Dcoord prev, cur;

	UU_REAL tol = *told;

/* set up bag ptr in isf-th element of evalent */
	ix = *isf - 1;

/* evolve curve as a sequence of points within given tolerance */

	uu_list_init (&cvpoint, sizeof(UM_coord), 100, 100);

	n1 = ncl_evolve_curve (&evalent[ix],evtfptr[ix],tol,&cvpoint,UU_NULL,UU_NULL,0);

	if (n1 > 0)
	{
/* check for enough room in caller point array */ 
		npts = n1;
		if (npts > *maxpts) npts = *maxpts;
/* recover pointer to point UU_LIST */
		pbuf = (UU_REAL *) UU_LIST_ARRAY(&cvpoint);
		for (i=0;i<npts;i++) ncl_mcstowcs(0,&pbuf[i*3],&pbuf[i*3]);

/* recover point array to pass back to caller */
		x0 = pbuf[0]; x1 = pbuf[3*npts-3];

		if (x0 > x1)
		{
			pts[0] = prev[0] = x0; 
			pts[1] = prev[1] = pbuf[1];
			pbuf+=3;
			j = 1;

			for (ipts = 1; ipts <= npts-1; ipts++)
			{
				cur[0] = pbuf[0]; cur[1] = pbuf[1];
				d = UM_DIST_2D(prev, cur);
				if (d < *told)
				{
					pbuf+=3;
					n1--;
					continue;
				}
				prev[0] = cur[0]; prev[1] = cur[1];
				pts[2*j] = pbuf[0];
				pts[2*j+1] = pbuf[1];
				pbuf+=3;
				j++;
			}
		}
		else
		{
			pts[0] = prev[0] = x1; 
			pbuf += 3*npts-3;
			pts[1] = prev[1] = pbuf[1];
			pbuf-=3;
			j = 1;

			for (ipts = 1; ipts <= npts-1; ipts++)
			{
				cur[0] = pbuf[0]; cur[1] = pbuf[1];
				d = UM_DIST_2D(prev, cur);
				if (d < *told)
				{
					pbuf-=3;
					n1--;
					continue;
				}
				prev[0] = cur[0]; prev[1] = cur[1];
				pts[2*j] = pbuf[0];
				pts[2*j+1] = pbuf[1];
				pbuf-=3;
				j++;
			}
		}
	}

   uu_list_free (&cvpoint);

   return (n1);
}

/*********************************************************************
**    E_FUNCTION : ncl_compuvcv_to_xyzcv(eptr1, eptr2)
**       This function converts a composite curve to a compostie 
**		surface spline and stored it as a new entity.
**			Note all subcurves are copied as well.
**    PARAMETERS   
**       INPUT  : 
**          eptr1		pointer to the composite curve entityto be copied
**       OUTPUT :  
**          eptr2		pointer to the copy of the composite curve.
**    RETURNS      : 0 if no error. 
**    SIDE EFFECTS : puts copy in UNIBASE.
**    WARNINGS     : none
*********************************************************************/
int ncl_compuvcv_to_xyzcv(eptr1, eptr2)
struct UM_compcrv_rec *eptr1;
struct UC_entitydatabag *eptr2;
{ 	
	struct UM_rbsplcrv_rec rbcv, *rbptr;
	struct UM_uvcvonsf_rec *uvcv;	
	struct NCL_fixed_databag c2;
	struct UM_compcrv_rec *eptr3;

	int icv,j,ncv,rev,status;
	struct NCL_fixed_databag ccrv1;

	rbptr = &rbcv;

	eptr3 = (struct UM_compcrv_rec *) eptr2;
	
	ur_setup_data(eptr1->rel_num, eptr3, sizeof(struct UC_entitydatabag));
	strcpy (eptr3->label, "");
	eptr3->subscr = 0;
	eptr3->arclen = eptr1->arclen;
	eptr3->planar = eptr1->planar;
	eptr3->open = eptr1->open;
	eptr3->continuity = eptr1->continuity;
	eptr3->fcolor = eptr1->fcolor;
	eptr3->no_cid = eptr1->no_cid;
	eptr3->t0 = eptr1->t0;
	eptr3->t1 = eptr1->t1;

	status = ncl_compcrv_getnents (eptr1, &ncv);
	for (icv = 0; icv < ncv; icv++)
	{
		status = ncl_compcrv_getelm(eptr1,icv,&ccrv1,&rev);
		if (status != UU_SUCCESS) 
			return status;
/*
.....Copy uvcvonsf_rec to rbcv structure
*/
		uvcv = (struct UM_uvcvonsf_rec *)&ccrv1;
		status = ncl_cp_struct_uvcv_rbcv (uvcv,&rbptr);

		for (j = 0; j < 3*(rbptr->no_pt); j = j+3)
		{
			rbptr->pt[j+2] = 0.0;
		}

		status = ncl_create_rbsp1 (rbptr->k,rbptr->no_pt,rbptr->no_wt,
									rbptr->t,rbptr->pt,rbptr->wt,
									rbptr->t0,rbptr->t1,&c2); 
		strcpy(c2.label,"@UN    ");
		eptr3->cid[icv].crvid = c2.key;

		eptr3->cid[icv].reverse = eptr1->cid[icv].reverse;
		eptr3->cid[icv].endparam = eptr1->cid[icv].endparam; 
		if(status == UU_FAILURE)
			return(status);
	}  

	eptr3->closdinu = eptr1->closdinu;

	status = ncl_create_entity (eptr3, NCLI_CURVE);

	strcpy(eptr3->label,"@UN    ");
	ncl_store_wf1(eptr3->key);
	ur_update_displayable(eptr3->key, UM_NEVERDISPLAYABLE);

	return status;
}

/*********************************************************************
**    E_FUNCTION: ncl_uvcv_to_xyzcv(ix,sfkey,tran,delkey)
**       convert uv-curves into xyz curves
**    PARAMETERS   
**       INPUT  : 
**			nclkey		-input curve key
**			ix			-evalent index
**       OUTPUT :  
**			sfkey		-surface key if curve on surface
**			tfmax		-transformation matrix of surface in MCS
**			delkey		-temporary xyzcv key to be deleted later
**    RETURNS      : 
**			0			-input is xyz curve
**			1			-input is surface spline
**			2			-input is composite surface spline
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_uvcv_to_xyzcv(ix,sfkey,tran,delkey)
int ix;
UU_KEY_ID *sfkey;
struct UM_transf_rec *tran;
UU_KEY_ID *delkey;
{
	struct UM_rbsplcrv_rec rbcv, *rbptr;
	struct UM_uvcvonsf_rec *uvcv;	
	struct NCL_fixed_databag c2;

	int j,isf,status;
	isf = ix + 1;

	rbptr = &rbcv;
/*
.....Check if curve is on surface
*/
	if (!ncl_itsa_cv_onsf(&evalent[ix]))		
		return 0;
	
/*
.....Get the surface key
*/
	*sfkey = ncl_get_uvcvonsf_sfkey(&evalent[ix]);

/*
.....Get the transformation matrix
*/
	if (*sfkey > NULLKEY)
		status = ncl_get_uvcvonsf_transf(&evalent[ix],tran);

	if (ncl_itsa_compcrv(&evalent[ix]))
	{
		status = ncl_compuvcv_to_xyzcv(&evalent[ix], &c2);
	}
	else
	{
/*
.....Copy uvcvonsf_rec to rbcv structure
*/
		uvcv = (struct UM_uvcvonsf_rec *) &evalent[ix];
		status = ncl_cp_struct_uvcv_rbcv (uvcv,&rbptr);

		for (j = 0; j < 3*(rbptr->no_pt); j = j+3)
		{
			rbptr->pt[j+2] = 0.0;
		}

		status = ncl_create_rbsp1 (rbptr->k,rbptr->no_pt,rbptr->no_wt,
							rbptr->t,rbptr->pt,rbptr->wt,
							rbptr->t0,rbptr->t1,&c2); 	
	}

/*
.....Put the entity with rb spline key into global structure evalent with index ix
*/
	evstup1 (&c2.key, &isf);
	*delkey = c2.key;

	return status;
}
