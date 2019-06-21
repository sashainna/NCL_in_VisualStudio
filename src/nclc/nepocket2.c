/*********************************************************************
**    NAME         :  nepocket2.c
**       CONTAINS:  Routines for Advanced pocket.
**           int pklini ()
**           int pklfin ()
**           int pklnew (lptr)
**           int pklwrt ()
**           int pklsto (itype, mtype, buf)
**           int ncl_poksto ()
**           int pkldel (lptr)
**           int pklild (lptr, ier)
**           int pkllod (buf, itype, ilast, ier)
**           int ncl_pok_load ()
**           int pklept (buf)
**           int pklnpt (lptr, npts)
**           int pklspt (buf)
**           int ncl_pockgen_newstart()
**           void nclf_pockgen_helix()
**           void pokopn
**    COPYRIGHT 1994 (c) Mills Data Systems Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nepocket2.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:41
*********************************************************************/

/* #include "usysdef.h" */
/* #include "mfort.h" */
/* #include "mdrel.h" */
/* #include "mcrv.h" */
/* #include "ulist.h" */
/* #include "ncl.h" */
#include "ncldef.h"
#include "modef.h"
#include "nclpockofs.h"

char *uu_lsinsrt(), *uu_lsend(), *uu_lsnext(), *uu_lsnew(), *uu_lsdel();

int NCL_pok_data_count = 0;
int NCL_pok_data_indx  = 0;
int NCL_pok_mtype = 0;
int NCL_pok_cont  = 0;
int NCL_pok_itype = 0;
char *NCL_pok_ptr = UU_NULL;
UM_real8 *NCL_pok_data_ptr = UU_NULL;
#define NCL_MAX_POK_BUF 120
UM_real8 NCL_pok_buf[NCL_MAX_POK_BUF];
char *NCL_pok_list = UU_NULL;
UU_LIST NCL_pok_entpts;
UM_real8 *NCL_pok_entpts_ptr = UU_NULL;
int NCL_pok_entpts_count = 0;
int NCL_pok_inx = 0;
struct NCL_pok_ent
  {
  int key;
  char *plist;
  };
/* extern int WATERLINE_DEBUG; */

static void S_poksto();

/*********************************************************************
**    E_FUNCTION     : int pklini ()
**       Initialize the advanced pocket list handling routines.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
pklini ()
   {
   
   int status;
   status = UU_SUCCESS;

   if (NCL_pok_list != UU_NULL) pklfin();
   NCL_pok_mtype = 0;
   NCL_pok_inx = 0;
   NCL_pok_list = uu_lsnew();
/*
   char ux_getenv();
   if (vptr = ux_getenv("WATRLN"))
   {
		strncpy(buf,vptr,1);
		if (strncmp(buf,"1",1) == 0) WATERLINE_DEBUG = 1;
		else WATERLINE_DEBUG = 0;
   }
*/
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int pklfin ()
**       Deallocate the advanced pocket lists.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
pklfin ()
   {
   
   int status;
   struct NCL_pok_ent *p1;

   status = UU_SUCCESS;

   p1 = UU_NULL;
   if (NCL_pok_list != UU_NULL)
     p1 = (struct NCL_pok_ent *)uu_lsnext(NCL_pok_list);

   while (p1 != UU_NULL)
     {
     uu_lsdel (p1->plist);
     p1 = (struct NCL_pok_ent *)uu_lsnext(p1);
     }

   if (NCL_pok_list != UU_NULL)
     uu_lsdel (NCL_pok_list);

   NCL_pok_list = UU_NULL;

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int pklnew (key)
**       Initialize an advanced pocket data list.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          key        - Key of list.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
pklnew (key)
UM_int4 *key;
   {
   int status;
   char *p1;
   struct NCL_pok_ent *plst;

   status = UU_FAILURE;

   *key = 0;
   p1 = uu_lsnew();
   if (p1 != UU_NULL)
     {
     plst = (struct NCL_pok_ent *)uu_lsinsrt(NCL_pok_list,
               sizeof(struct NCL_pok_ent));
     NCL_pok_inx++;
     *key = NCL_pok_inx;
     plst->key = NCL_pok_inx;
     plst->plist = p1;
     NCL_pok_ptr = p1;
     NCL_pok_data_count = 0;
     uu_list_init (&NCL_pok_entpts, 6*sizeof(UM_real8), 20, 20);
     NCL_pok_entpts_count = 0;
     status = UU_SUCCESS;
     }

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int pklwrt
**       Flush any data remaining in the pocket buffer into the
**       current pocket list.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
pklwrt ()
   {
   int status, i, n;
   UM_real8 *p1, *p2;

   status = ncl_poksto();
/*
...   Insert pocket entry points at begining of list.
*/
   if (NCL_pok_entpts_count > 0)
     {
     p1 = (UM_real8 *)UU_LIST_ARRAY (&NCL_pok_entpts);
     n = 6*NCL_pok_entpts_count;
     p2 = (UM_real8 *) uu_lsinsrt(NCL_pok_ptr, n*sizeof(UM_real8));
     for (i=0;i<n;i++) *p2++ = *p1++;
     uu_list_free (&NCL_pok_entpts);
     }

   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int pklsto (itype, mtype, buf)
**       Store data specified by itype argument in the current pocket list.
**    PARAMETERS   
**       INPUT  : 
**          itype      - =0, TE point only
**                       =1, TE point & tool axis
**                       =2, circular interpolation record.
**                       =3, Feed rate.
**                       =4, Global pcket data.
**                       =5, "COUPLE" record.
**                       =6, "CYCLE" record.
**          mtype      - =0 First record of loop.
**                       =1 Continuation record.
**          buf        - Data
**       OUTPUT :  
**          mtype      - Set to 1 so next record from calling routine will
**                       default to continuation.
**          ier        - =0 no error, =1 error
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
pklsto (itype, mtype, buf, ier)
UM_int2 *itype, *mtype;
UM_real8 *buf;
UM_int4 *ier;
   {
   
   int status;
   int i, n, lfull;

   status = UU_SUCCESS;

   *ier = 0;
   if (*itype == 0 || *itype == 1) n=6;
   else if (*itype == 2) n=7;
   else if (*itype == 3) n=1;
   else if (*itype == 4) n=32;
   else if (*itype == 5) n=9;
   else if (*itype == 6) n=6;
   else n=0;

/*
...  If the pocket buffer is full or this is not a tool a tool end point,
...  flush the buffer.
*/
   lfull = NCL_MAX_POK_BUF - NCL_pok_data_count < n;
   if (lfull || NCL_pok_mtype != *mtype || *itype > 1)
     {
     status = ncl_poksto();
     if (lfull) NCL_pok_cont = 1;
     }
   if (status == UU_SUCCESS)
     {
/*
... Move this data to the buffer.
*/
     for (i=0;i<n;i++)
       {
       NCL_pok_buf[NCL_pok_data_count] = buf[i];
       NCL_pok_data_count++;
       }
/*
... Set continuation flag appropriately.
*/
     if (*itype > 1) NCL_pok_cont = 0;

     if (*mtype == 0)
       {
       NCL_pok_mtype = *mtype = 1;
       NCL_pok_itype = *itype;
       NCL_pok_cont = 0;
       }
/*
...  If this is not a tool end point, flush the buffer.
*/
     if (*itype > 1)
       status = ncl_poksto();
     }

   if (status != UU_SUCCESS) *ier = 1;

   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_poksto ()
**       Flush the pocket buffer into the current pocket list.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_poksto ()
   {
   int status;
   int i, n;
   char *p1;
   UM_int2 *p2;
   UM_real8 *p8;

   status = UU_SUCCESS;

   if (NCL_pok_data_count > 0)
     {
/*
...  There's data in the buffer. Add after end of current pocket list
...  (or after list pointer if this is the first entry).
*/
     n = (NCL_pok_data_count+1)*8;
     p1 = uu_lsend(NCL_pok_ptr);
     if (p1 == UU_NULL) p1 = NCL_pok_ptr;
     if (p1 != UU_NULL) p1 = uu_lsinsrt(p1,n);
     if (p1 == UU_NULL)
       status = UU_FAILURE;
     else
       {
/*
...  Fill in first real*8 with count, type etc.
*/
       p2 = (UM_int2 *)p1;
       *p2++ = NCL_pok_data_count;
       *p2++ = NCL_pok_cont;
       *p2++ = NCL_pok_itype;
       *p2 = 0;
/*
...  Load data starting at second word in list data.
*/
       p8 = (UM_real8 *)p1;
       for (i=0; i<NCL_pok_data_count; i++) *++p8 = NCL_pok_buf[i];
       NCL_pok_data_count = 0;
       }
     }

   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int pkldel (key)
**       Delete a pocket list.
**    PARAMETERS   
**       INPUT  : 
**          key        - Key of pocket list to delete.
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
pkldel (key)
UM_int4 *key;
   {
   
   int status;
   UU_LOGICAL found;
   struct NCL_pok_ent *pent;

   status = UU_SUCCESS;
/*
...  Search the list of pocket lists for requested list
*/
   pent = (struct NCL_pok_ent *) uu_lsnext (NCL_pok_list);
   found = UU_FALSE;
   while (pent != UU_NULL && !found)
     {
     if (pent->key == *key)
       {
/*
...  Pointer == requested index, delete this pocket list & the entry to it
...  in the list of pocket lists.
*/
       found = UU_TRUE;
       uu_lsdel(pent->plist);
       uu_lsdele (pent);
       }
     else
       {
       pent = (struct NCL_pok_ent *) uu_lsnext (pent);
       }
     }

   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int pklild (key, ier)
**       Initialize for loading of data from a pocket list.
**    PARAMETERS
**       INPUT  :
**          lptr       - Key of list to load from.
**       OUTPUT :
**          ier        - =0 no error, =1 error.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
pklild (key, ier)
UM_int4 *key;
UM_int4 *ier;
   {
   
   int status;
   int ilast;

   *ier = 0;
/*
...  Set the current pocket pointer.
*/
   status = ncl_pklfind (*key);
/*
...  Get past the entry points record
*/
   if (status == UU_SUCCESS)
     {
     NCL_pok_ptr =  uu_lsnext (NCL_pok_ptr);
     NCL_pok_entpts_ptr = (UM_real8 *) NCL_pok_ptr;
/*
...  Load the first record.
*/
     status = ncl_pok_load(&ilast);
     }
   if (status != UU_SUCCESS || ilast == 2) *ier = 1;

   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int pkllod (buf, itype, ilast, ier)
**       Load a tool end point (& tool axis vector if nec.) or a circular
**       record from the current pocket list.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          buf        - Tool end point or circular data.
**          itype      - Type of data
**                         0 = Tool end point only
**                         1 = Tool end & tool axis
**                         2 = Circular record
**                         3 = Feed rate.
**                         4 = Global pcket data.
**                         5 = "COUPLE" record.
**                         6 = "CYCLE" record.
**          ilast      - 1 iff last point in loop,
**                       2 iff last point in pocket,
**                       0 otherwise.
**          ier        - 1 iff error else 0.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
pkllod (buf, itype, npts, ilast, ier)
UM_real8 buf[];
UM_int2 *itype, *npts, *ilast;
UM_int4 *ier;
   {
   int status;
   int i, n;
   UM_real8 *p1;

   status = UU_SUCCESS;

   *ier = *ilast = 0;
   *itype = NCL_pok_itype;
   if (NCL_pok_itype == 0 || NCL_pok_itype == 1) n=NCL_pok_data_count;
   else if (NCL_pok_itype == 2) n=7;
   else if (NCL_pok_itype == 3) n=1;
   else if (NCL_pok_itype == 4) n=32;
   else if (NCL_pok_itype == 5) n=9;
   else if (NCL_pok_itype == 6) n=6;

   *npts = n/6;

   p1 = &NCL_pok_data_ptr[NCL_pok_data_indx];
   for (i=0;i<n;i++)
     {
     buf[i] =  *p1++;
     }

   NCL_pok_data_indx += n;
   if (NCL_pok_data_indx >= NCL_pok_data_count)
     {
     status = ncl_pok_load(&i);
     *ilast = i;
     }

	if (NCL_pok_itype == 3 && NCL_pok_data_ptr[NCL_pok_data_indx] == 6.0)
		*ier = -6;
   if (status != UU_SUCCESS) *ier = 1;

   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_pok_load (ilast)
**       Set up to load next element from pocket list.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          ilast       - =1 iff next list element starts a new loop.
**                        =2 iff this is the last element of pocket.
**                        =0 otherwise.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_pok_load (ilast)
int *ilast;
   {
   
   int status;
   UM_int2 *p2;

   if (NCL_pok_ptr != UU_NULL)
     {
     status = UU_SUCCESS;
     NCL_pok_ptr = uu_lsnext (NCL_pok_ptr);
     }
   else
     status = UU_FAILURE;

   if (NCL_pok_ptr != UU_NULL)
     {
/*
...  Found another record. Set global variables & pointer to data.
*/
     NCL_pok_data_ptr = (UM_real8 *) &NCL_pok_ptr[8];
     p2 = (UM_int2 *)NCL_pok_ptr;
     NCL_pok_data_count = *p2++;
     *ilast = *p2++;
     NCL_pok_itype = *p2;
     NCL_pok_data_indx = 0;
     }
   else
     {
     *ilast = 2;
     }

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int pklspt (buf)
**       Store a pocket entry point in the entry point list.
**    PARAMETERS
**       INPUT  :
**          buf        - Entry point.
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
pklspt (buf)
UM_real8 buf[];
   {
   int status;

   status = UU_SUCCESS;
   uu_list_push (&NCL_pok_entpts, buf);
   NCL_pok_entpts_count++;

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int pklnpt (key, npts)
**       Return the number of entry points in a pocket and
**       set up to begin returning them.
**    PARAMETERS
**       INPUT  :
**          key        - Key of pocket list.
**       OUTPUT :
**          npts       - Number of entry points.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
pklnpt (key, npts)
UM_int4 *key;
UM_int2 *npts;
   {
   int status;

   *npts = 0;
   status = ncl_pklfind (*key);
   if (status == UU_SUCCESS)
     {
     NCL_pok_entpts_ptr = (UM_real8 *) uu_lsnext (NCL_pok_ptr);
     *npts = uu_lslen (NCL_pok_entpts_ptr) / (6*sizeof(UM_real8));
     }

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int pklept (buf)
**       Return the next entry point in the current pocket.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          buf        - Next entry point.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
pklept (buf)
UM_real8 buf[];
   {
   int status;
   int i;

   status = UU_SUCCESS;
   for (i=0; i<6; i++) buf[i] = *NCL_pok_entpts_ptr++;

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_pklfind (key)
**       Set the current pocket list.
**    PARAMETERS   
**       INPUT  : 
**          key        - Key of pocket in pocket list.
**       OUTPUT :  
**          lptr       - Pointer to pocket.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : 
**         Sets NCL_pok_ptr
**    WARNINGS     : none
*********************************************************************/
int
ncl_pklfind (key)
UM_int4 key;
   {
   
   int status;
   struct NCL_pok_ent *pent;
/*
...  Search the list of pocket lists for requested list
*/
   pent = (struct NCL_pok_ent *) uu_lsnext (NCL_pok_list);
   status = UU_FAILURE;
   while (pent != UU_NULL && status == UU_FAILURE)
     {
     if (pent->key == key)
       {
/*
...  Pointer == requested index, set current pocket list.
*/
       status = UU_SUCCESS;
       NCL_pok_ptr = pent->plist;
       }
     else
       {
       pent = (struct NCL_pok_ent *) uu_lsnext (pent);
       }
     }

   return (status);
   }
/*********************************************************************
**    I_FUNCTION     : ncl_genpocket_newstart(entpt,spt,motvec,mlen,
**                          arad,cclw,newpt)
**       Offset the given point so the circle with radius arad will
**       be tangent to the line along mvec.  The circle has its
**       center along the vector from entpt to spt.
**    PARAMETERS
**       INPUT  :
**          entpt  - Pocket entry point.
**          spt    - Pocket loop start point.
**          motvec - Vector along first motion segment.
**          mlen   - Length of first motion segment.
**          arad   - Helix arc radius.
**          cclw   - UU_TRUE iff CCLW movement.
**       OUTPUT :
**          newpt  - New point.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_genpocket_newstart(entpt,spt,motvec,arad,cclw,newpt,flag)
UM_coord spt,entpt,newpt;
UM_vector motvec;
UU_REAL arad;
UU_LOGICAL cclw,flag;
{
	int nint;
	UU_REAL dis,tdis1,tdis2,scal;
	UM_vector uvec,dvec,mvec,zvec;
	UM_coord cpt,ipt,mpt,tcpt;
	char tbuf[80];
/*
.....Set up data.
*/
	zvec[0] = zvec[1] = 0.; zvec[2] = 1.;
	um_vcmnvc(spt,entpt,uvec); um_unitvc(uvec,uvec);
	um_translate_point(spt,-arad,uvec,cpt);
	um_unitvc(motvec,mvec);
if (flag)
{
	sprintf(tbuf,"PT/%lf,%lf,%lf",spt[0],spt[1],spt[2]);
	NclxDbgPstr(tbuf);
	sprintf(tbuf,"PT/%lf,%lf,%lf",entpt[0],entpt[1],entpt[2]);
	NclxDbgPstr(tbuf);
	sprintf(tbuf,"PT/%lf,%lf,%lf",cpt[0],cpt[1],cpt[2]);
	NclxDbgPstr(tbuf);
	sprintf(tbuf,"PV/%lf,%lf,%lf,%lf,%lf,%lf",spt[0],spt[1],spt[2],
		mvec[0],mvec[1],mvec[2]);
	NclxDbgPstr(tbuf);
}
/*
.....Find intersection and find distance to offset by.
*/
	um_vctovc(spt,newpt);
	um_nptln(cpt,spt,mvec,ipt);
/*
.....Offset start point.
*/
	um_vcmnvc(ipt,cpt,dvec); um_unitvc(dvec,dvec);
	if (UM_MAG(dvec) < UM_DFUZZ)
	{
		if (cclw) um_cross (zvec,mvec,dvec);
		else um_cross (mvec,zvec,dvec);
		um_unitvc(dvec,dvec);
	}
	um_translate_point(cpt,arad,dvec,ipt);
	um_vcmnvc(spt,ipt,dvec);
if (flag)
{
	sprintf(tbuf,"PT/%lf,%lf,%lf",ipt[0],ipt[1],ipt[2]);
	NclxDbgPstr(tbuf);
	sprintf(tbuf,"PV/%lf,%lf,%lf,%lf,%lf,%lf",ipt[0],ipt[1],ipt[2],
		dvec[0],dvec[1],dvec[2]);
	NclxDbgPstr(tbuf);
}
	um_translate_point(cpt,1.,dvec,tcpt);
	um_vcmnvc(tcpt,entpt,dvec); um_unitvc(dvec,dvec);
	um_translate_point(tcpt,arad,dvec,newpt);
if (flag)
{
	sprintf(tbuf,"PT/%lf,%lf,%lf",newpt[0],newpt[1],newpt[2]);
	NclxDbgPstr(tbuf);
	sprintf(tbuf,"PV/%lf,%lf,%lf,%lf,%lf,%lf",newpt[0],newpt[1],newpt[2],
		mvec[0],mvec[1],mvec[2]);
	NclxDbgPstr(tbuf);
}
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : nclf_pockgen_helix(arcrad,filrad,pptx,ppty,sptx,
**                        spty,zbot,znow,intol,dir,trans,mx,ier,fent,
**                        fhld,mtype)
**       Build helix entry through open side of pocket.
**    PARAMETERS
**       INPUT  :
**          arcrad    - Helix radius.
**          filrad    - Fillet radius.
**          pptx,ppty - Pocket point the cutter is moving to.
**          sptx,spty - Start point pocket is moving from.
**          zbot      - Bottom z level.
**          znow      - Current z level.
**          intol     - Tolerance.
**          dir       - 0: CCLW
**                      1: CLW
**          trans     -
**          mx        -
**          fent      - Feed rate value
**          fhld      - Feed rate value
**          mtype     - Point type
**       OUTPUT :
**          ier      - Error flag.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_pockgen_helix(arcrad,filrad,pptx,ppty,ibuf,znow,intol,dir,
							motvec,trans,mx,ier,fret,fent,fhld,mtype)
UM_real4 *arcrad,*filrad,*intol;
UM_real8 *pptx,*ppty,ibuf[6],*znow;
UM_real8 *mx,*fret,*fent,*fhld,motvec[3];
UM_int2 *dir,*mtype,*ier,*trans;
{
	int i,j,num_helix,asides,fsides,omit;
	UU_LOGICAL first;
	UM_transf tf,tfi;
	UU_REAL fact,arad,frad,tol,ptdis,cidis,ang,um_angle2p_acc();
	UU_REAL ang0,dang0,dang,tzlev,angf,theta1,theta2,motlen,dot;
	UM_coord ppt,ippt,nppt,inppt,cpt,icpt,cipt;
	UM_coord ticpt,tpt,fcpt1,fcpt2,conpt1,conpt2;
	UM_vector zvec,upvec,xvec,convec,civec,iupvc,ixvec,izvec;
	UM_vector mvec,imvec,dvec;
	UU_LOGICAL cclw,disagree = UU_FALSE;
	UM_int2 idx,ifl,ityp,i2v2,i2v3,i2v4;
	UM_real8 buf[7];
	char tbuf[80];
/*
.....Set up data.
.......For easier calculations, the data will be translated from the
.......standard coordinate system.
*/
	*mtype = *ier = 0;
	first = UU_TRUE;
	i2v2 = 2; i2v3 = 3; i2v4 = 4;
	cclw = (*dir == 0);
	idx = 264; getifl(&idx,&ifl);
	if (ifl == 0) fact = 25.4;
	else fact = 1.;
	zvec[0] = ibuf[3]; zvec[1] = ibuf[4]; zvec[2] = ibuf[5];
	tol = *intol;
	arad = *arcrad;
	frad = *filrad;
	ppt[0] = *pptx; ppt[1] = *ppty;
	cpt[0] = ibuf[0]; cpt[1] = ibuf[1];
	buf[3] = buf[4] = 0.; buf[5] = 1.;
	cpt[2] = ppt[2] = ibuf[2];
	mvec[0] = motvec[0]; mvec[1] = motvec[1]; mvec[2] = motvec[2];
	motlen = UM_MAG(mvec); um_unitvc(mvec,mvec);
/*
.....Find direction vector for offsetting connecting line to create
.....tangent line.
*/
	ncl_genpocket_newstart(cpt,ppt,mvec,arad,cclw,nppt,UU_FALSE);
	um_vcmnvc(nppt,cpt,upvec); um_unitvc(upvec,upvec);
	um_cross(upvec,zvec,xvec); um_unitvc(xvec,xvec);
	ptdis = um_dcccc(nppt,cpt);
	num_helix = ceil(ptdis/arad);
	arad = ptdis/(UU_REAL)num_helix;
	frad = arad/8.;
/*
.....The tangent line for the fillet circles is parallel to the
.....line connecting thier center points (they have equal radius).
*/
	if (cclw)
	{
		convec[0] = 2.*(arad-frad); convec[1] = arad; convec[2] = 0.;
	}
	else
	{
		convec[0] = -2.*(arad-frad); convec[1] = arad; convec[2] = 0.;
	}
	cidis = UM_MAG(convec); um_unitvc(convec,convec);
/*
.....Move to base coordinate system.
*/
	um_ptzx_tf(ppt,zvec,xvec,tf); um_inverttf(tf,tfi);
	um_cctmtf(ppt,tfi,ippt); um_cctmtf(cpt,tfi,icpt); um_cctmtf(nppt,tfi,inppt);
	um_vctmtf(upvec,tfi,iupvc); um_unitvc(iupvc,iupvc);
	um_vctmtf(xvec,tfi,ixvec); um_unitvc(ixvec,ixvec);
	um_vctmtf(mvec,tfi,imvec); um_unitvc(imvec,imvec);
	um_vctmtf(zvec,tfi,izvec); um_unitvc(izvec,izvec);
	if (cclw)
		um_cross(convec,izvec,civec);
	else
		um_cross(izvec,convec,civec);
	um_unitvc(civec,civec);
/*
.....Determine whether a reverse arc needs to be used to finish the
.....helix motion so the last move is tangent to the first loop segment
.....and in the same direction.  This is to prevent sudden stops and
.....shifts in direction.
*/
	um_translate_point(inppt,-arad,iupvc,ticpt);
	um_vcmnvc(ippt,ticpt,dvec); um_unitvc(dvec,dvec);
	ang = acos(dvec[0]); dot = UM_DOT(iupvc,imvec);
	if ((cclw && ang > UM_HALFPI == dot > 0) || 
		(!cclw && ang > UM_HALFPI == dot < 0))
	{
		disagree = UU_TRUE;
		omit = 3;
	}
	else
		omit = 1;
	if (disagree && num_helix == 2)
	{
		um_translate_point(ippt,-3.*arad,iupvc,icpt);
		num_helix += 1;
	}
/*
.....Calculate angle for fillet circles and determine number of
.....points to generate.
*/
	angf = um_angle2p_acc(ixvec,civec,izvec);
	if (cclw)
		theta1 = angf - UM_PI;
	else
		theta1 = UM_TWOPI - angf;
	theta2 = UM_PI - theta1;
	um_circle_nsides(arad,UM_PI,tol,&asides);
	um_circle_nsides(frad,UM_HALFPI,tol,&fsides);
	if (cclw)
	{
		ang0 = 0.;
		dang0 = UM_PI/(UU_REAL)asides;
	}
	else
	{
		um_negvc(ixvec,ixvec);
		ang0 = UM_PI;
		dang0 = -UM_PI/(UU_REAL)asides;
	}
/*
.....Move to first position and make a fillet arc move to first
.....large arc in helix.
*/
	dang = UM_HALFPI/(UU_REAL)fsides;
	if (!cclw) dang = -dang;
	ang = UM_PI + UM_HALFPI;
	um_translate_point(icpt,-frad,iupvc,ticpt);
	um_vctovc(ticpt,tpt);
	um_cctmtf(tpt,tf,tpt);
	tzlev = tpt[2];
	tpt[2] = *znow;
/*
.....Start point of helix entry
*/
	*mtype = 0;
	pokfed (fret, fhld, mtype, ier);
	buf[0] = tpt[0]; buf[1] = tpt[1]; buf[2] = tpt[2];
   S_poksto (buf, mtype, trans, mx, ier, &first);
	buf[2] = tzlev;
	*mtype = 0;
   S_poksto (buf, mtype, trans, mx, ier, &first);
	pokfed (fent, fhld, mtype, ier);
	if (cclw) ticpt[0] += (arad-frad);
	else ticpt[0] -= (arad-frad);
/*
.....Move to center of first small arc
*/
	ticpt[1] += frad;
/*
.....Start of small arc
*/
	buf[0] = ticpt[0] + frad*cos(ang);
	buf[1] = ticpt[1] + frad*sin(ang);
	buf[2] = 0.;
	buf[3] = buf[4] = 0.; buf[5] = 1.;
	um_cctmtf(buf,tf,buf);
	*mtype = 0;
   S_poksto (buf, mtype, trans, mx, ier, &first);
/*
.....Small arc circle record
*/
	buf[0] = ticpt[0];
	buf[1] = ticpt[1];
	buf[2] = 0.;
	buf[3] = buf[4] = 0.; buf[5] = 1.;
	um_cctmtf(buf,tf,buf);
	if (*trans == 1) conent (&buf[0], mx, &i2v3);
	if (*trans == 1) conent (&buf[3], mx, &i2v4);
	buf[6] = frad;
	*mtype = 0;
	pklsto (&i2v2, mtype, buf, ier);
/*
.....Output circle points
*/
	*mtype = 0;
	for (j=0;j<fsides;j++)
	{
		ang += dang;
		buf[0] = ticpt[0] + frad*cos(ang);
		buf[1] = ticpt[1] + frad*sin(ang);
		buf[2] = 0.;
		um_cctmtf(buf,tf,buf);
      S_poksto (buf, mtype, trans, mx, ier, &first);
		if (*ier != 0) return;
	}
/*
.....Build points to define ends of each component of the helix
*/
	if (cclw)
	{
		fcpt1[0] = icpt[0] - arad + frad;
		fcpt2[0] = icpt[0] + arad - frad;
		fcpt1[1] = fcpt2[1] = icpt[1];
	}
	else
	{
		fcpt1[0] = icpt[0] + arad - frad;
		fcpt2[0] = icpt[0] - arad + frad;
		fcpt1[1] = fcpt2[1] = icpt[1];
	}
	fcpt2[1] += arad;
	conpt1[0] = fcpt1[0] + frad*cos(angf);
	conpt1[1] = fcpt1[1] + frad*sin(angf);
	fcpt1[2] = fcpt2[2] = conpt1[2] = conpt2[2] = 0.;
	um_translate_point(conpt1,cidis,convec,conpt2);
/*
.....Build helixes.  The helix radius is modified so the ending point
.....will lie on the arc and so we can connect to is using a quarter arc.
*/
	for (i=0;i<num_helix-omit;i++)
	{
/*
.....Output start of big arc
*/
		buf[0] = icpt[0] + arad*cos(ang0);
		buf[1] = icpt[1] + arad*sin(ang0);
		buf[2] = 0.;
		buf[3] = buf[4] = 0.; buf[5] = 1.;
		um_cctmtf(buf,tf,buf);
		*mtype = 0;
		S_poksto (buf, mtype, trans, mx, ier, &first);
/*
.....Ouput big arc CIRCLE record
*/
		buf[0] = icpt[0];
		buf[1] = icpt[1];
		buf[2] = 0.;
		buf[3] = buf[4] = 0.; buf[5] = 1.;
		um_cctmtf(buf,tf,buf);
		if (*trans == 1) conent (&buf[0], mx, &i2v3);
		if (*trans == 1) conent (&buf[3], mx, &i2v4);
		buf[6] = arad;
		*mtype = 0;
		pklsto (&i2v2, mtype, buf, ier);
/*
.....Build big arc.
*/
		ang = ang0;
		dang = dang0;
		*mtype = 0;
		for (j=0;j<asides;j++)
		{
			ang += dang;
			buf[0] = icpt[0] + arad*cos(ang);
			buf[1] = icpt[1] + arad*sin(ang);
			buf[2] = 0.;
			buf[3] = buf[4] = 0.; buf[5] = 1.;
			um_cctmtf(buf,tf,buf);
         S_poksto (buf, mtype, trans, mx, ier, &first);
			if (*ier != 0) return;
		}
/*
.....Start of first small arc
*/
		buf[0] = fcpt1[0] + frad*cos(ang);
		buf[1] = fcpt1[1] + frad*sin(ang);
		buf[2] = 0.;
		buf[3] = buf[4] = 0.; buf[5] = 1.;
		um_cctmtf(buf,tf,buf);
		*mtype = 0;
		S_poksto (buf, mtype, trans, mx, ier, &first);
/*
.....Output first small arc CIRCLE record
*/
		buf[0] = fcpt1[0];
		buf[1] = fcpt1[1];
		buf[2] = 0.;
		buf[3] = buf[4] = 0.; buf[5] = 1.;
		um_cctmtf(buf,tf,buf);
		if (*trans == 1) conent (&buf[0], mx, &i2v3);
		if (*trans == 1) conent (&buf[3], mx, &i2v4);
		buf[6] = frad;
		*mtype = 0;
		pklsto (&i2v2, mtype, buf, ier);
/*
.....Build first fillet arc.
*/
		um_circle_nsides(frad,theta1,tol,&fsides);
		dang = theta1/(UU_REAL)fsides;
		if (!cclw) dang = -dang;
		um_cctmtf(buf,tfi,cipt);
		um_translate_point(cipt,frad,ixvec,cipt);
		*mtype = 0;
		for (j=0;j<fsides;j++)
		{
			ang += dang;
			buf[0] = fcpt1[0] + frad*cos(ang);
			buf[1] = fcpt1[1] + frad*sin(ang);
			buf[2] = 0.;
			buf[3] = buf[4] = 0.; buf[5] = 1.;
			um_cctmtf(buf,tf,buf);
         S_poksto (buf, mtype, trans, mx, ier, &first);
			if (*ier != 0) return;
		}
/*
.....Output start of second fillet arc
*/
		buf[0] = fcpt2[0] + frad*cos(ang);
		buf[1] = fcpt2[1] + frad*sin(ang);
		buf[2] = 0.;
		buf[3] = buf[4] = 0.; buf[5] = 1.;
		um_cctmtf(buf,tf,buf);
		*mtype = 0;
		S_poksto (buf, mtype, trans, mx, ier, &first);
/*
.....Output second fillet arc CIRCLE record.
*/
		buf[0] = fcpt2[0];
		buf[1] = fcpt2[1];
		buf[2] = 0.;
		buf[3] = buf[4] = 0.; buf[5] = 1.;
		um_cctmtf(buf,tf,buf);
		if (*trans == 1) conent (&buf[0], mx, &i2v3);
		if (*trans == 1) conent (&buf[3], mx, &i2v4);
		buf[6] = frad;
		*mtype = 0;
		pklsto (&i2v2, mtype, buf, ier);
/*
.....Build second small arc.
*/
		um_translate_point(cipt,cidis,convec,cipt);
		um_circle_nsides(frad,theta2,tol,&fsides);
		dang = theta2/(UU_REAL)fsides;
		if (!cclw) dang = -dang;
		*mtype = 0;
		for (j=0;j<fsides;j++)
		{
			ang += dang;
			buf[0] = fcpt2[0] + frad*cos(ang);
			buf[1] = fcpt2[1] + frad*sin(ang);
			buf[2] = 0.;
			buf[3] = buf[4] = 0.; buf[5] = 1.;
			um_cctmtf(buf,tf,buf);
         S_poksto (buf, mtype, trans, mx, ier, &first);
			if (*ier != 0) return;
		}
/*
.....Move to next helix level
*/
		um_translate_point(icpt,arad,iupvc,icpt);
		um_translate_point(fcpt1,arad,iupvc,fcpt1);
		um_translate_point(fcpt2,arad,iupvc,fcpt2);
		um_translate_point(conpt1,arad,iupvc,conpt1);
		um_translate_point(conpt2,arad,iupvc,conpt2);
	}
	if (disagree)
	{
		angf = UM_HALFPI;
		um_circle_nsides(arad,angf,tol,&asides);
		dang = angf/(UU_REAL)asides;
		if (!cclw) dang = -dang;
		ang = ang0;
/*
.....Start of arc
*/
		buf[0] = icpt[0] + arad*cos(ang);
		buf[1] = icpt[1] + arad*sin(ang);
		buf[2] = 0.;
		buf[3] = buf[4] = 0.; buf[5] = 1.;
		um_cctmtf(buf,tf,buf);
		*mtype = 0;
		S_poksto (buf, mtype, trans, mx, ier, &first);
/*
.....Output circle record.
*/
		buf[0] = icpt[0];
		buf[1] = icpt[1];
		buf[2] = 0.;
		buf[3] = buf[4] = 0.; buf[5] = 1.;
		um_cctmtf(buf,tf,buf);
		if (*trans == 1) conent (&buf[0], mx, &i2v3);
		if (*trans == 1) conent (&buf[3], mx, &i2v4);
		buf[6] = arad;
		*mtype = 0;
		pklsto (&i2v2, mtype, buf, ier);
/*
.....Build arc to transition to reverse arc.
*/
		*mtype = 0;
		for (j=0;j<asides;j++)
		{
			ang += dang;
			buf[0] = icpt[0] + arad*cos(ang);
			buf[1] = icpt[1] + arad*sin(ang);
			buf[2] = 0.;
			buf[3] = buf[4] = 0.; buf[5] = 1.;
			um_cctmtf(buf,tf,buf);
			S_poksto (buf, mtype, trans, mx, ier, &first);
			if (*ier != 0) return;
		}
/*
.....Move to next helix level
*/
		um_translate_point(icpt,2.*arad,iupvc,icpt);
	}
	um_vcmnvc(ippt,icpt,dvec); um_unitvc(dvec,dvec);
	angf = acos(dvec[0]);
	if (disagree)
	{
/*
.....Build reversed helix.
.......Ensures the final arc arrives at ppt tangent and in the
.......same direction as mvec.
*/
		if (cclw) angf = UM_PI + UM_HALFPI - angf;
		else angf += UM_HALFPI;
	}
	else
	{
/*
.....Build normal helix.
*/
		if (!cclw) angf = UM_PI - angf;
	}
	um_circle_nsides(arad,angf,tol,&asides);
	dang = angf/(UU_REAL)asides;
	if (disagree)
	{
		if (cclw) dang = -dang;
		ang = -UM_HALFPI;
	}
	else
	{
		if (!cclw) dang = -dang;
		ang = ang0;
	}
/*
.....Start of arc
*/
	buf[0] = icpt[0] + arad*cos(ang);
	buf[1] = icpt[1] + arad*sin(ang);
	buf[2] = 0.;
	buf[3] = buf[4] = 0.; buf[5] = 1.;
	um_cctmtf(buf,tf,buf);
	*mtype = 0;
	S_poksto (buf, mtype, trans, mx, ier, &first);
/*
.....Output circle record.
*/
	buf[0] = icpt[0];
	buf[1] = icpt[1];
	buf[2] = 0.;
	buf[3] = buf[4] = 0.; buf[5] = 1.;
	um_cctmtf(buf,tf,buf);
	if (*trans == 1) conent (&buf[0], mx, &i2v3);
	if (*trans == 1) conent (&buf[3], mx, &i2v4);
	buf[6] = arad;
	*mtype = 0;
	pklsto (&i2v2, mtype, buf, ier);
/*
.....Build final arc.
*/
	*mtype = 0;
	for (j=0;j<asides-1;j++)
	{
		ang += dang;
		buf[0] = icpt[0] + arad*cos(ang);
		buf[1] = icpt[1] + arad*sin(ang);
		buf[2] = 0.;
		buf[3] = buf[4] = 0.; buf[5] = 1.;
		um_cctmtf(buf,tf,buf);
		S_poksto (buf, mtype, trans, mx, ier, &first);
		if (*ier != 0) return;
	}
	*mtype = 0;
}

/*********************************************************************
**    I_FUNCTION     : pokopn (xx,yy,crad,buf,offdis,do_helix,islnds,
**                             rmpdis,trans,mx,jcut,ier)
**       Find an entry point so the cutter can enter off the part
**       through an open side without gouging geometry. The returned
**       point will be on the clearance plane and a move down to the
**       current bottom z level is made in the calling routine.
**    PARAMETERS
**       INPUT  :
**          xx,yy    - Coords of point entry point will connect to.
**          crad     - Cutter radius.
**          offdis   - Distance cutter edge should be from boudnary.
**          do_helix - Doing helix entry iff 1.
**          islnds   - Number of islands in pocket.
**          rmpdis   - Helix radius.
**          trans    - 
**          mx       -
**       OUTPUT :
**          buf      - Calculated entry point on clearance plane.
**          jcut     - Good path found iff 0.
**          do_helix - Do helix entry iff 1.
**          ier      - Error flag.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void pokopn (xx,yy,crad,buf,zbot,motvec,offdis,do_helix,islnds,rmpdis,indir,
	trans,mx,lwrk,invwrk,lctyp,jcut,ier)
UM_real8 *xx,*yy,*buf,*zbot,motvec[3],*mx,*invwrk;
UM_real4 *offdis,*rmpdis,*crad;
UM_int2 *indir,*islnds,*do_helix,*jcut,*trans,*lwrk,*lctyp;
UM_int4 *ier;
{
	int i,j,k,m,ind1,ind2,ind3,ind4,which,npts,status;
	UU_REAL tol,fact,arad,dis,dmin,ddis,tdis,motlen,hyp;
	UU_REAL sav_hyp,ang,ptdis,diam,z,maxdis,mm;
	UM_coord *pts,pt,entpt,lpt,newlpt,bpt1,bpt2;
	UM_vector dvec,mvec,svec;
	UU_LIST nbrlst,ptlst;
	ncl_nbr_struc *nbrs;
	ncl_bndpt_struc *bpts,*bpts2;
	UU_LOGICAL done,cclw,found,helfl;
	UM_int2 idx,ifl,ityp;
	UM_real4 rad,x0,y0,a1,b1,ta1,tb1,tolm;
	UM_real8 xx0,yy0,xx1,yy1;
	char tbuf[80];
	
	gettol(&tol);
	helfl = found = UU_FALSE;
	idx = 264; getifl(&idx,&ifl);
	cclw = (*indir == 0);
	mm = 25.4;
	if (ifl == 0) fact = 25.4;
	else fact = 1.;
	a1 = (*xx)*fact; b1 = (*yy)*fact; z = (*zbot)*fact;
	rad = (*crad)*fact; 
	if (fabs(*offdis) > UM_FUZZ) dis = (*offdis)*fact;
	else dis = rad;
	arad = (*rmpdis)*fact;
	if (*do_helix)
	{
		helfl = UU_TRUE;
		rad += arad;
	}
	diam = 2.*rad;
	tolm = tol*fact;
	dmin = fact*10000.;
	*ier = *jcut = 0;
	sav_hyp = 0.;
	if (*islnds > 0) *jcut = 1;
	*do_helix = 1;
	for (i=0;i<3;i++) mvec[i] = motvec[i];
	motlen = UM_MAG(mvec); um_unitvc(mvec,mvec);
	lpt[0] = a1; lpt[1] = b1; lpt[2] = z;
	ityp = 3;
	if (*lctyp > 0) maxdis = 2.*diam;
	else maxdis = 1.e12*fact;
/*
.....Add points to segments longer than the cutter diameter.
.....Use the midpoint of each segment to search for a valid
.....entry point.
*/
	ncl_get_nbrlst(&nbrlst);
	nbrs = (ncl_nbr_struc *) UU_LIST_ARRAY(&nbrlst);
	for (i=0;i<nbrlst.cur_cnt;i++)
	{
		if (nbrs[i].open)
		{
			npts = 0;
			bpts = (ncl_bndpt_struc *) UU_LIST_ARRAY(nbrs[i].bpts);
			uu_list_init(&ptlst,sizeof(UM_coord),nbrs[i].no_bpts,100);
			for (j=0;j<nbrs[i].no_bpts-1;j++)
			{
				uu_list_push(&ptlst,&bpts[j].pt);
				npts++;
				ptdis = fact*um_dcccc(bpts[j].pt,bpts[j+1].pt);
				if (ptdis > diam)
				{
					um_vcmnvc(bpts[j+1].pt,bpts[j].pt,svec);
					um_unitvc(svec,svec);
					ind1 = (ptdis/diam) + 1;
					tdis = ptdis/(UU_REAL)ind1;
					tdis = tdis/fact;
					ddis = tdis;
					for (k=0;k<ind1-1;k++)
					{
						um_translate_point(bpts[j].pt,tdis,svec,pt);
						uu_list_push(&ptlst,&pt);
						npts++; tdis += ddis;
					}
				}
			}
			uu_list_push(&ptlst,&bpts[j].pt);
			npts++;
/*
.....Move along open side to try to find an open entry section.
.......Note that the open side points will be offset so the distance
.......to offset only needs to be based off the cutter radius or off
.......part distance (OFFDIS).
*/
			pts = (UM_coord *) UU_LIST_ARRAY(&ptlst);
			ind1 = 0; ind2 = ind1 + 1;
			while (ind2 < npts)
			{
				*ier = *jcut = 0;
				ind3 = ind1; ind4 = ind2; ind1++; ind2++;
				if (um_dcccc(pts[ind3],pts[ind4]) < UM_FUZZ)
				{
					if (npts == 2) break;
					continue;
				}
				um_middlept(pts[ind3],pts[ind4],pt);
/*
.....Translate to pocket coordinate system.
*/
				if (*lwrk == 1) conent(pt,invwrk,&ityp);
				um_vctmsc(pt,mm,pt);
				if (*trans == 1) conent(pt,mx,&ityp);
				um_vcmnvc(pt,lpt,dvec);
				ptdis = UM_MAG(dvec); um_unitvc(dvec,dvec);
				if (ptdis > maxdis)
					continue;
				um_translate_point(pt,rad+2.*tolm,dvec,entpt);
				x0 = entpt[0]; y0 = entpt[1];
				if (helfl)
					status = ncl_genpocket_newstart(entpt,lpt,mvec,arad,
						cclw,newlpt,UU_FALSE);
				else
					um_vctovc(lpt,newlpt);
				ta1 = newlpt[0]; tb1 = newlpt[1];
/*
.....Check entrance against the islands.
*/
				chkrmp(&rad,&x0,&y0,&ta1,&tb1,&tolm,jcut,ier);
				if (*ier != 0) return;
				if (*jcut == 1 && npts == 2) break;
				else if (*jcut == 1) continue;
/*
.....Check entrance against the closed sides if any exist.
*/
				if (nbrlst.cur_cnt > 1)
				{
					done = UU_FALSE;
					for (k=0;k<nbrlst.cur_cnt && !done;k++)
					{
						if (nbrs[k].open) continue;
						bpts2 = (ncl_bndpt_struc *) UU_LIST_ARRAY(nbrs[k].bpts);
						for (m=0;m<nbrs[k].no_bpts-1 && !done;m++)
						{
							um_vctovc(bpts2[m].pt,bpt1); um_vctovc(bpts2[m+1].pt,bpt2);
							if (UM_MAG(bpt1) > 1.e8 || UM_MAG(bpt2) > 1.e8) continue;
/*
.....Translate to pocket coordinate system.
*/
							if (*lwrk == 1)
							{
								conent(bpt1,invwrk,&ityp);
								conent(bpt2,invwrk,&ityp);
							}
							um_vctmsc(bpt1,mm,bpt1); um_vctmsc(bpt2,mm,bpt2);
							if (*trans == 1)
							{
								conent(bpt1,mx,&ityp);
								conent(bpt2,mx,&ityp);
							}
							xx0 = bpt1[0]; yy0 = bpt1[1];
							xx1 = bpt2[0]; yy1 = bpt2[1];
							chkseg(&ta1,&tb1,&x0,&y0,&rad,&xx0,&yy0,&xx1,&yy1,jcut);
							if (*jcut != 0) done = UU_TRUE;
						}
					}
				}
				if (*jcut == 0 && ptdis < dmin)
				{
					found = UU_TRUE;
					dmin = ptdis;
					um_vctovc(pts[ind3],bpt1); um_vctovc(pts[ind4],bpt2);
/*
.....Translate to pocket coordinate system.
*/
					if (*trans == 1 || *lwrk == 1)
					{
						if (*lwrk == 1)
						{
							conent(bpt1,invwrk,&ityp);
							conent(bpt2,invwrk,&ityp);
						}
						um_vctmsc(bpt1,mm,bpt1); um_vctmsc(bpt2,mm,bpt2);
						if (*trans == 1)
						{
							conent(bpt1,mx,&ityp);
							conent(bpt2,mx,&ityp);
						}
					}
					um_vcmnvc(bpt1,bpt2,svec);
					um_unitvc(svec,svec);
/*
.....Offset the entry point so the cutter is at least the
.....cutter radius away.
.......Note that the offset is removed so the cutter position
.......is based on the original boundary.
*/
					ang = um_angle(svec,dvec);
					hyp = dis-fact*nbrs[i].rad;
					if (hyp > 0.) hyp /= fabs(sin(ang));
					else hyp = 0.;
					um_translate_point(pt,hyp+2.*tolm,dvec,entpt);
					buf[0] = entpt[0]/fact; buf[1] = entpt[1]/fact;
					buf[2] = entpt[2]/fact;
				}
				if (npts == 2) break;
			}
			uu_list_free(&ptlst);
		}
	}
	if (found) *jcut = 0;
	else *jcut = 1;
	uu_list_free(&nbrlst);
}

/*********************************************************************
**    I_FUNCTION     : S_poksto (buf, mtype, trans, mx, ier, first);
**       Output point to pocket list if it is different than last
**       point.
**    PARAMETERS
**       INPUT  :
**          buf       - Point and vector to store.
**          mtype     - Motion type.
**          trans     - Transformation flag.
**          mx        - Transformation matrix.
**          init      - UU_TRUE = First point output in the current section.
**       OUTPUT :
**          ier      - Error flag.
**          first    - Set to UU_FALSE after first call.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_poksto(buf, mtype, trans, mx, ier, first)
UU_REAL buf[];
UM_int2 *mtype;
UM_int2 *trans;
UU_REAL mx[];
UM_int4 *ier;
UU_LOGICAL *first;
{
	int i;
	static UU_REAL Sbuf[6]={0,0,0, 0,0,0};
/*
.....Only output point if it has changed
*/
	*ier = 0;
	if (*first || !um_cceqcc(&buf[0],&Sbuf[0]) || !um_cceqcc(&buf[3],&Sbuf[3]))
	{
		*first = UU_FALSE;
		poksto(buf, mtype, trans, mx, ier);
		for (i=0;i<6;i++) Sbuf[i] = buf[i];
	}
}
