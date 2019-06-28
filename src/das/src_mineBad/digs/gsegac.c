/********************************************************************* 
**  NAME:  gsegac.c  -- Segment table access routines.
**		These routines call upon the hash table routines (found in uhash.c)
**		to access the graphics segment headers. Each segment header is an
**		entry in a single hash table. This hash table variable is named
**		ug_seghash and is declared in  gtblvar1.h.
**		Within a segment, each graphics command is accessed via the
**		indexed list processing routines found in glsi2.c. 
**       CONTAINS:
**		ug_segacinit() -- init segment storage.
**		ug_segacterm() -- terminate segment storage.
**		UG_segstli *ug_segadd(segid) -- adds a segment. Return ptr to state info.
**		ug_segdel(segid) -- deletes a segment.
**		The rest of the segment table access routines are macros, defined in
**			gsegac.h. These macros are:
**	  	UG_segstli *ug_segac(segid) -- returns a pointer to the seg's state info. 
**		ug_seginitscan() -- initialize segment state info scan.
**		UG_segstli *ug_segscan() -- return ptr to next segment's state info.
**		int *ug_segtop(segptr) -- set current cmd at beginning of seg (before the 
**									first command). return NULL.
**		ug_segcomp(n) -- compact a segment.
**    ug_lsins(lpt,buf,len) -- insert new command.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       gsegac.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:24
*********************************************************************/
#include "gtbl.h"
#include "udebug.h"
#include "gsegac.h"
#include "ualloc.h"
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) gsegac.c 1.27 10/23/86 17:37:16 single"};
#else
static char uu_sccsident[]={"@(#) gsegac.c 1.27 10/23/86 17:37:16 double"};
#endif

UU_STORE *uu_segstore;				/* store for segment data */
UU_STORE *uu_create_store();
/*********************************************************************
**    I_FUNCTION     :  ug_segacinit() -- init seg storage.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : deletes all existing segments.
**    WARNINGS     : none
*********************************************************************/
ug_segacinit()
{
	uu_denter(UU_GITRC,(us,"ug_segacinit()"));
	uu_segstore=uu_create_store();
	uu_hash_init(ug_seghash,512);
	uu_dexit;
}
	
/*********************************************************************
**    I_FUNCTION     :  ug_segacterm() -- terminate seg storage.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : deletes all existing segments.
**    WARNINGS     : none
*********************************************************************/
ug_segacterm()
{
	uu_denter(UU_GITRC,(us,"ug_segacterm()"));
	uu_hash_del(ug_seghash);
	uu_free_store(uu_segstore);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  UG_segstli *ug_segadd(segid) -- add a segment.
**    PARAMETERS   
**       INPUT  :  int segid -- segment id.
**       OUTPUT :  
**    RETURNS      : Pointer to new seg's state list if all went OK.
**							Else NULL if out of memory.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UG_segstli *ug_segadd(segid)
int segid;
{
	UG_segstli *here;
	int er;
	int index,i;
	char us[80];

	/* call hash table routine to add segment state list to the segment table */
	here =  ( UG_segstli * )uu_hash_add(ug_seghash,segid,sizeof(UG_segstli));
	if (here!=NULL) {
		UU_STORE *oldstore=(UU_STORE *)uu_get_current_store();
		here->segid = segid ;
		here->xforms = 0;				/* No ntrans, or inherited xforms yet */
		here->segatts.prio=1;		/* highest priority */
		
		/* initialize list for this segment */
		uu_set_current_store(uu_segstore);
		er=ug_lsiinit(here->seglist);
		uu_set_current_store(oldstore);
		if (er) {		/* out of memory */
			uu_hash_dele(ug_seghash,segid);		/* delete the segment table entry*/
			here=NULL;
		}
	}
	uu_denter2(UU_GITRC,(us,"%d=ug_segadd(%d)",here,segid));
	uu_dexit;
	return ( here ) ;
}

/*********************************************************************
**    I_FUNCTION     :  int ug_segdel(segid) -- delete a segment.
**    PARAMETERS   
**       INPUT  :  int segid; -- segment id to delete.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_segdel(segid)
int segid;
{
	UG_segstli *here;
	int *p,*nxt;
	char us[80];

	/* get pointer to hash table entry for this segment */
	here = (UG_segstli *)uu_hash_get(ug_seghash,segid);
	uu_denter2(UU_GITRC,(us,"ug_segdel(%d). segptr=%d",segid,here));
	if ( here != NULL ) { 	 /* segment exists */
		UU_STORE *oldstore=(UU_STORE *)uu_get_current_store();

		/* found the segment. first delete seg's command list */
		uu_set_current_store(uu_segstore);
		ug_lsidel(here->seglist);
		/* next delete the segment's raster copy if it exists */
		if (here->rassegpt!=NULL)  ug_lsidel((*(here->rassegpt)));
		uu_set_current_store(oldstore);

		/* delete the segment statelist */
		uu_hash_dele(ug_seghash,segid);
	}
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_seginitscan ( ) -- init segment scan.
**				Scan through the segment hash-table making lists of the segs
**				in each priority. 
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/* priorities range from 1(highest) to UG_MAXSEGPRIO (lowest) */
static UG_segstli **ug_prioptr[UG_MAXSEGPRIO+1];	/* pointer to array for 
							each prio. Each array is a set of UG_segstli pointers*/
static int ug_priolen[UG_MAXSEGPRIO+1];	/* length of each prio array, or 0 */
/* the code assumes ug_priolen will be initialized with zeros */
static int ug_prio;					/* current priority */
static int ug_prioindx;				/* last index in prioptr */
ug_seginitscan()
{
	int prio;
	UG_segstli *segptr;
	UU_STORE *oldstore;

	uu_denter(UU_GITRC,(us,"ug_seginitscan()"));

	/* First, go thru and delete all allocated space from last seginitscan */
	oldstore=(UU_STORE *)uu_get_current_store();
	uu_set_current_store(uu_segstore);
	for (prio=1; prio<=UG_MAXSEGPRIO; prio++) {
		if (ug_priolen[prio]>0) {
			uu_free(ug_prioptr[prio]);
			ug_priolen[prio]=0;
		}
	}
	uu_set_current_store(oldstore);

	/* scan hash table, making prio arrays */
	uu_hash_initscan(ug_seghash);	/* init hash table scan */
	while ((segptr=(UG_segstli *)uu_hash_scan(ug_seghash))!=NULL) {
		if (segptr->segid<=UG_MAXSEGNO) {	/* a legal segment no. */
			prio=segptr->segatts.prio;
			if (ug_priolen[prio]==0) { 	/* start a new priority array */
				oldstore=(UU_STORE *)uu_get_current_store();
				uu_set_current_store(uu_segstore);
				ug_prioptr[prio]=(UG_segstli **)
					uu_malloc(UG_MAXSEGNO*sizeof(int *));
				uu_set_current_store(oldstore);
			}
			ug_prioptr[prio][ug_priolen[prio]]=segptr;	/* save segptr */
			ug_priolen[prio]++;		/* bump length of this prio array */
		}										/* end of a legal segment no. */
	}											/* end of while */
	ug_prio=UG_MAXSEGPRIO;
	ug_prioindx= -1;
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  UG_segstli *ug_segscan ( ) -- get next segment.
**					Return segment pointers, highest prio 1st. Within a
**					priority, order is random.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UG_segstli *ug_segscan()
{
	int i;
	UG_segstli *irtn;
	UU_STORE *oldstore;

	uu_denter(UU_GITRC,(us,"ug_segscan(). ug_prio=%d ug_prioindx=%d",
		ug_prio,ug_prioindx));

	while (ug_prioindx>=(ug_priolen[ug_prio]-1)) {	/*to next higher priority */
		if (ug_prio>1) ug_prio--;
		else {								/* no more segs, free arrays. */
			oldstore=(UU_STORE *)uu_get_current_store();
			uu_set_current_store(uu_segstore);
			for (i=1; i<=UG_MAXSEGPRIO; i++) {
				if (ug_priolen[i]>0) {
					uu_free(ug_prioptr[i]);
					ug_priolen[i]=0;
				}
			}
			uu_set_current_store(oldstore);
			ug_prio=0; ug_prioindx=0;
			irtn=NULL;
			goto rtn;
		}										/* end no more segs */
		ug_prioindx= -1;
	}											/* end while */
	ug_prioindx++;
	irtn=ug_prioptr[ug_prio][ug_prioindx];
rtn:	uu_dprint(UU_GITRC,(us,"%x=ug_segscan(). ug_prio=%d ug_prioindx=%d",
		irtn,ug_prio,ug_prioindx));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     :  int ug_lsins(lpt,buf,len) -- insert new command.
**       Insert a new graphics command of len words, after the
**			current command in indexed list lpt.
**    PARAMETERS   
**       INPUT  : 	UG_LSI lpt -- pointer to indexed list.
** 						int buf[] -- command to be inserted.
**							int len -- length of buf.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_lsins(lpt,buf,len)
int buf[],len;
UG_LSI lpt;
{
	UU_STORE *oldstore=(UU_STORE *)uu_get_current_store();
	int *p,i;
	char *ug_lsiins();						/* inserts element in list */
	int icmd;									/* ith command of segment */

	/*icmd = ug_lsicur(lpt);
	/*icmd++;									/* icmd = current command */
	icmd=ug_gksstli.curelptr++;		/* icmd is 0 origin, cureltptr is 1 origin,
													so we mean this. */
	uu_set_current_store(uu_segstore);
	p = (int *)ug_lsiins(lpt,icmd,sizeof(int) * len);
	uu_set_current_store(oldstore);
	for (i=0; i<len; i++) 
		p[i]=buf[i];
}
