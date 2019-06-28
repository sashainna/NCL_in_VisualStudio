/*********************************************************************
**    NAME         :  archio.c
**
**       CONTAINS:	Structure archiving routines.
**			ug_getfile(buf,offset,len) 	-- read len words from archive file.
**			Gint ug_archid(id) 				-- returns archive index of structure.
**			garchstr(num_strs,str_ids)		-- write structures into archive file.
**			gretrstr(num_strs,str_nums)	-- retrieve structures from archive file.
**			gretrstr2(n,str,seg) 			-- retrieve strs into specified names.
**      	Gint ug_segsiz(l)					-- Calculate size of segment l.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
** 
**    MODULE NAME AND RELEASE LEVEL
**       garchio.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:17
*********************************************************************/
#include <stdio.h>
#include "gerrorst.h"
#include "gerrormet.h"
#include "gerrorid.h"
#include "gtbl.h"
#include "udebug.h"
#include "zsysdep.h"
#include "gsegac.h"
#include "ualloc.h"
#include "garch.h"
#include "xfsys1.h"

#define TRUE 1
#define FALSE 0

extern UU_STORE *uu_segstore;       /* store for segment data */

char *ug_lsielt();
char *ug_lsiins();

/*********************************************************************
**    I_FUNCTION     :  Gint ug_segsiz(l) 
**		Calculate archived size of segment l.
**    PARAMETERS   
**       INPUT  : 
**          UG_seg l -- segment id.
**       OUTPUT :  
**    RETURNS      : segment size in bytes.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static Gint ug_segsiz(l)
Gint l;
{
	Gint size;
	Gint cmdlen;
	Gint icmd, ncmd;
   UG_segstli *sp;				/* pointer to seg's header */

	uu_denter(UU_GTRC,(us,"ug_segsiz(%d)",l));

	sp=ug_segac(l);

	/* Add up the sizes of each element */
	size = sizeof(int);							/* Number of elements */
	ncmd = ug_lsinelt(sp->seglist);
	for( icmd = 0; icmd < ncmd; ++icmd){
		cmdlen = ug_lsilen(sp->seglist, icmd);
     	size += (cmdlen + sizeof(int));		/* Command + length */
	}

	uu_dprint(UU_GTRC,(us,"ug_segsiz returns %d",size));
	uu_dexit;
   return(size);
}

/*********************************************************************
**    I_FUNCTION     :  ug_getfile(buf,offset,len)				
**			 get len words starting at offset'th word.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_getfile(buf,offset,len)			/* get len words starting at offset'th word*/
Gint buf[];								/* buffer into which to read len words */
Gint offset,len;
{
	Gint i;

	uu_denter(UU_GTRC,(us,"ug_getfile(%x, %d, %d)", buf, offset, len));

	lseek(UG_Archfid, (long)(offset*sizeof(Gint)), 0);
	i = read(UG_Archfid, buf, len*sizeof(Gint));
	ug_check_rstat(i, len*sizeof(Gint), "ug_getfile");

	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  Gint ug_archid(id)					
**		 Return the index in archive file of segment id.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gint ug_archid(id)
Gint id;
{
	Gint i;
	Gint nstructs;	/* Number of structures in table of contents */
	UG_Tofc *h;		/* Table of contents entry of a segment */

	uu_denter(UU_GTRC,(us,"ug_archid(%d)", id));

	nstructs = ug_lsinelt(ug_tofc);
	uu_dprint(UU_GTRC,(us,"table length %d",nstructs));

	for (i=0; i < nstructs; i++) {
		h = (UG_Tofc *)ug_lsielt(ug_tofc, i);
		uu_dprint(UU_GTRC,(us,"struct %d named %d",i,h->id));
		if( h->id == id ) break;
	}

	if (i >= nstructs) i = -1;		/* Return -1 if no structure found */

	uu_dprint(UU_GTRC,(us,"ug_archid returns %d",i));
	uu_dexit;
	return(i);
}
	
/*********************************************************************
**    E_FUNCTION     :  Gerror garchstr(num_strs,str_ids)
**       Archive structures on the currently open archive file.
**    PARAMETERS   
**       INPUT  :  Gint num_strs -- number of structures to archive.
**						 Gint str_ids[num_strs] -- ids of structures to archive.
**       OUTPUT :   none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror garchstr(num_strs,str_ids)
/*$ INPUT */
Gint num_strs;
Gint str_ids[];
{
	Gint i;
	Gint list_count;					/* Structure we're traversing */
	Gint str_length;					/* Length of this structure */
	Gint elm_length;					/* Length of one structure element */
	Gint offset;						/* File offset of a structure */
	Gint ncmd;							/* number of commands in a segment */
	Gint icmd;							/* ith segment command */
	UG_Tofc *h;							/* Pointer to table of contents entry */
	UG_segstli *sp;					/* pointer to current segment header */
	Gint *cmdp;							/* pointer to current graphics cmd in sp */
	Gint nstructs;						/* Number of segments archived */
	Gint overwrite=FALSE;			/* TRUE if overwriting archived seg */
	Gint id;								/* Archive id if overwriting */

	uu_denter(UU_GTRC,(us,"garchstr(num_strs=%d, str_ids[0]=%d)",
					num_strs,str_ids[0]));
	
	/* Check the Phigs active state */
	if(!UG_AROP) {
		ug_errorhand(ENOARCF,"garchstr",NULL);
		uu_dexit;
		return;
	}

	/* Make toolstore the memory store for the table of contents */
	uu_alloc_push(uu_toolstore);
	
	/* Remember we've modified the archive */
	ug_tofcmod = TRUE;

	/* Current number of structures */
	nstructs = ug_lsinelt(ug_tofc);

	/* Position file pointer to location for writing this data */
   if(UG_NEWFILE) {

		uu_dprint(UU_GTRC,(us,"newfile, archfid = %d", UG_Archfid));
		/* Round to nearest multiple of TOFCINC large enough to hold num_strs */
		ug_tofcsize = (num_strs + TOFCINC) / TOFCINC  * TOFCINC;
		uu_dprint(UU_GTRC,(us,"ug_tofcsize=%d",ug_tofcsize));
   	ug_filelen = 
			UX_HDR_LEN + 						/* Xio file header */
			2*sizeof(Gint) +					/* 2 header Gints */
			ug_tofcsize*sizeof(UG_Tofc);	/* 3 Gints per table of contents entry */
   }

	/* Set the file position to the end of file */
	uu_dprint(UU_GTRC,(us,"ug_filelen %d",ug_filelen));

	list_count = lseek(UG_Archfid, ug_filelen, 0);
	uu_dprint(UU_GTRC,(us,"lseek returns %d", list_count));

	/* Process each list as requested */
	for( list_count=0; list_count<num_strs; list_count++ ) {

		uu_dprint(UU_GITRC,(us,"\nseg %d, id=%d",
			list_count, str_ids[list_count]));

		/* check if this list exists */
	   if (ug_segac(str_ids[list_count]) == NULL) {
			uu_dprint(-1,(us, "list %d doesn't exist\n",str_ids[list_count]));
			fprintf (stderr, "list %d doesn't exist\n",str_ids[list_count]);
			continue;
		}
		
   	/* Check if this structure exists in the existing archive file...
    	* If it's an old archive file get old structure position and length,
		* compare structure sizes, write new into old if it will fit.
		* If it won't fit, don't archive it.
		*/
		if( !UG_NEWFILE ) {
			id = ug_archid(str_ids[list_count]);
			if( id >= 0 ) {		/* Segment exists in archive */
				h = (UG_Tofc *)ug_lsielt(ug_tofc, id);
				uu_dprint(UU_GTRC,(us,"strct %d exists, len=%d", h->id, h->len));

				if( ug_segsiz(str_ids[list_count]) > h->len ) {
					uu_dprint(-1,(us,"segment won't fit in archive"));
					continue;
				}
				else {
					overwrite = 1;
					offset = h->offset;
					lseek(UG_Archfid, offset, 0);
					uu_dprint(UU_GTRC,(us,"overwriting existing seg at %d", offset));
				}
			}
			else {		/* Segment doesn't exist in archive, add to end */
				lseek(UG_Archfid, 0, 2);		/* Seek to end */
			}
		}

		if( !overwrite ) 	/* Write at end of existing file */
			offset = ug_filelen;

		str_length = 0;				/* Init the structure length */

		/* Get pointer to start of segment data */
		sp = ug_segac(str_ids[list_count]);

		/* Get number of structure elements */
		ncmd = ug_lsinelt(sp->seglist);

		/* Write number of elements in the structure */
		i = write(UG_Archfid, &ncmd, sizeof(Gint));
		ug_check_wstat (i, sizeof(Gint), "ncmds");
		str_length += sizeof(Gint);


		/* Process structure elements until there are no more */
		for( icmd=0; icmd < ncmd; icmd++ ) {

			cmdp = (Gint *)ug_lsielt(sp->seglist,icmd);
			elm_length = ug_lsilen(sp->seglist,icmd)/sizeof(Gint);

			/* Write out the element length and the element data */
			i = write(UG_Archfid, &elm_length, sizeof(Gint));
			ug_check_wstat(i, sizeof(Gint), "elm_length");

			uu_dprint(UU_GTRC,(us,
				"elm_length=%d, str_length=%d", 
				elm_length,str_length));
			i = write(UG_Archfid, cmdp, elm_length*sizeof(Gint));
			ug_check_wstat(i, elm_length*sizeof(Gint), "elm_data");

			/* Update str_length...add elm_length + 1 for elm_length */
			str_length += elm_length*sizeof(Gint) + sizeof(Gint); 

			uu_dprint(UU_GTRC,(us,"elm_data[0...1]=%d,%d, ug_filelen=%d", 
				cmdp[0],cmdp[1],ug_filelen));

		}		/* end for each structure element */

		/* Add this structure to table of contents */
		if( overwrite ) {
			uu_dprint(UU_GTRC,(us,"rewriting table of contents entry %d",id));
			uu_dprint(UU_GTRC,(us,"id %d, offset %d, length %d",
				id, offset, str_length));
			h->len = str_length;
			overwrite = 0;
		}
		else {
			uu_dprint(UU_GTRC,(us,"table of contents entry %d",nstructs));
			uu_dprint(UU_GTRC,(us,"id  %d, offset %d, length %d",
				str_ids[list_count], offset, str_length));
			h = (UG_Tofc *)ug_lsiins(ug_tofc, nstructs, sizeof(UG_Tofc));
			h->id = str_ids[list_count];
			h->offset = offset;
			h->len = str_length;

			nstructs++; 						/* Update number of strs in archive */
			ug_filelen += str_length;		/* Update length of data on file */
			uu_dprint(UU_GTRC,(us,"new file end %d", ug_filelen));
		}

	}	/* end for each structure */

	/* Remember, no longer a newfile... */
	UG_NEWFILE = FALSE;

	/* Restore the old memory store */
	uu_alloc_pop();
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     :  gretrstr(num_strs,str_nums)
**       Retrieve structures from the currently open archive file into
**			same segment numbers as structure names.
**    PARAMETERS   
**       INPUT  :	num_strs       -- number of structures to retrieve.
**						Gint str_nums[] -- ids of structures to retrieve.
**       OUTPUT :   none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
gretrstr(num_strs,str_nums)
/*$ INPUT */
  Gint num_strs;
  Gint str_nums[];
{
	uu_denter(UU_GTRC,(us,"gretrstr(num_strs=%d, str_nums[0]=%d), curvwindex=%d",
					num_strs,str_nums[0],ug_gksstli.curvwindex));
	gretrstr2(num_strs,str_nums,str_nums);
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     :  Gerror gretrstr2(num_strs,str_nums,seg_nums)
**       Retrieve structures from the currently open archive file into
**			segments specified by seg_nums.
**    PARAMETERS   
**       INPUT  :	num_strs       	-- number of structures to retrieve.
**						Gint str_nums[] 	-- ids of structures to retrieve.
**						Gint seg_nums[] 	-- ids of segments to retrieve into.
**													These segments should not exist.
**       OUTPUT :   none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gretrstr2(num_strs,str_nums,seg_nums)
/*$ INPUT */
Gint num_strs;
Gint str_nums[];
Gint seg_nums[];
{
	Gint i;
	Gint id;							/* Segment to be read */
	Gint newid;						/* Segment id to be created */
	Gint list_count;				/* Which structure we're retrieving */
	Gint str_length;				/* Length of a structure */
	Gint elm_count;				/* Which element we're using */
	Gint elm_length;				/* Length of one structure element */
	Gint num_elms;					/* Number of elements in a structure */
	Gint data_loc;					/* Location of segment data in file */
	UG_Tofc *h;						/* Table of contents entry of a segment */
	UG_segstli *newsp;			/* Pointer to new segment header */
	Gint *newcmdp;					/* Pointer to current graphics cmd in newsp*/
	UU_STORE *oldstore;			/* Memory store at invocation */

	uu_denter(UU_GTRC,(us,
		"gretrstr2(num_strs=%d, str_nums[0]=%d, seg_nums[0]=%d), curvwindex=%d",
		num_strs,str_nums[0],seg_nums[0],ug_gksstli.curvwindex));

	/* Check the Phigs active state */
	if (!UG_AROP) {
		uu_dprint(-1,(us,"Error. No archive open"));
		uu_dexit;
		return(ENOARCF); 
	}

	/* Can only read structures from an old file */
	if( UG_NEWFILE ) {
		uu_dprint(-1,(us,"Error. Can't read from new archive"));
		uu_dexit;
		return(ENOARCOLD);
	}

	/* Visible list no longer current */
	ug_vislistok = 0;

	/* Set the current store */
	oldstore = (UU_STORE *)uu_get_current_store();
	uu_set_current_store(uu_segstore);

	/* Process each list as requested */
	for (list_count=0; list_count < num_strs; list_count++) {

		id = ug_archid(str_nums[list_count]);	/* Get file posn of this struct */

		if (id<0) {			/* Requested segment not on archive file */
			uu_dprint(-1,(us,"requested structure %d not found", id));
			continue;
		}

		newid = seg_nums[list_count];

		if (ug_conresflag==UG_REPLACE) { /* PHIGS conresflag is replace */
			/* Delete this segment and create a new one - same name */
			if(ug_segac(newid)!=NULL) gdeleteseg(newid); 
		}

		/* Create the new segment */
		newsp = ug_segadd(newid);
		if (newsp==0) {
			uu_dprint(-1,(us,"Error. No segment space"));
			uu_dexit;
			return(EMEMSPAC);
		}

		uu_dprint(UU_GTRC,(us,"list_count=%d, id=%d, newid=%d",
			list_count,id,newid));

		h = (UG_Tofc *)ug_lsielt(ug_tofc, id);
		data_loc   = h->offset;
		str_length = h->len;

		/* Check for zero length structure */
		if (str_length == 0) {
  	     uu_dprint(-1,(us,"structure %d not in archive file/n",id));
  	     fprintf(stderr,"structure %d not in archive file/n",id);
		}
		else {

			uu_dprint(UU_GTRC,(us,"data_loc=%d",data_loc));

			/* offset file position to the data location in bytes */
         lseek(UG_Archfid, data_loc, 0);

			/* Initialize digs stuff for the structure */
			newsp->segid = newid;						/* segment id */
         zbytecp(newsp->segatts,ug_defsegat);	/* default attributes */
			newsp->rassegpt=NULL;						/* no raster copy */
			UG_RESET_WCBOX(newsp);						/* resets box & flag */
			newsp->xforms = -1;							/* worst cases */

			/* Read number of elements in the structure */
         i = read(UG_Archfid, &num_elms, sizeof(Gint));
			ug_check_rstat (i, sizeof(Gint), "num_elms");
			uu_dprint(UU_GTRC,(us,"Read num_elms=%d", num_elms));

			/* For each element, read its length and its data */
         for (elm_count=0 ; elm_count < num_elms ; elm_count ++) {

				/* Read the element length from the archive file */
				i = read (UG_Archfid, &elm_length, sizeof(Gint));
				ug_check_rstat (i, sizeof(Gint), "elm_length");

				/* Get pointer for element data in the segment */
				newcmdp = (Gint *)ug_lsiins(newsp->seglist, elm_count, 
								elm_length*sizeof(Gint));

				/* Read the element data from the archive file */
            i = read (UG_Archfid, newcmdp, elm_length * sizeof(Gint));
				ug_check_rstat (i, elm_length * sizeof(Gint), "elm_data");

			}
      }
	}

	uu_dprint(UU_GTRC,(us,"xforms = %x",newsp->xforms));

	/* Restore the memory store */
	uu_set_current_store(oldstore);

	uu_dexit;
	return(NCL_NO_ERROR);
}
