/*********************************************************************
**    NAME         :  rioradel.c
**       CONTAINS:
**       uri_del_part_oracle() and stub for no ORACLE
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rioradel.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:45
*********************************************************************/

#include "udebug.h"
#include "uhep.h"
#include "rerrdef.h"

#ifdef UR_ORACLE

#include "usysdef.h"
#include "ribase.h"

/*********************************************************************
**    E_FUNCTION     :  uri_del_part_oracle(partnm)
**       delete a part from oracle database
**    PARAMETERS   
**       INPUT  : 
**				partnm	char*			part name to delete
**       OUTPUT :  
**          none
**    RETURNS      : 0 for no error else error code
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uri_del_part_oracle(partnm)
char			*partnm;				/* part name to delete */
{
	extern short	cur[5][32];	/* logon data area and 4 cursors			*/
	UU_LOGICAL		partexists;	/* part exists in the database */
	int				partid;		/* part id of the part */
	static char 	part_exist[] =
		"SELECT PART_ID FROM PART_LIS WHERE PART_NAME = :PART_NAME ";
	short				indp;
	static char 	del_part[] =
		"DELETE FROM PART_LIS WHERE PART_NAME = :PART_NAME AND PART_ID = :PART_ID";
	static char 	sl_geom[]  ="SELECT GEOM_TYPE,GEOM_ID FROM GEOMETRY";

	/* the SQL delete string is built for each type of geometry */
	/* by concatenating dl_geom1 + GEOM_TYPE + dl_geom2 -> dl_geom */
	char 				dl_geom[80];
	static char 	dl_geom1[] = "DELETE FROM ";
	static char 	dl_geom2[] = " WHERE PART_ID = :PART_ID ";
	short				ret_len,
						ret_code;	/* returned length & code from odefin	*/
	int				status;		/* holds status of unibase calls			*/
	char				geom_type[24];
	long				geom_id;

/*--------------------------------------------------------------------
** Start of Executable Code
**--------------------------------------------------------------------
*/
	uu_denter(UU_RTRC,(us,"uri_del_part_oracle"));

	/* get the part id which identifies this part */
	uu_dprint(UU_RITRC,(us,"retrieve part_id"));
	if(osql3(cur[3],part_exist,-1)	||
		odefin(cur[3],1,&partid,sizeof(partid),3,-1,&indp,-1,-1,-1,
			&ret_len,&ret_code)	||
		obndrv(cur[3],":PART_NAME",-1,partnm,-1,5,-1,(short *)-1)	||
		oexec(cur[3])
	 	)
	{
		uu_dprint(-1,(us,"ERROR:ur_del_part_oracle can't retrieve part id"));
		ur_o_erpt(cur[0],4);		/* error on parse, define, bind, exec		*/
		uu_dexit;
		return(URM_RDBMS_ERR);			/* return an error */
	}
	status = ofetch(cur[3]);
	while(status == 0)				/* got part id.	*/
	{
		/* delete part from part list */
		uu_dprint(UU_RITRC,(us,"delete part_id %d from part_list", partid));
		if(osql3(cur[1],del_part,-1)	||
			obndrv(cur[1],":PART_NAME",-1,partnm,-1,5,-1,(short *)-1)	||
			obndrv(cur[1],":PART_ID",-1,&partid,sizeof(partid),3,-1,
				(short *)-1)	||
			oexec(cur[1]) 
			)
		{
			uu_dprint(-1,(us,"ERROR:ur_del_part_oracle can't delete from part_lis"));
			ur_o_erpt(cur[0],4);	/* error on delete part from part list */
			uu_dexit;
			return(URM_RDBMS_ERR);			/* return an error */
		}
		uu_dprint(UU_RTRC,(us,"delete from part_list completed")) ;

		/* in preperation of delete all for this part parse the geom select, */
		/*	define a vars for it, and exec */
		if(osql3(cur[1],sl_geom,-1)	||
			odefin(cur[1],1,geom_type,sizeof(geom_type),5,-1,&indp,
				-1,-1,-1,&ret_len,&ret_code)	||
			odefin(cur[1],2,&geom_id,sizeof(geom_id),3,-1,&indp,
				-1,-1,-1,&ret_len,&ret_code)	||
			oexec(cur[1])
			)
		{
			uu_dprint(-1,(us,"ERROR:ur_del_part_oracle can't select from geom"));
			ur_o_erpt(cur[0],4);
			uu_dexit;
			return(URM_RDBMS_ERR);			/* return an error */
		}
		uu_dprint(UU_RTRC,(us," parse of geom select done")) ;

		/* for each geometry table in database */
		uu_dprint(UU_RITRC,(us,"retrieve geometry table"));
		while(ofetch(cur[1]) == 0)
		{
			uu_dprint(UU_RTRC,(us," select geom done"));
			strcpy(dl_geom,dl_geom1);	/* first build the delete sql statement */
			strcat(dl_geom,geom_type);
			strcat(dl_geom,dl_geom2);
			uu_dprint(UU_RTRC,(us,"%s",dl_geom));

			/* parse the SQL DELETE statement, bind PART_ID, & execute it */
			if(osql3(cur[2],dl_geom,-1)	||
				obndrv(cur[2],":PART_ID",-1,&partid,sizeof(partid),3,-1,
			 		(short *)-1)	||
				oexec(cur[2])
				)
			{
				uu_dprint(-1,(us,"ERROR:ur_del_part_oracle can't delete %s",geom_type));
				ur_o_erpt(cur[0],4);
				uu_dexit;
				return(URM_RDBMS_ERR);			/* return an error */
			}
			if (ocom(cur[0]))				/* commit each to avoid table limit */
			{
				uu_dprint(-1,(us,"ERROR:ur_del_part_oracle can't commit delete"));
				uu_dprint(UU_RITRC,(us,"commit failure"));
				ur_o_erpt(cur[0],4);
			}
			uu_dprint(UU_RITRC,(us,"retrieve geometry table"));
		}
		if(cur[1][0] != 4)	/* error ? selecting from geometry */
		{
			uu_dprint(-1,(us,"ERROR:ur_del_part_oracle can't select from geom"));
			ur_o_erpt(cur[0],4)	;
			uu_dexit;
			return(URM_RDBMS_ERR);			/* return an error */
		}
		status = ofetch(cur[3]);
	}
	uu_dexit;
	return(0);
}

#else
/* stub for ORACLEless machines */
uri_del_part_oracle(partnm)
char			*partnm;						/* part name to delete */
{
	uu_denter(UU_RTRC,(us,"uri_del_part_oracle-----(not available)"));
	uu_dexit;
	return(URM_NO_RDBMS);
}
#endif
