/*********************************************************************
**    NAME:  bautoup.c
**       CONTAINS:
**    		ub_conAutoUpdate(descrptr) 
**    		ubi_regenConnector(conptr,descrptr)
**				ubi_connects2SingleInst
**    		ubi_doTransf4ConnUpdate(conptr,updateNbr,transfWholeConn,
**				ubi_useStartTransf(conptr, tfUpdateOfStart,j1)
**    		ubi_chkMsgs4Regen(descrptr, tfupdate, nbrptr, quitptr)
**    		ubi_pairTransfs(updateStart,sNbr,updateEnd, eNbr)
**    		ubi_updateConTuple(conptr,causeKey,tfmat)
**    		ub_msymAutoUpdate(descrptr)
**    		ub_instAutoUpdate(descrptr)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       bautoup.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:02
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "mdrel.h"		/* for relation numbers */
#include "mdattr.h"		/* for UM_UNDISPLAYABLE */
#include "mdcoord.h"		/* for UM_transf */
#include "mcrv.h"			/* UM_polyline_rec */
#include "umessages.h"	/* for auto update messages */
#include "bsym.h"			/* for connector and instance definitions */
#include "r1emacro.h"	/*for ur1_nxtUpdateSameTuple, ur1_getUpdateSchedEntry */
#include "r1esched.h" 	/* for UR_assocUpdateDescriptor */

#define UB_MAX_UPDATES_SAME_TUPLE 100

#define CHK4ERRS(dataptr, errno, strg)		\
	if (dataptr == UU_NULL)						\
	{													\
		uu_uerror1(UB_SYMBOL, errno, strg);	\
		goto failed;								\
	}

#define UB_NOTPAIRED -1

/*
.....Modified data structure '*entry' to '*ntry' to compile on VMS.
.....'entry' is a VMS reserve word. - Roberta Z.
*/
typedef struct {
		UR_schedEntry *ntry;
		int paired;
		int updateNbr;
	} UB_updateMsg;

#define TRACE UU_TRUE	/* for debugging only */
/*********************************************************************
**    E_FUNCTION: ub_conAutoUpdate(descrptr) 
**			This function does the auto-updates for symbol instance connector 
**			associativity.	 There is too much to explain here about how it 
**			the associativity mechanism works.  Please refer to the documentation.
**    PARAMETERS   
**       INPUT: 
**				descrptr		Pointer to the associativity descriptor containing
**								the information for updating a connector.
**       OUTPUT:  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_conAutoUpdate(descrptr)
	UR_assocUpdateDescriptor *descrptr;
{
	struct UB_conector_rec con;
	UR_messagePacket *msgPacketptr;
	char *dataptr;
	int datasz;
	UU_LOGICAL stillooking = UU_TRUE;
	int dsegid, status = UU_SUCCESS;			
	uu_denter((UU_BTRC|UU_R1TRC),(us,"ub_conAutoUpdate(dscrptr>ascKy:%d,msg:%s)",
						descrptr->assocKey,uu_msg(UR1_CURNT_MSG(descrptr))));

	/* get next (first) message packet from the association descriptor */
	if (UR1_NXT_CURNT_MSG_PAK(descrptr, &msgPacketptr) != UU_SUCCESS)
			goto failed;
	/* get the first data buffer from the message packet; this should be
	 * a transformation. */
	if (ur1_getMsgData(msgPacketptr,0/*data nbr*/,&dataptr,&datasz)
			!= UU_SUCCESS) goto failed;

	if (ur_setup_data(UB_CONECTOR_REL, &con, sizeof(con)) != 0) goto failed;
	con.key = descrptr->assocKey;
	if (ur_retrieve_app_data(&con) != UU_SUCCESS) goto failed;
	switch (UR1_GET_MSG(msgPacketptr))
	{
		case UU_TRANSLATE:	/* an associated instance was translated */
			uu_dprint((UU_BTRC|UU_R1TRC),(us,"translate message"));
			stillooking = UU_FALSE;
		case UU_ROTATE:
			if (stillooking)
			{
				uu_dprint((UU_BTRC|UU_R1TRC),(us,"rotate message"));
				stillooking = UU_FALSE;
			}
		case UU_MIRROR:
			if (stillooking)
			{
				uu_dprint((UU_BTRC|UU_R1TRC),(us,"mirror message"));
				stillooking = UU_FALSE;
			}
		case UU_SCALE:
			if (stillooking)
			{
				uu_dprint((UU_BTRC|UU_R1TRC),(us,"scale message"));
				stillooking = UU_FALSE;
			}
			/* now do updates to connector for translate, rotate, mirror, scale;
			 * note, "UR1_AUTO_UPDATE" be used at a low enough level so that
			 * it is clear what message is to be passed on to other associated
			 * entities in "msgPacketptr".  It just so happens that for instance
			 * connector pairs, every message that was received from an instance
			 * saying: "this was done to me" is the same thing that we are going
			 * to do to the connector here; i.e. UU_TRANSLATE of an instance causes
			 * its associated connectors to be UU_TRANSLATEd; albeit, 
			 * UU_TRANSLATE has a little different meaning here; i.e. for 
			 * connectors this means either an end segment got stretched, or, 
			 * the entire connector got translated. */
			UR1_AUTO_UPDATE(&con,msgPacketptr, 1 /* nbr message packets */,
					UU_NO_REC_CHANGE,
					ubi_updateConTuple(&con,descrptr->causeKey,dataptr), &status);
			if (status != UU_SUCCESS) goto failed;

			if (uc_display(&con) != UU_SUCCESS) goto failed;
			break;
		case UU_DELETE: /* instance deleted; delete connector */
			uu_dprint((UU_BTRC|UU_R1TRC),(us,"delete message"));
			UR1_AUTO_UPDATE(&con,msgPacketptr, 1 /* nbr message packets */,
					UU_REC_CHANGE, ub_dl64_connect(descrptr->assocKey), &status);
			if (status != UU_SUCCESS) goto failed;
			break;
		case UU_VIS: /* instance set visible */
			uu_dprint((UU_BTRC|UU_R1TRC),(us,"visible message"));
			break;
		case UU_INVIS: /* an instance made invisible; make connector invisible */
			uu_dprint((UU_BTRC|UU_R1TRC),(us,"invisible message"));
			if (ub_retrieve_disp_segid(descrptr->assocKey, &dsegid) != UU_SUCCESS)
					goto failed;
			uv_blanksegs(dsegid, descrptr->assocKey);
			if (ur_update_displayable(descrptr->assocKey, UM_UNDISPLAYABLE)
					!= UU_SUCCESS) goto failed;
			break;
		case UU_CHANGE_ATTR:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,"change attribute message"));
			break;
		case UU_REGENERATE: /* there is more than 1 update pending */
			uu_dprint((UU_BTRC|UU_R1TRC),(us,"regenerate message"));

			/* UR1_AUTO_UPDATE(&con,msgPacketptr,0,
						ubi_regenConnector(&con,descrptr),&status); */
			if (ubi_regenConnector(&con,descrptr) != UU_SUCCESS) goto failed;

			if (uc_display(&con) != UU_SUCCESS) goto failed;
			break;
		case UU_DEFAULT_UPDATE:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update message, no update done"));
			break;
		case UU_DEFAULT_UPDT_FIXED:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update fixed message, no update done"));
			break;
		case UU_DEFAULT_UPDT_DVARL:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update varlist message, no update done"));
			break;
		case UU_DEFAULT_UPDT_TRANS:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update transf, no update done"));
			break;
		case UU_DEFAULT_UPDT_ATTR:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update attribute bundle message, no update done"));
			break;
		case UU_DEFAULT_UPDT_DSEG:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update display segment message, no update done"));
			break;
		case UU_DEFAULT_UPDT_COLOR:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update color message, no update done"));
			break;
		case UU_DEFAULT_UPDT_LAYER:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update layer message, no update done"));
			break;
		case UU_DEFAULT_UPDT_PEN:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update pen message, no update done"));
			break;
		case UU_DEFAULT_UPDT_LNSTY:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update line style message, no update done"));
			break;
		case UU_DEFAULT_UPDT_LNWGT:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update line wt. message, no update done"));
			break;
		case UU_DEFAULT_UPDT_LNWID:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update line width message, no update done"));
			break;
		case UU_DEFAULT_UPDT_DISP:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update displayable message, no update done"));
			break;
		case UU_DEFAULT_UPDT_SEL:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update selectable message, no update done"));
			break;
		case UU_DEFAULT_UPDT_BLNKD:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update blanked message, no update done"));
			break;
		case UU_DEFAULT_UPDT_VKEY:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update view key message, no update done"));
			break;
		case UU_DEFAULT_DELETE:	
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default delete message, no update done"));
			break;
		case UU_DEFAULT_DEL_ATTR:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default delete attribute bundle message, no update done"));
			break;
		case UU_DEFAULT_DEL_DVARL:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default delete variable list message, no update done"));
			break;
		case UU_DEFAULT_DEL_TRANS:
			uu_dprint((UU_BTRC|UU_R1TRC),(us, 
					"default delete transf message, no update done"));
			break;
		case UU_DEFAULT_DEL_VARLA:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default delete variable list atom(s) message, no update done"));
			break;
		case UU_NOMESSAGE:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
				"At UU_NOMESSAGE handler, no update done"));
			break;
		default: goto failed;
	}
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ub_conAutoUpdate", status);
	return(status);
}	

/*********************************************************************
**    I_FUNCTION:int ubi_regenConnector(conptr,descrptr)
**       This function does the actual update of a connector to Unibase.
**    PARAMETERS   
**       INPUT: 
**			conptr		Pointer to the connector entity to be updated.
**			descrptr		Pointer to the update record.
**       OUTPUT :  
**          conptr		Pointer to the updated entity.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubi_regenConnector(conptr, descrptr)
	struct UB_conector_rec *conptr;
	UR_assocUpdateDescriptor *descrptr;
{
	UR_assocUpdateDescriptor *topDescrptr, *nxtdescrptr;
	UB_updateMsg transfWholeConn[UB_MAX_UPDATES_SAME_TUPLE];
	UB_updateMsg tfUpdateOfStart[UB_MAX_UPDATES_SAME_TUPLE];
	UB_updateMsg tfUpdateOfEnd[UB_MAX_UPDATES_SAME_TUPLE];
	UU_REAL (*tfmat)[3]; /* to access a 4 by 3 transformation easily */
	UU_LOGICAL stillRegening = UU_TRUE;
	int tfupdateNbr = 0;
		/* this variable helps keep the order of the updates to the connector
		 * the same as in the schedule */
	int i, j, k, tfNbr = 0, tf1Nbr = 0, tf2Nbr = 0;
	int tfNbrsav, tf1Nbrsav, tf2Nbrsav, j1, j2, status = UU_SUCCESS;
	uu_denter((UU_BTRC|UU_R1TRC),(us,
			"ubi_regenConnector(conptr->key:%d,descrptr:%x)", 
			conptr->key,descrptr));

	topDescrptr = descrptr;
	ur1_nxtUpdateSameTuple(topDescrptr);
	/* now have the first real update in "topDescrptr" */

	nxtdescrptr = topDescrptr;
	while ((nxtdescrptr != UU_NULL) && (stillRegening))
	{
#if (TRACE)
		uu_dprint(UU_BTRC,(us,"found an update descriptor to check"));
#endif
		/* note, among other things, this loop takes care of the case where 
		 * the connector connects snap nodes of a single instance. */
		if ((conptr->ainst[0] == nxtdescrptr->causeKey) 
				&& (conptr->ainst[1] == nxtdescrptr->causeKey))
		{ /* then the connector connects snap nodes from a single instance 
			* so there will be 2 update entries for this connector and they
			* must be paired so that the connector is transformed as a unit. */
			if (ubi_connects2SingleInst(conptr,nxtdescrptr,transfWholeConn,&tfNbr,
					&tfupdateNbr,&stillRegening) != UU_SUCCESS) goto failed;
			if (!stillRegening) goto done;
		}
		else if (conptr->ainst[0] == nxtdescrptr->causeKey) 
		{	/* then first instance caused update */
#if (TRACE)
			uu_dprint(UU_BTRC,(us,"first instance caused update descriptor"));
#endif
			tf1Nbrsav = tf1Nbr;
			if (ubi_chkMsgs4Regen(conptr, nxtdescrptr, tfUpdateOfStart, &tf1Nbr, 
						&stillRegening) != UU_SUCCESS) goto failed;
			if (!stillRegening) goto done; /* nothing else to do */
			if (tf1Nbr != tf1Nbrsav) 
			{	/* then there is at least one update involving tranfs */
				tfUpdateOfStart[tf1Nbr-1].updateNbr = tfupdateNbr;
				tfupdateNbr++; /* an accounting convenience */
			}
		}
		else if (conptr->ainst[1] == nxtdescrptr->causeKey) 
		{	/* then second instance caused update */
#if (TRACE)
			uu_dprint(UU_BTRC,(us,"second instance caused update descriptor"));
#endif
			tf2Nbrsav = tf2Nbr;
			if (ubi_chkMsgs4Regen(conptr, nxtdescrptr, tfUpdateOfEnd, &tf2Nbr, 
						&stillRegening) != UU_SUCCESS) goto failed;
			if (!stillRegening) goto done;
			if (tf2Nbr != tf2Nbrsav) 
			{	/* then there is at least one update involving tranfs */
				tfUpdateOfEnd[tf2Nbr-1].updateNbr = tfupdateNbr;
				tfupdateNbr++; /* an accounting convenience */
			}
		}
		ur1_nxtUpdateSameTuple(nxtdescrptr);
	} /* end while */
	if (tfupdateNbr == 0) goto done;
	/* otherwise, check to see which of the transformation updates apply to 
	 * both ends of the connector rather than just one; transformations are 
	 * assumed to be in the first data buffer of the first message packet and 
	 * nothing else can be there */
	if (ubi_pairTransfs(tfUpdateOfStart,tf1Nbr, tfUpdateOfEnd,tf2Nbr) 
				!= UU_SUCCESS) goto failed;

	/* now do transformations of the connector */
	if (ubi_doTransf4ConnUpdate(conptr,tfupdateNbr,transfWholeConn,tfNbr,
			tfUpdateOfStart,tf1Nbr,tfUpdateOfEnd,tf2Nbr) != UU_SUCCESS)
			goto failed;
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ubi_regenConnector", status);
	return(status);
}	

/*********************************************************************
**    I_FUNCTION:int ubi_connects2SingleInst(conptr,descrptr,transfWholeConn,
**									tfNbrptr,tfupdateNbrptr,stillRegeningptr)
**		This function takes care of the case where the connector has both it's
**		end points on the same connector. That is, if the connector needs 
**		updated for something other than a transformation, it ought to be
**		done here; transformations are retrieved here and passed back so
**		that connector can be transformed later when all transforms have
**		been obtained.
**    PARAMETERS   
**       INPUT: 
**				conptr				Pointer to the connector entity.
**				descrptr				Pointer to an association update descriptor for 
**										the connector.
**				transfWholeConn	Array of update records to store any transforms
**										obtained.
**				tfNbrptr				Pointer to the number of valid entries in 
**										"transfWholeConn"
**				tfupdateNbrptr		An accounting convenience indicating the number 
**										total transforms being applied to the connector.
**       OUTPUT :  
**				transfWholdConn	May contain a new transform record.
**				tfNbrptr				Pointer to the number of valid entries in 
**										"transfWholeConn"
**				tfupdateNbrptr		An accounting convenience indicating the number 
**										total transforms being applied to the connector.
**				stillRegeningptr	Pointer to UU_TRUE iff we should continue trying
**										to regenerate; i.e. the connector wasn't deleted 
**										here.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubi_connects2SingleInst(conptr,descrptr,transfWholeConn,tfNbrptr,
					tfupdateNbrptr, stillRegeningptr)
	struct UB_conector_rec *conptr;
	UR_assocUpdateDescriptor *descrptr;
	UB_updateMsg transfWholeConn[UB_MAX_UPDATES_SAME_TUPLE];
	int *tfNbrptr;
	int *tfupdateNbrptr;
	UU_LOGICAL *stillRegeningptr;
{
	UU_LOGICAL um_tfeqtf();
	UR_messagePacket *msgPacketptr, *tfMsgPacketptr;
	char *dataptr, *tfdataptr;
	int datasz, tfdatasz;
	int tfNbrsav, i, status = UU_SUCCESS;
	uu_denter(UU_BTRC,(us,
	"ubi_connects2SingleInst(conptr,dscrptr,tfWhlCon,tfNbrptr,tfupdtNbrptr,?)"));

	tfNbrsav = *tfNbrptr;
	if (ubi_chkMsgs4Regen(conptr,descrptr,transfWholeConn,tfNbrptr,
					stillRegeningptr) != UU_SUCCESS) goto failed;
	if (!(*stillRegeningptr)) goto done; /* nothing else to do */
	if (*tfNbrptr != tfNbrsav) 
	{	/* then a transformation was passed as part of the message;
		 * check to see if it's pair has come thru. Note, change color messages
		 * and such require no transformation. */
		for (i=0; i<tfNbrsav; i++) /* chk against all prior transfs */
		{
			/* get next (first) message packet from the association descriptor */
			if (UR1_NXT_CURNT_MSG_PAK(descrptr, &msgPacketptr) != UU_SUCCESS)
				goto failed;
			/* get first data buffer from the packet; should be transformation. */
			if (ur1_getMsgData(msgPacketptr,0/*data nbr*/,&dataptr,&datasz)
				!= UU_SUCCESS) goto failed;

			if (ur1_getMsgPacket(&(transfWholeConn[i].ntry->descrpt),0/*msg nbr*/,
						&tfMsgPacketptr) != UU_SUCCESS) goto failed;
			if (ur1_getMsgData(tfMsgPacketptr,0/*data nbr*/,&tfdataptr,&tfdatasz)
					!= UU_SUCCESS) goto failed;

			if (	/* chk to see if the transformations are the same */
					um_tfeqtf(dataptr,tfdataptr) 
			  		/* chk to see that it is not already paired */
			  		&& (transfWholeConn[i].paired == UB_NOTPAIRED) ) 
			{	/* then throw the new one away */
				transfWholeConn[i].paired = 0; /* any non-negative nbr ok here */
				*tfNbrptr = tfNbrsav;
				break;
			}
		}
		if (i == tfNbrsav)/* then not previously found */
		{
			transfWholeConn[*tfNbrptr-1].updateNbr = *tfupdateNbrptr;
			(*tfupdateNbrptr)++;
		}
	}
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ubi_connects2SingleInst", status);
	return(status);
}	

/*********************************************************************
**    I_FUNCTION:int ubi_doTransf4ConnUpdate(conptr,updateNbr,transfWholeConn,
**							 tfNbr, tfUpdateOfStart, tf1Nbr,	tfUpdateOfEnd, tf2Nbr)
**       This function transforms the connector due to auto-updates.
**    PARAMETERS   
**       INPUT: 
**			conptr		Pointer to the connector entity to be updated.
**			updateNbr	An accounting convenience, this is the number of
**							total transformations causing updates to the 
**							connector; i.e. = tfNbr+tf1Nbr+tf2Nbr.
**			transWholeConn	Array of transformation records where the entire
**								connector is to be transformed (because the 
**								connector "connects" an instance to itself).
**			tfNbr				Number of valid records in "transWholeConn".
**			tfUpdateOfStart	Array of transformation records where at 
**									least the start end of the connector is to be
**									transformed; note, the whole connector may be
**									transformed if a start record is also paired
**									with an end record.
**			tf1Nbr			Number of valid records in "tfUpdateOfStart"
**			tfUpdateOfEnd	Array of transformation records where the end
**								of a connector is to be transformed.
**			tf2Nbr			Number of valid records in "tfUpdateOfEnd".
**       OUTPUT :  
**          conptr		Pointer to the updated entity.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

/**** this macro only moves a single end ******/
#define UB_USE_END_TRANSF	\
{ \
	if (tfUpdateOfEnd[j2].paired == UB_NOTPAIRED)\
	{\
		uu_dprint((UU_BTRC|UU_R1TRC),(us,"transforming end pt of connector %d",conptr->key));\
		if (ur1_getMsgPacket(&(tfUpdateOfEnd[j2].ntry->descrpt),0/*msg nbr*/, \
					&msgPacketptr) != UU_SUCCESS) goto failed; \
		if (ur1_getMsgData(msgPacketptr,0/*data nbr*/,&dataptr,&datasz) \
				!= UU_SUCCESS) goto failed; \
		causeKey = tfUpdateOfEnd[j2].ntry->descrpt.causeKey;\
		ur1_printTfmat(UU_BTRC,dataptr,"from ubi_doTransf4ConnUpdate transform used was:"); \
		if (ubi_updateConTuple(conptr,causeKey,dataptr) != UU_SUCCESS) \
				goto failed;\
	}\
	j2++; \
}

#define UB_TRANSF_WHOLE_CONN	\
{ \
	if (ur1_getMsgPacket(&(transfWholeConn[j0].ntry->descrpt),0/*msg nbr*/, \
				&msgPacketptr) != UU_SUCCESS) goto failed; \
	if (ur1_getMsgData(msgPacketptr,0/*data nbr*/,&dataptr,&datasz) \
				!= UU_SUCCESS) goto failed; \
	uu_dprint((UU_BTRC|UU_R1TRC),(us,"transforming whole connector %d",conptr->key));\
	ur1_printTfmat(UU_BTRC,dataptr,"from ubi_doTransf4ConnUpdate transform used was:"); \
	if (ub_transf64_connect(conptr, dataptr, UU_TRUE) != UU_SUCCESS)\
			goto failed;\
	j0++; \
}


/**************** HERE'S THE FUNCTION *******************/
int ubi_doTransf4ConnUpdate(conptr,updateNbr,transfWholeConn, tfNbr, 
					tfUpdateOfStart, tf1Nbr, tfUpdateOfEnd, tf2Nbr)
	struct UB_conector_rec *conptr;
	int updateNbr;
	UB_updateMsg transfWholeConn[];
	int tfNbr;
	UB_updateMsg tfUpdateOfStart[UB_MAX_UPDATES_SAME_TUPLE];
	int tf1Nbr;
	UB_updateMsg tfUpdateOfEnd[UB_MAX_UPDATES_SAME_TUPLE];
	int tf2Nbr;
{
	UR_assocUpdateDescriptor *topDescrptr, *nxtdescrptr;
	UR_messagePacket *msgPacketptr;
	char *dataptr;
	int datasz;
	UU_KEY_ID causeKey;
	int j, j0, j1, j2; 
	int status = UU_SUCCESS;
	uu_denter((UU_BTRC|UU_R1TRC),(us,
	"ubi_doTransf4ConnUpdate(conptr,updatNbr:%d,,tfNbr:%d,,tf1Nbr:%d,tf2Nbr:%d)",
						updateNbr,tfNbr,tf1Nbr,tf2Nbr));

	j0 = j1 = j2 = 0;
	for (j=0; j<updateNbr; j++) /* for each transformation update */
	{
		if (j0 < tfNbr)
		{
			if (j1 < tf1Nbr)
			{	
				if (j2 < tf2Nbr)
				{
					if ((transfWholeConn[j0].updateNbr <= 
													tfUpdateOfStart[j1].updateNbr) &&
						 (transfWholeConn[j0].updateNbr <= 
													tfUpdateOfEnd[j2].updateNbr))
									UB_TRANSF_WHOLE_CONN /* macro above */
					else /* one of the other 2 has the smallest "updateNbr" */
						if (tfUpdateOfStart[j1].updateNbr <= 
													tfUpdateOfEnd[j2].updateNbr)
						{
							if (ubi_useStartTransf(conptr, tfUpdateOfStart, j1)
								!= UU_SUCCESS) goto failed;
							j1++;
						}

					else /* only update second point */		
									UB_USE_END_TRANSF	/* macro above */
				}/* j2 < tf2Nbr */
				else /* only j0 < tfNbr and j1 < tf1Nbr */
					if (transfWholeConn[j0].updateNbr 
									<= tfUpdateOfStart[j1].updateNbr)
									UB_TRANSF_WHOLE_CONN
					else /* first end causes update */
					{
							if (ubi_useStartTransf(conptr, tfUpdateOfStart, j1)
								!= UU_SUCCESS) goto failed;
							j1++;
					}
			}/* j1 < tf1Nbr */
			else /* j1 >= tf1Nbr  and j0 < tfNbr */
				if (j2 < tf2Nbr)
				{
					if (transfWholeConn[j0].updateNbr <= tfUpdateOfEnd[j2].updateNbr)
									UB_TRANSF_WHOLE_CONN
					else /* "end" has smallest "updateNbr" */
									UB_USE_END_TRANSF
				}
				else /* only j0 < tfNbr */
									UB_TRANSF_WHOLE_CONN
		}
		else /* j0 >= tfNbr */
			if (j1 < tf1Nbr)
			{
				if (j2 < tf2Nbr)
				{
					if (tfUpdateOfStart[j1].updateNbr <= tfUpdateOfEnd[j2].updateNbr)
					{
						if (ubi_useStartTransf(conptr, tfUpdateOfStart, j1)
							!= UU_SUCCESS) goto failed;
						j1++;
					}
					else
									UB_USE_END_TRANSF
				}
				else /* j2 >= tf2Nbr and j1 < tf1Nbr and j0 >= tfNbr */
				{
						if (ubi_useStartTransf(conptr, tfUpdateOfStart, j1)
								!= UU_SUCCESS) goto failed;
						j1++;
				}
			}
			else /* j0 >= tfNbr and j1 >= tf1Nbr */
				if (j2 < tf2Nbr)
					UB_USE_END_TRANSF
				else; /* do nothing */
	}
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ubi_doTransf4ConnUpdate", status);
	return(status);
}	

/*********************************************************************
**    I_FUNCTION : int ubi_useStartTransf(conptr, tfUpdateOfStart,j1)
**			This function transforms either the start point of the connector
**			or the whole connector.  It depends on whether the transformation 
**			in the record, "tfUpdateOfStart[j1]", is paired with a
**			transformation in another record.  If it is, then the whole
**			connector is transformed; if it isn't, then only the start point
**			is transformed.
**    PARAMETERS   
**       INPUT  : 
**				conptr				Pointer to the connector to be transformed.
**				tfUpdateOfStart	Array of records containing the transformations
**										to be applied to the start point of the connector
**										or to the whole connector.  The choice depends
**										on whether the transformation is "paired".
**				j1						Index to the record in "tfUpdateOfStart"
**										to use.
**       OUTPUT : none. 
**    RETURNS      : UU_SUCCESS if no problems encountered, UU_FAILURE 
**							otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubi_useStartTransf(conptr, tfUpdateOfStart, j1)
	struct UB_conector_rec *conptr;
	UB_updateMsg tfUpdateOfStart[UB_MAX_UPDATES_SAME_TUPLE];
	int j1;
{ 
	UR_messagePacket *msgPacketptr;
	UU_KEY_ID causeKey;
	char *dataptr;
	int datasz;
	int status = UU_SUCCESS;
	uu_denter(UU_BTRC,(us,"ubi_useStartTransf(tfUpdateOfStart,recNbr:%d)", j1));

	if (ur1_getMsgPacket(&(tfUpdateOfStart[j1].ntry->descrpt),0/*msg nbr*/, 
					&msgPacketptr) != UU_SUCCESS) goto failed; 
	if (ur1_getMsgData(msgPacketptr,0/*data nbr*/,&dataptr,&datasz) 
					!= UU_SUCCESS) goto failed; 

	ur1_printTfmat(UU_BTRC,dataptr,
		"from ubi_useStartTransf transform used was:"); 

	if (tfUpdateOfStart[j1].paired == UB_NOTPAIRED) 
	{ 	/* then merely transform connector end point */
		uu_dprint((UU_BTRC|UU_R1TRC),(us,
			"transforming only start end of connector %d",conptr->key));
		causeKey = tfUpdateOfStart[j1].ntry->descrpt.causeKey;
		if (ubi_updateConTuple(conptr,causeKey,dataptr) != UU_SUCCESS) 
				goto failed;
	}
	else /* the instances were moved together; so transform the entire 
			* connector */
	{
		uu_dprint((UU_BTRC|UU_R1TRC),(us,
			"transforming both ends of connector %d",conptr->key));
		if (ub_transf64_connect(conptr, dataptr, UU_TRUE) != UU_SUCCESS)
				goto failed;
	}
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ubi_useStartTransf", status);
	return(status);
}

/*********************************************************************
**    I_FUNCTION:int ubi_chkMsgs4Regen(conptr, descrptr, tfupdate, nbrptr, 
**											quitptr)
**			This function inspects the message for the auto-update descriptor,
**			"descrptr".  Depending on what the message is, this function
**			may store away a pointer(s) to the update message(s) so that they
**			can be processed appropriately later, or, delete the
**			connector altogether if an associated instance is deleted.
**			Note, we assume the primary message can not be UU_REGENERATE; if it
**			is this function will fail!
**    PARAMETERS   
**       INPUT: 
**				conptr		Pointer to the connector to be updated.
**				descrptr		The auto-update descriptor to have its message
**								checked.
**				tfupdate		Buffer for collecting messages, in particular,
**								transformations.
**				nbrptr		Pointer to number of valid (filled-in) entries in 
**								"tfupdate".
**       OUTPUT :  
**				tfupdate		Buffer for collecting messages, in particular,
**								schedule update records.
**				nbrptr		Pointer to number of valid entries in "tfupdate".
**				quitptr		Pointer to UU_TRUE iff the connector has been 
**								deleted.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubi_chkMsgs4Regen(conptr, descrptr, tfupdate, nbrptr, quitptr)
	struct UB_conector_rec *conptr;
	UR_assocUpdateDescriptor *descrptr;
	UB_updateMsg tfupdate[];
	int *nbrptr;
	UU_LOGICAL *quitptr;
{
	UR_assocUpdateDescriptor *topDescrptr;
	UR_messagePacket *msgPacketptr; 
	char *dataptr; 
	int datasz, i, dsegid, status = UU_SUCCESS;
	uu_denter((UU_BTRC|UU_R1TRC),(us,
		"ubi_chkMsgs4Regen(conptr,descrptr>message:%s,tfupdate,nbr:%d,?)",
		uu_msg(UR1_CURNT_MSG(descrptr)),*nbrptr));

	i = *nbrptr;
	switch (UR1_CURNT_MSG(descrptr))
	{
		case UU_TRANSLATE:	/* an associated instance was translated */
		case UU_ROTATE:		/* an associated instance was rotated */
		case UU_MIRROR:		/* an associated instance was mirrored */
		case UU_SCALE:			/* an associated instance was scaled */
			if (UR1_CURNT_MSG_PAK(descrptr, &msgPacketptr) != UU_SUCCESS)
					goto failed;
			if (ur1_getMsgData(msgPacketptr, 0/*data nbr*/, &dataptr, &datasz)
					!= UU_SUCCESS) goto failed;
#if (TRACE)
			ur1_printTfmat(UU_BTRC,dataptr,"from ubi_chkMsgs4Regen");
#endif

			CHK4ERRS(dataptr, 1111, "ubi_chkMsgs4Regen")
			/* error: No transformation data given (%s) */
			tfupdate[i].ntry = ur1_getUpdateSchedEntry(descrptr);
			tfupdate[i].paired = UB_NOTPAIRED;
			i++;
			if (i >= UB_MAX_UPDATES_SAME_TUPLE) goto failed;
			break;
		case UU_DELETE: /* instance deleted; delete connector */
			uu_dprint((UU_BTRC|UU_R1TRC),(us,"delete message found"));
			if (UR1_CURNT_MSG_PAK(descrptr, &msgPacketptr) != UU_SUCCESS)
					goto failed;

			UR1_AUTO_UPDATE(conptr,msgPacketptr,1,UU_REC_CHANGE,
					ub_dl64_connect(descrptr->assocKey),&status);
			if (status != UU_SUCCESS) goto failed;
			*quitptr = UU_TRUE;
			break;
		case UU_VIS: /* instance set visible */
			break;
		case UU_INVIS: /* an instance made invisible; make connector invisible */
			if (ub_retrieve_disp_segid(descrptr->assocKey, &dsegid) != UU_SUCCESS)
					goto failed;
			uv_blanksegs(dsegid, descrptr->assocKey);
			if (ur_update_displayable(descrptr->assocKey, UM_UNDISPLAYABLE)
					!= UU_SUCCESS) goto failed;
			break;
		case UU_CHANGE_ATTR:
			break;
		case UU_DEFAULT_UPDATE:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update message"));
			break;
		case UU_DEFAULT_UPDT_FIXED:
			uu_dprint((UU_BTRC|UU_R1TRC), (us,
					"default update fixed message"));
			break;
		case UU_DEFAULT_UPDT_DVARL:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update varlist message"));
			break;
		case UU_DEFAULT_UPDT_TRANS:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update transf"));
			break;
		case UU_DEFAULT_UPDT_ATTR:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update attribute bundle message"));
			break;
		case UU_DEFAULT_UPDT_DSEG:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update display segment message"));
			break;
		case UU_DEFAULT_UPDT_COLOR:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update color message"));
			break;
		case UU_DEFAULT_UPDT_LAYER:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update layer message"));
			break;
		case UU_DEFAULT_UPDT_PEN:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update pen message"));
			break;
		case UU_DEFAULT_UPDT_LNSTY:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update line style message"));
			break;
		case UU_DEFAULT_UPDT_LNWGT:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update line wt. message"));
			break;
		case UU_DEFAULT_UPDT_LNWID:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update line width message"));
			break;
		case UU_DEFAULT_UPDT_DISP:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update displayable message"));
			break;
		case UU_DEFAULT_UPDT_SEL:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update selectable message"));
			break;
		case UU_DEFAULT_UPDT_BLNKD:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update blanked message"));
			break;
		case UU_DEFAULT_UPDT_VKEY:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default update view key message"));
			break;
		case UU_DEFAULT_DELETE:	
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default delete message"));
			break;
		case UU_DEFAULT_DEL_ATTR:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default delete attribute bundle message"));
			break;
		case UU_DEFAULT_DEL_DVARL:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default delete variable list message"));
			break;
		case UU_DEFAULT_DEL_TRANS:
			uu_dprint((UU_BTRC|UU_R1TRC),(us, 
					"default delete transf message"));
			break;
		case UU_DEFAULT_DEL_VARLA:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,
					"default delete variable list atom(s) message"));
			break;
		case UU_REGENERATE: /* there is more than 1 update pending */
			goto failed;
		case UU_NOMESSAGE:
			uu_dprint((UU_BTRC|UU_R1TRC),(us,"no update done"));
			break;
		default: 
			uu_dprint((UU_BTRC|UU_R1TRC),(us,"UNKNOWN MESSAGE"));
			goto failed;
	}
	*nbrptr = i;

	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ubi_chkMsgs4Regen", status);
	return(status);
}	

/*********************************************************************
**    I_FUNCTION:int ubi_pairTransfs(updateStart,sNbr,updateEnd, eNbr)
**			This function pairs up those update entries that would cause
**			the connector to be completely transformed rather than just 
**			one of it's end lines modified.  For example, if both 
**			instances to which a connector is associated are translated, 
**			then the connector ought to be translated rather than its end 
**			line segments modified.
**    PARAMETERS   
**       INPUT: 
**				updateStart	Array of records, each containing a schedule entry
**								such that all these entries were caused by the first
**								instance on the connector's list.
**				sNbr			The number of valid elements in "update1".
**				updateEnd	Array of records, each containing a schedule entry
**								such that all these entries were caused by the second
**								instance on the connector's list.
**				eNbr			The number of valid elements in "update2".
**       OUTPUT :  
**				updateStart	Array of records, each containing a schedule entry
**								such that all these entries were caused by the first
**								instance on the connector's list; but now we have 
**								determined whether each record is paired with one from
**								"update2".
**				updateEnd	Array of records, each containing a schedule entry
**								such that all these entries were caused by the second
**								instance on the connector's list; but now we have 
**								determined whether each record is paired with one from
**								"update1".
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubi_pairTransfs(updateStart,sNbr, updateEnd, eNbr)
	UB_updateMsg updateStart[];
	int sNbr;
	UB_updateMsg updateEnd[];
	int eNbr;
{
	UU_LOGICAL um_tfeqtf();
	UB_updateMsg *jptr, *kptr;
	UR_messagePacket *jmsgPacketptr, *kmsgPacketptr;
	char *jdataptr, *kdataptr;
	int jdatasz, kdatasz;
	int j, k, status = UU_SUCCESS;
	uu_denter((UU_BTRC|UU_R1TRC),(us,
		"ubi_pairTransfs(upStart,nbr:%d,upEnd,nbr:%d)", sNbr, eNbr));

	for (j=0; j<sNbr; j++)
	{
		jptr = &(updateStart[j]);
		if (ur1_getMsgPacket(&(jptr->ntry->descrpt),0/*msg nbr*/, 
						&jmsgPacketptr) != UU_SUCCESS) goto failed; 
		if (ur1_getMsgData(jmsgPacketptr,0/*data nbr*/,&jdataptr,&jdatasz) 
						!= UU_SUCCESS) goto failed; 
		for (k=0; k<eNbr; k++)
		{
			kptr = &(updateEnd[k]);
			if (ur1_getMsgPacket(&(kptr->ntry->descrpt),0/*msg nbr*/, 
							&kmsgPacketptr) != UU_SUCCESS) goto failed; 
			if (ur1_getMsgData(kmsgPacketptr,0/*data nbr*/,&kdataptr,&kdatasz) 
							!= UU_SUCCESS) goto failed; 
			if (UR1_CURNT_MSG(&(jptr->ntry->descrpt)) == 
			 		UR1_CURNT_MSG(&(kptr->ntry->descrpt)))
			{	/* then the messages are the same */	
				if ( /* chk to see if the transformations are the same */
					  um_tfeqtf(jdataptr, kdataptr)
					  /* chk to see that neither are already paired */
					  && ( (jptr->paired == UB_NOTPAIRED) 
			       		&& (kptr->paired == UB_NOTPAIRED)) )
				{
					jptr->paired = k;
					kptr->paired = j;
				}
			}
		}
	}
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ubi_pairTransfs", status);
	return(status);
}	
		
/*********************************************************************
**    I_FUNCTION:int ubi_updateConTuple(conptr,causeKey,tfmat)
**       This function does the actual update of a connector to Unibase.
**    PARAMETERS   
**       INPUT: 
**			conptr		Pointer to the connector entity to be updated.
**			causeKey		Key of the entity that caused the connector association
**							to be updated.
**			tfmat			Transformation to be used in the update.
**       OUTPUT :  
**			conptr		Pointer to the updated entity.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubi_updateConTuple(conptr,causeKey,tfmat)
	struct UB_conector_rec *conptr;
	UU_KEY_ID causeKey;
	UM_transf tfmat;
{
	UR_messagePacket msgPacket;
	struct UM_polyline_rec pline;
	UU_REAL *pt2mvptr;
	int indxInLst;
	UU_LOGICAL firstKey = UU_FALSE, secondKey = UU_FALSE;
	int status = UU_SUCCESS;
	uu_denter((UU_BTRC|UU_R1TRC),(us,
		"ubi_updateConTuple(conptr>key:%d,causeKey:%d,tfmat)",
					conptr->key,causeKey));

	/* construct message packet(s) indicating what is to be done */
	if (ur1_initMsgPacket(&msgPacket, UU_TRANSFORM, 1/*nbr data packs*/) 
		!= UU_SUCCESS) goto failed;
	if (ur1_addData2MsgPacket(&msgPacket,1,tfmat,sizeof(UM_transf)) 
		!= UU_SUCCESS) goto failed;

	pline.key = conptr->pline;
	if (ur_setup_data(UM_POLYLINE_REL, &pline, sizeof(pline)) != 0) goto failed;
	if (uc_retrieve_data(&pline, sizeof(pline)) != UU_SUCCESS) goto failed;

	if (conptr->ainst[0] == causeKey) /* then first instance moved */
		firstKey = UU_TRUE;
	if (conptr->ainst[1] == causeKey) /* then second instance moved */
		secondKey = UU_TRUE;
	if (firstKey)
		if (secondKey) 
		{	/* then both instances moved; so move the connector */
			UR1_AUTO_UPDATE(conptr,&msgPacket,1,UU_NO_REC_CHANGE,
				ub_transf64_connect(conptr, tfmat, UU_TRUE), &status);
			if (status != UU_SUCCESS) goto failed;
				goto failed;
		}
		else /* move only the beginning of the connector */
		{
			pt2mvptr = pline.pt;
			indxInLst = 1;
			um_cctmtf(pt2mvptr, tfmat, pt2mvptr);
			UR1_AUTO_UPDATE(conptr,&msgPacket,1,UU_NO_REC_CHANGE,
				ur_update_data_varlist(pline.key, 1 /* first list */, pt2mvptr,
					indxInLst, 1), &status);
			if (status != UU_SUCCESS) goto failed;
		}
	else if (secondKey) /* move only the end of the connector */
	{
		pt2mvptr = &(pline.pt[3*pline.no_pt-3]);
		indxInLst = pline.no_pt;
		um_cctmtf(pt2mvptr, tfmat, pt2mvptr);
		if (ur_update_data_varlist(pline.key, 1 /* first list */, pt2mvptr, 
					indxInLst, 1) != 0) goto failed;
	}
	else /* the causeKey is not one of the instances */
	{
		uu_uerror1(UB_SYMBOL, 1111, "ubi_updateConTuple");
		/* error: Erroneous entity causing update of connector (%s). */
		goto failed;
	}
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ubi_updateConTuple", status);
	return(status);
}	

/*********************************************************************
**    E_FUNCTION: int ub_msymAutoUpdate(descrptr)
**			This function does the automatic update to master symbol association
**			tuples.  The descriptor indicates what master symbol tuple may need
**			updating and what needs to be done.
**    PARAMETERS   
**       INPUT: 
**				descrptr		Pointer to the auto update descriptor.
**       OUTPUT:  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_msymAutoUpdate(descrptr)
	UR_assocUpdateDescriptor *descrptr;
{
	int status = UU_SUCCESS;
	uu_denter((UU_BTRC|UU_R1TRC),(us,
		"ub_msymAutoUpdate(descrptr>[assocKey:%d,msg:%s])",
		descrptr->assocKey,uu_msg(UR1_CURNT_MSG(descrptr))));

	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ub_msymAutoUpdate", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION: int ub_instAutoUpdate(descrptr)
**			This function does the automatic update to symbol instance association
**			tuples.  The descriptor indicates what symbol instance tuple may need
**			updating and what needs to be done.
**    PARAMETERS   
**       INPUT: 
**				descrptr		Pointer to the auto update descriptor.
**       OUTPUT:  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_instAutoUpdate(descrptr)
	UR_assocUpdateDescriptor *descrptr;
{
	int status = UU_SUCCESS;
	uu_denter((UU_BTRC|UU_R1TRC),(us,
		"ub_instAutoUpdate(descrptr>[assocKey:%d,msg:%s])",
		descrptr->assocKey,uu_msg(UR1_CURNT_MSG(descrptr))));
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ub_instAutoUpdate", status);
	return(status);
}
