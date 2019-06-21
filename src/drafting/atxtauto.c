/*********************************************************************
**    NAME:  atxtauto.c
**       CONTAINS:
**    		ua_txt_autoupdat(descrptr)
**				uai_aso_msgs(descrptr,&tfmat,first,&quit)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       atxtauto.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:40
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "go3.h"
#include "mdrel.h"		/* for relation numbers */
#include "mdattr.h"		/* for UM_UNDISPLAYABLE */
#include "mdcoord.h"		/* for UM_transf */
#include "umessages.h"	/* for auto update messages */
#include "r1emacro.h"	/*for ur1_nxtUpdateSameTuple, ur1_getUpdateSchedEntry */
#include "r1esched.h" 	/* for UR_assocUpdateDescriptor */
#include "atext.h"


#define CHK4ERRS(dataptr, errno, strg)		\
	if (dataptr == UU_NULL)						\
	{													\
		uu_uerror1(UB_SYMBOL, errno, strg);	\
		goto failed;								\
	}

#define TRACE UU_TRUE	/* for debugging only */

/*********************************************************************
**    E_FUNCTION: ua_txt_autoupdate(descrptr) 
**			This function does the automatic update for text that is
**			associated with geometry that has been changed.
**    PARAMETERS   
**       INPUT: 
**				descrptr		Pointer to the associativity descriptor containing
**								the information for updating text.
**       OUTPUT:  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ua_txt_autoupdate(descrptr)
	UR_assocUpdateDescriptor *descrptr;
{
	struct UA_txt_rec	txt;
	struct UA_txtattr_rec	txtattr;
	UM_transf	tfmat;
	UR_messagePacket msgPacket;
	char *dataptr;
	int datasz;
	int	status = UU_SUCCESS;
	UU_LOGICAL	quit = UU_FALSE;
	UU_LOGICAL	um_is_idmat();

	uu_denter((UU_STRC|UU_R1TRC),(us,"ua_txt_autoupdate,key=%d, msg=%s",
						descrptr->assocKey,uu_msg(UR1_CURNT_MSG(descrptr))));

	UA_TXT_ASSO = UU_TRUE;
	txt.key = descrptr->assocKey;
	if (ur_retrieve_data(&txt) != UU_SUCCESS) goto failed;
	txtattr.key = txt.key;
	if (ur_retrieve_attr(&txtattr) != UU_SUCCESS) goto failed;

	um_tftotf(UM_idmat,tfmat);

	status = uai_aso_msgs(&txt,&txtattr,descrptr,tfmat,&quit);

	if ((status==UU_SUCCESS)&&(!quit)&&(!um_is_idmat(tfmat)))
	  {
			/* construct message packet(s) indicating what is to be done */
		if (ur1_initMsgPacket(&msgPacket,UU_TRANSFORM,1/*nbr data packs*/) != UU_SUCCESS) 
			goto failed;
		if (ur1_addData2MsgPacket(&msgPacket,1,tfmat,sizeof(UM_transf)) 
				!= UU_SUCCESS) goto failed;
		UR1_AUTO_UPDATE(&txt,&msgPacket,1,UU_NO_REC_CHANGE, 
				uc_transform(&txt,tfmat,UU_TRUE), &status);
		if (status != UU_SUCCESS) goto failed;
		if (uc_display(&txt) != UU_SUCCESS) goto failed;
	  }

	goto done;
failed: status = UU_FAILURE;
done:;
	UA_TXT_ASSO = UU_FALSE;
	uu_dexitstatus("ua_txt_autoupdat", status);
	return(status);

}	/* ua_txt_autoupdate */



/*********************************************************************
**    I_FUNCTION :  uai_aso_msgs(txt,txtattr,descrptr,tfmat,quit)
**       Process through the message list.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uai_aso_msgs(txt,txtattr,descrptr,tfmat,quit)
struct UA_txt_rec	*txt;
struct UA_txtattr_rec	*txtattr;
UR_assocUpdateDescriptor *descrptr;
UM_transf	tfmat;
UU_LOGICAL	*quit;

{
	UR_messagePacket *msgPacketptr;
	UR_messagePacket msgPacket;
	char *dataptr;
	int datasz;
	int status = UU_SUCCESS;
	UU_LOGICAL First = UU_TRUE;

	uu_denter((UU_STRC|UU_R1TRC),(us,"enter uai_aso_msgs,descrptr=%x",descrptr));

	do
	{
		/* get next (first) message packet from the association descriptor */
		if (UR1_NXT_CURNT_MSG_PAK(descrptr, &msgPacketptr) != UU_SUCCESS)
			goto failed;

		/* get the first data buffer from the message packet; this should be
	 	 * a transformation. */
	 	if (ur1_getMsgData(msgPacketptr,0/*data nbr*/,&dataptr,&datasz)
			!= UU_SUCCESS) goto failed;

#if (TRACE)
		ur1_printTfmat(UU_BTRC,dataptr,"from ubi_chkMsgs4Regen");
#endif

	 	switch (UR1_GET_MSG(msgPacketptr))
		{
			case UU_TRANSLATE:	/* an associated instance was translated */
			case UU_ROTATE:
			case UU_SCALE:
			case UU_TRANSFORM:
				uu_dprint(UU_STRC,(us,"transform message"));
				/* form the transformation matrix */
				um_tftmtf(tfmat,dataptr,tfmat);
				break;

			case UU_MIRROR:
				uu_dprint(UU_STRC,(us,"mirror message"));
/*				for the time being, just delete the text.
				txtattr->path =
				(txtattr->path==(int)UG_TP_LEFT)?(int)UG_TP_RIGHT: (int)UG_TP_LEFT;
				if (ur_update_attr(txtattr) != 0) goto failed;
				um_tftmtf(tfmat,dataptr,tfmat);
*/
			case UU_DELETE:
			case UU_DEFAULT_DELETE:
				uu_dprint(UU_STRC,(us,"delete message"));
				*quit = UU_TRUE; 
				/* construct message packet(s) indicating what is to be done */
				if (ur1_initMsgPacket(&msgPacket, UU_DEFAULT_DELETE, 
					1/*nbr data packs*/) != UU_SUCCESS) goto failed;
				if (ur1_addData2MsgPacket(&msgPacket,1,tfmat,sizeof(UM_transf)) 
					!= UU_SUCCESS) goto failed;

				UR1_AUTO_UPDATE(txt,&msgPacket, 1, UU_REC_CHANGE, 
						ua_delete_text(descrptr->assocKey), &status); 
				if (status != UU_SUCCESS) goto failed;
				/* if message list has DELETE, forget any action given before */
				goto done;

			case UU_REGENERATE: /* there is more than 1 update pending */
				uu_dprint(UU_STRC,(us,"regenerate message"));
				if (!First)			/* can't have the second UU_REGENERATE */
					goto failed;
				break;

			case UU_VIS: /* instance set visible */
				uu_dprint(UU_STRC,(us,"visible message"));
			break;
			case UU_INVIS: /* geometry made invisible; make text invisible */
				uu_dprint(UU_STRC,(us,"invisible message"));
			break;
			case UU_CHANGE_ATTR:
				uu_dprint(UU_STRC,(us,"change attribute message"));
				break;
			case UU_DEFAULT_UPDATE:
				uu_dprint(UU_STRC,(us, "default update message, no update done"));
				break;
			case UU_DEFAULT_UPDT_FIXED:
				uu_dprint(UU_STRC,(us,
					"default update fixed message, no update done"));
				break;
			case UU_DEFAULT_UPDT_DVARL:
				uu_dprint(UU_STRC,(us, 
					"default update varlist message, no update done"));
				break;
			case UU_DEFAULT_UPDT_TRANS:
				uu_dprint(UU_STRC,(us, "default update transf, no update done"));
				break;
			case UU_DEFAULT_UPDT_ATTR:
				uu_dprint(UU_STRC,(us,
					"default update attribute bundle message, no update done"));
				break;
			case UU_DEFAULT_UPDT_DSEG:
				uu_dprint(UU_STRC,(us,
					"default update display segment message, no update done"));
				break;
			case UU_DEFAULT_UPDT_COLOR:
				uu_dprint(UU_STRC,(us,
					"default update color message, no update done"));
				break;
			case UU_DEFAULT_UPDT_LAYER:
				uu_dprint(UU_STRC,(us,
					"default update layer message, no update done"));
				break;
			case UU_DEFAULT_UPDT_PEN:
				uu_dprint(UU_STRC,(us,
					"default update pen message, no update done"));
				break;
			case UU_DEFAULT_UPDT_LNSTY:
				uu_dprint(UU_STRC,(us,
					"default update line style message, no update done"));
				break;
			case UU_DEFAULT_UPDT_LNWGT:
				uu_dprint(UU_STRC,(us,
					"default update line wt. message, no update done"));
				break;
			case UU_DEFAULT_UPDT_LNWID:
				uu_dprint(UU_STRC,(us,
					"default update line width message, no update done"));
				break;
			case UU_DEFAULT_UPDT_DISP:
				uu_dprint(UU_STRC,(us,
					"default update displayable message, no update done"));
				break;
			case UU_DEFAULT_UPDT_SEL:
				uu_dprint(UU_STRC,(us,
					"default update selectable message, no update done"));
				break;
			case UU_DEFAULT_UPDT_BLNKD:
				uu_dprint(UU_STRC,(us,
					"default update blanked message, no update done"));
				break;
			case UU_DEFAULT_UPDT_VKEY:
				uu_dprint(UU_STRC,(us,
					"default update view key message, no update done"));
				break;
			case UU_DEFAULT_DEL_ATTR:
				uu_dprint(UU_STRC,(us,
					"default delete attribute bundle message, no update done"));
				break;
			case UU_DEFAULT_DEL_DVARL:
				uu_dprint(UU_STRC,(us,
					"default delete variable list message, no update done"));
				break;
			case UU_DEFAULT_DEL_TRANS:
				uu_dprint(UU_STRC,(us, 
					"default delete transf message, no update done"));
				break;
			case UU_DEFAULT_DEL_VARLA:
				uu_dprint(UU_STRC,(us,
					"default delete variable list atom(s) message, no update done"));
				break;
			case UU_NOMESSAGE:
				uu_dprint(UU_STRC,(us, "At UU_NOMESSAGE handler, no update done"));
				break;
			default: goto failed;
	 	}
	 	ur1_nxtUpdateSameTuple(descrptr);		/* get the next descriptor */
	 	if (First)
			First = UU_FALSE;
	}
	while (descrptr != UU_NULL);

	goto done;
failed: 
	status = UU_FAILURE;
done:;
	uu_dexitstatus("uai_ass_msgs", status);
	return(status);
}	/* uai_ass_msgs */



