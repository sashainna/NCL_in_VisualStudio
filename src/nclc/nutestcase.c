/*********************************************************************
**    NAME         :  nutestcase.c
**       CONTAINS:
**       nclu_create_test_case()
**       nclu_goto_command()
**       nclu_indirv_command()
**    COPYRIGHT 2015 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nutestcase.c , 25.5
**    DATE AND TIME OF LAST  MODIFICATION
**       01/11/16 , 08:55:39
*********************************************************************/

#include "usysdef.h"
#include "ustdio.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "class.h"
#include "lcom.h"
#include "mdcpln.h"
#include "mdunits.h"
#include "mfort.h"
#include "nclfc.h"
#include "nkeywd.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nclx.h"
#include "nclupok.h"
#include "nclxmdl.h"
#include "nclxmot.h"

extern UU_LOGICAL UR_EOF_stat;
extern UM_transf *REFTM,*MODTM;

static FILE *Sfd;

#define NEWLINE status = ux_fwrite0("\n",1,1,Sfd,&numint);

void nclu_goto_command();
void nclu_indirv_command();

/*********************************************************************
**    E_FUNCTION     :  nclu_create_test_case()
**       Creates an NCL test case based on the current command and
**       settings.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_create_test_case()
{
	int mode,status,hstat,numint,i,nwds,nc,itoler[2],inum,nkeys,pokfl;
	int gougck[3];
	UM_int2 idx,ifl2,knc2[10],ifl307,nwds2,ietype;
	UU_KEY_ID pskey;
	UU_REAL rclw[20],iter[3],rnum,pspl[4];
	char descrip[80],sbuf[80],fstr[10][80];
	char label[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	char *p,*ux_getenv(), ext[UX_SUFFIX_LEN];
	UM_f77_str fbuf[10];
	UX_pathname filename,unifile,fname,fullpp,fulluni;
	NCL_cmdbuf cmdbuf;
	NCLU_pokmod pokmod;
	NCLX_mdl_pntvec pv;
	NCLX_mot_cutter cutter;
	NCLX_mot_feedrate fedrat;
	NCLX_mot_fillet fillet;
	NCLX_mot_maxdp maxdp;
	NCLX_mot_thick thick;
	NCLX_mot_tlaxis tlaxis;
	NCLX_mot_toler toler;
	NCLX_mdl_curve ta_curve;
	NCLX_mdl_data ta_guide;
	NCLX_mdl_struct ta_secps;
/*
.....Get the filename to create
.....Both the .pp and .u files will have the same basename
.....And neither file can pre-exist
*/
	filename[0] = '\0';
again:;
	ud_get_filename("Enter Test Case Filename","Enter Test Case Filename",
		"*.*", filename, &numint, "All Files (*.*)", UU_FALSE, UU_TRUE);
	if (strlen(filename) == 0) goto done;
	ul_default_ftype(UL_program_suffix,filename);
/*
.....Build Unibase filename
*/
	strcpy(unifile,filename);
	ux_add_ftype("UR_PART_SUFFIX",unifile,UX_NPRTERRS);
/*
.....Make sure part program file does not exist
*/
	mode = UX_CREATE | UX_EXISTS;
	status = ux_file_inquire(UU_NULL,UU_NULL,filename,UU_NULL,UU_NULL,&mode,
		&hstat,fullpp,UX_NPRTERRS);
	if (status != UU_SUCCESS)
	{
		ul_short_filename(filename,fname,40);
		sprintf(sbuf,"Bad filename '%s' entered.",fname);
		ud_wrerr(sbuf);
		goto again;
	}

	if (!(mode & UX_NEXISTS))
	{
		ul_short_filename(filename,fname,40);
		sprintf(sbuf,"Part program file '%s' already exists.",fname);
		ud_wrerr(sbuf);
		goto again;
	}
	ul_remove_quotes(fullpp);
/*
.....Make sure Unibase file does not exist
*/
	mode = UX_CREATE | UX_EXISTS;
	status = ux_file_inquire(UU_NULL,UU_NULL,unifile,UU_NULL,UU_NULL,&mode,
		&hstat,fulluni,UX_NPRTERRS);
	if (status != UU_SUCCESS)
	{
		ul_short_filename(unifile,fname,40);
		sprintf(sbuf,"Bad filename '%s' entered.",fname);
		ud_wrerr(sbuf);
		goto again;
	}

	if (!(mode & UX_NEXISTS))
	{
		ul_short_filename(filename,fname,40);
		sprintf(sbuf,"Unibase file '%s' already exists.",fname);
		ud_wrerr(sbuf);
		goto again;
	}
	ul_remove_quotes(fulluni);
/*
.....Open Part program file
*/
	status = ux_fopen0(fullpp,"w",&Sfd);
	if (status != UU_SUCCESS)
	{
		ul_short_filename(fullpp,fname,40);
		sprintf(sbuf,"Could not create '%s'.",fname);
		ud_wrerr(sbuf);
		goto again;
	}
/*
.....Initialize command strings
*/
	for (i=0;i<10;i++) UM_init_f77_str(fbuf[i],fstr[i],80);
/*
.....Create Part program file
........UNITS
*/
	if (UM_cpln.length_unit == UM_MM)
	{
		nc = strlen(NCL_unitsmm);
		status = ux_fwrite0(NCL_unitsmm,nc,1,Sfd,&numint);
	}
	else
	{
		nc = strlen(NCL_unitsin);
		status = ux_fwrite0(NCL_unitsin,nc,1,Sfd,&numint);
	}
	NEWLINE
	if (status != UU_SUCCESS) goto wrterr;
/*
........MULTAX
*/
	nc = strlen(NCL_multax_on);
	status = ux_fwrite0(NCL_multax_on,nc,1,Sfd,&numint);
	NEWLINE
	if (status != UU_SUCCESS) goto wrterr;
/*
........CUTTER
*/
	NclxMotGetCutter(&cutter);
	rclw[0] = cutter.diameter;
	rclw[1] = cutter.radius;
	rclw[2] = cutter.height;
	nwds = 3;
	if (cutter.height == 0.)
	{
		nwds = 2;
		if (cutter.radius == 0.) nwds = 1;
	}
	if (cutter.side_angle != 0 || cutter.zheight != 0 || cutter.flat_angle != 0)
	{
		nwds++;
		rclw[3] = cutter.side_angle;
		if (cutter.zheight != 0 || cutter.flat_angle != 0)
		{
			nwds++;
			rclw[4] = cutter.zheight;
			if (cutter.flat_angle != 0)
			{
				nwds++;
				rclw[5] = cutter.flat_angle;
			}
		}
	}
	ifl2 = 1;
	idx = 307; getifl(&idx,&ifl307);
	setifl(&idx,&ifl2);
	ifl2 = 0;
	ascutr(rclw,&nwds,&ifl2,UM_addr_of_f77_str(fbuf[0]),knc2);
	setifl(&idx,&ifl307);
	nc = knc2[0];
	status = ux_fwrite0(fstr,nc,1,Sfd,&numint);
	NEWLINE
	if (status != UU_SUCCESS) goto wrterr;
/*
........FEDRAT
*/
	NclxMotGetFeedrate(&fedrat);
	fedrat.mode++;
	nclu_fedrat_command(&cmdbuf,&fedrat,UU_NULL,UU_FALSE,UU_TRUE);
	S_output_command(&cmdbuf);
/*
........Motion Calculation
*/
	NclxMotGetMaxdp(&maxdp);
	NclxMotGetNumpts(&nc);
	iter[0] = nc;
	NclxMotGetMaxang(&iter[1]);
	NclxMotGetThick(&thick);
	NclxMotGetToler(&toler);
	idx = 2; getlfl(&idx,&ifl2); itoler[0] = ifl2;
	idx = 363; getlfl(&idx,&ifl2); itoler[1] = ifl2;
	NclxMotGetGougck1(&gougck[0],&gougck[1],&gougck[2]);
	nclu_tool_calc_command(&cmdbuf,&maxdp,iter,&thick,&toler,itoler,gougck,
		UU_NULL,UU_FALSE,UU_TRUE);
	S_output_command(&cmdbuf);
/*
.....Position the tool
*/
	NclxMotGetTool(&pv);
	nclu_goto_command(&cmdbuf,pv.pt,pv.vec);
	S_output_command(&cmdbuf);
/*
.....Set the wireframe contact mode
*/
	NclxMotGetContact(&inum);
	if (inum == 1)
	{
		ncl_init_cmdbuf(&cmdbuf);
		ncl_add_token(&cmdbuf,"NCL_contct_on",NCL_nocomma);
		S_output_command(&cmdbuf);
	}
/*
........ARCSLP
*/
	NclxMotGetFillet(&fillet);
	if (fillet.rad > 0.)
	{
		nclu_arcslp_command(&cmdbuf,&fillet,UU_NULL,UU_FALSE,UU_TRUE);
		S_output_command(&cmdbuf);
	}
/*
.....TLAXIS
*/
	tlaxis.curve = &ta_curve;
	tlaxis.modify.guide = &ta_guide;
	tlaxis.modify.secps = &ta_secps;
	NclxMotGetTlaxis(&tlaxis);
/*
........Save any TLAXIS geometry
*/
	ifl2 = 0; netsky(&ifl2);
	ietype = 0;
	if (tlaxis.mode == NCLX_MOT_TLAXIS_CURVE)
		addsky(&ietype,&tlaxis.curve->header.key,&ifl2,&nwds);
	if (tlaxis.modify.guide_flag)
		addsky(&ietype,&tlaxis.modify.guide->data.header.key,&ifl2,&nwds);
	if (tlaxis.modify.secps_flag)
		addsky(&ietype,&tlaxis.modify.secps->key,&ifl2,&nwds);
/*
.....Set the forward direction
*/
	NclxMotGetFwd(pv.vec);
	nclu_indirv_command(&cmdbuf,pv.vec);
	S_output_command(&cmdbuf);
/*
.....Set the tool condition
*/
	idx = 219; getifl(&idx,&ifl2);
	ncl_init_cmdbuf(&cmdbuf);
	if (ifl2 == -1)
		ncl_add_token(&cmdbuf,NCL_tllft,NCL_nocomma);
	else if (ifl2 == 1)
		ncl_add_token(&cmdbuf,NCL_tlrgt,NCL_nocomma);
	else
		ncl_add_token(&cmdbuf,NCL_tlon,NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
	S_output_command(&cmdbuf);
/*
.....Get the part surface
*/
	NclxMotGetPsis(&pskey,pspl);
	if (pskey != 0)
	{
		ietype = 0; ifl2 = 0;
		addsky(&ietype,&pskey,&ifl2,&nwds);
	}
/*
.....Get the actual command that failed
*/
	testcs(UM_addr_of_f77_str(fbuf[0]),&knc2,&nkeys,&pokfl);
/*
.....Create a Unibase with the referenced geometry
*/
	if (nkeys > 0)
	{
		status = S_create_unibase(fulluni,nkeys);
		if (status != UU_SUCCESS) goto uniberr;
/*
.....Output LOADU command
*/
		ul_break_fname(fulluni,fname,filename);
		ncl_init_cmdbuf(&cmdbuf);
		ncl_add_token(&cmdbuf,"LOADU/",NCL_nocomma);
		ncl_add_token(&cmdbuf,filename,NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
		S_output_command(&cmdbuf);
	}
	ifl2 = 1; netsky(&ifl2);
/*
.....Display the cutter
*/
	ncl_init_cmdbuf(&cmdbuf);
	ncl_add_token(&cmdbuf,"DISPLY/CUTTER",NCL_nocomma);
	ncl_add_cmdbuf(&cmdbuf);
	S_output_command(&cmdbuf);
/*
.....Output the TLAXIS command
*/
	nclu_tlaxis_command(&cmdbuf,&tlaxis,UU_NULL,UU_NULL,UU_FALSE,UU_TRUE);
	S_output_command(&cmdbuf);
/*
.....Set the part surface
*/
	ncl_init_cmdbuf(&cmdbuf);
	ncl_add_token(&cmdbuf,NCL_psis,NCL_nocomma);
	if (pskey != 0)
	{
		ncl_get_label_with_key(pskey,label);
		ncl_add_token(&cmdbuf,label,NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
		S_output_command(&cmdbuf);
	}
	else
	{
		ncl_add_token(&cmdbuf,"(PL/",NCL_nocomma);
		ncl_sprintf(sbuf,pspl,4);
		ncl_add_token(&cmdbuf,sbuf,NCL_nocomma);
		ncl_add_token(&cmdbuf,")",NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
		S_output_command(&cmdbuf);
	}
	NclxMotGetPSCond(&inum);
	if (inum == NCLX_TLONPS)
	{
		ncl_init_cmdbuf(&cmdbuf);
		ncl_add_token(&cmdbuf,NCL_tlonps,NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
		S_output_command(&cmdbuf);
	}
/*
.....Output the pocket modals
*/
	if (pokfl == 1)
	{
		nclu_pokpar(&pokmod);
		nclu_pokmod_cmd(&cmdbuf,&pokmod,UU_TRUE,UU_FALSE);
		S_output_command(&cmdbuf);
	}
/*
.....Output the actual command that failed
*/
	ncl_init_cmdbuf(&cmdbuf);
	for (i=0;i<10;i++)
	{
		if (knc2[i] > 0)
		{
			fstr[i][knc2[i]] = '\0';
			ncl_add_token(&cmdbuf,fstr[i],NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);
		}
		else
			break;
	}
	S_output_command(&cmdbuf);
/*
.....Close the Part program file
*/
	ux_fclose0(Sfd);
	goto done;
/*
.....Error writing to Part program file
*/
wrterr:
	ul_short_filename(fullpp,fname,40);
	sprintf(sbuf,"Error writing to '%s'.",fname);
	ud_wrerr(sbuf);
	ux_fclose0(Sfd);
	goto done;
/*
.....Error writing to Unibase file
*/
uniberr:
	ul_short_filename(fulluni,fname,40);
	sprintf(sbuf,"Error creating Unibase file '%s'.",fname);
	ud_wrerr(sbuf);
	ux_fclose0(Sfd);
	goto done;
/*
.....End of routine
*/
done:
	return;
}

/*********************************************************************
**    E_FUNCTION     :  nclu_goto_command(cmdbuf,pt,vec)
**       Outputs a GOTO/x,y,z,i,j,k command.
**    PARAMETERS   
**       INPUT  : 
**          pt       = XYZ of point.
**          vec      = IJK of point.
**       OUTPUT :  
**          cmdbuf   = Command buffer to receive the GOTO command.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_goto_command(cmdbuf,pt,vec)
NCL_cmdbuf *cmdbuf;
UU_REAL *pt,*vec;
{
	char sbuf[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	ncl_init_cmdbuf(cmdbuf);
	ncl_add_token(cmdbuf,NCL_goto,NCL_nocomma);
	ncl_sprintf(sbuf,pt,3);
	ncl_add_token(cmdbuf,sbuf,NCL_comma);
	ncl_sprintf(sbuf,vec,3);
	ncl_add_token(cmdbuf,sbuf,NCL_nocomma);
	ncl_add_cmdbuf(cmdbuf);
}

/*********************************************************************
**    E_FUNCTION     :  nclu_indirv_command(cmdbuf,vec)
**       Outputs a INDIRV/x,y,z command.
**    PARAMETERS   
**       INPUT  : 
**          vec      = IJK of forward vector.
**       OUTPUT :  
**          cmdbuf   = Command buffer to receive the GOTO command.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_indirv_command(cmdbuf,vec)
NCL_cmdbuf *cmdbuf;
UU_REAL *vec;
{
	char sbuf[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	ncl_init_cmdbuf(cmdbuf);
	if (um_mag(vec) != 0.)
	{
		ncl_add_token(cmdbuf,NCL_indirv,NCL_nocomma);
		ncl_sprintf(sbuf,vec,3);
		ncl_add_token(cmdbuf,sbuf,NCL_nocomma);
		ncl_add_cmdbuf(cmdbuf);
	}
}

/*********************************************************************
**    I_FUNCTION     : S_output_command(cmdbuf)
**       Outputs the commands stored in the command buffer to the test
**       case file.
**    PARAMETERS
**       INPUT  :
**          cmdbuf   = Command buffer to output.
**       OUTPUT :
**          none
**    RETURNS      : UU_TRUE if the settings have changed.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_output_command(cmdbuf)
NCL_cmdbuf *cmdbuf;
{
	int i,status,nc,numint;
	status = UU_SUCCESS;
	for (i=0;i<cmdbuf->num_cmd;i++)
	{
		nc = strlen(cmdbuf->cmd[i]);
		if (nc > 0)
		{
			status = ux_fwrite0(cmdbuf->cmd[i],nc,1,Sfd,&numint);
			NEWLINE
			if (status != UU_SUCCESS) break;
		}
	}
	return(status);
}

/*********************************************************************
**    I_FUNCTION     : S_create_unibase(fname,nkeys)
**       Creates a Unibase that contains all geometry referenced in the
**       current command.
**    PARAMETERS
**       INPUT  :
**          fnameu   = Name of Unibase to create.
**          nkeys    = Number of geometry keys stored in 'skylst'.
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS if Unibase was created without error,
**                   UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_create_unibase(fnameu,nkeys)
char *fnameu;
int nkeys;
{
	int itype,i,status;
	UM_int2 ierr;
	UM_transf *reftm,*modtm;
	struct UC_entitydatabag ent;
/*
.....Open the secondary Unibase
*/
	status = ur_op04_session(fnameu,&itype,UU_TRUE);
	if (status != UX_NFOUND) goto done;
	ierr = 2; ubact(&ierr);
	f77_resu();
	ur_getu_work();
	ncl_ublist_init();
	ncl_ubkey_init();
/*'
.....Cancel REFSYS & MODSYS
*/
	reftm = REFTM; REFTM = UU_NULL;
	modtm = MODTM; MODTM = UU_NULL;
/*
.....Put referenced geometry in secondary Unibase
*/
	UR_EOF_stat = UU_TRUE;
	for (i=0;i<nkeys;i++)
	{
		ncl_get_listkey(0,i,&ent.key);
		status = ncl_retrieve_data_fixed(&ent);
		if (status != UU_SUCCESS) goto done;
		status = ncl_ubput_ent(&ent);
		if (status != UU_SUCCESS) goto done;
	}
/*
.....Free the key list and
.....close the Unibase
*/
done:;
	delsky();
	ierr = 2; ubact(&ierr);
	ur_cl04(&ierr);
	if (ierr != 0) status = UU_FAILURE;
	ierr = 1; ubact(&ierr);
	ncl_ublist_free();
	ncl_ubkey_free();
/*
.....Reset REFSYS & MODSYS matrices
*/
	REFTM = reftm;
	MODTM = modtm;
	return(status);
}
