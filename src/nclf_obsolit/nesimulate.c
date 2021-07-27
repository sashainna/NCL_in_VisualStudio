/*********************************************************************
**    NAME         :  nesimulate.c
**       CONTAINS:
**			ncl_simulate_attrib
**			ncl_simulate_circle
**			ncl_simulate_cutter
**			ncl_simulate_header
**			ncl_simulate_hedmot
**			ncl_simulate_isn
**			ncl_simulate_load
**			ncl_simulate_machine
**			ncl_simulate_motion
**			ncl_simulate_parse
**			ncl_simulate_profil
**			ncl_simulate_read
**			ncl_simulate_status
**			ncl_simulate_text
**			ncl_simulate_stock
**       ncl_simulate_stock_params
**    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nesimulate.c , 26.3
**    DATE AND TIME OF LAST  MODIFICATION
**       04/16/19 , 15:08:36
*********************************************************************/

#include <stdio.h>
#include "usysdef.h"
#include "nsimulate.h"
#include "lipvmach.h"
#include "mfort.h"
#include "nclfc.h"
#include "xfsys1.h"

static int Mtype=1;
static UM_coord Sspt;

#define MAXSIMREC 64536
#if UU_COMP == UU_WIN2K
#define is1 3
#define is4 0
#else
#define is1 0
#define is4 3
#endif

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_attrib(cdat,cstat)
**			This routine parses a Machine Simulation ATTRIB record.
**    PARAMETERS   
**       INPUT  : 
**			cdat    = Machine simulation record to parse.
**       OUTPUT :  
**			cstat   = Simulation STATUS structure.
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**					 otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_simulate_attrib(cdat,cstat)
char *cdat;
UN_sim_status *cstat;
{
	int nc,irtn;
	char ldat[MAXSIMREC],lparm[80];
/*
.....Initialize routine
*/
	irtn = UU_SUCCESS;
	strcpy(ldat,cdat);
/*
.....Get Attribute parameters
*/
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cstat->isn);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_reals(&cstat->seq,&nc,1,lparm);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cstat->stop);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_reals(&cstat->rpm,&nc,1,lparm);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cstat->coolant);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cstat->cutcom);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_reals(&cstat->tlno,&nc,1,lparm);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_reals(&cstat->tlength,&nc,1,lparm);
   irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cstat->mode);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	if (strcmp(lparm,"#EOL#") == 0)
	{
		cstat->offsets[0] = cstat->offsets[1] = 0;
		cstat->offsets[2] = cstat->offsets[3] = 0;
	}
	else
	{
		irtn = ul_to_number(lparm,&cstat->offsets[0]);
		irtn = ncl_simulate_parse(ldat,1,lparm);
		irtn = ul_to_number(lparm,&cstat->offsets[1]);
		irtn = ncl_simulate_parse(ldat,1,lparm);
		irtn = ul_to_number(lparm,&cstat->offsets[2]);
		irtn = ncl_simulate_parse(ldat,1,lparm);
		irtn = ul_to_number(lparm,&cstat->offsets[3]);
	}
/*
.....End of routine
*/
done:;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_circle(cdat,ccut)
**			This routine parses a Machine Simulation CUTTER record.
**    PARAMETERS   
**       INPUT  : 
**			cdat    = Machine simulation record to parse.
**       OUTPUT :  
**			ccut    = Simulation CUTTER structure.
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**					 otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_simulate_circle(cdat,ccir)
char *cdat;
UN_sim_circle *ccir;
{
	int nc,irtn;
	char ldat[MAXSIMREC],lparm[80];
/*
.....Initialize routine
*/
	irtn = UU_SUCCESS;
	strcpy(ldat,cdat);
/*
.....Get Circle parameters
*/
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&ccir->isn);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_reals(&ccir->seq,&nc,1,lparm);
	irtn = ncl_simulate_parse(ldat,7,lparm);
	irtn = ul_to_reals(ccir->parms,&nc,7,lparm);
/*
.....End of routine
*/
done:;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_cutter(cdat,ccut)
**			This routine parses a Machine Simulation CUTTER record.
**    PARAMETERS   
**       INPUT  : 
**			cdat    = Machine simulation record to parse.
**       OUTPUT :  
**			ccut    = Simulation CUTTER structure.
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**					 otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_simulate_cutter(cdat,ccut)
char *cdat;
UN_sim_cutter *ccut;
{
	int nc,irtn;
	char ldat[MAXSIMREC],lparm[80];
/*
.....Initialize routine
*/
	irtn = UU_SUCCESS;
	strcpy(ldat,cdat);
/*
.....Get Cutter parameters
*/
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&ccut->isn);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_reals(&ccut->seq,&nc,1,lparm);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&ccut->ncut);
	if (ccut->ncut != 0)
	{
		irtn = ncl_simulate_parse(ldat,ccut->ncut,lparm);
		irtn = ul_to_reals(ccut->cutter,&nc,ccut->ncut,lparm);
	}
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&ccut->ncutd);
	if (ccut->ncutd != 0)
	{
		irtn = ncl_simulate_parse(ldat,ccut->ncutd,lparm);
		irtn = ul_to_reals(ccut->cutterd,&nc,ccut->ncutd,lparm);
	}
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&ccut->cutfl[0]);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&ccut->cutfl[1]);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&ccut->cutfl[2]);
	irtn = ncl_simulate_parse(ldat,1,ccut->cutsym[0]);
	irtn = ncl_simulate_parse(ldat,1,ccut->cutsym[1]);
	irtn = ncl_simulate_parse(ldat,1,ccut->cutsym[2]);
	irtn = ncl_simulate_parse(ldat,12,lparm);
	irtn = ul_to_reals(ccut->symofs,&nc,12,lparm);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&ccut->shkfl);
/*
.....End of routine
*/
done:;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_header(cdat,chead)
**			This routine parses a Machine Simulation HEADER record.
**    PARAMETERS   
**       INPUT  : 
**			cdat    = Machine simulation record to parse.
**       OUTPUT :  
**			chead   = Simulation HEADER structure.
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**					 otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_simulate_header(cdat,chead)
char *cdat;
UN_sim_header *chead;
{
	int irtn;
	char ldat[MAXSIMREC],lparm[80];
/*
.....Initialize routine
*/
	irtn = 0;
	strcpy(ldat,cdat);
/*
.....Get Header parameters
*/
	irtn = ncl_simulate_parse(ldat,1,chead->post);
	irtn = ncl_simulate_parse(ldat,1,chead->prev);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&chead->mach);
	if (irtn != UU_SUCCESS)
	{
		strcpy(chead->post,lparm);
		chead->mach = 0;
	}
	irtn = ncl_simulate_parse(ldat,1,chead->date);
	irtn = ncl_simulate_parse(ldat,1,chead->time);
	irtn = ncl_simulate_parse(ldat,1,chead->clfile);
	irtn = ncl_simulate_parse(ldat,1,chead->cldate);
	irtn = ncl_simulate_parse(ldat,1,chead->cltime);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&chead->units);
/*
.....End of routine
*/
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_isn(cdat,cstat)
**			This routine parses a Machine Simulation ISN record.
**    PARAMETERS   
**       INPUT  : 
**			cdat    = Machine simulation record to parse.
**       OUTPUT :  
**			kisn    = ISN of simulation record.
**			knparm  = # of values in 'kdata', not including kdata[0] which
**                 contains the same value as 'knparm'.
**			kdata   = Clfile compatible ISN record.
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**					 otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_simulate_isn(cdat,kisn,knparm,kdata)
char *cdat;
int *kisn,*knparm,*kdata;
{
	int i,nc,irtn;
	char ldat[MAXSIMREC],lparm[80];
/*
.....Initialize routine
*/
	irtn = UU_SUCCESS;
	strcpy(ldat,cdat);
/*
.....Get record ISN
*/
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,kisn);
/*
.....Get Attribute parameters
*/
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&kdata[0]);
	*knparm = kdata[0];
	for (i=1;i<=kdata[0];i++)
	{
		irtn = ncl_simulate_parse(ldat,1,lparm);
		irtn = ul_to_number(lparm,&kdata[i]);
	}
/*
.....End of routine
*/
done:;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_load(cfil,mach)
**			This routine loads a Machine Simulation file as an external
**			clfile.
**    PARAMETERS   
**       INPUT  : 
**			   cfil    = Name of Machine Simulation file to load.
**       OUTPUT :  
**			   mach    = Machine definition structure.
**    RETURNS      : Returns UU_FAILURE on load failure, UU_SUCCESS
**					 otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_simulate_load(cfil,mach)
char *cfil;
UN_sim_machine *mach;
{
	int irtn,imot[4],i,j,n,knc,kerr,ktype,nc,inc;
	UN_clstruc *iclrec;
	UM_int4 *jdatp,*jp;
	char *pc,cdat[MAXSIMREC],ldat[MAXSIMREC],cmsg[80],lbuf[MAXSIMREC];
	UM_int2 iclf,*pi;
	UM_int4 iclw[6],ciclw[6];
	UU_LOGICAL ihed,iscirc;
	UU_REAL rdata[420],crdata[20],fsav,fabs(),rnum[10],*rp;
	FILE *fptr;
	UN_sim_header header;
	UN_sim_status stat;
	UN_sim_motion motion;
	UN_sim_text ctext;
	UN_sim_stock stock;
	UN_sim_cutter cutter;
	UN_sim_profil profil;
	UN_sim_circle cir;
/*
.....Initialize routine
*/
	irtn = UU_SUCCESS;
	imot[0] = 3; imot[1] = 5; imot[2] = 7; imot[3] = 8;
	fsav = 0.;
	ihed = UU_FALSE;
	iscirc = UU_FALSE;
	Sspt[0] = Sspt[1] = Sspt[2] = 0.;
	uu_list_init(&stock.idlist,sizeof(int),0,20);
	uu_list_init(&stock.ptlist,sizeof(UU_REAL),0,20*3);
/*
.....Open file
*/
	fptr = UU_NULL;
	irtn = ux_fopen0(cfil,"r",&fptr);
	if (irtn != UU_SUCCESS) goto done;
/*
.....Initialize secondary clfile
*/
	iclf = 1;
	clopen(&iclf,&iclrec);
/*
.....Read next record
*/
	do
	{
		ncl_simulate_read(fptr,&ktype,cdat,&knc,cmsg,&kerr);
		if (kerr != 0) goto failed;
		switch (ktype)
		{
/*
.....HEADER record
*/
		case SIM_HEADER:
			irtn = ncl_simulate_header(cdat,&header);
			if (irtn != UU_SUCCESS) goto failed;
			ihed = UU_TRUE;
/*
........Fill in character strings
*/
			for (i=strlen(header.post);i<8;i++) header.post[i] = ' ';
			header.post[8] = '\0';
			for (i=strlen(header.prev);i<11;i++) header.prev[i] = ' ';
			header.prev[11] = '\0';
			for (i=strlen(header.date);i<11;i++) header.date[i] = ' ';
			header.date[11] = '\0';
			for (i=strlen(header.time);i<8;i++) header.time[i] = ' ';
			header.time[8] = '\0';
			for (i=strlen(header.clfile);i<80;i++) header.clfile[i] = ' ';
			header.clfile[80] = '\0';
			for (i=strlen(header.cldate);i<11;i++) header.cldate[i] = ' ';
			header.cldate[11] = '\0';
			for (i=strlen(header.cltime);i<8;i++) header.cltime[i] = ' ';
			header.cltime[8] = '\0';
/*
........Store clfile header
*/
			iclw[0] = 0; iclw[1] = 0; iclw[2] = 7400; iclw[3] = 0; iclw[4] = 16;
			pc = (char *)&rdata[0];
			strcpy(pc,header.clfile);
			pc = (char *)&rdata[10];
			strcpy(pc,header.date);
			pc = (char *)&rdata[12];
			strcpy(pc,header.time);
			pc = (char *)&rdata[13];
			strcpy(pc,header.post);
			pc = (char *)&rdata[14];
			strcpy(pc,header.prev);
			clstor(&iclf,&iclrec,iclw,rdata);
/*
........Store MACHIN card
*/
			iclw[0] = 0; iclw[1] = 0; iclw[2] = 2000; iclw[3] = 1015; iclw[4] = 7;
			pc = (char *)&rdata[0];
			strcpy(pc,header.post);
			rdata[1] = header.mach;
			pi = (UM_int2 *)&rdata[2];
			pi[is1] = 0;
			pi[is4] = 144;	/* OPTION */
			rdata[3] = 1; rdata[4] = header.units;
			rdata[5] = 2; rdata[6] = header.units;
			clstor(&iclf,&iclrec,iclw,rdata);
/*
........Store UNITS record
*/
			iclw[0] = 0; iclw[1] = 0; iclw[2] = 7300; iclw[3] = header.units; iclw[4] = 0;
			clstor(&iclf,&iclrec,iclw,rdata);
/*
........Store MULTAX record
*/
			iclw[0] = 0; iclw[1] = 0; iclw[2] = 9000; iclw[3] = 0; iclw[4] = 0;
			clstor(&iclf,&iclrec,iclw,rdata);
			break;
/*
.....MACHINE record
*/
		case SIM_MACHINE:
			irtn = ncl_simulate_machine(cdat,mach);
			if (irtn != UU_SUCCESS) goto failed;
/*
........Store Type of machine
*/
			Mtype = mach->type;
			break;
/*
.....PARTNO record
*/
		case SIM_PARTNO:
			irtn = ncl_simulate_text(cdat,&ctext);
			if (irtn != UU_SUCCESS) goto failed;
			for (i=strlen(ctext.str);i<66;i++) ctext.str[i] = ' ';
			ctext.str[66] = '\0';
			iclw[0] = ctext.isn; iclw[1] = 0; iclw[2] = 2000; iclw[3] = 1045;
			iclw[4] = 9;
			pc = (char *)&rdata[0];
			strcpy(pc,ctext.str);
			clstor(&iclf,&iclrec,iclw,rdata);
			break;
/*
.....ISN record
*/
		case SIM_ISN:
			irtn = ncl_simulate_isn(cdat,&iclw[0],&nc,(int *)rdata);
			if (irtn != UU_SUCCESS) goto failed;
			iclw[1] = 0; iclw[2] = 1000; iclw[3] = 0;
			iclw[4] = (nc+3) / 2;
			clstor(&iclf,&iclrec,iclw,rdata);
			break;
/*
.....ERROR & PPRINT records
*/
		case SIM_ERROR:
		case SIM_PPRINT:
			irtn = ncl_simulate_text(cdat,&ctext);
			if (irtn != UU_SUCCESS) goto failed;
			for (i=strlen(ctext.str);i<66;i++) ctext.str[i] = ' ';
			ctext.str[66] = '\0';
			if (ktype == SIM_PPRINT)
			{
				iclw[2] = 2000; iclw[3] = 1044;
			}
			else
			{
				iclw[2] = 8000; iclw[3] = 0;
			}
			iclw[0] = ctext.isn; iclw[1] = 0; iclw[4] = 9;
			pc = (char *)&rdata[0];
			strcpy(pc,ctext.str);
			clstor(&iclf,&iclrec,iclw,rdata);
			break;
/*
.....STATUS record
*/
		case SIM_STATUS:
		case SIM_ATTRIB:
			if (ktype == SIM_STATUS)
				irtn = ncl_simulate_status(cdat,&stat);
			else
				irtn = ncl_simulate_attrib(cdat,&stat);
			if (irtn != UU_SUCCESS) goto failed;
/*
........Store STOP statement
*/
			switch (stat.stop)
			{
			case 1:
				iclw[0] = stat.isn; iclw[1] = 0; iclw[2] = 2000; iclw[3] = 2; iclw[4] = 0;
				clstor(&iclf,&iclrec,iclw,rdata);
				break;
/*
........Store OPSTOP statement
*/
			case 2:
				iclw[0] = stat.isn; iclw[1] = 0; iclw[2] = 2000; iclw[3] = 3; iclw[4] = 0;
				clstor(&iclf,&iclrec,iclw,rdata);
				break;
/*
........Store LOADTL statement
*/
			case 3:
				iclw[0] = stat.isn; iclw[1] = 0; iclw[2] = 2000; iclw[3] = 1055; iclw[4] = 3;
				rdata[0] = stat.tlno;
				pi = (UM_int2 *)&rdata[1];
				pi[is1] = 0;
				pi[is4] = 9;
				rdata[2] = stat.tlength;
				clstor(&iclf,&iclrec,iclw,rdata);
				break;
			}
/*
........Store CUTTER statement
*/
			if (ktype == SIM_STATUS)
			{
				if (stat.ncut > 0)
				{
					iclw[0] = stat.isn; iclw[1] = 0; iclw[2] = 6000; iclw[3] = 0;
					iclw[4] = stat.ncut;
					for (i=0;i<stat.ncut;i++) rdata[i] = stat.cutter[i];
					clstor(&iclf,&iclrec,iclw,rdata);
				}
/*
........Store CUTTER/DISPLY statement
*/
				if (stat.ncutd > 0)
				{
					iclw[0] = stat.isn; iclw[1] = 0; iclw[2] = 7100; iclw[3] = 1;
					iclw[4] = stat.ncutd;
					for (i=0;i<stat.ncutd;i++) rdata[i] = stat.cutterd[i];
					clstor(&iclf,&iclrec,iclw,rdata);
				}
/*
........Store CUTTER symbol
*/
				if (strlen(stat.cutsym) != 0)
				{
					iclw[0] = stat.isn; iclw[1] = 0; iclw[2] = 7100; iclw[3] = 2;
					iclw[4] = 8;
					for (i=strlen(stat.cutsym);i<20;i++) stat.cutsym[i] = ' ';
					stat.cutsym[20] = '\0';
		    		pc = (char *)&rdata[0];
					strcpy(pc,stat.cutsym);
					pi = (UM_int2 *)&rdata[3];
					pi[0] = 0;
					pi[3] = stat.cutfl;
					for (i=0;i<4;i++) rdata[i+4] = stat.symofs[i];
					clstor(&iclf,&iclrec,iclw,rdata);
				}
/*
........Store CUTTER shank
*/
				if (stat.nshank != 0)
				{
					iclw[0] = stat.isn; iclw[1] = 0; iclw[2] = 7100; iclw[3] = 4;
					iclw[4] = stat.nshank;
					for (i=0;i<stat.nshank;i++) rdata[i] = stat.shank[i];
					clstor(&iclf,&iclrec,iclw,rdata);
				}
			}
/*
........Store COOLNT statement
*/
			iclw[0] = stat.isn; iclw[1] = 0; iclw[2] = 2000; iclw[3] = 1030;
			iclw[4] = 1;
			pi = (UM_int2 *)&rdata[0];
			pi[is1] = 0;
			pi[is4] = 72;
			if (stat.coolant == 1) pi[is4] = 71;
			clstor(&iclf,&iclrec,iclw,rdata);
/*
........Store SPINDL statement
*/
			iclw[0] = stat.isn; iclw[1] = 0; iclw[2] = 2000; iclw[3] = 1031;
			iclw[4] = 1;
			if (stat.rpm == 0)
			{
				pi = (UM_int2 *)&rdata[0];
				pi[is1] = 0;
				pi[is4] = 72;
			}
			else
			{
				rdata[0] = stat.rpm;
			}
			clstor(&iclf,&iclrec,iclw,rdata);
/*
........Store CUTCOM statement
*/
			iclw[0] = stat.isn; iclw[1] = 0; iclw[2] = 2000; iclw[3] = 1007;
			iclw[4] = 1;
			pi = (UM_int2 *)&rdata[0];
			pi[is1] = 0;
			pi[is4] = 72;
			if (stat.cutcom == 1) pi[is4] = 8;	/* LEFT */
			else if (stat.cutcom == 2) pi[is4] = 24;	/* RIGHT */
			clstor(&iclf,&iclrec,iclw,rdata);
/*
........Store MODE statement
*/
			iclw[0] = stat.isn; iclw[1] = 0; iclw[2] = 2000; iclw[3] = 1003;
			iclw[4] = 1;
			pi = (UM_int2 *)&rdata[0];
			pi[is1] = 0;
			pi[is4] = 151; /* MILL */
			if (stat.mode == 2) pi[is4] = 700;	/* LATHE */
			clstor(&iclf,&iclrec,iclw,rdata);
/*
........Store CUTCOM/ADJUST & TOOLNO/ADJUST statements
*/
			iclw[0] = stat.isn; iclw[1] = 0; iclw[2] = 2000; iclw[3] = 1007;
			pi = (UM_int2 *)&rdata[0];
			pi[is1] = 0;
			pi[is4] = 159;	/* ADJUST */
			inc = 0;
			for (i=0;i<2;i++)
			{
				iclw[4] = 2;
				if (stat.offsets[inc] == 0)
				{
					pi = (UM_int2 *)&rdata[1];
					pi[is1] = 0;
					pi[is4] = 72;	/* OFF */
				}
				else
				{
					rdata[1] = stat.offsets[inc+1];
					if (stat.offsets[inc] == 2)
					{
						pi = (UM_int2 *)&rdata[2];
						pi[is1] = 0;
						pi[is4] = 10;	/* MINUS */
						iclw[4]++;
					}
				}
				clstor(&iclf,&iclrec,iclw,rdata);
				iclw[3] = 1025;
				inc += 2;
			}
			break;
/*
.....PROFIL record
*/
		case SIM_PROFIL:
			irtn = ncl_simulate_profil(cdat,&profil);
			if (irtn != UU_SUCCESS) goto failed;
/*
........Store PROFIL statement
*/
			iclw[0] = profil.isn; iclw[1] = 0; iclw[2] = 7120;
			iclw[3] = profil.subt; iclw[4] = profil.mxcl + 4;
    		pc = (char *)&rdata[1];
			strcpy(pc,profil.symbol);
			jp = (UM_int4 *)&rdata[0];
			jp[1] = profil.npts;
			pi = (UM_int2 *)&rdata[0];
			nc = strlen(profil.symbol);
			pi[0] = nc;
			pi[1] = profil.type;
			inc = 1 + (nc+7) / 8;
			for (i=0;i<profil.mxcl;i++) rdata[inc+i] = profil.pts[i];
			clstor(&iclf,&iclrec,iclw,rdata);
			uu_free(profil.pts);
			break;
/*
.....CUTTER record
*/
		case SIM_CUTTER:
			irtn = ncl_simulate_cutter(cdat,&cutter);
			if (irtn != UU_SUCCESS) goto failed;
/*
........Store CUTTER statement
*/
			if (cutter.ncut > 0)
			{
				iclw[0] = cutter.isn; iclw[1] = 0; iclw[2] = 6000; iclw[3] = 0;
				iclw[4] = cutter.ncut;
				for (i=0;i<cutter.ncut;i++) rdata[i] = cutter.cutter[i];
				clstor(&iclf,&iclrec,iclw,rdata);
			}
/*
........Store CUTTER/DISPLY statement
*/
			if (cutter.ncutd > 0)
			{
				iclw[0] = cutter.isn; iclw[1] = 0; iclw[2] = 7100; iclw[3] = 1;
				iclw[4] = cutter.ncutd;
				for (i=0;i<cutter.ncutd;i++) rdata[i] = cutter.cutterd[i];
				clstor(&iclf,&iclrec,iclw,rdata);
			}
/*
........Store CUTTER symbol
*/
			if (cutter.cutfl[0] >= 2)
			{
				iclw[0] = cutter.isn; iclw[1] = 0; iclw[2] = 7100; iclw[3] = 7;
    			pc = (char *)&rdata[3];
				strcpy(pc,cutter.cutsym[0]);
				nc = strlen(cutter.cutsym[0]);
				pi = (UM_int2 *)&rdata[0];
				pi[0] = nc;
				pi[1] = cutter.cutfl[0];
				rdata[1] = cutter.symofs[0][0];
				rdata[2] = cutter.symofs[0][1];
				iclw[4] = 3 + (nc+7) / 8;
				clstor(&iclf,&iclrec,iclw,rdata);
			}
/*
........Store SHANK and HOLDER
*/
			for (j=1;j<3;j++)
			{
				if (cutter.cutfl[j] != 0)
				{
					iclw[0] = cutter.isn; iclw[1] = 0; iclw[2] = 7100; iclw[3] = 8;
					nc = strlen(cutter.cutsym[j]);
    				pc = (char *)&rdata[5];
					strcpy(pc,cutter.cutsym[j]);
					pi = (UM_int2 *)&rdata[0];
					pi[0] = nc;
					pi[1] = cutter.cutfl[j];
					if (j == 1) pi[2] = cutter.shkfl;
					else pi[2] = 2;
					for (i=0;i<4;i++) rdata[i+1] = cutter.symofs[j][i];
					iclw[4] = 5 + (nc+7) / 8;
					clstor(&iclf,&iclrec,iclw,rdata);
				}
			}
			break;
/*
.....CIRCLE record
*/
		case SIM_CIRCLE:
			irtn = ncl_simulate_circle(cdat,&cir);
			if (irtn != UU_SUCCESS) goto failed;
/*
........Store circle statement
*/
			ciclw[0] = cir.isn; ciclw[1] = 0; ciclw[2] = 3000; ciclw[3] = 5;
			ciclw[4] = 12;
			for (i=0;i<5;i++) crdata[i] = 0.;
			for (i=0;i<7;i++) crdata[i+5] = cir.parms[i];
			iscirc = UU_TRUE;
			break;
/*
.....MOTION record
*/
		case SIM_MOTION:
		case SIM_HEDMOT:
			if (ktype == SIM_MOTION)
				irtn = ncl_simulate_motion(cdat,&motion);
			else
				irtn = ncl_simulate_hedmot(cdat,&motion);
			if (irtn != UU_SUCCESS) goto failed;
			if (motion.type == 5) break;
/*
........Output RAPID statement
*/
			if (motion.feed == 0.)
			{
				iclw[0] = motion.isn; iclw[1] = 0; iclw[2] = 2000;
				iclw[3] = 5; iclw[4] = 0;
				clstor(&iclf,&iclrec,iclw,rdata);
			}
/*
........Output FEDRAT statement
*/
			else if (fabs(motion.feed-fsav) > .1)
			{
				iclw[0] = motion.isn; iclw[1] = 0; iclw[2] = 2000;
				iclw[3] = 1009; iclw[4] = 1;
				rdata[0] = motion.feed;
				clstor(&iclf,&iclrec,iclw,rdata);
			}
			fsav = motion.feed;
/*
........Output circle record if set
*/
			if (iscirc)
			{
				clstor(&iclf,&iclrec,ciclw,crdata);
				iscirc = UU_FALSE;
			}
/*
........Output motion record
........Simulation style records are
........always used now
........Bobby - 02/09/04
*/
			iclw[0] = motion.isn; iclw[1] = 0; iclw[2] = 5001;
			iclw[3] = imot[motion.type-1]; iclw[4] = 21;
			pi = (UM_int2 *)&rdata[0];
			pi[0] = motion.seq;
			pi[1] = 2 - motion.skip;
			pi[2] = motion.head;
			rdata[1] = motion.time;
			for (i=0;i<6;i++) rdata[i+2] = motion.tend[i];
			if (motion.head == 0)
			{
				iclw[2] = 5001;
				iclw[4] = 21;
				for (i=0;i<10;i++) rdata[i+8] = motion.axis[i];
				for (i=0;i<3;i++) rdata[i+18] = motion.vfwd[i];
			}
			else
			{
				iclw[2] = 5002;
				iclw[4] = 21;
				for (i=0;i<13;i++) rdata[i+8] = motion.axis[i];
			}
			clstor(&iclf,&iclrec,iclw,rdata);
			break;
/*
.....STOCK/FIXTUR record
*/
		case SIM_STOCK:
		case SIM_FIXTUR:
			irtn = ncl_simulate_stock(cdat,ldat,&iclw[0],&iclw[3]);
			if (irtn != UU_SUCCESS) goto failed;
			iclw[1] = 0;
			if (ktype == SIM_STOCK)
				iclw[2] = 2600;
			else
				iclw[2] = 2601;
			jdatp = (UM_int4 *)rdata;
			switch (iclw[3])
			{
			case SIM_STOCK_BOX:
				irtn = ncl_simulate_stock_box(ldat,&stock);
				if (irtn != UU_SUCCESS) goto failed;
				iclw[4] = 7;
				jdatp[0] = stock.data.box.subt;
				jdatp[1] = stock.data.box.id;
				for (i=0;i<3;i++) rdata[i+1] = stock.data.box.pt1[i];
				for (i=0;i<3;i++) rdata[i+4] = stock.data.box.pt2[i];
				break;
			case SIM_STOCK_CONE:
				irtn = ncl_simulate_stock_params(ldat,&stock,9);
				if (irtn != UU_SUCCESS) goto failed;
				iclw[4] = 10;
				jdatp[0] = stock.data.pstk.subt;
				jdatp[1] = stock.data.pstk.id;
				for (i=0;i<9;i++) rdata[i+1] = stock.data.pstk.params[i];
				break;
			case SIM_STOCK_CYL:
				irtn = ncl_simulate_stock_cyl(ldat,&stock);
				if (irtn != UU_SUCCESS) goto failed;
				iclw[4] = 9;
				jdatp[0] = stock.data.cyl.subt;
				jdatp[1] = stock.data.cyl.id;
				for (i=0;i<7;i++) rdata[i+1] = stock.data.cyl.cir[i];
				rdata[8] = stock.data.cyl.hgt;
				break;
			case SIM_STOCK_SPHERE:
				irtn = ncl_simulate_stock_params(ldat,&stock,4);
				if (irtn != UU_SUCCESS) goto failed;
				iclw[4] = 5;
				jdatp[0] = stock.data.pstk.subt;
				jdatp[1] = stock.data.pstk.id;
				for (i=0;i<4;i++) rdata[i+1] = stock.data.pstk.params[i];
				break;
			case SIM_STOCK_TORUS:
				irtn = ncl_simulate_stock_params(ldat,&stock,8);
				if (irtn != UU_SUCCESS) goto failed;
				iclw[4] = 9;
				jdatp[0] = stock.data.pstk.subt;
				jdatp[1] = stock.data.pstk.id;
				for (i=0;i<8;i++) rdata[i+1] = stock.data.pstk.params[i];
				break;
			case SIM_STOCK_LOAD:
				irtn = ncl_simulate_stock_load(ldat,&stock);
				if (irtn != UU_SUCCESS) goto failed;
				jdatp[0] = stock.data.load.id;
				jdatp[1] = strlen(stock.data.load.fnam);
				pc = (char *)&rdata[1];
				strcpy(pc,stock.data.load.fnam);
				iclw[4] = (jdatp[1]-1)/8+2;
				break;
			case SIM_STOCK_CLONE:
				irtn = ncl_simulate_stock_clone(ldat,&stock);
				if (irtn != UU_SUCCESS) goto failed;
				jdatp[0] = stock.data.clone.id;
				jdatp[1] = stock.data.clone.stock;
				jdatp[2] = stock.data.clone.ncopy;
				jdatp[3] = 0;
				iclw[4]  = 2;
				break;
			case SIM_STOCK_MOVE:
				irtn = ncl_simulate_stock_move(ldat,&stock);
				if (irtn != UU_SUCCESS) goto failed;
				jdatp[0] = UU_LIST_LENGTH(&stock.idlist);
				jdatp[1] = 0;
				for (i=0;i<12;i++) rdata[i+1] = stock.data.move.mx[i];
				pc = (char *)&rdata[13];
				strcpy(pc,stock.data.move.mnam);
				n = strlen(pc);
				for (i=n;i<6;i++) pc[i] = ' ';
				pi = (UM_int2 *)&rdata[13];
				pi[3] = stock.data.move.msub;
				jp = (int *)UU_LIST_ARRAY(&stock.idlist);
				for (i=0;i<jdatp[0];i++) jdatp[i+28] = jp[i];
				iclw[4]  = (jdatp[0]-1)/2 + 15;
				UU_LIST_EMPTY(&stock.idlist);
				break;
			case SIM_STOCK_REMOVE:
				irtn = ncl_simulate_stock_remove(ldat,&stock);
				if (irtn != UU_SUCCESS) goto failed;
				jdatp[0] = UU_LIST_LENGTH(&stock.idlist);
				jdatp[1] = 0;
				jp = (int *)UU_LIST_ARRAY(&stock.idlist);
				for (i=0;i<jdatp[0];i++) jdatp[i+2] = jp[i];
				iclw[4]  = (jdatp[0]-1)/2 + 2;
				UU_LIST_EMPTY(&stock.idlist);
				break;
			case SIM_STOCK_CHIPS:
				irtn = ncl_simulate_stock_chips(ldat,&stock);
				if (irtn != UU_SUCCESS) goto failed;
				nc = UU_LIST_LENGTH(&stock.ptlist);
				rp = (UU_REAL *)UU_LIST_ARRAY(&stock.ptlist);
				for (i=0;i<nc;i++) rdata[i] = rp[i];
				iclw[4]  = nc;
				UU_LIST_EMPTY(&stock.ptlist);
				break;
			case SIM_STOCK_MODIFY:
				irtn = ncl_simulate_stock_modify(ldat,&stock);
				if (irtn != UU_SUCCESS) goto failed;
				jdatp[0] = UU_LIST_LENGTH(&stock.idlist);
				jdatp[1] = stock.data.modify.col;
				jdatp[2] = stock.data.modify.vis;
				jdatp[3] = stock.data.modify.trans;
				jdatp[4] = stock.data.modify.act;
				jdatp[5] = 0;
				rdata[3] = stock.data.modify.tol;
				jp = (int *)UU_LIST_ARRAY(&stock.idlist);
				for (i=0;i<jdatp[0];i++) jdatp[i+8] = jp[i];
				iclw[4]  = (jdatp[0]-1)/2 + 5;
				UU_LIST_EMPTY(&stock.idlist);
				break;
			case SIM_STOCK_STL:
				irtn = ncl_simulate_stock_stl(ldat,&stock);
				if (irtn != UU_SUCCESS) goto failed;
				jdatp[0] = stock.data.stl.id;
				jdatp[1] = strlen(stock.data.stl.fnam);
				jdatp[2] = stock.data.stl.units;
				jdatp[3] = 0;
				pc = (char *)&rdata[2];
				strcpy(pc,stock.data.stl.fnam);
				iclw[4] = (jdatp[1]-1)/8+3;
				break;
			}
			clstor(&iclf,&iclrec,iclw,rdata);
			break;
/*
.....TOOLPN record
*/
		case SIM_TOOLPN:
			irtn = ncl_simulate_parse(cdat,10,lbuf);
			if (irtn != UU_SUCCESS) goto failed;
			irtn = ul_to_reals(rnum,&nc,10,lbuf);
			if (irtn != UU_SUCCESS) goto failed;
			iclw[0] = rnum[0]; iclw[1] = 0; iclw[2] = 2000; iclw[3] = 1053;
			iclw[4] = 9;
			for (i=0;i<9;i++) rdata[i] = rnum[i+1];
			clstor(&iclf,&iclrec,iclw,rdata);
			break;
/*
.....FINI record
*/
		case SIM_FINI:
			if (!ihed) goto failed;
			iclw[0] = 0; iclw[1] = 0; iclw[2] = 14000; iclw[3] = 0; iclw[4] = 0;
			clstor(&iclf,&iclrec,iclw,rdata);
			break;
		}
	} while (iclw[2] != 14000);
	goto done;
/*
.....Not a simulation file
*/
failed:;
	irtn = UU_FAILURE;
/*
.....End of routine
*/
done:;
	if (fptr != UU_NULL) ux_fclose0(fptr);
	uu_list_free(&stock.idlist);
/*
........Match axes against already defined machine
*/
	ul_ipv_match_mach(LW_mach_simul,&LW_mach_model,LW_mach_nmodel);
/*
........Mount lathe tools on turret
*/
	if (LW_mach_simul && LW_is_lathe)
	{
/*		ul_ipv_free_turret();*/
		ul_ipv_mount_lathe_tools();
	}
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_machine(cdat,cmach)
**			This routine parses a Machine Simulation MACHINE record.
**    PARAMETERS   
**       INPUT  : 
**			cdat    = Machine simulation record to parse.
**       OUTPUT :  
**			chead   = Simulation HEADER structure.
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**					 otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_simulate_machine(cdat,cmach)
char *cdat;
UN_sim_machine *cmach;
{
	int i,nc,inc,irtn;
	char ldat[MAXSIMREC],lparm[80];
/*
.....Initialize routine
*/
	irtn = UU_SUCCESS;
	strcpy(ldat,cdat);
/*
.....Get Machine parameters
*/
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cmach->type);
	inc = 0;
	for (i=0;i<3;i++)
	{
		irtn = ncl_simulate_parse(ldat,1,cmach->linaxs[inc++]);
		irtn = ncl_simulate_parse(ldat,1,cmach->linaxs[inc++]);
		irtn = ncl_simulate_parse(ldat,1,lparm);
		irtn = ul_to_number(lparm,&cmach->numlin[i]);
		irtn = ncl_simulate_parse(ldat,1,lparm);
		irtn = ul_to_number(lparm,&cmach->lincalc[i]);
		irtn = ncl_simulate_parse(ldat,1,lparm);
		irtn = ul_to_reals(&cmach->lindis[i],&nc,1,lparm);
	}
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cmach->numrot);
	if (irtn != UU_SUCCESS) goto done;
	for (i=0;i<cmach->numrot;i++)
	{
		irtn = ncl_simulate_parse(ldat,1,cmach->rotaxs[i]);
		irtn = ncl_simulate_parse(ldat,1,lparm);
		irtn = ul_to_number(lparm,&cmach->rottype[i]);
		irtn = ncl_simulate_parse(ldat,3,lparm);
		irtn = ul_to_reals(cmach->rotvec[i],&nc,3,lparm);
		irtn = ncl_simulate_parse(ldat,3,lparm);
		irtn = ul_to_reals(cmach->rotorg[i],&nc,3,lparm);
	}
/*
.....End of routine
*/
done:;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_motion(cdat,cmach)
**			This routine parses a Machine Simulation MOTION record.
**    PARAMETERS   
**       INPUT  : 
**			cdat    = Machine simulation record to parse.
**       OUTPUT :  
**			cmot    = Simulation MOTION structure.
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**					 otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_simulate_motion(cdat,cmot)
char *cdat;
UN_sim_motion *cmot;
{
	int nc,irtn;
	char ldat[MAXSIMREC],lparm[80];
/*
.....Initialize routine
*/
	irtn = UU_SUCCESS;
	strcpy(ldat,cdat);
	cmot->head = 0;
/*
.....Get Motion parameters
*/
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cmot->isn);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_reals(&cmot->seq,&nc,1,lparm);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cmot->skip);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cmot->type);
	irtn = ncl_simulate_parse(ldat,6,lparm);
	irtn = ul_to_reals(cmot->tend,&nc,6,lparm);
	irtn = ncl_simulate_parse(ldat,10,lparm);
	irtn = ul_to_reals(cmot->axis,&nc,10,lparm);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_reals(&cmot->feed,&nc,1,lparm);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_reals(&cmot->time,&nc,1,lparm);
/*
.....Get blade forward direction
*/
	if (Mtype == 3)
	{
		irtn = ncl_simulate_parse(ldat,3,lparm);
		if (irtn == UU_SUCCESS)
			irtn = ul_to_reals(cmot->vfwd,&nc,3,lparm);
		else
		{
			um_vcmnvc(cmot->tend,Sspt,cmot->vfwd);
			um_unitvc(cmot->vfwd);
		}
		um_vctovc(cmot->tend,Sspt);
	}
/*
.....End of routine
*/
done:;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_hedmot(cdat,cmach)
**			This routine parses a Machine Simulation HEDMOT record.
**    PARAMETERS   
**       INPUT  : 
**			cdat    = Machine simulation record to parse.
**       OUTPUT :  
**			cmot    = Simulation MOTION structure.
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**					 otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_simulate_hedmot(cdat,cmot)
char *cdat;
UN_sim_motion *cmot;
{
	int nc,irtn,inum;
	char ldat[MAXSIMREC],lparm[200];
/*
.....Initialize routine
*/
	irtn = UU_SUCCESS;
	strcpy(ldat,cdat);
/*
.....Get Motion parameters
*/
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cmot->isn);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_reals(&cmot->seq,&nc,1,lparm);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cmot->skip);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cmot->type);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&inum);
	cmot->head = inum * 10;
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&inum);
	cmot->head = cmot->head + inum;
	irtn = ncl_simulate_parse(ldat,6,lparm);
	irtn = ul_to_reals(cmot->tend,&nc,6,lparm);
	irtn = ncl_simulate_parse(ldat,13,lparm);
	irtn = ul_to_reals(cmot->axis,&nc,13,lparm);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_reals(&cmot->feed,&nc,1,lparm);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_reals(&cmot->time,&nc,1,lparm);
	um_nullvc(cmot->vfwd);
/*
.....End of routine
*/
done:;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_parse(cdat,knum,cout);
**			This routine parses a Machine Simulation record and returns
**			the text of the requested parameter(s).
**    PARAMETERS   
**       INPUT  : 
**			cdat    = Machine Simulation record to parse.
**
**			knum    = Number of parameters to return.
**
**       OUTPUT :  
**			cdat    = Remainder of Machine Simulation record.
**
**			cout    = Text string containing requested parameters.
**    RETURNS      : Returns UU_FAILURE on parsing error, otherwise
**					 UU_SUCCESS.
**    SIDE EFFECTS : none.
**    WARNINGS     : none
*********************************************************************/
int ncl_simulate_parse(cdat,knum,cout)
int knum;
char *cdat,*cout;
{
	int i,irtn;
	char *pinc,*index(),buf[MAXSIMREC];
/*
.....Initialize routine
*/
	irtn = UU_SUCCESS;
	strcpy(buf,cdat);
/*
.....No more parameters
*/
	if (buf[0] == '\0') goto failed;
/*
.....Search for next comma
*/
	pinc = buf;
	for (i=0;i<knum;i++)
	{
		pinc = index(pinc,',');
/*
........Last parameter
*/
		if (pinc == 0 && i+1 != knum) goto failed;
		if (pinc != 0) pinc++;
	}
/*
.....Set text buffers
*/
	if (pinc == 0) cdat[0] = '\0';
	else
	{
		strcpy(cdat,pinc);
		*--pinc = '\0';
	}
	strcpy(cout,buf);
	goto done;
/*
.....Could not find enough parameters
*/
failed:;
	irtn = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_profil(cdat,cprof)
**			This routine parses a Machine Simulation PROFIL record.
**    PARAMETERS   
**       INPUT  : 
**			cdat    = Machine simulation record to parse.
**       OUTPUT :  
**			cprof   = Simulation PROFIL structure.
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**					 otherwise.
**    SIDE EFFECTS : Allocates memory to store the profile points in.
**		               This memory in 'cprof.pts' must be freed by the
**		               calling routine.
**    WARNINGS     : none
*********************************************************************/
ncl_simulate_profil(cdat,cprof)
char *cdat;
UN_sim_profil *cprof;
{
	int nc,irtn,inc;
	char ldat[MAXSIMREC],lparm[80];
/*
.....Initialize routine
*/
	irtn = UU_SUCCESS;
	strcpy(ldat,cdat);
/*
.....Get Profile parameters
*/
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cprof->isn);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_reals(&cprof->seq,&nc,1,lparm);
	irtn = ncl_simulate_parse(ldat,1,cprof->symbol);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cprof->subt);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cprof->npts);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cprof->type);
	cprof->pts = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*cprof->npts*4);
	if (cprof->pts == UU_NULL)
	{
		irtn = UU_FAILURE;
		goto done;
	}
	inc = 0;
	do
	{
		irtn = ncl_simulate_parse(ldat,4,lparm);
		irtn = ul_to_reals(&(cprof->pts[inc]),&nc,4,lparm);
		inc = inc + 4;
	} while (strcmp(ldat,"#EOL#") != 0);
	cprof->mxcl = inc;
/*
.....End of routine
*/
done:;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_read(fptr,ktype,cdat,knc,cmsg,kerr)
**			This routine reads a record from a Machine Simulation file
**			and returns the record text along with the record type.
**    PARAMETERS   
**       INPUT  : 
**			fptr    = FILE number of simulation file to read.
**       OUTPUT :  
**			ktype   = SIM_HEADER, SIM_MACHINE, SIM_PARTNO, SIM_ERROR,
**			          SIM_PPRINT, SIM_STATUS, SIM_MOTION, SIM_HEDMOT,
**                 SIM_FINI.
**
**			cdat    = Text of simulation record.
**
**			knc     = Number of chars in 'ldat'.
**
**			cmsg    = Text of error message.
**
**			kerr    = Returns 1 on parsing error, -1 on I/O error, or
**			          0 on success.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_simulate_read(fptr,ktype,cdat,knc,cmsg,kerr)
FILE *fptr;
int *knc,*kerr,*ktype;
char *cdat,*cmsg;
{
	int nc,stat;
	char buf[MAXSIMREC],ldat[MAXSIMREC],*q;
/*
.....Initialize routine
*/
	*kerr = 0;
    *knc = 0;
	cdat[0] = '\0';
/*
.....Read the record
*/
loop:;
	stat = ul_fread (fptr,buf,MAXSIMREC,&nc);
	if (stat == UX_EOF)
	{
		*ktype = SIM_FINI;
		goto done;
	}
    if (stat != UU_SUCCESS)
	{
		strcpy(cmsg,"Error reading Machine Simulation file.");
		*kerr = -1;
		goto done;
	}
/*
.....Check for continuation record
*/
	q = (char *)((long)&buf + nc - 5);
	*knc = *knc + nc;
	if (*knc >= MAXSIMREC) goto failed;
	strcat(cdat,buf);
	if (strcmp(q,"#EOL#") != 0) goto loop;
	*knc = strlen(cdat);
/*
.....Determine type of record
*/
	if (ncl_simulate_parse(cdat,1,ldat) != UU_SUCCESS) goto failed;
	if (strcmp(ldat,"HEADER") == 0) *ktype = SIM_HEADER;
	else if (strcmp(ldat,"MACHINE") == 0) *ktype = SIM_MACHINE;
	else if (strcmp(ldat,"PARTNO") == 0) *ktype = SIM_PARTNO;
	else if (strcmp(ldat,"ISN") == 0) *ktype = SIM_ISN;
	else if (strcmp(ldat,"PPRINT") == 0) *ktype = SIM_PPRINT;
	else if (strcmp(ldat,"ERROR") == 0) *ktype = SIM_ERROR;
	else if (strcmp(ldat,"STATUS") == 0) *ktype = SIM_STATUS;
	else if (strcmp(ldat,"ATTRIB") == 0) *ktype = SIM_ATTRIB;
	else if (strcmp(ldat,"CUTTER") == 0) *ktype = SIM_CUTTER;
	else if (strcmp(ldat,"PROFIL") == 0) *ktype = SIM_PROFIL;
	else if (strcmp(ldat,"CIRCLE") == 0) *ktype = SIM_CIRCLE;
	else if (strcmp(ldat,"MOTION") == 0) *ktype = SIM_MOTION;
	else if (strcmp(ldat,"HEDMOT") == 0) *ktype = SIM_HEDMOT;
	else if (strcmp(ldat,"STOCK") == 0) *ktype = SIM_STOCK;
	else if (strcmp(ldat,"FIXTUR") == 0) *ktype = SIM_FIXTUR;
	else if (strcmp(ldat,"TOOLPN") == 0) *ktype = SIM_TOOLPN;
	else if (strcmp(ldat,"FINI") == 0) *ktype = SIM_FINI;
	else goto failed;
	goto done;
/*
.....Invalid Simulation File
*/
failed:;
	strcpy(cmsg,"This is not a valid simulation file.");
	*kerr = 1;
/*
.....End of routine
*/
done:;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_status(cdat,cstat)
**			This routine parses a Machine Simulation STATUS record.
**    PARAMETERS   
**       INPUT  : 
**			cdat    = Machine simulation record to parse.
**       OUTPUT :  
**			cstat   = Simulation STATUS structure.
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**					 otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_simulate_status(cdat,cstat)
char *cdat;
UN_sim_status *cstat;
{
	int i,nc,irtn;
	char ldat[MAXSIMREC],lparm[80];
/*
.....Initialize routine
*/
	irtn = UU_SUCCESS;
	strcpy(ldat,cdat);
/*
.....Get Status parameters
*/
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cstat->isn);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_reals(&cstat->seq,&nc,1,lparm);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cstat->stop);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cstat->ncut);
	irtn = ncl_simulate_parse(ldat,cstat->ncut,lparm);
	irtn = ul_to_reals(cstat->cutter,&nc,cstat->ncut,lparm);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cstat->ncutd);
	irtn = ncl_simulate_parse(ldat,cstat->ncutd,lparm);
	irtn = ul_to_reals(cstat->cutterd,&nc,cstat->ncutd,lparm);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_reals(&cstat->rpm,&nc,1,lparm);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cstat->coolant);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&cstat->cutcom);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_reals(&cstat->tlno,&nc,1,lparm);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_reals(&cstat->tlength,&nc,1,lparm);
/*
.....Cutter symbol added 1/30/96
*/
	if (irtn == UU_SUCCESS)
	{
		irtn = ncl_simulate_parse(ldat,1,lparm);
		irtn = ul_to_number(lparm,&cstat->cutfl);
		if (irtn == UU_SUCCESS)
		{
			irtn = ncl_simulate_parse(ldat,1,cstat->cutsym);
			irtn = ncl_simulate_parse(ldat,1,lparm);
			irtn = ul_to_reals(&cstat->symofs[0],&nc,1,lparm);
		}
		else
		{
			irtn = UU_SUCCESS;
			cstat->cutfl = 0;
		}
	}
	if (irtn == UU_SUCCESS)
   {
	   irtn = ncl_simulate_parse(ldat,1,lparm);
	   irtn = ul_to_number(lparm,&cstat->mode);
/*
.....Lathe offset added 2/3/04
.....Cutter shank
*/
      if (irtn != UU_SUCCESS)
		{
			irtn = ul_to_reals(&cstat->symofs[1],&nc,1,lparm);
			irtn = ncl_simulate_parse(ldat,2,lparm);
			irtn = ul_to_reals(&cstat->symofs[2],&nc,1,lparm);
			irtn = ncl_simulate_parse(ldat,1,lparm);
			irtn = ul_to_number(lparm,&cstat->nshank);
			if (cstat->nshank != 0)
			{
				irtn = ncl_simulate_parse(ldat,cstat->nshank,lparm);
				irtn = ul_to_reals(cstat->shank,&nc,cstat->nshank,lparm);
			}
	   	irtn = ncl_simulate_parse(ldat,1,lparm);
	   	irtn = ul_to_number(lparm,&cstat->mode);
		}
		else
		{
			for (i=1;i<4;i++) cstat->symofs[i] = 0.;
			cstat->nshank = 0;
			for (i=0;i<5;i++) cstat->shank[i] = 0.;
		}
   }
/*
.....End of routine
*/
done:;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_text(cdat,ctext)
**			This routine parses a Machine Simulation text (PARTNO,
**			ERROR, etc.) record.
**    PARAMETERS   
**       INPUT  : 
**			cdat    = Machine simulation record to parse.
**       OUTPUT :  
**			cout    = Simulation text structure.
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**					 otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_simulate_text(cdat,ctext)
char *cdat;
UN_sim_text *ctext;
{
	int irtn,nc;
	char *rindex(),*pinc,ldat[MAXSIMREC],lparm[20];
/*
.....Initialize routine
*/
	irtn = UU_SUCCESS;
/*
.....Get sequence numbers
*/
	strcpy(ldat,cdat);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_number(lparm,&ctext->isn);
	irtn = ncl_simulate_parse(ldat,1,lparm);
	irtn = ul_to_reals(&ctext->seq,&nc,1,lparm);
/*
.....Copy PARTNO string
*/
	strcpy(ctext->str,ldat);
/*
.....Get Text string
*/
	pinc = rindex(ctext->str,',');
	*pinc = '\0';
/*
.....End of routine
*/
done:;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_stock(cdat,ldat,isn,type)
**       This routine parses a Machine Simulation STOCK or FIXTUR record.
**    PARAMETERS   
**      INPUT  : 
**        cdat    = Machine simulation record to parse.
**      OUTPUT :  
**        ldat    = Remainder of machine simulation record to parse.
**        isn     = Isn of record.
**        type    = type of record
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**                   otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_simulate_stock(cdat,ldat,isn,type)
char *cdat;
char *ldat;
UM_int4 *isn;
UM_int4 *type;
{
	int irtn,num;
	char lparm[80];
/*
.....Initialize routine
*/
	if (strlen(cdat) > MAXSIMREC)
	{
		irtn = UU_FAILURE;
		goto done;
	}
	strcpy(ldat,cdat);
/*
.....Get Stock or fixture type.
*/
	irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_number(lparm,&num);
	if (irtn == UU_SUCCESS)
	{
		*isn = num;
		irtn = ncl_simulate_parse(ldat,1,lparm);
	}
	if (irtn == UU_SUCCESS)
		irtn = ul_to_number(lparm,&num);
	if (irtn == UU_SUCCESS)
		*type = num;
/*
.....End of routine
*/
done:;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_stock_box(ldat,cstock)
**       This routine parses a Machine Simulation STOCK or FIXTUR BOX record.
**    PARAMETERS
**       INPUT  :
**       cdat    = Machine simulation record to parse.
**       OUTPUT :
**       cstock  = Simulation STOCK/FIXTUR structure.
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**                   otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_simulate_stock_box(ldat,cstock)
char *ldat;
UN_sim_stock *cstock;
{
	int nc,irtn;
	char lparm[80];

	irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_number(lparm,&cstock->data.box.subt);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_number(lparm,&cstock->data.box.id);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_reals(&cstock->data.box.pt1[0],&nc,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_reals(&cstock->data.box.pt1[1],&nc,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_reals(&cstock->data.box.pt1[2],&nc,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_reals(&cstock->data.box.pt2[0],&nc,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_reals(&cstock->data.box.pt2[1],&nc,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_reals(&cstock->data.box.pt2[2],&nc,1,lparm);
/*
.....End of routine
*/
done:;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_stock_cyl(ldat,cstock)
**       This routine parses a Machine Simulation STOCK or FIXTUR CYL record.
**    PARAMETERS
**       INPUT  :
**       cdat    = Machine simulation record to parse.
**       OUTPUT :
**       cstock  = Simulation STOCK/FIXTUR structure.
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**                   otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_simulate_stock_cyl(ldat,cstock)
char *ldat;
UN_sim_stock *cstock;
{
	int nc,irtn;
	char lparm[80];

	irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_number(lparm,&cstock->data.cyl.subt);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_number(lparm,&cstock->data.cyl.id);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_reals(&cstock->data.cyl.cir[0],&nc,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_reals(&cstock->data.cyl.cir[1],&nc,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_reals(&cstock->data.cyl.cir[2],&nc,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_reals(&cstock->data.cyl.cir[3],&nc,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_reals(&cstock->data.cyl.cir[4],&nc,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_reals(&cstock->data.cyl.cir[5],&nc,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_reals(&cstock->data.cyl.cir[6],&nc,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_reals(&cstock->data.cyl.hgt,&nc,1,lparm);
/*
.....End of routine
*/
done:;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_stock_params(ldat,cstock,nprm)
**       This routine parses a Machine Simulation standard STOCK or FIXTUR
**       record.
**    PARAMETERS
**       INPUT  :
**       cdat    = Machine simulation record to parse.
**       nprm    = Number of parameters to parse.
**       OUTPUT :
**       cstock  = Simulation STOCK/FIXTUR structure.
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**                   otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_simulate_stock_params(ldat,cstock,nprm)
char *ldat;
UN_sim_stock *cstock;
{
	int nc,irtn,i;
	char lparm[80];

	irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_number(lparm,&cstock->data.pstk.subt);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_number(lparm,&cstock->data.pstk.id);
	for (i=0;i<nprm;i++)
	{
		if (irtn == UU_SUCCESS)
			irtn = ncl_simulate_parse(ldat,1,lparm);
		if (irtn == UU_SUCCESS)
			irtn = ul_to_reals(&cstock->data.pstk.params[i],&nc,1,lparm);
	}
/*
.....End of routine
*/
done:;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_stock_load(ldat,cstock)
**       This routine parses a Machine Simulation STOCK or FIXTUR LOAD record.
**    PARAMETERS
**       INPUT  :
**       cdat    = Machine simulation record to parse.
**       OUTPUT :
**       cstock  = Simulation STOCK/FIXTUR structure.
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**                   otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_simulate_stock_load(ldat,cstock)
char *ldat;
UN_sim_stock *cstock;
{
	int irtn;
	char lparm[80];

	irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_number(lparm,&cstock->data.load.id);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,cstock->data.load.fnam);
/*
.....End of routine
*/
done:;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_stock_clone(ldat,cstock)
**       This routine parses a Machine Simulation STOCK or FIXTUR CLONE record.
**    PARAMETERS
**       INPUT  :
**       cdat    = Machine simulation record to parse.
**       OUTPUT :
**       cstock  = Simulation STOCK/FIXTUR structure.
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**                   otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_simulate_stock_clone(ldat,cstock)
char *ldat;
UN_sim_stock *cstock;
{
	int irtn;
	char lparm[80];

	irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_number(lparm,&cstock->data.clone.id);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_number(lparm,&cstock->data.clone.stock);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_number(lparm,&cstock->data.clone.ncopy);
/*
.....End of routine
*/
done:;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_stock_move(ldat,cstock)
**       This routine parses a Machine Simulation STOCK or FIXTUR MOVE record.
**    PARAMETERS
**       INPUT  :
**       cdat    = Machine simulation record to parse.
**       OUTPUT :
**       cstock  = Simulation STOCK/FIXTUR structure.
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**                   otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_simulate_stock_move(ldat,cstock)
char *ldat;
UN_sim_stock *cstock;
{
	int i,j,nc,irtn=UU_SUCCESS;
	char lparm[80];

	for (i=0;i<12 && irtn==UU_SUCCESS;i++)
	{
		irtn = ncl_simulate_parse(ldat,1,lparm);
		if (irtn == UU_SUCCESS)
			irtn = ul_to_reals(&cstock->data.move.mx[i],&nc,1,lparm);
	}
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,cstock->data.move.mnam);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_number(lparm,&cstock->data.move.msub);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	while (strcmp(lparm,"#EOL#") != 0 && irtn == UU_SUCCESS)
	{
		irtn = ul_to_number(lparm,&j);
		if (irtn == UU_SUCCESS)
			uu_list_push(&cstock->idlist,&j);
		if (irtn == UU_SUCCESS)
			irtn = ncl_simulate_parse(ldat,1,lparm);
	}
/*
.....End of routine
*/
done:;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_stock_remove(ldat,cstock)
**       This routine parses a Machine Simulation STOCK or FIXTUR REMOVE record.
**    PARAMETERS
**       INPUT  :
**       cdat    = Machine simulation record to parse.
**       OUTPUT :
**       cstock  = Simulation STOCK/FIXTUR structure.
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**                   otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_simulate_stock_remove(ldat,cstock)
char *ldat;
UN_sim_stock *cstock;
{
	int j,irtn;
	char lparm[80];

	irtn = ncl_simulate_parse(ldat,1,lparm);
	while (strcmp(lparm,"#EOL#") != 0 && irtn == UU_SUCCESS)
	{
		irtn = ul_to_number(lparm,&j);
		if (irtn == UU_SUCCESS)
			uu_list_push(&cstock->idlist,&j);
		if (irtn == UU_SUCCESS)
			irtn = ncl_simulate_parse(ldat,1,lparm);
	}
/*
.....End of routine
*/
done:;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_stock_chips(ldat,cstock)
**       This routine parses a Machine Simulation STOCK/REMOVE,CHIPS record.
**    PARAMETERS
**       INPUT  :
**       cdat    = Machine simulation record to parse.
**       OUTPUT :
**       cstock  = Simulation STOCK structure.
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**                   otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_simulate_stock_chips(ldat,cstock)
char *ldat;
UN_sim_stock *cstock;
{
	int irtn,nc;
	UU_REAL rnum;
	char lparm[80];

	irtn = ncl_simulate_parse(ldat,1,lparm);
	while (strcmp(lparm,"#EOL#") != 0 && irtn == UU_SUCCESS)
	{
		irtn = ul_to_reals(&rnum,&nc,1,lparm);
		if (irtn == UU_SUCCESS)
			uu_list_push(&cstock->ptlist,&rnum);
		if (irtn == UU_SUCCESS)
			irtn = ncl_simulate_parse(ldat,1,lparm);
	}
/*
.....End of routine
*/
done:;
	return(irtn);
}


/*********************************************************************
**    E_FUNCTION     : ncl_simulate_stock_modify(ldat,cstock)
**       This routine parses a Machine Simulation STOCK or FIXTUR MODIFY record.
**    PARAMETERS
**       INPUT  :
**       cdat    = Machine simulation record to parse.
**       OUTPUT :
**       cstock  = Simulation STOCK/FIXTUR structure.
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**                   otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_simulate_stock_modify(ldat,cstock)
char *ldat;
UN_sim_stock *cstock;
{
	int j,nc,irtn;
	char lparm[80];

	irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_number(lparm,&cstock->data.modify.col);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_number(lparm,&cstock->data.modify.vis);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_number(lparm,&cstock->data.modify.trans);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_number(lparm,&cstock->data.modify.act);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_reals(&cstock->data.modify.tol,&nc,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	while (strcmp(lparm,"#EOL#") != 0 && irtn == UU_SUCCESS)
	{
		irtn = ul_to_number(lparm,&j);
		if (irtn == UU_SUCCESS)
			uu_list_push(&cstock->idlist,&j);
		if (irtn == UU_SUCCESS)
			irtn = ncl_simulate_parse(ldat,1,lparm);
	}
/*
.....End of routine
*/
done:;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_simulate_stock_stl(ldat,cstock)
**       This routine parses a Machine Simulation STOCK or FIXTUR STL record.
**    PARAMETERS
**       INPUT  :
**       cdat    = Machine simulation record to parse.
**       OUTPUT :
**       cstock  = Simulation STOCK/FIXTUR structure.
**    RETURNS      : Returns UU_FAILURE on parsing error, UU_SUCCESS
**                   otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_simulate_stock_stl(ldat,cstock)
char *ldat;
UN_sim_stock *cstock;
{
	int irtn;
	char lparm[80];

	irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_number(lparm,&cstock->data.stl.id);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,lparm);
	if (irtn == UU_SUCCESS)
		irtn = ul_to_number(lparm,&cstock->data.stl.units);
	if (irtn == UU_SUCCESS)
		irtn = ncl_simulate_parse(ldat,1,cstock->data.stl.fnam);
/*
.....End of routine
*/
done:;
	return(irtn);
}
