/*********************************************************************
**	FILENAME: auth.c
**	CONTAINS:     
**           getver
**           getrel
**           auth_bat
** IF NOT CIMLINC:
**			 initaut - FORTRAN CALLABLE, calls ncl_init_auth.
**           ncl_init_auth - Authorize terminal with run privileges.
**           cauth
**           unauth
**           ncl_get_licenses()
** CIMLINC VERSION:
**           cauth
**           unauth
**
** Test file
**     
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nauth.c , 26.4
**    DATE AND TIME OF LAST  MODIFICATION
**       01/16/19 , 09:36:18
*********************************************************************/

#include "usysdef.h"
#include "xenv1.h"
#include "nclfc.h"
#define NCLVERSION
#include "nclver.h"		/* Defines's version number */

char *moindx[] = {"JAN","FEB","MAR",
					"APR","MAY","JUN",
					"JUL","AUG","SEP",
					"OCT", "NOV","DEC"};

/* MILLS: variables to hold valid software and options */
int NAUTCAM = 0;
int NAUTCAD = 0;
int NAUT502 = 0;
int NAUTLTH = 0;
int NAUT501 = 0;
int NAUTIGES = 0;
int NAUTSTEP = 0;
int NAUTVDA = 0;
int NAUTMESH = 0;
int NAUTQUILT = 0;
int NAUTNMG = 0;
int N3AXIS = 0;
int NAUTIPV = 0;
int NAUTMACH = 0;
int NAUTVT = 0;
int NPRPST = 0;
int NVMILL = 0;

#if UU_COMP == UU_WINNT
char NCL_lic[120];
#endif

char NCL_keyinfo[9] = "00000000";
extern unsigned int NCL_subprocess;

/*********************************************************************
**    E_SUBROUTINE     : subroutine getver
**          C callable routine to get the NCL version number.
**       INPUT  : 
**          none
**       OUTPUT :  
**          ver    -  Current NCL version number.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void
getver(ver)
UM_real8 *ver;
	{
	*ver = NCL_version;
	return;
	}

/*********************************************************************
**    E_SUBROUTINE     : subroutine getrel
**          C and FORTRAM callable routine to get the date and time of last 
**          "release" - that is, the date and time of the last change to this 
**          module.
**       INPUT  : 
**          none
**       OUTPUT :  
**          buf    -  date and time of last "release".
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void getrel(buf)
char *buf;
	{
	int d,mo,yr;

	sscanf(last_release,"%d/%d/%d", &mo, &d, &yr);

	/* cover ourselves in case we make it this far ... */
	if (yr < 90) 
		yr = 2000+yr;
	else 
		yr = 1900+yr;

	sprintf(buf,"%d-%s-%d\0", d, moindx[mo-1], yr);
		
	return;
	}
/*********************************************************************
**	 E_FUNCTION : auth_batch()
**			Authorize batch.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/

void auth_batch(rcam,lathe,tl3axis,prepst,vmill)
int *rcam,*lathe,*tl3axis,*prepst,*vmill;
	{

/*
.....Set LATHE module and 3AXIS mode flags
.....Not supported on Cimlinc
*/
	*lathe = NAUTLTH;
	*tl3axis = N3AXIS;
	*prepst  = NPRPST;
	*vmill  = NVMILL;
	if (NAUTCAM && NAUT502) 
		*rcam = 1;
	else
		*rcam = -1;
	return;
	}
/*********************************************************************
**   E_FUNCTION : auth_vt()
**          Authorize vtmode.
**   PARAMETERS
**       INPUT  :  none.
**       OUTPUT :  none.
**   RETURNS: none
**   SIDE EFFECTS: none.
**   WARNINGS: none.
*********************************************************************/
 
void auth_vt(rcam,lathe,tl3axis)
int *rcam,*lathe,*tl3axis;
    {
 
/*
.....Set LATHE module and 3AXIS mode flags
.....Not supported on Cimlinc
*/
    *lathe = NAUTLTH;
    *tl3axis = N3AXIS;
    if (NAUTCAM && NAUT502 && NAUTVT)
        *rcam = 1;
    else
        *rcam = -1;
    return;
    }

/*********************************************************************
**    E_FUNCTION     :  int iniaut()
**       FORTRAN CALLABLE routine to setup authorization
**		 Valid for MESH SURFPAC, QUILT SURFPAC, NCL501 and IGES501
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : ???
**    WARNINGS     : none
*********************************************************************/
void iniaut(f77program,f77opts, optlst)
UM_f77_str_ptr f77program, f77opts;
UM_int2 optlst[9];
	{
	char *program, *opts;
	int i;

	program = UM_cstr_of_f77_str(f77program);
	opts = UM_cstr_of_f77_str(f77opts);

	for (i = 0; i < 80; i++)
		if (program[i] == '_') 
			program[i] = ' ';
		else if (program[i] == ' ') 
			{
			program[i] = '\0';
			break;
			}	
	ncl_init_auth(program);

	/* set optlst here ... */
	if (!strcmp(program,"NCLCAM"))
		{
		optlst[0] = NAUTMESH;
		optlst[1] = NAUT501;
		}
	if (!strcmp(program,"IGES"))
		{
		optlst[0] = NAUTMESH;
		optlst[1] = NAUT501;
		}
	else if (!strcmp(program,"MESH SURFPAC"))
		optlst[0] = NAUTMESH;
	else if (!strcmp(program,"QUILT SURFPAC"))
		optlst[0] = NAUTQUILT;
	else if (!strcmp(program,"NMG"))
		optlst[0] = NAUTNMG;
	return;
	}

/*********************************************************************
**    E_FUNCTION     :  int ncl_init_auth()
**       Verify calling routine is authorized to run on this terminal
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : ???
**    WARNINGS     : none
*********************************************************************/
int ncl_init_auth(program)
char *program;	/* program string */
	{
	char rel[20], lmsg[80], msg[256];
#if UU_COMP == UU_WINNT
	char *p,*ux_getenv();
#endif
	int iop[8], ierr, ret;

/*
.....Get current version as date string
*/
	getrel(rel);
/*
.....On Windows NT if 'getenv' is called
.....from 'gtenv' from 'pwdaut', then
.....you cannot run the post directly from NCL
....Thus this call to get the NCCS_LICENSE variabe
*/
#if UU_COMP == UU_WINNT
	p = ux_getenv("NCCS_LICENSE");
	if (p != 0) strcpy(NCL_lic,p);
	else NCL_lic[0] = '\0';
	ul_remove_quotes(NCL_lic);
#endif
/*
......if this is a sub-NCL process, then we look at the key common area
......to get the keys
*/
	ret = 0;
	if (NCL_subprocess>0)
	{
		ret = ul_chkkey_common(NCL_keyinfo);
	}
/*
.....Authorize NCLCAM
*/
	ierr = 0;
	if (!strcmp(program,"NCLCAM,NCLCADD") || !strcmp(program,"NCLCAM"))
	{
		if ((ret==1)&&(NCL_keyinfo[0]=='1'))
		{
			NAUTCAM = 1;
			NAUT502 = 1;
			if (NCL_keyinfo[2] == '1')
				NAUTLTH = 1;
			if (NCL_keyinfo[3] == '1')
				NPRPST = 1;
			if (NCL_keyinfo[5] == '1')
				NVMILL = 1;
			if (NCL_keyinfo[4] == '1')
				N3AXIS = 1;
		}
		else
		{
			pwdaut("NCLCAM","LATHE,PREPST,VOLUMILL,3AXIS",rel,iop,lmsg,&ierr);
/*
.....Check options
*/
			if (ierr == 0)
			{
				NAUTCAM = 1;
				NAUT502 = 1;
				NCL_keyinfo[0] = '1';
				if (iop[0] == 1)
				{
					NAUTLTH = 1;
					NCL_keyinfo[2] = '1';
				}
				if (iop[1] == 1)
				{
					NPRPST = 1;
					NCL_keyinfo[3] = '1';
				}
				if (iop[2] == 1)
				{
					NVMILL = 1;
					NCL_keyinfo[5] = '1';
				}
				if (iop[3] == 1)
				{
					N3AXIS = 1;
					NCL_keyinfo[4] = '1';
				}
			}
/*
.....Could not authorize NCLCAM
*/
			else
			{
				sprintf(msg, "NCLCAM: %s\n", lmsg);
				ud_printmsg(msg);
				NCL_keyinfo[0] = '0';
				NCL_keyinfo[2] = '0';
				NCL_keyinfo[3] = '0';
				NCL_keyinfo[4] = '0';
			}
		}
	}
/*
.....Authorize NCLCADD
*/
	if (!strcmp(program,"NCLCAM,NCLCADD") || !strcmp(program,"NCLCADD"))
	{
		if ((ret==1)&&(NCL_keyinfo[1]=='1'))
		{
			NAUTCAD = 1;
		}
		else
		{
			pwdaut("NCLCADD","",rel,iop,lmsg,&ierr);
			NCL_keyinfo[1] = '0';
			if (ierr == 0)
			{
				NAUTCAD = 1;
				NCL_keyinfo[1] = '1';
			}
/*
.....Could not authorize NCLCADD
*/
#if UU_COMP != UU_WIN2K
			else
			{
				sprintf(msg, "NCLCADD: %s\n", lmsg);
				ud_printmsg(msg);
			}
#endif
		}
	}
/*
.....Not authorized to run CAM or CADD
*/
	if (!NAUTCAM && !NAUTCAD && ierr != 0)
	{
		ud_printmsg("Not authorized to run on this terminal.  Exiting.\n");
		exit(0);
	}
/*
.....Authorize NCLIPV
*/
	ierr = 0;
#ifdef UU_IPV
	if (!strcmp(program,"NCLCAM,NCLCADD") || !strcmp(program,"NCLCAM") ||
		!strcmp(program,"NCLIPV"))
	{
		pwdaut("NCLIPV","SIMULATE,MACHINE",rel,iop,lmsg,&ierr);
/*
.....Check options
*/
		NCL_keyinfo[5] = '0';
		NCL_keyinfo[6] = '0';
		NCL_keyinfo[7] = '0';
		if (ierr == 0)
		{
			NCL_keyinfo[5] = '1';
			if (iop[0] == 1)
			{
				NAUTIPV = 1;
				NCL_keyinfo[6] = '1';
			}
			if (iop[1] == 1)
			{
				NAUTMACH = 1;
				NCL_keyinfo[7] = '1';
			}
		}
/*
.....Could not authorize NCLCAM
*/
		else if (!strcmp(program,"NCLIPV"))
		{
			sprintf(msg, "NCLIPV: %s\nExiting.\n", lmsg);
			ud_printmsg(msg);
		}
	}
#endif
/*
.....Authorize IGES
*/
	if (!strcmp(program,"IGES"))
	{
		pwdaut(program,"IGES502",rel,iop,lmsg,&ierr);
		if ((ierr == 0) /* && (iop[0] == 1) */ )
		{
			NAUTIGES = 1;
			NAUT502 = 1;
		}
		else
		{
			sprintf(msg, "IGES: %s\nExiting.\n", lmsg);
			ud_printmsg(msg);
		}
	}
/*
.....Authorize STEP
*/
	if (!strcmp(program,"STEP"))
	{
		pwdaut(program,"NCLSTEP",rel,iop,lmsg,&ierr);
		if ((ierr == 0) /* && (iop[0] == 1) */ )
		{
			NAUTSTEP = 1;
		}
		else
		{
			sprintf(msg, "NCLSTEP: %s\nExiting.\n", lmsg);
			ud_printmsg(msg);
		}
	}
/*
.....Authorize VDA
*/
	if (!strcmp(program,"VDA"))
	{
		pwdaut(program,"",rel,iop,lmsg,&ierr);
		if (ierr == 0)
		{
			NAUTVDA = 1;
		}
		else
		{
			sprintf(msg, "VDA: %s\nExiting.\n", lmsg);
			ud_printmsg(msg);
		}
	}
/*
.....Authorize MESH Surfpac
*/
	if (!strcmp(program,"MESH SURFPAC"))
	{
		pwdaut(program,"",rel,iop,lmsg,&ierr);
		if (ierr == 0)
		{
			NAUTMESH = 1;
		}
		else
		{
			sprintf(msg, "%s: %s\nExiting.\n", program, lmsg);
			ud_printmsg(msg);
		}
	}
/*
.....Authorize QUILT Surfpac
*/
	if (!strcmp(program,"QUILT SURFPAC"))
	{
		pwdaut(program,"",rel,iop,lmsg,&ierr);
		if (ierr == 0)
		{
			NAUTQUILT = 1;
		}
		else
		{
			sprintf(msg, "%s: %s\nExiting.\n", program, lmsg);
			ud_printmsg(msg);
		}
	}
/*
.....Authorize NMG Surfpac
*/
	if (!strcmp(program,"NMG"))
	{
		pwdaut(program,"",rel,iop,lmsg,&ierr);
		if (ierr == 0)
		{
			NAUTNMG = 1;
		}
		else
		{
			sprintf(msg, "%s: %s\nExiting.\n", program, lmsg);
			ud_printmsg(msg);
		}
	}
/*
.....Authorize SolidWorks
*/
	if (!strcmp(program,"SOLIDWORKS"))
	{
		pwdaut(program,"",rel,iop,lmsg,&ierr);
	}
	return(ierr);
}

/*********************************************************************
**    E_FUNCTION     :  int cauth()
**       Allocate terminal.
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : ???
**    WARNINGS     : none
*********************************************************************/
void cauth(ucam,ucad,rcam,rcad,lathe,tl3axis, prepst, vmill, mbuf, cbuf)
int *ucam,*ucad;	/* User requested these */
int *rcam,*rcad;	/* This is what the user can use */
int *lathe;			/* Lathe enabled or not */
int *tl3axis;			/* 3AXIS mode enabled or not */
int *prepst;      /* Prepst functions enabled or not */
int *vmill;       /* VoluMill functions enabled or not */
char *mbuf, *cbuf;	/* Error message buffers */
	{
	char lmsg[80];
	int ierr;
/*
.....Set LATHE module and 3AXIS mode flags
*/
	*lathe = NAUTLTH;
	*tl3axis = N3AXIS;
	*prepst  = NPRPST;
	*vmill = NVMILL;
	*rcam = 0;
	*rcad = 0;

/*
....Changed for checking NCL501+ mode.
....Paul. 04/13/92
*/
	if (*ucam == 2)
	{
		if (NAUTCAM && NAUT501)
		{
			pwdall("NCLCAM",lmsg,&ierr);
			if (ierr == 0)
				*rcam = 1;
			else
			{
				*rcam = -1;
				strcpy(mbuf,lmsg);
			}
		}
		else
		{
			*rcam = -1;
			sprintf(mbuf,"Not authorized to run NCL501+.");
		}
	}
	else if (NAUTCAM && NAUT502 && *ucam) 
		{
		/* call pwdall() and set *rcam = 1 if can do */
		pwdall("NCLCAM",lmsg,&ierr);
		if (ierr == 0)
			*rcam = 1;
		else
			{
			*rcam = -1;
			strcpy(mbuf, lmsg);
			}
		}
	else if ((!NAUTCAM || !NAUT502) && *ucam)
		{
		*rcam = -1;
		sprintf (mbuf,"Not authorized to run NCLCAM on this terminal.");
		}

	 if (NAUTCAD && *ucad)
		{
		/* call pwdall() and set *rcad = 1 if can do */
		pwdall("NCLCADD",lmsg,&ierr);
		if (ierr == 0)
			*rcad = 1;
		else
			{
			*rcad = -1;
			strcpy(cbuf, lmsg);
			}
		}
	else if (!NAUTCAD && *ucad)
		{
		*rcad = -1;
		sprintf (cbuf,"Not authorized to run NCLCADD on this terminal.");
		}
	}
/*********************************************************************
**	 E_FUNCTION : unauth()
**			Deallocates a user of CAM and CADD.
**	 PARAMETERS	
**		 INPUT  :
**        icam    = 1 - Deallocate CAM user.
**        icad    = 1 - Deallocate CADD user.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
void unauth(icam,icad)
int *icam,*icad;
	{
	if (*icam)
		pwddea("NCLCAM");
	if (*icad)
		pwddea("NCLCADD");
	pwddea("");
	return;
	}

/*********************************************************************
**	 E_FUNCTION : ncl_get_license(lic)
**			Returns the state of available licenses in NCL.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT :
**        lic    = 0 - CAM, 1 - CADD, 2 - Lathe, 3 - 3-axis, 4 - IPV,
**                 5 - MachSim, 6 - Prepst.
**	 RETURNS: none
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
void ncl_get_licenses(lic)
int *lic;
{
	lic[0] = NAUTCAM;
	lic[1] = NAUTCAD;
	lic[2] = NAUTLTH;
	lic[3] = N3AXIS;
	lic[4] = NAUTIPV;
	lic[5] = NAUTMACH;
	lic[6] = NPRPST;
	lic[7] = NVMILL;
}

