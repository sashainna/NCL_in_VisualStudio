/*********************************************************************
**    NAME         :  nemotattr.c
**       CONTAINS:
**          ncl_motattr_set
**          ncl_motattr_get
**          ncl_motmattr_set
**          ncl_motmattr_get
**          ncl_motblade_set
**          ncl_motblade_get
**				ncl_motblade_reset
**          ncl_motview_set
**          ncl_motview_get
**          ncl_motisn_set
**          ncl_motisn_get
**          ncl_motisn_getrec
**          ncl_motisn_get_count
**          ncl_motisn_reset
**          ncl_motisn_size
**          ncl_motisn_call_list
**          ncl_motisn_source_list
**          ncl_motattr_reset
**          ncl_motattr_size
**          ncl_motmattr_reset
**          ncl_motmattr_size
**          ncl_motblade_size
**          ncl_motview_size
**				ncl_motattr_get_ptrs
**				ncl_motattr_set_ptrs
**          ncl_motattr_store
**          ncl_motmattr_store
**          ncl_motblade_store
**          ncl_motview_store
**    COPYRIGHT 2006 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nemotattr.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:37
*********************************************************************/
#include "usysdef.h"
#include "udforms.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "nclmplay.h"
#include "ulist.h"
#include "view.h"

static int S_init_attr();
static int S_match_attr();
static int S_match_mattr();
static int S_init_blade();
static int S_init_view();
static int S_match_view();

static int Sactive_attr = -1;
static int Sactive_mattr = -1;
static int Sactive_blade = -1;
static int Sactive_view = -1;
static int Sactive_isn = -1;
static UU_LIST Sattr_list;
static UU_LIST Smattr_list;
static UU_LIST Sblade_list;
static UU_LIST Sview_list;
static UU_LIST Sisn_list;

void ncl_motattr_get();
void ncl_motmattr_get();
void ncl_motview_get();

/*********************************************************************
**    E_FUNCTION     : ncl_motattr_set(color,lnstyle,pen)
**			Stores the motion display attributes in the list (attempts to
**			match them with an already stored attribute bundle first) and
**			returns the value of the attribute bundle where these attributes
**       are stored.
**    PARAMETERS   
**       INPUT  : 
**          color    = Motion display color.
**          lnstyle  = Motion display line style.
**          pen      = Motion display pen number.
**
**       OUTPUT : 
**			none.
**    RETURNS      : Index of attribute bundle where the input values are
**                   stored.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_motattr_set(color,lnstyle,pen)
int color,lnstyle,pen;
{
	UN_motseg_attr myattr;
/*
.....Get active attribute structure
*/
	ncl_motattr_get(&myattr,UN_MOTSEG_ACTIVE);
/*
.....Find matching motion attributes
*/
	Sactive_attr = S_match_attr(&myattr,color,lnstyle,pen);
	return(Sactive_attr);
}

/*********************************************************************
**    I_FUNCTION     : ncl_motattr_get(attr,which)
**			Returns the requested full motion attribute structure.
**    PARAMETERS   
**       INPUT  : 
**          which    = Which motion attribute structure to return.
**                     UN_MOTSEG_ACTIVE returns the active structure.
**
**       OUTPUT : 
**          attr     = Requested motion attribute structure.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_motattr_get(attr,which)
UN_motseg_attr *attr;
int which;
{
	int inc;
	UN_motseg_attr *myattr;
/*
.....Initialize routine
*/
	if (which == UN_MOTSEG_ACTIVE) inc = Sactive_attr;
	else inc = which;
/*
.....No attribute has been defined
*/
	if (inc == -1) S_init_attr(attr);
/*
.....Return full attribute structure
*/
	else
	{
		myattr = (UN_motseg_attr *)UU_LIST_ARRAY(&Sattr_list);
		attr->color = myattr[inc].color;
		attr->lnstyle = myattr[inc].lnstyle;
		attr->pen = myattr[inc].pen;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_motmattr_set(mattr)
**			Stores the motion cutting attributes in the list (attempts to
**			match them with an already stored attribute bundle first) and
**			returns the value of the attribute bundle where these attributes
**       are stored.
**    PARAMETERS   
**       INPUT  :
**          mattr  = Motion cutting attributes to store.
**       OUTPUT : none
**			none.
**    RETURNS      : Index of attribute bundle where the input values are
**                   stored.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_motmattr_set(mattr)
UN_mot_attr *mattr;
{
	UN_mot_attr myattr;
/*
.....Get active attribute structure
*/
	ncl_motmattr_get(&myattr,UN_MOTSEG_ACTIVE);
/*
.....Find matching motion attributes
*/
	Sactive_mattr = S_match_mattr(&myattr,mattr);
	return(Sactive_mattr);
}

/*********************************************************************
**    I_FUNCTION     : ncl_motmattr_get(mattr,which)
**			Returns the requested full motion cutting attribute structure.
**    PARAMETERS   
**       INPUT  : 
**          which    = Which cutting attribute structure to return.
**                     UN_MOTSEG_ACTIVE returns the active structure.
**
**       OUTPUT : 
**          mattr     = Requested motion cutting attribute structure.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_motmattr_get(mattr,which)
UN_mot_attr *mattr;
int which;
{
	int inc;
	UN_mot_attr *myattr;
/*
.....Initialize routine
*/
	if (which == UN_MOTSEG_ACTIVE) inc = Sactive_mattr;
	else inc = which;
/*
.....No attribute has been defined
*/
	if (inc == -1) S_init_mattr(mattr);
/*
.....Return full attribute structure
*/
	else
	{
		myattr = (UN_mot_attr *)UU_LIST_ARRAY(&Smattr_list);
		*mattr = myattr[inc];
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_motblade_set(vfwd)
**			Stores the motion blade direction in the list if a Blade cutter
**       is currently defined.
**    PARAMETERS   
**       INPUT  : 
**          vfwd     = Blade forward direction.
**
**       OUTPUT : none
**
**    RETURNS      : Index of blade structure where the input values are
**                   stored.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_motblade_set(vfwd)
UU_REAL *vfwd;
{
	int nc,i,ifl;
	UN_motseg_blade lblade;
/*
.....Do nothing if blade cutter not defined
*/
	nc = -1;
	if (ncl_cutter_is_blade())
	{
/*
.....Blade list not used yet
.....initialize it
*/
		if (Sactive_blade == -1) S_init_blade(&lblade);
/*
.....Push blade parameters onto stack
*/
		nc = UU_LIST_LENGTH(&Sblade_list);
		ifl = 0;
		for (i=0;i<3;i++)
		{
			lblade.tfwd[i] = vfwd[i];
			if (fabs(lblade.tfwd[i]) > UM_DFUZZ) ifl = 1;
		}
		if (ifl == 1 || nc == 0)
		{
			if (um_mag(lblade.tfwd) < UM_DFUZZ) lblade.tfwd[0] = 1.;
			uu_list_push(&Sblade_list,&lblade);
			Sactive_blade = nc;
		}
		else Sactive_blade = nc - 1;
	}
	return(Sactive_blade);
}

/*********************************************************************
**    I_FUNCTION     : ncl_motblade_get(blade,which)
**			Returns the requested full motion blade structure.
**    PARAMETERS   
**       INPUT  : 
**          which    = Which motion blade structure to return.
**                     UN_MOTSEG_ACTIVE returns the active structure.
**
**       OUTPUT : 
**          blade    = Requested motion blade structure.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_motblade_get(blade,which)
UN_motseg_blade *blade;
int which;
{
	int inc,i;
	UN_motseg_blade *myblade;
/*
.....Initialize routine
*/
	if (which == UN_MOTSEG_ACTIVE) inc = Sactive_blade;
	else inc = which;
/*
.....No blade has been defined
*/
	if (inc == -1)
	{
		blade->tfwd[0] = 1.;
		blade->tfwd[1] = 0.;
		blade->tfwd[2] = 0.;
	}
/*
.....Return full blade structure
*/
	else
	{
		myblade = (UN_motseg_blade *)UU_LIST_ARRAY(&Sblade_list);
		for (i=0;i<3;i++) blade->tfwd[i] = myblade[inc].tfwd[i];
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_motblade_reset()
**			Resets (clears) the motion blade direction list.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_motblade_reset()
{
/*
.....Delete the blade list
*/
	if (Sactive_blade != -1)
	{
		UU_LIST_EMPTY(&Sblade_list);
		Sactive_blade = 0.;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_motview_set()
**			Stores the active motion viewports in the list (attempts to
**			match them with an already stored view bundle first) and
**			returns the value of the view bundle where these attributes
**       are stored.
**    PARAMETERS   
**       INPUT  : none
**
**       OUTPUT : none
**    RETURNS      : Index of view bundle where the active views are
**                   stored.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_motview_set()
{
	UN_motseg_view myview;
/*
.....Get active view structure
*/
	ncl_motview_get(&myview,UN_MOTSEG_ACTIVE);
/*
.....Find matching motion views
*/
	Sactive_view = S_match_view(&myview);
	return(Sactive_view);
}

/*********************************************************************
**    I_FUNCTION     : ncl_motview_get(view,which)
**			Returns the requested motion view structure.
**    PARAMETERS   
**       INPUT  : 
**          which    = Which motion view structure to return.
**                     UN_MOTSEG_ACTIVE returns the active structure.
**
**       OUTPUT : 
**          view     = Requested motion view structure.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_motview_get(view,which)
UN_motseg_view *view;
int which;
{
	int inc,i;
	UN_motseg_view *myview;
/*
.....Initialize routine
*/
	if (which == UN_MOTSEG_ACTIVE) inc = Sactive_view;
	else inc = which;
/*
.....No view has been defined
*/
	if (inc == -1) S_init_view(view);
/*
.....Return full view structure
*/
	else
	{
		myview = (UN_motseg_view *)UU_LIST_ARRAY(&Sview_list);
		view->nview = myview[inc].nview;
		for (i=0;i<view->nview;i++)
		{
			view->view[i] = myview[inc].view[i];
			view->vinc[i] = myview[inc].vinc[i];
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_motisn_set(isn,flag)
**			Stores the ISN call/loop stack for the active motion.
**    PARAMETERS   
**       INPUT  : 
**          isn      = ISN call/loop stack.
**          flag     = UU_TRUE = Check for duplicate entries,
**                     UU_FALSE = Store isn structure without checking
**                     for duplicate entry.
**       OUTPUT : none
**
**    RETURNS      : Index of ISN structure where the input values are
**                   stored.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_motisn_set(isn,flag)
UN_motseg_isn *isn;
UU_LOGICAL flag;
{
	int i;
	UU_LOGICAL found;
	UN_motseg_isn lisn,*pisn;
/*
.....ISN list not used yet
.....initialize it
*/
	if (Sactive_isn == -1)
	{
		uu_list_init(&Sisn_list,sizeof(UN_motseg_isn),1000,500);
		if (Sisn_list.data == UU_NULL) goto done;
	}
/*
.....See if this call/loop isn matches
.....the previously stored entry
*/
	else if (flag)
	{
		pisn = (UN_motseg_isn *)UU_LIST_ARRAY(&Sisn_list);
		if (isn->nent == pisn[Sactive_isn].nent)
		{
			found = UU_TRUE;
			for (i=0;i<isn->nent;i++)
				if (isn->line[i] != pisn[Sactive_isn].line[i]) found = UU_FALSE;
			if (found) goto done;
		}
	}
/*
.....Allocate memory for call/loop ISN storage
*/
	lisn.nent = isn->nent;
	lisn.line = (int *)uu_malloc(isn->nent*sizeof(int));
	if (lisn.line == UU_NULL) goto done;
	for (i=0;i<lisn.nent;i++) lisn.line[i] = isn->line[i];
/*
.....Push call/loop isns onto stack
*/
	Sactive_isn = UU_LIST_LENGTH(&Sisn_list);
	uu_list_push(&Sisn_list,&lisn);
/*
.....End of routine
*/
done:;
	return(Sactive_isn);
}

/*********************************************************************
**    E_FUNCTION :  ncl_motisn_get(isn,nline,ktype,dval,cstr,nc,indx)
**       Returns the line number along with the text of the source line
**       that is associated with the motion.
**
**       This routine should be called repeatedly to return all source
**       lines in the calling stack.  Calling it once with 'indx'
**       set to 0 will return the current line number.
**    PARAMETERS   
**       INPUT  : 
**          isn      Pointer to ISN structure of motion data.
**          indx     Should be set to 0 on initial call and will be
**                   adjusted from then on out.
**       OUTPUT :
**          nline    Line number of source line.
**          ktype    0 = Current line, 1 = Macro Call, 2 = DO Loop.
**          dval     DO loop index value when 'ktype' = 2.
**          cstr     Text of source line.
**          nc       Number of characters in 'cstr'.
**          indx     Updated to point to the next source line associated
**                   with the motion.  Returns -1 if there are no more
**                   associated lines.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_motisn_get(isn,nline,ktype,dval,cstr,nc,indx)
int isn;
int *nline,*ktype;
UU_REAL *dval;
char *cstr;
int *nc,*indx;
{
	int i,inc,nent,ilin,pinc,stat;
	UN_motseg_isn *myisn;
	union {UU_REAL val; UM_int4 ival[2];} doindx;
/*
.....Find source lines associated with geometry key
*/
	if (isn < 0 || isn > UU_LIST_LENGTH(&Sisn_list)) goto failed;
	myisn = (UN_motseg_isn *)UU_LIST_ARRAY(&Sisn_list);
	if (*indx >= myisn[isn].nent) goto failed;
/*
.....Find the requested entry
*/

	nent = myisn[isn].nent;
	inc = 0;
	do
	{
		if (myisn[isn].line[inc] < 0)
		{
			ilin = -(myisn[isn].line[inc]);
			*ktype = 2;
			doindx.ival[0] = myisn[isn].line[inc+1];
			doindx.ival[1] = myisn[isn].line[inc+2];
			*dval = doindx.val;
			inc += 3;
			pinc = 3;
		}
		else
		{
			ilin = myisn[isn].line[inc];
			*ktype = 1;
			if (inc == 0) *ktype = 0;
			inc++;
			pinc = 1;
		}
		if (inc >= nent) break;
	} while (inc <= *indx);
/*
.....Get actual line number & source statement
*/
	*indx += pinc;
	stat = ncl_getsrc_rec(ilin,nline,cstr,nc);
	if (stat != UU_SUCCESS) *nline = ilin;
	goto done;
/*
.....Failure storing source line
*/
failed:
	*indx = -1;
/*
.....End of routine
*/
done:
	return;
}

/*********************************************************************
**    E_FUNCTION :  ncl_motisn_getrec(lines,indx)
**       Returns the ISN call/loop stack data store in the requested
**       ISN record.
**
**    PARAMETERS   
**       INPUT  : 
**          indx     Index into ISN list of record to return.
**       OUTPUT :
**          lines    Call/loop stack data for requested ISN record.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_motisn_getrec(lines,indx)
int *lines,indx;
{
	int i;
	UN_motseg_isn *myisn;
/*
.....Return call/loop stack
*/
	if (indx >= 0 && indx < UU_LIST_LENGTH(&Sisn_list))
	{
		myisn = (UN_motseg_isn *)UU_LIST_ARRAY(&Sisn_list);
		for (i=0;i<myisn[indx].nent;i++) lines[i] = myisn[indx].line[i];
	}
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**    E_FUNCTION :  ncl_motisn_get_count(isn,nent)
**       Returns the number of lines in the call stack for the specified
**       motion.
**
**    PARAMETERS   
**       INPUT  : 
**          isn      Pointer to ISN structure of motion data.
**       OUTPUT :
**          nent     Number of lines in call stack.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_motisn_get_count(isn,nent)
int isn;
int *nent;
{
	UN_motseg_isn *myisn;
/*
.....Return the number of lines on the call stack
*/
	if (isn >= 0 && isn < UU_LIST_LENGTH(&Sisn_list))
	{
		myisn = (UN_motseg_isn *)UU_LIST_ARRAY(&Sisn_list);
		if (myisn == UU_NULL) *nent = 0;
		else *nent = myisn[isn].nent;
	}
	else
		*nent = 0;
}

/*********************************************************************
**    E_FUNCTION     : ncl_motisn_reset()
**			Resets (clears) the motion ISN call/loop stack list.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_motisn_reset()
{
	int i;
	UN_motseg_isn *pisn;
/*
.....Delete the ISN list
*/
	if (Sactive_isn != -1)
	{
		pisn = (UN_motseg_isn *)UU_LIST_ARRAY(&Sisn_list);
		for (i=0;i<UU_LIST_LENGTH(&Sisn_list);i++) uu_free(pisn[i].line);
		uu_list_free(&Sisn_list);
		Sactive_isn = -1;
	}
}

/*********************************************************************
**    I_FUNCTION     : ncl_motisn_size()
**			Returns the size of the motion ISN call/loop stack list.
**    PARAMETERS   
**       INPUT  : none
**
**       OUTPUT : 
**          nc       = Size of motion ISN list.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_motisn_size()
{
/*
.....Return size of list
*/
	return(UU_LIST_LENGTH(&Sisn_list));
}

/*********************************************************************
**    E_FUNCTION     :  ncl_motisn_call_list(isn,key,clist,iline,flag)
**       Propogates the form Call Stack list with the data associated
**       with the current selection.
**    PARAMETERS
**       INPUT  :
**          isn          Pointer to ISN structure of motion data
**                       (flag = 2).
**          key          Key of entity to use as reference (flag = 1).
**          flag         1 = Use entity as reference, 2 = Use motion as
**                       reference.
**       OUTPUT :
**          call_list    DAS list to create with call stack.
**          isnout       Line number command associated with current
**                       selection.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_motisn_call_list(isn,key,call_list,isnout,flag)
int isn;
UU_KEY_ID key;
UD_LIST *call_list;
int *isnout,flag;
{
	int indx,nent,nline,type,nc,i,iline;
	UM_int2 icmt,itype;
	UU_REAL dval;
	char sbuf[NCL_MAX_COMLINE],buf[NCL_MAX_COMLINE],tbuf[NCL_MAX_COMLINE];
	char *p,*p1,*p2,**cptr,*strstr(),*strchr(),lnum[40];
/*
.....Initialize routine
*/
	*isnout = 0;
/*
.....Call list
*/
	if (flag == 2)
	{
		if (UN_mcd_nlines != 0)
		{
			nent = 0;
			indx = 0;
			ncl_motisn_get(isn,&nline,&type,&dval,sbuf,&nc,&indx);
			*isnout = nline;
		}
		else ncl_motisn_get_count(isn,&nent);
	}
	else ncl_srcctl_get_count(key,&nent);
	if (nent != 0)
	{
		ul_ipv_init_list(call_list,nent);
		indx = 0;
		do
		{
			if (flag == 2)
				ncl_motisn_get(isn,&nline,&type,&dval,sbuf,&nc,&indx);
			else
				ncl_srcctl_get(key,&nline,&type,&dval,sbuf,&nc,&indx);
			if (indx == -1) break;
			if (nc > 80)
			{
				sbuf[80] = '\0';
				nc = 80;
			}
/*
........Shorten the CALL command
*/
			if (type == 1)
			{
				icmt = 0;
				iline = nline - 1;
				strcpy(buf,sbuf);
				for (i=0;i<50;i++)
				{
					strcpy(tbuf,sbuf);
					ul_to_upper(tbuf);
					p = strstr(tbuf,"CALL");
					if (p != UU_NULL)
					{
						p1 = strchr(p,'/');
						if (p1 != UU_NULL)
						{
							p2 = strchr(p1,',');
							if (p2 != UU_NULL) *p2 = '\0';
							strcpy(sbuf,"CALL");
							strcat(sbuf,p1);
							nline = iline + 1;
							break;
						}
					}
					iline--;
					nclf_getsrc(&iline,sbuf,&nc,&itype,&icmt);
					sbuf[nc] = '\0';
				}
				if (i == 50) strcpy(sbuf,buf);
			}
/*
........Shorten the DO command
*/
			else if (type == 2)
			{
				ncl_sprintf2(lnum,&dval,1,-1);
				strcpy(tbuf,sbuf);
				ul_to_upper(tbuf);
				p = strchr(tbuf,'/');
				if (p != UU_NULL)
				{
					p++;
					p1 = strchr(p,'=');
					if (p1 != UU_NULL)
					{
						*p1 = '\0';
						sprintf(sbuf,"DO/%s=%s",p,lnum);
					}
					else
						sprintf(sbuf,"DO/%s",lnum);
				}
				else
					sprintf(sbuf,"DO/%s",lnum);
			}
/*
........Store the line in the list
*/
			sprintf(buf,"%d %s",nline,sbuf);
			buf[80] = '\0';
			ul_ipv_put_list(call_list,buf);
			if (type == 0)
			{
				*isnout = nline;
				strcpy(call_list->answer,buf);
			}
		} while (indx != -1);
	}
/*
.....No source lines to display
*/
	else
	{
		ul_ipv_init_list(call_list,1);
		ul_ipv_put_list(call_list,"");
	}
}

/*********************************************************************
**    E_FUNCTION     :  ncl_motisn_source_list(isn,src_list)
**       Propogates the form Source list with the data associated
**       with the current selection.
**    PARAMETERS
**       INPUT  :
**          isn          ISN of line to store in list.
**       OUTPUT :
**          src_list     DAS list to create with source lines.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_motisn_source_list(isn,src_list)
int isn;
UD_LIST *src_list;
{
	int ibeg,iend,ns,i,jnc;
	UM_int2 nc;
	UU_REAL dval;
	char sbuf[NCL_MAX_COMLINE],buf[NCL_MAX_COMLINE],**cptr;
/*
.....Propogate Source list
*/
	if (isn != 0)
	{
		ns = 5;
		ul_ipv_init_list(src_list,ns);
		ibeg = isn - ns/2;
		if (ibeg <=0) ibeg = 1;
		iend = ibeg + ns;
		for (i=ibeg;i<iend;i++)
		{
			if (UN_mcd_nlines == 0)
				gtpsrc(&i,&nc,sbuf);
			else
			{
				ncl_mcd_read(i-1,sbuf,&jnc);
				nc = jnc;
			}
			if (nc > 80) nc = 80;
			sbuf[nc] = '\0';
			sprintf(buf,"%d %s",i,sbuf);
			buf[80] = '\0';
			ul_ipv_put_list(src_list,buf);
			if (i == isn) strcpy(src_list->answer,buf);
		}
	}
/*
.....No source lines to display
*/
	else
	{
		ul_ipv_init_list(src_list,1);
		ul_ipv_put_list(src_list,"");
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_motattr_reset()
**			Frees the motion attribute, blade, view, and ISN lists.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_motattr_reset()
{
/*
.....Free motion attribute list
*/
	if (Sactive_attr != -1)
	{
		Sactive_attr = -1;
		uu_list_free(&Sattr_list);
	}
/*
.....Free cutting attribute list
*/
	if (Sactive_mattr != -1)
	{
		Sactive_mattr = -1;
		uu_list_free(&Smattr_list);
	}
/*
.....Free blade list
*/
	if (Sactive_blade != -1)
	{
		Sactive_blade = -1;
		uu_list_free(&Sblade_list);
	}
/*
.....Free view list
*/
	if (Sactive_view != -1)
	{
		Sactive_view = -1;
		uu_list_free(&Sview_list);
	}
/*
.....Free ISN list
*/
	if (Sactive_isn != -1)
	{
		Sactive_isn = -1;
		uu_list_free(&Sisn_list);
	}
}

/*********************************************************************
**    I_FUNCTION     : ncl_motattr_size()
**			Returns the size of the motion attribute list.
**    PARAMETERS   
**       INPUT  : none
**
**       OUTPUT : 
**          nc       = Size of motion attribute list.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_motattr_size()
{
/*
.....Return size of list
*/
	return(UU_LIST_LENGTH(&Sattr_list));
}

/*********************************************************************
**    I_FUNCTION     : ncl_motmattr_size()
**			Returns the size of the cutting attribute list.
**    PARAMETERS   
**       INPUT  : none
**
**       OUTPUT : 
**          nc       = Size of cutting attribute list.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_motmattr_size()
{
/*
.....Return size of list
*/
	return(UU_LIST_LENGTH(&Smattr_list));
}

/*********************************************************************
**    I_FUNCTION     : ncl_motblade_size()
**			Returns the size of the motion blade list.
**    PARAMETERS   
**       INPUT  : none
**
**       OUTPUT : 
**          nc       = Size of motion blade list.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_motblade_size()
{
/*
.....Return size of list
*/
	return(UU_LIST_LENGTH(&Sblade_list));
}

/*********************************************************************
**    I_FUNCTION     : ncl_motview_size()
**			Returns the size of the motion view list.
**    PARAMETERS   
**       INPUT  : none
**
**       OUTPUT : 
**          nc       = Size of motion blade list.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_motview_size()
{
/*
.....Return size of list
*/
	return(UU_LIST_LENGTH(&Sview_list));
}

/*********************************************************************
**    E_FUNCTION     : ncl_motattr_get_ptrs(ptrs)
**			Returns the active motion attribute pointers.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : 
**          ptrs     = [0] = Attribute, [1] = Blade, [2] = View.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_motattr_get_ptrs(ptrs)
int ptrs[];
{
	ptrs[0] = Sactive_attr;
	ptrs[1] = Sactive_blade;
	ptrs[2] = Sactive_view;
	ptrs[3] = Sactive_isn;
	ptrs[4] = Sactive_mattr;
}

/*********************************************************************
**    E_FUNCTION     : ncl_motattr_set_ptrs(ptrs)
**			Sets the active motion attribute pointers.
**    PARAMETERS   
**       INPUT  :
**          ptrs     = [0] = Attribute, [1] = Blade, [2] = View.
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_motattr_set_ptrs(ptrs)
int ptrs[];
{
	Sactive_attr = ptrs[0];
	Sactive_blade = ptrs[1];
	Sactive_view = ptrs[2];
	Sactive_isn = ptrs[3];
	Sactive_mattr = ptrs[4];
}

/*********************************************************************
**    E_FUNCTION     : ncl_motattr_store(attr)
**			Pushes a motion attribute structure onto the list.
**    PARAMETERS   
**       INPUT  : 
**          attr     = Attribute structure.
**
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_motattr_store(attr)
UN_motseg_attr *attr;
{
	if (Sactive_attr == -1)
	{
		uu_list_init(&Sattr_list,sizeof(UN_motseg_attr),20,5);
		Sactive_attr = 0;
	}
	uu_list_push(&Sattr_list,attr);
}

/*********************************************************************
**    E_FUNCTION     : ncl_motmattr_store(mattr)
**			Pushes a cutting attribute structure onto the list.
**    PARAMETERS   
**       INPUT  : 
**          mattr    = Attribute structure.
**
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_motmattr_store(mattr)
UN_mot_attr *mattr;
{
	if (Sactive_mattr == -1)
	{
		uu_list_init(&Smattr_list,sizeof(UN_mot_attr),20,5);
		Sactive_mattr = 0;
	}
	uu_list_push(&Smattr_list,mattr);
}

/*********************************************************************
**    E_FUNCTION     : ncl_motblade_store(attr)
**			Pushes a motion blade structure onto the list.
**    PARAMETERS   
**       INPUT  : 
**          blade    = Blade structure.
**
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_motblade_store(blade)
UN_motseg_blade *blade;
{
	if (Sactive_blade == -1)
	{
		uu_list_init(&Sblade_list,sizeof(UN_motseg_blade),500,100);
		Sactive_blade = 0;
	}
	uu_list_push(&Sblade_list,blade);
}

/*********************************************************************
**    E_FUNCTION     : ncl_motview_store(attr)
**			Pushes a motion view structure onto the list.
**    PARAMETERS   
**       INPUT  : 
**          view     = View structure.
**
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_motview_store(view)
UN_motseg_view *view;
{
	if (Sactive_view == -1)
	{
		uu_list_init(&Sview_list,sizeof(UN_motseg_view),20,5);
		Sactive_view = 0;
	}
	uu_list_push(&Sview_list,view);
}

/*********************************************************************
**    I_FUNCTION     : S_init_attr(attr)
**			Initializes the motion attribute storage list.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :
**          attr   = Initialized Motion Attribute structure.
**    RETURNS      : UU_FAILURE if memory could not be allocated.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_init_attr(attr)
UN_motseg_attr *attr;
{
/*
.....Initialize attribute list
*/
	uu_list_init(&Sattr_list,sizeof(UN_motseg_attr),20,5);
	if (Sattr_list.data == UU_NULL) goto failed;
/*
.....Initialize provided attribute structure
*/
	attr->color = -1;
	attr->lnstyle = 0;
	attr->pen = 0;
	return(UU_SUCCESS);
/*
.....Failed trying to allocate list
*/
failed:;
	return(UU_FAILURE);
}

/*********************************************************************
**    I_FUNCTION     : S_init_mattr(attr)
**			Initializes the cutting attribute storage list.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :
**          mattr  = Initialized Cutting Attribute structure.
**    RETURNS      : UU_FAILURE if memory could not be allocated.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_init_mattr(mattr)
UN_mot_attr *mattr;
{
/*
.....Initialize attribute list
*/
	uu_list_init(&Smattr_list,sizeof(UN_mot_attr),20,5);
	if (Smattr_list.data == UU_NULL) goto failed;
/*
.....Initialize provided attribute structure
*/
	mattr->loadtl = 0;
	mattr->tlen = 0.;
	mattr->sp_mode = 0;
	mattr->sp_val = 0.;
	mattr->coolnt = 0;
	mattr->cc_mode = 0;
	mattr->cc_dir = 0;
	return(UU_SUCCESS);
/*
.....Failed trying to allocate list
*/
failed:;
	return(UU_FAILURE);
}

/*********************************************************************
**    I_FUNCTION     : S_init_blade(blade)
**			Initializes the motion blade direction storage list.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :
**          blade  = Initialized Motion Blade direction structure.
**    RETURNS      : UU_FAILURE if memory could not be allocated.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_init_blade(blade)
UN_motseg_blade *blade;
{
/*
.....Initialize blade list
*/
	uu_list_init(&Sblade_list,sizeof(UN_motseg_blade),500,100);
	if (Sblade_list.data == UU_NULL) goto failed;
/*
.....Initialize provided attribute structure
*/
	blade->tfwd[0] = 1.;
	blade->tfwd[1] = 0.;
	blade->tfwd[2] = 0.;
	return(UU_SUCCESS);
/*
.....Failed trying to allocate list
*/
failed:;
	return(UU_FAILURE);
}

/*********************************************************************
**    I_FUNCTION     : S_init_view(view)
**			Initializes the motion view storage list.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :
**          attr   = Initialized Motion View structure.
**    RETURNS      : UU_FAILURE if memory could not be allocated.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_init_view(view)
UN_motseg_view *view;
{
/*
.....Initialize view list
*/
	uu_list_init(&Sview_list,sizeof(UN_motseg_view),20,5);
	if (Sview_list.data == UU_NULL) goto failed;
/*
.....Initialize provided attribute structure
*/
	view->nview = 0;
	return(UU_SUCCESS);
/*
.....Failed trying to allocate list
*/
failed:;
	return(UU_FAILURE);
}

/*********************************************************************
**    I_FUNCTION     : S_match_attr(attr,color,lnstyle,pen)
**			Determines if the motion attributes match an existing attributes.
**       Returns a pointer to the motion attributes structure that matches
**			the input attributes.
**    PARAMETERS   
**       INPUT  : 
**          attr     = Active attribute structure.
**          color    = Motion color.
**          lnstyle  = Motion line style.
**          pen      = Motion pen.
**
**       OUTPUT : none
**
**    RETURNS      : Pointer to matched attribute bundle.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_match_attr(attr,color,lnstyle,pen)
UN_motseg_attr *attr;
int color,lnstyle,pen;
{
	int j,nc,iret;
	UU_LOGICAL matched;
	UN_motseg_attr *myattr,lattr;
/*
.....See if parameters match active motion attributes
*/
	if (color == attr->color && lnstyle == attr->lnstyle && pen == attr->pen)
	{
		iret = Sactive_attr;
	}
/*
.....No match
.....Search attribute list
*/
	else
	{
		nc = UU_LIST_LENGTH(&Sattr_list);
		myattr = (UN_motseg_attr *)UU_LIST_ARRAY(&Sattr_list);
		matched = UU_FALSE;
		for (j=0;j<nc;j++)
		{
			if (color == myattr[j].color && lnstyle == myattr[j].lnstyle &&
				pen == myattr[j].pen)
			{
				attr->color = myattr[j].color;
				attr->lnstyle = myattr[j].lnstyle;
				attr->pen = myattr[j].pen;
				iret = j;
				matched = UU_TRUE;
				break;
			}
		}
/*
........No match found
........add new record
*/
		if (!matched)
		{
			lattr.color = color; lattr.lnstyle = lnstyle; lattr.pen = pen;
			uu_list_push(&Sattr_list,&lattr);
			iret = nc;
		}
	}
/*
.....End of routine
*/
	return(iret);
}

/*********************************************************************
**    I_FUNCTION     : S_match_mattr(mattr1,mattr2)
**			Determines if the motion cutting attributes match an existing
**       attributes.  Returns a pointer to the motion cutting attributes
**       structure that matches the input attributes.
**    PARAMETERS   
**       INPUT  : 
**          mattr1   = Active cutting attribute structure.
**          mattr2   = Cutting attribute structure to set.
**
**       OUTPUT : none
**
**    RETURNS      : Pointer to matched attribute bundle.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_match_mattr(mattr1,mattr2)
UN_mot_attr *mattr1,*mattr2;
{
	int j,nc,iret;
	UU_LOGICAL matched;
	UN_mot_attr *myattr,lattr;
/*
.....See if parameters match active motion attributes
*/
	nc = UU_LIST_LENGTH(&Smattr_list);
	if (mattr2->loadtl == mattr1->loadtl && mattr2->tlen == mattr1->tlen &&
		mattr2->sp_mode == mattr1->sp_mode && mattr2->sp_val == mattr1->sp_val &&
		mattr2->coolnt == mattr1->coolnt && mattr2->cc_mode == mattr1->cc_mode &&
		mattr2->cc_dir == mattr1->cc_dir && nc > 0)
	{
		iret = Sactive_mattr;
	}
/*
.....No match
.....Search attribute list
*/
	else
	{
		myattr = (UN_mot_attr *)UU_LIST_ARRAY(&Smattr_list);
		matched = UU_FALSE;
		for (j=0;j<nc;j++)
		{
			if (mattr2->loadtl == myattr[j].loadtl &&
				mattr2->tlen == myattr[j].tlen &&
				mattr2->sp_mode == myattr[j].sp_mode &&
				mattr2->sp_val == myattr[j].sp_val &&
				mattr2->coolnt == myattr[j].coolnt &&
				mattr2->cc_mode == myattr[j].cc_mode &&
				mattr2->cc_dir == myattr[j].cc_dir) 
			{
				*mattr1 = myattr[j];
				iret = j;
				matched = UU_TRUE;
				break;
			}
		}
/*
........No match found
........add new record
*/
		if (!matched)
		{
			lattr = *mattr2;
			uu_list_push(&Smattr_list,&lattr);
			iret = nc;
		}
	}
/*
.....End of routine
*/
	return(iret);
}

/*********************************************************************
**    I_FUNCTION     : S_match_view(view)
**			Determines if the current view definitions matches an existing
**       view bundle.  Returns a pointer to the motion view structure that
**       matches the current view definitions.
**    PARAMETERS   
**       INPUT  : 
**          view     = Active view structure.
**
**       OUTPUT : none
**
**    RETURNS      : Pointer to matched view bundle.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_match_view(view)
UN_motseg_view *view;
{
	int i,j,nc,iret;
	UV_vport vport;
	UU_LOGICAL matched;
	UN_motseg_view *myview,lview;
/*
.....Get active views
*/
	lview.nview = 0;
	for (i=1;i<=UV_act_screen[0].nvports;i++)
	{
		uv_getvpid(UV_act_screen[0].vports[i-1],&vport);
		if (vport.motion)
		{
			lview.view[lview.nview] = vport.cur_view;
			lview.vinc[lview.nview] = i;
			lview.nview++;
		}
	}
/*
.....See if parameters match active views
*/
	matched = UU_FALSE;
	if (lview.nview == view->nview)
	{
		matched = UU_TRUE;
		for (i=0;i<lview.nview;i++)
		{
			if (lview.view[i] != view->view[i])
			{
				matched = UU_FALSE;
				break;
			}
		}
	}
	if (matched)
	{
		iret = Sactive_view;
	}
/*
.....No match
.....Search view list
*/
	else
	{
		nc = UU_LIST_LENGTH(&Sview_list);
		myview = (UN_motseg_view *)UU_LIST_ARRAY(&Sview_list);
		matched = UU_FALSE;
		for (j=0;j<nc;j++)
		{
			if (lview.nview == myview[j].nview)
			{
				matched = UU_TRUE;
				for (i=0;i<lview.nview;i++)
				{
					if (lview.view[i] != myview[j].view[i])
					{
						matched = UU_FALSE;
						break;
					}
				}
				if (matched)
				{
					iret = j;
					break;
				}
			}
		}
/*
........No match found
........add new record
*/
		if (!matched)
		{
			uu_list_push(&Sview_list,&lview);
			iret = nc;
		}
	}
/*
.....End of routine
*/
	return(iret);
}
