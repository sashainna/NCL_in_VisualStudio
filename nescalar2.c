/*********************************************************************
**    NAME         :  nescalar.c
**       CONTAINS: routines to handle NCL scalars
**					ncl_get_scalar_value
**					ncl_parse_scalar_values
**					ncl_rmend_unit
**					ncl_get_data_value
**					ncl_get_vocwrd
**
**    COPYRIGHT 1988 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nescalar2.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 17:29:57
*********************************************************************/
#include <math.h>
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mfort.h"
#include "mdrel.h"
#include "mattr.h"
#include "mcrv.h"
#include "mdebug.h"
#include "mdcoord.h"
#include "udforms.h"
#include "ulist.h"
#include "gtbl.h"
#include "gdidd.h"

#include "ncl.h"
#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"
#include "nclfc.h"
#include "mdunits.h"
#include "mdcpln.h"
#include "nclvx.h"
extern int MSLite;
void ncl_get_vocwrd(int, char *);
/*********************************************************************
**    E_FUNCTION     : int ncl_get_scalar_value(string, scalar_value)
**       Get a scalar value or a data item value from a text string.
**    PARAMETERS   
**       INPUT  : 
**          string: scalar or data item name
**       OUTPUT :  
**          scalar_value:	scalar value
**    RETURNS      : 
**       1: get scalar value successfully
**       0: it's a number value
**       -1: not a scalar value
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_scalar_value(in_string, scalar_value)
char *in_string;
UU_REAL	*scalar_value;
{
	struct NCL_scalar_rec e;
	int status, ifnd, len, label_end, sub_end, i, j, not_num, type;
	char label[65], substr[20], string[65], string_sav[65];
	UM_int4 nclsubscript;
	UM_int4 ipg,iel,nclkey;
	UM_f77_str ncllabel;
	UM_int2 nwds,ietype;
	int rel_num;
	char *savptr,*ncl_get_macptr();
	char data_value[NCL_MAX_LABEL+1];

	status = 1;
/*
.....get rid of preceeding space
*/
	len = strlen (in_string);
	if (len>NCL_MAX_LABEL) len = NCL_MAX_LABEL;
	strncpy(string, in_string, len);
	string[len] = '\0';
	i = 0;
	while ((string[i]==' ') && (i<len)) i++;
	strcpy (string, &(string[i]));
	i = strlen (string);
	if (i==0)
		return UU_FAILURE;
/*
.....get rid of trailing space
*/  
	while ((string[i-1]==' ') && (i>0)) i--;
	string[i] = '\0';
	len = i;
	if (i==0)
		return UU_FAILURE;
/*
.....save string to use later
*/
	strcpy(string_sav, string);
	for (i=0, j=0, label_end = 0, sub_end = 0, not_num=0; i<len;i++)
	{
		if (isalpha(string[i]) || (string[i]=='#') || (string[i]=='_'))
/*
.....label string
*/
		{
			not_num = 1;
		}
		else if ((string[i]=='(') && (label_end==0))
		{
			strncpy(label, string, i);
			label[i] = '\0';
			label_end = 1;
			not_num = 1;
			continue;
		}
		else if ((string[i]==')') && (sub_end==0))
		{
			substr[j] = '\0';
			sub_end = 1;
			not_num = 1;
			continue;
		}
		else if ((isdigit(string[i])==0)&&(string[i]!='.')
			&&(string[i]!='+') && (string[i]!='-'))
		{
			goto check_data;
		}
		if (label_end==0)
			label[i] = string[i];
		else if (sub_end==0)
			substr[j++] = string[i];
		else
			goto check_data;
	}
	if (not_num==0)
	{
		if (strlen(string)>0)
		{
			*scalar_value = atof(string);
			return 0;
		}
		else
		{
			*scalar_value = 0.;
			return 0;
		}
	}
/*
.....not number, failed since we are not support string value
.....in MS LITE
*/
	if (MSLite)
		return -1;

	if (sub_end)
		nclsubscript = atoi(substr);
	else
		nclsubscript = 0;
	if ((i>0) && (label_end==0))
		label[i] = '\0';
	ul_to_upper(label);
	UM_init_f77_str(ncllabel, label, NCL_MAX_LABEL);
/*
......if the ncllabel is a macro, vxchk will set change the current macro pointer
......so we need save and reset it
*/
	savptr = ncl_get_macptr();
	ifnd = vxchk(UM_addr_of_f77_str(ncllabel), &nclsubscript, &nclkey, &ipg,
			&iel,&nwds,&ietype);
	ncl_set_macptr(savptr);
	if ((ifnd != UU_SUCCESS) || (nclkey<=0))
		goto check_data;
	ur_retrieve_data_relnum(nclkey, &rel_num);
	if (rel_num==NCL_SCALAR_REL)
	{
		e.rel_num = NCL_SCALAR_REL;
		e.key = nclkey;
		status = ur_retrieve_data(&e, sizeof(struct NCL_scalar_rec));
		if (status==0)
		{
			*scalar_value = e.scalar_value;
			return 1;
		}
		else
			goto check_data;
	}
	else
		goto check_data;
check_data:
	status = ncl_get_data_value(string_sav, data_value, &type);
	if (status!=1)
		return UU_FAILURE;
	if (type!=0)
		return UU_FAILURE;
	else
	{
		*scalar_value = atof(data_value);
		return 1;
	}
}

/*********************************************************************
**    E_FUNCTION     : int ncl_parse_scalar_values(string1, string2, typ)
**       parse a string include a scalar values to convert to a string
**		into a string with the scalar value replace by values (other string
**		if not the scalar return as it is
**    PARAMETERS   
**       INPUT  : 
**          string1: string to be parse/convert
**			typ: data type
**       OUTPUT :  
**          string2: standard output string
**    RETURNS      : 
**       1: get scalar value successfully
**       0: it's all number values, no scalar value inside the string
**       -1: not a scalar value
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_parse_scalar_values(string1, string2, typ)
char *string1, *string2;
int typ;
{
	char *tok, tempstr[256], tempstr2[256], *p, string[100], string3[100];
	int stat, ret, vect;
	UU_REAL scalar_value1, scalar_value2, scalar_value3;
	char unit[20], bstr[256];
	int len;
	int error = 0;

	string[0] = '\0';
	len = strlen(string1);
	if (len == 0) return -1;
/*
......in NCL previous version, we all allow spaces inside a scalar string,
......so before we parse the string, remove all spaces inside the string
*/
	strcpy(tempstr, string1);
	ul_strip_blanks(tempstr,&len);

	vect = 0;

	if ((typ==UD_SCAVEC)||(typ==UD_SCAVEC2))
	{
/*
......we need check if it is a name of vector
*/
		stat = ncl_get_vector_string(tempstr, string2);
		if (stat==1)
			return 1;
	}
	if ((typ==0) || (typ==UD_DASCART) || (typ==UD_SCACART)
			|| (typ==UD_DASVEC) || (typ==UD_SCAVEC))
	{
		p = tempstr;
		while (*p == ' ') p++;
		if (*p=='<')
		{
			p++;
			strcpy(tempstr, p);
			p = (char *)index(tempstr,'>');
			if (p!=NULL)
				*p = '\0';
			vect = 1;
			strcat(string, "<");
		}
	}
	tok = (char*)strtok (tempstr, "\r\n,");
	ret  = 0;

	if (tok==NULL) return -1;
	strcpy(tempstr2, tok);
/*
.....remove the ending unit if have it
*/
	unit[0] = '\0';
	if ((typ==UD_DASCART) || (typ==UD_SCACART) || (typ==UD_DASDISTANCE)
		|| (typ==UD_DASANGLE))
	{
		ncl_rmend_unit (tempstr2, bstr, unit);
		strcpy(tempstr2, bstr);
	}
/*
......parse/convert the express string into value
*/
	stat = ncl_get_scalar_value(tempstr2, &scalar_value1);

	if (stat==-1)
	{
		scalar_value1 = 0;
		error = 1;
		ret = -1;
	}
	else if (stat==1)
		ret = 1;
	if ((typ==UD_DASINT) || (typ==UD_SCAINT))
		sprintf(string3, "%d", (int)scalar_value1);
	else
		sprintf(string3, "%f", scalar_value1);
	strcat(string, string3);
	strcat(string, unit);

	tok  = (char*)strtok(NULL, "\r\n,");
	if (tok==NULL)
	{
/*
.....it must three numbers on those type
*/
		if ((typ==UD_SCAVEC)||(typ==UD_SCAVEC2)||(typ==UD_DASVEC)||(typ==UD_SCACART)||(typ==UD_DASCART))
		{
			return -1;
		}
		goto done;
	}
	strcpy(tempstr2, tok);
/*
.....remove the ending unit if have it
*/
	unit[0] = '\0';
	if ((typ==UD_DASCART) || (typ==UD_SCACART)  || (typ==UD_DASDISTANCE)
				|| (typ==UD_DASANGLE))
	{
		ncl_rmend_unit (tempstr2, bstr, unit);
		strcpy(tempstr2, bstr);
	}
/*
......parse/convert the express string into value
*/
/*	stat = ncl_parse_express (tempstr2, &scalar_value2); */
	stat = ncl_get_scalar_value(tempstr2, &scalar_value2);
	if (stat==-1)
	{
		scalar_value2 = 0;
		error = 1;
		ret = -1;
	}
	else if (stat==1)
		ret = 1;
	sprintf(string3, "%f", scalar_value2);
	strcat(string, ",");
	strcat(string, string3);
	strcat(string, unit);

	tok  = (char*)strtok(NULL, "\r\n,");
	if (tok==NULL)
	{
/*
.....it must three numbers on those type
*/
		if ((typ==UD_SCAVEC)||(typ==UD_SCAVEC2)||(typ==UD_DASVEC)||(typ==UD_SCACART)||(typ==UD_DASCART))
		{
			return -1;
		}
		goto done;
	}
	strcpy(tempstr2, tok);
/*
.....remove the ending unit if have it
*/
	unit[0] = '\0';
	if ((typ==UD_DASCART) || (typ==UD_SCACART)  || (typ==UD_DASDISTANCE)
		|| (typ==UD_DASANGLE))
	{
		ncl_rmend_unit (tempstr2, bstr, unit);
		strcpy(tempstr2, bstr);
	}
/*
......parse/convert the express string into value
*/
/*	stat = ncl_parse_express (tempstr2, &scalar_value3); */
	stat = ncl_get_scalar_value(tempstr2, &scalar_value3);
	if (stat==-1)
	{
		scalar_value3 = 0;
		error = 1;
		ret = -1;
	}
	else if (stat==1)
		ret = 1;
	sprintf(string3, "%f", scalar_value3);
	strcat(string, ",");
	strcat(string, string3);
	strcat(string, unit);
	if (vect==1)
		strcat(string, ">");
	tok  = (char*)strtok(NULL, "\r\n,");
	if (tok!=NULL)
	{
/*
......it should not have more than 3 number
*/
		error = 1;
		ret = -1;
	}
done:;
	strcpy(string2, string);
	if (error==0)
		return ret;
	else
		return -1;
}		
/*********************************************************************
**    E_FUNCTION     : ncl_rmend_unit (str, bstr, unit)
**       Remove the unit string after the data string
**		
**    PARAMETERS   
**       INPUT  : str: data string with unit
**       OUTPUT :  
**					bstr: data string with unit
**					unit: unit that removed
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_rmend_unit (str, bstr, unit)
char *str, *bstr, *unit;
{
	int i, slen, len;
	char tmp_unit[20], tempstr[80], *p;

	unit[0] = '\0';
	strcpy (bstr, str);
	slen = strlen (str);
/*
.....remove trailing space
*/
	while (slen>=0)
	{
		if (str[slen]==' ')
			slen--;
		else
			break;
	}
	if (slen<=0)
		return -1;
	str[slen] = '\0';
/*
.....remove preceeding space
*/
	slen = strlen (str);
	i = 0;
	while ((str[i]==' ') && (i<slen)) i++;
	strcpy (str, &(str[i]));

	slen = strlen (str);
	for (i=0; i<8; i++)
	{
		strcpy(tempstr, str);
		strcpy(tmp_unit, UM_linear_units_name[i]);
		len = strlen (tmp_unit);
		p = (char*)strstr(tempstr, tmp_unit);
again:;
		if (p==NULL)
			continue;
/*
.....if it have character after unit, then, it is not the unit we want to find
*/
		if (*(p+len)!='\0')
		{
			p = (char *)strstr(p+len, tmp_unit);
			goto again;
		}
/*
.....if it have character charater except numbers (exclude '.' also) before
.....the unit, it is not the unit we want to find also
*/
		
		if ((tempstr - p!=0) && (*(p-1)!='.') &&(isdigit(*(p-1))==0))
			continue;
		strcpy (unit, tmp_unit);
		strncpy(bstr, str, (slen-len));
		bstr[slen-len] = '\0';
		return 1;
	}
/*
.....we need concider UM_angular_units_name too
*/
	for (i=0; i<2; i++)
	{
		strcpy(tempstr, str);
		strcpy(tmp_unit, UM_angular_units_name[i]);
		len = strlen (tmp_unit);
		p = (char*)strstr(tempstr, tmp_unit);
again2:;
		if (p==NULL)
			continue;
/*
.....if it have character after unit, then, it is not the unit we want to find
*/
		if (*(p+len)!='\0')
		{
			p = (char *)strstr(p+len, tmp_unit);
			goto again2;
		}
/*
.....if it have character charater except numbers (exclude '.' also) before
.....the unit, it is not the unit we want to find also
*/
		
		if ((tempstr - p!=0) && (*(p-1)!='.') &&(isdigit(*(p-1))==0))
			continue;
		strcpy (unit, tmp_unit);
		strncpy(bstr, str, (slen-len));
		bstr[slen-len] = '\0';
		return 1;
	}
	strcpy (bstr, str);
	return -1;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_get_data_value(in_string, data_value, type)
**       Get a data item value from a data string.
**    PARAMETERS   
**       INPUT  : 
**          in_string: data name
**       OUTPUT :  
**          data_value:	data value string
**			type: data item type
**    RETURNS      : 
**       1: get data value successfully
**       -1: not a data value
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_data_value(in_string, data_value, type)
char *in_string;
char *data_value;
int *type;
{
	struct NCL_datael_rec *dsp;
	int status, ifnd, len, label_end, sub_end, i, j,indx;
	struct NCL_datast_rec datarec;
	char label[NCL_MAX_LABEL+1], substr[20], string[NCL_MAX_LABEL+1];
	UM_int4 nclsubscript, sub;
	UM_int4 ipg,iel,nclkey;
	UM_f77_str ncllabel;
	UM_int2 nwds,ietype;
	int rel_num;
	char *savptr,*ncl_get_macptr();

	status = 1;
	*type = -1;
/*
.....get rid of preceeding space
*/
	len = strlen (in_string);
	if (len>NCL_MAX_LABEL) len = NCL_MAX_LABEL;
	strncpy(string, in_string, len);
	string[len] = '\0';
	i = 0;
	while ((string[i]==' ') && (i<len)) i++;
	strcpy (string, &(string[i]));
	i = strlen (string);
	if (i==0)
		return UU_FAILURE;
/*
.....get rid of trailing space
*/  
	while ((string[i-1]==' ') && (i>0)) i--;
	string[i] = '\0';
	len = i;

	for (i=0, j=0, label_end = 0, sub_end = 0; i<len;i++)
	{
		if ((string[i]=='[') && (label_end==0))
		{
			strncpy(label, string, i);
			label[i] = '\0';
			label_end = 1;
			continue;
		}
		else if ((string[i]==']') && (sub_end==0))
		{
			substr[j] = '\0';
			sub_end = 1;
			continue;
		}

		if (label_end==0)
			label[i] = string[i];
		else if (sub_end==0)
			substr[j++] = string[i];
		else
			return UU_FAILURE;
	}
/*
......get the subscript for data item
*/
	if (sub_end)
		nclsubscript = atoi(substr);
	else
		nclsubscript = 0;
	
	if ((i>0) && (label_end==0))
		label[i] = '\0';
/*
......we need get the subscript for data name
*/
	strcpy(string, label);
	len = strlen(string);
	for (i=0, j=0, label_end = 0, sub_end = 0; i<len;i++)
	{
		if ((string[i]=='(') && (label_end==0))
		{
			strncpy(label, string, i);
			label[i] = '\0';
			label_end = 1;
			continue;
		}
		else if ((string[i]==')') && (sub_end==0))
		{
			substr[j] = '\0';
			sub_end = 1;
			continue;
		}

		if (label_end==0)
			label[i] = string[i];
		else if (sub_end==0)
			substr[j++] = string[i];
		else
			return UU_FAILURE;
	}
/*
......get the subscript for data item
*/
	if (sub_end)
		sub = atoi(substr);
	else
		sub = 0;
	if ((i>0) && (label_end==0))
		label[i] = '\0';

	ul_to_upper(label);
	UM_init_f77_str(ncllabel, label, NCL_MAX_LABEL);
/*
......if the ncllabel is a macro, vxchk will set change the current macro pointer
......so we need save and reset it
*/
	savptr = ncl_get_macptr();
	ifnd = vxchk(UM_addr_of_f77_str(ncllabel), &sub, &nclkey, &ipg,
			&iel,&nwds,&ietype);
	ncl_set_macptr(savptr);
	data_value[0] = '\0';
	*type = -1;
	if ((ifnd != UU_SUCCESS) || (nclkey<=0))
		return UU_FAILURE;
	ur_retrieve_data_relnum(nclkey, &rel_num);
	if (rel_num==NCL_DATAST_REL)
	{
		datarec.rel_num = NCL_DATAST_REL;
		datarec.key = nclkey;
		status = ncl_retrieve_data_fixed (&datarec);
		if (status==0)
		{
			if (nclsubscript==0)
			{
				if (datarec.subscr<=0)
					strcpy(data_value, datarec.label);
				else
					sprintf(data_value, "%s(%d)", datarec.label, datarec.subscr);
				*type = -1;
			}
			else if (nclsubscript>0)
			{
				dsp = datarec.datael;
				indx = nclsubscript - 1;
				*type = dsp[indx].type;
				if (dsp[indx].type==0)
					ncl_sprintf (data_value, &(dsp[indx].value), 1);
				else if (dsp[i].type==1)
				{
					ncl_get_vocwrd((int)(dsp[indx].value), data_value);
				}
				else
				{
					strncpy(data_value, dsp[indx].label, NCL_MAX_LABEL); 
					data_value[NCL_MAX_LABEL] = '\0';
/*
.....remove trailing spaces
*/
					len = strlen(data_value);
					while ((len!=0) &&(data_value[len-1]==' ')) len--;
						data_value[len] = '\0';
					if (dsp[indx].isub>0)
						sprintf (data_value, "%s(%d)", data_value, dsp[indx].isub);
				}
			}
			return 1;
		}
		else
			return UU_FAILURE;
	}
	else
		return UU_FAILURE;
}

void ncl_get_vocwrd(value, wrd)
int value;
char *wrd;
{
	int i;
	char tok[24];
	short inum, iwrd;

	inum = (short)value;
	iwrd = 0;
	asvoc (&inum,&iwrd,tok);
	tok[6] = '\0';
/*
.....remove trailing spaces
*/
	i = strlen(tok);
	while ((i!=0) &&(tok[i-1]==' ')) i--;
		tok[i] = '\0';
	strcpy (wrd, tok);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_get_vector_string(vec_name, vec_string)
**       Get a vector value from a text string and put it into a vector string
**    PARAMETERS   
**       INPUT  : 
**          vec_name: vector name
**       OUTPUT :  
**          vec_string:	vector value string
**    RETURNS      : 
**       1: get vector value string successfully
**       0: it's a number value
**       -1: not a vector value
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_vector_string(vec_name, vec_string)
char *vec_name, *vec_string;
{
	struct NCL_vector_rec e;
	int status, ifnd, len, label_end, sub_end, i, j, not_num;
	char label[65], substr[20], string[65], string_sav[65];
	UM_int4 nclsubscript;
	UM_int4 ipg,iel,nclkey;
	UM_f77_str ncllabel;
	UM_int2 nwds,ietype;
	int rel_num;
	char *savptr,*ncl_get_macptr();

	status = 1;
/*
.....get rid of preceeding space
*/
	len = strlen (vec_name);
	if (len>NCL_MAX_LABEL) len = NCL_MAX_LABEL;
	strncpy(string, vec_name, len);
	string[len] = '\0';
	i = 0;
	while ((string[i]==' ') && (i<len)) i++;
	strcpy (string, &(string[i]));
	i = strlen (string);
	if (i==0)
		return UU_FAILURE;
/*
.....get rid of trailing space
*/  
	while ((string[i-1]==' ') && (i>0)) i--;
	string[i] = '\0';
	len = i;
	if (i==0)
		return UU_FAILURE;
/*
.....save string to use later
*/
	strcpy(string_sav, string);
	for (i=0, j=0, label_end = 0, sub_end = 0, not_num=0; i<len;i++)
	{
		if (isalpha(string[i]) || (string[i]=='#') || (string[i]=='_'))
/*
.....label string
*/
		{
			not_num = 1;
		}
		else if ((string[i]=='(') && (label_end==0))
		{
			strncpy(label, string, i);
			label[i] = '\0';
			label_end = 1;
			not_num = 1;
			continue;
		}
		else if ((string[i]==')') && (sub_end==0))
		{
			substr[j] = '\0';
			sub_end = 1;
			not_num = 1;
			continue;
		}
		else if ((isdigit(string[i])==0)&&(string[i]!='.')
			&&(string[i]!='+') && (string[i]!='-'))
		{
			return -1;
		}
		if (label_end==0)
			label[i] = string[i];
		else if (sub_end==0)
			substr[j++] = string[i];
		else
			return -1;
	}
	if (not_num==0)
	{
		return 0;
	}
	if (MSLite)
		return -1;

	if (sub_end)
		nclsubscript = atoi(substr);
	else
		nclsubscript = 0;
	if ((i>0) && (label_end==0))
		label[i] = '\0';
	ul_to_upper(label);
	UM_init_f77_str(ncllabel, label, NCL_MAX_LABEL);

	savptr = ncl_get_macptr();
	ifnd = vxchk(UM_addr_of_f77_str(ncllabel), &nclsubscript, &nclkey, &ipg,
			&iel,&nwds,&ietype);
	ncl_set_macptr(savptr);
	if ((ifnd != UU_SUCCESS) || (nclkey<=0))
		return -1;
	ur_retrieve_data_relnum(nclkey, &rel_num);
	if (rel_num==NCL_VECTOR_REL)
	{
		e.rel_num = NCL_VECTOR_REL;
		e.key = nclkey;
		status = ur_retrieve_data(&e, sizeof(struct NCL_vector_rec));
		if (status==0)
		{
			sprintf(vec_string, "%f,%f,%f", e.vec[0], e.vec[1], e.vec[2]);
			return 1;
		}
	}
	return -1;
}
