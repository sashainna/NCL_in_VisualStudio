#define MODULEDBG 0
/*********************************************************************
**    NAME         :  redatdct.c
**       CONTAINS:
**       ur_data_dict()
**       ur_data_dict_o()
**       ur_dict_refer()
**       ur_sdict_refer()
**       ur_dict_lookup()
**       ur_init_data_dict(fd)
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**
**    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**    NOTE: This module must be recompiled when isysddl.ddl
**          is modified to incorporate the changes.
**    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**
**    MODULE NAME AND RELEASE LEVEL 
**       redatdct.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:28
*********************************************************************/

#include "usysdef.h"
#include "uminmax.h"
#include "nclver.h"
#include "ribase.h"
#include "rireldef.h"
#include "riddldef.h"
#include "udebug.h"
#include "ridctdef.h"
#include "umoveb.h"

/* arrays for the data dictionary */
#include "rireldct.h"
#ifdef BUILD_32
#include "riatrdct32.h"
#else
#include "riatrdct.h"
#endif
#include "ridctver.h"

struct relblk		unDef =
	{
		"emptyRel",
		0,
		0,
		0
	};

/*********************************************************************
**    E_FUNCTION :  ur_data_dict(defind, def_lim, rel_name, rel_type)
**			look up a definition in the system data dictionary
**    PARAMETERS   
**       INPUT  : 
**          def_lim	int	max. fields in the def. (elements in array)
**          rel_name	char*	name of the relation to be found.
**       OUTPUT :  
**          defind[]	struct contains def. one field per element.
**				rel_type	int*	type of relation
**    RETURNS      : number of attributes, 0 not found, <0 error
**							(more fields than def_lim)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_data_dict(def, def_lim, rel_name, rel_type)
char					*rel_name;
struct attr_def	def[];
int					def_lim;
int					*rel_type;
{
	return(ur_dict_lookup(&UR_datadict, def, def_lim, rel_name, rel_type));
}

/*********************************************************************
**    E_FUNCTION :  ur_data_dict_o(defind, def_lim, rel_name, rel_type,
**                                 v_num,flag)
**			Look up a definition in the older version of data dictionary.
**    PARAMETERS   
**       INPUT  : 
**          def_lim	int	max. fields in the def. (elements in array)
**          rel_name	char*	name of the relation to be found.
**          v_num    int   dictionary version number.
**          flag     int   UU_TRUE if variable list packets need to be
**                         updated to the current OS.  UU_FALSE otherwise.
**                         Typically set to UU_TRUE with .ud files and
**                         UU_FALSE with binary files.
**       OUTPUT :  
**          defind[]	struct contains def. one field per element.
**				rel_type	int*	type of relation
**    RETURNS      : number of attributes, 0 not found, <0 error
**							(more fields than def_lim)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_data_dict_o(def, def_lim, rel_name, rel_type, v_num, flag)
char					*rel_name;
struct attr_def	def[];
int					def_lim, v_num;
int					*rel_type;
UU_LOGICAL			flag;
{
	int ndct,i,ofs,siz,over;
	UU_LOGICAL first;
	ndct = ur_dict_lookup
          (&UR_datadict_o[v_num], def, def_lim, rel_name, rel_type);
/*
.....The JOIN and STRING structures changed
.....during the port to 64-bits.
.....The change was made so that the data pointer
.....stays aligned on 64-bit boundaries.
.....
.....Old: {int nent; char *ptr}
.....New: {char *ptr; int nent; int padding;}
.....
.....This logic also adjusts 32-bit Joins
.....to 64-bit joins and vice versa
.....Bobby - 07/14/11
*/
	if (flag)
	{
		ofs = 0;
		first = UU_TRUE;
		for (i=0;i<ndct;i++)
		{
			if (def[i].attr_type == JOIN || def[i].attr_type == STRING)
			{
				if (NCL_infile_version < 9.950) siz = sizeof(struct UR_lpacket_99);
				else siz = sizeof(struct UR_lpacket);
				def[i].attr_here = def[i].attr_here + ofs;
				ofs = ofs + siz-def[i].attr_off;
				def[i].attr_size = def[i].attr_off = siz;
/*
.....This logic must match the logic
.....in the 'ur_data_dict_o' routine
.....in 'redatdct.c'
*/
				if (NCL_infile_version < 9.950 && first)
				{
					over = def[i].attr_here % sizeof(char *);
					ofs -= over;
				}
				first = UU_FALSE;
			}
		}
	}
	return(ndct);
}

/*********************************************************************
**    I_FUNCTION :  ur_sdict_refer(RDef, ADef, rel_name)
**			look up definition in the system dictionary, return direct reference
**			to the dictionary rather than a copy
**    PARAMETERS   
**       INPUT  : 
**				dict		ptr	the data dictionary to use
**          rel_name	char*	name of the relation to be found.
**       OUTPUT :  
**          RDef[]		pointer to the relation definition for the relation
**    RETURNS      : number of attributes, 0 not found
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_sdict_refer(RDef, ADef, rel_name)
struct relblk		*RDef[];
struct attr_def	*ADef[];
char					*rel_name;
{
	return(ur_dict_refer(&UR_datadict, RDef, ADef, rel_name));
}

/*********************************************************************
**    E_FUNCTION :  ur_dict_lookup(dict, defind, def_lim, rel_name, rel_type)
**			look up definition in the dictionary given the definition is
**			copied for dictionary integrity
**    PARAMETERS   
**       INPUT  : 
**				dict		ptr	the data dictionary to use
**          def_lim	int	max. fields in the def. (elements in array)
**          rel_name	char*	name of the relation to be found.
**       OUTPUT :  
**          defind[]	struct contains def. one field per element.
**				rel_type	int*	type of relation
**    RETURNS      : number of attributes, 0 not found, <0 error
**							(more fields than def_lim)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_dict_lookup(dict, def, def_lim, rel_name, rel_type)
struct UR_dct		*dict;
char					*rel_name;
struct attr_def	def[];
int					def_lim;
int					*rel_type;
{
	register int		i;			/* loop index */
	int					found;	/* result flag */
	struct relblk		*localRDef;
	struct attr_def	*localADef;

	uu_denter(UU_RTRC,(us,"ur_dict_lookup(%s)", rel_name));
	/* look up relation name in index */
	found = ur_dict_refer(dict, &localRDef, &localADef, rel_name);

	if (found)
	{
		if (localRDef->rellen > def_lim)
		{
			uu_dexit;
			return(-1);				/* error more fields than array holds */
		}

		/*	copy the definition for integrity */
		*rel_type = localRDef->reltyp;
		uu_move_byte(&(dict->dctattrs[localRDef->relindx]),
						def, localRDef->rellen * sizeof(struct attr_def));
		uu_dexit;
		return(localRDef->rellen);	/* return relation length */
	}
	else	/* (not found) */
	{
		uu_dprint(-1,(us,"unable to find %s",rel_name));
		uu_dexit;
		return(0);		/* return zero length for not found */
	}
}

/*********************************************************************
**    I_FUNCTION :  ur_dict_refer(dict, RDef, ADef, rel_name)
**			look up definition in the dictionary given, return direct reference
**			to the dictionary rather than a copy
**    PARAMETERS   
**       INPUT  : 
**				dict		ptr	the data dictionary to use
**          rel_name	char*	name of the relation to be found.
**       OUTPUT :  
**          RDef[]		pointer to the relation definition for the relation
**    RETURNS      : number of attributes, 0 not found
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_dict_refer(dict, RDef, ADef, rel_name)
struct UR_dct		*dict;
struct relblk		*RDef[];
struct attr_def	*ADef[];
char					*rel_name;
{
	register int	i;		/* loop index */
	UU_LOGICAL	found;	/* result flag */

	uu_denter(UU_RTRC,(us,"ur_dict_refer(%s)", rel_name));
	/* look up relation name in index */
	found = UU_FALSE;
	for (i = 0; i < dict->dctnumrels; i++)
	{
		if (strcmp(dict->dctrels[i].relnm, rel_name) == 0)
		{
			found = UU_TRUE;
#if MODULEDBG != 0
			uu_dprint(UU_RITRC,(us,"found as #%d",i));
#endif
			break;
		}
	}

	if (found)
	{
		*RDef = &(dict->dctrels[i]);		/* set the reference to this rel */
		*ADef = &(dict->dctattrs[(*RDef)->relindx]);
		uu_dexit;
		return(dict->dctrels[i].rellen);	/* return relation length */
	}
	else	/* (not found) */
	{
		*RDef = &unDef;	/* set reference to undefined rel */
		*ADef = 0;
		uu_dprint(-1,(us,"unable to find %s",rel_name));
		uu_dexit;
		return(0);			/* return zero length for not found */
	}
}

/*********************************************************************
**    I_FUNCTION :  ur_init_data_dict(fd)
**       performs the initialization of the data dictionary by
**			parsing the uniddl file passed to it.
**    PARAMETERS   
**       INPUT  : 
**          fd			int	file descriptor returned by ur_popen().
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

#define MAXFIELD 64

ur_init_data_dict(fd)
int	fd;
{
	uu_denter(UU_RTRC,(us,"ur_init_data_dict"));
	/* now compiled in -- nothing to do */
	uu_dexit;
	return(0);
}

