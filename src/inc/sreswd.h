/*********************************************************************
**    NAME         : sreswd.h 
**       SALEX reserved word table 
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       sreswd.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:52
*********************************************************************/

#ifndef SRESWDH


#include "syacc.h"		/* token define's from YACC */ 
#include "sxreswd.h"		/* Extra reserved word def's */ 

struct t_rw
{
	 char		*rw_word;			/* name of reserved word */
	 int		rw_value;			/* token types */
};

static struct t_rw
sal_reswds[] = {
"abs",			SABS,								
"all",			SALL,								
"and",			SAND,								
"angle",			SANGLE,							
"append",		SAPPEND,							
"at",				SAT,							
"auto",			0,
"begin",			SBEGIN,							
"bit",			SBIT,				
"break",			0,
"by",				SBY,
"character",	SCHARACTER,
"cl",				SCL,										
"class",			SCLASS,										
"cm",				SCM,										
"case",			SCASE,								
"constants",	SCONSTANTS,
"continue",		SCONTINUE,
"copy",			SCOPY,
"coord",			SCOORD,				
"cross",			SCROSS,							
"cylndr",		SCYLNDR,							
"default",		0,
"delete",		SDELETE,
"disp",			SDISP,									
"dist",			SDIST,									
"do",				SDO,									
"dot",			SDOT,								
"double",		0,
"downto",		SDOWNTO,
"else",			SELSE,								
"empty",			SEMPTY,
"endif",			SENDIF,							
"enddo",			SENDDO,									
"endenum",		SENDENUM,									
"endfor",		SENDFOR,									
"endpacked",	SENDPACKED,						
"endproc",		SENDPROC,						
"endrecord",	SENDREC,	
"endselect",	SENDSEL,
"endvariant",	SENDVAR,				
"enum",			SENUM,				
"exit",			SEXIT,								
"extern",		0,
"external",		SEXTERNAL,					
"false",			SFALSE,							
"float",			0,
"for",			SFOR, 								
"fortran",		SFORTRAN,						
"free",			SFREE,
"from",			SFROM,
"goto",			SGOTO,								
"if",				SIF,									
"in",				SIN,									
"include",		SINCLUDE,
"insert",		SINSERT,
"int",			0,
"integer",		SINTEGER,						
"len",			SLEN,						
"list",			SLIST,						
"logical",		SLOGICAL,						
"long",			0,
"mag",			SMAG,								
"message",		SMESSAGE,								
"methods",		SMETHODS,								
"mod",			SMOD,								
"module",		SMODULE,								
"nodisp",		SNODISP,
"not",			SNOT,								
"object_key",	SOBJECT_KEY,						
"object_data",	SOBJECT_DATA,						
"of",				SOF,									
"or",				SOR,									
"otherwise",	SOTHERWISE,
"packed",		SPACKED,
"pop_list",		SPOPLIST,
"private",		SPRIVATE,
"procedure",	SPROCEDURE,				
"procedures",	SPROCEDURES,				
"read",			SREAD,
"real",			SREAL,								
"record",		SRECORD,						
"register",		0,
"return",		SRETURN,						
"returns",		SRETURNS,						
"rot",			SROT,						
"sal",			SSAL,
"save",			SSAVE,								
"scale",			SSCALE,								
"select",		SSELECT,
"send",			SSEND,
"set",			SSET,
"short",			0,
"sizeof",		SSIZEOF,
"sort",			SSORT,
"spher",			SSPHER,
"static",		0,
"stop",			SSTOP,
"string",		SSTRING,
"struct",		0,
"super",			SSUPER,
"switch",		0,
"then",			STHEN,								
"to",				STO,									
"transf",		STRANSF,	
"true",			STRUE,								
"types",			STYPES,
"typedef",		0,
"union",			0,
"unit",			SUNIT,								
"unsigned",		0,
"until",			SUNTIL,							
"var",			SVAR,								
"variables",	SVARIABLES,								
"variant",		SVARIANT,						
"void",			0,
"while",			SWHILE,
"write",			SWRITE,
"xor",			SXOR,

/* DDL Section Reserved Words */
"create",			SCREATE,
"ddl",				SDDL,
"domain",			SDOMAIN,
"field_group",		SFLDGRP,
"join",				SJOIN,
"key_id",			SKEYID,
"table",				STABLE,

/* UNIBASE Section Reserved Words */
"attribute",		SATTRIBUTE,
"classes",			SCLASSES,										
"c_header_output",SCHDROUT,
"c_struct_names",	SCSTRUCT,
"data",				SDATA,
"endclass",			SENDCLASS,							
"expansion_size",	SEXPSIZE,
"initial_size",	SINITSIZE,
"message",			SMESSAGE,
"messages",			SMESSAGES,
"methods",			SMETHODS,
"relations",		SRELATIONS,
"subclass",			SSUBCLASS,
"temporary",		STEMPORARY,
"unibase",			SUNIBASE,

"",					0,	 				/* to stop the search */
};


#define SRESWDH
#endif
