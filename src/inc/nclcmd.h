
/*********************************************************************
**    NAME         :  nclcmd.h
**       CONTAINS: This file contains C structure definitions for
**							mapping data between NCL and UNIBASE representations.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nclcmd.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:34
*********************************************************************/
#ifndef NCLCMD_H
#define NCLCMD_H
/*
.....MAXPICK defines limit of entities to be picked via the SELECT subsystem.
.....Currently limited by max. num of curves from which to build a composite
.....curve.
*/
#define NCL_MAXPICK 100
#define NCL_MAX_COMLINE 1024
typedef struct
	{
	int active;
	int num_cmd;
	int cur_cmd;
	int max_cmd;
	char cur_str[NCL_MAX_COMLINE];
	char cmd[25][NCL_MAX_COMLINE];
	} NCL_cmdbuf;
#endif
