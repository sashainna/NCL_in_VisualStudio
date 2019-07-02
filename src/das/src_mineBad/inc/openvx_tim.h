/*********************************************************************
**    NAME         :  openvx_tim.h
**       CONTAINS: VX structures.
**    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**        openvx_tim.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**        04/29/15 , 15:06:40
*********************************************************************/
/* ID:obi * DATE::950428082525 * PROJECT: Fixes to OPENVX */
/*    Copyright Varimetrix Corporation
*/

#ifndef __VX_APP
#ifndef __openvx_tim_h
#define __openvx_tim_h
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

extern int TASK_NAME;
extern int TASK_FROM;

/********** error structure ***************************************************/

/* system string length (including the null character) */
#define SYS_STRG_LEN 80

/* Define error structure to be passed from task to task */
typedef struct _task_err_struc
   {
   int err;
   int task;
   int cmd_code;
   int line_num;
   char func_name[80];
   int severity;
   char message[256];
   } task_err_struc;

/* system string structure */
typedef struct _sys_strg
   {
   char s[SYS_STRG_LEN];
   } sys_strg;

#ifdef __cplusplus
}
#endif /* __cplusplus */
#endif /* __openvx_tim_h */
#endif /* __VX_APP */
