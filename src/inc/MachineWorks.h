/*********************************************************************
**    NAME         :  MachineWorks.h
**     MODULE NAME AND RELEASE LEVEL
**       MachineWorks.h , 25.3
**    DATE AND TIME OF LAST MODIFICATION
**       07/13/15 , 09:14:03
*********************************************************************/
/*
 *   MachineWorks header file 
 *
 *   This header file is required to initialise and use the MachineWorks libraries 
 * 
 * 
 *   Copyright Notice:
 *
 *      $Copyright: MachineWorks Ltd. 1990-2011, 2012$
 *      All rights reserved.
 *
 *      This software and its associated documentation contains proprietary,
 *      confidential and trade secret information of MachineWorks Limited
 *      and may not be (a) disclosed to third parties, (b) copied in any form,
 *      or (c) used for any purpose except as specifically permitted in
 *      writing by MachineWorks Ltd.
 *
 *      This software is provided "as is" without express or implied
 *      warranty.
 */

#define MWINIT_OPTIONED              /* Turn on new system of LiInitilaise */

#define MWINIT_BUILD_EXE             /* This file will be built into a EXE */

#define MWINIT_SKIP_Multicut         /* Do not install Multicut            */ 
#define MWINIT_SKIP_RenderAdvanced   /* Do not install Advanced rendering  */
#define MWINIT_SKIP_Pixelcut         /* Skips PixelCut */
#define MWINIT_SKIP_Samplecut         /* Skips SampleCut */
#define MWINIT_SKIP_RenderDirectX10   

/* Start up journaling for debug builds */
#ifdef _DEBUG
#define MWINIT_JOURNAL_FILE "nclipv.jnl"
#endif

/* The following is commented out, so we will use LI_DIR to find */
/* the messages */
/* #define MWINIT_MESSAGES_ENV_VAR "LI_DIAG_MESSAGE_FILE" */

/* enabling diagnostic handler will turn off the MW messages window */
#define MWINIT_DIAGNOSTIC_HANDLER ul_ipv_diag_handler

#include "li/mwinit.h"

