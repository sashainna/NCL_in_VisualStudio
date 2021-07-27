#ifdef UU_IPV
/*********************************************************************
** FILENAME: lmiconf.c
** CONTAINS:   Contains MachineWorks initialization/termination calls,
**             and the MachineWorks.h function definitions.
**     MODULE NAME AND RELEASE LEVEL
**       lmiconf.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       06/17/15 , 13:22:31
*********************************************************************/
#include "MachineWorks.h"
#include "li/mwauthnt.h"

LiInitialiseLocal()
{
	LiInitialise(authenticate);
}

LiTerminateLocal()
{
	LiTerminate();
}
#endif
