/********************************************************************* 
**  NAME:  fastimmed.h 
**
**      Unicad version of sgi fastimmed.h.  Includes two modified
**			versions of sgi include files.  The modifications just
**			comment out some unused defines so that our code will
**			compile.
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       fastimmed.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:15
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/

#define UNIX
#define IP2
#define DC4
#define UC4
#include "gl2/globals.h"
#include "gl2/gltypes.h"
#include "imdraw.h"				/* Unicad's own stripped down version */
#include "imattrib.h"			/* Unicad's own stripped down version */
#include "gl2/immed.h"
#include "gl2/imsetup.h"
#include "gl2/glerror.h"
