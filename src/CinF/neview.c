/*********************************************************************
**    NAME         :  neview.c
**       CONTAINS:
**           int rstvu (keys)
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       neview.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:56
*********************************************************************/

#include "usysdef.h"
#include "mdrel.h"
#include "mfort.h"
#include "view.h"
#include "nclfc.h"

/*********************************************************************
**    E_FUNCTION     : int rstvu (keys)
**       Reset a list of views.
**    PARAMETERS   
**       INPUT  : 
**          keys       - Array of view keys to reset, ending in zero.
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
rstvus (keys)
   UM_int4 *keys;
   {
   int status, i;
   int next_tupleid;
   UM_int4 *keyp;
   UU_KEY_ID key;
   UU_LOGICAL done;
   UV_vport  vport;
   UV_view  view;

   status = UU_SUCCESS;
/*
...   Retrieve key of each view in unibase
*/
   next_tupleid = 1;
   while (ur_get_next_data_key(UV_VIEW_REL, &next_tupleid, &key) > -1)
      {
      next_tupleid++;
/*
...   If this view is in the in the list, reset it 
*/
      for (keyp = keys, done=UU_FALSE; *keyp > 0 && !done; keyp++)
        if (*keyp == key)
        {
        done = UU_TRUE;
        if (uv_getvid(key, &view) == UU_SUCCESS) uv_vrestore (&view);
/*
...   If this view is in the current screen, redisplay it.
*/
        for (i=0; i<UV_act_screen[0].nvports; i++)
          {
          uv_getvpid(UV_act_vports[0][i].key, &vport);
          if (key == vport.cur_view) uv_updatevp(&vport, UU_TRUE);
          }
        }
      }

   return (status);
   }
