/*********************************************************************
**    NAME         :  rienvtab.c
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 14:29:47  2015
*********************************************************************/
#include "rienvtab.h"
#include "riallmod.h"
struct UR_env_table_rec UR_environ_table[] = {
{"UM_attrmdl", (char *)&UM_attrmdl, (char *)&UM_new_attrmdl, sizeof(struct UM_attrmdl_rec)},
{"UM_mtrlmdl", (char *)&UM_mtrlmdl, (char *)&UM_new_mtrlmdl, sizeof(struct UM_mtrlmdl_rec)},
{"UM_dispattr", (char *)&UM_dispattr, (char *)&UM_new_dispattr, sizeof(struct UM_dispattr_rec)},
{"UM_drwmdl", (char *)&UM_drwmdl, (char *)&UM_new_drwmdl, sizeof(struct UM_drwmdl_rec)},
{"UM_labelmdl", (char *)&UM_labelmdl, (char *)&UM_new_labelmdl, sizeof(struct UM_labelmdl_rec)},
{"UR_unimod", (char *)&UR_unimod, (char *)&UR_new_unimod, sizeof(struct UR_unimod_rec)},
{"", 0, 0, 0}
};
