/*********************************************************************
**  NAME:  SwModals.h
**
**       Implementation of NCL/Solid modal class functions.
**
** CONTAINS: SwModals class functions
**
**    COPYRIGHT 2003 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**			SwModals.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:06:08
*********************************************************************/
#include "ustdio.h"
#include "files.h"
#include "nclxunib.h"

class CMod
{
private:
	
public:
	int open_mod_file(char *env_dir,char *file, FILE *fptr);
	int label_options(char modal[81],char param[81]);
	int load_bound_crvs(char modal[81],char param[81]);
	int load_shade_surf(char modal[81],char param[81]);
	int load_cv_option(char modal[81],char param[81]);
	int load_name_data(char modal[81],char param[81]);
	int load_level_no(char modal[81],char cmsg[81]);
	int load_match_layer(char modal[81],char param[81],int j);
	int load_match_color(char modal[81],char param[81],int j);
	int load_sec_label(char modal[81],char param[81]);
	int load_reg_match(char modal[81],char param[81]);
	int load_start_unmatch(char modal[81],char param[81]);
	int load_sketch_data(char modal[81],char param[81]);
};

static CMod *mod_this;
