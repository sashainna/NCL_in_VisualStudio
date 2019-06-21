/*********************************************************************
**  NAME:  OgModals.h
**
**       Implementation of NCL/Solid modal class functions.
**
** CONTAINS: OgModals class functions
**
**    COPYRIGHT 2003 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**			OgModals.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:06:06
*********************************************************************/
#include "ustdio.h"
#include "files.h"
#include "nclxunib.h"

class OgModals
{
private:
	
public:
	int og_open_mod_file(char *env_dir,char *file, FILE *fptr);
	int og_label_options(char modal[81],char param[81]);
int og_load_bound_crvs(char modal[81],char param[81]);
int og_load_shade_surf(char modal[81],char param[81]);
int og_load_cv_option(char modal[81],char param[81]);
int og_load_name_data(char modal[81],char param[81]);
int og_load_level_no(char modal[81],char cmsg[81]);
int og_load_match_layer(char modal[81],char param[81],int j);
int og_load_match_color(char modal[81],char param[81],int j);
int og_load_sec_label(char modal[81],char param[81]);
};

static OgModals *og_this;
