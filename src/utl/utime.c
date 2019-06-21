/*********************************************************************
**    NAME         :  tutime.c
**       CONTAINS:
**         uu_udatime(sdatime)   return 29byte ascii date time
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       utime.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:55
*********************************************************************/
/*********************************************************************
**    I_FUNCTION :  uu_udatime( sdatime)
**       Get system date and time string of the form:
**       "DOW MMM DD HH:MM:SS TMZ YYYY\n" (like the UNIX date command)
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          char sdatime[29]   ascii date time format
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_udatime( sdatime)
char   *sdatime;
{
char *ctime();
long clock;
char *atime;

/*-------------- begin function code -------------------------------*/
clock = time((long *) 0);
atime = ctime(&clock);
 strncpy(sdatime,atime,20);      /* move DOW MMM DD HH:MM:SS */
	*(sdatime+20) = '\0';           /* terminate the string */
 strncat(sdatime,(atime+19),5);  /* add YYYY year */
}
