/*********************************************************************
**    NAME         :  utime.h
**       CONTAINS:
**			structures: timeval timezone tm
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       utime.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:06
*********************************************************************/

#ifndef UTIMEH

/*
 * Structure returned by gettimeofday(2) system call,
 * and used in other calls.
 */
struct timeval {
	long	tv_sec;		/* seconds */
	long	tv_usec;	/* and microseconds */
};

struct timezone {
	int	tz_minuteswest;	/* minutes west of Greenwich */
	int	tz_dsttime;	/* type of dst correction */
};

/*
 * Structure returned by gmtime and localtime calls (see ctime(3)).
 */
struct tm {
	int	tm_sec;
	int	tm_min;
	int	tm_hour;
	int	tm_mday;
	int	tm_mon;
	int	tm_year;
	int	tm_wday;
	int	tm_yday;
	int	tm_isdst;
};

#ifndef KERNEL
extern	struct tm *gmtime(), *localtime();
extern	char *asctime(), *ctime();
#endif

#define UTIMEH
#endif
