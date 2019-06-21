/********************************************************************* 
**  NAME:  wsxw.h
**
**      GKS workstation: X-Window data definitions section.
**
**  COPYRIGHT  1992  Numerical Control Computer Sciences
**  MODULE NAME AND RELEASE LEVEL 
**       wsxw.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:23
**
**  PARAMETERS   
**      INPUT:  none 
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/

#if UU_COMP != UU_WIN2K
#ifdef EXT
#undef EXT
#endif
#ifdef MFPGM
#define EXT
#else
#define EXT extern
#endif

EXT int BLACK,WHITE,BLUE,RED,GREEN,MAGENTA,YELLOW,CYAN,BROWN,CTAN;
EXT int LTBLUE,LTGREEN,ORANGE,PINK,PURPLE,GREY,DEFAULT,DKBLUE;

/*
.....Basic information structure for
.....X-window driver
*/
typedef struct
{
	Display *disp;		/* virtual display id */
	Window wd_id;		/* display window id */
	int screen_no;		/* active screen number */
	int dev_xmax;		/* display width */
	int dev_ymax;		/* display height */
} UG_Gxwdat;

EXT UG_Gxwdat uw_xw;
#undef EXT
#endif
