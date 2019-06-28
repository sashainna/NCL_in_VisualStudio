/*********************************************************************
**    NAME         :  glnstyle.c
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       glnstyle.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:20
*********************************************************************/

#include <stdio.h>
#include "umath.h"
#include "udebug.h"
#include "g.h"
#include "usysdef.h"
#include "dinput.h"
#include "gobas.h"

#define UU_SPEC UU_REAL
/* 32768  4194304 */
#define SCALE(p)     p
#define UN_SCALE(p)  p


static char test_pattern[] = {1};
#define PATLEN (UU_REAL) 100000.

typedef struct {
	UU_SPEC pos;
	int	state;
	} transition;

static transition *t=0;
static transition *next;
static int			next_trans;
static int			ntrans;
static UU_SPEC		pattern_length;
static int			current_state;
static UU_SPEC		current_pos;

extern UU_LOGICAL T_PLOT;
UU_REAL line_scale;

#define ug_test_pattern(){ug_set_pattern(1,test_pattern,PATLEN);}

static int init=0;
ug_set_pattern(style)
	Glntype *style;
	{
	UU_SPEC bit_width;
	int i,j,tmp;
	Gchar typepatn[200];
	int npatn=0;
	int start;

	uu_denter(-1,(us,"ug_set_pattern(%d,?,%f)",style->npatn,style->patnlen));
	init = 1;

	for(i=style->npatn-1;i>=0;i--)
		if(style->typepatn[i] == 0) break;
	start = i+1;
	for(i=start;i<style->npatn;i++)
		{
		typepatn[npatn++] = style->typepatn[i];
		typepatn[npatn++] = style->typepatn[i];
		}
	for(i=0;i<start;i++)
		{
		typepatn[npatn++] = style->typepatn[i];
		typepatn[npatn++] = style->typepatn[i];
		}
	for(i=0;i<npatn;i++) if(typepatn[i] == 0) break;
	tmp = i/2;
	for(j=tmp,i=0;j<npatn;j++,i++)
		{
		typepatn[i]=typepatn[j];
		}
	for(;i<npatn;i++) typepatn[i] = 1;
	for(i=0;i<npatn;i++)
		uu_dprint(UU_GITRC,(us,"typepatn[%d] = %d", i, typepatn[i]));
	if (t != 0) uu_toolfree(t);

	t = (transition *) uu_toolmalloc((npatn+1)*sizeof(transition));

	pattern_length = SCALE(style->patnlen);
	bit_width = SCALE(style->patnlen / npatn);
	ntrans = 0;

	for (i=0 ; i < npatn ; ) {
		t[ntrans].pos   = bit_width * i;
		t[ntrans].state = (typepatn[i] ? 1 : 0);
		uu_dprint(-1,(us,"tran %g,state %d\n",
				UN_SCALE(t[ntrans].pos),t[ntrans].state));
		ntrans++;

		for (i++ ; (i < npatn) 
						&& (typepatn[i] == typepatn[i-1]) ; i++);
		}

	t[ntrans].pos   = bit_width * npatn;
	t[ntrans].state = -1;
	uu_dprint(-1,(us,"tran %g,state %d\n",
			UN_SCALE(t[ntrans].pos),t[ntrans].state));
	ntrans++;

	ug_reset_pattern();

	uu_dexit;
	}

ug_reset_pattern() {
	next_trans = 1;
	next = &t[1];
	current_state = t[0].state;
	current_pos = 0;
	}

		
ug_lnstyle(point,n,lines)
	Gnpoint3 point[2];
	int *n;
	Gnpoint3 **lines;
	{
	Gnpoint3 *line;
	int i;
	UU_REAL linelen, delx, dely, delz;
	UU_REAL lscale;
	UU_SPEC llen;
	int ipattern;
	UU_SPEC running_pos;
	int prev_state;

	uu_denter(-1,(us,"line from %g %g %g to %g %g %g\n",
			point[0].x,point[0].y,point[0].z,point[1].x,point[1].y,point[1].z));

	if (! init) ug_test_pattern();
/*
.....If we are plotting we need to adjust the linelen depending
.....on the papersize used and the machine.  line_scale is
.....is set in utp_set_dashlen. JLS 9/1/99
*/
	if(T_PLOT == UU_TRUE)
		lscale = line_scale;
	else 
		lscale = 1.0;

	delx = point[1].x - point[0].x;
	dely = point[1].y - point[0].y;
	delz = point[1].z - point[0].z;
	linelen = lscale*sqrt((delx*delx)+(dely*dely)+(delz*delz));

	/* NEED to test for linelen=0, and return if so. */
	if (linelen==0) {
		*n = 1;
		line = *lines = (Gnpoint3 *)uu_toolmalloc(2*sizeof(Gnpoint3));
		line[0].x = point[0].x; line[0].y = point[0].y; line[0].z = point[0].z;
		line[1].x = point[1].x; line[1].y = point[1].y; line[1].z = point[1].z;
		uu_dprint(UU_GITRC,(us,"linelen=0. returning."));
		uu_dexit;
		return;
	}

	llen = SCALE(linelen);
	delx /= linelen;
	dely /= linelen;
	delz /= linelen;

	ipattern = linelen / (UN_SCALE(pattern_length)) + 1;
	uu_dprint(UU_GITRC,(us,"ug_lnstyle: ipattern = %d",ipattern));

	line= *lines= (Gnpoint3 *)uu_toolmalloc(ipattern*(ntrans+1)*sizeof(Gnpoint3)*2);
	*n = 0;

	running_pos = 0;
	prev_state = 0;

	while (llen > 0) {
		if (current_state == 1)
			if (prev_state == 1) {
				line--;
				(*n)--;
				}
			else {
				line->x = UN_SCALE(running_pos);
				line++;
				(*n)++;
				}

		if ((current_pos+llen) < next->pos) { /* segment from cp to cp+llen */
			current_pos += llen;
			running_pos += llen;

			llen = 0;

			if (current_state == 1) {
				line->x = UN_SCALE(running_pos);
				line++;
				(*n)++;
				}
			}
		else { /* make a segment from current pos to next->pos */
			llen -= (next->pos - current_pos);
			running_pos += (next->pos - current_pos);

			if (current_state == 1) {
				line->x = UN_SCALE(running_pos);
				line++;
				(*n)++;
				}

			current_pos = next->pos;
			prev_state = current_state;
			current_state = next->state;
			next_trans = next_trans % (ntrans-1) + 1;
			next = &t[next_trans];
			if (current_state == -1) {
				current_pos = 0;
				current_state = t[0].state;
				}
			}
		}

		

		/* convert distance from start point to actual point */
	for (line=(*lines),i=0 ; i < *n ; line++,i++) {
		line->z = line->x * delz + point[0].z;
		line->y = line->x * dely + point[0].y;
		line->x = line->x * delx + point[0].x;
		}
	*n /= 2;

/*****

	for each transition segment do
		if segment is on
			if segment-1 was also on then
				change the end point of the last line added
				to be the end point of this segment
			else
				add a new line segment to line list
		else
			ignore all the other records
	
*****/
	uu_dexit;
	}
