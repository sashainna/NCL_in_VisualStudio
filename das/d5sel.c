/*********************************************************************
**
**    NAME         :  d5sel.c
**
**       CONTAINS:
**          ud_allpoints  
**          ud_alldisp
**         ud_chain
**         ud_chain_modals
**         ud_region
**         ud_region2
**         ud_prmsel
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       d5sel.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:12
**
*********************************************************************/
/*
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) d5sel.c 2.2 9/30/85 14:49:55 single"};
#else
static char uu_sccsident[]={"@(#) d5sel.c 2.2 9/30/85 14:49:55 double"};
#endif
*/
#include "usysdef.h"
#include "nclfc.h"
#include "dasnog.h"
#include "dselmask.h"
#include "drubber.h"
#include "view.h"
#include "umath.h"
#include "dselect.h"
#include "mdrel.h"
#include "mdpick.h"
#include "dmark.h"
#include "udebug.h"
#include "uhep.h"
#include "zsysdep.h"
#include "ginq.h"
#include "lumb.h"
#include "mdcpln.h"

#include "tzdcoord.h"
#include "class.h"
#include "mfort.h"
#include "mdunits.h"

#define UD_NULLPICKID -1      /* invalid pick id */
#define ENDSEG -1
#define UD_POLYMARKER 0
#define UD_POLYLINE    1
#define UD_TEXT      2
#define UD_FILLAREA    3
#define SEGNO  picker.ppath.pickpath[picker.ppath.depth - 2]

extern UU_LOGICAL UD_hilite;
/*
.....
.....This variable added to be used in the um_cceqcc_ch(...)
.....model/m7mathco.c. Paul.
.....
*/
#include "modef.h"
double toler = UM_FUZZ;

/* 
.....
.....These include files to make "ud_form1" call work. Paul
.....
*/

#include "mattr.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include <ctype.h>

#include "nccs.h"

/*
.....
.....This variabe used in ud_chain with ud_form1 call. It made GLOBAL
.....for this file becouse we need to know it's value in the modat()
.....at the end of this file. Paul.
.....
*/

static int planar;

static int Add_mask[UD_NMENTWD]; /* Keeps result mask for enabled/disabled ent.*/
static int All_mask[UD_NMENTWD];
static double plan[4];           /* Keeps converted values of requested PLAN. */
int cond;                 /* Condition: 0 - Normal; 1-Conditional  */

static int S_save_modfile();
int UD_from_chain = 0;

/*********************************************************************
**
**    E_FUNCTION        :  ud_allpoints()
**       all points select handler in select subsystem
**
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : adds entities to global select list
**    WARNINGS     : none
**
*********************************************************************/

ud_allpoints()
{
	extern UD_FILTREC UD_filter;  /* attribute filter */
	UD_FILTREC save_filter;
	int i;

/* temporarily change select filter, all ud_alldisp() with filtering,
**  and then restore the select filter to its original state */

	uu_denter(UU_DTRC,(us,"entering ud_allpoints()"));

  /* save current filter */
	zbytecp(save_filter, UD_filter);

/* -- set filter to include only points -- */

	UD_filter.layer_state = UD_INACTIVE;
	UD_filter.color_state = UD_INACTIVE;
	UD_filter.linestyle_state = UD_INACTIVE;
	UD_filter.pen_state = UD_INACTIVE;
	UD_filter.entity_state = UD_INCLUDE;
	UD_filter.marker_state = UD_INACTIVE;

/* -- clear entity filter -- */

	for (i=0; i<UD_NENTITY_WORDS; i++)
		UD_filter.f_entity[i] = 0;

/* -- set the points bit -- */

	uu_set_bit(UD_filter.f_entity, UM_POINT_REL - 1);

/* -- now call all displayed with filtering and no hiliting-- */

	ud_alldisp(UU_TRUE, UU_FALSE, UU_TRUE);

/* -- restore original filter -- */

	zbytecp(UD_filter, save_filter);
	uu_dexit;
	return 0;
}
    
/*********************************************************************
**
**    E_FUNCTION        :  ud_alldisp(filter_on, hiflag, detflag)
**       all displayed select handler in select subsystem
**
**    PARAMETERS   
**       INPUT  :  filter_on = UU_TRUE, UU_FALSE
**             hiflag = if UU_TRUE, then hightlite entities
**             detflag = if UU_TRUE, then make entities undetectable
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : adds entities to global select list
**    WARNINGS     : none
**
*********************************************************************/
    
ud_alldisp(filter_on, hiflag, detflag)
UU_LOGICAL filter_on;      /* attribute filtering? */
UU_LOGICAL hiflag;        /* hilite entity? */
UU_LOGICAL detflag;        /* make entity undetectable */
{
	Gnpoint center;              /* center of pick window */
	Gfloat dx,dy;                /* half lengths of pick window */
	UV_vport vport;              /* vport info */
	Gnrect3 *nrect;              /* ndc rect for vport */
	Gnpoint3	sav_llf;
	Gnpoint3	sav_urb;
	UV_view view;

	int i, vis = 0;                /* select only visible entities */

	int selmode = UD_SELECTXIN;      /* select any entities which are
                                visible in viewport */
	int markval;                /* mark system cell */
	UU_LOGICAL UD_hilite_save;        /* save hilite state cell */
	UU_LOGICAL UD_det_save;          /* save detectablility state cell */
	void ud_init_select_buffer();

	uu_denter(UU_DTRC,(us,"entering ud_alldisp, filter=%d, hilite=%d, det=%d",
      filter_on, hiflag, detflag));

/*  -- initialze selct buffer if needed -- */
  
	ud_init_select_buffer();

  /* -- have operator select a viewport - calc center,dx,dy -- */
again:;
	if (ud_getpick_type()==UD_PICK_SECONDARY)
	{
/*
.....there should only one active vports for secondary view
*/
		for (i=0; i<UV_act_screen[0].nvports; i++)
		{
			uv_getvpid(UV_act_screen[0].vports[i], &vport);
			uv_getvid(vport.cur_view, &view);
			if (view.vtype==UV_SECONDARY_VIEW)
			{
				break;
			}
		}
	}
	else
	{
		if ((uvu_pickvp(&vport)) == UU_FAILURE)
		{
			uu_dprint(UU_DTRC,(us,"ud_alldisp uvu_pickvp NOT OK")); 
		}
	}
/*
.....check if the viewport is the valid viewport corrending to
.....the picking type
*/
	if (!ud_verifypick_type(&vport,UU_TRUE)) goto again;
	nrect = gqvport3(vport.xform);
	center.x= (nrect->urb.x + nrect->llf.x) / 2.0;
	center.y=(nrect->urb.y + nrect->llf.y) / 2.0;
	dx = (nrect->urb.x - center.x);
	dy = (nrect->urb.y - center.y); 
	uu_dprint(UU_DTRC,(us,"ud_alldisp center %f %f dx dy %f %f",center.x,
		center.y,dx,dy)); 

	UD_MARK(markval, UU_FALSE);
	if(markval == 0)
	{
/*   -- call region2() to put entities in select buffer -- */

		UD_hilite_save = UD_hilite;
		UD_hilite = hiflag;
		UD_det_save = UD_Sellim_mp;
		UD_Sellim_mp = detflag;
		ud_region2(&center, dx, dy, selmode, filter_on, vis);
	}
/*  -- activate hiliting again -- */

	UD_hilite = UD_hilite_save;
	UD_Sellim_mp = UD_det_save;

	UD_UNMARK(markval);
	uu_dexit;
	return 0;
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_chain(filter_on)
**       chain select handler in select subsystem
**
**    PARAMETERS   
**       INPUT  :  filter_on = UU_TRUE, UU_FALSE
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : adds entities to global select list
**    WARNINGS     : none
**
*********************************************************************/

ud_chain(filter_on)
UU_LOGICAL filter_on;            /* attribute filtering? */
{
  UU_REAL (*cord_buf)[3];        /*  polyline return buffer */
  UU_REAL next_cord[3];          /*  next endpoint to find */
  UU_REAL pend_cord[3];          /*  possible end coordinate 
                                     (if closed loop selected) */
  UD_PLOCREC picker;             /* pickloc record */
  int len;                       /* length return cell */
  int i, j, k, n;
  int count;                     /*  length of gks segment */
  int type;                      /*  type of gks segment */
  UU_LOGICAL done;               /*  done flag */
  UU_LOGICAL found;              /*  found next chained entity flag */
  UU_KEY_ID mtid;                /*  mtid number for picked item */
  int lastmtid;
  int locdepth;
  int baseptr;                   /* save pointer for select stack */
  uv_segbuff(udata);             /* buffer for segment user data */
  UU_KEY_ID viewkey;             /* "legal" view key for chain */
  UU_KEY_ID segkey;              /* view key for each segment */
  int tmpbits[UD_NMENTWD];       /* temporary bit array buffer */
  UU_LOGICAL tmpflag;            /* geometry buffer limit flag */
  UU_LOGICAL  ud_limsel();
/*
.....
.....Added by Paul for "CONDITIONAL CHAIN"
.....
*/

    typedef struct
    {
    UU_KEY_ID mtid;
    UU_REAL   tmp_cord[3];
    int       depth;
    int       pickpath[5];
    } STACK;
 
    static STACK stk_buf[50];
    static int   stk_ptr;

    UM_transf tfmat;
    struct UC_attributedatabag attr;
/*
... aak 10-nov-1997: replaced
    struct UC_entitydatabag  e;
*/
    struct NCL_fixed_databag e;

    UM_PLOCREC pent;
    UM_PICKENT ent_lst;

    int numint, stat;

    int markval;
    
    int ii,ist,ien;

/*
....
....The end of my additional definitions. Paul
....
*/
  uu_denter(UU_DTRC,(us,"entering ud_chain"));

/*
.....
.....Added by Paul.
.....
*/
UD_MARK(markval,UU_FALSE);
if(markval == 0)
{
/*
.....
.....We need this call to set up all defaults if we did not call
.....ud_chain_modals from SELECT menu before calling this routine.
.....
*/
	UD_from_chain = 1;
	ud_chain_modals(UU_TRUE); 
	
/*  -- set the reject unselect pointer -- */

  UD_Selrej_ptr = UD_Select_ptr;
  UD_Selrej_cnt = UD_Select_cnt;

/*  -- get the user defined geometry limit info. Propogate limit if 
    indicated -- */

  ud_inqgeo(&tmpflag, tmpbits);
  if(tmpflag == UU_TRUE)
  {
    for (i=0; i<UD_NMENTWD; i++)
/*
....Added to disallow lines/circles/bslines/comp. curves. Old version:
....tmpbits[i] = UD_allcurves[i] & tmpbits[i];
*/
/*		tmpbits[i] = (UD_allcurves[i] & tmpbits[i]) & (~Add_mask[i]);*/
		tmpbits[i] = tmpbits[i] & All_mask[i] & (~Add_mask[i]);
  }
  else
  {
    for (i=0; i<UD_NMENTWD; i++)
/*
....Added to disallow lines/circles/bsplines/comp. curves. Old version:
....tmpbits[i] = UD_allcurves[i];
*/
/*		tmpbits[i] = UD_allcurves[i] & (~Add_mask[i]);*/
		tmpbits[i] = All_mask[i] & (~Add_mask[i]);
  }

/*  -- limit the DAS to splines, lines, and arcs -- */

  ud_lgeo(UU_TRUE, tmpbits);

  /*  -- pick start of chain -- */

pick_it_up:;
  ud_ldas(UD_DASPCKLOC, UD_DASHEP, 57, &picker, 1,  &len, UD_NODEFAULT);
/*
.....
.....This call is necessary if we pick something like comp. curv.
.....In this case ud_ldas returns info about subentity and the
.....following call corrects this problem.
.....
*/
stat = um_d_pickresolve(&picker,1,&ent_lst);
picker.ppath.pickpath[picker.ppath.depth-1] = ent_lst.key[0];

  uu_dprint(UU_DTRC,(us,"in ud_chain, ploc = (%g,%g)",
        picker.pndc.cord[0],picker.pndc.cord[1]));

/*  -- restore DAS select state -- */

  if(len < 1)
  {
    goto fini;
  }

/* -- get viewkey for chain -- */

  gsegrud(SEGNO,udata);
  viewkey = uv_getviewkey(udata);
  uu_dprint(UU_DTRC,(us,"ud_chain: viewkey %d",viewkey));

/*  -- get the endpoint of the entity picked -- */

  um_d_endpts(1, &picker.ppath, &picker.pndc, &type, next_cord, pend_cord);

/*  -- if item picked is a point then point select, else coincident
    select -- */

  if(type != UM_POINT_REL)
  {

/*    -- COINCIDENT SELECT -- */

/*    -- filter this entity through the DAS limit select mask and 
      attribute filter -- */

    if(ud_limsel(SEGNO, filter_on, UU_NULL) == UU_TRUE)
    {

/*      -- make sure it is a polyline -- */

/*      -- get the segment -- */

      gtrav(SEGNO);
      gtravnxt(&type, &count, &cord_buf, picker.ppath.pickpath,
              &picker.ppath.depth);
/*
.....
.....We have to call this routine to get real coordinates and key
.....for entity. gtevnxt returns coord. and the key of subentity
.....in case, for instance, comp. curve. Paul.
.....
*/
          um_d_travresolve(&picker.ppath,&cord_buf[0][0],&cord_buf[1][0]);
          count = 2;


      if(type == UD_FILLAREA)
      {
        uu_uerror0(UD_DASHEP, 96);
        goto fini;
      }

     if((planar == 1)&&
        (um_layonpln(&picker.ppath.pickpath[picker.ppath.depth-1],
         &plan[0])!=UU_TRUE))
     {
        ud_wrerr("The entity is not on the plane. Retry. [More info in HELP.]");
        goto pick_it_up;
     }


/*  -- store in pick buffer -- */

      UD_Select_cnt++;
      baseptr = UD_Select_ptr;
      UD_Select_buf[UD_Select_ptr] = picker.ppath.depth;
      UD_Select_ptr++;
      for(i=0; i<picker.ppath.depth; i++)
      {
        UD_Select_buf[UD_Select_ptr] = picker.ppath.pickpath[i];
        UD_Select_ptr++;
      }
  
/*     -- set last mtid so we don't pick this entity again. -- */

      lastmtid = UD_Select_buf[UD_Select_ptr - 1];

/*      --  highlight and set entity not pickable -- */

      ud_spstat(UU_FALSE, baseptr);

      uu_dprint(UU_DTRC,(us,"ud_chain it, lastm=%d, Sbuf=(%d,%d,%d)",
        lastmtid, UD_Select_buf[0],UD_Select_buf[1], UD_Select_buf[2]));

      uu_dprint(UU_DTRC,(us,"chain it, p_cord=(%g,%g,%g), n_cord=(%g,%g,%g)"
        ,pend_cord[0],pend_cord[1], pend_cord[2],
        next_cord[0],next_cord[1], next_cord[2]));

/*****************************************************
          MAIN LOOP
*****************************************************/

      done = UU_FALSE;
      while(done == UU_FALSE)
      {

/*        --  Rewind segment file -- */

        gtrav(-1);
        found = UU_FALSE;
/*
....
....Initiation of pointer for stk_buf. Paul.
....
*/
                stk_ptr = 0;
/**/

        uu_dprint(UU_DTRC,(us,"calling gtrav"));

/*        --  get the endpoints of the next segment -- */

        while(found == UU_FALSE && done == UU_FALSE)
        {

          type = UD_POLYMARKER;
          while(type == UD_POLYMARKER)
            gtravnxt(&type, &count, &cord_buf, 
              &UD_Select_buf[UD_Select_ptr+1],
              &UD_Select_buf[UD_Select_ptr]);
/*
.....
.....We have to call this routine to get real coordinates and key
.....for entity. gtevnxt returns coord. and the key of subentity
.....in case, for instance, comp. curve. Paul.
.....
*/
          if( type != ENDSEG)
          {
               um_d_travresolve(&UD_Select_buf[UD_Select_ptr],
                           &cord_buf[0][0],&cord_buf[1][0]);
               count = 2;
          }

          uu_dprint(UU_DTRC,(us,"ud_chain at gtravnxt, type=%d, count=%d",
                type, count));

/*          -- if EOF then selection complete -- */

          if(type == ENDSEG || (cond == 0 && stk_ptr != 0))
          {
            if(cond == 0 && stk_ptr != 0) stk_ptr = 1;
/*
....
....This section is changed according with changes made for section
...."if ud_limsel == UU_TRUE". Paul.
....
*/
            if (stk_ptr == 0)
            {
              done  = UU_TRUE;
              found = UU_TRUE;
            }
            else if(stk_ptr == 1)
            {
              for (i=0; i < 3; i++)
                  next_cord[i] = stk_buf[stk_ptr-1].tmp_cord[i];
              
              baseptr = UD_Select_ptr;
              UD_Select_buf[UD_Select_ptr]=stk_buf[stk_ptr-1].depth;
              
              for(i=0; i<stk_buf[stk_ptr-1].depth; i++)
                  UD_Select_buf[UD_Select_ptr+1+i]=stk_buf[stk_ptr-1].pickpath[i];             
              UD_Select_ptr = UD_Select_ptr + UD_Select_buf[UD_Select_ptr]+1;
              UD_Select_cnt++;
            
              lastmtid = UD_Select_buf[UD_Select_ptr-1];
 
              ud_spstat(UU_FALSE,baseptr);
         
              done = um_cceqcc_ch(next_cord,pend_cord);
              found= UU_TRUE;
              
              stk_ptr=0;
            }
            else if(stk_ptr > 1)
            {
              for(i=stk_ptr-1; i >= 0; i--)
              {
                e.key = stk_buf[i].mtid;
                ncl_retrieve_data_fixed(&e);
                uc_retrieve_attr(e.key,&attr);
                uc_retrieve_transf(e.key,tfmat);
                attr.color = UM_DARKRED;
                ist = 1;
                ien = UV_act_screen[0].nvports;
                for (ii=ist;ii<=ien;ii++)
                {
                   ug_sntran(ii);
                   uc_draw(&e,tfmat,&attr); 
                }

              }
              while (1)
              {
               stat =  
               ud_ldas(UD_DASPCKLOC,UD_DASHEP,57,&pent,1,&numint,UD_NODEFAULT);
/*
.....
.....This call is necessary if we pick something like comp. curv
.....
*/
               stat = um_d_pickresolve(&pent,1,&ent_lst);
               pent.pent.key[pent.pent.num-1] = ent_lst.key[0];

               for (i=stk_ptr-1; i>=0; i--)
                  if(pent.pent.key[pent.pent.num-1] == stk_buf[i].mtid) goto cont;

               ud_wrerr("Attempted pick was unsuccessful.  Retry.  [More info in HELP.]"); 
              }
cont:;
              for(j=stk_ptr-1; j >= 0; j--)
              {
                e.key = stk_buf[j].mtid;
                ncl_retrieve_data_fixed(&e);
                uc_retrieve_attr(e.key,&attr);
                uc_retrieve_transf(e.key,tfmat);
                ist = 1;
                ien = UV_act_screen[0].nvports;
                for (ii=ist;ii<=ien;ii++)
                {
                   ug_sntran(ii);
                   uc_draw(&e,tfmat,&attr);
                }

              }

              for (j=0; j < 3; j++)
                  next_cord[j] = stk_buf[i].tmp_cord[j];

              baseptr = UD_Select_ptr;
              UD_Select_buf[UD_Select_ptr]=stk_buf[i].depth;

              for(j=0; j<stk_buf[i].depth; j++)
                  UD_Select_buf[UD_Select_ptr+1+j]=stk_buf[i].pickpath[j];

              UD_Select_ptr = UD_Select_ptr + UD_Select_buf[UD_Select_ptr]+1;
              UD_Select_cnt++;

              lastmtid = UD_Select_buf[UD_Select_ptr-1]; 

              ud_spstat(UU_FALSE,baseptr);

              done = um_cceqcc_ch(next_cord,pend_cord);
              found= UU_TRUE;

              stk_ptr=0;
            }
          }

          else
          {
            /*  -- check the start and end points for coincidence -- */

            if(count > 1)
            {
              j = UD_Select_ptr;
              uu_dprint(UU_DTRC,(us,"UD_Select_buf=(%d,%d,%d)", 
                UD_Select_buf[j], UD_Select_buf[j+1], 
                UD_Select_buf[j+2]));

              /* -- make sure this seg is in correct view -- */

              locdepth = UD_Select_buf[UD_Select_ptr];
              gsegrud(UD_Select_buf[UD_Select_ptr+locdepth-1],udata);
              segkey = uv_getviewkey(udata);
              uu_dprint(UU_DTRC,(us,"ud_chain: segno %d segkey %d",
              UD_Select_buf[UD_Select_ptr+locdepth-1], segkey));

              if (segkey == viewkey)
              {

                /*  -- make sure we are not looking at last selected entity -- */
                mtid = UD_Select_buf[UD_Select_ptr+locdepth]; 

                uu_dprint(UU_DTRC,(us,"mtid=%d,lastmtid=%d",mtid,lastmtid));

                if(mtid != (UU_KEY_ID)lastmtid)
                {
/*
.....
.....Make sure we not count this entity twice if it is something like
.....full circle.
.....
*/
                  for(j=0, k=count-1; j<count && found == UU_FALSE;
                    j=j+count-1, k=k-count+1)
                  {

                    /*  -- If end is coincident to other end of last 
                    entity, then save -- */

                    uu_dprint(UU_DTRC,(us,"cord=(%g,%g,%g)", 
                      cord_buf[j][0], cord_buf[j][1], 
                      cord_buf[j][2]));
                    uu_dprint(UU_DTRC,(us,"next_cord=(%g,%g,%g)", 
                      next_cord[0], next_cord[1], next_cord[2]));
                    uu_dprint(UU_DTRC,(us,"cceqcc return=%d",
                      um_cceqcc_ch(next_cord, &(cord_buf[j][0]))));

                    if((um_cceqcc_ch(next_cord, &(cord_buf[j][0])) == UU_TRUE)
                  &&(((planar == 1)&&(um_layonpln(&mtid,&plan[0])==UU_TRUE))||
                        planar == 0))
                    {

                      /*  -- filter this entity through the DAS limit 
                      select mask -- */

                      if(ud_limsel(UD_Select_buf[UD_Select_ptr + 
                        locdepth - 1], filter_on, UU_NULL) == UU_TRUE)
    
                      /*  -- found an endpoint - save other end -- */
                      {
/*
....
....Make sure we do not have this entity in the stack yet. This can
....happend if we work with entity like full circle (start point &
....end point are the same);
....
*/
                       for (n = 0; n < stk_ptr; n++)
                           if(stk_buf[n].mtid == mtid) goto skip;

                       stk_buf[stk_ptr].mtid = mtid;

                       for(i=0; i<3; i++)
                          stk_buf[stk_ptr].tmp_cord[i] = cord_buf[k][i];

                       stk_buf[stk_ptr].depth = UD_Select_buf[UD_Select_ptr];

                       for(i=0; i<stk_buf[stk_ptr].depth; i++)
                          stk_buf[stk_ptr].pickpath[i]=UD_Select_buf[UD_Select_ptr+1+i];
                       lastmtid = mtid;

                       stk_ptr++;

skip:;
                      } /* end of if ud_limsel == UU_TRUE */

                    } /* end of if um_cceqcc_ch == UU_TRUE */

                  } /* end of for j=0 ... */

                } /* end of if mtid != lastmtid */

              } /* end of if segkey == viewkey */

            } /* end of if count > 1 */

          } /* end of else (type != ENDSEG) */

        } /* end of while found == UU_FALSE && done == UU_FALSE */

      } /* end of while done == UU_FALSE */

    } /* end of if ud_limsel == UU_TRUE */

  } /* end of if type != UM_POINT_REL */

fini:;
  uu_dexit;
} /* end of markval == 0 */
else
{
  for(i=stk_ptr-1; i >= 0; i--)
  {
     e.key = stk_buf[i].mtid;
     ncl_retrieve_data_fixed(&e);
     uc_retrieve_attr(e.key,&attr);
     uc_retrieve_transf(e.key,tfmat);
     ist = 1;
     ien = UV_act_screen[0].nvports;
     for (ii=ist;ii<=ien;ii++)
     {
        ug_sntran(ii);
        uc_draw(&e,tfmat,&attr);
     }

     
  }
  stk_ptr = 0;

} /* end of markval != 0 */
UD_UNMARK(markval);
return 0 ;
}

#define FEATURE -1
#define NOTHING -2
#define VECTOR   -3
#define PICKARPETURE (UU_REAL) .01
#define FMODE 3
#define PICKPTR (*event).indata.pickdata

/*********************************************************************
**
**    E_FUNCTION         :  ud_region(selmode,filter_on)
**       region select handler in select subsystem
**
**    PARAMETERS   
**       INPUT  : 
**          selmode = UD_SELECTIN, UD_SELECTOUT, UD_SELECTXIN, UD_SELECTXOUT
**        filter_on = UU_TRUE, UU_FALSE
**      OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : adds entities to global select list
**    WARNINGS     : none
**
*********************************************************************/

ud_region(selmode,filter_on)
int selmode;                  /* in, out, or cross */
UU_LOGICAL filter_on;            /* attribute filtering? */
{
  int retcount;
  UD_NDCLOCREC pnt1, pnt2;        /* window corners */
  int vis = 0;                /* pick only visible entities */
  Gnpoint center;              /* center of window */
  Gfloat dx,dy;                /* length, width of window */
  UD_RUBBER rubber;              /*  rubber box control block */

  uu_denter(UU_DTRC,(us,"entering ud_region"));

  /*  -- get first rectangle corner -- */

restart:
  ud_ldas(UD_DASNDC, UD_DASHEP, 58, &pnt1, 1, &retcount, UD_NODEFAULT);
  if(retcount < 1)
    goto done;

  /*  -- get second rectangle corner -- */

  RUBBER_BOX(&rubber);
  RUBBER_ON(&rubber);
  ud_ldas(UD_DASNDC, UD_DASHEP, 59, &pnt2, 1, &retcount, UD_NODEFAULT);
  RUBBER_OFF(&rubber);
  if(retcount < 1)
    goto done;

  /*  -- test to see if the points are in the same viewport -- */

  if(pnt1.transform != pnt2.transform)
  {

    /*  -- points not in same viewport -- */

    uu_uerror0(UD_DASHEP, 78);
    goto restart;
  }

  /* compute center, width, length of window */
  center.x = (pnt1.cord[0] + pnt2.cord[0]) / 2;
  center.y = (pnt1.cord[1] + pnt2.cord[1]) / 2;
  dx = fabs((pnt1.cord[0] - pnt2.cord[0]) / 2);
  dy = fabs((pnt1.cord[1] - pnt2.cord[1]) / 2);

  uu_dprint(UU_DTRC,(us,"ud_region center %g %g dx dy %g %g",
    center.x,center.y,dx,dy));

  /* -- call region2 to fill select buffer -- */

  ud_region2(&center,dx,dy,selmode,filter_on,vis);
done:
  uu_dexit;
  return 0;
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_region2(center,dx,dy,selmode,filter_on,vis)
**       part 2 of region select handler in select subsystem
**
**    PARAMETERS   
**       INPUT  : 
**        *center = center of region
**        dx, dy = half length, width of region
**          selmode = UD_SELECTIN, UD_SELECTOUT, UD_SELECTXIN, UD_SELECTXOUT
**        filter_on = UU_TRUE or UU_FALSE
**        vis = 0,1,2: visible,invisible,both
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : adds entities to global select list
**    WARNINGS     : none
**
*********************************************************************/

ud_region2(center,dx,dy,selmode,filter_on,vis)
Gnpoint *center;                /* center of pick window */
Gfloat dx, dy;                  /* half length of select window */
int selmode;                   /* in, out, or cross */
UU_LOGICAL filter_on;            /* attribute filtering? */
int vis;                      /* get vis or invis entities */
{

  int retcount, count, wdcount;    /* input counters */
  int baseptr;
  UV_vport vport;
  UU_KEY_ID viewkey;
  int stat;

  uu_denter(UU_DTRC,(us,"entering ud_region2"));

  /* -- need viewkey for OUT, XOUT -- */

  if (selmode == UD_SELECTOUT || selmode == UD_SELECTXOUT)
  {
    stat = uv_pickvp(center->x, center->y, &vport);
    viewkey = vport.cur_view;
	if (!ud_verifypick_type(&vport,UU_TRUE)) return 0;
    uu_dprint(UU_DTRC,(us,"ud_region2: center %f %f viewkey %d stat %d",
      center->x, center->y, viewkey, stat));
  }
  else
/*
.....if the selection is for views other than normal view (invisible, external)
.....we have to select the correct view
*/
  {
    stat = uv_pickvp(center->x, center->y, &vport);
	if (!ud_verifypick_type(&vport,UU_TRUE)) return 0;
    viewkey = 0;
  }
  /*  -- set the reject unselect pointer -- */

  UD_Selrej_ptr = UD_Select_ptr;
  UD_Selrej_cnt = UD_Select_cnt;

  /*  -- now get the selected items -- */

  count = UD_Selbuf_size - UD_Select_ptr;
  baseptr = UD_Select_ptr;

  gfindreg2(selmode, center, dx, dy, &UD_Select_buf[UD_Select_ptr],
    count, &retcount, &wdcount, vis);

/*  -- check for buffer overflow -- */

/*-----------------------------------
  if(count >= retcount - 3)
  {
    uu_uerror0(UD_DASHEP, 98);
    goto done;
  }
-------------------------------------*/

  /*  -- update the total number of entities picked -- */

  UD_Select_ptr = UD_Select_ptr + wdcount;
  UD_Select_cnt = UD_Select_cnt + retcount;

  /*  -- filter entities excluded by DAS limit or attribute filter -- */

  ud_limsss(baseptr, filter_on, viewkey);

  /*  -- set the entities as unpickable -- */

  ud_spstat(UU_FALSE, baseptr);

  uu_dprint(UU_DTRC,(us,"in ud_region2, count=%d, buffer=%d %d %d",
    UD_Select_ptr, UD_Select_buf[0], UD_Select_buf[1], UD_Select_buf[2]));

  uu_dexit;
  return 0 ;
}
    
/*********************************************************************
**
**    E_FUNCTION        :  ud_prmsel()
**       single select with operator verify
**
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : adds entities to global select list
**    WARNINGS     : none
**
*********************************************************************/

ud_prmsel()
{
  UU_LOGICAL status;
  UU_LOGICAL ud_lyesno();
  int baseptr;
  UD_PLOCREC ploc;
  int numint;
  int i;
  int segno;
  int transform;          /* transformation number of this pick query */
  Gqpicks pick;          /* pick record */
  UU_REAL x, y;          /* location of pick */
  UU_LOGICAL first;        /* first time flag */

  uu_denter(UU_DTRC,(us,"entering ud_prmsel"));

/*  -- set the reject unselect pointer -- */

  UD_Selrej_ptr = UD_Select_ptr;
  UD_Selrej_cnt = 0;

/*  -- indicate entity by location -- */

  ud_ldas(UD_DASPCKLOC, UD_DASHEP, 65, &ploc, 1, &numint, UD_NODEFAULT) ;
  if (numint > 0)
  {
    first = UU_TRUE;
    x = ploc.pndc.cord[0];
    y = ploc.pndc.cord[1];
    transform = ploc.pndc.transform;
    pick.depth = ploc.ppath.depth;
    pick.pickpath = ploc.ppath.pickpath;
    pick.status = UG_OK;
    baseptr = UD_Select_ptr;
    for(;;)
    {
/*  --   go get the new pick id -- */

      if(first == UU_FALSE)
        gpckm(x, y, &pick, (UU_REAL) .01, transform); 
      else
        first = UU_FALSE;

      if(pick.status == UG_OK)
      {

        UD_Select_buf[UD_Select_ptr] = pick.depth;
        for(i=0; i<pick.depth; i++)
          UD_Select_buf[UD_Select_ptr+i+1] = pick.pickpath[i];

        baseptr = UD_Select_ptr;
        segno = UD_Select_buf[UD_Select_ptr+1];

        UD_Select_ptr = UD_Select_ptr + UD_Select_buf[UD_Select_ptr] + 1;
        UD_Select_cnt++;
        UD_Selrej_cnt++;

/*    -- set items unpickable -- */

        gssegdet(segno, UG_UNDETECTABLE);
        gsseghilite(segno, UG_HIGHLIGHTED);
      }
      else
      {

/*        -- pick was unsuccesful -- */

        uu_uerror0(UD_DASHEP, 22);
        ud_reject(UD_Selrej_ptr, UD_Selrej_cnt);
        goto done;
      }

/*  --    Correct entity ?? -- */

      status = ud_lyesno(UD_DASHEP, 66);
      if(status == UU_TRUE)
      {

/* --      found the desired entity -- */

        ud_spstat(UU_TRUE, UD_Selrej_ptr);

        UD_Select_buf[UD_Selrej_ptr] = pick.depth;
        for(i=0; i<pick.depth; i++)
          UD_Select_buf[UD_Selrej_ptr+i+1] = pick.pickpath[i];

/* --      collapse select buffer -- */

        UD_Select_ptr = UD_Selrej_ptr + UD_Select_buf[UD_Selrej_ptr] + 1;
        UD_Select_cnt = UD_Selrej_cnt + 1;

        segno = pick.pickpath[0];
        gssegdet(segno, UG_UNDETECTABLE);
        gsseghilite(segno, UG_HIGHLIGHTED);
        goto done;
      }
      else
      {
        gsseghilite(segno, UG_NORMAL);
      }
    }
  }

done:
  uu_dexit;
	return 0 ;
}


/*********************************************************************
**
**    E_FUNCTION         :  modat()             
**
*********************************************************************/
static UD_FSTAT  modat(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
    ud_default_method(fieldno, val, stat);

    switch(*fieldno)
    {
       case 2: 
            if(planar)
                ud_set_traverse_mask(3, UU_TRUE);
            else
                ud_set_traverse_mask(3, UU_FALSE);
            break;
       default:
            uu_dprint(UU_MTRC,(us," Bad field number: %d",*fieldno));
    }
    uu_dexit;
	 return(stat);
}
/*********************************************************************
**
**    E_FUNCTION         :  ud_chain_modals()  
**       changes the list of conditions for ud_chain().
**
**    PARAMETERS
**       INPUT  :  
**       OUTPUT :
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
ud_chain_modals(default_only)
UU_LOGICAL default_only;
{
	int j,status;
	static char plan_char[64];             /* "planar"(def. at the top of   */
	static int lines, circles, comp_curves;/* answers from the form.        */
	static int bsplcrv,msurf; 
	static double tol, surftol;
	static int vectors; 
	static int direction; 
	UM_int2 primtyp;
	UM_real8 primdata[16];
	
	UM_vector vec;
 
    UM_f77_str f77_str; 


	int idummy;
	static char traverse[]     = {1, 1, 1, 1, 1, 1, 1, 1,1,1,1,1};
	static UD_METHOD methods[] = {modat,modat,modat,modat,modat,
	                              modat,modat,modat, modat,modat,modat,modat};
	static char called[]       = {6, 6, 6, 6, 6, 6, 6, 6, 6,6,6,6};
	int *ans[12];
	struct NCL_nclpl_rec  e1;
	int rel_num,i,stat;
/*
.....Set defaults for 'chain' form .
.....Setting values from mod file.
*/

	cond = UD_chain_mod.conditional;    
	tol = UD_chain_mod.toler;
	planar = UD_chain_mod.planar;          
	strcpy(plan_char,UD_chain_mod.plane);
	lines = UD_chain_mod.lines;  
	circles = UD_chain_mod.circles;
	comp_curves = UD_chain_mod.composites;    
	bsplcrv = UD_chain_mod.splines; 
	surftol = UD_chain_mod.surftoler;
	vectors = UD_chain_mod.vectors;    
	direction = UD_chain_mod.direction;
	msurf = UD_chain_mod.mult_surf;

	ans[0] = &cond;
	ans[1] = (int *)&tol;
	ans[2] = &planar;
	ans[3] = (int *)&plan_char[0];
	ans[4] = &lines;
	ans[5] = &circles;
	ans[6] = &bsplcrv;
	ans[7] = &comp_curves;
	ans[8] = (int *)&surftol;
	ans[9] = (int *)&vectors;
	ans[10] = (int *)&direction;
	ans[11] = (int *)&msurf;
/*
.....Get the Form input
*/

form:;
	if (planar)
	 {
		traverse[3] = 1;
		methods[3] = modat;
	 }
	 else
	 {
		traverse[3] = 0;
		methods[3] = NULL;
	 }

	if(!default_only)
	{
/*
.....Limit picking to planes and planar surfaces only
*/
		ud_lgeo (UU_TRUE, UD_ncl_pl);
		status = ud_form1("chain.frm", ans, ans, methods, called, NULL, traverse);
		ud_unlimit();
		if (status==-1)
			return -1;
	}
/*
.....Set the global variables to the form values.
*/
	UD_chain_mod.conditional = cond;
	UD_chain_mod.toler = tol;
	toler = UD_chain_mod.toler;
	UD_chain_mod.planar = planar;
	strcpy(UD_chain_mod.plane,plan_char);   
	UD_chain_mod.lines = lines;
	UD_chain_mod.circles = circles;
	UD_chain_mod.composites = comp_curves;
	UD_chain_mod.splines = bsplcrv;
	UD_chain_mod.surftoler = surftol;
	UD_chain_mod.vectors = vectors;
	UD_chain_mod.direction = direction;
	UD_chain_mod.mult_surf = msurf;
/*
.....Get  the plan values or name (in case of the wrong conversion to
.....the number. Do it if "planar" == 1 (*Yes) only.
*/
    if (planar == 1)
    {
      /* for ( i=0; plan_char[i] != 0; i++)
         if(plan_char[i] == ' ') plan_char[i]=0;
*/
      idummy = 4;
      stat = ul_to_reals(plan,&idummy,4,plan_char);
      if(stat != UU_SUCCESS || idummy != 4)
      {
         for ( i=0; plan_char[i] != 0; i++)
            plan_char[i]=islower(plan_char[i]) ? toupper(plan_char[i]):
                                                 plan_char[i];

          UM_init_f77_str(f77_str,plan_char,i);
			getkey(UM_addr_of_f77_str(f77_str), &e1.key);
         if (e1.key == 0)
         {
            ud_wrerr("The Chain selection plane does not exist.");
				if (UD_from_chain)
				{
					UD_chain_mod.planar = 0;
					planar = 0;
					UD_from_chain = 0;
					goto nonplanar;
				}
				else
            	goto form;
         }
         else
         {
            ur_retrieve_data_relnum(e1.key, &rel_num);
            if (rel_num != NCL_PLN_REL && rel_num != UM_RBSPLSRF_REL &&
					rel_num != NCL_SURF_REL && rel_num != NCL_TRIMSF_REL)
            {
					ud_wrerr("Selected entity is not a plane");
					if (UD_from_chain)
					{
						UD_chain_mod.planar = 0;
						planar = 0;
						UD_from_chain = 0;
						goto nonplanar;
					}
					else
						goto form;
				}
/*
......allow picking of planar surfaces too
*/
				if (rel_num == UM_RBSPLSRF_REL || rel_num == NCL_SURF_REL ||
					rel_num == NCL_TRIMSF_REL)
				{
				   status = ncl_get_sf_primdat(&e1.key,&primtyp,primdata);
				   if (primtyp == 3) 
				   {
					   for(j=0;j<4;j++)
						   plan[j]=primdata[j];
				   }
				   else
				   {
						ud_wrerr("Selected entity is not a plane");
						if (UD_from_chain)
						{
							UD_chain_mod.planar = 0;
							planar = 0;
							UD_from_chain = 0;
							goto nonplanar;
						}
						else
							goto form;
					}
				}
				else
				{
					ncl_retrieve_data_fixed(&e1);
					plan[0] = e1.nvec[0];
					plan[1] = e1.nvec[1];
					plan[2] = e1.nvec[2];
					plan[3] = e1.nvec[0]*e1.pt[0] + e1.nvec[1]*e1.pt[1]
							 +e1.nvec[2]*e1.pt[2];
				}
         }
           
      }
		vec[0] = plan[0];
		vec[1] = plan[1];
		vec[2] = plan[2];
		if(um_mag(vec) == 0)
		{
   		ud_wrerr(" Invalid Plane.");
   		goto form;
		}
    }
nonplanar:;
/*
....Select appropriate mask for lines, circles, comp. curves
*/
    for (i=0; i<UD_NMENTWD; i++)
    {
/*
.....Only allow entities which are valid for
.....chain selection
.....Bobby  -  10/19/99
*/
		All_mask[i] = UD_line[i] | UD_circle[i] | UD_rbspline[i] | UD_bspline[i] |
			UD_conics[i] | UD_ncl_cv[i] | UD_ncl_cvonsf[i] | UD_compcrv[i];

      Add_mask[i] = 0;
      if(lines == 0)       Add_mask[i] = Add_mask[i] | UD_line[i];
      if(circles == 0)     Add_mask[i] = Add_mask[i] | UD_circle[i];
      if(bsplcrv == 0)     Add_mask[i] = Add_mask[i] | UD_rbspline[i];
      if(bsplcrv == 0)     Add_mask[i] = Add_mask[i] | UD_bspline[i];
      if(bsplcrv == 0)     Add_mask[i] = Add_mask[i] | UD_conics[i];
      if(bsplcrv == 0)     Add_mask[i] = Add_mask[i] | UD_ncl_cv[i];
/*
... aak 31-oct-1997: added uv curves on surfaces
*/
      if(bsplcrv == 0)     Add_mask[i] = Add_mask[i] | UD_ncl_cvonsf[i];
      if(comp_curves == 0) Add_mask[i] = Add_mask[i] | UD_compcrv[i];
    }
/*
.....Store modals
*/
	if(!default_only)
		S_save_modfile();
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : S_save_modfile
**       Save the NCL Chain Modals properties into modals file.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : UU_FAILURE if could not save modals file,  UU_SUCCESS
**                   otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static int S_save_modfile()
{
   int stat;
   char msg[80];
	UU_REAL tol;
	UX_pathname fname;
   FILE *fptr;
	static char chaindir[4][10] = {"*UV","*PREVIOUS","*PERPTO","*EDGE"};
	static char chaincond[2][10] = {"*NORMAL","*CONDTN"};
	static char inex[2][10] = {"*EXCLUDE","*INCLUDE"};
/*
.....changes to array of 10 chars instead of 4 for end of string
*/
   static char yesno[2][10] = {"*NO","*YES"};
/*
.....Initialize routine
*/
   stat = UU_SUCCESS;
/*
.....Open modals file
*/
	strcpy(fname,"ncl_chain.mod");
   stat = ul_open_mod_file("UU_USER_SETTINGS","modals",UU_NULL,UU_NULL,fname,
		3,&fptr);
   if (stat != UU_SUCCESS || fptr == UU_NULL) goto done;
/*
.....Store modals
*/
   ux_fputs0("#CHAIN#\n", fptr);
   sprintf(msg,"/CHAIN/ %s\n",chaincond[UD_chain_mod.conditional]);
   ux_fputs0(msg, fptr);

	UM_len_inttoext(UD_chain_mod.toler,tol);
	sprintf(msg,"/TOL/ %f%s\n",tol, UM_linear_units_name[UM_cpln.length_unit]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/PLANAR/ %s\n",yesno[0/*UD_chain_mod.planar*/]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/PLANE/ %s\n",UD_chain_mod.plane);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/LINES/ %s\n",inex[UD_chain_mod.lines]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/CIRCLES/ %s\n",inex[UD_chain_mod.circles]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/BSPLINES/ %s\n",inex[UD_chain_mod.splines]);
	ux_fputs0(msg, fptr);
	
	sprintf(msg,"/COMPCURVES/ %s\n",inex[UD_chain_mod.composites]);
	ux_fputs0(msg, fptr);
	
	UM_len_inttoext(UD_chain_mod.surftoler,tol);
 	sprintf(msg,"/SURFTOL/ %f%s\n", tol, UM_linear_units_name[UM_cpln.length_unit]);
 	ux_fputs0(msg, fptr);

	sprintf(msg,"/VECTORS/ %d\n",UD_chain_mod.vectors);
	ux_fputs0(msg, fptr);
	
	sprintf(msg,"/DIRECTION/ %s\n",chaindir[UD_chain_mod.direction]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/MULTI_PICK/ %s\n",yesno[UD_chain_mod.mult_surf]);
	ux_fputs0(msg, fptr);

/*
.....Close modals file
*/
   ux_fclose0 (fptr);
/*
.....End of routine
*/
done:
   return(stat);
}
