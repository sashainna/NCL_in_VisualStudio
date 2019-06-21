C*********************************************************************
C*    NAME         :  lathes.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       lathes.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:14
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine lathes
c*       this routine parses the lathe statement.  it sets up the jbrx array
c*       and stores the parsed form into the next available ranfil record(s).
c*       sc(10) contains the header for the statement. the firsti*2 of sc(10)
c*       has a 700 (ist for lathe).  the second i*2 is a zero for finish, and a
c*       1 for a rough lathe statement.  the third i*2 contains the number of
c*       real*8 words in the jb array describing the total lathe statement.
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
C
c****************************************************************************
       subroutine lathes

       include 'com8a.com'
       integer*2 itemp (4),i2bld(4), jbinx, jbx(144)
       real*4 r4bld(2)
       real*8 r8bld,jbrx(36),temp
       equivalence (r8bld,r4bld,i2bld)
       equivalence (jbrx,jbx),(itemp,temp)
       logical depth,cutang

       depth=.false.
       cutang=.false.
       if (ifl(142).eq.0) then
           call error(235)
           go to 99999
       endif
       isc10(1)=ist
       ifl(44)=9
       call parsit
       if (ityp.eq.1.and.ist.eq.320) then
c                                           ****    rough
           isc10(2)=1
           call parsit
       else
c                                           ***     finish
           isc10(2)=0
           if (ityp.eq.1.and.ist.eq.323) call parsit
       endif

       if (ityp.eq.2.and.ist.eq.18) then
           jbrx(1)=tv
           call parsit
       else
           call error(236)
           go to 99999
       endif
c                             ***   cldist
       if (ityp.eq.1.and.ist.eq.1077.and.isc10(2).eq.1) then
           call parsit
           if (ityp.eq.2.and.ist.eq.2.or.ityp.eq.3.or.ityp.eq.4) then
               jbrx(2)=tv
               call parsit
           else
               call error(7)
           endif
       else
           jbrx(2)=0
       endif

       if (isc10(2).eq.1) then
           if (ityp.eq.2.and.ist.eq.18) then
               jbrx(3)=tv
               call parsit
           else
               call error(236)
               go to 99999
           endif
       endif
c                        ***  stock
       jbrx(4)=0
       jbrx(5)=0
       if (ityp.eq.1.and.ist.eq.321) then
           call parsit
           if (ityp.eq.2.and.ist.eq.2.or.ityp.eq.3.or.ityp.eq.4) then
               jbrx(4)=tv
               call parsit
           else
               call error(7)
               go to 99999
           endif
           if (ityp.eq.2.and.ist.eq.2.or.ityp.eq.3.or.ityp.eq.4) then
               jbrx(5)=tv
               call parsit
           else
               jbrx(5)=jbrx(4)
           endif
       endif
       jbinx=5
c                        ***   get the rest of the clauses in any order
c
100    r8bld=0
       if (ityp.eq.1.and.(ist.gt.999.or.ist.eq.5)) then
c                        ***  its a major post word
           i2bld(1)=2000
           if (nextyp.eq.5) then
c                skip over the '/'
               inx=inx+1
           else
               i2bld(2)=ist
               i2bld(3)=1
               jbinx=jbinx+1
               jbrx(jbinx)=r8bld
               call parsit
               go to 100
           endif
           i2bld(2)=ist
           jbinx=jbinx+1
c                       ***   save the index
           isv=jbinx
           icount=1
           do 150,i=1,50
           call parsit

           if (ityp.eq.2.and.ist.eq.2.or.ityp.eq.3.or.ityp.eq.4) then
               jbinx=jbinx+1
               jbrx(jbinx)=tv
               icount=icount+1
               call parsit
               go to 151
           else if (ityp.eq.1.and.ist.lt.160.and.ist.ne.7.and.
     1          ist.ne.3) then
               jbinx=jbinx+1
               itemp(4)=ist
               jbrx(jbinx)=temp
               icount=icount+1
           else
               go to 151
           endif
150        continue
151        i2bld(3)=icount
           jbrx(isv)=r8bld
       else if (ityp.eq.1.and.ist.eq.510) then
c                               ***   depth
           depth=.true.
           i2bld(1)=ist
           call parsit
           if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.ityp.eq.4) then
               r4bld(2)=tv
           else
               call error(7)
               go to 99999
           endif
           jbinx=jbinx+1
           jbrx(jbinx)=r8bld
           call parsit
       else if (ityp.eq.1.and.ist.eq.160) then
c                               ***  cutang
           cutang=.true.
           i2bld(1)=ist
           i2bld(2)=0
           call parsit
           if (ityp.eq.2.and.ist.eq.2.or.ityp.eq.3.or.ityp.eq.4) then
               r4bld(2)=tv
               jbinx=jbinx+1
               jbrx(jbinx)=r8bld
               call parsit
           else
               call error(7)
               go to 99999
           endif
       else if (ityp.eq.1.and.ist.eq.324) then
c                               ***  engage
           i2bld(1)=ist
c                    out is the default
           i2bld(2)=653
           jbrx(jbinx+1)=r8bld
           call parsit
           if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.ityp.eq.4) then
              r4bld(1)=tv
              call parsit
           else
              call error(7)
              go to 99999
           endif
           if ((ityp.eq.2.and.ist.eq.2).or.ityp.eq.3.or.ityp.eq.4) then
              r4bld(2)=tv
              call parsit
           else
              call error(7)
              go to 99999
           endif
           jbrx(jbinx+2)=r8bld
           if (ityp.eq.1.and.(ist.eq.653.or.ist.eq.71.or.ist.eq.652))
     1         then
               if (ist.eq.71.or.ist.eq.652) then
                   r8bld=0
                   i2bld(2)=ist
                   i2bld(1)=324
                   jbrx(jbinx+1)=r8bld
               endif
               call parsit
           endif
           jbinx=jbinx+2
       else if (ityp.eq.1.and.ist.eq.7) then
c                               *** retrct
           i2bld(1)=ist
           i2bld(2)=653
           jbrx(jbinx+1)=r8bld
           call parsit
           if (ityp.eq.2.and.ist.eq.2.or.ityp.eq.3.or.ityp.eq.4) then
                r4bld(1)=tv
                call parsit
           else
                call error(7)
                go to 99999
           endif
           if (ityp.eq.2.and.ist.eq.2.or.ityp.eq.3.or.ityp.eq.4) then
                r4bld(2)=tv
                call parsit
           else
                call error(7)
                go to 99999
           endif
           jbrx(jbinx+2)=r8bld
           if (isc10(2).eq.0) then
               if (ityp.eq.1.and.(ist.eq.653.or.ist.eq.71.or.ist.eq.
     1             652)) then
                   if (ist.eq.71.or.ist.eq.652) then
                       r8bld=0
                       i2bld(2)=ist
                       i2bld(1)=7
                       jbr(jbinx+1)=r8bld
                   endif
                   call parsit
               endif
           endif
           jbinx=jbinx+2
       else if (ityp.eq.1.and.ist.eq.322) then
c                                   ***  return
           i2bld(1)=ist
           jbinx=jbinx+1
           jbrx(jbinx)=r8bld
           jbinx=jbinx+1
c                  default is null
           jbrx(jbinx)=0
           call parsit
           if (ityp.eq.1) then
               if (ist.eq.72.or.ist.eq.84.or.ist.eq.85) then
c                    off, xaxis or yaxis
                   r8bld=0
                   i2bld(4)=ist
                   jbrx(jbinx)=r8bld
                   call parsit
               endif
           else if (ityp.eq.2.and.ist.eq.3) then
               jbrx(jbinx)=tv
               call parsit
           endif
       else if (ityp.eq.1.and.ist.eq.822) then
c                       ***  invers
           i2bld(1)=ist
           jbinx=jbinx+1
           jbrx(jbinx)=r8bld
           call parsit
       else if (ityp.ne.7) then
           call error(239)
           go to 99999
       endif
       if (ityp.ne.7) then
           go to 100
       endif
       if (isc10(2).eq.1) then
c                 its a lathe/rough
           if (.not.(depth.and.cutang)) then
                call error(245)
                go to 99999
           endif
       endif
       isc10(3)=jbinx
       call putran (jbx,ifl(4)+1)
99999  continue
       if (debug) then
            write(cout,1230)sc(10)
1230        format('lathes: sc(10)=',f12.4)
            call putmsg(cout,80,17,0)
            do 1010,i=1,jbinx
            write (cout,1231)jbrx(i)
1231        format ('lathes: ',f12.4)
            call putmsg(cout,80,17,0)
1010        continue
       endif
       return
       end

