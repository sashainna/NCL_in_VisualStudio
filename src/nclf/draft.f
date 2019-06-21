c*****************************************************
c**   NAME         :  draft.f
c**   CONTAINS     :  Handles the DRAFT/... statement.
c**
c**     MODULE NAME AND RELEASE LEVEL
c**       draft.f , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:09:57
c**
c*****************************************************
c**
c** COPYRIGHT (c) 1993 NCCS
c*********************************************************************
c*    E_SUBROUTINE     : subroutine draft 
c*     The main module for DRAFT statement routines.
c*
c*    We call this routine from DRIVER in case if ITYP returned by 
c*    PARSER is 1 and IST is 1059 (DRAFT).
c*
c*********************************************************************

      subroutine draft 

      include 'com4a.com'

      integer*2 iletdir, iletorg, iletsiz, iorigin, ixyplan,
     *          iyzplan, izxplan, ixyzpln, iscale
 
      integer*2 imodify, iformat, iview, icutter, irepaint, ifit, iname
      integer*2 idefault, ianote
c.....
c.....Set up some defaults:
c.....
      data iletdir /1041/, iletorg /1042/, iletsiz /1040/,
     *     iorigin /1027/, ixyplan /33/,  iyzplan   /37/, izxplan /41/,
     *     ixyzpln   /45/,  iscale /25/

      data imodify  /732/, iformat /849/, iview /923/, icutter /716/,
     *     irepaint /924/, ifit /834/, iname /933/, idefault  /903/,
     2     ianote   /616/

      integer*2 ILABEL
      parameter (ILABEL=936)

      if (nextyp .ne. 5) then
           isvinx=inx
           call error(22)
           return
      endif
      ifl(44) = 9
c....
c....Get the next item and find out what to do next.
      idtype = 0
      call parsit
c....
c....First separate all NCL501 draft commands and put appropriate message
c....on the screen. 
c....
  600 if (ITYP.eq.1.and.(IST .eq. iletdir .or.
     x                   IST .eq. iletorg .or. IST .eq. iletsiz .or.
     x                   IST .eq. iorigin .or. IST .eq. ixyplan .or.
     x                   IST .eq. iyzplan .or. IST .eq. izxplan .or.
     x                   IST .eq. ixyzpln .or. IST .eq. iscale)) then
         return
      endif
c....
c....If it is not a NCL501 minor word, it may be 
c....MODIFY FORMAT VIEW CUTTER REPANT FIT NAME
c....or... it is a syntax error.
c....
      if (ITYP .eq. 1 )  then
         if (IST .eq.  imodify ) then
            call drftmod()
         else if (IST .eq.  iformat ) then     
            call drftfmt()
         else if (IST .eq.  iview   ) then     
            call drftvw()
         else if (IST .eq.  icutter ) then     
            call drftcut()
         else if (IST .eq.  irepaint) then
            call drftpnt()
         else if (IST .eq.  ifit    ) then
            call drftfit()
         else if (IST .eq.  iname   ) then
            call drftnam()
         else if (IST .eq.  ILABEL  ) then
            call drftlab()
         else if (IST .eq.  idefault  ) then
            call drftdef()
         else if (IST .eq.  ianote  ) then
            call drftan()
         else
            call error(9)
         endif
      else
         call error(232)
      endif
      return
      end
