C*********************************************************************
C*    NAME         :  decmac.f
C*       CONTAINS:
C*    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*    MODULE NAME AND RELEASE LEVEL 
C*       decmac.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:09:54
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine decmac
C* **                                                                  **
C* **  purpose of subroutine: to parse a macro declaration statement   **
C* **                                                                  **
C* **  each 'parms' array record consists of:                          **
C* **                                                                  **
C* **   one 24 byte header record:                                     **
C* **                                                                  **
C* **      maclin    next    termln    mode    pr. rec  pr. ele        **
C* **   ********************************************************       **
C* **   *   i4    *   i4   *   i4   *   i2   *   i2   *   i2   *       **
C* **   ********************************************************       **
C* **    where:                                                        **
C* **       maclin = line number of the macro statement.               **
C* **       next = this will contain the line of the next line after   **
C* **              the macro statement if this is the last parameter   **
C* **              record from the ranfile.                            **
C* **              this will contain >ffff if it is not the last       **
C* **              parameter record from the ranfile.                  **
C* **       termln = line number of termac statement. (1st record only)**
C* **       mode = program state at time of call.                      **
C* **       pr. rec = Record number of Macro description               **
C* **       pr. ele = Element within record of Macro description       **
C* **       (3) blank I*2 fields follow 'pr. ele'.
C* **                                                                  **
C* **   the next (33) i2 fields contain information about the          **
C* **   corresponding 12 parameter defaults. (3) blank I*2 fields      **
C* **   follow these fields (for at total of 36 i2 fields).            **
C* **                                                                  **
C* **    typrm value            type of parameter default              **
C* **      >f00f                    no default                         **
C* **      >0000                    scaler                             **
C* **      >ffff                    variable name                      **
C* **                                                                  **
C* **       pr. rec = Record number of Variable description (prompt)   **
C* **       pr. ele = Element within record of Variable description    **
C* **                                                                  **
C* **                                                                  **
C* **   the remaining 22 fields (8 bytes each) contain information     **
C* **   about the parameters:                                          **
C* **                                                                  **
C* **     each parameter uses two 8 byte fields                        **
C* **                                                                  **
C* **     for a parameter with no default:                             **
C* **       *************************************************          **
C* **       *    parameter name     *        blank          *          **
C* **       *************************************************          **
C* **                                                                  **
C* **     for a parameter with an expression, integer or real number:  **
C* **       *************************************************          **
C* **       *    parameter name     *  expression, integer  *          **
C* **       *                       *     or real value     *          **
C* **       *************************************************          **
C* **                                                                  **
C* **     for a parameter with a variable name:                        **
C* **                                                                  **
C* **       *************************************************          **
C* **       *    parameter name     *     variable name     *          **
C* **       *************************************************          **
C* **                                                                  **
C* **********************************************************************
C* **********************************************************************
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
      subroutine decmac

      include 'com8a.com'
      include 'status.com'

      integer*2 irest(4),head(12),name,pg,el
      integer*4 maclin,next,jrest(2), callin,termln
      integer*2 tparms,mode
      integer*4 namsub
      logical first,nored
      character*64 mcname,macnam
      equivalence (sc165, mcname)
      equivalence (rest,irest,jrest)
      equivalence (mode,head(7))
      integer*2 melem
      equivalence (melem,ifl(39))
      integer*4 msubsv
      integer*2 ipgtmp, i
      character*64 svtok2
      integer*4 i4sub,psav(2)
      logical lmcini

      lmcini = .false.
      if (ifl(38) .eq. 1) then
          call error(74)
          go to 99999
      endif

c         Store MACRO name for later use by STATLN.F routine.
      mcname = savid2
      macnam = savid2
      namsub = isvsub
      if (namsub .ne. x0000) then
          call error(73)
          go to 99999
      endif
c         Save the line number of the MACRO statement so DECLLB.F 
c         can verify that a label token that it may find already defined
c         was not defined in this MACRO.
      call nclf_src_line_to_rec (svll,ifl4(5))
      maclin=ifl4(5)
      name=0
      tparms=0
      ifl(38)=1
      mode=ifl(152)
      ifl(152)=1
      if (pgindx .lt. 30) pgindx = pgindx + 1
      pgmode(pgindx) = 1
      first=.true.
      nored=.false.
      ipgtmp=ifl(4)+40
      pg=ipgtmp
      el=1
      call mclini ()
      lmcini = .true.
c
c...Zero out Macro prompt (description) record
c...Bobby -  10/2/97
c
      head(8) = 0
      head(9) = 0
c
c...store header info
c
      callin = -1
      termln = -1
      next = -1
      call ncl_storehd (maclin, next, termln, mode, head(8),
     1                      head(9), callin)
c
c...  If Lang= CADRA then varialbes and vocab words can be used as mac params
c
500   if ((ityp .eq. 2 .and. (ist .eq. 1 .or. ist .eq. 12)) .or.
     1    (ifl(374) .ne. 0 .and. (ityp .eq. 1 .or. ityp .eq. 2))) then
          name=name+1
          call mclspt (psav)
          call vstchk
          call mclrpt (psav)
          savid2=token2
          isvsub = ivxsub
          irest(4)=12
          idst = 12
          call mclspt (psav)
          call vstore
          call mclrpt (psav)
          tparms=tparms+1
          svtok2 = token2
          msubsv = ivxsub
          iflg = 1
          if (nextyp .eq. 1) then
             inx=inx+1
c
c...Save this Macro's list
c...in case a Macro is used as a default parameter
c...Calls to 'mclspt' and 'mclrpt' throughout routine
c...Bobby  -  8/18/97
c
             call mclspt (psav)
             idtype = -1
             call parsit
             call mclrpt (psav)
             if ((ityp .eq. 2 .and. 
     c                ((ist .gt. 0 .and. ist .le. 12) .or.
     c                  ist .eq. 14 .or. ist .eq. 20 .or. ist .eq. 21
     c                  .or. ist .eq. VSOLID))
     c           .or.ityp.eq.1 .or. ityp .eq. 3 .or. ityp .eq. 4) then
                 if (ityp .eq. 3 .or. ityp .eq. 4) then
                     iflg = 3
                 else
                     iflg = 2
                 endif
             else
                 call error(71)
                 go to 99999
             endif
         endif
         call mclstr(iflg,svtok2,msubsv,token2,ivxsub,tv,ityp,ist)
         call mclspt (psav)
         idtype = -1
         call parsit
         call mclrpt (psav)
         go to 500
      else if (ityp.eq.7) then
          go to 1000
      else if (ityp .eq. 2) then
          call error(8)
          go to 99999
      else
          call error(69)
          go to 99999
      endif

c               set all parameter names to parameter variable type
1000  rest=0.
      idst = 12
      i=name*2+12
      call ncl_getmc_hd (maclin, next, termln, mode, head(8), 
     1               head(9), callin)
      call nclf_src_line_to_rec (nline+1,next)
      call ncl_storehd (maclin, next, termln, mode, head(8),
     1                      head(9), callin)
C
C...   Remove this macro's entry in name list (if any)
C
      token2 = macnam
      i4sub = namsub
      i = 0
      call vxdel (token2, i4sub, i)
      savid2=macnam
      isvsub = 0
         irest(3)=tparms
         irest(4)=11
         idst = 11
99999 if (err) then
          ifl4(5)=-1
          ifl(38)=0
          if (lmcini) call mcldel
      endif

      return
      end
