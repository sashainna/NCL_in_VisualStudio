c
c***********************************************************************
c
c   FILE NAME: version.f
c   CONTAINS:
c              version_number pworks_version  pmacro_version  pcomp_version
c              ghelp_version  gword_version   gxfer_version   gprompt_version
c              pted_version   mpost_version   pwconv_version  license_version
c              pw_getver_info
c
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        version.f , 26.2
c     DATE AND TIME OF LAST  MODIFICATION
c        07/23/18 , 13:41:56
c
c***********************************************************************
c
c***********************************************************************
c
c   SUBROUTINE:  version_number
c
c   FUNCTION:  This routine sets the version number for PostWorks routines.
c
c   INPUT:  none
c
c   OUTPUT: none
c
c
c***********************************************************************
c
      subroutine version_number
c
      include 'menu.inc'
      include 'pregen.inc'

      PWVER1 = 18
      PWVER2 = 1
      OBJUPD = '06.09.14'
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pworks_version
c
c   FUNCTION:  This routine set 'program name', 'revdate'
c              and copy right dates for PWORKS
c
c   INPUT:  none
c
c   OUTPUT: none
c
c
c***********************************************************************
c
      subroutine pworks_version
c
      include 'menu.inc'

      PGMNAM = 'PostWorks'
      IPROGM = 1
      REVDAT = '07.23.18'
      CPYDATE = '1990-2018'
      call version_number
      return
      end
c
c***********************************************************************
c
c   SUBROUTINE:  pmacro_version
c
c   FUNCTION:  This routine set 'program name', 'revdate'
c              and copy right dates for PMACRO
c
c   INPUT:  none
c
c   OUTPUT: none
c
c
c***********************************************************************
c
      subroutine pmacro_version
c
      include 'menu.inc'

      PGMNAM = 'PostMacro'
      REVDAT = '07.23.18'
      CPYDATE = '1990-2018'
      call version_number
      return
      end

c
c***********************************************************************
c
c   SUBROUTINE:  pcomp_version
c
c   FUNCTION:  This routine set 'program name', 'revdate'
c              and copy right dates for POSTCOMP
c
c   INPUT:  none
c
c   OUTPUT: none
c
c
c***********************************************************************
c
      subroutine pcomp_version
c
      include 'menu.inc'

      PGMNAM = 'PostComp'
      REVDAT = '07.23.18'
      CPYDATE = '1990-2018'
      call version_number
      return
      end

c
c***********************************************************************
c
c   SUBROUTINE:  ghelp_version
c
c   FUNCTION:  This routine set 'program name', 'revdate'
c              and copy right dates for genhelp
c
c   INPUT:  none
c
c   OUTPUT: none
c
c
c***********************************************************************
c
      subroutine ghelp_version
c
      include 'menu.inc'

      PGMNAM = 'GenHelp'
      REVDAT = '06.09.14'
      CPYDATE = '1990-2014'
      call version_number
      return
      end

c
c***********************************************************************
c
c   SUBROUTINE:  gxfer_version
c
c   FUNCTION:  This routine set 'program name', 'revdate'
c              and copy right dates for genxfer
c
c   INPUT:  none
c
c   OUTPUT: none
c
c
c***********************************************************************
c
      subroutine gxfer_version
c
      include 'menu.inc'

      PGMNAM = 'GenXfer'
      REVDAT = '06.09.14'
      CPYDATE = '1990-2014'
      call version_number
      return
      end

c
c***********************************************************************
c
c   SUBROUTINE:  gword_version
c
c   FUNCTION:  This routine set 'program name', 'revdate'
c              and copy right dates for genword
c
c   INPUT:  none
c
c   OUTPUT: none
c
c
c***********************************************************************
c
      subroutine gword_version
c
      include 'menu.inc'

      PGMNAM = 'GenWord'
      REVDAT = '06.09.14'
      CPYDATE = '1990-2014'
      call version_number
      return
      end

c
c***********************************************************************
c
c   SUBROUTINE: gprompt_version
c
c   FUNCTION:  This routine set 'program name', 'revdate'
c              and copy right dates for genprompt
c
c   INPUT:  none
c
c   OUTPUT: none
c
c
c***********************************************************************
c
      subroutine gprompt_version
c
      include 'menu.inc'

      PGMNAM = 'GenPrompt'
      REVDAT = '06.09.14'
      CPYDATE = '1990-2014'
      call version_number
      return
      end

c
c***********************************************************************
c
c   SUBROUTINE:  pted_version
c
c   FUNCTION:  This routine set 'program name', 'revdate'
c              and copy right dates for PTED
c
c   INPUT:  none
c
c   OUTPUT: none
c
c
c***********************************************************************
c
      subroutine pted_version
c
      include 'menu.inc'

      PGMNAM = 'Pted'
      IPROGM = 2
      REVDAT = '08.11.16'
      CPYDATE = '2001-2016'
      call version_number
      return
      end

c
c***********************************************************************
c
c   SUBROUTINE:  mpost_version
c
c   FUNCTION:  This routine set 'program name', 'revdate'
c              and copy right dates for MPOST
c
c   INPUT:  none
c
c   OUTPUT: none
c
c
c***********************************************************************
c
      subroutine mpost_version
c
      include 'menu.inc'

      PGMNAM = 'MPost'
      REVDAT = '07.23.18'
      CPYDATE = '2001-2018'
      call version_number
      return
      end

c
c***********************************************************************
c
c   SUBROUTINE:  pwconv_version
c
c   FUNCTION:  This routine set 'program name', 'revdate'
c              and copy right dates for PWCONV
c
c   INPUT:  none
c
c   OUTPUT: none
c
c
c***********************************************************************
c
      subroutine pwconv_version
c
      include 'menu.inc'

      PGMNAM = 'Pwconv'
      REVDAT = '11.06.17'
      CPYDATE = '2011-2017'
      call version_number
      return
      end

c***********************************************************************
c
c   SUBROUTINE:  License_version
c
c   FUNCTION:  This routine set 'program name', 'revdate'
c              and copy right dates for Nccs_License
c
c   INPUT:  none
c
c   OUTPUT: none
c
c
c***********************************************************************
c
      subroutine License_version
c
      include 'menu.inc'

      PGMNAM = 'NCCS_License'
      REVDAT = '01.14.13'
      CPYDATE = '2001-2013'
      call version_number
      return
      end

c***********************************************************************
c
c   SUBROUTINE:  pw_getver_info(prog, revdate, rightdate)
c
c   FUNCTION:  This routine return 'program name', 'revdate'
c              and copy right dates
c
c   INPUT:  none
c
c   OUTPUT: program: prog name
c			revdate : revised date
c			rightdate: copyright date
c
c
c***********************************************************************
c
      subroutine pw_getver_info(prog, revdate, rightdate, kmaj, kmin)
c
      include 'menu.inc'

      integer*4 kmaj,kmin
c
      character*11 prog
      character*9 revdate
      character*11 rightdate

      prog(1:10) = PGMNAM
      revdate(1:8) = REVDAT
      rightdate(1:9) = CPYDATE
      kmaj = PWVER1
      kmin = PWVER2
      return
      end
