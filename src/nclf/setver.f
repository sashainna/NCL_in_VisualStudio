C*********************************************************************
C*    NAME         :  setver.f
C*       CONTAINS:
C*    COPYRIGHT 1988 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
C*     MODULE NAME AND RELEASE LEVEL 
C*       setver.f , 25.1
C*    DATE AND TIME OF LAST  MODIFICATION
C*       04/29/15 , 15:10:41
C********************************************************************/
C
C*********************************************************************
C*    E_SUBROUTINE     : subroutine setver
c*       Set the current version number of this NCL executable
C*    PARAMETERS   
C*       INPUT  : 
C*          none
C*       OUTPUT :  
C*          none
C*    RETURNS      : none
C*    SIDE EFFECTS : none
C*    WARNINGS     : none
C********************************************************************/
c 
c                        DETAILS OF CHANGING SETVER.F WHEN 
c                  NEW ALPHA, BETA OR PRODUCTION RELEASES ARE MADE
c 
c 
c In order to keep track of what changes to source code are incorporated into 
c specific executables of NCL502, the following procedure must be followed:
c 
c   Immediately after a new executable has been linked and released for testing 
c   by NCCS or for productions usage, the SETVER.F file must be EDITed, the 
c   version number changed and the file DELTAed back into the SCCS data base.
c 
c This will ensure that when a problem is reported to us and a version number is
c given, we can look at source code change dates and tell what changes went into
c that specific release.
c 
c For example, if the current version number is 7.37 and we generate a new
c executable then we should bump the version number to 7.38.  If an error is
c reported to be in 7.37, we can look at the change date of the SETVER.F file to
c see when the version was changed FROM 7.37 and ignore any changes made to
c source after that date in finding the cause of the bug.  We can also check the
c date the version number was changed TO 7.37 and ignore any changes made prior
c to that date if we are sure the bug was introduced by a change since the
c previous release. 

      subroutine setver 

      include 'com4a.com' 

      real*8 ver

c          This version number is used by the *VER command which is handled in
c          the routine UREST.F and it is printed on the batch listing by the 
c          routine loadpp.  

c          The value assigned to sc(119) should be changed for every release
c          that is sent out to any customers.
	  call getver(ver)
	  sc(119) = ver

      return
      end 
