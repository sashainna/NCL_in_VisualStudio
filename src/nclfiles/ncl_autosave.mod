!  Set up the auto save parameters for the unibase and part program.
!
!
!  UNIBASE:
!  --------
!    /AUTO_U/ *ON  = Enable Auto Save
!             *OFF = Disable Auto Save
!    /TYPE_U/ *TIME    = Auto save after set number of minutes
!             *CHANGES = Auto save after set number of changes
!    /INTERVAL_U/ Positive integer that specifies the number of
!                   minutes/changes at which the auto save occurs
!
#AUTOSAVE#
/AUTO_U/ *OFF
/TYPE_U/ *TIME
/INTERVAL_U/ 5
!
!  PART PROGRAM:
!  -------------
!   /AUTO_PP/ Same as UNI_AUTO
!   /TYPE_PP/ Same as UNI_TYPE
!   /INTERVAL_PP/ Same as UNI_INTERVAL
!   /MAX_SAV_PP/  Non-negative integer. Specifies the maximum number of
!                  auto save part programs that can be created
!
/AUTO_PP/ *OFF
/TYPE_PP/ *CHANGES
/INTERVAL_PP/ 2
/MAX_SAVE_PP/ 2
