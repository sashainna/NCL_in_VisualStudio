/*********************************************************************
**    NAME         :  lumb.h
**     MODULE NAME AND RELEASE LEVEL 
**       lumb.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:28
*********************************************************************/
#ifdef UU_DEBUGOFF 
#define UL_IF_FAILURE_PRINT_IT
#else
#define UL_IF_FAILURE_PRINT_IT                       \
	if (status != UU_SUCCESS)                         \
		uu_dprint(UU_ITRC,(us, "returned FAILURE"));   
#endif

#if UU_COMP == UU_VAXVMS
#define UL_DIR_SEP "."
#define UL_OPTION_SEP "/"
#else 
/*
.....added native WinNT
*/
#if UU_COMP == UU_WIN2K
#define UL_DIR_SEP "\\"
#define UL_OPTION_SEP "-"
#else
#define UL_DIR_SEP "/"
#define UL_OPTION_SEP "-"
#endif
#endif
/*********************************************************************
**    NAME         :  lumb.h
*********************************************************************/
