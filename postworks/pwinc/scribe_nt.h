/************************************************************************
c
c   FILE NAME: Scribe_nt.h
c
c	 Description - Include file for SCRIBE.LIB (key device)
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        scribe_nt.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:41
c
c**********************************************************************
*/
#ifndef _SCRIBE_H
#define _SCRIBE_H

                                       /* for dword alignment roll-up      */ 
#define SCRIBE_APIPACKET_ALIGNMENT_VALUE (sizeof(unsigned long))
#define SCRIBE_APIPACKET_SIZE            (1024+SCRIBE_APIPACKET_ALIGNMENT_VALUE)

/**  Scribe API error codes.  **/
#define SK_SUCCESS                      0
#define SK_INVALID_PACKET               2
#define SK_UNIT_NOT_FOUND               3
#define SK_ACCESS_DENIED                4
#define SK_PORT_IS_BUSY                 7
#define SK_NO_PORT_FOUND                9
#define SK_DRIVER_OPEN_ERROR            11
#define SK_DRIVER_NOT_INSTALLED         12
#define SK_IO_COMMUNICATIONS_ERROR      13
#define SK_PACKET_TOO_SMALL             15
#define SK_INVALID_PARAMETER            16
#define SK_VERSION_NOT_SUPPORTED        18
#define SK_OS_NOT_SUPPORTED             19
#define SK_INVALID_COMMAND              21
#define SK_MEM_ALIGNMENT_ERROR          29
#define SK_PORT_ALLOCATION_FAILURE      31
#define SK_PORT_RELEASE_FAILURE         32
#define SK_ACQUIRE_PORT_TIMEOUT         39
#define SK_SIGNAL_NOT_SUPPORTED         42
#define SK_UNKNOWN_MACHINE              44
#define SK_SYS_API_ERROR                45
#define SK_INVALID_PORT_TYPE            47
#define SK_INVALID_MACH_TYPE            48
#define SK_INVALID_IRQ_MASK             49
#define SK_INVALID_CONT_METHOD          50
#define SK_INVALID_PORT_FLAGS           51
#define SK_INVALID_LOG_PORT_CFG         52
#define SK_INVALID_OS_TYPE              53
#define SK_INVALID_LOG_PORT_NUM         54
#define SK_INVALID_ROUTER_FLGS          56
#define SK_INIT_NOT_CALLED              57
#define SK_DRIVER_TYPE_NOT_SUPPORTED    58
#define SK_FAIL_ON_DRIVER_COMM          59

/* Define possible driver types (Not used refer to Driver Types). */
#define SCRIBE_DRVR_WIN31  4
#define SCRIBE_DRVR_WINNT  5

/* Create SCRIBE types */
#ifdef __cplusplus
#define SCRIBE_EXPORT extern "C"
#else
#define SCRIBE_EXPORT extern
#endif
#define SCRIBE_LOCAL static

#if (defined(WIN32) || defined(_WIN32) || defined(__WIN32__) || defined(__NT__))
#ifndef _WIN32_
#define _WIN32_ 1
#endif
#endif

#if  ((defined(__BORLANDC__) || defined(_MSC_VER)) && defined(_WIN32_))

#define SCRIBE_STDCALL  __stdcall
#define SCRIBE_FASTCALL __fastcall
#define SCRIBE_PASCAL
#define SCRIBE_CDECL    __cdecl
#define SCRIBE_FAR
#define SCRIBE_NEAR
#define SCRIBE_HUGE
#define SCRIBE_API      WINAPI
#define SCRIBE_PTR      *

#elif (defined(_MSC_VER) && !defined(_WIN32_))

#define SCRIBE_STDCALL
#define SCRIBE_FASTCALL _fastcall
#define SCRIBE_PASCAL   _pascal
#define SCRIBE_CDECL    _cdecl
#define SCRIBE_FAR      _far
#define SCRIBE_NEAR     _near
#define SCRIBE_HUGE     _huge
#define SCRIBE_API      SCRIBE_FAR SCRIBE_PASCAL
#define SCRIBE_PTR      SCRIBE_FAR *

#elif (defined(__BORLANDC__) && !defined(_WIN32_))

#define SCRIBE_STDCALL
#define SCRIBE_FASTCALL
#define SCRIBE_PASCAL  pascal
#define SCRIBE_CDECL   _cdecl
#define SCRIBE_FAR     far
#define SCRIBE_NEAR    near
#define SCRIBE_HUGE    huge
#define SCRIBE_PTR     SCRIBE_FAR *
#define SCRIBE_API     SCRIBE_FAR SCRIBE_PASCAL

#elif (defined(__WATCOMC__) && defined(_WIN32_))

#define SCRIBE_STDCALL  __stdcall
#define SCRIBE_FASTCALL __fastcall
#define SCRIBE_PASCAL
#define SCRIBE_CDECL    __cdecl
#define SCRIBE_FAR
#define SCRIBE_NEAR
#define SCRIBE_HUGE
#define SCRIBE_API      WINAPI

#define SCRIBE_PTR      *

#else

#define SCRIBE_STDCALL
#define SCRIBE_FASTCALL
#define SCRIBE_PASCAL
#define SCRIBE_CDECL
#define SCRIBE_FAR
#define SCRIBE_NEAR
#define SCRIBE_HUGE
#define SCRIBE_API
#define SCRIBE_PTR *

#endif

#define SCRIBE_IN
#define SCRIBE_OUT
#define SCRIBE_IO

#if !defined(_RBTYPES_INC)

typedef                void RB_VOID;
typedef unsigned       char RB_BOOLEAN;
typedef unsigned       char RB_BYTE;
typedef short int           RB_SHORT;
typedef unsigned short int  RB_WORD;
typedef unsigned long  int  RB_DWORD;
typedef RB_VOID    SCRIBE_PTR  RBP_VOID;
typedef RB_BYTE    SCRIBE_PTR  RBP_BYTE;
typedef RB_BOOLEAN SCRIBE_PTR  RBP_BOOLEAN;
typedef RB_WORD    SCRIBE_PTR  RBP_WORD;
typedef RB_SHORT   SCRIBE_PTR  RBP_SHORT;
typedef RB_DWORD   SCRIBE_PTR  RBP_DWORD;

#endif

typedef RB_DWORD RB_SCRIBE_APIPACKET[SCRIBE_APIPACKET_SIZE/sizeof(RB_DWORD)];
typedef RB_WORD  SCRIBE_STATUS;
typedef RBP_VOID RBP_SCRIBE_APIPACKET;

/* machine types */
#define RB_MIN_MACH_TYPE          0
#define RB_AUTODETECT_MACHINE     0          /* Autodetect machine type    */
#define RB_IBM_MACHINE            1          /* defines IBM type hw        */
#define RB_NEC_MACHINE            2          /* defines NEC PC-9800 hw     */
#define RB_FMR_MACHINE            3          /* defines Fujitsu FMR hw     */
#define RB_MAX_MACH_TYPE          3

/* OS types */
#define RB_MIN_OS_TYPE            0
#define RB_AUTODETECT_OS_TYPE     0          /* Autodetect OS type         */
#define RB_OS_DOS                 1          /* DOS operating system       */
#define RB_OS_RSRV1               2          /* reserved                   */
#define RB_OS_RSRV2               3          /* reserved                   */
#define RB_OS_WIN3x               4          /* Windows 3.x operating env  */
#define RB_OS_WINNT               5          /* Windows NT operating system*/
#define RB_OS_OS2                 6          /* OS/2 operating system      */
#define RB_OS_WIN95               7          /* Windows 95 operating system*/
#define RB_OS_WIN32s              8          /* Windows WIN32s env         */
#define RB_MAX_OS_TYPE            8

/* Driver types */
#define RB_DOSRM_LOCAL_DRVR       1          /* DOS Real Mode local driver */
#define RB_WIN3x_LOCAL_DRVR       2          /* Windows 3.x local driver   */
#define RB_WIN32s_LOCAL_DRVR      3          /* Win32s local driver        */
#define RB_WIN3x_SYS_DRVR         4          /* Windows 3.x system driver  */
#define RB_WINNT_SYS_DRVR         5          /* Windows NT system driver   */
#define RB_OS2_SYS_DRVR           6          /* OS/2 system driver         */
#define RB_WIN95_SYS_DRVR         7          /* Windows 95 system driver   */

/* Router Flags */
#define RB_ROUTER_USE_LOCAL_DRVR  0x0001     /* use linked in driver       */
#define RB_ROUTER_USE_SYS_DRVR    0x0002     /* use system driver          */
#define RB_ROUTER_AUTODETECT_DRVR (RB_ROUTER_USE_LOCAL_DRVR | \
                                   RB_ROUTER_USE_SYS_DRVR)      
#define RB_MAX_ROUTER_FLAGS       (RB_ROUTER_USE_LOCAL_DRVR | \
                                   RB_ROUTER_USE_SYS_DRVR)

/* Port Params flags */
#define RB_FIRST_LOG_PORT         0          /* first logical port         */
#define RB_LAST_LOG_PORT          3          /* last logical port          */
#define RB_VALIDATE_PORT          0x00000001 /* I/O validate port exsitence*/
#define RB_CONT_HNDLR_INSTALLED   0x00000002 /* OUT    system contention   */
#define RB_USER_DEFINED_PORT      0x00000004 /* OUT    user defined port   */
#define RB_RSRV_PORT_FLAGS        0xFFFFFFF8 /* reserved                   */
#define RB_DEFAULT_PORT_FLAGS     (RB_VALIDATE_PORT)
#define RB_USE_AUTOTIMING         0

/* Port types */
#define RB_MIN_PORT_TYPE          0
#define RB_AUTODETECT_PORT_TYPE   0          /* IN   autodetect port type  */
#define RB_NEC_PORT_TYPE          1          /* I/O NEC-PC9800 series port */
#define RB_FMR_PORT_TYPE          2          /* I/O Fujitus FMR series port*/
#define RB_PS2_PORT_TYPE          3          /* I/O IBM/AT/PS2 series port */
#define RB_PS2_DMA_PORT_TYPE      4          /* I/O IBM PS2 DMA series port*/
#define RB_MAX_PORT_TYPE          4

/* Contention Methods (bit mask) */
#define RB_CONT_METH_SYS_ALLOC    0x00000001 /* I/O System port allocation */     
#define RB_CONT_METH_NT_RIRQL     0x00000002 /* OUT    NT Raise IRQ level  */    
#define RB_CONT_METH_SYS_INT      0x00000004 /* I/O Disable System Ints    */ 
#define RB_CONT_METH_MASK_INT     0x00000008 /* I/O Mask ints at PIC       */     
#define RB_CONT_METH_WIN_CS       0x00000010 /* I/O Windows Critical Sect  */   
#define RB_CONT_METH_POLL_HW      0x00000020 /* I/O H/W polling of port    */
#define RB_CONT_METH_RBW          0x00000040 /* I/O Read Before Write      */
#define RB_CONT_METH_DRVR_DEFINED 0x80000000 /* Contention defined by drvr.*/

/* Interrupts to mask (bit mask) */
#define RB_IRQ_MASK_LPT1      0x0001         /* mask LPT1  interrupt       */
#define RB_IRQ_MASK_LPT2      0x0002         /* mask LPT2  interrupt       */
#define RB_IRQ_MASK_TIMER     0x0004         /* mask TIMER interrupt       */
#define RB_IRQ_MAX_MASK       (RB_IRQ_MASK_LPT1 | \
                               RB_IRQ_MASK_LPT2 | \
                               RB_IRQ_MASK_TIMER)
#define RB_IRQ_MASK_DEF       (RB_IRQ_MASK_LPT1 | \
                               RB_IRQ_MASK_TIMER)

/* Define default retry counts and intervals */
#define RB_PORT_CONT_RETRY_CNT_DEF 100       /* 100 retries for port cont  */
#define RB_PORT_CONT_RETRY_INT_DEF 300       /* 300 ms retry interval      */
#define RB_DEV_RETRY_CNT_DEF       100       /* 100 retries for device     */

/* Define the cmd field values for RB_SCRIBE_LIB_PARAMS  */
#define RB_SET_LIB_PARAMS_CMD      0x0001    /* Set library parameters     */
#define RB_GET_LIB_PARAMS_CMD      0x0002    /* Get library parameters     */

/* define the func field values for RB_SCRIBE_LIB_PARAMS */
#define RB_MACHINE_TYPE_FUNC       0x0001    /* Set/Get Machine type       */
#define RB_DELAY_FUNC              0x0002    /* Set/Get Delay value        */
#define RB_MASK_INTS_FUNC          0x0003    /* Set/Get Mask interrupts    */
#define RB_ROUTER_FLAGS_FUNC       0x0004    /* Set/Get Router flags       */
#define RB_OS_PARAMS_FUNC          0x0005    /* Set/Get O/S parameters     */
#define RB_PORT_PARAMS_FUNC        0x0006    /* Set/Get Port Parameters    */

typedef struct _RB_SCRIBE_OS_PARAMS {
SCRIBE_IO  RB_WORD osType;                   /* type of Operating System   */
SCRIBE_OUT RB_WORD osVer;                    /* version of Operating System*/
} RB_SCRIBE_OS_PARAMS;
typedef RB_SCRIBE_OS_PARAMS SCRIBE_PTR RBP_SCRIBE_OS_PARAMS;

typedef struct _RB_SCRIBE_PORT_PARAMS {
SCRIBE_IO  RB_WORD  logPortNum;              /* logical port number        */
SCRIBE_IO  RB_WORD  sysPortNum;              /* system  port number        */
SCRIBE_IO  RB_WORD  portType;                /* port type                  */
SCRIBE_IO  RB_WORD  phyAddr;                 /* physcial address           */
SCRIBE_OUT RB_WORD  mappedAddr;              /* map address                */
SCRIBE_IO  RB_WORD  deviceRetryCnt;          /* device retry count         */
SCRIBE_IO  RB_WORD  contentionRetryCnt;      /* port contention retry count*/
SCRIBE_IO  RB_WORD  padding1;
SCRIBE_IO  RB_DWORD contentionMethod;        /* port contention method     */
SCRIBE_IO  RB_DWORD contentionRetryInterval; /* port contention retry int  */
SCRIBE_IO  RB_DWORD flags1;                  /* port flags                 */
} RB_SCRIBE_PORT_PARAMS;
typedef RB_SCRIBE_PORT_PARAMS SCRIBE_PTR RBP_SCRIBE_PORT_PARAMS;

typedef union  _RB_SCRIBE_CFG_PARAMS {
SCRIBE_IO RB_WORD        machineType;      /* machine type: IBM, NEC, or FMR*/
SCRIBE_IO RB_WORD        delay;            /* number of loops for 2us delay */
SCRIBE_IO RB_WORD        maskInterrupts;   /* interrupts to mask            */
SCRIBE_IO RB_WORD        routerFlags;      /* request routing flags         */
SCRIBE_IO RB_SCRIBE_OS_PARAMS   osParams;  /* OS parameters                 */
SCRIBE_IO RB_SCRIBE_PORT_PARAMS portParams;/* port parameters               */
} RB_SCRIBE_CFG_PARAMS;
typedef RB_SCRIBE_CFG_PARAMS SCRIBE_PTR RBP_SCRIBE_CFG_PARAMS;

typedef struct _RB_SCRIBE_LIB_PARAMS {
SCRIBE_IN RB_WORD   cmd;                   /* command - set/get parameters  */
SCRIBE_IN RB_WORD   func;                  /* function to set/get           */
SCRIBE_IO RB_SCRIBE_CFG_PARAMS params;
} RB_SCRIBE_LIB_PARAMS;
typedef RB_SCRIBE_LIB_PARAMS SCRIBE_PTR RBP_SCRIBE_LIB_PARAMS;

/* Define all RNBOskCommand related macros and data */
#define  HEXVAL_SIGN    36     /* '$' */
#define  SKCMD_FAILURE                   -1
#define  SKCMD_FAILED_ACCESS_PORT      -895  /* for OS/2 only */
#define  SKCMD_PORT_IS_BUSY            -896
#define  SKCMD_PORT_NOT_FOUND          -899
#define  SKCMD_INVALID_REQUEST         -993  /* requested command is invalid */
#define  SKCMD_NO_KEY_DETECTED         -994
#define  SKCMD_INVALID_PARAMETER       -995  /* requested command has invalid parameter */
#define  SKCMD_ACCESS_DENIED           -997  /* requested command requires a higher
                                             permission level */

#define  SKCMD_PASSWORD_SIZE        16
#define  SKCMD_FAMILY_CODE_SIZE     2
#define  LINE_LENGTH                80
#define  MIN_NO_ARG_CMD_LEN         2
#define  MIN_ENABLE_CMD_LEN         18
#define  MIN_INIT_CMD_LEN           3
#define  MIN_FAMILY_CMD_LEN         4
#define  MIN_READ_CMD_LEN           2
#define  MIN_WRITE_CMD_LEN          2
#define  WRITEABLE_CELL_RANGE       60

typedef  struct  {
   RB_WORD port_address;  /* support logical port no. only */
   RB_WORD timing;        /* for backward compatibility only */
                          /* Not used under WIN32 */
} INIT_BLK;

typedef  struct  {
   RB_WORD address;
   RB_WORD value;
} WRITE_BLK;

typedef  struct
{
   RB_BYTE function_code;
   union
   {
      struct { RB_BYTE   pw[SKCMD_PASSWORD_SIZE+1];   } enable_params;
      struct { RB_BYTE   fc[SKCMD_FAMILY_CODE_SIZE+1];} setfamily_params;
      struct { INIT_BLK  data;                        } init_params;
      struct { RB_SHORT  cell;                        } read_params;
      struct { WRITE_BLK data;                        } write_params;
   }  params;

} RB_SCRIBE_SKCOMMAND_BLK;
typedef RB_SCRIBE_SKCOMMAND_BLK SCRIBE_PTR RBP_SCRIBE_SKCOMMAND_BLK;

/* end of all RNBOskCommand related macros and data */

/* Define the extern routines */
SCRIBE_EXPORT
SCRIBE_STATUS SCRIBE_API RNBOskCfgLibParams( SCRIBE_IO RBP_SCRIBE_APIPACKET  thePacket,
                                             SCRIBE_IO RBP_SCRIBE_LIB_PARAMS params );
SCRIBE_EXPORT
SCRIBE_STATUS SCRIBE_API RNBOskFormatPacket( SCRIBE_OUT RBP_SCRIBE_APIPACKET thePacket,
                                             SCRIBE_IN  RB_WORD              thePacketSize );
SCRIBE_EXPORT
SCRIBE_STATUS SCRIBE_API RNBOskInitialize( SCRIBE_OUT RBP_SCRIBE_APIPACKET thePacket );

SCRIBE_EXPORT
SCRIBE_STATUS SCRIBE_API RNBOskRead( SCRIBE_IN  RBP_SCRIBE_APIPACKET thePacket,
                                     SCRIBE_IN  RB_WORD              cellAddr,
                                     SCRIBE_OUT RBP_WORD             cellValue );
SCRIBE_EXPORT
SCRIBE_STATUS SCRIBE_API RNBOskWrite( SCRIBE_OUT RBP_SCRIBE_APIPACKET thePacket,
                                      SCRIBE_IN  RB_WORD              cellAddr,
                                      SCRIBE_IN  RB_WORD              cellValue );

SCRIBE_EXPORT
RB_WORD SCRIBE_API RNBOskGetFullStatus( SCRIBE_IN RBP_SCRIBE_APIPACKET thePacket );

SCRIBE_EXPORT
SCRIBE_STATUS SCRIBE_API RNBOskGetVersion( SCRIBE_IN  RBP_SCRIBE_APIPACKET thePacket,
                                           SCRIBE_OUT RBP_BYTE             majVer,
                                           SCRIBE_OUT RBP_BYTE             minVer,
                                           SCRIBE_OUT RBP_BYTE             rev,
                                           SCRIBE_OUT RBP_BYTE             osDrvrType );
SCRIBE_EXPORT
SCRIBE_STATUS SCRIBE_API RNBOskEnable( SCRIBE_IN RBP_SCRIBE_APIPACKET  thePacket,
                                       SCRIBE_IN RBP_BYTE              thePassword,

                                       SCRIBE_IN RB_WORD               passwordLen );
SCRIBE_EXPORT
SCRIBE_STATUS SCRIBE_API RNBOskSetFamily( SCRIBE_IN RBP_SCRIBE_APIPACKET  thePacket,
                                          SCRIBE_IN RB_BYTE               famA,
                                          SCRIBE_IN RB_BYTE               famB );

SCRIBE_EXPORT
SCRIBE_STATUS SCRIBE_API RNBOskSetPort( SCRIBE_IN RBP_SCRIBE_APIPACKET  thePacket,
                                        SCRIBE_IN RB_BYTE               thePort );

SCRIBE_EXPORT
SCRIBE_STATUS SCRIBE_API RNBOskCommand( SCRIBE_IN RBP_SCRIBE_APIPACKET  thePacket,
                                        SCRIBE_IN RBP_BYTE              commandStr,
                                        SCRIBE_IN RB_WORD               commandStrLen,
                                        SCRIBE_OUT RBP_SHORT            skResultCode );
#endif
/* end of file */
