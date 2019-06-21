/* this ALWAYS GENERATED file contains the proxy stub code */


/* File created by MIDL compiler version 5.01.0164 */
/* at Mon Jul 12 17:29:36 2004
 */
/* Compiler settings for C:\User\bob\SolidWorks\API\SwUnibase_new\SwUnibase.idl:
    Oicf (OptLev=i2), W1, Zp8, env=Win32, ms_ext, c_ext
    error checks: allocation ref bounds_check enum stub_data 
*/
//@@MIDL_FILE_HEADING(  )

#define USE_STUBLESS_PROXY


/* verify that the <rpcproxy.h> version is high enough to compile this file*/
#ifndef __REDQ_RPCPROXY_H_VERSION__
#define __REQUIRED_RPCPROXY_H_VERSION__ 440
#endif


#include "rpcproxy.h"
#ifndef __RPCPROXY_H_VERSION__
#error this stub requires an updated version of <rpcproxy.h>
#endif // __RPCPROXY_H_VERSION__


#include "SwUnibase.h"

#define TYPE_FORMAT_STRING_SIZE   7                                 
#define PROC_FORMAT_STRING_SIZE   73                                

typedef struct _MIDL_TYPE_FORMAT_STRING
    {
    short          Pad;
    unsigned char  Format[ TYPE_FORMAT_STRING_SIZE ];
    } MIDL_TYPE_FORMAT_STRING;

typedef struct _MIDL_PROC_FORMAT_STRING
    {
    short          Pad;
    unsigned char  Format[ PROC_FORMAT_STRING_SIZE ];
    } MIDL_PROC_FORMAT_STRING;


extern const MIDL_TYPE_FORMAT_STRING __MIDL_TypeFormatString;
extern const MIDL_PROC_FORMAT_STRING __MIDL_ProcFormatString;


/* Object interface: IUnknown, ver. 0.0,
   GUID={0x00000000,0x0000,0x0000,{0xC0,0x00,0x00,0x00,0x00,0x00,0x00,0x46}} */


/* Object interface: IDispatch, ver. 0.0,
   GUID={0x00020400,0x0000,0x0000,{0xC0,0x00,0x00,0x00,0x00,0x00,0x00,0x46}} */


/* Object interface: ISwUnibaseObj, ver. 0.0,
   GUID={0x46A48BAE,0x9093,0x11D7,{0x87,0xE7,0x08,0x00,0x69,0x13,0x62,0xA7}} */


extern const MIDL_STUB_DESC Object_StubDesc;


extern const MIDL_SERVER_INFO ISwUnibaseObj_ServerInfo;

#pragma code_seg(".orpc")
static const unsigned short ISwUnibaseObj_FormatStringOffsetTable[] = 
    {
    (unsigned short) -1,
    (unsigned short) -1,
    (unsigned short) -1,
    (unsigned short) -1,
    0,
    22,
    50
    };

static const MIDL_SERVER_INFO ISwUnibaseObj_ServerInfo = 
    {
    &Object_StubDesc,
    0,
    __MIDL_ProcFormatString.Format,
    &ISwUnibaseObj_FormatStringOffsetTable[-3],
    0,
    0,
    0,
    0
    };

static const MIDL_STUBLESS_PROXY_INFO ISwUnibaseObj_ProxyInfo =
    {
    &Object_StubDesc,
    __MIDL_ProcFormatString.Format,
    &ISwUnibaseObj_FormatStringOffsetTable[-3],
    0,
    0,
    0
    };

CINTERFACE_PROXY_VTABLE(10) _ISwUnibaseObjProxyVtbl = 
{
    &ISwUnibaseObj_ProxyInfo,
    &IID_ISwUnibaseObj,
    IUnknown_QueryInterface_Proxy,
    IUnknown_AddRef_Proxy,
    IUnknown_Release_Proxy ,
    0 /* (void *)-1 /* IDispatch::GetTypeInfoCount */ ,
    0 /* (void *)-1 /* IDispatch::GetTypeInfo */ ,
    0 /* (void *)-1 /* IDispatch::GetIDsOfNames */ ,
    0 /* IDispatch_Invoke_Proxy */ ,
    (void *)-1 /* ISwUnibaseObj::CreateUnibase */ ,
    (void *)-1 /* ISwUnibaseObj::ToolbarUpdate */ ,
    (void *)-1 /* ISwUnibaseObj::DisplayPMP */
};


static const PRPC_STUB_FUNCTION ISwUnibaseObj_table[] =
{
    STUB_FORWARDING_FUNCTION,
    STUB_FORWARDING_FUNCTION,
    STUB_FORWARDING_FUNCTION,
    STUB_FORWARDING_FUNCTION,
    NdrStubCall2,
    NdrStubCall2,
    NdrStubCall2
};

CInterfaceStubVtbl _ISwUnibaseObjStubVtbl =
{
    &IID_ISwUnibaseObj,
    &ISwUnibaseObj_ServerInfo,
    10,
    &ISwUnibaseObj_table[-3],
    CStdStubBuffer_DELEGATING_METHODS
};


/* Object interface: IPMPage, ver. 0.0,
   GUID={0x46A48BB7,0x9093,0x11D7,{0x87,0xE7,0x08,0x00,0x69,0x13,0x62,0xA7}} */


extern const MIDL_STUB_DESC Object_StubDesc;


#pragma code_seg(".orpc")
CINTERFACE_PROXY_VTABLE(7) _IPMPageProxyVtbl = 
{
    0,
    &IID_IPMPage,
    IUnknown_QueryInterface_Proxy,
    IUnknown_AddRef_Proxy,
    IUnknown_Release_Proxy ,
    0 /* (void *)-1 /* IDispatch::GetTypeInfoCount */ ,
    0 /* (void *)-1 /* IDispatch::GetTypeInfo */ ,
    0 /* (void *)-1 /* IDispatch::GetIDsOfNames */ ,
    0 /* IDispatch_Invoke_Proxy */
};


static const PRPC_STUB_FUNCTION IPMPage_table[] =
{
    STUB_FORWARDING_FUNCTION,
    STUB_FORWARDING_FUNCTION,
    STUB_FORWARDING_FUNCTION,
    STUB_FORWARDING_FUNCTION
};

CInterfaceStubVtbl _IPMPageStubVtbl =
{
    &IID_IPMPage,
    0,
    7,
    &IPMPage_table[-3],
    CStdStubBuffer_DELEGATING_METHODS
};


/* Object interface: IPart, ver. 0.0,
   GUID={0x46A48BAF,0x9093,0x11D7,{0x87,0xE7,0x08,0x00,0x69,0x13,0x62,0xA7}} */


extern const MIDL_STUB_DESC Object_StubDesc;


#pragma code_seg(".orpc")
CINTERFACE_PROXY_VTABLE(7) _IPartProxyVtbl = 
{
    0,
    &IID_IPart,
    IUnknown_QueryInterface_Proxy,
    IUnknown_AddRef_Proxy,
    IUnknown_Release_Proxy ,
    0 /* (void *)-1 /* IDispatch::GetTypeInfoCount */ ,
    0 /* (void *)-1 /* IDispatch::GetTypeInfo */ ,
    0 /* (void *)-1 /* IDispatch::GetIDsOfNames */ ,
    0 /* IDispatch_Invoke_Proxy */
};


static const PRPC_STUB_FUNCTION IPart_table[] =
{
    STUB_FORWARDING_FUNCTION,
    STUB_FORWARDING_FUNCTION,
    STUB_FORWARDING_FUNCTION,
    STUB_FORWARDING_FUNCTION
};

CInterfaceStubVtbl _IPartStubVtbl =
{
    &IID_IPart,
    0,
    7,
    &IPart_table[-3],
    CStdStubBuffer_DELEGATING_METHODS
};


/* Object interface: IAssembly, ver. 0.0,
   GUID={0x46A48BB1,0x9093,0x11D7,{0x87,0xE7,0x08,0x00,0x69,0x13,0x62,0xA7}} */


extern const MIDL_STUB_DESC Object_StubDesc;


#pragma code_seg(".orpc")
CINTERFACE_PROXY_VTABLE(7) _IAssemblyProxyVtbl = 
{
    0,
    &IID_IAssembly,
    IUnknown_QueryInterface_Proxy,
    IUnknown_AddRef_Proxy,
    IUnknown_Release_Proxy ,
    0 /* (void *)-1 /* IDispatch::GetTypeInfoCount */ ,
    0 /* (void *)-1 /* IDispatch::GetTypeInfo */ ,
    0 /* (void *)-1 /* IDispatch::GetIDsOfNames */ ,
    0 /* IDispatch_Invoke_Proxy */
};


static const PRPC_STUB_FUNCTION IAssembly_table[] =
{
    STUB_FORWARDING_FUNCTION,
    STUB_FORWARDING_FUNCTION,
    STUB_FORWARDING_FUNCTION,
    STUB_FORWARDING_FUNCTION
};

CInterfaceStubVtbl _IAssemblyStubVtbl =
{
    &IID_IAssembly,
    0,
    7,
    &IAssembly_table[-3],
    CStdStubBuffer_DELEGATING_METHODS
};


/* Object interface: IDrawing, ver. 0.0,
   GUID={0x46A48BB3,0x9093,0x11D7,{0x87,0xE7,0x08,0x00,0x69,0x13,0x62,0xA7}} */


extern const MIDL_STUB_DESC Object_StubDesc;


#pragma code_seg(".orpc")
CINTERFACE_PROXY_VTABLE(7) _IDrawingProxyVtbl = 
{
    0,
    &IID_IDrawing,
    IUnknown_QueryInterface_Proxy,
    IUnknown_AddRef_Proxy,
    IUnknown_Release_Proxy ,
    0 /* (void *)-1 /* IDispatch::GetTypeInfoCount */ ,
    0 /* (void *)-1 /* IDispatch::GetTypeInfo */ ,
    0 /* (void *)-1 /* IDispatch::GetIDsOfNames */ ,
    0 /* IDispatch_Invoke_Proxy */
};


static const PRPC_STUB_FUNCTION IDrawing_table[] =
{
    STUB_FORWARDING_FUNCTION,
    STUB_FORWARDING_FUNCTION,
    STUB_FORWARDING_FUNCTION,
    STUB_FORWARDING_FUNCTION
};

CInterfaceStubVtbl _IDrawingStubVtbl =
{
    &IID_IDrawing,
    0,
    7,
    &IDrawing_table[-3],
    CStdStubBuffer_DELEGATING_METHODS
};


/* Object interface: IDocView, ver. 0.0,
   GUID={0x46A48BB5,0x9093,0x11D7,{0x87,0xE7,0x08,0x00,0x69,0x13,0x62,0xA7}} */


extern const MIDL_STUB_DESC Object_StubDesc;


#pragma code_seg(".orpc")

static const MIDL_STUB_DESC Object_StubDesc = 
    {
    0,
    NdrOleAllocate,
    NdrOleFree,
    0,
    0,
    0,
    0,
    0,
    __MIDL_TypeFormatString.Format,
    1, /* -error bounds_check flag */
    0x20000, /* Ndr library version */
    0,
    0x50100a4, /* MIDL Version 5.1.164 */
    0,
    0,
    0,  /* notify & notify_flag routine table */
    1,  /* Flags */
    0,  /* Reserved3 */
    0,  /* Reserved4 */
    0   /* Reserved5 */
    };

CINTERFACE_PROXY_VTABLE(7) _IDocViewProxyVtbl = 
{
    0,
    &IID_IDocView,
    IUnknown_QueryInterface_Proxy,
    IUnknown_AddRef_Proxy,
    IUnknown_Release_Proxy ,
    0 /* (void *)-1 /* IDispatch::GetTypeInfoCount */ ,
    0 /* (void *)-1 /* IDispatch::GetTypeInfo */ ,
    0 /* (void *)-1 /* IDispatch::GetIDsOfNames */ ,
    0 /* IDispatch_Invoke_Proxy */
};


static const PRPC_STUB_FUNCTION IDocView_table[] =
{
    STUB_FORWARDING_FUNCTION,
    STUB_FORWARDING_FUNCTION,
    STUB_FORWARDING_FUNCTION,
    STUB_FORWARDING_FUNCTION
};

CInterfaceStubVtbl _IDocViewStubVtbl =
{
    &IID_IDocView,
    0,
    7,
    &IDocView_table[-3],
    CStdStubBuffer_DELEGATING_METHODS
};

#pragma data_seg(".rdata")

#if !defined(__RPC_WIN32__)
#error  Invalid build platform for this stub.
#endif

#if !(TARGET_IS_NT40_OR_LATER)
#error You need a Windows NT 4.0 or later to run this stub because it uses these features:
#error   -Oif or -Oicf, more than 32 methods in the interface.
#error However, your C/C++ compilation flags indicate you intend to run this app on earlier systems.
#error This app will die there with the RPC_X_WRONG_STUB_VERSION error.
#endif


static const MIDL_PROC_FORMAT_STRING __MIDL_ProcFormatString =
    {
        0,
        {

	/* Procedure CreateUnibase */

			0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/*  2 */	NdrFcLong( 0x0 ),	/* 0 */
/*  6 */	NdrFcShort( 0x7 ),	/* 7 */
#ifndef _ALPHA_
/*  8 */	NdrFcShort( 0x8 ),	/* x86, MIPS, PPC Stack size/offset = 8 */
#else
			NdrFcShort( 0x10 ),	/* Alpha Stack size/offset = 16 */
#endif
/* 10 */	NdrFcShort( 0x0 ),	/* 0 */
/* 12 */	NdrFcShort( 0x8 ),	/* 8 */
/* 14 */	0x4,		/* Oi2 Flags:  has return, */
			0x1,		/* 1 */

	/* Return value */

/* 16 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
#ifndef _ALPHA_
/* 18 */	NdrFcShort( 0x4 ),	/* x86, MIPS, PPC Stack size/offset = 4 */
#else
			NdrFcShort( 0x8 ),	/* Alpha Stack size/offset = 8 */
#endif
/* 20 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure ToolbarUpdate */

/* 22 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 24 */	NdrFcLong( 0x0 ),	/* 0 */
/* 28 */	NdrFcShort( 0x8 ),	/* 8 */
#ifndef _ALPHA_
/* 30 */	NdrFcShort( 0xc ),	/* x86, MIPS, PPC Stack size/offset = 12 */
#else
			NdrFcShort( 0x18 ),	/* Alpha Stack size/offset = 24 */
#endif
/* 32 */	NdrFcShort( 0x0 ),	/* 0 */
/* 34 */	NdrFcShort( 0x10 ),	/* 16 */
/* 36 */	0x4,		/* Oi2 Flags:  has return, */
			0x2,		/* 2 */

	/* Parameter status */

/* 38 */	NdrFcShort( 0x2150 ),	/* Flags:  out, base type, simple ref, srv alloc size=8 */
#ifndef _ALPHA_
/* 40 */	NdrFcShort( 0x4 ),	/* x86, MIPS, PPC Stack size/offset = 4 */
#else
			NdrFcShort( 0x8 ),	/* Alpha Stack size/offset = 8 */
#endif
/* 42 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Return value */

/* 44 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
#ifndef _ALPHA_
/* 46 */	NdrFcShort( 0x8 ),	/* x86, MIPS, PPC Stack size/offset = 8 */
#else
			NdrFcShort( 0x10 ),	/* Alpha Stack size/offset = 16 */
#endif
/* 48 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure DisplayPMP */

/* 50 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 52 */	NdrFcLong( 0x0 ),	/* 0 */
/* 56 */	NdrFcShort( 0x9 ),	/* 9 */
#ifndef _ALPHA_
/* 58 */	NdrFcShort( 0x8 ),	/* x86, MIPS, PPC Stack size/offset = 8 */
#else
			NdrFcShort( 0x10 ),	/* Alpha Stack size/offset = 16 */
#endif
/* 60 */	NdrFcShort( 0x0 ),	/* 0 */
/* 62 */	NdrFcShort( 0x8 ),	/* 8 */
/* 64 */	0x4,		/* Oi2 Flags:  has return, */
			0x1,		/* 1 */

	/* Return value */

/* 66 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
#ifndef _ALPHA_
/* 68 */	NdrFcShort( 0x4 ),	/* x86, MIPS, PPC Stack size/offset = 4 */
#else
			NdrFcShort( 0x8 ),	/* Alpha Stack size/offset = 8 */
#endif
/* 70 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

			0x0
        }
    };

static const MIDL_TYPE_FORMAT_STRING __MIDL_TypeFormatString =
    {
        0,
        {
			NdrFcShort( 0x0 ),	/* 0 */
/*  2 */	
			0x11, 0xc,	/* FC_RP [alloced_on_stack] [simple_pointer] */
/*  4 */	0x8,		/* FC_LONG */
			0x5c,		/* FC_PAD */

			0x0
        }
    };

const CInterfaceProxyVtbl * _SwUnibase_ProxyVtblList[] = 
{
    ( CInterfaceProxyVtbl *) &_ISwUnibaseObjProxyVtbl,
    ( CInterfaceProxyVtbl *) &_IPartProxyVtbl,
    ( CInterfaceProxyVtbl *) &_IAssemblyProxyVtbl,
    ( CInterfaceProxyVtbl *) &_IDrawingProxyVtbl,
    ( CInterfaceProxyVtbl *) &_IDocViewProxyVtbl,
    ( CInterfaceProxyVtbl *) &_IPMPageProxyVtbl,
    0
};

const CInterfaceStubVtbl * _SwUnibase_StubVtblList[] = 
{
    ( CInterfaceStubVtbl *) &_ISwUnibaseObjStubVtbl,
    ( CInterfaceStubVtbl *) &_IPartStubVtbl,
    ( CInterfaceStubVtbl *) &_IAssemblyStubVtbl,
    ( CInterfaceStubVtbl *) &_IDrawingStubVtbl,
    ( CInterfaceStubVtbl *) &_IDocViewStubVtbl,
    ( CInterfaceStubVtbl *) &_IPMPageStubVtbl,
    0
};

PCInterfaceName const _SwUnibase_InterfaceNamesList[] = 
{
    "ISwUnibaseObj",
    "IPart",
    "IAssembly",
    "IDrawing",
    "IDocView",
    "IPMPage",
    0
};

const IID *  _SwUnibase_BaseIIDList[] = 
{
    &IID_IDispatch,
    &IID_IDispatch,
    &IID_IDispatch,
    &IID_IDispatch,
    &IID_IDispatch,
    &IID_IDispatch,
    0
};


#define _SwUnibase_CHECK_IID(n)	IID_GENERIC_CHECK_IID( _SwUnibase, pIID, n)

int __stdcall _SwUnibase_IID_Lookup( const IID * pIID, int * pIndex )
{
    IID_BS_LOOKUP_SETUP

    IID_BS_LOOKUP_INITIAL_TEST( _SwUnibase, 6, 4 )
    IID_BS_LOOKUP_NEXT_TEST( _SwUnibase, 2 )
    IID_BS_LOOKUP_NEXT_TEST( _SwUnibase, 1 )
    IID_BS_LOOKUP_RETURN_RESULT( _SwUnibase, 6, *pIndex )
    
}

const ExtendedProxyFileInfo SwUnibase_ProxyFileInfo = 
{
    (PCInterfaceProxyVtblList *) & _SwUnibase_ProxyVtblList,
    (PCInterfaceStubVtblList *) & _SwUnibase_StubVtblList,
    (const PCInterfaceName * ) & _SwUnibase_InterfaceNamesList,
    (const IID ** ) & _SwUnibase_BaseIIDList,
    & _SwUnibase_IID_Lookup, 
    6,
    2,
    0, /* table of [async_uuid] interfaces */
    0, /* Filler1 */
    0, /* Filler2 */
    0  /* Filler3 */
};
