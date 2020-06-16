/*********************************************************************
**  NAME:  SwUnibase_i.h
**
**       SolidWorks generated file.
**
** CONTAINS:
**
**    COPYRIGHT 2014 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       SwUnibase_i.h , 24.2
**    DATE AND TIME OF LAST  MODIFICATION
**       08/13/14 , 15:04:57
*********************************************************************/

/* this ALWAYS GENERATED file contains the definitions for the interfaces */


 /* File created by MIDL compiler version 7.00.0555 */
/* at Wed Aug 06 17:54:46 2014
 */
/* Compiler settings for SwUnibase.idl:
    Oicf, W1, Zp8, env=Win32 (32b run), target_arch=X86 7.00.0555 
    protocol : dce , ms_ext, c_ext, robust
    error checks: allocation ref bounds_check enum stub_data 
    VC __declspec() decoration level: 
         __declspec(uuid()), __declspec(selectany), __declspec(novtable)
         DECLSPEC_UUID(), MIDL_INTERFACE()
*/
/* @@MIDL_FILE_HEADING(  ) */

#pragma warning( disable: 4049 )  /* more than 64k source lines */


/* verify that the <rpcndr.h> version is high enough to compile this file*/
#ifndef __REQUIRED_RPCNDR_H_VERSION__
#define __REQUIRED_RPCNDR_H_VERSION__ 475
#endif

#include "rpc.h"
#include "rpcndr.h"

#ifndef __RPCNDR_H_VERSION__
#error this stub requires an updated version of <rpcndr.h>
#endif // __RPCNDR_H_VERSION__

#ifndef COM_NO_WINDOWS_H
#include "windows.h"
#include "ole2.h"
#endif /*COM_NO_WINDOWS_H*/

#ifndef __SwUnibase_i_h__
#define __SwUnibase_i_h__

#if defined(_MSC_VER) && (_MSC_VER >= 1020)
#pragma once
#endif

/* Forward Declarations */ 

#ifndef __ISwUnibase_FWD_DEFINED__
#define __ISwUnibase_FWD_DEFINED__
typedef interface ISwUnibase ISwUnibase;
#endif 	/* __ISwUnibase_FWD_DEFINED__ */


#ifndef __ISwDocument_FWD_DEFINED__
#define __ISwDocument_FWD_DEFINED__
typedef interface ISwDocument ISwDocument;
#endif 	/* __ISwDocument_FWD_DEFINED__ */


#ifndef __IDocView_FWD_DEFINED__
#define __IDocView_FWD_DEFINED__
typedef interface IDocView IDocView;
#endif 	/* __IDocView_FWD_DEFINED__ */


#ifndef __IBitmapHandler_FWD_DEFINED__
#define __IBitmapHandler_FWD_DEFINED__
typedef interface IBitmapHandler IBitmapHandler;
#endif 	/* __IBitmapHandler_FWD_DEFINED__ */


#ifndef __IPMPageHandler_FWD_DEFINED__
#define __IPMPageHandler_FWD_DEFINED__
typedef interface IPMPageHandler IPMPageHandler;
#endif 	/* __IPMPageHandler_FWD_DEFINED__ */


#ifndef __IUserPropertyManagerPage_FWD_DEFINED__
#define __IUserPropertyManagerPage_FWD_DEFINED__
typedef interface IUserPropertyManagerPage IUserPropertyManagerPage;
#endif 	/* __IUserPropertyManagerPage_FWD_DEFINED__ */


#ifndef __SwUnibase_FWD_DEFINED__
#define __SwUnibase_FWD_DEFINED__

#ifdef __cplusplus
typedef class SwUnibase SwUnibase;
#else
typedef struct SwUnibase SwUnibase;
#endif /* __cplusplus */

#endif 	/* __SwUnibase_FWD_DEFINED__ */


#ifndef __SwDocument_FWD_DEFINED__
#define __SwDocument_FWD_DEFINED__

#ifdef __cplusplus
typedef class SwDocument SwDocument;
#else
typedef struct SwDocument SwDocument;
#endif /* __cplusplus */

#endif 	/* __SwDocument_FWD_DEFINED__ */


#ifndef __DocView_FWD_DEFINED__
#define __DocView_FWD_DEFINED__

#ifdef __cplusplus
typedef class DocView DocView;
#else
typedef struct DocView DocView;
#endif /* __cplusplus */

#endif 	/* __DocView_FWD_DEFINED__ */


#ifndef __BitmapHandler_FWD_DEFINED__
#define __BitmapHandler_FWD_DEFINED__

#ifdef __cplusplus
typedef class BitmapHandler BitmapHandler;
#else
typedef struct BitmapHandler BitmapHandler;
#endif /* __cplusplus */

#endif 	/* __BitmapHandler_FWD_DEFINED__ */


#ifndef __PMPageHandler_FWD_DEFINED__
#define __PMPageHandler_FWD_DEFINED__

#ifdef __cplusplus
typedef class PMPageHandler PMPageHandler;
#else
typedef struct PMPageHandler PMPageHandler;
#endif /* __cplusplus */

#endif 	/* __PMPageHandler_FWD_DEFINED__ */


#ifndef __UserPropertyManagerPage_FWD_DEFINED__
#define __UserPropertyManagerPage_FWD_DEFINED__

#ifdef __cplusplus
typedef class UserPropertyManagerPage UserPropertyManagerPage;
#else
typedef struct UserPropertyManagerPage UserPropertyManagerPage;
#endif /* __cplusplus */

#endif 	/* __UserPropertyManagerPage_FWD_DEFINED__ */


/* header files for imported files */
#include "oaidl.h"
#include "ocidl.h"

#ifdef __cplusplus
extern "C"{
#endif 


#ifndef __ISwUnibase_INTERFACE_DEFINED__
#define __ISwUnibase_INTERFACE_DEFINED__

/* interface ISwUnibase */
/* [unique][helpstring][nonextensible][dual][uuid][object] */ 


EXTERN_C const IID IID_ISwUnibase;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("547A6AD7-A7D0-4931-8A28-5CEC5669871D")
    ISwUnibase : public IDispatch
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ToolbarCallback0( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ToolbarEnable0( 
            /* [retval][out] */ long *status) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE FlyoutCallback( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE FlyoutCallback0( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE FlyoutCallback1( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE FlyoutEnable0( 
            /* [retval][out] */ long *status) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE FlyoutEnableCallback0( 
            /* [retval][out] */ long *status) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct ISwUnibaseVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            ISwUnibase * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            __RPC__deref_out  void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            ISwUnibase * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            ISwUnibase * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            ISwUnibase * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            ISwUnibase * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            ISwUnibase * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            ISwUnibase * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ToolbarCallback0 )( 
            ISwUnibase * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ToolbarEnable0 )( 
            ISwUnibase * This,
            /* [retval][out] */ long *status);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *FlyoutCallback )( 
            ISwUnibase * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *FlyoutCallback0 )( 
            ISwUnibase * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *FlyoutCallback1 )( 
            ISwUnibase * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *FlyoutEnable0 )( 
            ISwUnibase * This,
            /* [retval][out] */ long *status);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *FlyoutEnableCallback0 )( 
            ISwUnibase * This,
            /* [retval][out] */ long *status);
        
        END_INTERFACE
    } ISwUnibaseVtbl;

    interface ISwUnibase
    {
        CONST_VTBL struct ISwUnibaseVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define ISwUnibase_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define ISwUnibase_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define ISwUnibase_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define ISwUnibase_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define ISwUnibase_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define ISwUnibase_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define ISwUnibase_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define ISwUnibase_ToolbarCallback0(This)	\
    ( (This)->lpVtbl -> ToolbarCallback0(This) ) 

#define ISwUnibase_ToolbarEnable0(This,status)	\
    ( (This)->lpVtbl -> ToolbarEnable0(This,status) ) 

#define ISwUnibase_FlyoutCallback(This)	\
    ( (This)->lpVtbl -> FlyoutCallback(This) ) 

#define ISwUnibase_FlyoutCallback0(This)	\
    ( (This)->lpVtbl -> FlyoutCallback0(This) ) 

#define ISwUnibase_FlyoutCallback1(This)	\
    ( (This)->lpVtbl -> FlyoutCallback1(This) ) 

#define ISwUnibase_FlyoutEnable0(This,status)	\
    ( (This)->lpVtbl -> FlyoutEnable0(This,status) ) 

#define ISwUnibase_FlyoutEnableCallback0(This,status)	\
    ( (This)->lpVtbl -> FlyoutEnableCallback0(This,status) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __ISwUnibase_INTERFACE_DEFINED__ */


#ifndef __ISwDocument_INTERFACE_DEFINED__
#define __ISwDocument_INTERFACE_DEFINED__

/* interface ISwDocument */
/* [unique][helpstring][nonextensible][dual][uuid][object] */ 


EXTERN_C const IID IID_ISwDocument;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("DB6A55BA-3107-4D83-9AAE-C870B1676AB6")
    ISwDocument : public IDispatch
    {
    public:
    };
    
#else 	/* C style interface */

    typedef struct ISwDocumentVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            ISwDocument * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            __RPC__deref_out  void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            ISwDocument * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            ISwDocument * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            ISwDocument * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            ISwDocument * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            ISwDocument * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            ISwDocument * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        END_INTERFACE
    } ISwDocumentVtbl;

    interface ISwDocument
    {
        CONST_VTBL struct ISwDocumentVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define ISwDocument_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define ISwDocument_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define ISwDocument_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define ISwDocument_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define ISwDocument_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define ISwDocument_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define ISwDocument_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __ISwDocument_INTERFACE_DEFINED__ */


#ifndef __IDocView_INTERFACE_DEFINED__
#define __IDocView_INTERFACE_DEFINED__

/* interface IDocView */
/* [unique][helpstring][nonextensible][dual][uuid][object] */ 


EXTERN_C const IID IID_IDocView;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("4A336EFB-40A0-420D-A25F-C84007E3893B")
    IDocView : public IDispatch
    {
    public:
    };
    
#else 	/* C style interface */

    typedef struct IDocViewVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IDocView * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            __RPC__deref_out  void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IDocView * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IDocView * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IDocView * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IDocView * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IDocView * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IDocView * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        END_INTERFACE
    } IDocViewVtbl;

    interface IDocView
    {
        CONST_VTBL struct IDocViewVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IDocView_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IDocView_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IDocView_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IDocView_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IDocView_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IDocView_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IDocView_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IDocView_INTERFACE_DEFINED__ */


#ifndef __IBitmapHandler_INTERFACE_DEFINED__
#define __IBitmapHandler_INTERFACE_DEFINED__

/* interface IBitmapHandler */
/* [unique][helpstring][nonextensible][dual][uuid][object] */ 


EXTERN_C const IID IID_IBitmapHandler;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("DCCD1577-CC64-4459-8E92-8A565B9C3BB4")
    IBitmapHandler : public IDispatch
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE CreateBitmapFileFromResource( 
            /* [in] */ DWORD resID,
            /* [retval][out] */ BSTR *retval) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Dispose( void) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IBitmapHandlerVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IBitmapHandler * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            __RPC__deref_out  void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IBitmapHandler * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IBitmapHandler * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IBitmapHandler * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IBitmapHandler * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IBitmapHandler * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IBitmapHandler * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *CreateBitmapFileFromResource )( 
            IBitmapHandler * This,
            /* [in] */ DWORD resID,
            /* [retval][out] */ BSTR *retval);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Dispose )( 
            IBitmapHandler * This);
        
        END_INTERFACE
    } IBitmapHandlerVtbl;

    interface IBitmapHandler
    {
        CONST_VTBL struct IBitmapHandlerVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IBitmapHandler_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IBitmapHandler_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IBitmapHandler_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IBitmapHandler_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IBitmapHandler_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IBitmapHandler_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IBitmapHandler_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IBitmapHandler_CreateBitmapFileFromResource(This,resID,retval)	\
    ( (This)->lpVtbl -> CreateBitmapFileFromResource(This,resID,retval) ) 

#define IBitmapHandler_Dispose(This)	\
    ( (This)->lpVtbl -> Dispose(This) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IBitmapHandler_INTERFACE_DEFINED__ */


#ifndef __IPMPageHandler_INTERFACE_DEFINED__
#define __IPMPageHandler_INTERFACE_DEFINED__

/* interface IPMPageHandler */
/* [unique][helpstring][nonextensible][dual][uuid][object] */ 


EXTERN_C const IID IID_IPMPageHandler;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("CEC42F44-A5F2-4633-B4CB-9483C60D30FC")
    IPMPageHandler : public IDispatch
    {
    public:
    };
    
#else 	/* C style interface */

    typedef struct IPMPageHandlerVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IPMPageHandler * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            __RPC__deref_out  void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IPMPageHandler * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IPMPageHandler * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IPMPageHandler * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IPMPageHandler * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IPMPageHandler * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IPMPageHandler * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        END_INTERFACE
    } IPMPageHandlerVtbl;

    interface IPMPageHandler
    {
        CONST_VTBL struct IPMPageHandlerVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IPMPageHandler_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IPMPageHandler_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IPMPageHandler_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IPMPageHandler_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IPMPageHandler_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IPMPageHandler_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IPMPageHandler_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IPMPageHandler_INTERFACE_DEFINED__ */


#ifndef __IUserPropertyManagerPage_INTERFACE_DEFINED__
#define __IUserPropertyManagerPage_INTERFACE_DEFINED__

/* interface IUserPropertyManagerPage */
/* [unique][helpstring][nonextensible][dual][uuid][object] */ 


EXTERN_C const IID IID_IUserPropertyManagerPage;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("287C3DC8-5A30-499A-9465-A4462FB42377")
    IUserPropertyManagerPage : public IDispatch
    {
    public:
    };
    
#else 	/* C style interface */

    typedef struct IUserPropertyManagerPageVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IUserPropertyManagerPage * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            __RPC__deref_out  void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IUserPropertyManagerPage * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IUserPropertyManagerPage * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IUserPropertyManagerPage * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IUserPropertyManagerPage * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IUserPropertyManagerPage * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IUserPropertyManagerPage * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        END_INTERFACE
    } IUserPropertyManagerPageVtbl;

    interface IUserPropertyManagerPage
    {
        CONST_VTBL struct IUserPropertyManagerPageVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IUserPropertyManagerPage_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IUserPropertyManagerPage_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IUserPropertyManagerPage_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IUserPropertyManagerPage_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IUserPropertyManagerPage_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IUserPropertyManagerPage_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IUserPropertyManagerPage_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IUserPropertyManagerPage_INTERFACE_DEFINED__ */



#ifndef __SwUnibaseLib_LIBRARY_DEFINED__
#define __SwUnibaseLib_LIBRARY_DEFINED__

/* library SwUnibaseLib */
/* [helpstring][version][uuid] */ 


EXTERN_C const IID LIBID_SwUnibaseLib;

EXTERN_C const CLSID CLSID_SwUnibase;

#ifdef __cplusplus

class DECLSPEC_UUID("3A46B873-BBA2-4787-8489-9FDA19FBE5D6")
SwUnibase;
#endif

EXTERN_C const CLSID CLSID_SwDocument;

#ifdef __cplusplus

class DECLSPEC_UUID("569655C9-D4B8-470A-98AC-CF7BCA983353")
SwDocument;
#endif

EXTERN_C const CLSID CLSID_DocView;

#ifdef __cplusplus

class DECLSPEC_UUID("65866DB4-8A06-4CE9-986F-C386791BF828")
DocView;
#endif

EXTERN_C const CLSID CLSID_BitmapHandler;

#ifdef __cplusplus

class DECLSPEC_UUID("B3E4297F-E47E-4E20-997B-1C5E5782E645")
BitmapHandler;
#endif

EXTERN_C const CLSID CLSID_PMPageHandler;

#ifdef __cplusplus

class DECLSPEC_UUID("E7393FA7-F505-4622-8057-2B408156CC24")
PMPageHandler;
#endif

EXTERN_C const CLSID CLSID_UserPropertyManagerPage;

#ifdef __cplusplus

class DECLSPEC_UUID("B2F593EF-9EEC-4C59-98F9-78930835D8C9")
UserPropertyManagerPage;
#endif
#endif /* __SwUnibaseLib_LIBRARY_DEFINED__ */

/* Additional Prototypes for ALL interfaces */

unsigned long             __RPC_USER  BSTR_UserSize(     unsigned long *, unsigned long            , BSTR * ); 
unsigned char * __RPC_USER  BSTR_UserMarshal(  unsigned long *, unsigned char *, BSTR * ); 
unsigned char * __RPC_USER  BSTR_UserUnmarshal(unsigned long *, unsigned char *, BSTR * ); 
void                      __RPC_USER  BSTR_UserFree(     unsigned long *, BSTR * ); 

/* end of Additional Prototypes */

#ifdef __cplusplus
}
#endif

#endif


