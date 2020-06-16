// SwUnibase.cpp : Implementation of CSwUnibase

#include "stdafx.h"
#include "SwUnibase.h"
#include "SwDocument.h"
#include "CSwUnibaseDialog.h"
#include <string>
#include <list>
#include "BitmapHandler.h"

// CSwUnibase
void CSwUnibase::AddCommandManager()
{
	CComPtr<ICommandGroup> icmdGroup;
	
	HRESULT hret = S_OK;

	CComObject<CBitmapHandler>::CreateInstance(&iBmp);
		long cmdIndex0;
		const int numDocumentTypes = 3;

	int docTypes[numDocumentTypes];

	docTypes[0] = swDocASSEMBLY,
	docTypes[1] = swDocDRAWING,
	docTypes[2] = swDocPART;


	CComBSTR title;
	title.LoadString(IDS_TOOLBAR_TITLE);

	CComBSTR hint;
	hint.LoadString(IDS_MENU_HINT);

	long cmdGroupErr = 0;
	bool ignorePrevious = false;
	VARIANT_BOOL getDataResult = VARIANT_FALSE;
	long count = 0;

	hret = iCmdMgr->GetCommandIDsCount(MAIN_CMD_GROUP_ID, &count);
	long * registryIDs = new long[count];


	HRESULT retVal = iCmdMgr->IGetGroupDataFromRegistry(MAIN_CMD_GROUP_ID, count, registryIDs, &getDataResult);
		long knownIDs[1] = {MAIN_ITEM_ID1};

	if(getDataResult)
	{
		if(!CompareIDs(registryIDs, count, knownIDs, 1)) // if the ID's don't match, reset the command group
		{
			ignorePrevious = true;
		}
	}

	
	iCmdMgr->CreateCommandGroup2(MAIN_CMD_GROUP_ID, title, title, hint, -1, ignorePrevious, &cmdGroupErr, &icmdGroup);

	CComBSTR smallImageFile;
	iBmp->CreateBitmapFileFromResource(IDB_TOOLBAR_SMALL, &smallImageFile);
	icmdGroup->put_SmallIconList(smallImageFile);

	CComBSTR largeImageFile;
	iBmp->CreateBitmapFileFromResource(IDB_TOOLBAR_LARGE, &largeImageFile);
	icmdGroup->put_LargeIconList(largeImageFile);

	CComBSTR largeIconFile;
	iBmp->CreateBitmapFileFromResource(IDB_ICON_LARGE, &largeIconFile);
	icmdGroup->put_LargeMainIcon(largeIconFile);

	CComBSTR smallIconFile;
	iBmp->CreateBitmapFileFromResource(IDB_ICON_SMALL, &smallIconFile);
	icmdGroup->put_SmallMainIcon(smallIconFile);

	int menuToolbarOption = (int)(swMenuItem | swToolbarItem);

	CComBSTR tip;
	CComBSTR callback;
	CComBSTR enable;
	VARIANT_BOOL cmdActivated;

	callback.LoadString(IDS_TOOLBAR_CALLBACK0);
	enable.LoadString(IDS_TOOLBAR_ENABLE0);
	tip.LoadString(IDS_TOOLBAR_TIP0);
	hint.LoadString(IDS_TOOLBAR_HINT0);
	hret = icmdGroup->AddCommandItem2(tip, -1, hint, tip, 0, callback, enable, MAIN_ITEM_ID1, menuToolbarOption, &cmdIndex0);



	icmdGroup->put_HasToolbar(true);
	icmdGroup->put_HasMenu(true);
	icmdGroup->Activate(&cmdActivated);

	bool bresult = false;

	CComPtr<IFlyoutGroup> flyGroup; 
	CComBSTR smallIcon;
	CComBSTR largeIcon;
	CComBSTR smallImageList;
	CComBSTR largeImageList;

	icmdGroup->get_SmallMainIcon(&smallIcon);
	icmdGroup->get_LargeMainIcon(&largeIcon);
	icmdGroup->get_SmallIconList(&smallImageList);
	icmdGroup->get_LargeIconList(&largeImageList);
	
	callback.LoadString(IDS_FLYOUT_CALLBACK);
	enable.LoadString(IDS_FLYOUT_ENABLE0);
	tip.LoadString(IDS_FLYOUT_TIP0);
	hint.LoadString(IDS_FLYOUT_HINT0);
	iCmdMgr->CreateFlyoutGroup(FLYOUT_GROUP_ID, L"Dynamic Flyout", L"Clicking this opens the Flyout menu", L"Click this to open the Flyout menu", smallIcon, largeIcon, smallImageList, largeImageList, callback, enable, &flyGroup);

	flyGroup->RemoveAllCommandItems();

	long flyoutType = (long)swCommandFlyoutStyle_Simple;
	flyGroup->put_FlyoutType(flyoutType);
	
	for(int i=0; i < numDocumentTypes; i++)
	{
		CComPtr<ICommandTab> cmdTab = NULL;
		long tabCount, docType = docTypes[i];

		iCmdMgr->GetCommandTabCount(docType, &tabCount);

		// check if tab exists
	    iCmdMgr->GetCommandTab(docType, title, &cmdTab);

		//If tab exists, but we have ignored the registry info, re-create the tab. Otherwise the ids won't matchup and the tab will be blank 
		if((cmdTab != NULL) && !getDataResult || ignorePrevious)
		{
			VARIANT_BOOL res;
			iCmdMgr->RemoveCommandTab(cmdTab, &res);
			cmdTab = NULL;
		}

		//If cmdTab is null, must be first load (possibly after reset), add the commands to the tabs
		if(cmdTab == NULL)
		{

						
			// if not, add one
			  iCmdMgr->AddCommandTab(docType, title, &cmdTab);

			  CComPtr<ICommandTabBox> cmdBox;

			  cmdTab->AddCommandTabBox(&cmdBox);
			  
			  // create 3 commands on this tab
			  long CommandIDCount = 3;
			  
			  
			  long CommandIDs[3];
			  long TextDisplayStyles[3];
			  
			  long cmdID = 0;

			  // the 2 command buttons have different text styles
			  icmdGroup->get_CommandID(cmdIndex0, &cmdID);      
			  CommandIDs[0] = cmdID;
			  TextDisplayStyles[0] = swCommandTabButton_TextHorizontal;

			  icmdGroup->get_ToolbarId(&cmdID);
			  CommandIDs[2] = cmdID;
			  TextDisplayStyles[2] = swCommandTabButton_TextHorizontal;

			  VARIANT_BOOL vbResult = VARIANT_FALSE;

			  cmdBox->IAddCommands(CommandIDCount, CommandIDs, TextDisplayStyles, &vbResult);

			  long CommandIDs1[1];
			  long TextDisplayStyles1[1];
			  
			  CommandIDCount = 1;

			  flyGroup->get_CmdID(&cmdID);
			  CommandIDs1[0] = cmdID;
			  TextDisplayStyles1[0] = swCommandTabButton_TextBelow | swCommandTabButton_ActionFlyout;


			  CComPtr<ICommandTabBox> cmdBox1;

			  cmdTab->AddCommandTabBox(&cmdBox1);

			  cmdBox1->IAddCommands(CommandIDCount, CommandIDs1, TextDisplayStyles1, &vbResult);
				  
			  CComPtr<ICommandTabBox> cmdBoxNew1; 

			  cmdTab->AddSeparator(cmdBox1, cmdID, &cmdBoxNew1);

		}
	
	}

	//Clean up
	delete [] registryIDs;
}


void CSwUnibase::RemoveCommandManager()
{
	iBmp->Dispose();
	iBmp->Release();

	VARIANT_BOOL cmdRemoved = VARIANT_FALSE; 
	VARIANT_BOOL rtOnly = VARIANT_TRUE;
	int ret = 0;

	ret = iCmdMgr->RemoveFlyoutGroup(FLYOUT_GROUP_ID, &cmdRemoved);
	ret = iCmdMgr->RemoveCommandGroup(MAIN_CMD_GROUP_ID, &cmdRemoved);
}

bool CSwUnibase::CompareIDs(long * storedIDs, long storedSize, long * addinIDs, long addinSize)
{
	std::list<long> storedList;
    std::list<long> addinList;

	for(int i = 0; i < storedSize; i++)
	{
		storedList.push_front(storedIDs[i]);
	}
	for(int j = 0; j < addinSize; j++)
	{
		addinList.push_front(addinIDs[j]);
	}

    addinList.sort();
    storedList.sort();

    if (addinList.size() != storedList.size())
    {
        return false;
    }
    else
    {
		std::list<long>::iterator al = addinList.begin();
		//Iterate through the two lists and make sure they match
		for(std::list<long>::iterator sl = storedList.begin(); sl != storedList.end(); sl++)
		{
			if(*al != *sl)
			{
				return false;
			}
			al++;
		}
    }
    return true;
}


STDMETHODIMP CSwUnibase::OnDocChange(void)
{
	// TODO: Add your implementation code here
	return S_OK;
}

//Called after a document is opened
STDMETHODIMP CSwUnibase::OnFileOpenPostNotify(BSTR fileName)
{
	HRESULT hres = S_OK;
	hres = AttachEventsToAllDocuments();
	return hres;
}

//Called when a new document is created or a document is loaded
STDMETHODIMP CSwUnibase::OnDocLoad(BSTR docTitle, BSTR docPath)
{

	return S_OK;
}
VARIANT_BOOL CSwUnibase::AttachEventHandlers()
{
	VARIANT_BOOL attached = VARIANT_TRUE;
	this->m_libid = LIBID_SldWorks;
	this->m_wMajorVerNum = GetSldWorksTlbMajor();
	this->m_wMinorVerNum = 0;

	CSldWorksEvents::_tih.m_wMajor = this->m_wMajorVerNum;

	// Connect to the SldWorks event sink
	HRESULT success = this->DispEventAdvise(iSwApp, &__uuidof(DSldWorksEvents));

	if (success != S_OK)
		return VARIANT_FALSE;

	// Connect event handlers to all previously open documents
	success = AttachEventsToAllDocuments();
	if (success != S_OK)
		attached = VARIANT_FALSE;
	return attached;
}

HRESULT CSwUnibase::AttachEventsToAllDocuments()
{
	VARIANT_BOOL attached = VARIANT_TRUE;
	USES_CONVERSION;

	TMapIUnknownToDocument::iterator iter;

	CComPtr<IModelDoc2> iModelDoc2;
	iSwApp->IGetFirstDocument2(&iModelDoc2);
	while (iModelDoc2 != NULL)
	{
			iter = openDocs.find(iModelDoc2);
			if (iter == openDocs.end())
			{
				ATLTRACE("\tCSwUnibase::DocumentLoadNotify current part not found\n");
				attached = AttachModelEventHandler(iModelDoc2);
				if(!attached)
					return E_FAIL;
			}
		
			else
			{
				VARIANT_BOOL connected = VARIANT_FALSE;
				CComObject<CSwDocument> *docHandler = NULL;
				docHandler = (CComObject<CSwDocument>*)openDocs[iModelDoc2];
				
				if (docHandler != NULL)
				{
				    connected = docHandler->AttachModelViewEventHandlers();
				}
			}
		
		CComPtr<IModelDoc2> iNextModelDoc2;
		iModelDoc2->IGetNext(&iNextModelDoc2);
		iModelDoc2.Release();
		iModelDoc2 = iNextModelDoc2;
	}
	return S_OK;
}

//Create an event handling object for this document and listen for the model's events
VARIANT_BOOL CSwUnibase::AttachModelEventHandler(CComPtr<IModelDoc2> iModelDoc2)
{
	VARIANT_BOOL attached = VARIANT_TRUE;
	if (iModelDoc2 == NULL)
		return VARIANT_FALSE;
	
	TMapIUnknownToDocument::iterator iter;
	iter = openDocs.find(iModelDoc2);
	if (iter == openDocs.end())
	{
		//Cretae a new Document event handler
		CComObject<CSwDocument> *pDoc;
		CComObject<CSwDocument>::CreateInstance( &pDoc);
		pDoc->Init((CComObject<CSwUnibase>*)this, iModelDoc2);

		//Listen for the doc's events
		attached = pDoc->AttachEventHandlers();
		if(!attached)
			return VARIANT_FALSE;

		//Add it to the map
		openDocs.insert(TMapIUnknownToDocument::value_type(iModelDoc2, pDoc));
		iModelDoc2.p->AddRef();
		pDoc->AddRef();
	}
	return attached;
}

//Stop listening for SolidWorks Events
VARIANT_BOOL CSwUnibase::DetachEventHandlers()
{
	VARIANT_BOOL detached = VARIANT_TRUE;

	// Disconnect from the SldWorks event sink
	HRESULT success = this->DispEventUnadvise(iSwApp, &__uuidof(DSldWorksEvents));

	CSldWorksEvents::_tih.m_plibid = &GUID_NULL;

	if (success != S_OK)
		return VARIANT_FALSE;

	// Disconnect all event handlers
	TMapIUnknownToDocument::iterator iter;
	iter = openDocs.begin();

	for (iter = openDocs.begin(); iter != openDocs.end(); /*iter++*/) //The iteration is performed inside the loop
	{
		TMapIUnknownToDocument::value_type obj = *iter;
		iter++;

		CComObject<CSwDocument> *pDoc = (CComObject<CSwDocument>*)obj.second;
		detached = pDoc->DetachEventHandlers();
	}
	return detached;
}

//Stop listening for events on the specified model
VARIANT_BOOL CSwUnibase::DetachModelEventHandler(IUnknown *iUnk)
{
	VARIANT_BOOL detached = VARIANT_TRUE;
	TMapIUnknownToDocument::iterator iter;

	iter = openDocs.find(iUnk);
	if (iter != openDocs.end())
	{
		TMapIUnknownToDocument::value_type obj = *iter;
		obj.first->Release();

		CComObject<CSwDocument> *pDoc = (CComObject<CSwDocument>*)obj.second;
		pDoc->Release();

		openDocs.erase(iter);
	}
	return detached;
}

//Called when the active model document changes in SolidWorks
STDMETHODIMP CSwUnibase::OnModelDocChange(void)
{
	// TODO: Add your implementation code here
	return S_OK;
}

//Called when a new file is created
STDMETHODIMP CSwUnibase::OnFileNew(LPDISPATCH newDoc, long docType, BSTR templateName)
{
	HRESULT hres = S_OK;
	hres = AttachEventsToAllDocuments();
   return hres;
}

STDMETHODIMP CSwUnibase::ConnectToSW(LPDISPATCH ThisSW, long Cookie, VARIANT_BOOL * IsConnected)
{
	ThisSW->QueryInterface(__uuidof(ISldWorks), (void**)&iSwApp);
	addinID = Cookie;
	iSwApp->GetCommandManager(Cookie,&iCmdMgr);

	VARIANT_BOOL status = VARIANT_FALSE;

	iSwApp->SetAddinCallbackInfo((long)_AtlBaseModule.GetModuleInstance(), static_cast<ISwUnibase*>(this), addinID, &status);
	//Get the current type library version.
	{
		USES_CONVERSION;
		CComBSTR bstrNum;
		std::string strNum;
		char *buffer;

		iSwApp->RevisionNumber(&bstrNum);

		strNum = W2A(bstrNum);
		m_swMajNum = strtol(strNum.c_str(), &buffer, 10 );

		m_swMinNum=0;

	}
	//Create the addin's UI
	AddCommandManager();
	//Listen for events
	*IsConnected = AttachEventHandlers();
	*IsConnected = VARIANT_TRUE;
	return S_OK;
}
STDMETHODIMP CSwUnibase::DisconnectFromSW(VARIANT_BOOL * IsDisconnected)
{
	//Remove the addin's UI
	RemoveCommandManager();
	//Stop listening for events
	*IsDisconnected = DetachEventHandlers();

	iCmdMgr.Release();
	//Make sure you release the SolidWorks pointer last
	iSwApp.Release();

	return S_OK;
}

STDMETHODIMP CSwUnibase::ToolbarCallback0(void)
{
	// TODO: Add your implementation code here

	{
		AFX_MANAGE_STATE(AfxGetStaticModuleState());
		
		CSwUnibaseDialog dlg(AfxGetMainWnd());
		dlg.DoModal();

	}
	return S_OK;
}

STDMETHODIMP CSwUnibase::ToolbarEnable0(long* status)
{
	// TODO: Add your implementation code here
	*status = 1;
	return S_OK;
}

STDMETHODIMP CSwUnibase::FlyoutCallback(void)
{
	CComBSTR tip;
	CComBSTR callback;
	CComBSTR enable;
	CComBSTR hint;
	CComPtr<IFlyoutGroup> flyGroup;

	HRESULT hr = iCmdMgr->GetFlyoutGroup(FLYOUT_GROUP_ID, &flyGroup);
	flyGroup->RemoveAllCommandItems();
	
	callback.LoadString(IDS_FLYOUT_CALLBACK0);	
	enable.LoadString(IDS_FLYOUT_ENABLE_CALLBACK0);
	hint.LoadString(IDS_FLYOUT_HINT0);
	long cmdCount;
	flyGroup->AddCommandItem(GetCurrentFile(), hint, 0, callback, enable, &cmdCount);

	callback.LoadString(IDS_FLYOUT_CALLBACK1);
	enable.LoadString(IDS_FLYOUT_ENABLE_CALLBACK0);
	hint.LoadString(IDS_FLYOUT_HINT1);
	flyGroup->AddCommandItem(L"FlyoutCommand 1", hint, 0, callback, enable, &cmdCount);

	return S_OK;
}

STDMETHODIMP CSwUnibase::FlyoutCallback0(void)
{
	// TODO: Add your implementation code here
	::WinExec("Notepad.exe", SW_SHOW);
	return S_OK;
}

STDMETHODIMP CSwUnibase::FlyoutCallback1(void)
{
	// TODO: Add your implementation code here
	iSwApp->SendMsgToUser(L"This is the second Flyout Callback item");

	return S_OK;
}

STDMETHODIMP CSwUnibase::FlyoutEnable0(long* status)
{
	// TODO: Add your implementation code here
	*status = 1;
	return S_OK;
}

STDMETHODIMP CSwUnibase::FlyoutEnableCallback0(long* status)
{
	// TODO: Add your implementation code here
	*status = 1;
	return S_OK;
}

BSTR CSwUnibase::GetCurrentFile()
{	
	BSTR ret = SysAllocString(L"No File Currently Open");
	CComPtr<IModelDoc2> iSwModel;

	iSwApp->get_IActiveDoc2(&iSwModel);
	if(iSwModel != NULL)
	{
		iSwModel->GetTitle(&ret);
	}

	return ret;
}

