/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <wx/wx.h> 
#include <wx/stdpaths.h>
#include <wx/filename.h>
#include <windows.h>
#include <winver.h>
#include "Controller.h"
#include "Constants.h"
#include "MainFrame.h"


extern MainFrame* gMainFrame;


Controller::Controller() {
  draggedFolderItemId_ = -1;  
  stopWatch_.Start();
  user_.reset(new User());
}


long Controller::GetTimer() {
  return stopWatch_.Time();
}


wxString Controller::GetVersionString(const wxString& filePath) {
  // Borrowed from FileZilla

	//Fill the version info
  TCHAR fullpath[MAX_PATH + 10];

  if (filePath == wxEmptyString) {
	  GetModuleFileName(0, fullpath, MAX_PATH + 10);
  } else {
    const wxChar* filePathChars = filePath.c_str();  
    for (int i = 0; i < filePath.Len(); i++) {
      fullpath[i] = filePathChars[i];
    }
    fullpath[filePath.Len()] = _T('\0');
  }

	TCHAR *str = new TCHAR[_tcslen(fullpath) + 1];
	_tcscpy(str, fullpath);
	DWORD tmp = 0;
	DWORD len = GetFileVersionInfoSize(str, &tmp);
	LPVOID pBlock = new char[len];
	GetFileVersionInfo(str, 0, len, pBlock);
	LPVOID ptr;
	UINT ptrlen;
	
	TCHAR SubBlock[50];
			
	// Structure used to store enumerated languages and code pages.
	struct LANGANDCODEPAGE {
		WORD wLanguage;
		WORD wCodePage;
	} *lpTranslate;

	UINT cbTranslate;
			
	// Read the list of languages and code pages.
	if (VerQueryValue(pBlock, 
				_T("\\VarFileInfo\\Translation"),
				(LPVOID*)&lpTranslate,
				&cbTranslate))
	{
	}
	wxString version;
 
	//Format the versionstring
	if (VerQueryValue(pBlock, _T("\\"), &ptr, &ptrlen)) {
		VS_FIXEDFILEINFO *fi = (VS_FIXEDFILEINFO*)ptr;

    version = wxString::Format(_T("%d.%d.%d.%d"),
               HIWORD(fi->dwFileVersionMS), LOWORD(fi->dwFileVersionMS),
               HIWORD(fi->dwFileVersionLS), LOWORD(fi->dwFileVersionLS));
  }

	delete [] str;
	delete [] pBlock;
	return version;
}


void Controller::SetDraggedFolderItem(int folderItemId) {
  draggedFolderItemId_ = folderItemId;
}


FolderItemSP Controller::GetDraggedFolderItem() {
  if (draggedFolderItemId_ < 0 || !user_) {
    FolderItemSP nullOutput;
    return nullOutput;
  }
  return user_->GetFolderItemById(draggedFolderItemId_);
}


UserSP Controller::GetUser() {
  return user_;
}


void Controller::User_FolderItemCollectionChange() {
  gMainFrame->GetIconPanel()->InvalidateIcons();
  GetUser()->ScheduleSave();
}


void Controller::User_FolderItemChange(FolderItemSP folderItem) {
  FolderItemRendererSP renderer = gMainFrame->GetIconPanel()->GetRendererFromFolderItem(*folderItem);
  if (!renderer.get()) {
    // The folder item is not on the icon panel. It
    // may happen if it has just been created.
    return;
  }

  renderer->InvalidateControlBitmap();
  GetUser()->ScheduleSave();
}


void Controller::User_LocaleChange() {
  gMainFrame->Localize();
}


void Controller::User_IconSizeChange() {
  gMainFrame->GetIconPanel()->ClearIcons();
  gMainFrame->GetIconPanel()->InvalidateIcons();
  gMainFrame->GetIconPanel()->InvalidateLayout();
}