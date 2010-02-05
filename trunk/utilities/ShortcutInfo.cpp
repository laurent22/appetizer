/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "ShortcutInfo.h"


ShortcutInfo::ShortcutInfo(const wxString& shortcutPath, HWND windowHandle) {
  isOk_ = false;
  iShellLink_ = NULL;
  iPersistFile_ = NULL;

  HRESULT hres; 
  WCHAR szGotPath[MAX_PATH]; 
  WIN32_FIND_DATA wfd;

  // Get a pointer to the IShellLink interface. 
  hres = CoCreateInstance(CLSID_ShellLink, NULL, CLSCTX_INPROC_SERVER, 
                          IID_IShellLink, (LPVOID*)&iShellLink_); 

  if (SUCCEEDED(hres)) {
    // Get a pointer to the IPersistFile interface. 
    hres = iShellLink_->QueryInterface(IID_IPersistFile, (void**)&iPersistFile_); 

    if (SUCCEEDED(hres)) { 
      WCHAR wsz[MAX_PATH]; 

      // Load the shortcut. 
      hres = iPersistFile_->Load(shortcutPath.wchar_str(), STGM_READ); 
      
      if (SUCCEEDED(hres)) { 
        // Resolve the link. 
        hres = iShellLink_->Resolve(windowHandle, 0); 

        if (SUCCEEDED(hres)) {
          // Get the path to the link target. 
          hres = iShellLink_->GetPath(szGotPath, 
                              MAX_PATH, 
                              (WIN32_FIND_DATA*)&wfd, 
                              SLGP_RAWPATH); 

          if (SUCCEEDED(hres)) {
            filePath_ = wxString(szGotPath);
            isOk_ = true;
          } 

        }
      } 
    }
  }

  if (!isOk_) {
    if (iShellLink_) iShellLink_->Release();
    if (iPersistFile_) iPersistFile_->Release();
    iShellLink_ = NULL;
    iPersistFile_ = NULL;
  }
}


bool ShortcutInfo::IsOk() {
  return isOk_;
}


wxString ShortcutInfo::GetPath() {
  return filePath_;
}


wxString ShortcutInfo::GetIconLocation() {
  if (!IsOk()) return wxEmptyString;

  WCHAR szOutput[MAX_PATH]; 
  int* offset = new int();
  iShellLink_->GetIconLocation(szOutput, MAX_PATH, offset);
  delete offset;
  wxString output(szOutput);
  return output;
}


int ShortcutInfo::GetIconIndex() {
  if (!IsOk()) return -1;

  WCHAR szOutput[MAX_PATH]; 
  int* offset = new int();
  iShellLink_->GetIconLocation(szOutput, MAX_PATH, offset);
  int output = *offset;
  delete offset;
  return output;
}


wxString ShortcutInfo::GetDescription() {
  if (!IsOk()) return wxEmptyString;

  WCHAR szOutput[MAX_PATH]; 
  iShellLink_->GetDescription(szOutput, MAX_PATH);
  wxString output(szOutput);
  return output;
}


wxString ShortcutInfo::GetArguments() {
  if (!IsOk()) return wxEmptyString;

  WCHAR szOutput[MAX_PATH]; 
  iShellLink_->GetArguments(szOutput, MAX_PATH);
  wxString output(szOutput);
  return output;
}


ShortcutInfo::~ShortcutInfo() {
  if (iShellLink_) iShellLink_->Release();
  if (iPersistFile_) iPersistFile_->Release();
  iShellLink_ = NULL;
  iPersistFile_ = NULL;
}