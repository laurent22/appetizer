/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <wx/filename.h>
#include "Utilities.h"
#include "../MessageBoxes.h"
#include "../MiniLaunchBar.h"
#include "../FilePaths.h"
#include "../Constants.h"
#include "../FolderItem.h"
#include "../Log.h"

#ifdef __WINDOWS__
#include <Rpc.h>
#pragma comment(lib, "Rpcrt4.lib")
#endif // __WINDOWS__


Utilities::Utilities() {
  configDialog_ = NULL;
  aboutDialog_ = NULL;
  treeViewDialog_ = NULL;
}


void Utilities::Localize() {
  if (configDialog_) configDialog_->Localize();
  if (aboutDialog_) aboutDialog_->Localize();
  if (treeViewDialog_) treeViewDialog_->Localize();
}


wxString Utilities::CreateUUID() {
  // http://nogeekhere.blogspot.com/2008/07/how-to-generate-uuid-guid-in-c.html

  #ifdef __WINDOWS__
  UUID uuid;
  ::ZeroMemory(&uuid, sizeof(UUID));
  ::UuidCreate(&uuid);

  // If you want to convert uuid to string, use UuidToString() function
  RPC_WSTR wszUuid = NULL;
  ::UuidToStringW(&uuid, &wszUuid);
  if(wszUuid != NULL) {
    wxString output((TCHAR*)wszUuid, wxConvUTF8);
    ::RpcStringFree(&wszUuid);
    wszUuid = NULL;
    return output;
  }
  #endif // __WINDOWS__

  return wxEmptyString;
}


Utilities::~Utilities() {
  if (configDialog_) configDialog_->Destroy();
  if (aboutDialog_) aboutDialog_->Destroy();
  if (treeViewDialog_) treeViewDialog_->Destroy();
  configDialog_ = NULL;
  aboutDialog_ = NULL;
  treeViewDialog_ = NULL;
}


bool Utilities::IsApplicationOnRemoteDrive() {
  #ifdef __WINDOWS__
  UINT result = GetDriveType(FilePaths::GetApplicationDrive());
  // Don't show the eject button if we are not on a removable drive.
  // However, to be safe, do show it if the call to GetDriveType
  // failed (result = 0 or 1)
  if (result >= 2 && result != DRIVE_REMOVABLE) return false;
  #endif // __WINDOWS__
  return true;
}


void Utilities::EjectDriveAndExit(bool askForConfirmation) {
  int answer = MessageBoxes::ShowConfirmation(_("Do you wish to eject the drive?"));
  if (answer != wxID_YES) return;

  #ifdef __WINDOWS__

  // In order to eject the drive we need to:
  //
  // 1. set the current directory to something other than the removable drive
  //    Otherwise, RunDll32 is going to lock the drive and so we can't eject it
  // 2. Call RunDll32.exe

  // Get the Windows folder path
  LPTSTR buffer = new TCHAR[MAX_PATH];
  int success = GetWindowsDirectory(buffer, MAX_PATH);
  wxString windowsPath;
  if (!success) {
    windowsPath = _T("c:\\windows");
  } else {
    windowsPath = wxString(buffer, wxConvUTF8);
  }

  // Set the current directory and call the eject dialog
  wxSetWorkingDirectory(windowsPath);
  int result = wxExecute(_T("RunDll32.exe shell32.dll,Control_RunDLL hotplug.dll"));

  #else
  elog("TO BE IMPLEMENTED");
  #endif

  wxGetApp().GetMainFrame()->Close();
}


void Utilities::DoMultiLaunch() {
  wxGetApp().GetUser()->GetRootFolderItem()->DoMultiLaunch();
}


void Utilities::ShowConfigDialog() {
  if (!configDialog_) configDialog_ = new ConfigDialog();
  configDialog_->LoadSettings();
  configDialog_->ShowModal();
}


void Utilities::CreateNewShortcut() {
  wxGetApp().GetUser()->EditNewFolderItem(wxGetApp().GetUser()->GetRootFolderItem());
}


void Utilities::ShowHelpFile() {
  wxString helpFile = FilePaths::GetHelpDirectory() + _T("/") + wxGetApp().GetUser()->GetSettings()->Locale + _T("/") + HELP_FILE_NAME;
  if (!wxFileName::FileExists(helpFile)) {
    // Default to english
    helpFile = FilePaths::GetHelpDirectory() + _T("/en/") + HELP_FILE_NAME;
  }

  wxFileName f(helpFile);
  f.Normalize();
  helpFile = f.GetFullPath();

  FolderItemSP pdfReaderFolderItem = wxGetApp().GetUser()->GetRootFolderItem()->SearchChildByFilename(_T("SumatraPDF"));
  if (pdfReaderFolderItem.get()) {
    pdfReaderFolderItem->LaunchWithArguments(_T("\"") + helpFile + _T("\""));
    return;
  }

  FolderItem::Launch(helpFile);
}


void Utilities::ShowAboutDialog() {
  if (!aboutDialog_) aboutDialog_ = new AboutDialog();
  aboutDialog_->LoadContent();
  aboutDialog_->ShowModal();
}


void Utilities::ShowTreeViewDialog(int selectedFolderItemId) {
  if (!treeViewDialog_) {
    treeViewDialog_ = new TreeViewDialog();
    treeViewDialog_->SetSize(300,500);
  }

  FolderItemSP selectedFolderItem = wxGetApp().GetUser()->GetRootFolderItem()->GetChildById(selectedFolderItemId);
  
  treeViewDialog_->LoadFolderItem(wxGetApp().GetUser()->GetRootFolderItem());
  treeViewDialog_->SelectAndExpandFolderItem(selectedFolderItem);
  treeViewDialog_->ShowModal();
}