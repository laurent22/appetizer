/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "Utilities.h"
#include "StringUtil.h"
#include "../MessageBoxes.h"
#include "../MiniLaunchBar.h"
#include "../FilePaths.h"
#include "../Constants.h"
#include "../FolderItem.h"
#include "../Log.h"


Utilities::Utilities() {
  configDialog_ = NULL;
  aboutDialog_ = NULL;
  treeViewDialog_ = NULL;
}


bool Utilities::InstallAutorunFile() {
  wxString skinDir = StringUtil::RemoveDriveFromPath(FilePaths::GetBaseSkinDirectory());
  wxString appPath = StringUtil::RemoveDriveFromPath(FilePaths::GetApplicationPath());

  wxString fileContent;
  fileContent += _T("[autorun]\n");
  fileContent += wxString::Format(_T("Icon=%s\\Application.ico\n"), skinDir);
  fileContent += wxString::Format(_T("Action=Start %s\n"), APPLICATION_NAME);
  fileContent += wxString::Format(_T("Open=%s\n"), appPath);
  fileContent += _T("UseAutoPlay=1\n");

  wxString filePath = FilePaths::GetApplicationDrive() + _T("\\autorun.inf");

  #ifdef __WINDOWS__
  // Remove "read-only" attribute if set
  SetFileAttributes(filePath.c_str(), FILE_ATTRIBUTE_NORMAL);
  #endif
  
  bool deleted = wxRemoveFile(filePath);
  if (!deleted) wlog("Couldn't delete autorun.inf");

  wxFile f;
  wxLogNull logNull;
  f.Create(filePath, true);
  bool opened = f.Open(filePath, wxFile::write);
  if (!opened) {    
    return false;
  }
  
  f.Write(fileContent, wxConvUTF8);
  f.Close();

  return true;
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
  if (wxGetApp().GetUser()->GetSettings()->ShowEjectDriveMessage) {
    int answer = MessageBoxes::ShowConfirmation(_("Do you wish to eject the drive?"), wxYES | wxNO, _("Don't show this message again"), false);
    if (!answer) return;
    wxGetApp().GetUser()->GetSettings()->ShowEjectDriveMessage = !MessageBoxes::GetCheckBoxState();
    wxGetApp().GetUser()->ScheduleSave();
    if (answer != wxID_YES) return;
  }

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

  // Note: this call creates a memory leak when the main frame is closed just after it.
  int result = wxExecute(_T("RunDll32.exe shell32.dll,Control_RunDLL hotplug.dll"));

  #else
  elog("TO BE IMPLEMENTED");
  #endif

  wxGetApp().GetMainFrame()->Close();
}


bool Utilities::DoMultiLaunch() {
  return wxGetApp().GetUser()->GetRootFolderItem()->DoMultiLaunch();
}


void Utilities::ShowConfigDialog() {
  if (!configDialog_) configDialog_ = new ConfigDialog();
  configDialog_->LoadSettings();
  configDialog_->ShowModal();
}


void Utilities::CreateNewShortcut() {
  wxGetApp().GetUser()->EditNewFolderItem(wxGetApp().GetUser()->GetRootFolderItem());
}


void Utilities::ShowHelpFile(const wxString& anchor) {
  wxString helpFile = FilePaths::GetHelpDirectory() + _T("/") + wxGetApp().GetUser()->GetSettings()->Locale + _T("/") + HELP_FILE_NAME;
  if (!wxFileName::FileExists(helpFile)) {
    // Default to english
    helpFile = FilePaths::GetHelpDirectory() + _T("/en/") + HELP_FILE_NAME;
  }

  wxFileName f(helpFile);
  f.Normalize();
  helpFile = f.GetFullPath();

  #ifdef __MLB_USE_PDF_HELP__
    // Keep support for PDF in case some languages don't work with CHM files
    FolderItemSP pdfReaderFolderItem = wxGetApp().GetUser()->GetRootFolderItem()->SearchChildByFilename(_T("SumatraPDF"));
    if (pdfReaderFolderItem.get()) {
      pdfReaderFolderItem->LaunchWithArguments(_T("\"") + helpFile + _T("\""));
      return;
    }    
  }
  #endif

  if (anchor != wxEmptyString) {
    // hh.exe mk:@MSITStore:c:\full\path\to\Appetizer.chm::1.htm#NameOfAnchor
    wxString parameters = wxString::Format(_T("mk:@MSITStore:%s::1.htm#%s"), helpFile, anchor);
    FolderItem::Launch(FilePaths::GetHHPath(), parameters);
  } else {
    FolderItem::Launch(helpFile);
  }  
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