/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "Utilities.h"
#include "StringUtil.h"
#include "process_util/ProcessUtil.h"
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


bool Utilities::IsApplicationOnPortableDrive() {
  #ifdef __WINDOWS__
  UINT result = GetDriveType(FilePaths::GetApplicationDrive());
  // Don't show the eject button if we are not on a removable drive.
  // However, to be safe, do show it if the call to GetDriveType
  // failed (result = 0 or 1)
  if (result >= 2 && result != DRIVE_REMOVABLE) return false;
  #endif // __WINDOWS__
  return true;
}


void Utilities::CreateAndRunVBScript(const wxString& filePath, const wxString& scriptContents) {
  if (!wxFileName::FileExists(filePath)) {
    wxFile file;
    bool success = file.Create(filePath);
    if (!success) {
      elog(_T("Couldn't create ") + filePath);
      return;
    }
    file.Open(filePath, wxFile::write);
    file.Write(scriptContents);
    file.Close();        
  }

  FolderItem::Launch(_T("wscript.exe"), filePath);
}


void Utilities::KillLockingProcesses(const wxString& drive, bool painless) {  

  int margin = 8;
  wxDialog* dialog = new wxDialog(wxGetApp().GetMainFrame(), wxID_ANY, _T(""), wxDefaultPosition, wxDefaultSize, wxBORDER_RAISED);
  wxStaticText* dialogText = new wxStaticText(dialog, wxID_ANY, _T(""), wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE);
  dialog->SetClientSize(300, 50);
  dialogText->SetLabel(_("Please wait while your applications are being closed."));
  dialogText->SetSize(margin, margin, dialog->GetSize().GetWidth() - margin * 2, dialog->GetSize().GetHeight() - margin * 2);
  dialogText->Wrap(dialogText->GetSize().GetWidth());
  wxSize dialogTextSize = dialogText->GetBestSize();
  dialogText->SetSize(dialogTextSize.GetWidth(), dialogTextSize.GetHeight());
  dialogText->Move((dialog->GetClientSize().GetWidth() - dialogTextSize.GetWidth()) / 2, (dialog->GetClientSize().GetHeight() - dialogTextSize.GetHeight()) / 2);
  dialog->CenterOnParent();
  dialog->Show();
  dialog->Update();

  #ifdef __WINDOWS__

  // ***********************************************************************
  // Get the list of modules that are locking the drive
  // ***********************************************************************

  CStdString result = ProcessUtil::EnableDebugPriv();

  if (result != _T("ok")) {
    MessageBoxes::ShowError(wxString::Format(_T("%s%s%s"), _("You do not have sufficient privilege to do this operation."), _T("\n\n"), result));
    dialog->Destroy();
    return;
  }

  ProcessUtilModuleVector moduleVector;
  ProcessUtil::ListAllModulesByDrive(drive, moduleVector);  

  // ***********************************************************************
  // For each module, get its associated windows and send to each
  // of them a CLOSE signal. This gives the app a chance to close
  // properly, however it won't work if it has an opened modal dialog or
  // if it ignores the CLOSE signal.
  // ***********************************************************************

  for (int i = 0; i < moduleVector.size(); i++) {
    ProcessUtilModule* m = moduleVector.at(i);

    WindowsEnumerator we;
    we.EnumerateForProcess(m->id);

    for (int j = 0; j < we.windows.size(); j++) {
      ProcessUtilWindow* w = we.windows.at(j);      
      if (w->handle <= 0) continue;

      LRESULT result = SendMessageTimeout(w->handle, WM_CLOSE, NULL, NULL, SMTO_BLOCK, 400, NULL);

      if (!result) {
        LPWSTR pBuffer = NULL;
        FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM, NULL, GetLastError(), MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPWSTR)&pBuffer, 0, NULL);
        wxString s(pBuffer, wxConvUTF8); s.Trim(true).Trim(false);
        wlog(_T("Could not close window: ") + s);
        LocalFree(pBuffer);
      } else {
        ilog("Window closed successfully");  
      }
    }
    
  }

  ProcessUtil::DestroyProcessUtilModuleVector(moduleVector);

  // ***********************************************************************
  // If the 'painless' flag was set, exit now. At this point some windows
  // may still be opened and the user will have to close manually.
  // ***********************************************************************

  if (painless) {
    dialog->Destroy();
    return;
  }

  // ***********************************************************************
  // Otherwise, get a second time the list of blocking processes (which 
  // should now only contains the ones that haven't been closed previously),
  // and kill the process directly.
  // ***********************************************************************

  wxSleep(1); // Wait for a second to allows the windows to close properly

  moduleVector.clear();
  ProcessUtil::ListAllModulesByDrive(drive, moduleVector);  

  for (int i = 0; i < moduleVector.size(); i++) {
    ProcessUtilModule* m = moduleVector.at(i);

    if (m->id <= 0) continue;

    ilog(m->path);

    ilog(_T("Killing ") + m->path);

    wxLogNull logNull; // Disable wxWidgets error messages
    wxKillError killError;
    wxKill(m->id, wxSIGKILL, &killError, wxKILL_CHILDREN);
  } 

  ProcessUtil::DestroyProcessUtilModuleVector(moduleVector);

  #endif // __WINDOWS__

  dialog->Destroy();
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

  if (wxGetApp().GetUser()->GetSettings()->CloseAppsOnEject)
    KillLockingProcesses(FilePaths::GetApplicationDrive(), false);

  // In order to eject the drive we need to:
  //
  // 1. set the current directory to something other than the removable drive
  //    Otherwise, RunDll32 is going to lock the drive and so we can't eject it
  // 2. Call RunDll32.exe

  // Set the current directory and call the eject dialog
  wxSetWorkingDirectory(FilePaths::GetWindowsDirectory());

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
    FolderItem* pdfReaderFolderItem = wxGetApp().GetUser()->GetRootFolderItem()->SearchChildByFilename(_T("SumatraPDF"));
    if (pdfReaderFolderItem) {
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

  FolderItem* selectedFolderItem = wxGetApp().GetUser()->GetRootFolderItem()->GetChildById(selectedFolderItemId);
  
  treeViewDialog_->LoadFolderItem(wxGetApp().GetUser()->GetRootFolderItem());
  treeViewDialog_->SelectAndExpandFolderItem(selectedFolderItem);
  treeViewDialog_->ShowModal();
}