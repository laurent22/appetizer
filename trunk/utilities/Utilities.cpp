/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "../stdafx.h"

#include "Utilities.h"
#include "StringUtil.h"
#include "process_util/ProcessUtil.h"
#include "../gui/ImportWizardDialog.h"
#include "../MessageBoxes.h"
#include "../MiniLaunchBar.h"
#include "../FilePaths.h"
#include "../Constants.h"
#include "../FolderItem.h"
#include "../Styles.h"


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
  if (!deleted) WLOG(_T("Couldn't delete autorun.inf"));

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


void Utilities::SwitchSkin(const wxString& skinName) {
  UserSettings* userSettings = wxGetApp().GetUser()->GetSettings();

  if (skinName != userSettings->GetString(_T("Skin"))) {

    wxString skinDirectory = FilePaths::GetBaseSkinDirectory() + _T("/") + skinName;
    if (!wxFileName::DirExists(skinDirectory)) {
      ELOG(_T("Cannot find skin directory: ") + skinDirectory);
      return;
    }

    wxString skinXmlPath = skinDirectory + _T("/") + SKIN_FILE_NAME;
    SkinMetadata metadata;

    Styles::GetSkinMetadata(skinXmlPath, metadata);

    if (!Styles::IsSkinVersionCompatible(metadata.CompatibleVersion)) {
      MessageBoxes::ShowError(wxString::Format(_("This skin is not compatible with the current version of %s."), APPLICATION_NAME));
    } else {
      userSettings->SetString(_T("Skin"), skinName);
      wxGetApp().GetMainFrame()->ApplySkin();
    }

  }

}


wxArrayString Utilities::GetSkinNames() {
  wxString skinFolderPath = FilePaths::GetBaseSkinDirectory();

  wxArrayString output;
  wxDir skinFolder;

  if (wxFileName::DirExists(skinFolderPath) && skinFolder.Open(skinFolderPath)) {
    wxString folderName;
    bool success = skinFolder.GetFirst(&folderName, wxALL_FILES_PATTERN, wxDIR_DIRS);
    int i = 0;

    while (success) {
      if (folderName != _T("Base")) output.Add(folderName);
      success = skinFolder.GetNext(&folderName);      
    }
  } 

  return output;

}


void Utilities::Localize() {
  if (configDialog_) configDialog_->Localize();
  if (aboutDialog_) aboutDialog_->Localize();
  if (treeViewDialog_) treeViewDialog_->Localize();
}


bool Utilities::RemoveFolderItemWithConfirmation(appFolderItem* folderItem) {
  if (!folderItem) return false;

  if (wxGetApp().GetUser()->GetSettings()->GetBool(_T("ShowDeleteIconMessage"))) {
    int result = MessageBoxes::ShowConfirmation(_("Do you wish to remove this icon?"), wxYES | wxNO, _("Don't show this message again"), false);
    if (!result) return false;

    wxGetApp().GetUser()->GetSettings()->SetBool(_T("ShowDeleteIconMessage"), !MessageBoxes::GetCheckBoxState());
    wxGetApp().GetUser()->ScheduleSave();
    if (result != wxID_YES) return false;
  }

  folderItem->Dispose();

  return true;
}


void Utilities::CreateShortcut(const wxString& filePath, const wxString& shortcutPath, const wxString& iconPath, int iconIndex) {
  if (wxFileName::FileExists(shortcutPath)) {
    int answer = MessageBoxes::ShowConfirmation(_("A shortcut with this name already exists. Do you wish to overwrite it?"));
    if (answer == wxID_YES) {
      wxRemoveFile(shortcutPath);
    } else {
      return;
    }
  }

  wxString script;
  script += _T("Set objShell = WScript.CreateObject(\"WScript.Shell\")\n");
  script += _T("Set link = objShell.CreateShortcut(\"") + shortcutPath + _T("\")\n");
  script += _T("link.TargetPath = \"") + filePath + _T("\"\n");

  if (iconPath != wxEmptyString) {
    wxString fullIconPath = iconPath;
    script += _T("link.IconLocation = \"") + iconPath + _T(",") + wxString::Format(_T("%d"), iconIndex) + _T("\"\n");
  }

  script += _T("link.Save");

  wxString scriptFilePath = FilePaths::GetTempDirectory() + _T("\\") + CreateUUID() + _T(".vbs");
  FilePaths::CreateDirectoryIfNotExists(FilePaths::GetTempDirectory());

  CreateAndRunVBScript(scriptFilePath, script);
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


/********************************************************
*
* FUNCTION: GetDisksProperty(HANDLE hDevice, 
* PSTORAGE_DEVICE_DESCRIPTOR pDevDesc)
*
* PURPOSE: get the info of specified device
*
* By f22_storm
* http://www.codeproject.com/KB/winsdk/usbdisks.aspx
*
******************************************************/
BOOL GetDisksProperty(HANDLE hDevice, 
  PSTORAGE_DEVICE_DESCRIPTOR& pDevDesc)
{
 STORAGE_PROPERTY_QUERY Query; // input param for query

 DWORD dwOutBytes; // IOCTL output length

 BOOL bResult; // IOCTL return val

 // specify the query type

 Query.PropertyId = StorageDeviceProperty;
 Query.QueryType = PropertyStandardQuery;

 // Query using IOCTL_STORAGE_QUERY_PROPERTY 

 bResult = ::DeviceIoControl(hDevice, // device handle

 IOCTL_STORAGE_QUERY_PROPERTY, // info of device property

  &Query, sizeof(STORAGE_PROPERTY_QUERY), // input data buffer

  pDevDesc, pDevDesc->Size, // output data buffer

  &dwOutBytes, // out's length

  (LPOVERLAPPED)NULL); 

 return bResult;
}


bool Utilities::IsApplicationOnPortableDrive() {
  #ifdef __WXDEBUG__
  return true;
  #endif

  #ifdef __WINDOWS__

  // Note that if anything fails in this function, it will
  // return true, so the app will assume that it *is* on a portable drive.
  wxString driveName = FilePaths::GetApplicationDrive();

  UINT result = GetDriveType(driveName);

  if (result <= 1) return true; // GetDriveType failed
  if (result == DRIVE_REMOVABLE) return true; // We are on a removable drive
  if (result != DRIVE_FIXED) return false; // We are definitely not a removable drive

  // Here we handle the case where the drive type is DRIVE_FIXED. In that case
  // the drive might be a USB hard drive, so we need to find out if it can
  // be ejected or not.

  wxString name = wxString::Format(_T("\\\\?\\%c:"), driveName[0]);

  HANDLE hDevice = CreateFile(name, 
    GENERIC_READ, 
    FILE_SHARE_READ | FILE_SHARE_WRITE, 
    NULL, OPEN_EXISTING, NULL, NULL);

  bool output = false;

  if (hDevice == INVALID_HANDLE_VALUE) {
    output = true;
  } else {
	  PSTORAGE_DEVICE_DESCRIPTOR pDevDesc = (PSTORAGE_DEVICE_DESCRIPTOR)new BYTE[sizeof(STORAGE_DEVICE_DESCRIPTOR) + 512 - 1];
	  pDevDesc->Size = sizeof(STORAGE_DEVICE_DESCRIPTOR) + 512 - 1;

    if (!GetDisksProperty(hDevice, pDevDesc)) {
      output = true;
    } else {
      output = pDevDesc->BusType == BusTypeUsb;
    }

    delete pDevDesc;
  }

  if (hDevice && hDevice != INVALID_HANDLE_VALUE) CloseHandle(hDevice);

  return output;

  #endif // __WINDOWS__
  
  return true;
}


void Utilities::CreateAndRunVBScript(const wxString& filePath, const wxString& scriptContents) {
  if (!wxFileName::FileExists(filePath)) {
    wxFile file;
    bool success = file.Create(filePath);
    if (!success) {
      ELOG(_T("Couldn't create ") + filePath);
      return;
    }
    file.Open(filePath, wxFile::write);
    file.Write(scriptContents);
    file.Close();        
  }

  appFolderItem::Launch(_T("wscript.exe"), filePath);
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
    MessageBoxes::ShowError(wxString::Format(_T("%s%s%s"), _("You do not have sufficient privileges for this operation."), _T("\n\n"), result));
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

  wxString applicationName = wxFileName(FilePaths::GetApplicationPath()).GetName();

  for (int i = 0; i < moduleVector.size(); i++) {
    ProcessUtilModule* m = moduleVector.at(i);

    wxFileName f(m->path);
    if (f.GetName().Contains(applicationName)) continue;

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
        WLOG(_T("Could not close window: ") + s);
        LocalFree(pBuffer);
      } else {
        ILOG(_T("Window closed successfully"));  
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

    wxFileName f(m->path);
    if (f.GetName().Contains(applicationName)) continue;

    if (m->id <= 0) continue;

    ILOG(m->path);

    ILOG(_T("Killing ") + m->path);

    wxLogNull logNull; // Disable wxWidgets error messages
    wxKillError killError;
    wxKill(m->id, wxSIGKILL, &killError, wxKILL_CHILDREN);
  } 

  ProcessUtil::DestroyProcessUtilModuleVector(moduleVector);

  #endif // __WINDOWS__

  dialog->Destroy();
}


void Utilities::ShowEjectDriveDialog() {
  #ifdef __WINDOWS__

  // Set the current directory and call the eject dialog
  wxString previousCWD = wxGetCwd();  
  wxSetWorkingDirectory(FilePaths::GetWindowsDirectory());

  // Note: this call creates a memory leak when the main frame is closed just after it.
  int result = wxExecute(_T("RunDll32.exe shell32.dll,Control_RunDLL hotplug.dll"));

  wxSetWorkingDirectory(previousCWD);

  #endif // __WINDOWS__
}


void Utilities::EjectDriveAndExit(bool askForConfirmation) {
  if (wxGetApp().GetUser()->GetSettings()->GetBool(_T("ShowEjectDriveMessage"))) {
    int answer = MessageBoxes::ShowConfirmation(_("Do you wish to eject the drive?"), wxYES | wxNO, _("Don't show this message again"), false);
    if (!answer) return;
    wxGetApp().GetUser()->GetSettings()->SetBool(_T("ShowEjectDriveMessage"), !MessageBoxes::GetCheckBoxState());
    wxGetApp().GetUser()->ScheduleSave();
    if (answer != wxID_YES) return;
  }

  #ifdef __WINDOWS__

  if (wxGetApp().GetUser()->GetSettings()->GetBool(_T("CloseAppsOnEject")))
    KillLockingProcesses(FilePaths::GetApplicationDrive(), false);

  #endif

  ShowEjectDriveDialog();

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


void Utilities::ShowImportDialog() {
  ImportWizardDialog* d = new ImportWizardDialog(wxGetApp().GetMainFrame());
  d->ShowModal();
  d->Destroy();
}


void Utilities::CreateNewShortcut() {
  wxGetApp().GetUser()->EditNewFolderItem(wxGetApp().GetUser()->GetRootFolderItem());
}


void Utilities::ShowHelpFile(const wxString& anchor) {
  wxString helpFile = FilePaths::GetHelpDirectory() + _T("/") + wxGetApp().GetUser()->GetSettings()->GetString(_T("Locale")) + _T("/") + HELP_FILE_NAME;
  if (!wxFileName::FileExists(helpFile)) {
    // Default to english
    helpFile = FilePaths::GetHelpDirectory() + _T("/en/") + HELP_FILE_NAME;
  }

  wxFileName f(helpFile);
  f.Normalize();
  helpFile = f.GetFullPath();

  // Built-in support for SumatraPDF and Foxit Reader. If any of these applications is on the
  // dock, use it to open the PDF file
  appFolderItem* pdfReaderFolderItem = wxGetApp().GetUser()->GetRootFolderItem()->SearchChildByFilename(_T("SumatraPDF"));
  if (!pdfReaderFolderItem) pdfReaderFolderItem = wxGetApp().GetUser()->GetRootFolderItem()->SearchChildByFilename(_T("Foxit Reader"));
  
  if (pdfReaderFolderItem) {
    pdfReaderFolderItem->LaunchWithArguments(_T("\"") + helpFile + _T("\""));
    return;
  } else {
    appFolderItem::Launch(helpFile);
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

  appFolderItem* selectedFolderItem = wxGetApp().GetUser()->GetRootFolderItem()->GetChildById(selectedFolderItemId);
  
  treeViewDialog_->LoadFolderItem(wxGetApp().GetUser()->GetRootFolderItem());
  treeViewDialog_->SelectAndExpandFolderItem(selectedFolderItem);
  treeViewDialog_->ShowModal();
}


void Utilities::ConvertStaticTextToLink(wxStaticText* label) {
  wxFont linkLabelFont(label->GetFont());
  linkLabelFont.SetUnderlined(true);
  label->SetFont(linkLabelFont);
  label->SetForegroundColour(wxColour(0,0,255));
  label->SetCursor(wxCursor(wxCURSOR_HAND));
}