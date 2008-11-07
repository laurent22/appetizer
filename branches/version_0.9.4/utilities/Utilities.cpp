/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <wx/filename.h>
#include "Utilities.h"
#include "../Controller.h"
#include "../MainFrame.h"
#include "../FilePaths.h"
#include "../Constants.h"
#include "../FolderItem.h"
#include "../Log.h"


extern Controller gController;
extern MainFrame* gMainFrame;


Utilities::Utilities() {
  configDialog_ = NULL;
  aboutDialog_ = NULL;
}


Utilities::~Utilities() {
  if (configDialog_) configDialog_->Destroy();
  if (aboutDialog_) aboutDialog_->Destroy();
  configDialog_ = NULL;
  aboutDialog_ = NULL;
}


void Utilities::EjectDriveAndExit() {
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

  gMainFrame->Close();
}


void Utilities::DoMultiLaunch() {
  gController.GetUser()->GetRootFolderItem()->DoMultiLaunch();
}


void Utilities::ShowConfigDialog() {
  if (!configDialog_) configDialog_ = new ConfigDialog();
  configDialog_->LoadSettings();
  configDialog_->ShowModal();
}


void Utilities::CreateNewShortcut() {
  gController.GetUser()->EditNewFolderItem(gController.GetUser()->GetRootFolderItem());
}


void Utilities::ShowHelpFile() {
  wxString helpFile = FilePaths::GetHelpDirectory() + _T("/") + gController.GetUser()->GetSettings()->Locale + _T("/") + HELP_FILE_NAME;
  if (!wxFileName::FileExists(helpFile)) {
    // Default to english
    helpFile = FilePaths::GetHelpDirectory() + _T("/en/") + HELP_FILE_NAME;
  }

  FolderItem::Launch(helpFile);
}


void Utilities::ShowAboutDialog() {
  if (!aboutDialog_) aboutDialog_ = new AboutDialog();
  aboutDialog_->LoadContent();
  aboutDialog_->ShowModal();
}