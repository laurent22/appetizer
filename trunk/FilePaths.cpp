/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <wx/filename.h>
#include <wx/stdpaths.h>
#include <wx/cmdline.h>
#include "FilePaths.h"
#include "Constants.h"
#include "Controller.h"


extern Controller gController;
extern wxCmdLineParser gCommandLine;


wxString FilePaths::ApplicationDrive_ = _T("");
wxString FilePaths::ApplicationDirectory_ = _T("");
wxString FilePaths::DataDirectory_ = _T("");
wxString FilePaths::SettingsDirectory_ = _T("");
wxString FilePaths::BaseSkinDirectory_ = _T("");
wxString FilePaths::SkinDirectory_ = _T("");
wxString FilePaths::LocalesDirectory_ = _T("");
wxString FilePaths::IconsDirectory_ = _T("");
wxString FilePaths::SettingsFile_ = _T("");
wxString FilePaths::FolderItemsFile_ = _T("");
wxString FilePaths::WindowFile_ = _T("");
wxString FilePaths::HelpDirectory_ = _T("");


wxString FilePaths::GetApplicationDrive() { return FilePaths::ApplicationDrive_; }
wxString FilePaths::GetApplicationDirectory() { return FilePaths::ApplicationDirectory_; }
wxString FilePaths::GetDataDirectory() { return FilePaths::DataDirectory_; }
wxString FilePaths::GetSettingsDirectory() { return FilePaths::SettingsDirectory_; }
wxString FilePaths::GetBaseSkinDirectory() { return FilePaths::BaseSkinDirectory_; }
wxString FilePaths::GetSkinDirectory() { return FilePaths::BaseSkinDirectory_ + _T("/") + gController.GetUser()->GetSettings()->Skin;; }
wxString FilePaths::GetLocalesDirectory() { return FilePaths::LocalesDirectory_; }
wxString FilePaths::GetHelpDirectory() { return FilePaths::HelpDirectory_; }
wxString FilePaths::GetIconsDirectory() { return FilePaths::GetSkinDirectory() + _T("/") + ICONS_FOLDER_NAME; }
wxString FilePaths::GetSettingsFile() { return FilePaths::SettingsFile_; }
wxString FilePaths::GetFolderItemsFile() { return FilePaths::FolderItemsFile_; }
wxString FilePaths::GetWindowFile() { return FilePaths::WindowFile_; }



void FilePaths::CreateSettingsDirectory() {
  if (wxFileName::DirExists(FilePaths::GetSettingsDirectory())) return;

  wxFileName::Mkdir(FilePaths::GetSettingsDirectory(), 0777, wxPATH_MKDIR_FULL);
}


void FilePaths::InitializePaths() {
  wxFileName executablePath = wxFileName(wxStandardPaths().GetExecutablePath());
  wxString applicationDirectory = executablePath.GetPath();
  wxString applicationDrive;
  wxFileName::SplitPath(executablePath.GetPath(), &applicationDrive, NULL, NULL, NULL, false, wxPATH_NATIVE);

  FilePaths::ApplicationDrive_ = applicationDrive;
  #ifdef __WINDOWS__
  FilePaths::ApplicationDrive_ += _T(":");
  #endif // __WINDOWS__

  FilePaths::ApplicationDirectory_ = applicationDirectory;
  FilePaths::DataDirectory_ = applicationDirectory + _T("/") + DATA_FOLDER_NAME; 

  if (gCommandLine.Found(_T("p"))) {
    FilePaths::SettingsDirectory_ = FilePaths::GetDataDirectory() + _T("/") + SETTING_FOLDER_NAME;
  } else {
    wxString userDataPath;
    bool found = gCommandLine.Found(_T("d"), &userDataPath);
    if (!found) {
      FilePaths::SettingsDirectory_ = wxString::Format(_T("%s/%s/%s"), wxStandardPaths().GetUserConfigDir(), APPLICATION_NAME, SETTING_FOLDER_NAME);
    } else {
      FilePaths::SettingsDirectory_ = userDataPath;
    }
  }

  FilePaths::LocalesDirectory_ = FilePaths::GetDataDirectory() + _T("/") + LOCALES_FOLDER_NAME;
  FilePaths::SettingsFile_ = FilePaths::GetSettingsDirectory() + _T("/") + SETTING_FILE_NAME;
  FilePaths::FolderItemsFile_ = FilePaths::GetSettingsDirectory() + _T("/") + FOLDER_ITEMS_FILE_NAME;
  FilePaths::WindowFile_ = FilePaths::GetSettingsDirectory() + _T("/") + WINDOW_FILE_NAME;  
  FilePaths::HelpDirectory_ = FilePaths::GetDataDirectory() + _T("/") + HELP_FOLDER_NAME;  
  FilePaths::BaseSkinDirectory_ = FilePaths::GetDataDirectory() + _T("/") + SKIN_FOLDER_NAME; 
}