/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "FilePaths.h"
#include <wx/filename.h>

wxString FilePaths::ApplicationDrive = _T("");
wxString FilePaths::ApplicationDirectory = _T("");
wxString FilePaths::DataDirectory = _T("");
wxString FilePaths::SettingsDirectory = _T("");
wxString FilePaths::BaseSkinDirectory = _T("");
wxString FilePaths::SkinDirectory = _T("");
wxString FilePaths::LocalesDirectory = _T("");
wxString FilePaths::UserSettingsFile = _T("");
wxString FilePaths::IconsDirectory = _T("");
wxString FilePaths::SettingsFile = _T("");
wxString FilePaths::FolderItemsFile = _T("");
wxString FilePaths::WindowFile = _T("");
wxString FilePaths::HelpDirectory = _T("");

void FilePaths::CreateSettingsDirectory() {
  if (wxFileName::DirExists(FilePaths::SettingsDirectory)) return;
  wxFileName::Mkdir(FilePaths::SettingsDirectory, wxPATH_MKDIR_FULL);
}