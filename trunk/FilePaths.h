/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __FilePaths_H
#define __FilePaths_H

class FilePaths {

public:

  static wxString GetApplicationDrive();

  static wxString GetApplicationDirectory();  
  static wxString GetDataDirectory();
  static wxString GetSettingsDirectory();
  static wxString GetBaseSkinDirectory();
  static wxString GetDefaultSkinDirectory();
  static wxString GetSkinDirectory();
  static wxString GetLocalesDirectory();
  static wxString GetHelpDirectory();
  static wxString GetToolsDirectory();
  static wxString GetPluginsDirectory();
  static wxString GetIconCacheDirectory();

  static wxString GetApplicationPath();
  static wxString GetSettingsFile();
  static wxString GetFolderItemsFile();
  static wxString GetWindowFile();

  static wxString GetWindowsDirectory();
  static wxString GetSystem32Directory();
  static wxString GetHHPath();
  static wxString GetUserShellDirectory(const wxString& itemName);
  static wxString GetAllUsersShellDirectory(const wxString& itemName);
  static wxString GetQuickLaunchDirectory();  

  static void CreateSettingsDirectory();
  static void InitializePaths();
  static wxString GetSkinFile(const wxString& filename);

private:

  static wxString ApplicationDrive_;
  static wxString ApplicationDirectory_;
  static wxString DataDirectory_;
  static wxString SettingsDirectory_;
  static wxString BaseSkinDirectory_;
  static wxString DefaultSkinDirectory_;
  static wxString LocalesDirectory_;
  static wxString HelpDirectory_;
  static wxString SettingsFile_;
  static wxString FolderItemsFile_;
  static wxString WindowFile_;
  static wxString System32Directory_;
  static wxString WindowsDirectory_;
  static wxString ApplicationPath_;
  static wxString IconCacheDirectory_;
  static wxString ToolsDirectory_;
  static wxString PluginsDirectory_;

};

#endif // __FilePaths_H