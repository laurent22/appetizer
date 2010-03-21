/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_FilePaths_H
#define Appetizer_FilePaths_H
namespace appetizer {

class FilePaths {

public:

  static QString GetApplicationDrive();

  static QString GetApplicationDirectory();  
  static QString GetDataDirectory();
  static QString GetSettingsDirectory();
  static QString GetBaseSkinDirectory();
  static QString GetDefaultSkinDirectory();
  static QString GetSkinDirectory();
  static QString GetLocalesDirectory();
  static QString GetHelpDirectory();
  static QString GetToolsDirectory();
  static QString GetPluginsDirectory();
  static QString GetPluginPreferenceDirectory();
  static QString GetBaseSkinAssetsDirectory();
  static QString GetIconCacheDirectory();
  static QString GetTempDirectory();

  static QString GetApplicationPath();
  static QString GetSettingsFile();
  static QString GetPluginSettingsFile();
  static QString GetFolderItemsFile();
  static QString GetWindowFile();

  static QString GetWindowsDirectory();
  static QString GetSystem32Directory();
  static QString GetUserShellDirectory(const QString& itemName);
  static QString GetAllUsersShellDirectory(const QString& itemName);
  static QString GetQuickLaunchDirectory();  
  static QString GetWindowsFontDirectory(); 

  static QString resolveVariables(const QString& path);
  static void CreateSettingsDirectory();
  static void CreateDirectoryIfNotExists(const QString& path);
  static void InitializePaths();
  static QString GetFontFilePath(const QString& fontAlias);
  static QString GetSkinFile(const QString& filename);

private:

  static QString ApplicationDrive_;
  static QString ApplicationDirectory_;
  static QString DataDirectory_;
  static QString SettingsDirectory_;
  static QString BaseSkinDirectory_;
  static QString DefaultSkinDirectory_;
  static QString BaseSkinAssetsDirectory_;
  static QString LocalesDirectory_;
  static QString HelpDirectory_;
  static QString SettingsFile_;
  static QString FolderItemsFile_;
  static QString WindowFile_;
  static QString System32Directory_;
  static QString WindowsDirectory_;
  static QString ApplicationPath_;
  static QString IconCacheDirectory_;
  static QString ToolsDirectory_;
  static QString PluginsDirectory_;
  static QString PluginSettingsFile_;
  static QString PluginPreferenceDirectory_;

};

}
#endif // Appetizer_FilePaths_H
