/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "FilePaths.h"
#include "Constants.h"
#include "MiniLaunchBar.h"
#include "utilities/StringUtil.h"


wxString FilePaths::ApplicationDrive_ = _T("");
wxString FilePaths::ApplicationDirectory_ = _T("");
wxString FilePaths::DataDirectory_ = _T("");
wxString FilePaths::SettingsDirectory_ = _T("");
wxString FilePaths::BaseSkinDirectory_ = _T("");
wxString FilePaths::DefaultSkinDirectory_ = _T("");
wxString FilePaths::LocalesDirectory_ = _T("");
wxString FilePaths::SettingsFile_ = _T("");
wxString FilePaths::FolderItemsFile_ = _T("");
wxString FilePaths::WindowFile_ = _T("");
wxString FilePaths::HelpDirectory_ = _T("");
wxString FilePaths::WindowsDirectory_ = _T("");
wxString FilePaths::System32Directory_ = _T("");
wxString FilePaths::ApplicationPath_ = _T("");
wxString FilePaths::IconCacheDirectory_ = _T("");
wxString FilePaths::ToolsDirectory_ = _T("");
wxString FilePaths::PluginsDirectory_ = _T("");
wxString FilePaths::PluginSettingsFile_ = _T("");
wxString FilePaths::BaseSkinAssetsDirectory_ = _T("");
wxString FilePaths::PluginPreferenceDirectory_ = _T("");


wxString FilePaths::GetApplicationDrive() { return FilePaths::ApplicationDrive_; }
wxString FilePaths::GetApplicationDirectory() { return FilePaths::ApplicationDirectory_; }
wxString FilePaths::GetDataDirectory() { return FilePaths::DataDirectory_; }
wxString FilePaths::GetSettingsDirectory() { return FilePaths::SettingsDirectory_; }
wxString FilePaths::GetBaseSkinDirectory() { return FilePaths::BaseSkinDirectory_; }
wxString FilePaths::GetSkinDirectory() { return FilePaths::BaseSkinDirectory_ + _T("/") + wxGetApp().GetUser()->GetSettings()->GetString(_T("Skin")); }
wxString FilePaths::GetDefaultSkinDirectory() { return FilePaths::DefaultSkinDirectory_; }
wxString FilePaths::GetLocalesDirectory() { return FilePaths::LocalesDirectory_; }
wxString FilePaths::GetHelpDirectory() { return FilePaths::HelpDirectory_; }
wxString FilePaths::GetToolsDirectory() { return FilePaths::ToolsDirectory_; }
wxString FilePaths::GetSettingsFile() { return FilePaths::SettingsFile_; }
wxString FilePaths::GetFolderItemsFile() { return FilePaths::FolderItemsFile_; }
wxString FilePaths::GetWindowFile() { return FilePaths::WindowFile_; }
wxString FilePaths::GetIconCacheDirectory() { return FilePaths::IconCacheDirectory_; }
wxString FilePaths::GetPluginsDirectory() { return FilePaths::PluginsDirectory_; }
wxString FilePaths::GetPluginSettingsFile() { return FilePaths::PluginSettingsFile_; }
wxString FilePaths::GetBaseSkinAssetsDirectory() { return FilePaths::BaseSkinAssetsDirectory_; }
wxString FilePaths::GetPluginPreferenceDirectory() { return FilePaths::PluginPreferenceDirectory_; }
wxString FilePaths::GetHHPath() { return GetWindowsDirectory() + _T("\\hh.exe"); }
wxString FilePaths::GetWindowsFontDirectory() { return GetWindowsDirectory() + _T("\\fonts"); }
wxString FilePaths::GetTempDirectory() { return GetSettingsDirectory() + _T("\\Temp"); }


wxString FilePaths::GetQuickLaunchDirectory() { 
  wxFileName f(_T("%APPDATA%\\Microsoft\\Internet Explorer\\Quick Launch"));
  f.Normalize();
  return f.GetFullPath();
}


wxString FilePaths::GetApplicationPath() {
  if (ApplicationPath_ != wxEmptyString) return ApplicationPath_;

  #ifdef __WINDOWS__
  TCHAR cName[MAX_PATH + 10];
	GetModuleFileName(0, cName, MAX_PATH + 10);
  ApplicationPath_ = wxString(cName, wxConvUTF8);
  #endif

  return ApplicationPath_;
}


wxString FilePaths::GetFontFilePath(const wxString& fontAlias) {
  wxString filename;

  wxLogNull* logNo = new wxLogNull();

  wxRegKey* regKey = new wxRegKey(_T("HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Fonts"));
  if (!regKey->Exists()) {
    ELOG(_T("Cannot get font file name from HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Fonts"));
    filename = wxEmptyString;
  } else {
    regKey->QueryValue(fontAlias + _T(" (TrueType)"), filename);
  }
  
  wxDELETE(regKey);
  wxDELETE(logNo);

  return GetWindowsFontDirectory() + _T("\\") + filename;
}


wxString FilePaths::GetSystem32Directory() {
  if (FilePaths::System32Directory_ != wxEmptyString) return FilePaths::System32Directory_;

  wxString output;

  // LPTSTR is wchar_t if UNICODE is enabled, or a char otherwise
  LPTSTR buffer = new TCHAR[MAX_PATH];
  int success = GetSystemDirectory(buffer, MAX_PATH);   

  if (!success) {
    wxLogDebug(_T("WARNING: coulnd't get System32 directory"));
    output = _T("c:\\window\\system32");
  } else {
    // Convert the LPTSTR to a char*
    char cString[MAX_PATH];
    wcstombs(cString, buffer, MAX_PATH);
    output = wxString::FromAscii(cString);
  }

  wxDELETE(buffer);

  FilePaths::System32Directory_ = output;

  return output;
}


wxString FilePaths::GetWindowsDirectory() {
  if (FilePaths::WindowsDirectory_ != wxEmptyString) return FilePaths::WindowsDirectory_;

  LPTSTR buffer = new TCHAR[MAX_PATH];
  int success = ::GetWindowsDirectory(buffer, MAX_PATH);
  wxString windowsPath;
  if (!success) {
    windowsPath = _T("c:\\windows");
  } else {
    windowsPath = wxString(buffer, wxConvUTF8);
  }

  wxDELETE(buffer);

  FilePaths::WindowsDirectory_ = windowsPath;

  return FilePaths::WindowsDirectory_;
}


wxString FilePaths::GetUserShellDirectory(const wxString& itemName) {
  wxLogNull logNo;

  wxRegKey regKey(_T("HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\User Shell Folders"));
  if (regKey.Exists()) {
    wxString filePath;
    regKey.QueryValue(itemName, filePath);
    return filePath;
  } else {
    ELOG(_T("Couldn't get user path: ") + itemName);
  }

  return _T("");
}


wxString FilePaths::GetAllUsersShellDirectory(const wxString& itemName) {
  wxLogNull logNo;

  wxRegKey regKey(_T("HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Explorer\\User Shell Folders"));
  if (regKey.Exists()) {
    wxString filePath;
    regKey.QueryValue(itemName, filePath);
    return filePath;
  } else {
    ELOG(_T("Couldn't get user path: ") + itemName);
  }

  return _T("");

}


/**
 * This method look for the given filename in the right directory. It first checks
 * the current skin directory then, if it can't find the file, it checks the default
 * skin directory.
 * @param filename The file to look for
 * @return The path to the file
 */
wxString FilePaths::GetSkinFile(const wxString& filename) {
  wxString output = FilePaths::GetSkinDirectory() + _T("/") + filename;
  if (wxFileName::FileExists(output)) return output;
  return GetBaseSkinAssetsDirectory() + _T("/") + filename;
}


void FilePaths::CreateSettingsDirectory() {
  if (wxFileName::DirExists(FilePaths::GetSettingsDirectory())) return;

  wxFileName::Mkdir(FilePaths::GetSettingsDirectory(), 0777, wxPATH_MKDIR_FULL);
}


void FilePaths::CreateDirectoryIfNotExists(const wxString& path) {
  if (wxFileName::DirExists(path)) return;
  
  wxFileName::Mkdir(path, 0777, wxPATH_MKDIR_FULL);
}


void FilePaths::InitializePaths() {
  wxFileName executablePath = wxFileName(wxStandardPaths().GetExecutablePath());
  wxString applicationDirectory = FolderItem::ResolvePath(executablePath.GetPath(), true);
  //applicationDirectory = StringUtil::RemoveTrailingSlash(applicationDirectory);

  wxString applicationDrive;
  wxFileName::SplitPath(executablePath.GetPath(), &applicationDrive, NULL, NULL, NULL, false, wxPATH_NATIVE);

  FilePaths::ApplicationDrive_ = applicationDrive;
  #ifdef __WINDOWS__
  FilePaths::ApplicationDrive_ += _T(":");
  #endif // __WINDOWS__

  FilePaths::ApplicationDirectory_ = applicationDirectory;
  FilePaths::DataDirectory_ = applicationDirectory + _T("/") + DATA_FOLDER_NAME;  

  const wxCmdLineParser& commandLine = wxGetApp().GetCommandLine();

  wxString userDataPath;
  bool found = commandLine.Found(_T("d"), &userDataPath);
  if (found) {
    FilePaths::SettingsDirectory_ = userDataPath;
  } else if (wxGetApp().GetCommandLineFound(_T("u"))) {
    FilePaths::SettingsDirectory_ = wxString::Format(_T("%s/%s/%s"), wxStandardPaths().GetUserConfigDir(), APPLICATION_NAME, SETTING_FOLDER_NAME);
  } else {
    FilePaths::SettingsDirectory_ = FilePaths::GetDataDirectory() + _T("/") + SETTING_FOLDER_NAME;
  }

  FilePaths::LocalesDirectory_ = FilePaths::GetDataDirectory() + _T("/") + LOCALES_FOLDER_NAME;
  FilePaths::SettingsFile_ = FilePaths::GetSettingsDirectory() + _T("/") + SETTING_FILE_NAME;
  FilePaths::FolderItemsFile_ = FilePaths::GetSettingsDirectory() + _T("/") + FOLDER_ITEMS_FILE_NAME;
  FilePaths::WindowFile_ = FilePaths::GetSettingsDirectory() + _T("/") + WINDOW_FILE_NAME;  
  FilePaths::HelpDirectory_ = FilePaths::GetDataDirectory() + _T("/") + HELP_FOLDER_NAME;  
  FilePaths::BaseSkinDirectory_ = FilePaths::GetDataDirectory() + _T("/") + SKIN_FOLDER_NAME; 
  FilePaths::BaseSkinAssetsDirectory_ = FilePaths::BaseSkinDirectory_ + _T("/") + BASE_SKIN_ASSETS_FOLDER_NAME; 
  FilePaths::DefaultSkinDirectory_ = FilePaths::BaseSkinDirectory_ + _T("/") + DEFAULT_SKIN;
  FilePaths::IconCacheDirectory_ = FilePaths::DataDirectory_ + _T("/") + ICON_CACHE_FOLDER_NAME;
  FilePaths::ToolsDirectory_ = FilePaths::DataDirectory_ + _T("/") + TOOLS_FOLDER_NAME;
  FilePaths::PluginsDirectory_ = FilePaths::DataDirectory_ + _T("/") + PLUGINS_FOLDER_NAME;
  FilePaths::PluginSettingsFile_ = FilePaths::GetSettingsDirectory() + _T("/") + PLUGINS_FILE_NAME;
  FilePaths::PluginPreferenceDirectory_ = FilePaths::GetSettingsDirectory() + _T("/") + PLUGINS_PREFERENCES_FOLDER_NAME;
}