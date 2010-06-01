/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <FilePaths.h>
#include <Constants.h>
using namespace appetizer;

QString FilePaths::ApplicationDrive_ = "";
QString FilePaths::ApplicationDirectory_ = "";
QString FilePaths::DataDirectory_ = "";
QString FilePaths::SettingsDirectory_ = "";
QString FilePaths::BaseSkinDirectory_ = "";
QString FilePaths::DefaultSkinDirectory_ = "";
QString FilePaths::LocalesDirectory_ = "";
QString FilePaths::SettingsFile_ = "";
QString FilePaths::FolderItemsFile_ = "";
QString FilePaths::WindowFile_ = "";
QString FilePaths::HelpDirectory_ = "";
QString FilePaths::WindowsDirectory_ = "";
QString FilePaths::System32Directory_ = "";
QString FilePaths::ApplicationPath_ = "";
QString FilePaths::IconCacheDirectory_ = "";
QString FilePaths::ToolsDirectory_ = "";
QString FilePaths::PluginsDirectory_ = "";
QString FilePaths::PluginSettingsFile_ = "";
QString FilePaths::BaseSkinAssetsDirectory_ = "";
QString FilePaths::PluginPreferenceDirectory_ = "";


QString FilePaths::GetApplicationDrive() { return FilePaths::ApplicationDrive_; }
QString FilePaths::GetApplicationDirectory() { return FilePaths::ApplicationDirectory_; }
QString FilePaths::GetDataDirectory() { return FilePaths::DataDirectory_; }
QString FilePaths::GetSettingsDirectory() { return FilePaths::SettingsDirectory_; }
QString FilePaths::GetBaseSkinDirectory() { return FilePaths::BaseSkinDirectory_; }
QString FilePaths::GetSkinDirectory() { return "TODO"; /*FilePaths::BaseSkinDirectory_ + "/" + wxGetApp().GetUser()->GetSettings()->GetString("Skin");*/ }
QString FilePaths::GetDefaultSkinDirectory() { return FilePaths::DefaultSkinDirectory_; }
QString FilePaths::GetLocalesDirectory() { return FilePaths::LocalesDirectory_; }
QString FilePaths::GetHelpDirectory() { return FilePaths::HelpDirectory_; }
QString FilePaths::GetToolsDirectory() { return FilePaths::ToolsDirectory_; }
QString FilePaths::GetSettingsFile() { return FilePaths::SettingsFile_; }
QString FilePaths::GetFolderItemsFile() { return FilePaths::FolderItemsFile_; }
QString FilePaths::GetWindowFile() { return FilePaths::WindowFile_; }
QString FilePaths::GetIconCacheDirectory() { return FilePaths::IconCacheDirectory_; }
QString FilePaths::GetPluginsDirectory() { return FilePaths::PluginsDirectory_; }
QString FilePaths::GetPluginSettingsFile() { return FilePaths::PluginSettingsFile_; }
QString FilePaths::GetBaseSkinAssetsDirectory() { return FilePaths::BaseSkinAssetsDirectory_; }
QString FilePaths::GetPluginPreferenceDirectory() { return FilePaths::PluginPreferenceDirectory_; }
QString FilePaths::GetWindowsFontDirectory() { return GetWindowsDirectory() + "\\fonts"; }
QString FilePaths::GetTempDirectory() { return GetSettingsDirectory() + "\\Temp"; }


QString FilePaths::GetQuickLaunchDirectory() { 
  return FilePaths::resolveVariables("%APPDATA%\\Microsoft\\Internet Explorer\\Quick Launch");
}


QString FilePaths::GetApplicationPath() {
  if (ApplicationPath_ != "") return ApplicationPath_;

  #ifdef __WINDOWS__
  TCHAR cName[MAX_PATH + 10];
	GetModuleFileName(0, cName, MAX_PATH + 10);
  ApplicationPath_ = QString::fromWCharArray(cName);
  #endif

  return ApplicationPath_;
}


QString FilePaths::GetFontFilePath(const QString& fontAlias) {
  QSettings settings("HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Fonts",  QSettings::NativeFormat);
  QString filename = settings.value(fontAlias + " (TrueType)", "").toString();
  if (filename == "") {
    qWarning() << "Cannot get font filename from HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Fonts";
  }

  return GetWindowsFontDirectory() + "\\" + filename;
}


QString FilePaths::GetSystem32Directory() {
  if (FilePaths::System32Directory_ != "") return FilePaths::System32Directory_;

  QString output;

  // LPTSTR is wchar_t if UNICODE is enabled, or a char otherwise
  LPTSTR buffer = new TCHAR[MAX_PATH];
  int success = GetSystemDirectory(buffer, MAX_PATH);   

  if (!success) {
    qWarning() << "WARNING: coulnd't get System32 directory";
    output = "c:\\window\\system32";
  } else {
    // Convert the LPTSTR to a char*
    output = QString::fromWCharArray(buffer);
  }

  delete buffer;

  FilePaths::System32Directory_ = output;

  return output;
}


QString FilePaths::GetWindowsDirectory() {
  if (FilePaths::WindowsDirectory_ != "") return FilePaths::WindowsDirectory_;

  LPTSTR buffer = new TCHAR[MAX_PATH];
  int success = ::GetWindowsDirectory(buffer, MAX_PATH);
  QString windowsPath;
  if (!success) {
    windowsPath = "c:\\windows";
  } else {
    windowsPath = QString::fromWCharArray(buffer);
  }

  delete buffer;

  FilePaths::WindowsDirectory_ = windowsPath;

  return FilePaths::WindowsDirectory_;
}


QString FilePaths::GetUserShellDirectory(const QString& itemName) {
  QSettings settings("HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\User Shell Folders",  QSettings::NativeFormat);
  QString filePath = settings.value(itemName, "").toString();
  if (filePath == "") {
    qWarning() << "Could not get user path: " << itemName;
  }

  return filePath;
}


QString FilePaths::GetAllUsersShellDirectory(const QString& itemName) {
  QSettings settings("HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Explorer\\User Shell Folders",  QSettings::NativeFormat);
  QString filePath = settings.value(itemName, "").toString();
  if (filePath == "") {
    qWarning() << "Could not get user path: " << itemName;
  }

  return filePath;
}


/**
 * This method look for the given filename in the right directory. It first checks
 * the current skin directory then, if it can't find the file, it checks the default
 * skin directory.
 * @param filename The file to look for
 * @return The path to the file
 */
QString FilePaths::GetSkinFile(const QString& filename) {
  QString output = FilePaths::GetSkinDirectory() + "/" + filename;
  if (QFileInfo(output).exists()) return output;
  return GetBaseSkinAssetsDirectory() + "/" + filename;
}


QString FilePaths::resolveVariables(const QString& path) {
  if (!path.contains("%")) return path;

  QStringList splitted = path.split("%");
  QString output = "";

  for (int i = 0; i < splitted.size(); i ++) {
    QString current = splitted.at(i);

    if (i % 2 == 1) {
      char* content;
      content = getenv(current.toStdString().c_str());
      if (content == NULL) {
        qWarning() << "Cannot resolve variable: " << current;
        current = "";
      } else {
        current = QString::fromAscii(content);
        //delete content;
      }
    }

    output = output + current;
  }

  return output;
}


void FilePaths::CreateSettingsDirectory() {
  if (QFileInfo(FilePaths::GetSettingsDirectory()).exists()) return;
  QDir().mkpath(FilePaths::GetSettingsDirectory());
}


void FilePaths::CreateDirectoryIfNotExists(const QString& path) {
  if (QFileInfo(path).exists()) return;
  QDir().mkpath(path);
}


void FilePaths::InitializePaths() {
  QString executablePath = FilePaths::GetApplicationPath();
  QString applicationDirectory = QFileInfo(executablePath).path();
  QString applicationDrive;

  FilePaths::ApplicationDrive_ = "";
  #ifdef __WINDOWS__
  QStringList stringList = executablePath.split(":");
  if (stringList.size() > 0) {
    applicationDrive = stringList.at(0) + ":";
  }
  #endif

  FilePaths::ApplicationDrive_ = applicationDrive;
  FilePaths::DataDirectory_ = applicationDirectory + "/" + DATA_FOLDER_NAME; 



  //TODO - handle parameters and location of data folder

  FilePaths::SettingsDirectory_ = FilePaths::GetDataDirectory() + "/" + SETTING_FOLDER_NAME;


  //wxFileName executablePath = wxFileName(wxStandardPaths().GetExecutablePath());
  //QString applicationDirectory = appFolderItem::ResolvePath(executablePath.GetPath(), true);
  //QString applicationDrive;
  //wxFileName::SplitPath(executablePath.GetPath(), &applicationDrive, NULL, NULL, NULL, false, wxPATH_NATIVE);

  //FilePaths::ApplicationDrive_ = applicationDrive;
  //#ifdef __WINDOWS__
  //FilePaths::ApplicationDrive_ += ":";
  //#endif // __WINDOWS__

  //FilePaths::ApplicationDirectory_ = applicationDirectory;
  //FilePaths::DataDirectory_ = applicationDirectory + "/" + DATA_FOLDER_NAME;  

  //const wxCmdLineParser& commandLine = wxGetApp().GetCommandLine();

  //QString userDataPath;
  //bool found = commandLine.Found("d", &userDataPath);
  //if (found) {
  //  wxFileName f(userDataPath);
  //  if (f.IsRelative()) {
  //    userDataPath = FilePaths::GetApplicationDirectory() + "/" + userDataPath;
  //    f = wxFileName(userDataPath);
  //    f.Normalize();
  //    userDataPath = f.GetFullPath();
  //  }
  //  FilePaths::SettingsDirectory_ = userDataPath;
  //} else if (wxGetApp().GetCommandLineFound("u")) {
  //  FilePaths::SettingsDirectory_ = QString::Format("%s/%s/%s", wxStandardPaths().GetUserConfigDir(), APPLICATION_NAME, SETTING_FOLDER_NAME);
  //} else {
  //  FilePaths::SettingsDirectory_ = FilePaths::GetDataDirectory() + "/" + SETTING_FOLDER_NAME;
  //}

  FilePaths::LocalesDirectory_ = FilePaths::GetDataDirectory() + "/" + LOCALES_FOLDER_NAME;
  FilePaths::SettingsFile_ = FilePaths::GetSettingsDirectory() + "/" + SETTING_FILE_NAME;
  FilePaths::FolderItemsFile_ = FilePaths::GetSettingsDirectory() + "/" + FOLDER_ITEMS_FILE_NAME;
  FilePaths::WindowFile_ = FilePaths::GetSettingsDirectory() + "/" + WINDOW_FILE_NAME;  
  FilePaths::HelpDirectory_ = FilePaths::GetDataDirectory() + "/" + HELP_FOLDER_NAME;  
  FilePaths::BaseSkinDirectory_ = FilePaths::GetDataDirectory() + "/" + SKIN_FOLDER_NAME; 
  FilePaths::BaseSkinAssetsDirectory_ = FilePaths::BaseSkinDirectory_ + "/" + BASE_SKIN_ASSETS_FOLDER_NAME; 
  FilePaths::DefaultSkinDirectory_ = FilePaths::BaseSkinDirectory_ + "/" + DEFAULT_SKIN;
  FilePaths::IconCacheDirectory_ = FilePaths::DataDirectory_ + "/" + ICON_CACHE_FOLDER_NAME;
  FilePaths::ToolsDirectory_ = FilePaths::DataDirectory_ + "/" + TOOLS_FOLDER_NAME;
  FilePaths::PluginsDirectory_ = FilePaths::DataDirectory_ + "/" + PLUGINS_FOLDER_NAME;
  FilePaths::PluginSettingsFile_ = FilePaths::GetSettingsDirectory() + "/" + PLUGINS_FILE_NAME;
  FilePaths::PluginPreferenceDirectory_ = FilePaths::GetSettingsDirectory() + "/" + PLUGINS_PREFERENCES_FOLDER_NAME;
}