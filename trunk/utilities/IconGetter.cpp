/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "IconGetter.h"
#include "wx/wx.h"
#include "../third_party/simpleini/SimpleIni.h"
#include "wx/mimetype.h"
#include "wx/filename.h"


wxIcon* IconGetter::GetFolderItemIcon(const wxString& filePath, int iconSize) {
  if (wxFileName::DirExists(filePath)) {
    return GetFolderIcon(filePath, iconSize);
  } else {
    wxFileName filename(filePath);

    if ((filename.GetExt().CmpNoCase(wxT("exe")) == 0) || (filename.GetExt().CmpNoCase(wxT("ico")) == 0)) {
      return GetExecutableIcon(filePath, iconSize);
    } else {
      return GetDocumentIcon(filePath, iconSize);
    }
  }

  return NULL;
}


wxIcon* IconGetter::GetFolderIcon(const wxString& filePath, int iconSize) {
  #ifdef __WINDOWS__

  wxString desktopIniFilePath = filePath + wxT("/Desktop.ini");
  wxFileName filename(desktopIniFilePath);

  if (filename.FileExists()) {
    // If the folder contains a Desktop.ini file, try
    // to get the icon path from it
    CSimpleIniA ini(false, false, true);
    ini.LoadFile(desktopIniFilePath.c_str());
    const char* cIconFileValue = ini.GetValue(".ShellClassInfo", "IconFile", "");
    
    if (cIconFileValue != "") {
      wxString iconFileValue = wxString::FromAscii(cIconFileValue);
      // Normalize the path since the IconFile value is usually stored
      // as a path relative to the folder
      wxFileName iconFileName(iconFileValue);
      iconFileName.Normalize(wxPATH_NORM_ALL, filePath);
      iconFileValue = iconFileName.GetFullPath();
      
      return GetExecutableIcon(iconFileValue, iconSize);
    }
  }

  SHFILEINFO fileInfo;
  int success;

  wxFileName fileName(filePath);
  
  if (iconSize == 32) {
    success = SHGetFileInfo(fileName.GetPath().c_str(), FILE_ATTRIBUTE_DIRECTORY, &fileInfo,
                            sizeof(fileInfo), SHGFI_ICON | SHGFI_LARGEICON);    
  } else {
    success = SHGetFileInfo(fileName.GetPath().c_str(), FILE_ATTRIBUTE_DIRECTORY, &fileInfo,
                            sizeof(fileInfo), SHGFI_ICON | SHGFI_SMALLICON);
  }
 
  if (success != 0) {
    wxIcon* icon = new wxIcon();
    icon->SetHICON((WXHICON)fileInfo.hIcon);
    icon->SetSize(iconSize, iconSize);
    return icon;
  }

  #endif // __WINDOWS__

  return NULL;
}


wxIcon* IconGetter::GetDocumentIcon(const wxString& filePath, int iconSize) {
  #ifdef __WINDOWS__

  // Get the file type from the file extension
  wxFileType* fileType = NULL;

  wxFileName filename = wxFileName(filePath);
  wxString fileExtension = filename.GetExt();
  if (fileExtension != wxEmptyString) {
    fileType = wxTheMimeTypesManager->GetFileTypeFromExtension(fileExtension);
  }

  if (!fileType) {
    // The file type is not registered on the system, so try to get the default
    // icon from shell32.dll (works on Windows XP - not sure on Vista)

    // LPTSTR is wchar_t if UNICODE is enabled, or a char otherwise
    LPTSTR buffer = new TCHAR[MAX_PATH];
    int success = GetSystemDirectory(buffer, MAX_PATH);

    // Convert the LPTSTR to a char*
    char cString[MAX_PATH];
    wcstombs(cString, buffer, MAX_PATH);

    wxString shell32Path;
    if (success) {
      shell32Path = wxString::FromAscii(cString);
      shell32Path += wxT("\\SHELL32.DLL");
    } else {
      // If we couldn't get the system directory, try to guess it.
      // If the file doesn't exist, GetExecutableIcon() will return wxNullIcon
      shell32Path = wxT("C:\\WINDOWS\\SYSTEM32\\SHELL32.DLL");
    }

    return GetExecutableIcon(shell32Path, iconSize);
  }

  // Try to get the icon location from the file type
  wxIconLocation iconLocation;
  bool success = fileType->GetIcon(&iconLocation);

  if (success) {
    wxIcon* icon = new wxIcon(iconLocation);
    icon->SetSize(iconSize, iconSize);
    return icon;
  } else {
    // @todo: If we couldn't get the icon, we
    // should look for it in the registry.
    // Noticed some problems with .pdf and .sln files among others
  }
  
  #endif // __WINDOWS__
  
  return NULL;
}


wxIcon* IconGetter::GetExecutableIcon(const wxString& filePath, int iconSize) {  
  #ifdef __WINDOWS__

  HICON smallIcon;
  HICON largeIcon;
  int result;

  if (iconSize == 16) {
    result = ExtractIconEx(filePath.c_str(), 0, NULL, &smallIcon, 1);	
  } else {
    result = ExtractIconEx(filePath.c_str(), 0, &largeIcon, NULL, 1);	
  }

  // If the function succeeds, the return value is the handle to an icon.
  // If the file specified was not an executable file, DLL, or icon file,
  // the return value is 1. If no icons were found in the file, the return 
  // value is NULL. If the file didn't exist, the return value is < 0
  if (result > 0) {
    wxIcon* icon = new wxIcon();

    if (iconSize == 16) {
      icon->SetHICON((WXHICON)smallIcon);
      icon->SetSize(16, 16);
    } else {
      icon->SetHICON((WXHICON)largeIcon);
      icon->SetSize(32, 32);
    }
    return icon;
  }

  #endif // __WINDOWS__

  return NULL;
}