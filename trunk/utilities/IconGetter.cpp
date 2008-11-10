/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "IconGetter.h"
#include "wx/wx.h"
#include "StringUtil.h"
#include "../third_party/simpleini/SimpleIni.h"
#include "wx/mimetype.h"
#include "wx/filename.h"

#ifdef __WINDOWS__
#include <wx/msw/registry.h>
#endif // __WINDOWS__


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
    wxDELETE(buffer);

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

  // Fixes a bug in wxWidgets: Sometime the icon is negative, in which case the wxIconLocation will be invalid
  if (iconLocation.GetIndex() < 0) iconLocation.SetIndex(0);

  // Fixes a bug in wxWidgets: The filename is sometime surrounded by quotes, and so the wxIconLocation will
  // again be invalid. We remove the quotes below:
  wxString iconLocFile = iconLocation.GetFileName();
  while (iconLocFile[0] == _T('"')) iconLocFile = iconLocFile.Mid(1, iconLocFile.Len());
  while (iconLocFile[iconLocFile.Len() - 1] == _T('"')) iconLocFile = iconLocFile.Mid(0, iconLocFile.Len() - 1);
  iconLocation.SetFileName(iconLocFile);

  wxDELETE(fileType);

  if (success) {
    wxIcon* icon = new wxIcon(iconLocation);
    icon->SetSize(iconSize, iconSize);
    return icon;
  } else {

    // If we couldn't find the icon at this stage, look for it in the registry. The icon file path
    // may be stored in many different places. We need to look in:
    //
    // HKEY_CLASSES_ROOT\.<extension>\DefaultIcon\<defaultValue>
    // HKEY_CLASSES_ROOT\<documentType>\DefaultIcon\<defaultValue>
    // HKEY_CLASSES_ROOT\CLSID\<classId>\DefaultIcon\<defaultValue>
    //
    // The document type can be read at HKEY_CLASSES_ROOT\.<extension>\<defaultValue>
    // The class ID is (sometime) at HKEY_CLASSES_ROOT\<documentType>\CLSID\<defaultValue>
    // 
    // Note: all the DefaultIcon values already seem to be handled by wxWidgets so we
    // only care about the CLSID case below.
    // 
    // If there is no class ID, we could also look in:
    // HKEY_CLASSES_ROOT\<documentType>\shell\open\command
    // then get the associated executable from there and, finally, get the icon.
    // This is currently not implemented.
    //
    // Finally, some icons are displayed using an icon handler that can be found at:
    // HKEY_CLASSES_ROOT\<documentType>\ShellEx\IconHandler\<defaultValue>
    // This is a special module which displays the icon depending on the file content. It
    // may actually display two different icons for the same file extension. For example,
    // a C# .sln file is going to be displayed differently than a C++ .sln file. These cases
    // are hopefully rare enough to ignore them.

    wxRegKey* regKey = new wxRegKey(_T("HKEY_CLASSES_ROOT\\.") + fileExtension);
    wxString iconPath;

    if (regKey->Exists()) {
      wxString extensionLink = regKey->QueryDefaultValue();    

      if (extensionLink != wxEmptyString) {
        wxDELETE(regKey);
        regKey = new wxRegKey(_T("HKEY_CLASSES_ROOT\\") + extensionLink + _T("\\CLSID"));

        if (regKey->Exists()) {
          wxString classIdLink = regKey->QueryDefaultValue();

          if (classIdLink != wxEmptyString) {
            wxDELETE(regKey);
            regKey = new wxRegKey(_T("HKEY_CLASSES_ROOT\\CLSID\\") + classIdLink + _T("\\DefaultIcon"));
            
            if (regKey->Exists()) {
              iconPath = regKey->QueryDefaultValue();
            }            
          }
        }   
      }
    }

    wxDELETE(regKey);
     
    if (iconPath != wxEmptyString) {
      // The icon path may be specified with coma or followed by the icon index, as in:
      // c:\WINDOWS\Installer\{AC76BA86-7AD7-1033-7B44-A90000000001}\PDFFile_8.ico,0
      // So we need to split the string and extract the file path and index.

      wxArrayString splitted;

      if (iconPath.Find(_T(';')) != wxNOT_FOUND) {
        StringUtil::Split(iconPath, splitted, _T(";"));
      } else {
        StringUtil::Split(iconPath, splitted, _T(","));
      }
      
      iconPath = splitted[0];
      long iconIndex = 0;

      // If the conversion to long fails, we default to "0" which should be safe
      if (splitted.Count() > 1) splitted[1].ToLong(&iconIndex);
      
      // Sometime, the icon index is negative, for example for .resx files. Not sure
      // why since the icon actually displayed by Windows is at index 0.
      if (iconIndex < 0) iconIndex = 0;

      wxFileName f(iconPath);
      wxString e = f.GetExt().Lower();
      // Normally, it shouldn't be anything other than ico, exe or dll. If it is, we just return NULL for now.
      if (e == _T("ico") || e == _T("exe") || e == _T("dll")) return IconGetter::GetExecutableIcon(iconPath, iconSize, iconIndex);
    }

  }
  
  #endif // __WINDOWS__
  
  return NULL;
}


wxIcon* IconGetter::GetExecutableIcon(const wxString& filePath, int iconSize, int iconIndex) {  
  #ifdef __WINDOWS__

  HICON smallIcon;
  HICON largeIcon;
  int result;

  if (iconSize == 16) {
    result = ExtractIconEx(filePath.c_str(), iconIndex, NULL, &smallIcon, 1);	
  } else {
    result = ExtractIconEx(filePath.c_str(), iconIndex, &largeIcon, NULL, 1);	
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