#ifndef __FilePaths_H
#define __FilePaths_H

#include "wx/wx.h"

class FilePaths {

public:

  static wxString ApplicationDrive;
  static wxString ApplicationDirectory;
  static wxString DataDirectory;
  static wxString SettingsDirectory;
  static wxString SkinDirectory;
  static wxString LocalesDirectory;
  static wxString UserSettingsFile;
  static wxString IconsDirectory;
  static wxString SettingsFile;
  static wxString FolderItemsFile;
  static wxString WindowFile;

};

#endif // __FilePaths_H