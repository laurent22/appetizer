#ifndef __User_H
#define __User_H

#include "wx/wx.h"
#include <wx/fileconf.h>
#include "FolderItem.h"
#include "UserSettings.h"
#include <vector>
using namespace std;


class User {

public:

  User();
  std::vector<FolderItem*> GetFolderItems();
  void AutomaticallyAddNewApps();
  void LoadSettings();
  UserSettings* GetSettings();
  FolderItem* GetFolderItemById(int folderItemId);

private:

  std::vector<FolderItem*> folderItems_;
  UserSettings* settings_;

};


#endif