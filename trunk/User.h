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
  void AutomaticallyAddNewApps();
  UserSettings* GetSettings();
  void SaveAll();
  void LoadAll();

  std::vector<FolderItem*> GetFolderItems();
  FolderItem* GetFolderItemById(int folderItemId);
  void DeleteFolderItem(int folderItemId);
  void DumpFolderItems();

private:

  std::vector<FolderItem*> folderItems_;
  UserSettings* settings_;

};


#endif