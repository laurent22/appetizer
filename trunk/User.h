#ifndef __User_H
#define __User_H

#include "wx/wx.h"
#include <wx/fileconf.h>
#include "FolderItem.h"
#include "UserSettings.h"
#include <vector>
using namespace std;
#include "boost/shared_ptr.hpp"


class User {

public:

  User();  
  void AutomaticallyAddNewApps();
  UserSettingsSP GetSettings();
  void SaveAll();
  void LoadAll();

  std::vector<FolderItemSP> GetFolderItems();
  FolderItemSP GetFolderItemById(int folderItemId);
  void DeleteFolderItem(int folderItemId);
  void DumpFolderItems();

private:

  std::vector<FolderItemSP> folderItems_;
  UserSettingsSP settings_;

};


typedef boost::shared_ptr<User> UserSP;


#endif