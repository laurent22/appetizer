#ifndef __User_H
#define __User_H

#include <wx/wx.h>
#include <wx/event.h>
#include <wx/timer.h>
#include "FolderItem.h"
#include "Enumerations.h"
#include "UserSettings.h"
#include <vector>
using namespace std;
#include "boost/shared_ptr.hpp"
#include "gui/ShortcutEditorDialog.h"


class User : public wxEvtHandler {

public:

  User();  
  void AutomaticallyAddNewApps();
  UserSettingsSP GetSettings();
  void ScheduleSave();
  void Save(bool force = false);
  void Load();

  wxTimer* scheduledSaveTimer_;
  std::vector<FolderItemSP> GetFolderItems();
  FolderItemSP GetFolderItemById(int folderItemId);
  int EditFolderItem(FolderItemSP folderItem);
  FolderItemSP EditNewFolderItem();
  void DeleteFolderItem(int folderItemId);
  void DumpFolderItems();

  void OnTimer(wxTimerEvent& evt);

private:

  std::vector<FolderItemSP> folderItems_;
  UserSettingsSP settings_;
  ShortcutEditorDialog* shortcutEditorDialog_;

  DECLARE_EVENT_TABLE()

};


typedef boost::shared_ptr<User> UserSP;


#endif