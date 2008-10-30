#ifndef __User_H
#define __User_H

#include <wx/wx.h>
#include <wx/event.h>
#include <wx/timer.h>
#include <wx/list.h>
#include <wx/arrstr.h>
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
  
  std::vector<FolderItemSP> GetFolderItems();
  FolderItemSP GetFolderItemById(int folderItemId);
  int EditFolderItem(FolderItemSP folderItem);
  FolderItemSP EditNewFolderItem();
  void DeleteFolderItem(int folderItemId);
  void MoveFolderItem(int folderItemId, int insertionIndex);
  void DumpFolderItems();
  void DoMultiLaunch();
  void AddAutoAddExclusion(const wxString& filePath);
  bool IsAutoAddExclusion(const wxString& filePath);

  void OnTimer(wxTimerEvent& evt);

private:

  wxTimer* scheduledSaveTimer_;
  std::vector<FolderItemSP> folderItems_;
  wxStringList folderItemExclusions_;
  UserSettingsSP settings_;
  ShortcutEditorDialog* shortcutEditorDialog_;
  wxArrayString autoAddExclusions_;

  DECLARE_EVENT_TABLE()

};


typedef boost::shared_ptr<User> UserSP;


#endif