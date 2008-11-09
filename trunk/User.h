/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

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
#include "boost/shared_ptr.hpp"
#include "gui/GroupEditorDialog.h"
#include "gui/ShortcutEditorDialog.h"


class User : public wxEvtHandler {

public:

  User();  
  void AutomaticallyAddNewApps();
  UserSettingsSP GetSettings();
  void ScheduleSave();
  void Save(bool force = false);
  void Load();
  
  int EditFolderItem(FolderItemSP folderItem);
  FolderItemSP EditNewFolderItem(FolderItemSP parent, bool isGroup = false);
  FolderItemSP AddNewFolderItemFromPath(FolderItemSP parent, wxString folderItemPath);
  void AddAutoAddExclusion(const wxString& filePath);
  bool IsAutoAddExclusion(const wxString& filePath);

  FolderItemSP GetRootFolderItem();

  void OnTimer(wxTimerEvent& evt);

private:

  FolderItemSP rootFolderItem_;
  wxTimer* scheduledSaveTimer_;
  wxStringList folderItemExclusions_;
  UserSettingsSP settings_;
  ShortcutEditorDialog* shortcutEditorDialog_;
  GroupEditorDialog* groupEditorDialog_;
  wxArrayString autoAddExclusions_;

  DECLARE_EVENT_TABLE()

};


typedef boost::shared_ptr<User> UserSP;


#endif