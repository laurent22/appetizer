/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __MiniLaunchBar_H
#define __MiniLaunchBar_H


#include <wx/wx.h>
#include <wx/snglinst.h>
#include <wx/cmdline.h>
#include "MainFrame.h"
#include "FolderItem.h"
#include "utilities/Utilities.h"


// Forward declarations:
class Utilities;
class MainFrame;


class MiniLaunchBar: public wxApp {

public:

  const wxCmdLineParser& GetCommandLine();
  MainFrame* GetMainFrame();
  Utilities& GetUtilities();
  long GetTimer();
  bool IsFirstLaunch();
  void SetDraggedFolderItem(int folderItemId);
  FolderItemSP GetDraggedFolderItem();
  User* GetUser();
  void CheckForNewVersion(bool silent = false);
  bool ChangeLocale(const wxString& localeCode);
  void CloseApplication();

  void FolderItems_CollectionChange();
  void FolderItems_FolderItemChange(FolderItemSP folderItem);
  void User_LocaleChange();
  void User_IconSizeChange();

private:
  
  virtual bool OnInit();
  virtual int OnExit();
  wxSingleInstanceChecker* singleInstanceChecker_;
  wxCmdLineParser commandLine_;
  MainFrame* mainFrame_;
  Utilities utilities_;
  wxStopWatch stopWatch_;
  bool isFirstLaunch_;
  int draggedFolderItemId_;
  User* user_;
  wxLocale* locale_;

};


DECLARE_APP(MiniLaunchBar)


#endif // __MiniLaunchBar_H