/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __Controller_H
#define __Controller_H

#include <wx/wx.h>
#include <wx/stopwatch.h>
#include "MainFrame.h"
#include "Constants.h"
#include "User.h"

class Controller {

  public:

    Controller();   
    ~Controller();
    UserSP GetUser();
    void SetDraggedFolderItem(int folderItemId);
    FolderItemSP GetDraggedFolderItem();
    long GetTimer();
    void CheckForNewVersion(bool silent = false);
    bool IsFirstLaunch();
    void SetIsFirstLaunch(bool value);
    void InitializeLocalization();
    bool ChangeLocale(const wxString& localeCode);

    /**
     * GLOBAL EVENTS
     */

    void FolderItems_CollectionChange();
    void FolderItems_FolderItemChange(FolderItemSP folderItem);
    void User_LocaleChange();
    void User_IconSizeChange();

  private:
      
    wxLocale* locale_;
    bool isFirstLaunch_;
    UserSP user_;
    wxString applicationDrive_;
    int draggedFolderItemId_;
    wxStopWatch stopWatch_;

};

#endif
