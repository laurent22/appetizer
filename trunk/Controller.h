#ifndef __Controller_H
#define __Controller_H

#include "wx/wx.h" 
#include "MainFrame.h"
#include "Constants.h"
#include "User.h"
#include "boost/shared_ptr.hpp"
#include <wx/msgdlg.h>


struct FilePaths {
  wxString ApplicationDirectory;
  wxString DataDirectory;
  wxString SettingsDirectory;
  wxString SkinDirectory;
  wxString LocalesDirectory;
  wxString UserSettingsFile;
  wxString IconsDirectory;
  wxString ConfigFile;
  wxString FolderItemsFile;
};

struct MainPanelStyle {
  int PaddingLeft;
  int PaddingRight;
  int PaddingTop;
  int PaddingBottom;
  int PaddingWidth;
  int PaddingHeight;
};

struct InnerPanelStyle {
  int PaddingLeft;
  int PaddingRight;
  int PaddingTop;
  int PaddingBottom;
  int PaddingWidth;
  int PaddingHeight;
};

struct OptionPanelStyle {
  int PaddingLeft;
  int PaddingRight;
  int PaddingTop;
  int PaddingBottom;
  int PaddingWidth;
  int PaddingHeight;
};

struct IconTooltipStyle {
  int PaddingLeft;
  int PaddingRight;
  int PaddingTop;
  int PaddingBottom;
  int PaddingWidth;
  int PaddingHeight;
  wxColor FontColor;
};

struct IconStyle {
  int PaddingLeft;
  int PaddingRight;
  int PaddingTop;
  int PaddingBottom;
  int PaddingWidth;
  int PaddingHeight;
};


struct ControllerStyles {
  MainPanelStyle MainPanel;
  InnerPanelStyle InnerPanel;
  IconStyle Icon;
};


class Controller {

  public:

    Controller();      
    ControllerStyles GetStyles();
    FilePaths GetFilePaths();
    UserSP GetUser();
    wxString GetApplicationDrive();
    void SetDraggedFolderItem(int folderItemId);
    FolderItemSP GetDraggedFolderItem();
    int ShowWarningMessage(const wxString& message, long style = wxOK);
    int ShowErrorMessage(const wxString& message, long style = wxOK);

    /**
     * GLOBAL EVENTS
     */

    void User_FolderItemCollectionChange();

  private:
      
    ControllerStyles styles_;
    FilePaths filePaths_;
    UserSP user_;
    wxString applicationDrive_;
    int draggedFolderItemId_;

};

typedef boost::shared_ptr<Controller> ControllerSP;

#endif
