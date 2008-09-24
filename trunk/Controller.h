#ifndef __Controller_H
#define __Controller_H

#include "wx/wx.h" 
#include "MainFrame.h"
#include "Constants.h"
#include "User.h"


struct FilePaths {
  wxString ApplicationDirectory;
  wxString DataDirectory;
  wxString SettingsDirectory;
  wxString SkinDirectory;
  wxString LocalesDirectory;
  wxString UserSettingsFile;
  wxString IconsDirectory;
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
};


class Controller {

  public:

    Controller();      
    MainFrame* GetMainFrame();
    void SetMainFrame(MainFrame* mainFrame);
    ControllerStyles GetStyles();
    FilePaths GetFilePaths();
    User GetUser();

  private:
      
    MainFrame* mainFrame_;
    ControllerStyles styles_;
    FilePaths filePaths_;
    User user_;

};

#endif
