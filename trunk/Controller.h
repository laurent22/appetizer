#ifndef __Controller_H
#define __Controller_H

#include "wx/wx.h" 
#include "MainFrame.h"
#include "StyleSelector.h"


struct ControllerStyles {
  StyleSelector BackgroundPanel;
  StyleSelector IconPanel;
};



class Controller {

  public:

    Controller();      
    MainFrame* GetMainFrame();
    void SetMainFrame(MainFrame* mainFrame);
    ControllerStyles GetStyles();

  private:
      
    MainFrame* pMainFrame;
    ControllerStyles pStyles;

};






#endif
