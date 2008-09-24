#include "wx/wx.h" 
#include "MainFrame.h"
#include "wx/sysopt.h"
#include "Controller.h"


// The application class. An instance is created and initialized
// below in IMPLEMENT_APP()
class MiniLaunchBar: public wxApp {
  virtual bool OnInit();
};


IMPLEMENT_APP(MiniLaunchBar) 


// Initialize the global controller
Controller gController = Controller();


bool MiniLaunchBar::OnInit() {
  // Required to enabled PNG support
  wxInitAllImageHandlers();

  // Setting this option to "0" removed the flickering.
  wxSystemOptions::SetOption(wxT("msw.window.no-clip-children"), wxT("0"));

  MainFrame *frame = new MainFrame();
  frame->Show(true);
  frame->SetBackgroundStyle(wxBG_STYLE_CUSTOM);

  SetTopWindow(frame);

  return true;
} 