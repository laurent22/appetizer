#include "wx/wx.h" 
#include "MainFrame.h"
#include "OptionFrame.h"
#include "wx/sysopt.h"
#include "Controller.h"



// The application class. An instance is created and initialized
// below in IMPLEMENT_APP()
class MiniLaunchBar: public wxApp {
  virtual bool OnInit();
};


IMPLEMENT_APP(MiniLaunchBar) 


// Initialize the global controller
ControllerSP gController;

// We can't use a smart pointer for the main frame
// since it's going to be owned by the wxApp
MainFrame* gMainFrame;

OptionFrame* gOptionFrame;


bool MiniLaunchBar::OnInit() {
  // Required to enabled PNG support
  wxInitAllImageHandlers();

  // Setting this option to "0" removed the flickering.
  wxSystemOptions::SetOption(wxT("msw.window.no-clip-children"), wxT("0"));

  gController.reset(new Controller());

  gMainFrame = new MainFrame();
  gMainFrame->Show(true);
  //gMainFrame->SetBackgroundStyle(wxBG_STYLE_CUSTOM);

  gOptionFrame = new OptionFrame();
  gOptionFrame->Show(true);

  gController->GetUser()->Load();
  gController->GetUser()->AutomaticallyAddNewApps();

  SetTopWindow(gMainFrame);

  return true;
} 