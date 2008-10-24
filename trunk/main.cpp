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
ControllerSP gController;

// We can't use a smart pointer for the main frame
// since it's going to be owned by the wxApp
MainFrame* gMainFrame;


bool MiniLaunchBar::OnInit() {
  // Required to enabled PNG support
  wxInitAllImageHandlers();

  // Setting this option to "0" removed the flickering.
  wxSystemOptions::SetOption(_T("msw.window.no-clip-children"), _T("0"));

  gController.reset(new Controller());

  gMainFrame = new MainFrame();
  gMainFrame->Show(true);
  // @todo: this line doesn't do anything:
  //gMainFrame->SetBackgroundStyle(wxBG_STYLE_CUSTOM);

  gController->GetUser()->Load();
  gController->GetUser()->AutomaticallyAddNewApps();

  gMainFrame->SetSize(100, 100, 400, 100);

  SetTopWindow(gMainFrame);

  return true;
} 