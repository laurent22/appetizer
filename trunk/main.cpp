#include "wx/wx.h" 
#include "wx/dcbuffer.h"
#include "NineSlicesPainter.h"
#include "MainFrame.h"
#include "wx/sysopt.h"



class MiniLaunchBar: public wxApp {
  virtual bool OnInit();
};



IMPLEMENT_APP(MiniLaunchBar) 



bool MiniLaunchBar::OnInit() {
  // Required to enabled PNG support
  wxInitAllImageHandlers();

  // This line might be needed to enable alpha transparent components - to be checked
  //wxSystemOptions::SetOption(wxT("msw.window.no-clip-children"),wxT("1"));

  MainFrame *frame = new MainFrame();
  frame->Show(true);

  SetTopWindow(frame);
  return true;
} 