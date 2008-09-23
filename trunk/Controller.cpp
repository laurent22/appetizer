#include "Controller.h"
#include "wx/wx.h" 
#include "StyleSelector.h"


Controller::Controller() {
  pMainFrame = NULL;

  pStyles = ControllerStyles();

  // paddingWidth and paddingHeight are here just as a shortcut to easily 
  // get the total vertical or horizontal space taken by the padding

  pStyles.BackgroundPanel.SetStyleInt(wxT("paddingLeft"), 8);
  pStyles.BackgroundPanel.SetStyleInt(wxT("paddingRight"), 8);
  pStyles.BackgroundPanel.SetStyleInt(wxT("paddingTop"), 8);
  pStyles.BackgroundPanel.SetStyleInt(wxT("paddingBottom"), 8);
  pStyles.BackgroundPanel.SetStyleInt(wxT("paddingWidth"), pStyles.BackgroundPanel.GetStyleInt(wxT("paddingLeft")) + pStyles.BackgroundPanel.GetStyleInt(wxT("paddingRight")));
  pStyles.BackgroundPanel.SetStyleInt(wxT("paddingHeight"), pStyles.BackgroundPanel.GetStyleInt(wxT("paddingTop")) + pStyles.BackgroundPanel.GetStyleInt(wxT("paddingBottom")));

  pStyles.IconPanel.SetStyleInt(wxT("paddingLeft"), 4);
  pStyles.IconPanel.SetStyleInt(wxT("paddingRight"), 4);
  pStyles.IconPanel.SetStyleInt(wxT("paddingTop"), 8);
  pStyles.IconPanel.SetStyleInt(wxT("paddingBottom"), 8);
  pStyles.IconPanel.SetStyleInt(wxT("paddingWidth"), pStyles.IconPanel.GetStyleInt(wxT("paddingLeft")) + pStyles.IconPanel.GetStyleInt(wxT("paddingRight")));
  pStyles.IconPanel.SetStyleInt(wxT("paddingHeight"), pStyles.IconPanel.GetStyleInt(wxT("paddingTop")) + pStyles.IconPanel.GetStyleInt(wxT("paddingBottom")));

}


ControllerStyles Controller::GetStyles() {
  return pStyles;
}


MainFrame* Controller::GetMainFrame() {
  return pMainFrame;
}


void Controller::SetMainFrame(MainFrame* mainFrame) {

}