#include "wx/wx.h" 
#include "wx/stdpaths.h"
#include "wx/filename.h"
#include "Controller.h"
#include "Constants.h"
#include "MainFrame.h"



extern MainFrame* gMainFrame;


Controller::Controller() {
  draggedFolderItemId_ = -1;  
  stopWatch_.Start();
  user_.reset(new User());
}


long Controller::GetTimer() {
  return stopWatch_.Time();
}


int Controller::ShowErrorMessage(const wxString& message, long style) {
  wxMessageDialog dialog(NULL, message, _T("Error"), style | wxICON_ERROR);
  return dialog.ShowModal();
}


int Controller::ShowWarningMessage(const wxString& message, long style) {
  wxMessageDialog dialog(NULL, message, _T("Warning"), style | wxICON_EXCLAMATION);
  return dialog.ShowModal();
}


void Controller::SetDraggedFolderItem(int folderItemId) {
  draggedFolderItemId_ = folderItemId;
}


FolderItemSP Controller::GetDraggedFolderItem() {
  if (draggedFolderItemId_ < 0 || !user_) {
    FolderItemSP nullOutput;
    return nullOutput;
  }
  return user_->GetFolderItemById(draggedFolderItemId_);
}


wxString Controller::GetApplicationDrive() {
  return applicationDrive_;
}


UserSP Controller::GetUser() {
  return user_;
}


void Controller::User_FolderItemCollectionChange() {
  gMainFrame->GetIconPanel()->InvalidateIcons();
}

