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


UserSP Controller::GetUser() {
  return user_;
}


void Controller::User_FolderItemCollectionChange() {
  gMainFrame->GetIconPanel()->InvalidateIcons();
  GetUser()->ScheduleSave();
}


void Controller::User_FolderItemChange(FolderItemSP folderItem) {
  FolderItemRendererSP renderer = gMainFrame->GetIconPanel()->GetRendererFromFolderItem(*folderItem);
  if (!renderer.get()) {
    // The folder item is not on the icon panel. It
    // may happen if it has just been created.
    return;
  }

  renderer->InvalidateControlBitmap();
  GetUser()->ScheduleSave();
}


void Controller::User_LocaleChange() {
  gMainFrame->Localize();
}


void Controller::User_IconSizeChange() {
  gMainFrame->GetIconPanel()->ClearIcons();
  gMainFrame->GetIconPanel()->InvalidateIcons();
  gMainFrame->GetIconPanel()->InvalidateLayout();
}