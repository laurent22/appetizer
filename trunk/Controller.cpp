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

  //***************************************************************************
  // Initialize paths
  //***************************************************************************

  wxFileName executablePath = wxFileName(wxStandardPaths().GetExecutablePath());

  wxString applicationDirectory = executablePath.GetPath();

  wxFileName::SplitPath(executablePath.GetPath(), &applicationDrive_, NULL, NULL, NULL, false, wxPATH_NATIVE);

  #ifdef __WIN32__
  applicationDrive_ += _T(":");
  #endif // __WIN32__

  filePaths_.ApplicationDirectory = applicationDirectory;
  filePaths_.DataDirectory = applicationDirectory + wxT("\\") + DATA_FOLDER_NAME;
  filePaths_.SettingsDirectory = filePaths_.DataDirectory + wxT("\\") + SETTING_FOLDER_NAME;
  filePaths_.SkinDirectory = filePaths_.DataDirectory + wxT("\\") + SKIN_FOLDER_NAME + wxT("\\Default");
  filePaths_.LocalesDirectory = filePaths_.DataDirectory + wxT("\\") + LOCALES_FOLDER_NAME;
  filePaths_.ConfigFile = filePaths_.SettingsDirectory + wxT("\\") + CONFIG_FILE_NAME;
  filePaths_.IconsDirectory = filePaths_.SkinDirectory + wxT("\\") + ICONS_FOLDER_NAME;
  filePaths_.FolderItemsFile = filePaths_.SettingsDirectory + wxT("\\") + FOLDER_ITEMS_FILE_NAME;

  //***************************************************************************
  // Initialize styles
  //***************************************************************************

  styles_ = ControllerStyles();

  styles_.MainPanel.PaddingLeft = 8;
  styles_.MainPanel.PaddingRight = 8;
  styles_.MainPanel.PaddingTop = 8;
  styles_.MainPanel.PaddingBottom = 8;
  styles_.MainPanel.PaddingWidth = styles_.MainPanel.PaddingLeft + styles_.MainPanel.PaddingRight;
  styles_.MainPanel.PaddingHeight = styles_.MainPanel.PaddingTop + styles_.MainPanel.PaddingBottom;

  styles_.InnerPanel.PaddingLeft = 8;
  styles_.InnerPanel.PaddingRight = 8;
  styles_.InnerPanel.PaddingTop = 8;
  styles_.InnerPanel.PaddingBottom = 8;
  styles_.InnerPanel.PaddingWidth = styles_.InnerPanel.PaddingLeft + styles_.InnerPanel.PaddingRight;
  styles_.InnerPanel.PaddingHeight = styles_.InnerPanel.PaddingTop + styles_.InnerPanel.PaddingBottom;

  styles_.Icon.PaddingLeft = 4;
  styles_.Icon.PaddingRight = 4;
  styles_.Icon.PaddingTop = 4;
  styles_.Icon.PaddingBottom = 4;
  styles_.Icon.PaddingWidth = styles_.Icon.PaddingLeft + styles_.Icon.PaddingRight;
  styles_.Icon.PaddingHeight = styles_.Icon.PaddingTop + styles_.Icon.PaddingBottom;

  styles_.OptionPanel.PaddingLeft = 4;
  styles_.OptionPanel.PaddingRight = 4;
  styles_.OptionPanel.PaddingTop = 4;
  styles_.OptionPanel.PaddingBottom = 4;
  styles_.OptionPanel.PaddingWidth = styles_.OptionPanel.PaddingLeft + styles_.OptionPanel.PaddingRight;
  styles_.OptionPanel.PaddingHeight = styles_.OptionPanel.PaddingTop + styles_.OptionPanel.PaddingBottom;
  styles_.OptionPanel.ArrowButtonWidth = 16;

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


ControllerStyles Controller::GetStyles() {
  return styles_;
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


FilePaths Controller::GetFilePaths() {
  return filePaths_;
}