#ifndef __ShortcutEditorDialogBase_H
#define __ShortcutEditorDialogBase_H


#include <wx/wx.h>
#include "ShortcutEditorDialogBase.h"
#include "../FolderItem.h"


class ShortcutEditorDialog: public ShortcutEditorDialogBase {

public:

  ShortcutEditorDialog();
  void LoadFolderItem(FolderItemSP folderItem);
  void Localize();

private:

  FolderItemSP folderItem_;

  void UpdateFromFolderItem();

  void OnCancelButtonClick(wxCommandEvent& evt);
  void OnSaveButtonClick(wxCommandEvent& evt);
  void OnSelectFileButtonClick(wxCommandEvent& evt);
  void OnSelectFolderButtonClick(wxCommandEvent& evt);

  DECLARE_EVENT_TABLE();

};


#endif // __ShortcutEditorDialogBase_H