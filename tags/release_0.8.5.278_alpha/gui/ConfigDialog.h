#ifndef __ConfigDialog_H
#define __ConfigDialog_H


#include <wx/wx.h>
#include "ConfigDialogBase.h"


class ConfigDialog: public ConfigDialogBase {

public:

  ConfigDialog();
  void Localize();
  void LoadSettings();

private:

  void OnCancelButtonClick(wxCommandEvent& evt);
  void OnSaveButtonClick(wxCommandEvent& evt);
  void OnShow(wxShowEvent& evt);

  DECLARE_EVENT_TABLE();

};


#endif // __ConfigDialog_H