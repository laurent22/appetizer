#ifndef __User_H
#define __User_H

#include "wx/wx.h"
#include "FolderItem.h"
#include <wx/xml/xml.h>
#include <vector>
using namespace std;


class User {

public:

  User();
  std::vector<FolderItem*> GetFolderItems();
  void AutomaticallyAddNewApps();
  void LoadSettings();
  void SetConfig(const wxString& name, const wxString& value);
  wxString GetConfig(const wxString& name);

private:

  std::vector<FolderItem*> folderItems_;
  //wxXmlDocument configDoc_;

};


#endif