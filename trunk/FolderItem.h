#ifndef __FolderItem_H
#define __FolderItem_H

#include "wx/wx.h"
#include "boost/shared_ptr.hpp"
#include "TypeDefinitions.h"

class FolderItem {

public:

  FolderItem();
  int GetId() const;
  void AutoSetName();
  wxString GetName();
  wxString GetResolvedFilePath();
  wxString GetFilePath();
  wxIconSP GetIcon(int iconSize);
  void SetFilePath(const wxString& filePath);
  void SetName(const wxString& name);
  static wxString ResolvePath(const wxString& filePath);
  void ClearCachedIcons();

private:

  static int uniqueID_;

  int id_;
  wxString name_;
  wxString filePath_;
  wxIconSP icon16_;
  wxIconSP icon32_;  

};


typedef boost::shared_ptr<FolderItem> FolderItemSP;


#endif // __FolderItem_H