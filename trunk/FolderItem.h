#ifndef __FolderItem_H
#define __FolderItem_H

#include "wx/wx.h"

class FolderItem {

public:

  FolderItem();
  int GetId() const;
  wxString GetResolvedFilePath();
  wxString GetFilePath();
  wxIcon* GetIcon(int iconSize);
  void SetFilePath(const wxString& filePath);
  static wxString ResolvePath(const wxString& filePath);
  void ClearCachedIcons();

private:

  static int uniqueID_;

  int id_;
  wxString filePath_;
  wxIcon* icon16_;
  wxIcon* icon32_;  

};


#endif // __FolderItem_H