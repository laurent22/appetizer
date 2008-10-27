#ifndef __FolderItem_H
#define __FolderItem_H

#include "wx/wx.h"
#include "boost/shared_ptr.hpp"
#include "TypeDefinitions.h"
#include "utilities/XmlUtil.h"


class FolderItem {

public:

  FolderItem();
  int GetId() const;
  void AutoSetName();
  wxString GetName();
  wxString GetResolvedPath();
  wxString GetFilePath();
  wxIconSP GetIcon(int iconSize);
  void SetFilePath(const wxString& filePath);
  void SetName(const wxString& name);  
  void ClearCachedIcons();  
  void Launch();
  TiXmlElement* ToXML();

  static wxString ResolvePath(const wxString& filePath);
  static wxString ConvertToRelativePath(const wxString& filePath);

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