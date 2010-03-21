/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <User.h>
#include <XmlUtil.h>
using namespace appetizer;

User::User() {
  rootFolderItem_ = NULL;
}


void User::load() {
  TiXmlDocument doc("s:\\Docs\\PROGS\\C++\\Appetizer\\branches\\QtVersion\\Data\\Settings\\FolderItems.xml");
  doc.LoadFile(TIXML_ENCODING_UTF8);

  TiXmlElement* root = doc.FirstChildElement("FolderItems");
  if (!root) {
    qWarning() << "User::load: Could not load XML. No FolderItems element found.";
    return;
  }

  rootFolderItem_ = FolderItem::createFolderItem(true);
  
  for (TiXmlElement* element = root->FirstChildElement(); element; element = element->NextSiblingElement()) {
    QString elementName = QString::fromUtf8(element->Value());

    if (elementName == "FolderItem" || elementName == "appFolderItem") {
      FolderItem* folderItem = FolderItem::createFolderItem();
      folderItem->fromXml(element);

      //rootFolderItem_->AddChild(folderItem);
    } else if (elementName == "ExcludedPath") {
      //const char* cString = element->GetText();
      //if (!cString) continue;
      //wxString path = wxString::FromUTF8(cString);
      //path.Trim(true).Trim(false);
      //if (path == wxEmptyString) continue;
      //autoAddExclusions_.Add(appFolderItem::ConvertToRelativePath(path));
    } else {
      qWarning() << QString("User::Load: Unknown element: %s").arg(elementName);
    }
  }

  qDebug() << "done";
}