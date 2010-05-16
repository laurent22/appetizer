/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <FolderItem.h>
#include <XmlUtil.h>
#include <FilePaths.h>
using namespace appetizer;


FolderItem::FolderItem(bool isGroup) {
  isGroup_ = isGroup;
  autoRun_ = false;
  name_ = "";
  filePath_ = "";
  automaticallyAdded_ = false;
  belongsToMultiLaunchGroup_ = false;
}


QString FolderItem::resolvePath(const QString& path) {
  QString output = FilePaths::resolveVariables(path);
  return output;
}


FolderItem* FolderItem::createFolderItem(bool isGroup) {
  FolderItem* f = new FolderItem(isGroup);
  //folderItemIdHashMap_[f->GetId()] = f;
  return f;
}


void FolderItem::setName(const QString& name) {
  name_ = name;
}


void FolderItem::setFilePath(const QString& filePath) {
  filePath_ = filePath;
}


void FolderItem::setAutomaticallyAdded(bool automaticallyAdded) {
  automaticallyAdded_ = automaticallyAdded;
}


void FolderItem::setAutoRun(bool autoRun) {
  autoRun_ = autoRun;
}


TiXmlElement* FolderItem::toXml() {
  TiXmlElement* xml = new TiXmlElement("FolderItem");

  //XmlUtil::AppendTextElement(xml, "FilePath", GetFilePath());
  //XmlUtil::AppendTextElement(xml, "Name", GetName());
  //XmlUtil::AppendTextElement(xml, "AutomaticallyAdded", GetAutomaticallyAdded());
  //XmlUtil::AppendTextElement(xml, "MultiLaunchGroup", BelongsToMultiLaunchGroup());
  //XmlUtil::AppendTextElement(xml, "IsGroup", IsGroup());
  //if (uuid_ != wxEmptyString) XmlUtil::AppendTextElement(xml, "UUID", uuid_);
  //if (parameters_ != wxEmptyString) XmlUtil::AppendTextElement(xml, "Parameters", parameters_);  
  //if (customIconPath_ != wxEmptyString) XmlUtil::AppendTextElement(xml, "CustomIcon", wxString::Format(_T("%s,%d"), customIconPath_, customIconIndex_));

  //if (IsGroup()) {
  //  TiXmlElement* childrenXml = new TiXmlElement("Children");
  //  xml->LinkEndChild(childrenXml);

  //  for (int i = 0; i < children_.size(); i++) {
  //    appFolderItem* folderItem = children_.at(i);
  //    if (!folderItem) continue;
  //    childrenXml->LinkEndChild(folderItem->ToXml());
  //  }    
  //}

  return xml;
}


void FolderItem::fromXml(TiXmlElement* xml) {
  TiXmlHandle handle(xml);

  setName(XmlUtil::readElementText(handle, "Name"));
  setFilePath(XmlUtil::readElementText(handle, "FilePath"));
  setAutomaticallyAdded(XmlUtil::readElementTextAsBool(handle, "AutomaticallyAdded"));
  belongsToMultiLaunchGroup_ = XmlUtil::readElementTextAsBool(handle, "MultiLaunchGroup");
  isGroup_ = XmlUtil::readElementTextAsBool(handle, "IsGroup");
  //uuid_ = XmlUtil::readElementText(handle, "UUID");
  parameters_ = XmlUtil::readElementText(handle, "Parameters");

  //wxArrayString customIconData;
  //XmlUtil::ReadElementTextAsArrayString(handle, "CustomIcon", customIconData);
  //if (customIconData.Count() >= 2) {
  //  customIconPath_ = customIconData[0];
  //  customIconIndex_ = 0;
  //  long t;
  //  if (customIconData[1].ToLong(&t)) customIconIndex_ = (int)t;
  //}

  //for (int i = 0; i < children_.size(); i++) children_[i]->Dispose();
  //children_.clear();
  //
  //TiXmlElement* childrenXml = handle.Child("Children", 0).ToElement();
  //if (childrenXml) {
  //  for (TiXmlElement* element = childrenXml->FirstChildElement(); element; element = element->NextSiblingElement()) {
  //    wxString elementName = wxString(element->Value(), wxConvUTF8);
  //    if ((elementName != _T("FolderItem")) && (elementName != _T("appFolderItem"))) continue;
  //    
  //    appFolderItem* folderItem = appFolderItem::CreateFolderItem();
  //    folderItem->FromXml(element);
  //    AddChild(folderItem);
  //  }
  //}

  //ConvertOldVariablesToNew(filePath_);
  //ConvertOldVariablesToNew(parameters_);
  //ConvertOldVariablesToNew(customIconPath_);
}