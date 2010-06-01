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


int FolderItem::uniqueID_ = 1;
FolderItemIdHashMap FolderItem::folderItemIdHashMap_;


FolderItem::FolderItem(int type) {
  FolderItem::uniqueID_++;
  id_ = FolderItem::uniqueID_;
  type_ = type;

  autoRun_ = false;
  name_ = "";
  path_ = "";
  automaticallyAdded_ = false;
  parent_ = NULL;
  disposed_ = false;
}


FolderItem::~FolderItem() {
  clearIconCache();
}


void FolderItem::clearIconCache() {
  for (std::map<int, IconData*>::iterator i = iconData_.begin(); i != iconData_.end(); ++i) {
    if (i->second) delete i->second;
  }
  for (std::map<int, QPixmap*>::iterator i = iconPixmaps_.begin(); i != iconPixmaps_.end(); ++i) {
    if (i->second) delete i->second;
  }
  iconData_.clear();
  iconPixmaps_.clear();
}


IconData* FolderItem::loadIconData(int iconSize) {
  if (iconData_.find(iconSize) != iconData_.end()) return iconData_[iconSize];
  if (getIconThreads_.find(iconSize) != getIconThreads_.end()) return NULL;
  
  GetIconThread* thread = new GetIconThread();
  thread->setIconData(resolvedPath(), iconSize);
  getIconThreads_[iconSize] = thread;

  QObject::connect(thread, SIGNAL(finished()),
                   this, SLOT(getIconThread_finished()));
  thread->start();

  return NULL;
}


int FolderItem::iconDataLoadingState(int iconSize) {
  if (iconData_.find(iconSize) != iconData_.end()) return ICON_LOADING_STATE_LOADED;
  if (getIconThreads_.find(iconSize) != getIconThreads_.end()) {
    GetIconThread* thread = getIconThreads_[iconSize];
    return thread->isRunning() ? ICON_LOADING_STATE_LOADING : ICON_LOADING_STATE_ERROR;
  }
  return ICON_LOADING_STATE_UNLOADED;
}


void FolderItem::getIconThread_finished() {
  GetIconThread* thread = (GetIconThread*)(this->sender());
  iconData_[thread->iconSize()] = thread->iconData();
  qDebug() << "Icon loaded:" << (thread->iconData() ? thread->iconData()->filePath : "NO DATA");
  emit iconLoaded(thread->iconSize());
}


IconData* FolderItem::getIconData(int iconSize) {
  if (iconData_.find(iconSize) != iconData_.end()) return iconData_[iconSize];

  return NULL;
  //IconData* d = IconUtil::getFolderItemIcon(resolvedPath(), iconSize);
  //iconData_[iconSize] = d;
  //return d;
}


QPixmap* FolderItem::getIconPixmap(int iconSize) {
  if (iconPixmaps_.find(iconSize) != iconPixmaps_.end()) return iconPixmaps_[iconSize];

  IconData* d = getIconData(iconSize);
  QPixmap* pixmap = NULL;

  if (d) pixmap = new QPixmap(QPixmap::fromWinHICON(d->hIcon));
  iconPixmaps_[iconSize] = pixmap;
  
  return pixmap;
}


void FolderItem::dispose() {
  disposed_ = true;
}


bool FolderItem::disposed() const {
  return disposed_;
}


int FolderItem::numChildren() const {
  return children_.size();
}


FolderItem* FolderItem::getChildAt(int index) const {
  return children_.at(index);
}


void FolderItem::setParent(FolderItem* folderItem) {
  parent_ = folderItem;
}


void FolderItem::addChild(FolderItem* folderItem) {
  if (folderItem->id() == id()) return;

  //if (folderItem->IsGroup()) {
  //  appFolderItem* f = folderItem->GetChildById(GetId());
  //  if (f) return;
  //}

  FolderItem* p = folderItem->parent();
  if (p) p->removeChild(folderItem);

  folderItem->setParent(this);
  children_.push_back(folderItem);
}


void FolderItem::removeChild(FolderItem* folderItem) {
  for (int i = 0; i < (int)children_.size(); i++) {
    FolderItem* child = children_.at(i);

    if (child->id() == folderItem->id()) {

      //if (folderItem->GetAutomaticallyAdded()) {
      //  wxGetApp().GetUser()->AddAutoAddExclusion(folderItem->GetResolvedPath());
      //}
      
      child->setParent(NULL);
      children_.erase(children_.begin() + i);

      //wxGetApp().FolderItems_CollectionChange();
      return;
    }
  }
}


int FolderItem::id() const {
  return id_;
}


FolderItem* FolderItem::parent() const {
  return parent_;
}


QString FolderItem::resolvePath(const QString& path) {
  QString output = FilePaths::resolveVariables(path);
  return output;
}


FolderItem* FolderItem::createFolderItem(int type) {
  FolderItem* f = new FolderItem(type);
  folderItemIdHashMap_[f->id()] = f;
  return f;
}


FolderItem* FolderItem::getFolderItemById(int id) {
  FolderItem* sp = folderItemIdHashMap_[id];
  
  // The folder item is not (or no longer) in the hash map
  if (!sp) return NULL;

  if (sp->disposed()) {
    // The folder item has been disposed, so remove it
    // from the hash map now and return NULL
    delete sp;
    folderItemIdHashMap_.erase(id);
    return NULL;
  }

  // Otherwise return the pointer
  return sp;
}


void FolderItem::setName(const QString& name) {
  name_ = name;
}


void FolderItem::setPath(const QString& filePath) {
  path_ = filePath;
  resolvedPath_ = "";
  clearIconCache();
}


QString FolderItem::path() const {
  return path_;
}


QString FolderItem::resolvedPath() {
  if (resolvedPath_ != "") return resolvedPath_;
  resolvedPath_ = FolderItem::resolvePath(path());
  return resolvedPath_;
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
  setPath(XmlUtil::readElementText(handle, "FilePath"));
  setAutomaticallyAdded(XmlUtil::readElementTextAsBool(handle, "AutomaticallyAdded"));
  type_ = XmlUtil::readElementTextAsBool(handle, "IsGroup") ? FOLDER_ITEM_TYPE_GROUP : FOLDER_ITEM_TYPE_FILE;
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

  for (int i = 0; i < (int)children_.size(); i++) children_[i]->dispose();
  children_.clear();
  
  TiXmlElement* childrenXml = handle.Child("Children", 0).ToElement();
  if (childrenXml) {
    for (TiXmlElement* element = childrenXml->FirstChildElement(); element; element = element->NextSiblingElement()) {
      QString elementName = QString::fromUtf8(element->GetText());
      if ((elementName != "FolderItem") && (elementName != "appFolderItem")) continue;
      
      FolderItem* folderItem = FolderItem::createFolderItem();
      folderItem->fromXml(element);
      addChild(folderItem);
    }
  }

  //ConvertOldVariablesToNew(filePath_);
  //ConvertOldVariablesToNew(parameters_);
  //ConvertOldVariablesToNew(customIconPath_);
}