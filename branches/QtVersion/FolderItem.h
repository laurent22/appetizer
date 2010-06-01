/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <Constants.h>
#include <GetIconThread.h>
#include <IconUtil.h>
#include <IconData.h>

#ifndef FolderItem_H
#define FolderItem_H
namespace appetizer {

class FolderItem;

typedef std::vector<FolderItem*> FolderItemVector;
typedef std::map<int, FolderItem*> FolderItemIdHashMap;

class FolderItem : public QObject {

  Q_OBJECT

public:

  FolderItem(int type = FOLDER_ITEM_TYPE_FILE);
  ~FolderItem();

  static FolderItem* createFolderItem(int type = FOLDER_ITEM_TYPE_FILE);
  static FolderItem* getFolderItemById(int id);    
  static QString resolvePath(const QString& path);

  TiXmlElement* toXml();
  void fromXml(TiXmlElement* xml);
  
  void setName(const QString& name);
  void setPath(const QString& filePath);
  void setAutomaticallyAdded(bool automaticallyAdded);
  void setAutoRun(bool autoRun);
  int type() const;
  int id() const;
  QString path() const;
  QString resolvedPath();

  bool disposed() const;
  void dispose();

  IconData* getIconData(int iconSize);
  QPixmap* getIconPixmap(int iconSize);
  void clearIconCache();

  FolderItem* parent() const;
  void setParent(FolderItem* folderItem);
  void addChild(FolderItem* folderItem);
  void removeChild(FolderItem* folderItem);
  int numChildren() const;
  FolderItem* getChildAt(int index) const;

  IconData* loadIconData(int iconSize);
  int iconDataLoadingState(int iconSize);

public slots:

  void getIconThread_finished();

signals:

  void iconLoaded(int iconSize);

private:

  QString name_;
  QString path_;
  QString resolvedPath_;
  bool automaticallyAdded_;
  int type_;
  QString uuid_;
  QString parameters_;
  bool autoRun_;
  int id_;
  FolderItemVector children_;
  FolderItem* parent_;
  bool disposed_;
  std::map<int, IconData*> iconData_;
  std::map<int, QPixmap*> iconPixmaps_;
  std::map<int, GetIconThread*> getIconThreads_;

  static int uniqueID_;
  static FolderItemIdHashMap folderItemIdHashMap_;

};

}
#endif // FolderItem_H
