/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <Constants.h>

#ifndef FolderItem_H
#define FolderItem_H
namespace appetizer {

class FolderItem;

typedef std::vector<FolderItem*> FolderItemVector;
typedef std::map<int, FolderItem*> FolderItemIdHashMap;

class FolderItem {

public:

    FolderItem(int type = FOLDER_ITEM_TYPE_FILE);
    static FolderItem* createFolderItem(int type = FOLDER_ITEM_TYPE_FILE);
    static FolderItem* getFolderItemById(int id);
    static QString resolvePath(const QString& path);
    TiXmlElement* toXml();
    void fromXml(TiXmlElement* xml);
    void setName(const QString& name);
    void setFilePath(const QString& filePath);
    void setAutomaticallyAdded(bool automaticallyAdded);
    void setAutoRun(bool autoRun);
    void addChild(FolderItem* folderItem);
    int type() const;
    int id() const;
    FolderItem* parent() const;
    void setParent(FolderItem* folderItem);
    void removeChild(FolderItem* folderItem);
    bool disposed() const;
    void dispose();

private:

    QString name_;
    QString filePath_;
    bool automaticallyAdded_;
    int type_;
    QString uuid_;
    QString parameters_;
    bool autoRun_;
    int id_;
    FolderItemVector children_;
    FolderItem* parent_;
    bool disposed_;

    static int uniqueID_;
    static FolderItemIdHashMap folderItemIdHashMap_;

};

}
#endif // FolderItem_H
