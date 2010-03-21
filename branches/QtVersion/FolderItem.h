/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef FolderItem_H
#define FolderItem_H
namespace appetizer {

class FolderItem {

public:

    FolderItem(bool isGroup);
    static FolderItem* createFolderItem(bool isGroup = false);
    static QString resolvePath(const QString& path);
    TiXmlElement* toXml();
    void fromXml(TiXmlElement* xml);
    void setName(const QString& name);
    void setFilePath(const QString& filePath);
    void setAutomaticallyAdded(bool automaticallyAdded);
    void setAutoRun(bool autoRun);

private:

    QString name_;
    QString filePath_;
    bool automaticallyAdded_;
    bool isGroup_;
    QString uuid_;
    QString parameters_;
    bool autoRun_;

};

}
#endif // FolderItem_H
