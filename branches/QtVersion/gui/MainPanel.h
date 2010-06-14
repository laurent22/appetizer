/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_MainPanel_H
#define Appetizer_MainPanel_H

#include <GraphicsItem.h>
#include <GraphicsWindow.h>
#include <IconPanel.h>
#include <NineSliceItem.h>
#include <ScrollPane.h>
#include <TabSprite.h>

namespace appetizer {


class PageData {

public :

  PageData();
  ~PageData();
  inline TabSprite* tab() const;
  inline IconPanel* iconPanel() const;
  void setTab(TabSprite* tab);
  void setIconPanel(IconPanel* iconPanel);
  FolderItem* folderItem();
  void setFolderItem(int folderItemId);

private:

  TabSprite* tab_;
  IconPanel* iconPanel_;
  int folderItemId_;

};

typedef std::vector<PageData*> PageDataVector;



class MainPanel : public GraphicsItem {

  Q_OBJECT

public:

  MainPanel(GraphicsWindow* parentWindow);
  ~MainPanel();
  void drawMask(QPainter* painter, int x, int y, int width, int height);
  NineSliceItem* backgroundSprite() const;
  void loadFolderItems(int rootFolderItemId);
  FolderItem* rootFolderItem();
  PageData* showPage(int index);
  PageData* getPage(int index);
  PageData* page();
  void updateDisplay();

private:

  QSize lastDrawnMaskSize_;
  NineSliceItem* backgroundSprite_;
  NineSlicePainter maskNineSlicePainter_;
  QPixmap maskPixmap_;
  PageDataVector pages_;
  int rootFolderItemId_;
  int pageIndex_;
  ScrollPane* scrollPane_;
  void clearPageData_();

public slots:

  void tab_clicked();

};

}
#endif // Appetizer_MainPanel_H
