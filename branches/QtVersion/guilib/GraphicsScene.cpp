/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <GraphicsScene.h>

using namespace appetizer;

GraphicsScene::GraphicsScene(GraphicsWindow* parentWindow) {
	parentWindow_ = parentWindow;
}


QList<QGraphicsItem*> GraphicsScene::topLevelItems() {
	int numItems, iItem;
	QList<QGraphicsItem*> topLevel;
	QList<QGraphicsItem*> itemList = items();

	numItems = itemList.size();
	for (iItem = 0; iItem < numItems; iItem++) {
		QGraphicsItem *item = itemList.at(iItem);
		if (item->parentItem() == NULL)
			topLevel.append(item);
	}
	return topLevel;
}


void GraphicsScene::paintAll_(QPainter* painter, QGraphicsItem* item, int x, int y) {
  QStyleOptionGraphicsItem options;
  item->paint(painter, &options);

  QList<QGraphicsItem*> childItems = item->childItems();
  for (int i = 0; i < childItems.size(); i++) {
    QGraphicsItem* childItem = childItems.at(i);
    paintAll_(painter, childItem, 0, 0);
  }
}


void GraphicsScene::paintAll(QPainter* painter) {
  QList<QGraphicsItem*> items = topLevelItems();
  for (int i = 0; i < items.size(); i++) {
    QGraphicsItem* item = items.at(i);
    paintAll_(painter, item, 0, 0);
  }
}


GraphicsScene::~GraphicsScene() {
  QList<QGraphicsItem*> items = topLevelItems();
  for (int i = 0; i < items.size(); i++) {
    QGraphicsItem* item = items.at(i);
    SAFE_DELETE(item);
  }
}