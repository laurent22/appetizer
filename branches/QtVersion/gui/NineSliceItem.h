/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef NineSliceItem_H
#define NineSliceItem_H

#include <GraphicsItem.h>
#include <GraphicsWindow.h>
#include <NineSlicePainter.h>

namespace appetizer {

class NineSliceItem : public GraphicsItem {

public:

  NineSliceItem(GraphicsWindow* parentWindow);
  void loadBackgroundImage(QString backgroundFilePath);
  NineSlicePainter& nineSlicePainter();
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);

private:

  NineSlicePainter nineSlicePainter_;
  QString backgroundFilePath_;

};

}
#endif // NineSliceItem_H
