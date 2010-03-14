/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef AZMAINSCENE_H
#define AZMAINSCENE_H

#include <azNineSliceItem.h>

class azMainScene : public QGraphicsScene {

public:

  azMainScene();

private:

  azNineSliceItem* backgroundItem_;
  void drawBackground(QPainter* painter, const QRectF& rect);

};

#endif // AZMAINSCENE_H
