/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef AZGRAPHICSITEM_H
#define AZGRAPHICSITEM_H

class azGraphicsItem : public QGraphicsItem {

public:

    azGraphicsItem();
    int width() const;
    int height() const;
    void setWidth(int width);
    void setHeight(int height);
    QRectF boundingRect() const;
    void invalidate();

private:

    int width_;
    int height_;

};

#endif // AZGRAPHICSITEM_H
